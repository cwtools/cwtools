namespace CWTools.Games
open CWTools.Common
open CWTools.Validation.ValidationCore
open CWTools.Utilities.Utils
open CWTools.Validation.Rules
open CWTools.Validation

type ValidationManagerSettings<'T, 'S when 'T :> ComputedData and 'S :> IScope<'S> and 'S : comparison> = {
    validators : (StructureValidator<'T> * string) list
    experimentalValidators : (StructureValidator<'T> * string) list
    heavyExperimentalValidators : (LookupValidator<'T, 'S> * string) list
    experimental : bool
    fileValidators : (FileValidator<'T> * string) list
    resources : IResourceAPI<'T>
    lookup : Lookup<'S>
    lookupValidators : (LookupValidator<'T, 'S> * string) list
    ruleApplicator : RuleApplicator<'S> option
    foldRules : FoldRules<'S> option
    useRules : bool
    debugRulesOnly : bool
    localisationKeys : unit -> (Lang * Set<string>) list
    localisationValidators : LocalisationValidator<'T> list
}
type ValidationManager<'T, 'S when 'T :> ComputedData and 'S :> IScope<'S> and 'S : comparison>(settings : ValidationManagerSettings<'T, 'S>) =
    let resources = settings.resources
    let validators = settings.validators
    let validate (shallow : bool) (entities : struct (Entity * Lazy<'T>) list) =
        eprintfn "Validating %i files" (entities.Length)
        let allEntitiesByFile = entities |> List.map (fun struct (f, _) -> f.entity)
        let flattened = allEntitiesByFile |> List.map (fun n -> n.Children) |> List.collect id

        let oldEntities = EntitySet (resources.AllEntities())
        let newEntities = EntitySet entities
        let runValidators f (validators : (StructureValidator<'T> * string) list) =
            (validators <&!!&> (fun (v, s) -> duration (fun _ -> f v) s) |> (function |Invalid es -> es |_ -> []))
            @ (if not settings.experimental then [] else settings.experimentalValidators <&!&> (fun (v, s) -> duration (fun _ -> f v) s) |> (function |Invalid es -> es |_ -> []))
        // eprintfn "Validating misc"
        let res = runValidators (fun f -> f oldEntities newEntities) validators
        // eprintfn "Validating rules"
        let rres = (if settings.useRules && settings.ruleApplicator.IsSome then (runValidators (fun f -> f oldEntities newEntities) [settings.ruleApplicator.Value.RuleValidate(), "rules"]) else [])
        // eprintfn "Validating files"
        let fres = settings.fileValidators <&!&> (fun (v, s) -> duration (fun _ -> v resources newEntities) s) |> (function |Invalid es -> es |_ -> [])
        // eprintfn "Validating effects/triggers"
        let lres = settings.lookupValidators <&!&> (fun (v, s) -> duration (fun _ -> v settings.lookup oldEntities newEntities) s) |> function |Invalid es -> es |_ -> []
        let hres = if settings.experimental && (not (shallow)) then settings.heavyExperimentalValidators <&!&> (fun (v, s) -> duration (fun _ -> v settings.lookup oldEntities newEntities) s) |> function |Invalid es -> es |_ -> [] else []
        let shallow = if settings.debugRulesOnly then rres else res @ fres @ lres @ rres
        let deep = hres
        shallow, deep

    let validateLocalisation (entities : struct (Entity * Lazy<'T>) list) =
        eprintfn "Localisation check %i files" (entities.Length)
        let oldEntities = EntitySet (resources.AllEntities())
        let newEntities = EntitySet entities
        let vs = (settings.localisationValidators |> List.map (fun v -> v oldEntities (settings.localisationKeys()) newEntities) |> List.fold (<&&>) OK)
        let typeVs =
            if settings.useRules && settings.foldRules.IsSome
            then
                eprintfn "locval"
                (entities |> List.map (fun struct (e, _) -> e)) <&!&> settings.foldRules.Value.GetTypeLocalisationErrors
            else OK
        let vs = if settings.debugRulesOnly then typeVs else vs <&&> typeVs
        ((vs) |> (function |Invalid es -> es |_ -> []))



    member __.Validate((shallow : bool), (entities : struct (Entity * Lazy<'T>) list))  = validate shallow entities
    member __.ValidateLocalisation(entities : struct (Entity * Lazy<'T>) list) = validateLocalisation entities