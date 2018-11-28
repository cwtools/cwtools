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
    useRules : bool
    debugRulesOnly : bool
}
type ValidationManager<'T, 'S when 'T :> ComputedData and 'S :> IScope<'S> and 'S : comparison>(settings : ValidationManagerSettings<'T, 'S>) =
    let resources = settings.resources
    let validators = settings.validators
    let validate (shallow : bool) (entities : struct (Entity * Lazy<'T>) list) =
        eprintfn "Validating %i files" (entities.Length)
        let allEntitiesByFile = entities |> List.map (fun struct (f, _) -> f.entity)
        let flattened = allEntitiesByFile |> List.map (fun n -> n.Children) |> List.collect id

        // let validators = if settings.useRules && settings.ruleApplicator.IsSome then (settings.ruleApplicator.Value.RuleValidate, "rules")::validators else validators
        // let experimentalvalidators = [valSectionGraphics, "sections"; valComponentGraphics, "component"]
        let oldEntities = EntitySet (resources.AllEntities())
        let newEntities = EntitySet entities
        let runValidators f (validators : (StructureValidator<'T> * string) list) =
            (validators <&!!&> (fun (v, s) -> duration (fun _ -> f v) s) |> (function |Invalid es -> es |_ -> []))
            @ (if not settings.experimental then [] else settings.experimentalValidators <&!&> (fun (v, s) -> duration (fun _ -> f v) s) |> (function |Invalid es -> es |_ -> []))
        //let res = validators |> List.map (fun v -> v oldEntities newEntities) |> List.fold (<&&>) OK
        // eprintfn "Validating misc"
        let res = runValidators (fun f -> f oldEntities newEntities) validators
        // eprintfn "Validating rules"
        let rres = (if settings.useRules && settings.ruleApplicator.IsSome then (runValidators (fun f -> f oldEntities newEntities) [settings.ruleApplicator.Value.RuleValidate(), "rules"]) else [])
        //let res = validators <&!&> (fun v -> v oldEntities newEntities) |> (function |Invalid es -> es |_ -> [])
        // eprintfn "Validating files"
        // let STLFileValidators = [valSpriteFiles, "sprites"; valMeshFiles, "mesh"; valAssetFiles, "asset"; valComponentIcons, "compicon"]
        let fres = settings.fileValidators <&!&> (fun (v, s) -> duration (fun _ -> v resources newEntities) s) |> (function |Invalid es -> es |_ -> [])
        // eprintfn "Validating effects/triggers"
        let lres = settings.lookupValidators <&!&> (fun (v, s) -> duration (fun _ -> v settings.lookup oldEntities newEntities) s) |> function |Invalid es -> es |_ -> []
        // let eres = duration (fun _ -> valAllEffects (lookup.scriptedTriggers) (lookup.scriptedEffects) (lookup.staticModifiers) newEntities  |> (function |Invalid es -> es |_ -> [])) "effects"
        // let tres = duration (fun _ ->  valAllTriggers (lookup.scriptedTriggers) (lookup.scriptedEffects) (lookup.staticModifiers) newEntities  |> (function |Invalid es -> es |_ -> [])) "triggers"
        // let wres = duration (fun _ ->  validateModifierBlocks (lookup.scriptedTriggers) (lookup.scriptedEffects) (lookup.staticModifiers) newEntities |> (function |Invalid es -> es |_ -> [])) "weights"
        // let mres = duration (fun _ ->  valAllModifiers (lookup.coreModifiers) newEntities  |> (function |Invalid es -> es |_ -> [])) "modifiers"
        let hres = if settings.experimental && (not (shallow)) then settings.heavyExperimentalValidators <&!&> (fun (v, s) -> duration (fun _ -> v settings.lookup oldEntities newEntities) s) |> function |Invalid es -> es |_ -> [] else []
        // let evres = duration (fun _ ->  ( if settings..experimental && (not(shallow)) then getEventChains (lookup.scriptedEffects) oldEntities newEntities else OK) |> (function |Invalid es -> es |_ -> [])) "events"
        //let etres = getEventChains newEntities |> (function |Invalid es -> es |_ -> [])
        //(validateShips (flattened)) @ (validateEvents (flattened)) @ res @ fres @ eres
        let shallow = if settings.debugRulesOnly then rres else res @ fres @ lres @ rres
        let deep = hres
        shallow, deep


    member __.Validate((shallow : bool), (entities : struct (Entity * Lazy<'T>) list))  = validate shallow entities
