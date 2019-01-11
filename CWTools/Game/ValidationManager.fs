namespace CWTools.Games
open CWTools.Common
open CWTools.Validation.ValidationCore
open CWTools.Utilities.Utils
open CWTools.Validation.Rules
open CWTools.Validation
open CWTools.Parser.ConfigParser
open CWTools.Process
open CWTools.Parser.Types
open CWTools.Validation.Stellaris.STLLocalisationValidation
open CWTools.Utilities.Position

type ValidationManagerSettings<'T, 'S, 'M when 'T :> ComputedData and 'S :> IScope<'S> and 'S : comparison and 'M :> IModifier> = {
    validators : (StructureValidator<'T> * string) list
    experimentalValidators : (StructureValidator<'T> * string) list
    heavyExperimentalValidators : (LookupValidator<'T, 'S, 'M> * string) list
    experimental : bool
    fileValidators : (FileValidator<'T> * string) list
    lookupValidators : (LookupValidator<'T, 'S, 'M> * string) list
    useRules : bool
    debugRulesOnly : bool
    localisationValidators : LocalisationValidator<'T> list
}

type ValidationManagerServices<'T, 'S, 'M when 'T :> ComputedData and 'S :> IScope<'S> and 'S : comparison and 'M :> IModifier> = {
    resources : IResourceAPI<'T>
    lookup : Lookup<'S, 'M>
    ruleApplicator : RuleApplicator<'S> option
    foldRules : FoldRules<'S> option
    localisationKeys : unit -> (Lang * Set<string>) list
}
type ValidationManager<'T, 'S, 'M when 'T :> ComputedData and 'S :> IScope<'S> and 'S : comparison and 'M :> IModifier>
        (settings : ValidationManagerSettings<'T, 'S, 'M>
        , services : ValidationManagerServices<'T, 'S, 'M>) =
    let resources = services.resources
    let validators = settings.validators
    let validate (shallow : bool) (entities : struct (Entity * Lazy<'T>) list) =
        log (sprintf "Validating %i files" (entities.Length))
        let allEntitiesByFile = entities |> List.map (fun struct (f, _) -> f.entity)
        let flattened = allEntitiesByFile |> List.map (fun n -> n.Children) |> List.collect id

        let oldEntities = EntitySet (resources.AllEntities())
        let newEntities = EntitySet entities
        let runValidators f (validators : (StructureValidator<'T> * string) list) =
            (validators <&!!&> (fun (v, s) -> duration (fun _ -> f v) s) |> (function |Invalid es -> es |_ -> []))
            @ (if not settings.experimental then [] else settings.experimentalValidators <&!&> (fun (v, s) -> duration (fun _ -> f v) s) |> (function |Invalid es -> es |_ -> []))
        // log "Validating misc"
        let res = runValidators (fun f -> f oldEntities newEntities) validators
        // log "Validating rules"
        let rres = (if settings.useRules && services.ruleApplicator.IsSome then (runValidators (fun f -> f oldEntities newEntities) [services.ruleApplicator.Value.RuleValidate(), "rules"]) else [])
        // log "Validating files"
        let fres = settings.fileValidators <&!&> (fun (v, s) -> duration (fun _ -> v resources newEntities) s) |> (function |Invalid es -> es |_ -> [])
        // log "Validating effects/triggers"
        let lres = settings.lookupValidators <&!&> (fun (v, s) -> duration (fun _ -> v services.lookup oldEntities newEntities) s) |> function |Invalid es -> es |_ -> []
        let hres = if settings.experimental && (not (shallow)) then settings.heavyExperimentalValidators <&!&> (fun (v, s) -> duration (fun _ -> v services.lookup oldEntities newEntities) s) |> function |Invalid es -> es |_ -> [] else []
        let shallow = if settings.debugRulesOnly then rres else res @ fres @ lres @ rres
        let deep = hres
        shallow, deep

    let validateLocalisation (entities : struct (Entity * Lazy<'T>) list) =
        log (sprintf "Localisation check %i files" (entities.Length))
        let oldEntities = EntitySet (resources.AllEntities())
        let newEntities = EntitySet entities
        let vs = (settings.localisationValidators |> List.map (fun v -> v oldEntities (services.localisationKeys()) newEntities) |> List.fold (<&&>) OK)
        let typeVs =
            if settings.useRules && services.foldRules.IsSome
            then
                (entities |> List.map (fun struct (e, _) -> e)) <&!&> services.foldRules.Value.GetTypeLocalisationErrors
            else OK
        let vs = if settings.debugRulesOnly then typeVs else vs <&&> typeVs
        ((vs) |> (function |Invalid es -> es |_ -> []))

    let globalTypeDefLoc () =
        let validateLoc (values : (string * range) list) (locdef : TypeLocalisation)  =
            values
                |> List.filter (fun (s, _) -> s.Contains(".") |> not)
                <&!&> (fun (key, range) ->
                            let fakeLeaf = LeafValue(Value.Bool true, range)
                            let lockey = locdef.prefix + key + locdef.suffix
                            checkLocKeysLeafOrNode (services.localisationKeys()) lockey fakeLeaf)
        let validateType (typename : string) (values : (string * range) list) =
            eprintfn "vt %s" typename
            match services.lookup.typeDefs |> List.tryFind (fun td -> td.name = typename) with
            |None -> OK
            |Some td ->
                eprintfn "vtd %A" td.localisation
                td.localisation |> List.filter (fun locdef -> locdef.required) <&!&> validateLoc values
        let validateSubType (typename : string) (values : (string * range) list) =
            let splittype = typename.Split([|'.'|], 2)
            if splittype.Length > 1
            then
                match services.lookup.typeDefs |> List.tryFind (fun td -> td.name = splittype.[0]) with
                |None -> OK
                |Some td ->
                    match td.subtypes |> List.tryFind (fun st -> st.name = splittype.[1]) with
                    |None -> OK
                    |Some st -> st.localisation |> List.filter (fun locdef -> locdef.required) <&!&> validateLoc values
            else OK
        services.lookup.typeDefInfo |> Map.toList <&!&> (fun (t, l) -> validateType t l)
        <&&>(services.lookup.typeDefInfo |> Map.toList <&!&> (fun (t, l) -> validateSubType t l))


    member __.Validate((shallow : bool), (entities : struct (Entity * Lazy<'T>) list))  = validate shallow entities
    member __.ValidateLocalisation(entities : struct (Entity * Lazy<'T>) list) = validateLocalisation entities
    member __.ValidateGlobalLocalisation() = globalTypeDefLoc()