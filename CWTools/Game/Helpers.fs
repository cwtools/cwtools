namespace CWTools.Games
open CWTools.Parser
open CWTools.Common
open CWTools.Process
open CWTools.Rules
open CWTools.Utilities
open CWTools.Utilities.Utils

module Helpers =
    let updateEventTargetLinks (embeddedSettings : EmbeddedSettings) =
        let simpleEventTargetLinks = embeddedSettings.eventTargetLinks |> List.choose (function | SimpleLink l -> Some (l :> Effect) | _ -> None)
        simpleEventTargetLinks

    let private convertSourceRuleType (lookup : Lookup) (link : EventTargetDataLink) =
        // log (sprintf "csr %A" link)
        let types = lookup.typeDefInfo
        let enums = lookup.enumDefs
        let ruleToCompletionListHelper =
                        function
                        | LeafRule (SpecificField (SpecificValue x), _), _ -> [StringResource.stringManager.GetStringForIDs x]
                        | NodeRule (SpecificField (SpecificValue x), _), _ -> [StringResource.stringManager.GetStringForIDs x]
                        | LeafRule (NewField.TypeField (TypeType.Simple t), _), _ ->
                            types.TryFind(t)
                            |> Option.map (fun s -> s |> List.map (fun s -> s.id)) |> Option.defaultValue []
                        | NodeRule (NewField.TypeField (TypeType.Simple t), _), _ ->
                            types.TryFind(t)
                            |> Option.map (fun s -> s |> List.map (fun s -> s.id)) |> Option.defaultValue []
                        | LeafRule (NewField.TypeField (TypeType.Complex (p,t,suff)), _), _ ->
                            types.TryFind(t)
                            |> Option.map (fun s -> s |> List.map (fun s -> (p + s.id + suff))) |> Option.defaultValue []
                        | NodeRule (NewField.TypeField (TypeType.Complex (p,t,suff)), _), _ ->
                            types.TryFind(t)
                            |> Option.map (fun s -> s |> List.map (fun s -> (p + s.id + suff))) |> Option.defaultValue []
                        | LeafRule (NewField.ValueField (Enum e), _), _ ->
                            enums.TryFind(e)
                            |> Option.map (fun (_, s) -> s |> List.map (fun (s, _) -> s)) |> Option.defaultValue []
                        | NodeRule (NewField.ValueField (Enum e), _), _ ->
                            enums.TryFind(e)
                            |> Option.map (fun (_, s) -> s |> List.map (fun (s, _) -> s)) |> Option.defaultValue []
                        | _ -> []
        let aliases =
                    lookup.configRules |> List.choose (function |AliasRule (a, rs) -> Some (a, rs) |_ -> None)
                              |> List.groupBy fst
                              |> List.map (fun (k, vs) -> k, vs |> List.map snd)
                              |> Collections.Map.ofList
                              
        let aliasKeyMap =
                    aliases |> Map.toList |> List.map (fun (key, rules) -> key, (rules |> List.collect ruleToCompletionListHelper))
                        |> List.map (fun (key, values) -> key, Collections.Set.ofList values)
                        |> Map.ofList
        match link.sourceRuleType.Trim() with
        | x when x.StartsWith "<" && x.EndsWith ">" ->
            let sourceType = x.Trim([|'<';'>'|])
            match lookup.typeDefInfo |> Map.tryFind sourceType with
                | Some x -> x |> List.map (fun tdi -> tdi.id, Some (TypeRef(sourceType, tdi.id)))
                | None ->
                    log (sprintf "Link %s refers to undefined type %s" link.name sourceType)
                    []
        | x when x.StartsWith "enum[" ->
            let enum = CWTools.Rules.RulesParser.getSettingFromString x "enum"
            match enum |> Option.bind (fun x -> Map.tryFind x lookup.enumDefs) with
            | Some (_, vs) -> (vs |> List.map (fun (x, _) -> x, None))
            | None ->
                log (sprintf "Link %s refers to undefined enum %A" link.name enum)
                []
        | x when x.StartsWith "value[" ->
            let valuename = CWTools.Rules.RulesParser.getSettingFromString x "value"
            match valuename |> Option.bind (fun x -> Map.tryFind x lookup.varDefInfo) with
            | Some vs -> vs |> List.map (fun x -> fst x, None)
            | None ->
                log (sprintf "Link %s refers to undefined value %A" link.name valuename)
                []
        | x when x.StartsWith "alias_keys_field[" ->
            let aliasname = CWTools.Rules.RulesParser.getSettingFromString x "alias_keys_field"
            match aliasname |> Option.bind (fun x -> Map.tryFind x aliasKeyMap) with
            | Some vs -> vs |> Set.toList |> List.map (fun x -> x, None)
            | None ->
                log (sprintf "Link %s refers to undefined alias %A" link.name aliasname)
                []
        | x ->
            log (sprintf "Link %s refers to invalid source %s" link.name x)
            []

    let private getWildCard (link : EventTargetDataLink) =
        match link.sourceRuleType.Trim(), link.dataPrefix with
        | x, Some prefix when x.StartsWith "value[" ->
            Some (ScopedEffect(prefix, link.inputScopes, Some link.outputScope, EffectType.Link, link.description, "", true, [], true, false, true, None))
        | _ -> None

    let addDataEventTargetLinks (lookup : Lookup) (embeddedSettings : EmbeddedSettings) (addWildCardLinks : bool) =
        let links = embeddedSettings.eventTargetLinks |> List.choose (function | DataLink l -> Some l | _ -> None)
  
        let convertLinkToEffects (link : EventTargetDataLink) =
            let typeDefinedKeys = convertSourceRuleType lookup link
            let keyToEffect (key : string, refHint : ReferenceHint option) =
                let prefkey = link.dataPrefix |> Option.map (fun pref -> pref + key ) |> Option.defaultValue key
                match link.dataLinkType with
                | DataLinkType.Scope ->
                    [ScopedEffect(prefkey, link.inputScopes, Some link.outputScope, EffectType.Link, link.description, "", true, false, refHint)]
                | DataLinkType.Value ->
                    [ScopedEffect(prefkey, link.inputScopes, Some link.outputScope, EffectType.ValueTrigger, link.description, "", true, false, refHint)]
                | DataLinkType.Both ->
                    [
                        ScopedEffect(prefkey, link.inputScopes, Some link.outputScope, EffectType.Link, link.description, "", true, false, refHint)
                        ScopedEffect(prefkey, link.inputScopes, Some link.outputScope, EffectType.ValueTrigger, link.description, "", true, false, refHint)
                    ]
            let all = typeDefinedKeys |> List.collect keyToEffect
            let extra =
                if addWildCardLinks
                then getWildCard link
                else None
            match extra with
            | Some e -> e::all
            | None -> all
        links |> List.collect convertLinkToEffects |> List.map (fun e -> e :> Effect)

    let getLocalisationErrors (game : GameObject<_,_>) globalLocalisation =
        fun (force : bool, forceGlobal : bool) ->
            let resources = game.Resources
            let rulesLocErrors = game.ValidationManager.CachedRuleErrors(resources.ValidatableEntities()) |> List.filter (fun e -> e.code = "CW100")
            let genGlobal() =
                let ges = globalLocalisation(game)
                game.LocalisationManager.globalLocalisationErrors <- Some ges
                ges
            let genAll() =
                let les = (game.ValidationManager.ValidateLocalisation (resources.ValidatableEntities()))
                game.LocalisationManager.localisationErrors <- Some les
                les
            rulesLocErrors @
            match game.LocalisationManager.localisationErrors, game.LocalisationManager.globalLocalisationErrors with
            |Some les, Some ges -> (if force then genAll() else les) @ (if forceGlobal then genGlobal() else ges)
            |None, Some ges -> genAll() @ (if forceGlobal then genGlobal() else ges)
            |Some les, None -> (if force then genAll() else les) @ genGlobal()
            |None, None -> genAll() @ genGlobal()

    let createTypeDefInfo validate id range explicitLocalisation subtypes =
        {
            TypeDefInfo.id = id
            validate = validate
            range = range
            explicitLocalisation = explicitLocalisation
            subtypes = subtypes
        }

    open CWTools.Process.Localisation.ChangeLocScope
    open CWTools.Validation.LocalisationString
    open CWTools.Process.Scopes.Scopes
    open CWTools.Process.Localisation
    open CWTools.Validation

    let hardcodedLocalisation =
        [
            "playername";
            "prov"
        ]

    let validateProcessedLocalisation : ((Lang * LocKeySet) list -> (Lang * Map<string,LocEntry>) list -> ValidationResult) = validateProcessedLocalisationBase hardcodedLocalisation

    let createJominiLocalisationFunctions (jominiLocDataTypes : CWTools.Parser.DataTypeParser.JominiLocDataTypes option) =
        fun (lookup : Lookup) ->
            let dataTypes = jominiLocDataTypes |> Option.defaultValue { promotes = Map.empty; confidentFunctions = Map.empty; functions = Map.empty; dataTypes = Map.empty; dataTypeNames = Set.empty }
            let localisationCommandValidator = createJominiLocalisationCommandValidator dataTypes
            let validateLocalisationCommand = validateJominiLocalisationCommandsBase localisationCommandValidator
            let localisationCommandValidatorDefaultContext = localisationCommandValidator defaultContext
            let processLocalisation = processJominiLocalisationBase localisationCommandValidatorDefaultContext
            let definedEventTargets =
                (lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst)
            let eventtargets =
                lookup.savedEventTargets |> Seq.map (fun (a, _, c) -> (a, c)) |> List.ofSeq
                                         |> List.distinct
                                         |> List.fold (fun map (k, s) -> if Map.containsKey k map then Map.add k (s::map.[k]) map else Map.add k [s] map) Map.empty
            let eventtargets =
                definedEventTargets |> List.fold (fun oldMap et -> oldMap |> Map.add et [scopeManager.AnyScope]) eventtargets
            let definedvars =
                (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
            processLocalisation eventtargets definedvars, validateLocalisationCommand eventtargets definedvars


    let createLocalisationFunctions locStaticSettings createLocDynamicSettings (commands, variableCommands, localisationLinks) =
        fun (lookup : Lookup) ->
            let localisationCommandValidator commands variableCommands =
                createLegacyLocalisationCommandValidator (locStaticSettings commands variableCommands localisationLinks)
            let processLocalisation =
                fun commands variableCommands dynamicSettings ->
                    processLocalisationBase (localisationCommandValidator commands variableCommands dynamicSettings) defaultContext
            let validateLocalisationCommand =
                fun commands variableCommands dynamicSettings ->
                    validateLocalisationCommandsBase (localisationCommandValidator commands variableCommands dynamicSettings)

            processLocalisation commands variableCommands (createLocDynamicSettings(lookup)),
            validateLocalisationCommand commands variableCommands (createLocDynamicSettings(lookup))
