namespace CWTools.Games
open CWTools.Parser
open CWTools.Common
open CWTools.Process
open CWTools.Utilities.Utils

module Helpers =
    let updateEventTargetLinks (embeddedSettings : EmbeddedSettings<_>) =
        let simpleEventTargetLinks = embeddedSettings.eventTargetLinks |> List.choose (function | SimpleLink l -> Some (l :> Effect) | _ -> None)
        simpleEventTargetLinks

    let private convertSourceRuleType (lookup : Lookup<'M>) (link : EventTargetDataLink) =
        // log (sprintf "csr %A" link)
        match link.sourceRuleType.Trim() with
        | x when x.StartsWith "<" && x.EndsWith ">" ->
            let sourceType = x.Trim([|'<';'>'|])
            match lookup.typeDefInfo |> Map.tryFind (sourceType) with
                | Some x -> x |> List.map (fun tdi -> tdi.id)
                | None ->
                    log (sprintf "Link %s refers to undefined type %s" link.name sourceType)
                    []
        | x when x.StartsWith "enum[" ->
            let enum = CWTools.Rules.RulesParser.getSettingFromString x "enum"
            match enum |> Option.bind (fun x -> Map.tryFind x lookup.enumDefs) with
            | Some (_, vs) -> vs
            | None ->
                log (sprintf "Link %s refers to undefined enum %A" link.name enum)
                []
        | x when x.StartsWith "value[" ->
            let valuename = CWTools.Rules.RulesParser.getSettingFromString x "value"
            match valuename |> Option.bind (fun x -> Map.tryFind x lookup.varDefInfo) with
            | Some vs -> vs |> List.map fst
            | None ->
                log (sprintf "Link %s refers to undefined value %A" link.name valuename)
                []
        | x ->
            log (sprintf "Link %s refers to invalid source %s" link.name x)
            []

    let private getWildCard (link : EventTargetDataLink) =
        match link.sourceRuleType.Trim(), link.dataPrefix with
        | x, Some prefix when x.StartsWith "value[" ->
            Some (ScopedEffect(prefix, link.inputScopes, Some link.outputScope, EffectType.Link, link.description, "", true, [], true, false, true))
        | _ -> None

    let addDataEventTargetLinks (lookup : Lookup<'M>) (embeddedSettings : EmbeddedSettings<_>) (addWildCardLinks : bool) =
        let links = embeddedSettings.eventTargetLinks |> List.choose (function | DataLink l -> Some (l) | _ -> None)
        let convertLinkToEffects (link : EventTargetDataLink) =
            let typeDefinedKeys = convertSourceRuleType lookup link
            let keyToEffect (key : string) =
                let prefkey = link.dataPrefix |> Option.map (fun pref -> pref + key ) |> Option.defaultValue key
                match link.dataLinkType with
                | DataLinkType.Scope ->
                    [ScopedEffect(prefkey, link.inputScopes, Some link.outputScope, EffectType.Link, link.description, "", true, false)]
                | DataLinkType.Value ->
                    [ScopedEffect(prefkey, link.inputScopes, Some link.outputScope, EffectType.ValueTrigger, link.description, "", true, false)]
                | DataLinkType.Both ->
                    [
                        ScopedEffect(prefkey, link.inputScopes, Some link.outputScope, EffectType.Link, link.description, "", true, false)
                        ScopedEffect(prefkey, link.inputScopes, Some link.outputScope, EffectType.ValueTrigger, link.description, "", true, false)
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

    let getLocalisationErrors (game : GameObject<_,_,_>) globalLocalisation =
        fun (force : bool, forceGlobal : bool) ->
            let resources = game.Resources
            let rulesLocErrors = game.ValidationManager.CachedRuleErrors(resources.ValidatableEntities()) |> List.filter (fun (id, _, _, _, _, _) -> id = "CW100")
            let genGlobal() =
                let ges = (globalLocalisation(game))
                game.LocalisationManager.globalLocalisationErrors <- Some ges
                ges
            let genAll() =
                let les = (game.ValidationManager.ValidateLocalisation (resources.ValidatableEntities()))
                game.LocalisationManager.localisationErrors <- Some les
                les
            rulesLocErrors @
            match game.LocalisationManager.localisationErrors, game.LocalisationManager.globalLocalisationErrors with
            |Some les, Some ges -> (if force then genAll() else les) @ (if forceGlobal then genGlobal() else ges)
            |None, Some ges -> (genAll()) @ (if forceGlobal then genGlobal() else ges)
            |Some les, None -> (if force then genAll() else les) @ (genGlobal())
            |None, None -> (genAll()) @ (genGlobal())

    let createTypeDefInfo validate id range explicitLocalisation subtypes =
        {
            TypeDefInfo.id = id
            validate = validate
            range = range
            explicitLocalisation = explicitLocalisation
            subtypes = subtypes
        }
