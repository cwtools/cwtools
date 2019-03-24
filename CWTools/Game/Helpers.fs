namespace CWTools.Games
open CWTools.Parser.ConfigParser
open CWTools.Common
open CWTools.Utilities
open CWTools.Utilities.Position
open FSharp.Collections.ParallelSeq
open CWTools.Process.Scopes
open CWTools.Process
open CWTools.Utilities.Utils
open CWTools.Validation.Rules

module Helpers =
    let updateEventTargetLinks (embeddedSettings : EmbeddedSettings<_,_>) =
        let simpleEventTargetLinks = embeddedSettings.eventTargetLinks |> List.choose (function | SimpleLink l -> Some (l :> Effect<_>) | _ -> None)
        simpleEventTargetLinks

    let private convertSourceRuleType (lookup : Lookup<'S,'M>) (link : EventTargetDataLink<_>) =
        // log (sprintf "csr %A" link)
        match link.sourceRuleType.Trim() with
        | x when x.StartsWith "<" && x.EndsWith ">" ->
            let sourceType = x.Trim([|'<';'>'|])
            match lookup.typeDefInfo |> Map.tryFind (sourceType) with
                | Some x -> x |> List.map fst
                | None ->
                    log (sprintf "Link %s refers to undefined type %s" link.name sourceType)
                    []
        | x when x.StartsWith "enum[" ->
            let enum = getSettingFromString x "enum"
            match enum |> Option.bind (fun x -> Map.tryFind x lookup.enumDefs) with
            | Some (_, vs) -> vs
            | None ->
                log (sprintf "Link %s refers to undefined enum %A" link.name enum)
                []
        | x when x.StartsWith "value[" ->
            let valuename = getSettingFromString x "value"
            match valuename |> Option.bind (fun x -> Map.tryFind x lookup.varDefInfo) with
            | Some vs -> vs |> List.map fst
            | None ->
                log (sprintf "Link %s refers to undefined value %A" link.name valuename)
                []
        | x ->
            log (sprintf "Link %s refers to invalid source %s" link.name x)
            []

    let addDataEventTargetLinks (lookup : Lookup<'S,'M>) (embeddedSettings : EmbeddedSettings<_,_>) =
        let links = embeddedSettings.eventTargetLinks |> List.choose (function | DataLink l -> Some (l) | _ -> None)
        let convertLinkToEffects (link : EventTargetDataLink<_>) =
            let typeDefinedKeys = convertSourceRuleType lookup link
            let keyToEffect (key : string) =
                let prefkey = link.dataPrefix |> Option.map (fun pref -> pref + key ) |> Option.defaultValue key
                ScopedEffect(prefkey, link.inputScopes, link.outputScope, EffectType.Both, link.description, "", true)
            typeDefinedKeys |> List.map keyToEffect
        links |> List.collect convertLinkToEffects |> List.map (fun e -> e :> Effect<_>)
