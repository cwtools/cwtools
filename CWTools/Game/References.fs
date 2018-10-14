namespace CWTools.Games
open CWTools.Process
open FSharp.Collections.ParallelSeq
open FParsec
open System.IO
open CWTools.Parser
open CWTools.Process.STLProcess
open CWTools.Process.STLScopes
open CWTools.Localisation
open CWTools.Common




type References<'T, 'S when 'S : comparison>(resourceManager : IResourceAPI<'T>, lookup : Lookup<'S>, localisation : ILocalisationAPI list) =
    let entities() = resourceManager.AllEntities() |> List.map (fun struct (e, _) -> e.entity)
    let events() =
        entities()
        |> List.collect(fun e -> e.Children)
        |> List.choose (function | :? Event as e -> Some e |_ -> None)
    let eventIDs() = events() |> List.map (fun e -> e.ID)
    let modifiers() = lookup.staticModifiers |> List.map(fun m -> m.tag)
    let triggers() = lookup.scriptedTriggers |> List.map(fun t -> t.Name)
    let effects() = lookup.scriptedEffects |> List.map(fun e -> e.Name)
    let localisation() =
        localisation |> List.filter (fun l -> l.GetLang = ((STL STLLang.English)) || l.GetLang = ((HOI4 HOI4Lang.English)))
                     |> List.collect (fun l -> l.ValueMap |> Map.toList)
    member __.EventIDs = eventIDs()
    member __.ModifierNames = modifiers()
    member __.TriggerNames = triggers()
    member __.EffectNames = effects()
    member __.ScopeNames = oneToOneScopes |> List.map (fun (n, _) -> n)
    member __.ScriptVariableNames = lookup.definedScriptVariables |> List.distinct
    member __.Technologies = lookup.technologies
    member __.Localisation = localisation()
    member __.TypeMapInfo = lookup.typeDefInfo