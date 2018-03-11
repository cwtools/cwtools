namespace CWTools.Games
open CWTools.Process
open FSharp.Collections.ParallelSeq
open FParsec
open System.IO
open CWTools.Parser
open CWTools.Process.STLProcess
open CWTools.Process.STLScopes

type References(resourceManager : IResourceAPI, lookup : Lookup) =
    let entities() = resourceManager.AllEntities() |> List.map (fun e -> e.entity)
    let events() = 
        entities()
        |> List.collect(fun e -> e.Children)
        |> List.choose (function | :? Event as e -> Some e |_ -> None)
    let eventIDs() = events() |> List.map (fun e -> e.ID)
    let modifiers() = lookup.staticModifiers |> List.map(fun m -> m.tag)
    let triggers() = lookup.scriptedTriggers |> List.map(fun t -> t.Name)
    let effects() = lookup.scriptedEffects |> List.map(fun e -> e.Name)
    member __.EventIDs = eventIDs()
    member __.ModifierNames = modifiers()
    member __.TriggerNames = triggers()
    member __.EffectNames = effects()
    member __.ScopeNames = oneToOneScopes |> List.map (fun (n, _) -> n)
    member __.ScriptVariableNames = lookup.definedScriptVariables |> List.distinct
