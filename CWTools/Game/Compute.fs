module CWTools.Games.Compute

open CWTools.Games
open System
open CWTools.Rules
open System.Collections.Generic
open System.Collections.Concurrent

/// Converts ConcurrentDictionary<string, ResizeArray<ReferenceDetails>> to IReadOnlyDictionary<string, ReferenceDetails list>
/// Uses Dictionary instead of Map to avoid O(N log N) MapTree node allocations
let convertReferencedTypes (r: ConcurrentDictionary<string, ResizeArray<ReferenceDetails>>) : IReadOnlyDictionary<string, ReferenceDetails list> =
    let dict = Dictionary<string, ReferenceDetails list>(r.Count)
    for kv in r do
        dict[kv.Key] <- List.ofSeq kv.Value
    dict :> IReadOnlyDictionary<_, _>

let computeData (infoService: unit -> InfoService option) (e: Entity) =
    match infoService() with
    | Some svc ->
        let types, defvars, (effectNodes, _), (triggerNodes, _), eventtargets = svc.BatchFolds(e)
        let referencedtypes = convertReferencedTypes types
        ComputedData(Some referencedtypes, Some defvars, true, Some effectNodes, Some triggerNodes, Some eventtargets)
    | None ->
        ComputedData(None, None, false, None, None, None)

let computeDataUpdate (infoService: unit -> InfoService option) (e: Entity) (data: ComputedData) =
    match infoService() with
    | Some svc ->
        let types, defvars, (effectNodes, _), (triggerNodes, _), eventtargets = svc.BatchFolds(e)
        let referencedtypes = convertReferencedTypes types
        data.Referencedtypes <- Some referencedtypes
        data.Definedvariables <- Some defvars
        data.SavedEventTargets <- Some eventtargets
        data.EffectBlocks <- Some effectNodes
        data.TriggerBlocks <- Some triggerNodes
        data.WithRulesData <- true
    | None ->
        data.Referencedtypes <- None
        data.Definedvariables <- None
        data.SavedEventTargets <- None
        data.EffectBlocks <- None
        data.TriggerBlocks <- None
        data.WithRulesData <- false

let computeCK2Data = computeData
let computeCK2DataUpdate = computeDataUpdate
let computeHOI4Data = computeData
let computeHOI4DataUpdate = computeDataUpdate
let computeVIC2Data = computeData
let computeVIC2DataUpdate = computeDataUpdate

module EU4 =
    open CWTools.Process
    open CWTools.Process.ProcessCore

    let getScriptedEffectParams (node: Node) =
        let getDollarText (s: string) acc =
            s.Split('$')
            |> Array.mapi (fun i s -> i, s)
            |> Array.fold (fun acc (i, s) -> if i % 2 = 1 then s :: acc else acc) acc
        // let split = s.Split([|'$'|],3)
        // if split.Length = 3 then split.[1]::acc else acc
        let fNode =
            fun (x: Node) acc ->
                let nodeRes = getDollarText x.Key acc

                x.Leaves
                |> Seq.fold (fun a n -> getDollarText n.Key (getDollarText (n.Value.ToRawString()) a)) nodeRes

        node |> (foldNode7 fNode) |> List.ofSeq

    let getScriptedEffectParamsEntity (e: Entity) =
        if
            (e.logicalpath.StartsWith("common/scripted_effects", StringComparison.OrdinalIgnoreCase)
             || e.logicalpath.StartsWith("common/scripted_triggers", StringComparison.OrdinalIgnoreCase))
        then
            getScriptedEffectParams e.entity
        else
            []

    let computeEU4Data (infoService: unit -> InfoService option) (e: Entity) =
        let scriptedeffectparams = Some(getScriptedEffectParamsEntity e)
        match infoService() with
        | Some svc ->
            let types, defvars, (effectNodes, _), (triggerNodes, _), eventtargets = svc.BatchFolds(e)
            let referencedtypes = convertReferencedTypes types
            EU4ComputedData(
                Some referencedtypes,
                Some defvars,
                scriptedeffectparams,
                true,
                Some effectNodes,
                Some triggerNodes,
                Some eventtargets
            )
        | None ->
            EU4ComputedData(
                None,
                None,
                scriptedeffectparams,
                false,
                None,
                None,
                None
            )

    let computeEU4DataUpdate (infoService: unit -> InfoService option) (e: Entity) (data: EU4ComputedData) =
        match infoService() with
        | Some svc ->
            let types, defvars, (effectNodes, _), (triggerNodes, _), eventtargets = svc.BatchFolds(e)
            let referencedtypes = convertReferencedTypes types
            data.Referencedtypes <- Some referencedtypes
            data.Definedvariables <- Some defvars
            data.SavedEventTargets <- Some eventtargets
            data.EffectBlocks <- Some effectNodes
            data.TriggerBlocks <- Some triggerNodes
            data.WithRulesData <- true
        | None ->
            data.Referencedtypes <- None
            data.Definedvariables <- None
            data.SavedEventTargets <- None
            data.EffectBlocks <- None
            data.TriggerBlocks <- None
            data.WithRulesData <- false

module STL =
    open CWTools.Process
    open CWTools.Process.ProcessCore
    open CWTools.Utilities.Utils

    let getAllTechPrereqs (e: Entity) =
        let fNode =
            (fun (x: Node) acc ->
                match x with
                | _ -> acc)

        let nodes = e.entity.Children |> List.collect (foldNode7 fNode)

        let fNode =
            fun (t: Node) children ->
                let inner ls (l: Leaf) =
                    if l.Key == "has_technology" then
                        l.Value.ToRawString() :: ls
                    else
                        ls

                t.Leaves |> Seq.fold inner children

        (nodes |> List.collect (foldNode7 fNode))

    let computeSTLData (infoService: unit -> InfoService option) (e: Entity) =
        let scriptedeffectparams = Some(EU4.getScriptedEffectParamsEntity e)
        match infoService() with
        | Some svc ->
            let types, defvars, (effectNodes, _), (triggerNodes, _), eventtargets = svc.BatchFolds(e)
            let referencedtypes = convertReferencedTypes types
            STLComputedData(
                Some referencedtypes,
                Some defvars,
                scriptedeffectparams,
                true,
                Some effectNodes,
                Some triggerNodes,
                Some eventtargets
            )
        | None ->
            STLComputedData(
                None,
                None,
                scriptedeffectparams,
                false,
                None,
                None,
                None
            )

    let computeSTLDataUpdate (infoService: unit -> InfoService option) (e: Entity) (data: STLComputedData) =
        match infoService() with
        | Some svc ->
            let types, defvars, (effectNodes, _), (triggerNodes, _), eventtargets = svc.BatchFolds(e)
            let referencedtypes = convertReferencedTypes types
            data.Referencedtypes <- Some referencedtypes
            data.Definedvariables <- Some defvars
            data.SavedEventTargets <- Some eventtargets
            data.EffectBlocks <- Some effectNodes
            data.TriggerBlocks <- Some triggerNodes
            data.WithRulesData <- true
        | None ->
            data.Referencedtypes <- None
            data.Definedvariables <- None
            data.SavedEventTargets <- None
            data.EffectBlocks <- None
            data.TriggerBlocks <- None
            data.WithRulesData <- false

module Jomini =
    open CWTools.Process
    open CWTools.Process.ProcessCore

    let getScriptedEffectParams (node: Node) =
        let getDollarText (s: string) acc =
            s.Split('$')
            |> Array.mapi (fun i s -> i, s)
            |> Array.fold (fun acc (i, s) -> if i % 2 = 1 then s :: acc else acc) acc
        // let split = s.Split([|'$'|],3)
        // if split.Length = 3 then split.[1]::acc else acc
        let fNode =
            (fun (x: Node) acc ->
                let nodeRes = getDollarText x.Key acc

                x.Leaves
                |> Seq.fold (fun a n -> getDollarText n.Key (getDollarText (n.Value.ToRawString()) a)) nodeRes)

        node |> (foldNode7 fNode) |> List.ofSeq

    let getScriptedEffectParamsEntity (e: Entity) =
        if
            (e.logicalpath.StartsWith("common/scripted_effects", StringComparison.OrdinalIgnoreCase)
             || e.logicalpath.StartsWith("common/scripted_triggers", StringComparison.OrdinalIgnoreCase))
        then
            getScriptedEffectParams e.entity
        else
            []

    let computeJominiData (infoService: unit -> InfoService option) (e: Entity) =
        let scriptedeffectparams = Some(getScriptedEffectParamsEntity e)
        match infoService() with
        | Some svc ->
            let types, defvars, (effectNodes, _), (triggerNodes, _), eventtargets = svc.BatchFolds(e)
            let referencedtypes = convertReferencedTypes types
            JominiComputedData(
                Some referencedtypes,
                Some defvars,
                scriptedeffectparams,
                true,
                Some effectNodes,
                Some triggerNodes,
                Some eventtargets
            )
        | None ->
            JominiComputedData(
                None,
                None,
                scriptedeffectparams,
                false,
                None,
                None,
                None
            )

    let computeJominiDataUpdate (infoService: unit -> InfoService option) (e: Entity) (data: JominiComputedData) =
        match infoService() with
        | Some svc ->
            let types, defvars, (effectNodes, _), (triggerNodes, _), eventtargets = svc.BatchFolds(e)
            let referencedtypes = convertReferencedTypes types
            data.Referencedtypes <- Some referencedtypes
            data.Definedvariables <- Some defvars
            data.SavedEventTargets <- Some eventtargets
            data.EffectBlocks <- Some effectNodes
            data.TriggerBlocks <- Some triggerNodes
            data.WithRulesData <- true
        | None ->
            data.Referencedtypes <- None
            data.Definedvariables <- None
            data.SavedEventTargets <- None
            data.EffectBlocks <- None
            data.TriggerBlocks <- None
            data.WithRulesData <- false
