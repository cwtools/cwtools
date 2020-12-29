module CWTools.Games.Compute

open CWTools.Games
open System
open CWTools.Rules
open CWTools.Common

let computeData (infoService : unit -> InfoService option) (e : Entity) =
    let withRulesData = infoService().IsSome
    let res = (if infoService().IsSome then Some ((infoService().Value.BatchFolds)(e)) else None)
    let referencedtypes, definedvariable, effectBlocks, triggersBlocks, savedEventTargets =
        match res with
        | Some (r, d, (e, _), (t, _), et) -> (Some r, Some d, Some e, Some t, Some et)
        | None -> (None, None, None, None, None)
    let referencedtypes = referencedtypes |> Option.map (fun r -> r |> Seq.fold (fun acc (kv) -> acc |> (Map.add kv.Key (kv.Value |> List.ofSeq))) Map.empty )
    ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks, savedEventTargets)

let computeDataUpdate (infoService : unit -> InfoService option) (e : Entity) (data : ComputedData) =
    let withRulesData = infoService().IsSome
    let res = (if infoService().IsSome then Some ((infoService().Value.BatchFolds)(e)) else None)
    let referencedtypes, definedvariable, effectBlocks, triggersBlocks, savedEventTargets =
        match res with
        | Some (r, d, (e, _), (t, _), et) -> (Some r, Some d, Some e, Some t, Some et)
        | None -> (None, None, None, None, None)

    let referencedtypes = referencedtypes |> Option.map (fun r -> r |> Seq.fold (fun acc (kv) -> acc |> (Map.add kv.Key (kv.Value |> List.ofSeq))) Map.empty )
    data.Referencedtypes <- referencedtypes
    data.Definedvariables <- definedvariable
    data.SavedEventTargets <- savedEventTargets
    data.EffectBlocks <- effectBlocks
    data.TriggerBlocks <- triggersBlocks
    data.WithRulesData <- withRulesData

let computeCK2Data = computeData
let computeCK2DataUpdate = computeDataUpdate
let computeHOI4Data = computeData
let computeHOI4DataUpdate = computeDataUpdate
let computeVIC2Data = computeData
let computeVIC2DataUpdate = computeDataUpdate

module EU4 =
    open CWTools.Process
    open CWTools.Process.ProcessCore

    let getScriptedEffectParams (node : Node) =
        let getDollarText (s : string) (acc) =
            s.Split('$') |> Array.mapi (fun i s -> i, s) |> Array.fold (fun acc (i, s) -> if i % 2 = 1 then s::acc else acc ) acc
            // let split = s.Split([|'$'|],3)
            // if split.Length = 3 then split.[1]::acc else acc
        let fNode = (fun (x:Node) acc ->
                        let nodeRes = getDollarText x.Key acc
                        x.Values |> List.fold (fun a n -> getDollarText n.Key (getDollarText (n.Value.ToRawString()) a)) nodeRes
                        )
        node |> (foldNode7 fNode) |> List.ofSeq

    let getScriptedEffectParamsEntity (e : Entity) =
        if (e.logicalpath.StartsWith("common/scripted_effects", StringComparison.OrdinalIgnoreCase)
                    || e.logicalpath.StartsWith("common/scripted_triggers", StringComparison.OrdinalIgnoreCase))
                then getScriptedEffectParams (e.entity) else []

    let computeEU4Data (infoService : unit -> InfoService option) (e : Entity) =
        let withRulesData = infoService().IsSome
        let res = (if infoService().IsSome then Some ((infoService().Value.BatchFolds)(e)) else None)
        let referencedtypes, definedvariable, effectBlocks, triggersBlocks, savedEventTargets =
            match res with
            | Some (r, d, (e, _), (t, _), et) -> (Some r, Some d, Some e, Some t, Some et)
            | None -> (None, None, None, None, None)
        // let hastechs = getAllTechPrereqs e
        let scriptedeffectparams = Some (getScriptedEffectParamsEntity e)

        let referencedtypes = referencedtypes |> Option.map (fun r -> r |> Seq.fold (fun acc (kv) -> acc |> (Map.add kv.Key (kv.Value |> List.ofSeq))) Map.empty )
        EU4ComputedData(referencedtypes, definedvariable, scriptedeffectparams, withRulesData, effectBlocks, triggersBlocks, savedEventTargets)
    let computeEU4DataUpdate (infoService : unit -> InfoService option) (e : Entity) (data : EU4ComputedData) =
        let withRulesData = infoService().IsSome
        let res = (if infoService().IsSome then Some ((infoService().Value.BatchFolds)(e)) else None)
        let referencedtypes, definedvariable, effectBlocks, triggersBlocks, savedEventTargets =
            match res with
            | Some (r, d, (e, _), (t, _), et) -> (Some r, Some d, Some e, Some t, Some et)
            | None -> (None, None, None, None, None)

        let referencedtypes = referencedtypes |> Option.map (fun r -> r |> Seq.fold (fun acc (kv) -> acc |> (Map.add kv.Key (kv.Value |> List.ofSeq))) Map.empty )
        data.Referencedtypes <- referencedtypes
        data.Definedvariables <- definedvariable
        data.SavedEventTargets <- savedEventTargets
        data.EffectBlocks <- effectBlocks
        data.TriggerBlocks <- triggersBlocks
        data.WithRulesData <- withRulesData

module STL =
    open CWTools.Process
    open CWTools.Process.ProcessCore
    open CWTools.Process.STLProcess
    open CWTools.Games.Stellaris.STLLookup
    open CWTools.Utilities.Utils
    open CWTools.Common.STLConstants
    open CWTools.Validation.Stellaris

    let getAllTechPrereqs (e : Entity) =
        let fNode = (fun (x : Node) acc ->
                            match x with
                            |_ -> acc
                                )
        let nodes = e.entity.Children |> List.collect (foldNode7 fNode)
        let fNode =
            fun (t : Node) children ->
                let inner ls (l : Leaf) = if l.Key == "has_technology" then l.Value.ToRawString()::ls else ls
                t.Values |> List.fold inner children
        (nodes |> List.collect (foldNode7 fNode))

    let computeSTLData (infoService : unit -> InfoService option) (e : Entity) =
        // eprintfn "csd %s" e.logicalpath
        let withRulesData = infoService().IsSome
        let eventIds = if e.entityType = EntityType.Events then e.entity.Children |> List.choose (fun ee -> if ee.Has "id" then Some (ee.TagText "id") else None) else []
        // let eventIds = if e.entityType = EntityType.Events then e.entity.Children |> List.choose (function | :? Event as e -> Some e.ID |_ -> None) else []
        let setvariables = STLValidation.getEntitySetVariables e
        let res = (if infoService().IsSome then Some ((infoService().Value.BatchFolds)(e)) else None)
        let referencedtypes, definedvariable, effectBlocks, triggersBlocks, savedEventTargets =
            match res with
            | Some (r, d, (e, _), (t, _), et) -> (Some r, Some d, Some e, Some t, Some et)
            | None -> (None, None, None, None, None)
        // let referencedtypes = (if infoService().IsSome then Some ((infoService().Value.GetReferencedTypes )(e)) else None)
        // let definedvariable = (if infoService().IsSome then Some ((infoService().Value.GetDefinedVariables )(e)) else None)
        // let effectBlocks, triggersBlocks = (if infoService().IsSome then let (e, t) = ((infoService().Value.GetEffectBlocks )(e)) in Some e, Some t else None, None)
        let hastechs = getAllTechPrereqs e
        let scriptedeffectparams = Some (EU4.getScriptedEffectParamsEntity e)
        let referencedtypes = referencedtypes |> Option.map (fun r -> r |>  List.ofSeq |> List.fold (fun acc (kv) -> acc |> (Map.add kv.Key (kv.Value))) Map.empty )
        let referencedtypes = referencedtypes |> Option.map (fun r -> r |> Map.map (fun k v -> (v.ToArray() |> List.ofSeq)))
        STLComputedData(eventIds, setvariables, referencedtypes, hastechs, definedvariable, withRulesData, effectBlocks, triggersBlocks, scriptedeffectparams, savedEventTargets)

    let computeSTLDataUpdate (infoService : unit -> InfoService option) (e : Entity) (data : STLComputedData) =
        let withRulesData = infoService().IsSome
        let res = (if infoService().IsSome then Some ((infoService().Value.BatchFolds)(e)) else None)
        let referencedtypes, definedvariable, effectBlocks, triggersBlocks, savedEventTargets =
            match res with
            | Some (r, d, (e, _), (t, _), et) -> (Some r, Some d, Some e, Some t, Some et)
            | None -> (None, None, None, None, None)
        let referencedtypes = referencedtypes |> Option.map (fun r -> r |> Seq.fold (fun acc (kv) -> acc |> (Map.add kv.Key (kv.Value |> List.ofSeq))) Map.empty )

        data.Referencedtypes <- referencedtypes
        data.Definedvariables <- definedvariable
        data.SavedEventTargets <- savedEventTargets
        // let effectBlocks, triggersBlocks = (if infoService().IsSome then let (e, t) = ((infoService().Value.GetEffectBlocks )(e)) in Some e, Some t else None, None)
        data.EffectBlocks <- effectBlocks
        data.TriggerBlocks <- triggersBlocks
        data.WithRulesData <- withRulesData

module Jomini =
    open CWTools.Process
    open CWTools.Process.ProcessCore

    let getScriptedEffectParams (node : Node) =
        let getDollarText (s : string) (acc) =
            s.Split('$') |> Array.mapi (fun i s -> i, s) |> Array.fold (fun acc (i, s) -> if i % 2 = 1 then s::acc else acc ) acc
            // let split = s.Split([|'$'|],3)
            // if split.Length = 3 then split.[1]::acc else acc
        let fNode = (fun (x:Node) acc ->
                        let nodeRes = getDollarText x.Key acc
                        x.Values |> List.fold (fun a n -> getDollarText n.Key (getDollarText (n.Value.ToRawString()) a)) nodeRes
                        )
        node |> (foldNode7 fNode) |> List.ofSeq

    let getScriptedEffectParamsEntity (e : Entity) =
        if (e.logicalpath.StartsWith("common/scripted_effects", StringComparison.OrdinalIgnoreCase)
                    || e.logicalpath.StartsWith("common/scripted_triggers", StringComparison.OrdinalIgnoreCase))
                then getScriptedEffectParams (e.entity) else []

    let computeJominiData (infoService : unit -> InfoService option) (e : Entity) =
        let withRulesData = infoService().IsSome
        let res = (if infoService().IsSome then Some ((infoService().Value.BatchFolds)(e)) else None)
        let referencedtypes, definedvariable, effectBlocks, triggersBlocks, savedEventTargets =
            match res with
            | Some (r, d, (e, _), (t, _), et) -> (Some r, Some d, Some e, Some t, Some et)
            | None -> (None, None, None, None, None)
        // let hastechs = getAllTechPrereqs e
        let scriptedeffectparams = Some (getScriptedEffectParamsEntity e)

        let referencedtypes = referencedtypes |> Option.map (fun r -> r |> Seq.fold (fun acc (kv) -> acc |> (Map.add kv.Key (kv.Value |> List.ofSeq))) Map.empty )
        JominiComputedData(referencedtypes, definedvariable, scriptedeffectparams, withRulesData, effectBlocks, triggersBlocks, savedEventTargets)
    let computeJominiDataUpdate (infoService : unit -> InfoService option) (e : Entity) (data : JominiComputedData) =
        let withRulesData = infoService().IsSome
        let res = (if infoService().IsSome then Some ((infoService().Value.BatchFolds)(e)) else None)
        let referencedtypes, definedvariable, effectBlocks, triggersBlocks, savedEventTargets =
            match res with
            | Some (r, d, (e, _), (t, _), et) -> (Some r, Some d, Some e, Some t, Some et)
            | None -> (None, None, None, None, None)

        let referencedtypes = referencedtypes |> Option.map (fun r -> r |> Seq.fold (fun acc (kv) -> acc |> (Map.add kv.Key (kv.Value |> List.ofSeq))) Map.empty )
        data.Referencedtypes <- referencedtypes
        data.Definedvariables <- definedvariable
        data.SavedEventTargets <- savedEventTargets
        data.EffectBlocks <- effectBlocks
        data.TriggerBlocks <- triggersBlocks
        data.WithRulesData <- withRulesData
