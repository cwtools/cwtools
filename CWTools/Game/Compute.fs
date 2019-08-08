module CWTools.Games.Compute

open CWTools.Games
open System
open CWTools.Rules
open CWTools.Common

let computeData (infoService : unit -> InfoService<_> option) (e : Entity) =
    let withRulesData = infoService().IsSome
    let res = (if infoService().IsSome then Some ((infoService().Value.BatchFolds)(e)) else None)
    let referencedtypes, definedvariable, effectBlocks, triggersBlocks =
        match res with
        | Some (r, d, (e, _), (t, _)) -> (Some r, Some d, Some e, Some t)
        | None -> (None, None, None, None)
    // let hastechs = getAllTechPrereqs e
    let referencedtypes = referencedtypes |> Option.map (fun r -> r |> Seq.fold (fun acc (kv) -> acc |> (Map.add kv.Key (kv.Value |> List.ofSeq))) Map.empty )
    ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks)

let computeDataUpdate (infoService : unit -> InfoService<_> option) (e : Entity) (data : ComputedData) =
    let withRulesData = infoService().IsSome
    let res = (if infoService().IsSome then Some ((infoService().Value.BatchFolds)(e)) else None)
    let referencedtypes, definedvariable, effectBlocks, triggersBlocks =
        match res with
        | Some (r, d, (e, _), (t, _)) -> (Some r, Some d, Some e, Some t)
        | None -> (None, None, None, None)

    let referencedtypes = referencedtypes |> Option.map (fun r -> r |> Seq.fold (fun acc (kv) -> acc |> (Map.add kv.Key (kv.Value |> List.ofSeq))) Map.empty )
    data.Referencedtypes <- referencedtypes
    data.Definedvariables <- definedvariable
    data.EffectBlocks <- effectBlocks
    data.TriggerBlocks <- triggersBlocks
    data.WithRulesData <- withRulesData

let computeCK2Data = computeData
let computeCK2DataUpdate = computeDataUpdate
let computeIRData = computeData
let computeIRDataUpdate = computeDataUpdate
let computeHOI4Data = computeData
let computeHOI4DataUpdate = computeDataUpdate
let computeVIC2Data = computeData
let computeVIC2DataUpdate = computeDataUpdate

module EU4 =
    open CWTools.Process
    open CWTools.Process.ProcessCore

    let getScriptedEffectParams (node : Node) =
        let getDollarText (s : string) (acc) =
            let split = s.Split([|'$'|],3)
            if split.Length = 3 then split.[1]::acc else acc
        let fNode = (fun (x:Node) acc ->
                        let nodeRes = getDollarText x.Key acc
                        x.Values |> List.fold (fun a n -> getDollarText n.Key (getDollarText (n.Value.ToRawString()) a)) nodeRes
                        )
        node |> (foldNode7 fNode) |> List.ofSeq

    let getScriptedEffectParamsEntity (e : Entity) =
        if (e.logicalpath.StartsWith("common/scripted_effects", StringComparison.OrdinalIgnoreCase)
                    || e.logicalpath.StartsWith("common/scripted_triggers", StringComparison.OrdinalIgnoreCase))
                then getScriptedEffectParams (e.entity) else []

    let computeEU4Data (infoService : unit -> InfoService<_> option) (e : Entity) =
        let withRulesData = infoService().IsSome
        let res = (if infoService().IsSome then Some ((infoService().Value.BatchFolds)(e)) else None)
        let referencedtypes, definedvariable, effectBlocks, triggersBlocks =
            match res with
            | Some (r, d, (e, _), (t, _)) -> (Some r, Some d, Some e, Some t)
            | None -> (None, None, None, None)
        // let hastechs = getAllTechPrereqs e
        let scriptedeffectparams = Some (getScriptedEffectParamsEntity e)

        let referencedtypes = referencedtypes |> Option.map (fun r -> r |> Seq.fold (fun acc (kv) -> acc |> (Map.add kv.Key (kv.Value |> List.ofSeq))) Map.empty )
        EU4ComputedData(referencedtypes, definedvariable, scriptedeffectparams, withRulesData, effectBlocks, triggersBlocks)
    let computeEU4DataUpdate (infoService : unit -> InfoService<_> option) (e : Entity) (data : EU4ComputedData) =
        let withRulesData = infoService().IsSome
        let res = (if infoService().IsSome then Some ((infoService().Value.BatchFolds)(e)) else None)
        let referencedtypes, definedvariable, effectBlocks, triggersBlocks =
            match res with
            | Some (r, d, (e, _), (t, _)) -> (Some r, Some d, Some e, Some t)
            | None -> (None, None, None, None)

        let referencedtypes = referencedtypes |> Option.map (fun r -> r |> Seq.fold (fun acc (kv) -> acc |> (Map.add kv.Key (kv.Value |> List.ofSeq))) Map.empty )
        data.Referencedtypes <- referencedtypes
        data.Definedvariables <- definedvariable
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
                            | :? TriggerBlock as e -> e :> Node ::acc
                            | :? WeightModifierBlock as e -> e :> Node ::acc
                            |_ -> acc
                                )
        let nodes = e.entity.Children |> List.collect (foldNode7 fNode)
        let fNode =
            fun (t : Node) children ->
                let inner ls (l : Leaf) = if l.Key == "has_technology" then l.Value.ToRawString()::ls else ls
                t.Values |> List.fold inner children
        (nodes |> List.collect (foldNode7 fNode))

    let setFlagMap =
            fun (x : Leaf) ->
                match x.Key with
                |"set_country_flag" -> Some (FlagType.Country, x.Value.ToRawString())
                |"set_planet_flag" -> Some (FlagType.Planet, x.Value.ToRawString())
                |"set_fleet_flag" -> Some (FlagType.Fleet, x.Value.ToRawString())
                |"set_ship_flag" -> Some (FlagType.Ship, x.Value.ToRawString())
                |"set_pop_flag" -> Some (FlagType.Pop, x.Value.ToRawString())
                |"set_global_flag" -> Some (FlagType.Global, x.Value.ToRawString())
                |"set_star_flag" -> Some (FlagType.Star, x.Value.ToRawString())
                |"set_relation_flag" -> Some (FlagType.Relation, x.Value.ToRawString())
                |"set_leader_flag" -> Some (FlagType.Leader, x.Value.ToRawString())
                |"set_ambient_object_flag" -> Some (FlagType.AmbientObject, x.Value.ToRawString())
                |"set_species_flag" -> Some (FlagType.Species, x.Value.ToRawString())
                |"set_megastructure_flag" -> Some (FlagType.Megastructure, x.Value.ToRawString())
                |"set_pop_faction_flag" -> Some (FlagType.PopFaction, x.Value.ToRawString())
                |_ -> None

    let setTimedFlagMap =
        fun (x : Node) ->
            match x.Key with
            |"set_timed_country_flag" -> Some (FlagType.Country, x.TagText "flag")
            |"set_timed_planet_flag" -> Some (FlagType.Planet, x.TagText "flag")
            |"set_timed_fleet_flag" -> Some (FlagType.Fleet, x.TagText "flag")
            |"set_timed_ship_flag" -> Some (FlagType.Ship, x.TagText "flag")
            |"set_timed_pop_flag" -> Some (FlagType.Pop, x.TagText "flag")
            |"set_timed_global_flag" -> Some (FlagType.Global, x.TagText "flag")
            |"set_timed_star_flag" -> Some (FlagType.Star, x.TagText "flag")
            |"set_timed_relation_flag" -> Some (FlagType.Relation, x.TagText "flag")
            |"set_timed_leader_flag" -> Some (FlagType.Leader, x.TagText "flag")
            |"set_timed_ambient_object_flag" -> Some (FlagType.AmbientObject, x.TagText "flag")
            |"set_timed_species_flag" -> Some (FlagType.Species, x.TagText "flag")
            |"set_timed_megastructure_flag" -> Some (FlagType.Megastructure, x.TagText "flag")
            |"set_timed_pop_faction_flag" -> Some (FlagType.PopFaction, x.TagText "flag")
            |_ -> None

    let findAllSetFlags (e : Entity) =
        let fNode = (fun (x : Node) acc ->
                        match x with
                        | :? EffectBlock as e -> e::acc
                        | :? Option as e -> e.AsEffectBlock::acc
                        |_ -> acc
                            )
        let nodes = e.entity.Children |> List.collect (foldNode7 fNode)

        let fNode = (fun (x : Node) acc ->
                    let newAcc = x.Values |> List.fold ((fun a v -> match setFlagMap v with |Some x -> x :: a |None -> a)) acc
                    x.Children |> List.fold ((fun a v -> match setTimedFlagMap v with |Some x -> x :: a |None -> a )) newAcc
                     )

        (nodes |> List.collect (foldNode7 fNode))

        //foldNode7 fNode node |> List.ofSeq |> Map.ofList

    let computeSTLData (infoService : unit -> InfoService<Scope> option) (e : Entity) =
        // eprintfn "csd %s" e.logicalpath
        let withRulesData = infoService().IsSome
        let eventIds = if e.entityType = EntityType.Events then e.entity.Children |> List.choose (fun ee -> if ee.Has "id" then Some (ee.TagText "id") else None) else []
        // let eventIds = if e.entityType = EntityType.Events then e.entity.Children |> List.choose (function | :? Event as e -> Some e.ID |_ -> None) else []
        let setvariables = STLValidation.getEntitySetVariables e
        let setflags = findAllSetFlags e
        let savedeventtargets = STLValidation.findAllSavedEventTargetsInEntity e
        let res = (if infoService().IsSome then Some ((infoService().Value.BatchFolds)(e)) else None)
        let referencedtypes, definedvariable, effectBlocks, triggersBlocks =
            match res with
            | Some (r, d, (e, _), (t, _)) -> (Some r, Some d, Some e, Some t)
            | None -> (None, None, None, None)
        // let referencedtypes = (if infoService().IsSome then Some ((infoService().Value.GetReferencedTypes )(e)) else None)
        // let definedvariable = (if infoService().IsSome then Some ((infoService().Value.GetDefinedVariables )(e)) else None)
        // let effectBlocks, triggersBlocks = (if infoService().IsSome then let (e, t) = ((infoService().Value.GetEffectBlocks )(e)) in Some e, Some t else None, None)
        let hastechs = getAllTechPrereqs e
        let scriptedeffectparams = Some (EU4.getScriptedEffectParamsEntity e)
        let referencedtypes = referencedtypes |> Option.map (fun r -> r |>  List.ofSeq |> List.fold (fun acc (kv) -> acc |> (Map.add kv.Key (kv.Value))) Map.empty )
        let referencedtypes = referencedtypes |> Option.map (fun r -> r |> Map.map (fun k v -> (v.ToArray() |> List.ofSeq)))
        STLComputedData(eventIds, setvariables, setflags, savedeventtargets, referencedtypes, hastechs, definedvariable, withRulesData, effectBlocks, triggersBlocks, scriptedeffectparams)

    let computeSTLDataUpdate (infoService : unit -> InfoService<Scope> option) (e : Entity) (data : STLComputedData) =
        let withRulesData = infoService().IsSome
        let res = (if infoService().IsSome then Some ((infoService().Value.BatchFolds)(e)) else None)
        let referencedtypes, definedvariable, effectBlocks, triggersBlocks =
            match res with
            | Some (r, d, (e, _), (t, _)) -> (Some r, Some d, Some e, Some t)
            | None -> (None, None, None, None)
        let referencedtypes = referencedtypes |> Option.map (fun r -> r |> Seq.fold (fun acc (kv) -> acc |> (Map.add kv.Key (kv.Value |> List.ofSeq))) Map.empty )

        data.Referencedtypes <- referencedtypes
        data.Definedvariables <- definedvariable
        // let effectBlocks, triggersBlocks = (if infoService().IsSome then let (e, t) = ((infoService().Value.GetEffectBlocks )(e)) in Some e, Some t else None, None)
        data.EffectBlocks <- effectBlocks
        data.TriggerBlocks <- triggersBlocks
        data.WithRulesData <- withRulesData