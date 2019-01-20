namespace CWTools.Validation.Stellaris
open CWTools.Games
open CWTools.Common.STLConstants
open CWTools.Process.STLProcess
open CWTools.Games.Stellaris.STLLookup
open CWTools.Validation.Rules
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Utilities.Utils
open CWTools.Games
module STLCompute =

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

    let computeSTLData (foldRules : unit -> FoldRules<Scope> option) (e : Entity) =
        let withRulesData = foldRules().IsSome
        let eventIds = if e.entityType = EntityType.Events then e.entity.Children |> List.choose (fun ee -> if ee.Has "id" then Some (ee.TagText "id") else None) else []
        // let eventIds = if e.entityType = EntityType.Events then e.entity.Children |> List.choose (function | :? Event as e -> Some e.ID |_ -> None) else []
        let setvariables = STLValidation.getEntitySetVariables e
        let setflags = findAllSetFlags e
        let savedeventtargets = STLValidation.findAllSavedEventTargetsInEntity e
        let referencedtypes = (if foldRules().IsSome then Some ((foldRules().Value.GetReferencedTypes )(e)) else None)
        let definedvariable = (if foldRules().IsSome then Some ((foldRules().Value.GetDefinedVariables )(e)) else None)
        let effectBlocks, triggersBlocks = (if foldRules().IsSome then let (e, t) = ((foldRules().Value.GetEffectBlocks )(e)) in Some e, Some t else None, None)
        let hastechs = getAllTechPrereqs e
        STLComputedData(eventIds, setvariables, setflags, savedeventtargets, referencedtypes, hastechs, definedvariable, withRulesData, effectBlocks, triggersBlocks)

    let computeSTLDataUpdate (foldRules : unit -> FoldRules<Scope> option) (e : Entity) (data : STLComputedData) =
        let withRulesData = foldRules().IsSome
        data.Referencedtypes <- (if foldRules().IsSome then Some ((foldRules().Value.GetReferencedTypes)(e)) else None)
        data.Definedvariables <- (if foldRules().IsSome then Some ((foldRules().Value.GetDefinedVariables )(e)) else None)
        let effectBlocks, triggersBlocks = (if foldRules().IsSome then let (e, t) = ((foldRules().Value.GetEffectBlocks )(e)) in Some e, Some t else None, None)
        data.EffectBlocks <- effectBlocks
        data.TriggerBlocks <- triggersBlocks
        data.WithRulesData <- withRulesData