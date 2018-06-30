namespace CWTools.Validation.Stellaris
open CWTools.Games
open CWTools.Common.STLConstants
open CWTools.Process.STLProcess
open CWTools.Games.Stellaris.STLLookup
open CWTools.Validation.Rules
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Utilities.Utils
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


    let computeSTLData (foldRules : unit -> FoldRules option) (e : Entity) =
        let eventIds = if e.entityType = EntityType.Events then e.entity.Children |> List.choose (function | :? Event as e -> Some e.ID |_ -> None) else []
        {
            eventids = eventIds
            setvariables = STLValidation.getEntitySetVariables e
            savedeventtargets = STLValidation.findAllSavedEventTargetsInEntity e
            referencedtypes = (if foldRules().IsSome then Some (foldRules().Value.GetReferencedTypes(e)) else None)
            hastechs = getAllTechPrereqs e
        }