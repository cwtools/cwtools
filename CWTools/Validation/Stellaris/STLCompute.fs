namespace CWTools.Validation.Stellaris
open CWTools.Games
open CWTools.Common.STLConstants
open CWTools.Process.STLProcess
open CWTools.Games.Stellaris.STLLookup
open CWTools.Validation.Rules
module STLCompute =
    let computeSTLData (foldRules : unit -> FoldRules option) (e : Entity) =
        let eventIds = if e.entityType = EntityType.Events then e.entity.Children |> List.choose (function | :? Event as e -> Some e.ID |_ -> None) else []
        {
            eventids = eventIds
            setvariables = STLValidation.getEntitySetVariables e
            savedeventtargets = STLValidation.findAllSavedEventTargetsInEntity e
            referencedtypes = (if foldRules().IsSome then Some (foldRules().Value.GetReferencedTypes(e)) else None)
        }