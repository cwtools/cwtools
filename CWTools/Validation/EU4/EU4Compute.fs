namespace CWTools.Validation.EU4
open CWTools.Common
open CWTools.Games

module EU4Compute =

    let computeEU4Data (foldRules : unit -> ('a * 'b) option) (e : Entity) =
        // let eventIds = if e.entityType = EntityType.Events then e.entity.Children |> List.choose (function | :? Event as e -> Some e.ID |_ -> None) else []
        // let setvariables = STLValidation.getEntitySetVariables e
        // let setflags = findAllSetFlags e
        // let savedeventtargets = STLValidation.findAllSavedEventTargetsInEntity e
        let referencedtypes = (if foldRules().IsSome then Some ((foldRules().Value |> snd )(e)) else None)
        // let hastechs = getAllTechPrereqs e
        EU4ComputedData(referencedtypes)
