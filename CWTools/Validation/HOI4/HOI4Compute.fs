namespace CWTools.Validation.HOI4
open CWTools.Common
open CWTools.Games
open CWTools.Validation.Rules

module HOI4Compute =

    let computeHOI4Data (foldRules : unit -> FoldRules<_> option) (e : Entity) =
        // let eventIds = if e.entityType = EntityType.Events then e.entity.Children |> List.choose (function | :? Event as e -> Some e.ID |_ -> None) else []
        // let setvariables = STLValidation.getEntitySetVariables e
        // let setflags = findAllSetFlags e
        // let savedeventtargets = STLValidation.findAllSavedEventTargetsInEntity e
        let withRulesData = foldRules().IsSome
        let referencedtypes = (if foldRules().IsSome then Some ((foldRules().Value.GetReferencedTypes)(e)) else None)
        let definedvariable = (if foldRules().IsSome then Some ((foldRules().Value.GetDefinedVariables )(e)) else None)
        // let hastechs = getAllTechPrereqs e
        HOI4ComputedData(referencedtypes, definedvariable, withRulesData)
    let computeHOI4DataUpdate (foldRules : unit -> FoldRules<_> option) (e : Entity) (data : HOI4ComputedData) =
        let withRulesData = foldRules().IsSome
        data.Referencedtypes <- (if foldRules().IsSome then Some ((foldRules().Value.GetReferencedTypes)(e)) else None)
        data.Definedvariables <- (if foldRules().IsSome then Some ((foldRules().Value.GetDefinedVariables )(e)) else None)
        data.WithRulesData <- withRulesData
