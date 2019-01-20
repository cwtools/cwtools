namespace CWTools.Validation.HOI4
open CWTools.Common
open CWTools.Games
open CWTools.Validation.Rules
open CWTools.Games

module HOI4Compute =

    let computeHOI4Data (foldRules : unit -> FoldRules<_> option) (e : Entity) =
        let withRulesData = foldRules().IsSome
        let referencedtypes = (if foldRules().IsSome then Some ((foldRules().Value.GetReferencedTypes)(e)) else None)
        let definedvariable = (if foldRules().IsSome then Some ((foldRules().Value.GetDefinedVariables )(e)) else None)
        let effectBlocks, triggersBlocks = (if foldRules().IsSome then let (e, t) = ((foldRules().Value.GetEffectBlocks )(e)) in Some e, Some t else None, None)
        // let hastechs = getAllTechPrereqs e
        HOI4ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks)
    let computeHOI4DataUpdate (foldRules : unit -> FoldRules<_> option) (e : Entity) (data : HOI4ComputedData) =
        let withRulesData = foldRules().IsSome
        data.Referencedtypes <- (if foldRules().IsSome then Some ((foldRules().Value.GetReferencedTypes)(e)) else None)
        data.Definedvariables <- (if foldRules().IsSome then Some ((foldRules().Value.GetDefinedVariables )(e)) else None)
        let effectBlocks, triggersBlocks = (if foldRules().IsSome then let (e, t) = ((foldRules().Value.GetEffectBlocks )(e)) in Some e, Some t else None, None)
        data.EffectBlocks <- effectBlocks
        data.TriggerBlocks <- triggersBlocks
        data.WithRulesData <- withRulesData
