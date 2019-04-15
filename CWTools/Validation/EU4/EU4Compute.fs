namespace CWTools.Validation.EU4
open CWTools.Games
open CWTools.Rules.Rules


module EU4Compute =

    let computeEU4Data (foldRules : unit -> FoldRules<_> option) (e : Entity) =
        let withRulesData = foldRules().IsSome
        let res = (if foldRules().IsSome then Some ((foldRules().Value.BatchFolds)(e)) else None)
        let referencedtypes, definedvariable, effectBlocks, triggersBlocks =
            match res with
            | Some (r, d, (e, _), (t, _)) -> (Some r, Some d, Some e, Some t)
            | None -> (None, None, None, None)
        // let hastechs = getAllTechPrereqs e
        let scriptedeffectparams = Some (EU4Validation.getScriptedEffectParamsEntity e)

        EU4ComputedData(referencedtypes, definedvariable, scriptedeffectparams, withRulesData, effectBlocks, triggersBlocks)
    let computeEU4DataUpdate (foldRules : unit -> FoldRules<_> option) (e : Entity) (data : EU4ComputedData) =
        let withRulesData = foldRules().IsSome
        let res = (if foldRules().IsSome then Some ((foldRules().Value.BatchFolds)(e)) else None)
        let referencedtypes, definedvariable, effectBlocks, triggersBlocks =
            match res with
            | Some (r, d, (e, _), (t, _)) -> (Some r, Some d, Some e, Some t)
            | None -> (None, None, None, None)

        data.Referencedtypes <- referencedtypes
        data.Definedvariables <- definedvariable
        data.EffectBlocks <- effectBlocks
        data.TriggerBlocks <- triggersBlocks
        data.WithRulesData <- withRulesData

