namespace CWTools.Validation.IR
open CWTools.Games
open CWTools.Rules.Rules

module IRCompute =

    let computeIRData (foldRules : unit -> FoldRules<_> option) (e : Entity) =
        let withRulesData = foldRules().IsSome
        let res = (if foldRules().IsSome then Some ((foldRules().Value.BatchFolds)(e)) else None)
        let referencedtypes, definedvariable, effectBlocks, triggersBlocks =
            match res with
            | Some (r, d, (e, _), (t, _)) -> (Some r, Some d, Some e, Some t)
            | None -> (None, None, None, None)
        // let hastechs = getAllTechPrereqs e
        IRComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks)
    let computeIRDataUpdate (foldRules : unit -> FoldRules<_> option) (e : Entity) (data : IRComputedData) =
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
