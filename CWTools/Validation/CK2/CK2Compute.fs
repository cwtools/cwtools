namespace CWTools.Validation.CK2
open CWTools.Common
open CWTools.Games
open CWTools.Validation.Rules
open CWTools.Games

module CK2Compute =

    let computeCK2Data (foldRules : unit -> FoldRules<_> option) (e : Entity) =
        let withRulesData = foldRules().IsSome
        let res = (if foldRules().IsSome then Some ((foldRules().Value.BatchFolds)(e)) else None)
        let referencedtypes, definedvariable, effectBlocks, triggersBlocks =
            match res with
            | Some (r, d, (e, _), (t, _)) -> (Some r, Some d, Some e, Some t)
            | None -> (None, None, None, None)
        // let hastechs = getAllTechPrereqs e
        CK2ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks)
    let computeCK2DataUpdate (foldRules : unit -> FoldRules<_> option) (e : Entity) (data : CK2ComputedData) =
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
