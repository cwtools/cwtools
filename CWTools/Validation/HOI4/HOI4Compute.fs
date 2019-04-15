namespace CWTools.Validation.HOI4
open CWTools.Games
open CWTools.Rules.Rules

module HOI4Compute =

    let computeHOI4Data (foldRules : unit -> FoldRules<_> option) (e : Entity) =
        let withRulesData = foldRules().IsSome
        let res = (if foldRules().IsSome then Some ((foldRules().Value.BatchFolds)(e)) else None)
        let referencedtypes, definedvariable, effectBlocks, triggersBlocks =
            match res with
            | Some (r, d, (e, _), (t, _)) -> (Some r, Some d, Some e, Some t)
            | None -> (None, None, None, None)
        // let hastechs = getAllTechPrereqs e
        HOI4ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks)
    let computeHOI4DataUpdate (foldRules : unit -> FoldRules<_> option) (e : Entity) (data : HOI4ComputedData) =
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
