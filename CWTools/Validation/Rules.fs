namespace CWTools.Validation

open CWTools.Parser.ConfigParser
open CWTools.Process
open CWTools.Utilities.Utils
open CWTools.Validation.ValidationCore
open CWTools.Common.STLConstants

module rec Rules =
    let fst3 (a, _, _) = a
    let snd3 (_, b, _) = b
    let thd3 (_, _, c) = c

    type RuleApplicator(effects : Rule list) =

        let rec applyClauseField (rules : Rule list) (node : Node) =
            let valueFun (leaf : Leaf) =
                match rules |> List.tryFind (fst3 >> (==) leaf.Key) with
                | Some (_, _, f) -> applyLeafRule f leaf
                | None -> Invalid [invCustom leaf]
            let nodeFun (node : Node) =
                match rules |> List.tryFind (fst3 >> (==) node.Key) with
                | Some (_, _, f) -> applyNodeRule f node
                | None -> Invalid [invCustom node]
            let checkCardinality (node : Node) (rule : Rule) =
                let key, opts, _ = rule
                let leafcount = node.Tags key |> Seq.length
                let childcount = node.Childs key |> Seq.length
                let total = leafcount + childcount
                if opts.min > total then Invalid [invCustom node]
                else if opts.max < total then Invalid [invCustom node]
                else OK
            node.Leaves <&!&> valueFun
            <&&>
            (node.Children <&!&> nodeFun)
            <&&>
            (rules <&!&> checkCardinality node)

        and applyObjectField (entityType : EntityType) (leaf : Leaf) =
            let values =
                match entityType with
                | EntityType.ShipSizes -> ["medium"; "large"]
                | EntityType.StarbaseModules -> ["trafficControl"]
                | EntityType.StarbaseBuilding -> ["crew"]
                | _ -> []
            let value = leaf.Value.ToString()
            if values |> List.exists (fun s -> s == value) then OK else Invalid [invCustom leaf]

        and applyLeafRule (rule : Field) (leaf : Leaf) =
            match rule with
            | Field.ValueField v -> OK
            | Field.ObjectField et -> applyObjectField et leaf
            | Field.TargetField -> OK
            | Field.EffectField -> Invalid [invCustom leaf]
            | Field.ClauseField rs -> Invalid [invCustom leaf]

        and applyNodeRule (rule : Field) (node : Node) =
            match rule with
            | Field.ValueField v -> Invalid [invCustom node]
            | Field.ObjectField et -> Invalid [invCustom node]
            | Field.TargetField -> Invalid [invCustom node]
            | Field.EffectField -> applyClauseField effects node
            | Field.ClauseField rs -> applyClauseField rs node

        member __.ApplyNodeRule(rule, node) = applyNodeRule rule node