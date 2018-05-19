namespace CWTools.Validation

open CWTools.Parser.ConfigParser
open CWTools.Process
open CWTools.Utilities.Utils
open CWTools.Validation.ValidationCore
open CWTools.Common.STLConstants
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range

module rec Rules =
    let fst3 (a, _, _) = a
    let snd3 (_, b, _) = b
    let thd3 (_, _, c) = c

    let getValidValues =
        function
        |ValueType.Bool -> Some ["yes"; "no"]
        |ValueType.Enum es -> Some es
        |_ -> None

    type CompletionResponse =
    |Simple of string
    |Snippet of label : string * snippet : string
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

        and applyValueField (vt : ValueType) (leaf : Leaf) =
            match getValidValues vt with
            | Some values -> 
                let value = leaf.Value.ToString()
                if values |> List.exists (fun s -> s == value) then OK else Invalid [invCustom leaf]
            | None -> OK
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
            | Field.ValueField v -> applyValueField v leaf
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

    type CompletionService(effects : Rule list) =
        let rec getRulePath (pos : pos) (stack : string list) (node : Node) =
           match node.Children |> List.tryFind (fun c -> Range.rangeContainsPos c.Position pos) with
           | Some c -> getRulePath pos (c.Key :: stack) c
           | None -> 
                match node.Leaves |> Seq.tryFind (fun l -> Range.rangeContainsPos l.Position pos) with
                | Some l -> l.Key::stack
                | None -> stack

        and getCompletionFromPath (rules : Rule list) (stack : string list) =
            let convRuleToCompletion (rule : Rule) =
                let s, _, f = rule
                let clause (inner : string) = Snippet (inner, (sprintf "%s = {\n\t$0\n}" inner))
                let keyvalue (inner : string) = Snippet (inner, (sprintf "%s = $0" inner))
                match f with
                |Field.ClauseField _ -> clause s
                |Field.EffectField -> clause s
                |Field.ObjectField _ -> keyvalue s
                |Field.ValueField _ -> keyvalue s
                |_ -> Simple s
            let rec findRule (rules : Rule list) (stack) =
                match stack with
                |[] -> rules |> List.map convRuleToCompletion
                |key::rest ->
                    match rules |> List.tryFind (fun (k,_,_) -> k == key) with
                    |Some (_,_,f) ->
                        match f with
                        |Field.EffectField -> findRule effects rest
                        |Field.ClauseField rs -> findRule rs rest
                        |Field.ObjectField et ->
                            match et with
                            |EntityType.ShipSizes -> [Simple "large"; Simple "medium"]
                            |_ -> []
                        |Field.ValueField v -> getValidValues v |> Option.defaultValue [] |> List.map Simple
                        |_ -> []
                    |None -> 
                        rules |> List.map convRuleToCompletion
            findRule rules stack

        let complete (pos : pos) (node : Node) =
            let path = getRulePath pos [] node |> List.rev
            let completion = getCompletionFromPath effects path
            completion 

        member __.Complete(pos : pos, node : Node) = complete pos node
