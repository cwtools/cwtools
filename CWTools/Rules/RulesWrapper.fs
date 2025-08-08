module CWTools.Rules.RulesWrapper

[<Sealed>]
type RulesWrapper(rules: RootRule list) =
    let aliases =
        rules
        |> Seq.choose (function
            | AliasRule(a, rs) -> Some(a, rs)
            | _ -> None)
        |> Seq.groupBy fst
        |> Seq.map (fun (k, vs) -> k, vs |> Seq.map snd |> List.ofSeq)
        |> Map.ofSeq

    let typeRules =
        rules
        |> List.choose (function
            | TypeRule(k, rs) -> Some(k, rs)
            | _ -> None)

    member this.Aliases = aliases
    member this.TypeRules = typeRules
