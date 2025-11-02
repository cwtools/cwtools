module CWTools.Rules.RulesWrapper

[<Sealed>]
type RulesWrapper(rules: RootRule array) =
    let aliases =
        rules
        |> Seq.choose (function
            | AliasRule(a, rs) -> Some(a, rs)
            | _ -> None)
        |> Seq.groupBy fst
        |> Seq.map (fun (k, vs) -> k, vs |> Seq.map snd |> Array.ofSeq)
        |> Map.ofSeq

    let typeRules =
        rules
        |> Array.choose (function
            | TypeRule(k, rs) -> Some(k, rs)
            | _ -> None)

    member this.Aliases = aliases
    member this.TypeRules = typeRules
