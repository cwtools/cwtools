module CWTools.Rules.RulesWrapper

type RulesWrapper(rules: RootRule list) =
    let aliases =
        rules
        |> List.choose (function
            | AliasRule(a, rs) -> Some(a, rs)
            | _ -> None)
        |> List.groupBy fst
        |> List.map (fun (k, vs) -> k, vs |> List.map snd)
        |> Collections.Map.ofList

    let typeRules =
        rules
        |> List.choose (function
            | TypeRule(k, rs) -> Some(k, rs)
            | _ -> None)

    member this.Aliases with get() = aliases
    member this.TypeRules with get() = typeRules
