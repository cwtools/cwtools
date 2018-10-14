namespace CWTools.Common

open System
open System.ComponentModel.Design
module EU4Constants =
    type Scope =
        |Country
        |Province
        |TradeNode
        |Unit
        |Any
        |InvalidScope
        override x.ToString() =
            match x with
            |TradeNode -> "Trade node"
            |Any -> "Any/Unknown"
            |x -> sprintf "%A" x

    let allScopes = [
            Country;
            Province;
            TradeNode;
            Unit
            ]
    let allScopesSet = allScopes |> Set.ofList
    let parseScope =
        (fun (x : string) ->
        x.ToLower()
        |>
            function
            |"country" -> Scope.Country
            |"province" -> Scope.Province
            |"trade_node" -> Scope.TradeNode
            |"tradenode" -> Scope.TradeNode
            |"unit" -> Scope.Unit
            |"any" -> Scope.Any
            |"all" -> Scope.Any
            |"no_scope" -> Scope.Any
            |x -> eprintfn "Unexpected scope %O" x; Scope.Any) //failwith ("unexpected scope" + x.ToString()))

    let parseScopes =
        function
        |"all" -> allScopes
        |x -> [parseScope x]

    type Effect = Effect<Scope>

    type DocEffect = DocEffect<Scope>
    type ScriptedEffect = ScriptedEffect<Scope>
    type ScopedEffect = ScopedEffect<Scope>
