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
        static member AnyScope = Scope.Any
        interface IScope<Scope> with
            member this.AnyScope = Scope.Any
            member this.MatchesScope target =
                match this, target with
                |TradeNode, Province -> true
                |this, target -> this = target

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
    type ModifierCategory =
        |Country
        |Province
        |Any

    type Modifier =
        {
            tag : string
            categories : ModifierCategory list
            /// Is this a core modifier or a static modifier?
            core : bool
        }
