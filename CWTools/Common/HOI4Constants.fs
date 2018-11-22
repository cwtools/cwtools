namespace CWTools.Common

open System
open System.ComponentModel.Design
module HOI4Constants =
    type Scope =
        |State
        |Country
        |UnitLeader
        |Any
        |InvalidScope
        override x.ToString() =
            match x with
            |UnitLeader -> "Unit leader"
            |Any -> "Any/Unknown"
            |x -> sprintf "%A" x
        static member AnyScope = Scope.Any
        interface IScope<Scope> with
            member this.AnyScope = Scope.Any
            member this.MatchesScope target = this = target

    let allScopes = [
        Scope.State;
        Scope.Country;
        Scope.UnitLeader;
            ]
    let allScopesSet = allScopes |> Set.ofList
    let parseScope =
        (fun (x : string) ->
        x.ToLower()
        |>
            function
            |"state" -> Scope.State
            |"country" -> Scope.Country
            |"unit leader" -> Scope.UnitLeader
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
        |State
        |Country
        |Any

    type Modifier =
        {
            tag : string
            categories : ModifierCategory list
            /// Is this a core modifier or a static modifier?
            core : bool
        }
