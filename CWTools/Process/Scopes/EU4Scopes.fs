namespace CWTools.Process.Scopes

open CWTools.Common.EU4Constants
open CWTools.Common
open CWTools.Process.Scopes
open CWTools.Common.NewScope
module EU4 =
    open CWTools.Utilities.Utils
    open Microsoft.FSharp.Collections.Tagged

    // type ScopeContext =
    //     {
    //         Root : Scope
    //         From : Scope list
    //         Scopes : Scope list
    //     }
    //     member this.CurrentScope = match this.Scopes with |[] -> Scope.Any |x::_ -> x
    //     member this.PopScope = match this.Scopes with |[] -> [] |_::xs -> xs
    //     member this.GetFrom i =
    //         if this.From.Length >= i then (this.From.Item (i - 1)) else Scope.Any
    //     interface IScopeContext<Scope> with
    //         member this.CurrentScope = this.CurrentScope
    //         member this.PopScope = this.PopScope
    //         member this.GetFrom i = this.GetFrom i
    //         member this.Root = this.Root
    //         member this.From = this.From
    //         member this.Scopes = this.Scopes

    let defaultContext =
        { Root = scopeManager.AnyScope; From = []; Scopes = [] }
    let noneContext =
        { Root = scopeManager.InvalidScope; From = []; Scopes = [scopeManager.InvalidScope] }
    let defaultDesc = "Scope (/context) switch"


    let scopedEffects() =
        [
            ScopedEffect<Scope>("owner", [scopeManager.ParseScope() "province"], scopeManager.ParseScope() "country", EffectType.Link, defaultDesc, "", true);
            // ScopedEffect("controller", [Scope.Province], Scope.Country, EffectType.Link, defaultDesc, "", true);
            // ScopedEffect("emperor", allScopes, Scope.Country, EffectType.Link, defaultDesc, "", true);
            // // Should be rhs only!
            // ScopedEffect("capital", [Scope.Country; Scope.Province], Scope.Province, EffectType.Link, defaultDesc, "", true);
        ]


    // type ScopeResult =
    //     | NewScope of newScope : ScopeContext<Scope> * ignoreKeys : string list
    //     | WrongScope of command : string * scope : Scope * expected : Scope list
    //     | NotFound

    let oneToOneScopes =
        let from i = fun ((s), change) -> {s with Scopes = (s.GetFrom i)::s.Scopes}, true
        let prev = fun ((s), change) -> {s with Scopes = s.PopScope}, true
        [
        "THIS", id;
        "ROOT", fun ((s), change) -> {s with Scopes = s.Root::s.Scopes}, true;
        "FROM", from 1;
        // "FROMFROM", from 2;
        // "FROMFROMFROM", from 3;
        // "FROMFROMFROMFROM", from 4;
        "PREV", prev;
        // "PREVPREV", prev >> prev;
        // "PREVPREVPREV", prev >> prev >> prev;
        // "PREVPREVPREVPREV", prev >> prev >> prev >> prev
        "AND", id;
        "OR", id;
        "NOR", id;
        "NOT", id;
        "hidden_effect", id;
        "hidden_trigger", id;
    ]
    let oneToOneScopesNames = List.map fst oneToOneScopes
    type EffectMap = Map<string, Effect, InsensitiveStringComparer>
    let changeScope = Scopes.createChangeScope<Scope> oneToOneScopes (Scopes.complexVarPrefixFun "variable:from:" "variable:")

    // let changeScope (skipEffect : bool) (effects : EffectMap) (triggers : EffectMap) (key : string) (source : ScopeContext<Scope>) =
    //     let key = if key.StartsWith("hidden:", StringComparison.OrdinalIgnoreCase) then key.Substring(7) else key
    //     if key.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase) || key.StartsWith("parameter:", StringComparison.OrdinalIgnoreCase) then NewScope ({ Root = source.Root; From = source.From; Scopes = Scope.Any::source.Scopes }, [])
    //     else
    //         let keys = key.Split('.')
    //         let inner ((context : ScopeContext<Scope>), (changed : bool)) (nextKey : string) =
    //             let onetoone = oneToOneScopes |> List.tryFind (fun (k, _) -> k == nextKey)
    //             match onetoone with
    //             | Some (_, f) -> f (context, false), NewScope (f (context, false) |> fst, [])
    //             | None ->
    //                 let effectMatch = effects.TryFind nextKey |> Option.bind (function | :? ScopedEffect as e when (not skipEffect) || e.ScopeOnlyNotEffect  -> Some e |_ -> None)
    //                 let triggerMatch = triggers.TryFind nextKey |> Option.bind (function | :? ScopedEffect as e when (not skipEffect) || e.ScopeOnlyNotEffect -> Some e |_ -> None)
    //                 // let effect = (effects @ triggers)
    //                 //             |> List.choose (function | :? ScopedEffect as e -> Some e |_ -> None)
    //                 //             |> List.tryFind (fun e -> e.Name == nextKey)
    //                 // if skipEffect then (context, false), NotFound else
    //                 match Option.orElse effectMatch triggerMatch with
    //                 | None -> (context, false), NotFound
    //                 | Some e ->
    //                     let possibleScopes = e.Scopes
    //                     let currentScope = context.CurrentScope :> IScope<_>
    //                     let exact = possibleScopes |> List.exists (fun x -> currentScope.MatchesScope x)
    //                     match context.CurrentScope, possibleScopes, exact, e.IsScopeChange with
    //                     | Scope.Any, _, _, true -> ({context with Scopes = e.InnerScope context.CurrentScope::context.Scopes}, true), NewScope ({source with Scopes = e.InnerScope context.CurrentScope::context.Scopes}, e.IgnoreChildren)
    //                     | Scope.Any, _, _, false -> (context, false), NewScope (context, e.IgnoreChildren)
    //                     | _, [], _, _ -> (context, false), NotFound
    //                     | _, _, true, true -> ({context with Scopes = e.InnerScope context.CurrentScope::context.Scopes}, true), NewScope ({source with Scopes = e.InnerScope context.CurrentScope::context.Scopes}, e.IgnoreChildren)
    //                     | _, _, true, false -> (context, false), NewScope (context, e.IgnoreChildren)
    //                     | current, ss, false, _ -> (context, false), WrongScope (nextKey, current, ss)
    //         let inner2 = fun a b -> inner a b |> (fun (c, d) -> c, Some d)
    //         let res = keys |> Array.fold (fun ((c,b), r) k -> match r with |None -> inner2 (c, b) k |Some (NewScope (x, i)) -> inner2 (x, b) k |Some x -> (c,b), Some x) ((source, false), None)// |> snd |> Option.defaultValue (NotFound)
    //         let res2 =
    //             match res with
    //             |(_, _), None -> NotFound
    //             |(_, true), Some r -> r |> function |NewScope (x, i) -> NewScope ({ source with Scopes = x.CurrentScope::source.Scopes }, i) |x -> x
    //             |(_, false), Some r -> r
    //         // let x = res |> function |NewScope x -> NewScope { source with Scopes = x.CurrentScope::source.Scopes } |x -> x
    //         // x
    //         res2

    let scopedLocEffects() = [
        ScopedEffect<Scope>("Capital", [scopeManager.ParseScope() "country"], (scopeManager.ParseScope() "province"), EffectType.Link, defaultDesc, "", true);
        ScopedEffect("ColonialParent", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Culture", [scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Province"; scopeManager.ParseScope() "RebelFaction"], scopeManager.ParseScope() "Culture", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Dynasty", [scopeManager.ParseScope() "Consort"; scopeManager.ParseScope() "Monarch"; scopeManager.ParseScope() "Heir"], scopeManager.AnyScope, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Heir", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Heir", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Location", [scopeManager.ParseScope() "RebelFaction"], scopeManager.AnyScope, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Monarch", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Monarch", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Owner", [scopeManager.ParseScope() "Province"], scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Religion", [scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Province"; scopeManager.ParseScope() "RebelFaction"], scopeManager.ParseScope() "Religion", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("SecondaryReligion", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Religion", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("TradeCompany", [scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Province"], scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Dip_Advisor", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Advisor", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Adm_Advisor", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Advisor", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Mil_Advisor", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Advisor", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Country", [scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "RebelFaction"], scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Province", [scopeManager.AnyScope], scopeManager.ParseScope() "Province", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Overlord", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Consort", [scopeManager.ParseScope() "Country";], scopeManager.ParseScope() "Consort", EffectType.Link, defaultDesc, "", true);


        // ScopedEffect("GetCult", [Scope.Country], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("GetDaughterSon", [Scope.Country], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("GetWifeHusband", [Scope.Country], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Capital", [Scope.Country], Scope.Province, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("ColonialParent", [Scope.Country], Scope.Country, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Culture", [Scope.Country; Scope.Province], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Dynasty", [Scope.Any], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Heir", [Scope.Country], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Location", [Scope.Any], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Monarch", [Scope.Country], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Owner", [Scope.Province], Scope.Country, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Religion", [Scope.Country; Scope.Province], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("SecondaryReligion", [Scope.Country; Scope.Province], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("TradeCompany", [Scope.Country], Scope.Country, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Dip_Advisor", [Scope.Country], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Adm_Advisor", [Scope.Country], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Mil_Advisor", [Scope.Country], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Country", [Scope.Country], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Province", [Scope.Any], Scope.Province, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Overlord", [Scope.Country], Scope.Country, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Consort", [Scope.Country], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("GetCult", [Scope.Country], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("GetDaughterSon", [Scope.Country], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("GetWifeHusband", [Scope.Country], Scope.Any, EffectType.Both, defaultDesc, "", true);
    ]
    let scopedLocEffectsMap() = EffectMap.FromList(InsensitiveStringComparer(), scopedLocEffects() |> List.map (fun se -> se.Name, se :> Effect<Scope>))


    let locPrimaryScopes() =
        let from = fun (s, change) -> {s with Scopes = scopeManager.AnyScope::s.Scopes}, true
        [
        "This", id;
        "Root", fun (s, change) -> {s with Scopes = s.Root::s.Scopes}, true;
        "From", from; //TODO Make it actually use FROM
        "FromFrom", from >> from;
        "FromFromFrom", from >> from >> from;
        "FromFromFromFrom", from >> from >> from >> from;
        ]

    let localisationCommandValidator() = Scopes.createLocalisationCommandValidator (locPrimaryScopes()) (scopedLocEffectsMap())
