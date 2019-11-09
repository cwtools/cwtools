namespace CWTools.Process.Scopes

open CWTools.Common.HOI4Constants
open CWTools.Common
open CWTools.Process.Scopes
open CWTools.Common.NewScope
module HOI4 =
    open CWTools.Utilities.Utils
    open Microsoft.FSharp.Collections.Tagged

    let defaultDesc = "Scope (/context) switch"


    let scopedEffects() =
        [
            ScopedEffect("owner", [scopeManager.ParseScope() "UnitLeader"; scopeManager.ParseScope() "State"], scopeManager.ParseScope() "Country", Link, defaultDesc, "", true);
            ScopedEffect("controller", [scopeManager.ParseScope() "State"], scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
            ScopedEffect("capital", [scopeManager.ParseScope() "State"], scopeManager.ParseScope() "State", EffectType.Link, defaultDesc, "", true);
            ScopedEffect("global", scopeManager.AllScopes, scopeManager.ParseScope() "InvalidScope", EffectType.Link, defaultDesc, "", true);
            // ScopedEffect("emperor", allScopes, scopeManager.ParseScope() "Country", EffectType.Both, defaultDesc, "", true);
        ]


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
        // "AND", id;
        // "OR", id;
        // "NOR", id;
        // "NOT", id;
        "hidden_effect", id;
        "hidden_trigger", id;
    ]
    let oneToOneScopesNames = List.map fst oneToOneScopes
    type EffectMap = Map<string, Effect, InsensitiveStringComparer>
    let changeScope = Scopes.createChangeScope oneToOneScopes (Scopes.simpleVarPrefixFun "var:") true

    let scopedLocEffects() = [
        // ScopedEffect("Capital", [Scope.Country], Scope.Province, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("ColonialParent", [Scope.Country], Scope.Country, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Culture", [Scope.Country; Scope.Province], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Dynasty", [Scope.Any], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Heir", [Scope.Country], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Location", [Scope.Any], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Monarch", [Scope.Country], Scope.Any, EffectType.Both, defaultDesc, "", true);
        ScopedEffect("capital", [scopeManager.ParseScope() "State"], scopeManager.ParseScope() "State", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("owner", [scopeManager.ParseScope() "UnitLeader"; scopeManager.ParseScope() "State"], scopeManager.ParseScope() "Country", Link, defaultDesc, "", true);
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
        // ScopedEffect("GetCult", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Any", EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("GetDaughterSon", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Any", EffectType.Both, defaultDesc, "", true);
        ScopedEffect("GetWifeHusband", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Any", EffectType.Link, defaultDesc, "", true);
    ]
    let scopedLocEffectsMap() = EffectMap.FromList(InsensitiveStringComparer(), scopedLocEffects() |> List.map (fun se -> se.Name, se :> Effect))


    let locPrimaryScopes() =
        let from = fun (s, change) -> {s with Scopes = scopeManager.AnyScope::s.Scopes}, true
        let prev = fun ((s), change) -> {s with Scopes = s.PopScope}, true
        [
        "This", id;
        "Root", fun (s, change) -> {s with Scopes = s.Root::s.Scopes}, true;
        "Prev", prev
        "From", from; //TODO Make it actually use FROM
        "FromFrom", from >> from;
        "FromFromFrom", from >> from >> from;
        "FromFromFromFrom", from >> from >> from >> from;
        ]

    let localisationCommandValidator() = Scopes.createLocalisationCommandValidator (locPrimaryScopes()) (scopedLocEffectsMap())
