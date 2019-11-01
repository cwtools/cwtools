namespace CWTools.Process.Scopes

open CWTools.Common.EU4Constants
open CWTools.Common
open CWTools.Process.Scopes
open CWTools.Common.NewScope
module EU4 =
    open CWTools.Utilities.Utils
    open Microsoft.FSharp.Collections.Tagged

    let defaultDesc = "Scope (/context) switch"


    let scopedEffects() =
        [
            ScopedEffect("owner", [scopeManager.ParseScope() "province"], scopeManager.ParseScope() "country", EffectType.Link, defaultDesc, "", true);
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
    let changeScope = Scopes.createChangeScope oneToOneScopes (Scopes.complexVarPrefixFun "variable:from:" "variable:") false


    let scopedLocEffects() = [
        ScopedEffect("Capital", [scopeManager.ParseScope() "country"], (scopeManager.ParseScope() "province"), EffectType.Link, defaultDesc, "", true);
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
    let scopedLocEffectsMap() = EffectMap.FromList(InsensitiveStringComparer(), scopedLocEffects() |> List.map (fun se -> se.Name, se :> Effect))


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
