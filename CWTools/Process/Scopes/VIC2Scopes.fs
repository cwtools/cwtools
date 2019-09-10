namespace CWTools.Process.Scopes

open CWTools.Common.VIC2Constants
open CWTools.Common
open CWTools.Process.Scopes
open CWTools.Common.NewScope
module VIC2 =
    open CWTools.Utilities.Utils
    open Microsoft.FSharp.Collections.Tagged

    let defaultDesc = "Scope (/context) switch"


    let scopedEffects =
        [
            // // To title
            // ScopedEffect("primary_title", [Scope.Character], Scope.Title, EffectType.Both, defaultDesc, "", true)
            // // To character
            // ScopedEffect("mother", [Scope.Character], Scope.Character, EffectType.Both, defaultDesc, "", true)
            // ScopedEffect("mother_even_if_dead", [Scope.Character], Scope.Character, EffectType.Both, defaultDesc, "", true)
            // ScopedEffect("father", [Scope.Character], Scope.Character, EffectType.Both, defaultDesc, "", true)
            // ScopedEffect("father_even_if_dead", [Scope.Character], Scope.Character, EffectType.Both, defaultDesc, "", true)
            // ScopedEffect("killer", [Scope.Character], Scope.Character, EffectType.Both, defaultDesc, "", true)
            // ScopedEffect("liege", [Scope.Character], Scope.Character, EffectType.Both, defaultDesc, "", true)
            // ScopedEffect("liege_before_war", [Scope.Character], Scope.Character, EffectType.Both, defaultDesc, "", true)
            // ScopedEffect("top_liege", [Scope.Character], Scope.Character, EffectType.Both, defaultDesc, "", true)
            // // To province
            // ScopedEffect("capital_scope", [Scope.Character; Scope.Title], Scope.Province, EffectType.Both, defaultDesc, "", true)
            // ScopedEffect("owner", [Scope.Province], Scope.Character, EffectType.Both, defaultDesc, "", true);
        ]


    // type ScopeResult =
    //     | NewScope of newScope : ScopeContext<Scope> * ignoreKeys : string list
    //     | WrongScope of command : string * scope : Scope * expected : Scope list
    //     | NotFound

    let oneToOneScopes =
        let from i = fun ((s), change) -> {s with Scopes = (s.GetFrom i)::s.Scopes}, true
        let prev = fun ((s), change) -> {s with Scopes = s.PopScope}, true
        let root = fun ((s), change) -> {s with Scopes = s.Root::s.Scopes}, true
        [
        "THIS", id;
        "ROOT", root;
        "ROOT_FROM", root >> from 1;
        "ROOT_FROMFROM", root >> from 2;
        "ROOT_FROMFROMFROM", root >> from 3;
        "ROOT_FROMFROMFROMFROM", root >> from 4;
        "FROM", from 1;
        "FROMFROM", from 2;
        "FROMFROMFROM", from 3;
        "FROMFROMFROMFROM", from 4;
        "PREV", prev;
        "PREVPREV", prev >> prev;
        "PREVPREVPREV", prev >> prev >> prev;
        "PREVPREVPREVPREV", prev >> prev >> prev >> prev
    ]
    let oneToOneScopesNames = List.map fst oneToOneScopes
    type EffectMap = Map<string, Effect, InsensitiveStringComparer>
    let changeScope = Scopes.createJominiChangeScope oneToOneScopes (Scopes.complexVarPrefixFun "variable:from:" "variable:")


    let scopedLocEffects() = [
         ScopedEffect("Capital", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Province", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("ColonialParent", [Scope.Country], Scope.Country, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Culture", [Scope.Country; Scope.Province; Scope.RebelFaction], Scope.Culture, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Dynasty", [Scope.Consort; Scope.Monarch; Scope.Heir], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Heir", [Scope.Country], Scope.Heir, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Location", [Scope.RebelFaction], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Monarch", [Scope.Country], Scope.Monarch, EffectType.Both, defaultDesc, "", true);
        //ScopedEffect("Owner", [Scope.Province], Scope.Character, EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("Religion", [Scope.Country; Scope.Province; Scope.RebelFaction], Scope.Religion, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("SecondaryReligion", [Scope.Country], Scope.Religion, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("TradeCompany", [Scope.Country; Scope.Province], Scope.Country, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Dip_Advisor", [Scope.Country], Scope.Advisor, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Adm_Advisor", [Scope.Country], Scope.Advisor, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Mil_Advisor", [Scope.Country], Scope.Advisor, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Country", [Scope.Country; Scope.RebelFaction], Scope.Country, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Province", [Scope.Any], Scope.Province, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Overlord", [Scope.Country], Scope.Country, EffectType.Both, defaultDesc, "", true);
    ]
    let scopedLocEffectsMap() = EffectMap.FromList(InsensitiveStringComparer(), scopedLocEffects() |> List.map (fun se -> se.Name, se :> Effect))


    let locPrimaryScopes() =
        let from = fun (s : ScopeContext, change) -> {s with Scopes = scopeManager.AnyScope::s.Scopes}, true
        [
        "This", id;
        "Root", fun (s : ScopeContext, change) -> {s with Scopes = s.Root::s.Scopes}, true;
        "From", from; //TODO Make it actually use FROM
        "FromFrom", from >> from;
        "FromFromFrom", from >> from >> from;
        "FromFromFromFrom", from >> from >> from >> from;
        ]

    let localisationCommandValidator() = Scopes.createLocalisationCommandValidator (locPrimaryScopes()) (scopedLocEffectsMap())
