namespace CWTools.Process.Scopes

open CWTools.Common.HOI4Constants
open CWTools.Common
open CWTools.Process.Scopes
open CWTools.Process.Scopes.Scopes
open CWTools.Common.NewScope

module HOI4 =
    open CWTools.Utilities.Utils
    open Microsoft.FSharp.Collections.Tagged



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

