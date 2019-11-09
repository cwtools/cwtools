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

