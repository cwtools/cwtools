namespace CWTools.Process.Scopes

open CWTools.Common.HOI4Constants
open CWTools.Common
open CWTools.Process.Scopes
open CWTools.Process.Scopes.Scopes
open CWTools.Common.NewScope

module HOI4 =
    open CWTools.Utilities.Utils
    open Microsoft.FSharp.Collections.Tagged



    let oneToOneScopes =
        let from i = fun (s, change) -> {s with Scopes = (s.GetFrom i)::s.Scopes}, (false, true)
        let prev = fun (s, change) -> {s with Scopes = s.PopScope}, (false, true)
        [
        "THIS", id;
        "ROOT", fun (s, change) -> {s with Scopes = s.Root::s.Scopes}, (false, true);
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

