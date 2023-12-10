namespace CWTools.Process.Scopes

open CWTools.Common
open CWTools.Process.Scopes

module EU4 =
    open CWTools.Utilities.Utils
    open Microsoft.FSharp.Collections.Tagged

    let defaultDesc = "Scope (/context) switch"


    let scopedEffects () =
        [ ScopedEffect(
              "owner",
              [ scopeManager.ParseScope () "province" ],
              scopeManager.ParseScope () "country",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
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
        let from i =
            fun (s, change) ->
                { s with
                    Scopes = (s.GetFrom i) :: s.Scopes },
                (false, true)

        let prev = fun (s, change) -> { s with Scopes = s.PopScope }, (false, true)

        [ "THIS", id
          "ROOT", (fun (s, change) -> { s with Scopes = s.Root :: s.Scopes }, (false, true))
          "FROM", from 1
          // "FROMFROM", from 2;
          // "FROMFROMFROM", from 3;
          // "FROMFROMFROMFROM", from 4;
          "PREV", prev
          "PREV_PREV", prev >> prev
          // "PREVPREV", prev >> prev;
          // "PREVPREVPREV", prev >> prev >> prev;
          // "PREVPREVPREVPREV", prev >> prev >> prev >> prev
          "AND", id
          "OR", id
          "NOR", id
          "NOT", id
          "hidden_effect", id
          "hidden_trigger", id ]

    let oneToOneScopesNames = List.map fst oneToOneScopes
    type EffectMap = Map<string, Effect, InsensitiveStringComparer>

    let changeScope =
        Scopes.createChangeScope oneToOneScopes (Scopes.complexVarPrefixFun "variable:from:" "variable:") false
