namespace CWTools.Process.Scopes

open CWTools.Common
open CWTools.Process.Scopes
open CWTools.Utilities.Utils2

module HOI4 =



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
          // "PREVPREV", prev >> prev;
          // "PREVPREVPREV", prev >> prev >> prev;
          // "PREVPREVPREVPREV", prev >> prev >> prev >> prev
          // "AND", id;
          // "OR", id;
          // "NOR", id;
          // "NOT", id;
          "hidden_effect", id
          "hidden_trigger", id ]

    let oneToOneScopesNames = List.map fst oneToOneScopes

    let changeScope: ChangeScope =
        Scopes.createChangeScope oneToOneScopes (Scopes.simpleVarPrefixFun "var:") true
