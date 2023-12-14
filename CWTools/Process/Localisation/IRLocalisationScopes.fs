namespace CWTools.Process.Localisation

open CWTools.Process.Scopes
open CWTools.Common
open CWTools.Utilities.Utils
open CWTools.Process.Scopes.Scopes

module IR =



    let scopedLocEffects () =
        [
          // ScopedEffect("Capital", [Scope.Country], Scope.Province, EffectType.Both, defaultDesc, "", true);
          // ScopedEffect("ColonialParent", [Scope.Country], Scope.Country, EffectType.Both, defaultDesc, "", true);
          // ScopedEffect("Culture", [Scope.Country; Scope.Province; Scope.RebelFaction], Scope.Culture, EffectType.Both, defaultDesc, "", true);
          // ScopedEffect("Dynasty", [Scope.Consort; Scope.Monarch; Scope.Heir], Scope.Any, EffectType.Both, defaultDesc, "", true);
          // ScopedEffect("Heir", [Scope.Country], Scope.Heir, EffectType.Both, defaultDesc, "", true);
          // ScopedEffect("Location", [Scope.RebelFaction], Scope.Any, EffectType.Both, defaultDesc, "", true);
          // ScopedEffect("Monarch", [Scope.Country], Scope.Monarch, EffectType.Both, defaultDesc, "", true);
          ScopedEffect(
              "Owner",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
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

    let scopedLocEffectsMap () =
        EffectMap.FromList(scopedLocEffects ())


    let locPrimaryScopes () =
        let from =
            fun (s: ScopeContext, change) ->
                { s with
                    Scopes = scopeManager.AnyScope :: s.Scopes },
                true

        [ "This", id
          "Root", (fun (s: ScopeContext, change) -> { s with Scopes = s.Root :: s.Scopes }, true)
          "From", from //TODO Make it actually use FROM
          "FromFrom", from >> from
          "FromFromFrom", from >> from >> from
          "FromFromFromFrom", from >> from >> from >> from ]

    let locStaticSettings commands variableCommands (localisationLinks: (string * Scope list * Scope) list) =
        let scopedLocEffects =
            localisationLinks
            |> List.map (fun (key, inputs, outputs) ->
                ScopedEffect(key, inputs, outputs, EffectType.Link, defaultDesc, "", true))

        let scopedLocEffectsMap =
            if localisationLinks |> List.isEmpty then
                scopedLocEffectsMap ()
            else
                EffectMap.FromList(scopedLocEffects)

        { questionMarkVariable = true
          usesVariableCommands = false
          parameterVariables = true
          locPrimaryScopes = locPrimaryScopes ()
          scopedLocEffectsMap = scopedLocEffectsMap
          commands = commands
          variableCommands = variableCommands }

// let localisationCommandValidator commands variableCommands = createLegacyLocalisationCommandValidator (locStaticSettings commands variableCommands)
