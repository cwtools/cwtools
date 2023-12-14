namespace CWTools.Process.Localisation

open CWTools.Process.Scopes
open CWTools.Common
open CWTools.Utilities.Utils
open CWTools.Process.Scopes.Scopes

module EU4 =


    let scopedLocEffects () =
        [ ScopedEffect(
              "Capital",
              [ scopeManager.ParseScope () "country" ],
              (scopeManager.ParseScope () "province"),
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "ColonialParent",
              [ scopeManager.ParseScope () "Country" ],
              scopeManager.ParseScope () "Country",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Culture",
              [ scopeManager.ParseScope () "Country"
                scopeManager.ParseScope () "Province"
                scopeManager.ParseScope () "RebelFaction" ],
              scopeManager.ParseScope () "Culture",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Dynasty",
              [ scopeManager.ParseScope () "Consort"
                scopeManager.ParseScope () "Monarch"
                scopeManager.ParseScope () "Heir" ],
              scopeManager.AnyScope,
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Heir",
              [ scopeManager.ParseScope () "Country" ],
              scopeManager.ParseScope () "Heir",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Location",
              [ scopeManager.ParseScope () "RebelFaction" ],
              scopeManager.AnyScope,
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Monarch",
              [ scopeManager.ParseScope () "Country" ],
              scopeManager.ParseScope () "Monarch",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Owner",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Country",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Religion",
              [ scopeManager.ParseScope () "Country"
                scopeManager.ParseScope () "Province"
                scopeManager.ParseScope () "RebelFaction" ],
              scopeManager.ParseScope () "Religion",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "SecondaryReligion",
              [ scopeManager.ParseScope () "Country" ],
              scopeManager.ParseScope () "Religion",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "TradeCompany",
              [ scopeManager.ParseScope () "Country"; scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Country",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Dip_Advisor",
              [ scopeManager.ParseScope () "Country" ],
              scopeManager.ParseScope () "Advisor",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Adm_Advisor",
              [ scopeManager.ParseScope () "Country" ],
              scopeManager.ParseScope () "Advisor",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Mil_Advisor",
              [ scopeManager.ParseScope () "Country" ],
              scopeManager.ParseScope () "Advisor",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Country",
              [ scopeManager.ParseScope () "Country"
                scopeManager.ParseScope () "RebelFaction" ],
              scopeManager.ParseScope () "Country",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Province",
              [ scopeManager.AnyScope ],
              scopeManager.ParseScope () "Province",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Overlord",
              [ scopeManager.ParseScope () "Country" ],
              scopeManager.ParseScope () "Country",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Consort",
              [ scopeManager.ParseScope () "Country" ],
              scopeManager.ParseScope () "Consort",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "NativeCulture",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Culture",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "OriginalReligion",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Religion",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )


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

    let scopedLocEffectsMap () = EffectMap.FromList(scopedLocEffects ())


    let locPrimaryScopes () =
        let from =
            fun (s, change) ->
                { s with
                    Scopes = scopeManager.AnyScope :: s.Scopes },
                true

        [ "This", id
          "Root", (fun (s, change) -> { s with Scopes = s.Root :: s.Scopes }, true)
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
