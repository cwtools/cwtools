namespace CWTools.Process.Localisation

open CWTools.Process.Scopes
open CWTools.Common
open CWTools.Process.Scopes.Scopes

module CK2 =
    let scopedLocEffects () =
        [ ScopedEffect(
              "Owner",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Betrothed",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Culture",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Culture",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Employer",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Father",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "FatherOfUnborn",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "GetLiege",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Governor",
              [ scopeManager.ParseScope () "Offmap" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "GovernorTitle",
              [ scopeManager.ParseScope () "Offmap" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "GrandMaster",
              [ scopeManager.ParseScope () "Society" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Guardian",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Heir",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Holder",
              [ scopeManager.ParseScope () "Title" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "job_chancellor",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "job_marshal",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "job_spiritual",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "job_spymaster",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "job_treasurer",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Killer",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Liege",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Location",
              [ scopeManager.ParseScope () "Title" ],
              scopeManager.ParseScope () "Province",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Lover",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Mother",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "NextGrandMaster",
              [ scopeManager.ParseScope () "Society" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Offmap",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Offmap",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "OldestChild",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "OriginalOwner",
              [ scopeManager.ParseScope () "Artifact"; scopeManager.ParseScope () "Wonder" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Owner",
              [ scopeManager.ParseScope () "Artifact"; scopeManager.ParseScope () "Wonder" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "ParentReligion",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Religion",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "PlotTarget",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "PlotTargetTitle",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Title",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "PrevRuler",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Province",
              [ scopeManager.ParseScope () "Wonder" ],
              scopeManager.ParseScope () "Province",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "PrimaryTitle",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Title",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "RealFather",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Realm",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Title",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Regent",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Reincarnation",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Religion",
              [ scopeManager.ParseScope () "Province"
                scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Religion",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Ruler",
              [ scopeManager.ParseScope () "Offmap" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "SecretReligion",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Religion",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Society",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Society",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Spouse",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "SupportedClaimant",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "SupportedClaimantTitle",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Title",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "ThePope",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "ThirdPartyCharacter",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "ThirdPartyTitle",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Title",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Title",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Title",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "TopLiege",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "TrueFather",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "TrueReligion",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Religion",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Twin",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Wonder",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Wonder",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )

          // TODO: Check these as
          ScopedEffect(
              "Actor",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "ActorExtra",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "AdultExtra",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "ChildExtra",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Clan",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "County",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Capital",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Province",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Duchy",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Government",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.AnyScope,
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Host",
              [ scopeManager.ParseScope () "Title" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Kingdom",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Title",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Player",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Recipient",
              [ scopeManager.ParseScope () "Character" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "RecipientExtra",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "RelHead",
              [ scopeManager.ParseScope () "Religion" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "SeaZone",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Character",
              EffectType.Link,
              defaultDesc,
              "",
              true
          )
          ScopedEffect(
              "Siege",
              [ scopeManager.ParseScope () "Province" ],
              scopeManager.ParseScope () "Siege",
              EffectType.Link,
              defaultDesc,
              "",
              true
          ) ]

    let scopedLocEffectsMap () = EffectMap.FromList(scopedLocEffects ())


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
