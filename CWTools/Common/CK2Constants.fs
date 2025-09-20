namespace CWTools.Common

module CK2Constants =

    let defaultScopes =
        [ "Character", [ "character" ], []
          "Title", [ "title" ], []
          "Province", [ "province" ], []
          "Offmap", [ "offmap" ], []
          "War", [ "war" ], []
          "Siege", [ "siege" ], []
          "Unit", [ "unit" ], []
          "Religion", [ "religion" ], []
          "Culture", [ "culture" ], []
          "Society", [ "society" ], []
          "Artifact", [ "artifact" ], []
          "Bloodline", [ "bloodline" ], []
          "Wonder", [ "wonder" ], [] ]

    let defaultScopeInputs =
        defaultScopes
        |> List.map (fun (n, s, ss) ->
            { NewScope.ScopeInput.name = n
              NewScope.ScopeInput.aliases = s
              NewScope.ScopeInput.isSubscopeOf = ss
              dataTypeName = None })

    let defaultModifiers =
        [ "Character", None, [ "character" ]
          "Province", None, [ "province" ]
          "Unit", None, [ "unit"; "province" ] ]

    let defaultModifiersInputs () =
        defaultModifiers
        |> List.map (fun (n, intID, ss) ->
            { NewScope.ModifierCategoryInput.name = n
              NewScope.ModifierCategoryInput.internalID = intID
              NewScope.ModifierCategoryInput.scopes = ss |> List.map (scopeManager.ParseScope()) })

    let scriptFolders =
        [| "common"
           "common/alternate_start"
           "common/artifact_spawns"
           "common/artifacts"
           "common/bloodlines"
           "common/bookmarks"
           "common/buildings"
           "common/cb_types"
           "common/combat_tactics"
           "common/council_positions"
           "common/council_voting"
           "common/cultures"
           "common/death"
           "common/death_text"
           "common/defines"
           "common/disease"
           "common/dynasties"
           "common/event_modifiers"
           "common/execution_methods"
           "common/game_rules"
           "common/government_flavor"
           "common/governments"
           "common/graphicalculturetypes"
           "common/heir_text"
           "common/holding_types"
           "common/job_actions"
           "common/job_titles"
           "common/landed_titles"
           "common/laws"
           "common/mercenaries"
           "common/minor_titles"
           "common/modifier_definitions"
           "common/nicknames"
           "common/objectives"
           "common/offmap_powers"
           "common/offmap_powers/policies"
           "common/offmap_powers/statuses"
           "common/on_actions"
           "common/opinion_modifiers"
           "common/province_setup"
           "common/religion_features"
           "common/religion_modifiers"
           "common/religions"
           "common/religious_titles"
           "common/retinue_subunits"
           "common/save_conversion"
           "common/scripted_effects"
           "common/scripted_score_values"
           "common/scripted_triggers"
           "common/societies"
           "common/special_troops"
           "common/succession_voting"
           "common/trade_routes"
           "common/traits"
           "common/tributary_types"
           "common/triggered_modifiers"
           "decisions"
           "events"
           "gfx"
           "history"
           "history/characters"
           "history/offmap_powers"
           "history/provinces"
           "history/technology"
           "history/titles"
           "history/wars"
           "interface"
           "interface/coat_of_arms"
           "interface/portrait_offsets"
           "interface/portrait_properties"
           "interface/portrait"
           "localisation"
           "localisation/customizable_localisation"
           "map"
           "map/statics"
           "music"
           "sound"
           "tutorial" |]
