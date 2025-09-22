namespace CWTools.Common



module EU4Constants =
    let defaultScopes =
        [ "Country", [ "country" ], []
          "Province", [ "province" ], []
          "Trade Node", [ "trade_node"; "tradenode" ], [ "province" ]
          "Unit", [ "unit" ], []
          "Monarch", [ "monarch" ], []
          "Heir", [ "heir" ], []
          "Consort", [ "consort" ], []
          "Rebel Faction", [ "rebel_faction" ], []
          "Religion", [ "religion" ], []
          "Culture", [ "culture" ], []
          "Advisor", [ "advisor" ], [] ]

    let defaultScopeInputs () =
        defaultScopes
        |> List.map (fun (n, s, ss) ->
            { ScopeInput.name = n
              ScopeInput.aliases = s
              ScopeInput.isSubscopeOf = ss
              dataTypeName = None })

    let defaultModifiers =
        [ "Country", None, [ "country" ]; "Province", None, [ "province" ] ]

    let defaultModifiersInputs () =
        defaultModifiers
        |> List.map (fun (n, intID, ss) ->
            { NewScope.ModifierCategoryInput.name = n
              NewScope.ModifierCategoryInput.internalID = intID
              NewScope.ModifierCategoryInput.scopes = ss |> List.map (scopeManager.ParseScope()) })

    let scriptFolders =
        [| "common/advisortypes"
           "common/ages"
           "common/ai_attitudes"
           "common/ai_personalities"
           "common/bookmarks"
           "common/buildings"
           "common/cb_types"
           "common/centers_of_trade"
           "common/church_aspects"
           "common/client_states"
           "common/colonial_regions"
           "common/countries"
           "common/country_colors"
           "common/country_tags"
           "common/cultures"
           "common/custom_country_colors"
           "common/custom_ideas"
           "common/decrees"
           "common/defines"
           "common/diplomatic_actions"
           "common/disasters"
           "common/dynasty_colors"
           "common/estates"
           "common/event_modifiers"
           "common/factions"
           "common/fervor"
           "common/fetishist_cults"
           "common/governments"
           "common/government_names"
           "common/government_ranks"
           "common/government_reforms"
           "common/great_projects"
           "common/ideas"
           "common/imperial_reforms"
           "common/incidents"
           "common/institutions"
           "common/insults"
           "common/isolationism"
           "common/leader_personalities"
           "common/natives"
           "common/native_advancement"
           "common/naval_doctrines"
           "common/new_diplomatic_actions"
           "common/on_actions"
           "common/opinion_modifiers"
           "common/parliament_bribes"
           "common/parliament_issues"
           "common/peace_treaties"
           "common/personal_deities"
           "common/policies"
           "common/powerprojection"
           "common/prices"
           "common/professionalism"
           "common/province_names"
           "common/province_triggered_modifiers"
           "common/rebel_types"
           "common/region_colors"
           "common/religions"
           "common/religious_conversions"
           "common/religious_reforms"
           "common/revolt_triggers"
           "common/ruler_personalities"
           "common/scripted_effects"
           "common/scripted_functions"
           "common/scripted_triggers"
           "common/state_edicts"
           "common/static_modifiers"
           "common/subject_types"
           "common/technologies"
           "common/timed_modifiers"
           "common/tradecompany_investments"
           "common/tradegoods"
           "common/tradenodes"
           "common/trade_companies"
           "common/trading_policies"
           "common/triggered_modifiers"
           "common/units"
           "common/units_display"
           "common/wargoal_types"
           "common"
           "customizable_localization"
           "decisions"
           "events"
           "hints"
           "history/advisors"
           "history/countries"
           "history/diplomacy"
           "history/provinces"
           "history/wars"
           "map"
           "music"
           "missions"
           "sound"
           "tutorial"
           "gfx"
           "interface"
           "localisation" |]
