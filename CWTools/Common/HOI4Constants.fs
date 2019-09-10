namespace CWTools.Common

open CWTools.Utilities.Utils
open CWTools.Common.NewScope
module HOI4Constants =
    let defaultScopes = [
        "Country", ["country"]
        "State", ["state"]
        "Unit Leader", ["unit leader"; "unit_leader"]
        "Air", ["air"]
    ]
    let defaultScopeInputs =
        defaultScopes |> List.map (fun (n, s) -> { NewScope.ScopeInput.name = n; NewScope.ScopeInput.aliases = s; NewScope.ScopeInput.isSubscopeOf = []})

    let defaultModifiers = [
        "State", None, ["any"]
        "Country", None, ["country"]
        "Unit", None, ["UnitLeader"; "Country"]
        "UnitLeader", None, ["UnitLeader"; "Country"]
        "Air", None, ["air"; "country"]
    ]

    let defaultModifiersInputs() =
        defaultModifiers |> List.map (fun (n, intID, ss) ->
            {
                NewScope.ModifierCategoryInput.name = n;
                NewScope.ModifierCategoryInput.internalID = intID;
                NewScope.ModifierCategoryInput.scopes = ss |> List.map (scopeManager.ParseScope())
                })

    let scriptFolders = [
        "common/abilities";
        "common/aces";
        "common/ai_areas";
        "common/ai_focuses";
        "common/ai_peace";
        "common/ai_strategy";
        "common/ai_strategy_plans";
        "common/ai_templates";
        "common/autonomous_states";
        "common/bookmarks";
        "common/buildings";
        "common/continuous_focus";
        "common/countries";
        "common/country_leader";
        "common/country_tags";
        "common/decisions";
        "common/difficulty_settings";
        "common/idea_tags";
        "common/ideas";
        "common/ideologies";
        "common/modifiers";
        "common/names";
        "common/national_focus";
        "common/on_actions";
        "common/opinion_modifiers";
        "common/resources";
        "common/scripted_effects";
        "common/scripted_localisation";
        "common/scripted_triggers";
        "common/state_category";
        "common/technologies";
        "common/technology_sharing";
        "common/technology_tags";
        "common/terrain";
        "common/timed_activities";
        "common/unit_leader";
        "common/unit_tags";
        "common/units";
        "common/units/equipment";
        "common/wargoals";
        "events";
        "gfx";
        "interface";
        "localisation";
        "history";
        "map"
    ]
