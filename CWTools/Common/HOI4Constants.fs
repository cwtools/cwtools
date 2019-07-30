namespace CWTools.Common

open CWTools.Utilities.Utils
open CWTools.Common.NewScope
module HOI4Constants =
    // type Scope =
    //     |State
    //     |Country
    //     |UnitLeader
    //     //Modifier scopes
    //     |Air
    //     //Misc
    //     |Any
    //     |InvalidScope
    //     override x.ToString() =
    //         match x with
    //         |UnitLeader -> "Unit leader"
    //         |State -> "State"
    //         |Air -> "Air"
    //         |InvalidScope -> "Invalid Scope"
    //         |Country -> "Country"
    //         |Any -> "Any/Unknown"
    //     static member AnyScope = scopeManager.ParseScope() "Any"
    //     interface IScope<Scope> with
    //         member this.AnyScope = scopeManager.ParseScope() "Any"
    //         member this.MatchesScope target =
    //             match this, target with
    //             | scopeManager.ParseScope() "Any", _
    //             | _, scopeManager.ParseScope() "Any" -> true
    //             | _, _ -> this = target

    // let allScopes = [
    //     scopeManager.ParseScope() "State";
    //     scopeManager.ParseScope() "Country";
    //     scopeManager.ParseScope() "UnitLeader";
    //         ]
    // let allScopesSet = allScopes |> Set.ofList
    // let parseScope =
    //     (fun (x : string) ->
    //     x.ToLower()
    //     |>
    //         function
    //         |"state" -> scopeManager.ParseScope() "State"
    //         |"country" -> scopeManager.ParseScope() "Country"
    //         |"unit leader" -> scopeManager.ParseScope() "UnitLeader"
    //         |"unit_leader" -> scopeManager.ParseScope() "UnitLeader"
    //         |"air" -> scopeManager.ParseScope() "Air"
    //         |"any" -> scopeManager.ParseScope() "Any"
    //         |"all" -> scopeManager.ParseScope() "Any"
    //         |"no_scope" -> scopeManager.ParseScope() "Any"
    //         |x -> log (sprintf "Unexpected scope %O" x); scopeManager.ParseScope() "Any") //failwith ("unexpected scope" + x.ToString()))

    // let parseScopes =
    //     function
    //     |"all" -> allScopes
    //     |x -> [parseScope x]

    let defaultScopes = [
        "Country", ["country"]
        "State", ["state"]
        "Unit Leader", ["unit leader"; "unit_leader"]
        "Air", ["air"]
    ]
    let defaultScopeInputs =
        defaultScopes |> List.map (fun (n, s) -> { NewScope.ScopeInput.name = n; NewScope.ScopeInput.aliases = s; NewScope.ScopeInput.isSubscopeOf = []})


    type Effect = Effect<Scope>

    type DocEffect = DocEffect<Scope>
    type ScriptedEffect = ScriptedEffect<Scope>
    type ScopedEffect = ScopedEffect<Scope>
    type ModifierCategory =
        |State
        |Country
        |Unit
        |UnitLeader
        |Air
        |Any

    type Modifier =
        {
            tag : string
            categories : ModifierCategory list
            /// Is this a core modifier or a static modifier?
            core : bool
        }
        interface IModifier with
            member this.Tag = this.tag

    let categoryScopeList() = [
        ModifierCategory.Country, [scopeManager.ParseScope() "Country"]
        ModifierCategory.UnitLeader, [scopeManager.ParseScope() "UnitLeader"; scopeManager.ParseScope() "Country"]
        ModifierCategory.Unit, [scopeManager.ParseScope() "UnitLeader"; scopeManager.ParseScope() "Country"]
        ModifierCategory.State, [scopeManager.ParseScope() "Any"]
        ModifierCategory.Air, [scopeManager.ParseScope() "Air"; scopeManager.ParseScope() "Country"]
    ]
    let modifierCategoryToScopesMap() = categoryScopeList() |> Map.ofList

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
