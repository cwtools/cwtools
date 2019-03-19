namespace CWTools.Common

open System
open System.ComponentModel.Design
open CWTools.Utilities.Utils

module HOI4Constants =
    type Scope =
        |State
        |Country
        |UnitLeader
        //Modifier scopes
        |Air
        //Misc
        |Any
        |InvalidScope
        override x.ToString() =
            match x with
            |UnitLeader -> "Unit leader"
            |State -> "State"
            |Air -> "Air"
            |InvalidScope -> "Invalid Scope"
            |Country -> "Country"
            |Any -> "Any/Unknown"
        static member AnyScope = Scope.Any
        interface IScope<Scope> with
            member this.AnyScope = Scope.Any
            member this.MatchesScope target =
                match this, target with
                | Scope.Any, _
                | _, Scope.Any -> true
                | _, _ -> this = target

    let allScopes = [
        Scope.State;
        Scope.Country;
        Scope.UnitLeader;
            ]
    let allScopesSet = allScopes |> Set.ofList
    let parseScope =
        (fun (x : string) ->
        x.ToLower()
        |>
            function
            |"state" -> Scope.State
            |"country" -> Scope.Country
            |"unit leader" -> Scope.UnitLeader
            |"unit_leader" -> Scope.UnitLeader
            |"air" -> Scope.Air
            |"any" -> Scope.Any
            |"all" -> Scope.Any
            |"no_scope" -> Scope.Any
            |x -> log (sprintf "Unexpected scope %O" x); Scope.Any) //failwith ("unexpected scope" + x.ToString()))

    let parseScopes =
        function
        |"all" -> allScopes
        |x -> [parseScope x]

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

    let categoryScopeList = [
        ModifierCategory.Country, [Scope.Country]
        ModifierCategory.UnitLeader, [Scope.UnitLeader; Scope.Country]
        ModifierCategory.Unit, [Scope.UnitLeader; Scope.Country]
        ModifierCategory.State, [Scope.Any]
        ModifierCategory.Air, [Scope.Air; Scope.Country]
    ]
    let modifierCategoryToScopesMap = categoryScopeList |> Map.ofList

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
