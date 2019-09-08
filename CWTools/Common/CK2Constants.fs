namespace CWTools.Common

open CWTools.Utilities.Utils

module CK2Constants =
    // type Scope =
    //     | Character
    //     | Title
    //     | Province
    //     | Offmap
    //     | War
    //     | Siege
    //     | Unit
    //     | Religion
    //     | Culture
    //     | Society
    //     | Artifact
    //     | Bloodline
    //     | Wonder
    //     //Misc
    //     | Any
    //     | InvalidScope
    //     override x.ToString() =
    //         match x with
    //         | Any -> "Any/Unknown"
    //         | Character -> "Character"
    //         | Title -> "Title"
    //         | Province -> "Province"
    //         | Offmap -> "Offmap"
    //         | War -> "War"
    //         | Siege -> "Siege"
    //         | Unit -> "Unit"
    //         | Religion -> "Religion"
    //         | Culture -> "Culture"
    //         | Society -> "Society"
    //         | Artifact -> "Artifact"
    //         | Bloodline -> "Bloodline"
    //         | Wonder -> "Wonder"
    //         | InvalidScope -> "InvalidScope"

    //     static member AnyScope = Scope.Any

    //     interface IScope<Scope> with
    //         member this.AnyScope = Scope.Any
    //         member this.MatchesScope target =
    //             match this, target with
    //             | Scope.Any, _
    //             | _, Scope.Any -> true
    //             | _, _ -> this = target

    // let allScopes = [
    //     Scope.Character;
    //     Scope.Title;
    //     Scope.Province;
    //     Scope.Offmap;
    //     Scope.War
    //     Scope.Siege;
    //     Scope.Unit;
    //     Scope.Religion;
    //     Scope.Culture;
    //     Scope.Society;
    //     Scope.Artifact;
    //     Scope.Bloodline
    //         ]
    // let allScopesSet = allScopes |> Set.ofList
    // let parseScope =
    //     (fun (x : string) ->
    //     x.ToLower()
    //     |>
    //         function
    //             | "character" -> Scope.Character
    //             | "title" -> Scope.Title
    //             | "province" -> Scope.Province
    //             | "offmap" -> Scope.Offmap
    //             | "war" -> Scope.War
    //             | "siege" -> Scope.Siege
    //             | "unit" -> Scope.Unit
    //             | "religion" -> Scope.Religion
    //             | "culture" -> Scope.Culture
    //             | "society" -> Scope.Society
    //             | "artifact" -> Scope.Artifact
    //             | "bloodline" -> Scope.Bloodline
    //             | "wonder" -> Scope.Wonder
    //             | "any" -> Scope.Any
    //             | "all" -> Scope.Any
    //             | "no_scope" -> Scope.Any
    //             | x -> log (sprintf "Unexpected scope %O" x); Scope.Any) //failwith ("unexpected scope" + x.ToString()))

    // let parseScopes =
    //     function
    //     | "all" -> allScopes
    //     | x -> [parseScope x]


    let defaultScopes = [
        "Character", ["character"], []
        "Title", ["title"], []
        "Province", ["province"], []
        "Offmap", ["offmap"], []
        "War", ["war"], []
        "Siege", ["siege"], []
        "Unit", ["unit"], []
        "Religion", ["religion"], []
        "Culture", ["culture"], []
        "Society", ["society"], []
        "Artifact", ["artifact"], []
        "Bloodline", ["bloodline"], []
        "Wonder", ["wonder"], []
    ]
    let defaultScopeInputs =
        defaultScopes |> List.map (fun (n, s, ss) -> { NewScope.ScopeInput.name = n; NewScope.ScopeInput.aliases = s; NewScope.ScopeInput.isSubscopeOf = ss })

    type ModifierCategory =
        | Character
        | Province
        | Unit
        | Any

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
        ModifierCategory.Character, [NewScope.scopeManager.ParseScope() "character"];
        ModifierCategory.Province, [NewScope.scopeManager.ParseScope() "province"];
        ModifierCategory.Unit, [NewScope.scopeManager.ParseScope() "province"; NewScope.scopeManager.ParseScope() "unit"];
        ModifierCategory.Any, [];
    ]
    let modifierCategoryToScopesMap() = categoryScopeList() |> Map.ofList

    let scriptFolders = [
        "common";
        "common/alternate_start";
        "common/artifact_spawns";
        "common/artifacts";
        "common/bloodlines";
        "common/bookmarks";
        "common/buildings";
        "common/cb_types";
        "common/combat_tactics";
        "common/council_positions";
        "common/council_voting";
        "common/cultures";
        "common/death";
        "common/death_text";
        "common/defines";
        "common/disease";
        "common/dynasties";
        "common/event_modifiers";
        "common/execution_methods";
        "common/game_rules";
        "common/government_flavor";
        "common/governments";
        "common/graphicalculturetypes";
        "common/heir_text";
        "common/holding_types";
        "common/job_actions";
        "common/job_titles";
        "common/landed_titles";
        "common/laws";
        "common/mercenaries";
        "common/minor_titles";
        "common/modifier_definitions";
        "common/nicknames";
        "common/objectives";
        "common/offmap_powers";
        "common/offmap_powers/policies";
        "common/offmap_powers/statuses";
        "common/on_actions";
        "common/opinion_modifiers";
        "common/province_setup";
        "common/religion_features";
        "common/religion_modifiers";
        "common/religions";
        "common/religious_titles";
        "common/retinue_subunits";
        "common/save_conversion";
        "common/scripted_effects";
        "common/scripted_score_values";
        "common/scripted_triggers";
        "common/societies";
        "common/special_troops";
        "common/succession_voting";
        "common/trade_routes";
        "common/traits";
        "common/tributary_types";
        "common/triggered_modifiers";
        "decisions";
        "events";
        "gfx";
        "history";
        "history/characters";
        "history/offmap_powers";
        "history/provinces";
        "history/technology";
        "history/titles";
        "history/wars";
        "interface";
        "interface/coat_of_arms";
        "interface/portrait_offsets";
        "interface/portrait_properties";
        "interface/portrait";
        "localisation";
        "localisation/customizable_localisation";
        "map";
        "map/statics";
        "music";
        "sound";
        "tutorial"
    ]
