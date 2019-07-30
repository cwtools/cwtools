namespace CWTools.Common

open System
open System.ComponentModel.Design
open CWTools.Utilities.Utils
open System.Collections.Generic


module EU4Constants =


    // type Scope =
    //     |Country
    //     |Province
    //     |TradeNode
    //     |Unit
    //     //Loc scopes
    //     |Monarch
    //     |Heir
    //     |Consort
    //     |RebelFaction
    //     |Religion
    //     |Culture
    //     |Advisor
    //     //Misc
    //     |Any
    //     |InvalidScope
    //     override x.ToString() =
    //         match x with
    //         |TradeNode -> "Trade node"
    //         |Country -> "Country"
    //         |Province -> "Province"
    //         |Unit -> "Unit"
    //         |Monarch -> "Monarch"
    //         |Heir -> "Heir"
    //         |Consort -> "Consort"
    //         |RebelFaction -> "Rebel Faction"
    //         |Religion -> "Religion"
    //         |Culture -> "Culture"
    //         |Advisor -> "Advisor"
    //         |InvalidScope -> "Invalid Scope"
    //         |Any -> "Any/Unknown"
    //     static member AnyScope = Scope.Any
    //     interface IScope<Scope> with
    //         member this.AnyScope = Scope.Any
    //         member this.MatchesScope target =
    //             match this, target with
    //             |TradeNode, Province -> true
    //             | _, Scope.Any
    //             | Scope.Any, _ -> true
    //             |this, target -> this = target
    // let allScopes = NewScope.scopeManager.allScopes
    // let allScopes = [
    //         Country;
    //         Province;
    //         TradeNode;
    //         Unit
    //         ]
    // let allScopesSet = allScopes |> Set.ofList
    let defaultScopes = [
        "Country", ["country"], []
        "Province", ["province"], []
        "Trade Node", ["trade_node"; "tradenode"], ["province"]
        "Unit", ["unit"], []
        "Monarch", ["monarch"], []
        "Heir",["heir"], []
        "Consort",["consort"], []
        "Rebel Faction",["rebel_faction"], []
        "Religion",["religion"], []
        "Culture",["culture"], []
        "Advisor",["advisor"], []
    ]
    let defaultScopeInputs =
        defaultScopes |> List.map (fun (n, s, ss) -> { NewScope.ScopeInput.name = n; NewScope.ScopeInput.aliases = s; NewScope.ScopeInput.isSubscopeOf = ss })


    // let parseScope =
    //     (fun (x : string) ->
    //     x.ToLower()
    //     |>
    //         function
    //         |"country" -> Scope.Country
    //         |"province" -> Scope.Province
    //         |"trade_node" -> Scope.TradeNode
    //         |"tradenode" -> Scope.TradeNode
    //         |"unit" -> Scope.Unit
    //         |"monarch" -> Scope.Monarch
    //         |"heir" -> Scope.Heir
    //         |"consort" -> Scope.Consort
    //         |"rebel_faction" -> Scope.RebelFaction
    //         |"religion" -> Scope.Religion
    //         |"culture" -> Scope.Culture
    //         |"advisor" -> Scope.Advisor
    //         |"any" -> Scope.Any
    //         |"all" -> Scope.Any
    //         |"no_scope" -> Scope.Any
    //         |x -> log (sprintf "Unexpected scope %O" x); Scope.Any) //failwith ("unexpected scope" + x.ToString()))

    // let parseScopes =
    //     function
    //     |"all" -> allScopes
    //     |x -> [parseScope x]

    type Effect = Effect<Scope>

    type DocEffect = DocEffect<Scope>
    type ScriptedEffect = ScriptedEffect<Scope>
    type ScopedEffect = ScopedEffect<Scope>
    type ModifierCategory =
        |Country
        |Province
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
    let scriptFolders = [
        "common/advisortypes";
        "common/ages";
        "common/ai_attitudes";
        "common/ai_personalities";
        "common/bookmarks";
        "common/buildings";
        "common/cb_types";
        "common/centers_of_trade";
        "common/church_aspects";
        "common/client_states";
        "common/colonial_regions";
        "common/countries";
        "common/country_colors";
        "common/country_tags";
        "common/cultures";
        "common/custom_country_colors";
        "common/custom_ideas";
        "common/decrees";
        "common/defines";
        "common/diplomatic_actions";
        "common/disasters";
        "common/dynasty_colors";
        "common/estates";
        "common/event_modifiers";
        "common/factions";
        "common/fervor";
        "common/fetishist_cults";
        "common/governments";
        "common/government_names";
        "common/government_ranks";
        "common/government_reforms";
        "common/great_projects";
        "common/ideas";
        "common/imperial_reforms";
        "common/incidents";
        "common/institutions";
        "common/insults";
        "common/isolationism";
        "common/leader_personalities";
        "common/natives";
        "common/native_advancement";
        "common/naval_doctrines";
        "common/new_diplomatic_actions";
        "common/on_actions";
        "common/opinion_modifiers";
        "common/parliament_bribes";
        "common/parliament_issues";
        "common/peace_treaties";
        "common/personal_deities";
        "common/policies";
        "common/powerprojection";
        "common/prices";
        "common/professionalism";
        "common/province_names";
        "common/province_triggered_modifiers";
        "common/rebel_types";
        "common/region_colors";
        "common/religions";
        "common/religious_conversions";
        "common/religious_reforms";
        "common/revolt_triggers";
        "common/ruler_personalities";
        "common/scripted_effects";
        "common/scripted_functions";
        "common/scripted_triggers";
        "common/state_edicts";
        "common/static_modifiers";
        "common/subject_types";
        "common/technologies";
        "common/timed_modifiers";
        "common/tradecompany_investments";
        "common/tradegoods";
        "common/tradenodes";
        "common/trade_companies";
        "common/trading_policies";
        "common/triggered_modifiers";
        "common/units";
        "common/units_display";
        "common/wargoal_types";
        "common";
        "customizable_localization";
        "decisions";
        "events";
        "hints";
        "history/advisors";
        "history/countries";
        "history/diplomacy";
        "history/provinces";
        "history/wars";
        "map";
        "music";
        "missions";
        "sound";
        "tutorial";
        "gfx";
        "interface";
        "localisation";]
