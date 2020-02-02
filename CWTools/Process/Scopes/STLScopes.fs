namespace CWTools.Process.Scopes

open System
open CWTools.Common
open CWTools.Process.Scopes
open CWTools.Common.NewScope
module STL =
    open CWTools.Common.STLConstants
    open CWTools.Utilities.Utils
    open Microsoft.FSharp.Collections.Tagged

    let defaultDesc = "Scope (/context) switch"
    let scopedEffects() = [
        ScopedEffect("space_owner", [scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "GalacticObject"; scopeManager.ParseScope() "Planet"], scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("overlord", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("defender", [scopeManager.ParseScope() "War"], scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("attacker", [scopeManager.ParseScope() "War"], scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("owner", [scopeManager.ParseScope() "Species" ;scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Pop"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "PopFaction"; scopeManager.ParseScope() "Sector"; scopeManager.ParseScope() "Leader"; scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Starbase"; scopeManager.ParseScope() "Tile"; scopeManager.ParseScope() "GalacticObject"], scopeManager.ParseScope() "Country", EffectType.Link, "", "", true); //Fleet, Planet, PopFaction, Sector, Leader, Country, Tile from vanilla use
        ScopedEffect("controller", [scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "Starbase"], scopeManager.ParseScope() "Country", EffectType.Link, "", "", true); //Removed controller of country
        ScopedEffect("planet_owner", [scopeManager.ParseScope() "Planet"], scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("last_created_country", scopeManager.AllScopes, scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("last_refugee_country", scopeManager.AllScopes, scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("leader", [scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "PopFaction"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "Sector"; scopeManager.ParseScope() "Army"], scopeManager.ParseScope() "Leader", EffectType.Link, "", "", true); // Army not in PDX list
        ScopedEffect("last_created_leader", scopeManager.AllScopes, scopeManager.ParseScope() "Leader", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("solar_system", scopeManager.AllScopes, scopeManager.ParseScope() "GalacticObject", EffectType.Link, "", "", true);
        ScopedEffect("last_created_system", scopeManager.AllScopes, scopeManager.ParseScope() "GalacticObject", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("planet", [scopeManager.ParseScope() "Pop"; scopeManager.ParseScope() "Tile"; scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "GalacticObject"; scopeManager.ParseScope() "Army"; scopeManager.ParseScope() "Megastructure"], scopeManager.ParseScope() "Planet", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("capital_scope", [scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Sector"], scopeManager.ParseScope() "Planet", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("orbit", [scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Megastructure"; scopeManager.ParseScope() "Army"], scopeManager.ParseScope() "Planet", EffectType.Link, defaultDesc, "", true); // Megastructure not in PDX list
        ScopedEffect("home_planet", [scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Species"; scopeManager.ParseScope() "Pop"; scopeManager.ParseScope() "Planet"], scopeManager.ParseScope() "Planet", EffectType.Link, defaultDesc, "", true); // Planet not in PDX list
        ScopedEffect("last_created_ship", scopeManager.AllScopes, scopeManager.ParseScope() "Ship", EffectType.Link, defaultDesc, "", true);
        //ScopedEffect("spaceport", [scopeManager.ParseScope() "Planet"], scopeManager.ParseScope() "Fleet", EffectType.Link, defaultDesc, "", true); Removed in 2.0
        ScopedEffect("mining_station", [scopeManager.ParseScope() "Planet"], scopeManager.ParseScope() "Fleet", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("research_station", [scopeManager.ParseScope() "Planet"], scopeManager.ParseScope() "Fleet", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("last_created_fleet", scopeManager.AllScopes, scopeManager.ParseScope() "Fleet", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("fleet", [scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Starbase"; scopeManager.ParseScope() "Leader"; scopeManager.ParseScope() "Army"], scopeManager.ParseScope() "Fleet", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("pop", [scopeManager.ParseScope() "Pop"; scopeManager.ParseScope() "Tile"], scopeManager.ParseScope() "Pop", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("last_created_pop", scopeManager.AllScopes, scopeManager.ParseScope() "Pop", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("last_created_ambient_object", scopeManager.AllScopes, scopeManager.ParseScope() "AmbientObject", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("last_created_army", scopeManager.AllScopes, scopeManager.ParseScope() "Army", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("tile", [scopeManager.ParseScope() "Pop"; scopeManager.ParseScope() "Tile"], scopeManager.ParseScope() "Tile", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("orbital_deposit_tile", [scopeManager.ParseScope() "Planet"], scopeManager.ParseScope() "Tile", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("best_tile_for_pop", [scopeManager.ParseScope() "Planet"], scopeManager.ParseScope() "Tile", EffectType.Link, defaultDesc, "", true);
        //ScopedEffect("owner_species", [scopeManager.ParseScope() "Species" ;scopeManager.ParseScope() "Pop"; scopeManager.ParseScope() "Leader"; scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Species", EffectType.Link, "", "", true); //PDX list
        ScopedEffect("owner_species", [scopeManager.ParseScope() "Species" ;scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Pop"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "PopFaction"; scopeManager.ParseScope() "Sector"; scopeManager.ParseScope() "Leader"; scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Starbase"; scopeManager.ParseScope() "Tile"; scopeManager.ParseScope() "GalacticObject"], scopeManager.ParseScope() "Species", EffectType.Link, "", "", true); //Copied owner
        ScopedEffect("last_created_species", scopeManager.AllScopes, scopeManager.ParseScope() "Species", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("species", [scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Leader"; scopeManager.ParseScope() "Pop"; scopeManager.ParseScope() "Army"], scopeManager.ParseScope() "Species", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("pop_faction", [scopeManager.ParseScope() "Pop"; scopeManager.ParseScope() "Leader"], scopeManager.ParseScope() "PopFaction", EffectType.Link, defaultDesc, "", true); //Leader from vanilla
        ScopedEffect("last_created_pop_faction", scopeManager.AllScopes, scopeManager.ParseScope() "PopFaction", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("default_pop_faction", scopeManager.AllScopes, scopeManager.ParseScope() "PopFaction", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("sector", [scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "GalacticObject"; scopeManager.ParseScope() "Leader"], scopeManager.ParseScope() "Sector", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("core_sector", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Sector", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("ruler", [scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "Tile"], scopeManager.ParseScope() "Leader", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("heir", [scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "Tile"], scopeManager.ParseScope() "Leader", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("alliance", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Alliance", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("star", [scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "GalacticObject";], scopeManager.ParseScope() "Planet", EffectType.Link, defaultDesc, "", true); //PDX List
        //ScopedEffect("star", [scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "AmbientObject"; scopeManager.ParseScope() "Megastructure"; scopeManager.ParseScope() "GalacticObject"; scopeManager.ParseScope() "Starbase"], scopeManager.ParseScope() "Planet", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("last_created_design", scopeManager.AllScopes, scopeManager.ParseScope() "Design", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("starbase", [scopeManager.ParseScope() "GalacticObject"; scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "Star"; scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Fleet"], scopeManager.ParseScope() "Starbase", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("capital_star", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Planet", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("no_scope", scopeManager.AllScopes, scopeManager.ParseScope() "Any", EffectType.Link, defaultDesc, "", true)
        ScopedEffect("megastructure", scopeManager.AllScopes, scopeManager.ParseScope() "Megastructure", EffectType.Link, defaultDesc, "", true)
        ScopedEffect("owner_main_species", [scopeManager.ParseScope() "Species" ;scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Pop"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "PopFaction"; scopeManager.ParseScope() "Sector"; scopeManager.ParseScope() "Leader"; scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Starbase"; scopeManager.ParseScope() "Tile"; scopeManager.ParseScope() "GalacticObject"], scopeManager.ParseScope() "Species", EffectType.Link, defaultDesc, "", true) //Copied owner
        ScopedEffect("system_star", [scopeManager.ParseScope() "GalacticObject"], scopeManager.ParseScope() "Planet", EffectType.Link, defaultDesc, "", true); // Not in PDX
        ScopedEffect("observation_outpost_owner", scopeManager.AllScopes, scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true); // Not in PDX
        ScopedEffect("branch_office_owner", [scopeManager.ParseScope() "Planet"], scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true); // New with 2.2


    ]
    let effectInnerScopes() =[
        //These are from blackninja9939
        "any_ship",  scopeManager.ParseScope() "Ship";
        "every_ship",  scopeManager.ParseScope() "Ship";
        "random_ship",  scopeManager.ParseScope() "Ship";

        "any_pop",  scopeManager.ParseScope() "Pop";
        "every_pop",  scopeManager.ParseScope() "Pop";
        "random_pop",  scopeManager.ParseScope() "Pop";

        "any_owned_pop",  scopeManager.ParseScope() "Pop";
        "every_owned_pop",  scopeManager.ParseScope() "Pop";
        "random_owned_pop",  scopeManager.ParseScope() "Pop";


        "any_owned_ship",  scopeManager.ParseScope() "Ship";
        "every_owned_ship",  scopeManager.ParseScope() "Ship";
        "random_owned_ship",  scopeManager.ParseScope() "Ship";

        "any_owned_planet",  scopeManager.ParseScope() "Planet";
        "every_owned_planet",  scopeManager.ParseScope() "Planet";
        "random_owned_planet",  scopeManager.ParseScope() "Planet";

        "any_controlled_planet",  scopeManager.ParseScope() "Planet";
        "every_controlled_planet",  scopeManager.ParseScope() "Planet";
        "random_controlled_planet",  scopeManager.ParseScope() "Planet";

        "any_war_defender",  scopeManager.ParseScope() "Country";
        "every_war_defender",  scopeManager.ParseScope() "Country";
        "random_war_defender",  scopeManager.ParseScope() "Country";

        "any_war_attacker",  scopeManager.ParseScope() "Country";
        "every_war_attacker",  scopeManager.ParseScope() "Country";
        "random_war_attacker",  scopeManager.ParseScope() "Country";

        "any_war_participant",  scopeManager.ParseScope() "Country";
        "every_war_participant",  scopeManager.ParseScope() "Country";
        "random_war_participant",  scopeManager.ParseScope() "Country";

        "any_planet",  scopeManager.ParseScope() "Planet";
        "every_planet",  scopeManager.ParseScope() "Planet";
        "random_planet",  scopeManager.ParseScope() "Planet";

        "any_planet_within_border",  scopeManager.ParseScope() "Planet";
        "every_planet_within_border",  scopeManager.ParseScope() "Planet";
        "random_planet_within_border",  scopeManager.ParseScope() "Planet";

        "any_ambient_object",  scopeManager.ParseScope() "AmbientObject";
        "every_ambient_object",  scopeManager.ParseScope() "AmbientObject";
        "random_ambient_object",  scopeManager.ParseScope() "AmbientObject";

        "any_system_ambient_object",  scopeManager.ParseScope() "AmbientObject";
        "every_system_ambient_object",  scopeManager.ParseScope() "AmbientObject";
        "random_system_ambient_object",  scopeManager.ParseScope() "AmbientObject";

        "any_mining_station",  scopeManager.ParseScope() "Fleet";
        "every_mining_station",  scopeManager.ParseScope() "Fleet";
        "random_mining_station",  scopeManager.ParseScope() "Fleet";

        "any_research_station",  scopeManager.ParseScope() "Fleet";
        "every_research_station",  scopeManager.ParseScope() "Fleet";
        "random_research_station",  scopeManager.ParseScope() "Fleet";

        "any_spaceport",  scopeManager.ParseScope() "Fleet";
        "every_spaceport",  scopeManager.ParseScope() "Fleet";
        "random_spaceport",  scopeManager.ParseScope() "Fleet";

        "any_system_planet",  scopeManager.ParseScope() "Planet";
        "every_system_planet",  scopeManager.ParseScope() "Planet";
        "random_system_planet",  scopeManager.ParseScope() "Planet";

        "any_neighboring_tile",  scopeManager.ParseScope() "Tile";
        "every_neighboring_tile",  scopeManager.ParseScope() "Tile";
        "random_neighboring_tile",  scopeManager.ParseScope() "Tile";

        "any_tile",  scopeManager.ParseScope() "Tile";
        "every_tile",  scopeManager.ParseScope() "Tile";
        "random_tile",  scopeManager.ParseScope() "Tile";

        "any_moon",  scopeManager.ParseScope() "Planet";
        "every_moon",  scopeManager.ParseScope() "Planet";
        "random_moon",  scopeManager.ParseScope() "Planet";

        "any_system_in_cluster",  scopeManager.ParseScope() "GalacticObject";
        "every_system_in_cluster",  scopeManager.ParseScope() "GalacticObject";
        "random_system_in_cluster",  scopeManager.ParseScope() "GalacticObject";

        "any_owned_leader",  scopeManager.ParseScope() "Leader";
        "every_owned_leader",  scopeManager.ParseScope() "Leader";
        "random_owned_leader",  scopeManager.ParseScope() "Leader";

        "any_pool_leader",  scopeManager.ParseScope() "Leader";
        "every_pool_leader",  scopeManager.ParseScope() "Leader";
        "random_pool_leader",  scopeManager.ParseScope() "Leader";


        "any_sector",  scopeManager.ParseScope() "Sector";
        "every_sector",  scopeManager.ParseScope() "Sector";
        "random_sector",  scopeManager.ParseScope() "Sector";

        "any_owned_fleet",  scopeManager.ParseScope() "Fleet";
        "every_owned_fleet",  scopeManager.ParseScope() "Fleet";
        "random_owned_fleet",  scopeManager.ParseScope() "Fleet";

        "any_fleet_in_system",  scopeManager.ParseScope() "Fleet";
        "every_fleet_in_system",  scopeManager.ParseScope() "Fleet";
        "random_fleet_in_system",  scopeManager.ParseScope() "Fleet";

                //"any_pop_faction", scopeManager.AllScopes, scopeManager.ParseScope() "PopFaction"; //Doesn't exist according to caligula
        "every_pop_faction",  scopeManager.ParseScope() "PopFaction";
        "random_pop_faction",  scopeManager.ParseScope() "PopFaction";

        "any_playable_country",  scopeManager.ParseScope() "Country";
        "every_playable_country",  scopeManager.ParseScope() "Country";
        "random_playable_country",  scopeManager.ParseScope() "Country";

        "any_bordering_country", scopeManager.ParseScope() "Country"
        "every_bordering_country", scopeManager.ParseScope() "Country"
        "random_bordering_country", scopeManager.ParseScope() "Country"

        "any_subject",  scopeManager.ParseScope() "Country";
        "every_subject",  scopeManager.ParseScope() "Country";
        "random_subject",  scopeManager.ParseScope() "Country";

                ///The following are assumptions
        "any_country",  scopeManager.ParseScope() "Country";
        "every_country",  scopeManager.ParseScope() "Country";
        "random_country",  scopeManager.ParseScope() "Country";

        "any_army",  scopeManager.ParseScope() "Army";
        "every_army",  scopeManager.ParseScope() "Army";
        "random_army",  scopeManager.ParseScope() "Army";

        "any_rim_system",  scopeManager.ParseScope() "GalacticObject";
        "every_rim_system",  scopeManager.ParseScope() "GalacticObject";
        "random_rim_system",  scopeManager.ParseScope() "GalacticObject";

        "any_neighbor_country",  scopeManager.ParseScope() "Country";
        "every_neighbor_country",  scopeManager.ParseScope() "Country";
        "random_neighbor_country",  scopeManager.ParseScope() "Country";

        "any_system_within_border",  scopeManager.ParseScope() "GalacticObject";
        "every_system_within_border",  scopeManager.ParseScope() "GalacticObject";
        "random_system_within_border",  scopeManager.ParseScope() "GalacticObject";

        "any_system", scopeManager.ParseScope() "GalacticObject";
        "every_system", scopeManager.ParseScope() "GalacticObject";
        "random_system", scopeManager.ParseScope() "GalacticObject";

        "any_relation", scopeManager.ParseScope() "Country";
        "every_relation", scopeManager.ParseScope() "Country";
        "random_relation", scopeManager.ParseScope() "Country";

        "any_planet_army",  scopeManager.ParseScope() "Army";
        "every_planet_army",  scopeManager.ParseScope() "Army";
        "random_planet_army",  scopeManager.ParseScope() "Army";

        "any_megastructure",  scopeManager.ParseScope() "Megastructure";
        "every_megastructure",  scopeManager.ParseScope() "Megastructure";
        "random_megastructure",  scopeManager.ParseScope() "Megastructure";

        "last_created_ambient_object", scopeManager.ParseScope() "AmbientObject";
        "last_created_country", scopeManager.ParseScope() "Country";
        "last_created_fleet", scopeManager.ParseScope() "Fleet";
        "last_created_leader", scopeManager.ParseScope() "Leader";
        "last_created_pop", scopeManager.ParseScope() "Pop";
        "last_created_species", scopeManager.ParseScope() "Species"

        ]
    let effectInnerScopeFunctions() = [
        "if", None, []
        "while", None, []
        "any_neighbor_system", (Some (scopeManager.ParseScope() "GalacticObject")), ["ignore_hyperlanes"];
        "every_neighbor_system", (Some  (scopeManager.ParseScope() "GalacticObject")), ["ignore_hyperlanes"];
        "random_neighbor_system", (Some (scopeManager.ParseScope() "GalacticObject")), ["ignore_hyperlanes"];
    ]

    let addInnerScope (des : DocEffect list) =
        let withSimple =
             des |> List.map (fun de ->
                match effectInnerScopes() |> List.tryPick (function | (n, t) when n = de.Name -> Some (t) |_ -> None) with
                | Some t -> ScopedEffect(de, Some t, true, [], false, false) :> DocEffect
                | None -> de)
        withSimple |> List.map (fun de ->
                match effectInnerScopeFunctions() |> List.tryPick (function | (n, t, s) when n = de.Name -> Some (t, s) |_ -> None) with
                | Some (t, s) -> ScopedEffect(de, t, false, s, false, false) :> DocEffect
                | None -> de)




    let oneToOneScopes =
        let from i = fun ((s), change) -> {s with Scopes = (s.GetFrom i)::s.Scopes}, (false, true)
        let prev = fun ((s), change) -> {s with Scopes = s.PopScope}, (false, true)
        [
        "THIS", id;
        "ROOT", fun ((s), change) -> {s with Scopes = s.Root::s.Scopes}, (false, true);
        "FROM", from 1;
        "FROMFROM", from 2;
        "FROMFROMFROM", from 3;
        "FROMFROMFROMFROM", from 4;
        "PREV", prev;
        "PREVPREV", prev >> prev;
        "PREVPREVPREV", prev >> prev >> prev;
        "PREVPREVPREVPREV", prev >> prev >> prev >> prev
        "hidden_effect", id;
        "hidden_trigger", id;
    ]
    let oneToOneScopesNames = List.map fst oneToOneScopes
    type EffectMap = Map<string, Effect, InsensitiveStringComparer>
    let changeScope = Scopes.createChangeScope oneToOneScopes (Scopes.simpleVarPrefixFun "var:") true

    let sourceScope (effects : Effect list) (key : string) =
        let key = if key.StartsWith("hidden:", StringComparison.OrdinalIgnoreCase) then key.Substring(7) else key
        let keys = key.Split('.') |> List.ofArray
        let inner (nextKey : string) =
            let onetoone = oneToOneScopes |> List.tryFind (fun (k, _) -> k == nextKey)
            match onetoone with
            | Some (_) -> None
            | None ->
                let effect = (effects)
                            |> List.choose (function | :? ScopedEffect as e -> Some e |_ -> None)
                            |> List.tryFind (fun e -> e.Name == nextKey)
                match effect with
                |None -> None
                |Some e -> Some e.Scopes
        keys |> List.fold (fun acc k -> match acc with |Some e -> Some e |None -> inner k) None |> Option.defaultValue scopeManager.AllScopes

