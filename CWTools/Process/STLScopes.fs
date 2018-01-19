namespace CWTools.Process

module STLScopes =
    open CWTools.Common.STLConstants
    



//         COUNTRY:
// space_owner
// overlord
// defender
// attacker
// owner
// controller
// planet_owner
// last_created_country
// last_refugee_country

// LEADER:
// leader
// last_created_leader
// ruler
// heir

// GALACTIC_OBJECT:
// solar_system
// last_created_system

// PLANET:
// planet
// capital_scope
// orbit
// home_planet
// star

// SHIP:
// last_created_ship

// FLEET:
// spaceport
// mining_station
// research_station
// last_created_fleet
// fleet

// POP:
// pop
// last_created_pop

// AMBIENT_OBJECT:
// last_created_ambient_object

// ARMY:
// last_created_army

// TILE:
// tile
// orbital_deposit_tile
// best_tile_for_pop

// SPECIES:
// owner_species
// last_created_species
// species

// POP_FACTION:
// pop_faction
// last_created_pop_faction

// SECTOR:
// core_sector
// sector

// LEADER:

// ALLIANCE:
// alliance

// RELATIVE SCOPES:
// root
// from
// prev
// prevprev
// prevprevprev
// prevprevprevprev
// fromfrom
// fromfromfrom
// fromfromfromfrom
// this
// event_target:
// parameter:

    let relativeScopes =
        ["root", Scope.Any, Scope.Any;
        "from", Scope.Any, Scope.Any;
        "prev", Scope.Any, Scope.Any;
        "prevprev", Scope.Any, Scope.Any;
        "prevprevprev", Scope.Any, Scope.Any;
        "prevprevprevprev", Scope.Any, Scope.Any;
        "fromfrom", Scope.Any, Scope.Any;
        "fromfromfrom", Scope.Any, Scope.Any;
        "fromfromfromfrom", Scope.Any, Scope.Any;
        "this", Scope.Any, Scope.Any;
        "event_target:", Scope.Any, Scope.Any;
        "parameter:", Scope.Any, Scope.Any]

// any/every/random_ship
// any/every/random_pop
// any/every/random_owned_ship
// any/every/random_owned_planet
// any/every/random_controlled_planet
// any/every/random_war_defender
// any/every/random_war_attacker
// any/every/random_planet
// any/every/random_planet_within_border
// any/every/random_ambient_object
// any/every/random_system_ambient_object
// any/every/random_mining_station
// any/every/random_research_station
// any/every/random_spaceport
// any/every/random_system_planet    
// any/every/random_neighboring_tile
// any/every/random_tile
// any/every/random_neighbor_system
// any/every/random_moon
// any/every/random_system_in_cluster
// any/every/random_owned_leader
// any/every/random_sector
// any/every/random_owned_fleet
// any/every/random_fleet_in_system
// any/every/random_pop_faction
// any/every/random_playable_country
// any/every/random_subject
    let scopeLists =[ 
        //These are from blackninja9939
        "any_ship", allScopes, Scope.Ship;
        "every_ship", allScopes, Scope.Ship;
        "random_ship", allScopes, Scope.Ship;

        "any_pop", allScopes, Scope.Pop;
        "every_pop", allScopes, Scope.Pop;
        "random_pop", allScopes, Scope.Pop;

        "any_owned_ship", allScopes, Scope.Ship;
        "every_owned_ship", allScopes, Scope.Ship;
        "random_owned_ship", allScopes, Scope.Ship;

        "any_owned_planet", allScopes, Scope.Planet;
        "every_owned_planet", allScopes, Scope.Planet;
        "random_owned_planet", allScopes, Scope.Planet;

        "any_controlled_planet", allScopes, Scope.Planet;
        "every_controlled_planet", allScopes, Scope.Planet;
        "random_controlled_planet", allScopes, Scope.Planet;

        "any_war_defender", allScopes, Scope.Country;
        "every_war_defender", allScopes, Scope.Country;
        "random_war_defender", allScopes, Scope.Country;

        "any_war_attacker", allScopes, Scope.Country;
        "every_war_attacker", allScopes, Scope.Country;
        "random_war_attacker", allScopes, Scope.Country;
        
        "any_planet", allScopes, Scope.Planet;
        "every_planet", allScopes, Scope.Planet;
        "random_planet", allScopes, Scope.Planet;
        
        "any_planet_within_border", allScopes, Scope.Planet;
        "every_planet_within_border", allScopes, Scope.Planet;
        "random_planet_within_border", allScopes, Scope.Planet;
        
        "any_ambient_object", allScopes, Scope.AmbientObject;
        "every_ambient_object", allScopes, Scope.AmbientObject;
        "random_ambient_object", allScopes, Scope.AmbientObject;
        
        "any_system_ambient_object", [Scope.GalacticObject], Scope.AmbientObject;
        "every_system_ambient_object", [Scope.GalacticObject], Scope.AmbientObject;
        "random_system_ambient_object", [Scope.GalacticObject], Scope.AmbientObject;
        
        "any_mining_station", allScopes, Scope.Fleet;
        "every_mining_station", allScopes, Scope.Fleet;
        "random_mining_station", allScopes, Scope.Fleet;
        
        "any_research_station", allScopes, Scope.Fleet;
        "every_research_station", allScopes, Scope.Fleet;
        "random_research_station", allScopes, Scope.Fleet;
        
        "any_spaceport", allScopes, Scope.Fleet;
        "every_spaceport", allScopes, Scope.Fleet;
        "random_spaceport", allScopes, Scope.Fleet;
        
        "any_system_planet", [Scope.GalacticObject], Scope.Planet;
        "every_system_planet", [Scope.GalacticObject], Scope.Planet;
        "random_system_planet", [Scope.GalacticObject], Scope.Planet;
        
        "any_neighboring_tile", [Scope.Tile], Scope.Tile;
        "every_neighboring_tile", [Scope.Tile], Scope.Tile;
        "random_neighboring_tile", [Scope.Tile], Scope.Tile;
        
        "any_tile", allScopes, Scope.Tile;
        "every_tile", allScopes, Scope.Tile;
        "random_tile", allScopes, Scope.Tile;
        
        "any_neighbor_system",  [Scope.GalacticObject], Scope.GalacticObject;
        "every_neighbor_system",  [Scope.GalacticObject], Scope.GalacticObject;
        "random_neighbor_system",  [Scope.GalacticObject], Scope.GalacticObject;
        
        "any_moon", allScopes, Scope.Planet;
        "every_moon", allScopes, Scope.Planet;
        "random_moon", allScopes, Scope.Planet;
        
        "any_system_in_cluster", allScopes, Scope.GalacticObject;
        "every_system_in_cluster", allScopes, Scope.GalacticObject;
        "random_system_in_cluster", allScopes, Scope.GalacticObject;
        
        "any_owned_leader", allScopes, Scope.Leader;
        "every_owned_leader", allScopes, Scope.Leader;
        "random_owned_leader", allScopes, Scope.Leader;
        
        "any_sector", allScopes, Scope.Sector;
        "every_sector", allScopes, Scope.Sector;
        "random_sector", allScopes, Scope.Sector;
        
        "any_owned_fleet", allScopes, Scope.Fleet;
        "every_owned_fleet", allScopes, Scope.Fleet;
        "random_owned_fleet", allScopes, Scope.Fleet;
        
        "any_fleet_in_system", [Scope.GalacticObject], Scope.Fleet;
        "every_fleet_in_system",[Scope.GalacticObject], Scope.Fleet;
        "random_fleet_in_system", [Scope.GalacticObject], Scope.Fleet;
        
        //"any_pop_faction", allScopes, Scope.PopFaction; //Doesn't exist according to caligula
        "every_pop_faction", allScopes, Scope.PopFaction;
        "random_pop_faction", allScopes, Scope.PopFaction;
        
        "any_playable_country", allScopes, Scope.Country;
        "every_playable_country", allScopes, Scope.Country;
        "random_playable_country", allScopes, Scope.Country;
        
        "any_subject", allScopes, Scope.Country;
        "every_subject", allScopes, Scope.Country;
        "random_subject", allScopes, Scope.Country;
        
        ///The following are assumptions 
        "any_country", allScopes, Scope.Country;
        "every_country", allScopes, Scope.Country;
        "random_country", allScopes, Scope.Country;
        
        "any_army", allScopes, Scope.Army;
        "every_army", allScopes, Scope.Army;
        "random_army", allScopes, Scope.Army;
        
        "any_rim_system", allScopes, Scope.GalacticObject;
        "every_rim_system", allScopes, Scope.GalacticObject;
        "random_rim_system", allScopes, Scope.GalacticObject;
        
        "any_neighbor_country", [Scope.Country], Scope.Country;
        "every_neighbor_country", [Scope.Country], Scope.Country;
        "random_neighbor_country", [Scope.Country], Scope.Country;
        
        "any_system_within_border", [Scope.Country], Scope.GalacticObject;
        "every_system_within_border", [Scope.Country], Scope.GalacticObject;
        "random_system_within_border", [Scope.Country], Scope.GalacticObject;
        
        "any_planet_army", [Scope.Planet], Scope.Army;
        "every_planet_army", [Scope.Planet], Scope.Army;
        "random_planet_army", [Scope.Planet], Scope.Army;

        ]

    let anyScopes = [
        "last_created_ambient_object", allScopes, Scope.AmbientObject
        ;"last_created_country", allScopes, Scope.Country
        ;"last_created_fleet", allScopes, Scope.Fleet
        ;"last_created_leader", allScopes, Scope.Leader
        ;"last_created_pop", allScopes, Scope.Pop
        ;"last_created_species", allScopes, Scope.Species
    ]

    let scopes =
        ["every_country", Scope.Any, Scope.Country
        ;"every_pop", Scope.Any, Scope.Pop
        ;"every_planet", Scope.Any, Scope.Planet
        ;"last_created_ambient_object", Scope.Any, Scope.AmbientObject
        ;"last_created_country", Scope.Any, Scope.Country
        ;"last_created_fleet", Scope.Any, Scope.Fleet
        ;"last_created_leader", Scope.Any, Scope.Leader
        ;"last_created_pop", Scope.Any, Scope.Pop
        ;"last_created_species", Scope.Any, Scope.Species
        ;"random_army", Scope.Any, Scope.Army
        ;"random_country", Scope.Any, Scope.Country
        ;"random_planet", Scope.Any, Scope.Planet
        ;"random_rim_system", Scope.Any, Scope.GalacticObject
        ;"random_system", Scope.Any, Scope.GalacticObject
        ;"any_country", Scope.Any, Scope.Country
        ;"any_system", Scope.Any, Scope.GalacticObject
        ;"every_owned_fleet", Scope.Country, Scope.Fleet
        ;"every_owned_planet", Scope.Country, Scope.Planet
        ;"every_owned_leader", Scope.Country, Scope.Leader
        ;"every_owned_pop", Scope.Country, Scope.Pop
        ;"every_owned_ship", Scope.Country, Scope.Ship
        ;"every_planet_within_border", Scope.Country, Scope.Planet
        ;"every_pop_faction", Scope.Country, Scope.PopFaction
        ;"owner_species", Scope.Country, Scope.Species
        ;"random_neighbor_country", Scope.Country, Scope.Country
        ;"random_owned_fleet", Scope.Country, Scope.Fleet
        ;"random_owned_leader", Scope.Country, Scope.Leader
        ;"random_owned_planet", Scope.Country, Scope.Planet
        ;"random_owned_pop", Scope.Country, Scope.Pop
        ;"random_owned_ship", Scope.Country, Scope.Ship
        ;"random_fleet_in_system", Scope.Country, Scope.Fleet
        ;"random_pop_faction", Scope.Country, Scope.PopFaction
        ;"random_sector", Scope.Country, Scope.Sector
        ;"random_system_within_border", Scope.Country, Scope.GalacticObject
        ;"any_controlled_planet", Scope.Country, Scope.Planet
        ;"any_neighbor_country", Scope.Country, Scope.Country
        ;"any_owned_fleet", Scope.Country, Scope.Fleet
        ;"any_owned_leader", Scope.Country, Scope.Leader
        ;"any_owned_planet", Scope.Country, Scope.Planet
        ;"any_owned_pop", Scope.Country, Scope.Pop
        ;"any_owned_ship", Scope.Country, Scope.Ship
        ;"any_planet_within_border", Scope.Country, Scope.Planet
        ;"any_sector", Scope.Country, Scope.Sector
        ;"any_system_within_border", Scope.Country, Scope.GalacticObject
        //;"any_war", Scope.Country, Scope.War //?CK2
        ;"capital_scope", Scope.Country, Scope.Planet
        ;"overlord", Scope.Country, Scope.Country //?
        ;"home_planet", Scope.Species, Scope.Planet //More
        ;"solar_system", Scope.Planet, Scope.GalacticObject //More
        ;"any_ship", Scope.Fleet, Scope.Ship
        ;"orbit", Scope.Fleet, Scope.Planet
        ;"orbit", Scope.Ship, Scope.Planet
        ;"best_tile_for_pop", Scope.Planet, Scope.Tile
        ;"every_spaceport", Scope.Planet, Scope.Ship //?
        ;"every_planet_army", Scope.Planet, Scope.Army
        ;"every_tile", Scope.Planet, Scope.Tile
        ;"random_moon", Scope.Planet, Scope.Planet //?
        ;"random_pop", Scope.Planet, Scope.Pop
        ;"random_tile", Scope.Planet, Scope.Tile
        ;"any_moon", Scope.Planet, Scope.Planet //?
        ;"any_pop", Scope.Planet, Scope.Pop
        ;"any_tile", Scope.Planet, Scope.Tile
        ;"orbital_deposit_tile", Scope.Planet, Scope.Tile
        ;"observation_outpost_owner", Scope.Planet, Scope.Country
        ;"observation_outpost", Scope.Planet, Scope.Fleet //?
        ;"closest_system", Scope.Fleet, Scope.GalacticObject
        ;"pop_faction", Scope.Pop, Scope.PopFaction
        ;"planet", Scope.Pop, Scope.Planet
        ;"fleet", Scope.Ship, Scope.Fleet
        ;"owner", Scope.Pop, Scope.Country
        ;"owner", Scope.Ship, Scope.Country
        ;"owner", Scope.Planet, Scope.Country //?
        ;"leader", Scope.Country, Scope.Leader
        ;"every_fleet_in_system", Scope.GalacticObject, Scope.Fleet
        ;"every_system_ambient_object", Scope.GalacticObject, Scope.AmbientObject
        ;"every_system_planet", Scope.GalacticObject, Scope.Planet
        ;"random_system_ambient_object", Scope.GalacticObject, Scope.AmbientObject
        ;"random_system_planet", Scope.GalacticObject, Scope.Planet
        ;"any_bordering_country", Scope.GalacticObject, Scope.Country
        ;"any_neighbor_system", Scope.GalacticObject, Scope.GalacticObject
        ;"any_planet", Scope.GalacticObject, Scope.Planet
        ;"any_ship_in_system", Scope.GalacticObject, Scope.Ship
        ;"space_owner", Scope.GalacticObject, Scope.Country
        ;"every_neighboring_tile", Scope.Tile, Scope.Tile
        ;"random_neighboring_tile", Scope.Tile, Scope.Tile
        ;"any_neighboring_tile", Scope.Tile, Scope.Tile
        //;"any_attacker", Scope.War, Scope.Country
        //;"any_defender", Scope.War, Scope.Country
        ;"pop", Scope.Tile, Scope.Pop //? Added manually
        ;"tile", Scope.Pop, Scope.Tile //? Added manually
        ;"species", Scope.Leader, Scope.Species //? Added manually
        ;"species", Scope.Ship, Scope.Species //? Added manually
        ;"owner", Scope.Fleet, Scope.Country //?Added manually
        ;"owner", Scope.Country, Scope.Country //?Added manually
        ;"any_owned_ship", Scope.Fleet, Scope.Ship //?Added manually
        ;"solar_system", Scope.Fleet, Scope.GalacticObject //?Added manually
        ;"random_owned_pop", Scope.Planet, Scope.Pop //?Added manually
        ;"any_owned_pop", Scope.Planet, Scope.Pop //?Added manually
        ;"every_owned_pop", Scope.Planet, Scope.Pop//?added manually
        ;"leader", Scope.Fleet, Scope.Leader //?Added manually
        ;"planet", Scope.Planet, Scope.Planet //?orbit?
        ;"species", Scope.Planet, Scope.Species //?Added manually
        ;"solar_system", Scope.Ship, Scope.GalacticObject //?Added manually
        ;"leader", Scope.Ship, Scope.Leader //?Added manually
        ;"closest_system", Scope.GalacticObject, Scope.GalacticObject //?Added manually
        ;"home_planet", Scope.Country, Scope.Planet//?Added manually
        ;"closest_system", Scope.Ship, Scope.GalacticObject//?Added manually
        ;"solar_system", Scope.AmbientObject, Scope.GalacticObject//?Added manually
        ;"species", Scope.Pop, Scope.Species //?Added manually
        ;"leader", Scope.Planet, Scope.Leader //?Added manually
        ;"closest_system", Scope.Planet, Scope.GalacticObject //?Added manually
        ;"closest_system", Scope.Country, Scope.GalacticObject //?Added manually
        ;"species", Scope.Country, Scope.Species //?Added manuallu
        ;"last_created_ship", Scope.Any, Scope.Ship //?Added manually
        ;"random_owned_ship", Scope.Fleet, Scope.Ship //?Added manually
        ;"ruler", Scope.Country, Scope.Leader//?Added manually
        ;"sector", Scope.Planet, Scope.Sector//??Added manullay?
        ;"root", Scope.Any, Scope.Any//??Added manullay?

        ]

    type ScopeResult =
        | NewScope of newScope : Scope
        | WrongScope of expected : Scope list
        | NotFound

    let changeScope (scope : string) source = 
        let possibles = scopes |> List.filter (fun (n, _, _) -> n.ToLower() = scope.ToLower())
        let exact = possibles |> List.tryFind (fun (_, s, _) -> s = source || s = Scope.Any)
        let possiblesScopes = possibles |> List.map (fun (_, s, _) -> s)
        match possiblesScopes, exact with
        | [], _ -> NotFound
        | _, Some (_, _, d) -> NewScope d
        | ss, None -> WrongScope ss

    // let changeScope (scope : string) source = scopes 
    //                                             |> List.tryFind (fun (n, s, _) -> n.ToLower() = scope.ToLower() && s = source)
    //                                             |> Option.bind (fun (_, _, d) -> Some d)
    let sourceScope (scope : string) = scopes
                                    |> List.choose (function | (n, s, _) when n.ToLower() = scope.ToLower() -> Some s |_ -> None)
                                    |> (function |x when List.contains Scope.Any x -> Some allScopes |[] -> None |x -> Some x)