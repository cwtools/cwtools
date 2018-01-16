namespace CWTools.Process

module STLScopes =
    type Scope =
        |Planet
        |Country
        |Fleet
        |Pop
        |GalacticObject
        |Leader
        |Species
        |PopFaction
        |Sector
        |Ship
        |Army
        |AmbientObject
        |War
        |Tile
        |Any

    let allScopes = [
            Planet;
            Country;
            Fleet;
            Pop;
            GalacticObject;
            Leader;
            Species;
            PopFaction;
            Sector;
            Ship;
            Army;
            AmbientObject;
            War;
            Tile
            ]
    let parseScope =
        function    
        |"planet" -> Scope.Planet
        |"country" -> Scope.Country
        |"fleet" -> Scope.Fleet
        |"pop" -> Scope.Pop
        |"system"
        |"galacticobject"
        |"galactic_object" -> Scope.GalacticObject
        |"leader" -> Scope.Leader
        |"species" -> Scope.Species
        |"popfaction"
        |"pop_faction" -> Scope.PopFaction
        |"sector" -> Scope.Sector
        |"ship" -> Scope.Ship
        |"army" -> Scope.Army
        |"ambientobject"
        |"ambient_object" -> Scope.AmbientObject
        |"war" -> Scope.War
        |"tile" -> Scope.Tile
        |"any" -> Scope.Any
        |"all" -> Scope.Any
        |"alliance" -> Scope.Country
        |"megastructure" -> Scope.Ship
        |x -> failwith ("unexpected scope" + x.ToString())

    let parseScopes =
        function
        |"all" -> allScopes
        |x -> [parseScope x]

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
        ;"any_war", Scope.Country, Scope.War
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
        ;"any_attacker", Scope.War, Scope.Country
        ;"any_defender", Scope.War, Scope.Country
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