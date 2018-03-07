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
    let defaultDesc = "Scope (/context) switch"
    let scopedEffects = [
        ScopedEffect("space_owner", [Scope.GalacticObject; Scope.Planet], Scope.Country, EffectType.Both, "", ""); //Planet from MoreEventsMod
        ScopedEffect("overlord", [Scope.Country], Scope.Country, EffectType.Both, "", "");
        ScopedEffect("defender", [Scope.War], Scope.Country, EffectType.Both, "", "");
        ScopedEffect("attacker", [Scope.War], Scope.Country, EffectType.Both, "", "");
        ScopedEffect("owner", [Scope.Ship; Scope.Pop; Scope.Fleet; Scope.Planet; Scope.PopFaction; Scope.Sector; Scope.Leader; Scope.Country; Scope.Starbase], Scope.Country, EffectType.Both, "", ""); //Fleet, Planet, PopFaction, Sector, Leader, Country from vanilla use
        ScopedEffect("controller", [Scope.Planet], Scope.Country, EffectType.Both, "", ""); //Planet from vanilla use
        ScopedEffect("planet_owner", [Scope.Planet], Scope.Country, EffectType.Both, "", "");
        ScopedEffect("last_created_country", allScopes, Scope.Country, EffectType.Both, "", "");
        ScopedEffect("last_refugee_country", [], Scope.Country, EffectType.Both, "", "");
        ScopedEffect("leader", [Scope.Ship; Scope.Planet; Scope.Country; Scope.PopFaction; Scope.Fleet], Scope.Leader, EffectType.Both, "", ""); //PopFaction, Fleet from vanilla use
        ScopedEffect("last_created_leader", allScopes, Scope.Leader, EffectType.Both, "", "");
        ScopedEffect("ruler", [Scope.Country], Scope.Leader, EffectType.Both, "", "");
        ScopedEffect("heir", [], Scope.Leader, EffectType.Both, "", "");
        ScopedEffect("solar_system", [Scope.Ship; Scope.Planet; Scope.Country; Scope.Fleet; Scope.AmbientObject], Scope.GalacticObject, EffectType.Both, "", ""); //Fleet, Ambient object from vanilla use
        ScopedEffect("last_created_system", allScopes, Scope.GalacticObject, EffectType.Both, "", "");
        ScopedEffect("planet", [Scope.Pop; Scope.Tile; Scope.Planet], Scope.Planet, EffectType.Both, "", "");
        ScopedEffect("capital_scope", [Scope.Country], Scope.Planet, EffectType.Both, "", "");
        ScopedEffect("orbit", [Scope.Fleet; Scope.Ship], Scope.Planet, EffectType.Both, "", "");
        ScopedEffect("home_planet", [Scope.Country; Scope.Species; Scope.Planet], Scope.Planet, EffectType.Both, "", "");
        ScopedEffect("star", [Scope.Planet; Scope.Ship; Scope.Fleet; Scope.AmbientObject; Scope.Megastructure; Scope.GalacticObject; Scope.Starbase], Scope.Planet, EffectType.Both, "", "");
        ScopedEffect("last_created_ship", allScopes, Scope.Ship, EffectType.Both, "", "");
        ScopedEffect("spaceport", [Scope.Planet], Scope.Fleet, EffectType.Both, "", "");
        ScopedEffect("mining_station", [Scope.Planet], Scope.Fleet, EffectType.Both, "", "");
        ScopedEffect("research_station", [Scope.Planet], Scope.Fleet, EffectType.Both, "", "");
        ScopedEffect("last_created_fleet", allScopes, Scope.Fleet, EffectType.Both, "", "");
        ScopedEffect("fleet", [Scope.Ship; Scope.Starbase], Scope.Fleet, EffectType.Both, "", "");
        ScopedEffect("pop", [Scope.Tile], Scope.Pop, EffectType.Both, "", "");
        ScopedEffect("last_created_pop", allScopes, Scope.Pop, EffectType.Both, "", "");
        ScopedEffect("last_created_ambient_object", allScopes, Scope.AmbientObject, EffectType.Both, "", "");
        ScopedEffect("last_created_army", allScopes, Scope.Army, EffectType.Both, "", "");
        ScopedEffect("tile", [Scope.Pop], Scope.Tile, EffectType.Both, "", "");
        ScopedEffect("orbital_deposit_tile", [Scope.Planet], Scope.Tile, EffectType.Both, "", "");
        ScopedEffect("best_tile_for_pop", [Scope.Planet], Scope.Tile, EffectType.Both, "", "");
        ScopedEffect("owner_species", [Scope.Country], Scope.Species, EffectType.Both, "", "");
        ScopedEffect("last_created_species", allScopes, Scope.Species, EffectType.Both, "", "");
        ScopedEffect("species", [Scope.Country; Scope.Ship; Scope.Leader; Scope.Pop], Scope.Species, EffectType.Both, "", "");
        ScopedEffect("pop_faction", [Scope.Pop], Scope.PopFaction, EffectType.Both, "", "");
        ScopedEffect("last_created_pop_faction", allScopes, Scope.PopFaction, EffectType.Both, "", "");
        ScopedEffect("core_sector", [], Scope.Sector, EffectType.Both, "", "");
        ScopedEffect("sector", [Scope.Planet], Scope.Sector, EffectType.Both, "", "");
        ScopedEffect("alliance", [], Scope.Alliance, EffectType.Both, "", "");
        ScopedEffect("starbase", [Scope.GalacticObject; Scope.Planet], Scope.Starbase, EffectType.Both, "", "");
    ]
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
        "any_ship",  Scope.Ship;
        "every_ship",  Scope.Ship;
        "random_ship",  Scope.Ship;

        "any_pop",  Scope.Pop;
        "every_pop",  Scope.Pop;
        "random_pop",  Scope.Pop;

        "any_owned_ship",  Scope.Ship;
        "every_owned_ship",  Scope.Ship;
        "random_owned_ship",  Scope.Ship;

        "any_owned_planet",  Scope.Planet;
        "every_owned_planet",  Scope.Planet;
        "random_owned_planet",  Scope.Planet;

        "any_controlled_planet",  Scope.Planet;
        "every_controlled_planet",  Scope.Planet;
        "random_controlled_planet",  Scope.Planet;

        "any_war_defender",  Scope.Country;
        "every_war_defender",  Scope.Country;
        "random_war_defender",  Scope.Country;

        "any_war_attacker",  Scope.Country;
        "every_war_attacker",  Scope.Country;
        "random_war_attacker",  Scope.Country;
                
        "any_planet",  Scope.Planet;
        "every_planet",  Scope.Planet;
        "random_planet",  Scope.Planet;
                
        "any_planet_within_border",  Scope.Planet;
        "every_planet_within_border",  Scope.Planet;
        "random_planet_within_border",  Scope.Planet;
                
        "any_ambient_object",  Scope.AmbientObject;
        "every_ambient_object",  Scope.AmbientObject;
        "random_ambient_object",  Scope.AmbientObject;
                
        "any_system_ambient_object",  Scope.AmbientObject;
        "every_system_ambient_object",  Scope.AmbientObject;
        "random_system_ambient_object",  Scope.AmbientObject;
                
        "any_mining_station",  Scope.Fleet;
        "every_mining_station",  Scope.Fleet;
        "random_mining_station",  Scope.Fleet;
                
        "any_research_station",  Scope.Fleet;
        "every_research_station",  Scope.Fleet;
        "random_research_station",  Scope.Fleet;
                
        "any_spaceport",  Scope.Fleet;
        "every_spaceport",  Scope.Fleet;
        "random_spaceport",  Scope.Fleet;
                
        "any_system_planet",  Scope.Planet;
        "every_system_planet",  Scope.Planet;
        "random_system_planet",  Scope.Planet;
                
        "any_neighboring_tile",  Scope.Tile;
        "every_neighboring_tile",  Scope.Tile;
        "random_neighboring_tile",  Scope.Tile;
                
        "any_tile",  Scope.Tile;
        "every_tile",  Scope.Tile;
        "random_tile",  Scope.Tile;
                
        "any_neighbor_system",  Scope.GalacticObject;
        "every_neighbor_system",  Scope.GalacticObject;
        "random_neighbor_system",  Scope.GalacticObject;
                
        "any_moon",  Scope.Planet;
        "every_moon",  Scope.Planet;
        "random_moon",  Scope.Planet;
                
        "any_system_in_cluster",  Scope.GalacticObject;
        "every_system_in_cluster",  Scope.GalacticObject;
        "random_system_in_cluster",  Scope.GalacticObject;
                
        "any_owned_leader",  Scope.Leader;
        "every_owned_leader",  Scope.Leader;
        "random_owned_leader",  Scope.Leader;
                
        "any_sector",  Scope.Sector;
        "every_sector",  Scope.Sector;
        "random_sector",  Scope.Sector;
                
        "any_owned_fleet",  Scope.Fleet;
        "every_owned_fleet",  Scope.Fleet;
        "random_owned_fleet",  Scope.Fleet;
                
        "any_fleet_in_system",  Scope.Fleet;
        "every_fleet_in_system",  Scope.Fleet;
        "random_fleet_in_system",  Scope.Fleet;
                
                //"any_pop_faction", allScopes, Scope.PopFaction; //Doesn't exist according to caligula
        "every_pop_faction",  Scope.PopFaction;
        "random_pop_faction",  Scope.PopFaction;
                
        "any_playable_country",  Scope.Country;
        "every_playable_country",  Scope.Country;
        "random_playable_country",  Scope.Country;
                
        "any_subject",  Scope.Country;
        "every_subject",  Scope.Country;
        "random_subject",  Scope.Country;
                
                ///The following are assumptions 
        "any_country",  Scope.Country;
        "every_country",  Scope.Country;
        "random_country",  Scope.Country;
                
        "any_army",  Scope.Army;
        "every_army",  Scope.Army;
        "random_army",  Scope.Army;
                
        "any_rim_system",  Scope.GalacticObject;
        "every_rim_system",  Scope.GalacticObject;
        "random_rim_system",  Scope.GalacticObject;
                
        "any_neighbor_country",  Scope.Country;
        "every_neighbor_country",  Scope.Country;
        "random_neighbor_country",  Scope.Country;
                
        "any_system_within_border",  Scope.GalacticObject;
        "every_system_within_border",  Scope.GalacticObject;
        "random_system_within_border",  Scope.GalacticObject;
                
        "any_planet_army",  Scope.Army;
        "every_planet_army",  Scope.Army;
        "random_planet_army",  Scope.Army;

        "last_created_ambient_object", Scope.AmbientObject;
        "last_created_country", Scope.Country;
        "last_created_fleet", Scope.Fleet;
        "last_created_leader", Scope.Leader;
        "last_created_pop", Scope.Pop;
        "last_created_species", Scope.Species

        ]
    let addInnerScope (des : DocEffect list) =
        des |> List.map (fun de ->
                match scopeLists |> List.tryPick (function | (n, t) when n = de.Name -> Some t |_ -> None) with
                | Some t -> ScopedEffect(de, t) :> DocEffect
                | None -> de) 

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
        ;"random_owned_pop", Scope.PopFaction, Scope.Pop//?added manually

        ]

 

    type ScopeContext =
        {
            Root : Scope
            From : Scope list
            Scopes : Scope list
        }
        member this.CurrentScope = match this.Scopes with |[] -> Scope.Any |x::_ -> x
        member this.PopScope = match this.Scopes with |[] -> [] |_::xs -> xs

    type ScopeResult =
        | NewScope of newScope : ScopeContext
        | WrongScope of expected : Scope list
        | NotFound

    let oneToOneScopes = 
        let from = fun s -> {s with Scopes = Scope.Any::s.Scopes}
        let prev = fun s -> {s with Scopes = s.PopScope}
        [
        "THIS", id;
        "ROOT", fun s -> {s with Scopes = s.Root::s.Scopes};
        "FROM", from; //TODO Make it actually use FROM
        "FROMFROM", from >> from;
        "FROMFROMFROM", from >> from >> from;
        "FROMFROMFROMFROM", from >> from >> from >> from;
        "PREV", prev;
        "PREVPREV", prev >> prev;
        "PREVPREVPREV", prev >> prev >> prev;
        "PREVPREVPREVPREV", prev >> prev >> prev >> prev
    ]
    let changeScope (effects : Effect list) (triggers : Effect list) (key : string) (source : ScopeContext) = 
        let key = if key.StartsWith("hidden:") then key.Substring(7) else key
        let keys = key.Split('.')
        let inner (context : ScopeContext) (nextKey : string) =
            let onetoone = oneToOneScopes |> List.tryFind (fun (k, f) -> k.ToLower() = nextKey.ToLower())
            match onetoone with
            | Some (_, f) -> f context, NewScope (f context)
            | None ->
                let effect = (effects @ triggers) 
                            |> List.choose (function | :? ScopedEffect as e -> Some e |_ -> None)
                            |> List.tryFind (fun e -> e.Name.ToLower() = nextKey.ToLower())
                match effect with
                | None -> context, NotFound
                | Some e -> 
                    let possibleScopes = e.Scopes
                    let exact = possibleScopes |> List.contains context.CurrentScope
                    match context.CurrentScope, possibleScopes, exact with
                    | Scope.Any, _, _ -> {context with Scopes = e.InnerScope::context.Scopes}, NewScope {context with Scopes = e.InnerScope::context.Scopes}
                    | _, [], _ -> context, NotFound
                    | _, _, true -> {context with Scopes = e.InnerScope::context.Scopes}, NewScope {context with Scopes = e.InnerScope::context.Scopes}
                    | _, ss, false -> context, WrongScope ss
        let inner2 = fun a b -> inner a b |> (fun (c, d) -> c, Some d)
        keys |> Array.fold (fun (c, r) k -> match r with |None -> inner2 c k |Some (NewScope x) -> inner2 x k |Some x -> c, Some x) (source, None) |> snd |> Option.defaultValue (NotFound)


    // let changeScope (scope : string) source = scopes 
    //                                             |> List.tryFind (fun (n, s, _) -> n.ToLower() = scope.ToLower() && s = source)
    //                                             |> Option.bind (fun (_, _, d) -> Some d)
    let sourceScope (scope : string) = scopes
                                    |> List.choose (function | (n, s, _) when n.ToLower() = scope.ToLower() -> Some s |_ -> None)
                                    |> (function |x when List.contains Scope.Any x -> Some allScopes |[] -> None |x -> Some x)

