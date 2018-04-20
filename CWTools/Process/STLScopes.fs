namespace CWTools.Process

open NodaTime.TimeZones
open System
open CWTools.Localisation
module STLScopes =
    open CWTools.Common.STLConstants
    open CWTools.Utilities.Utils
    open Microsoft.FSharp.Collections.Tagged



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
        ScopedEffect("space_owner", [Scope.GalacticObject; Scope.Planet], Scope.Country, EffectType.Both, defaultDesc, ""); //Planet from MoreEventsMod
        ScopedEffect("overlord", [Scope.Country], Scope.Country, EffectType.Both, defaultDesc, "");
        ScopedEffect("defender", [Scope.War], Scope.Country, EffectType.Both, defaultDesc, "");
        ScopedEffect("attacker", [Scope.War], Scope.Country, EffectType.Both, defaultDesc, "");
        ScopedEffect("owner", [Scope.Ship; Scope.Pop; Scope.Fleet; Scope.Planet; Scope.PopFaction; Scope.Sector; Scope.Leader; Scope.Country; Scope.Starbase; Scope.Tile; Scope.GalacticObject], Scope.Country, EffectType.Both, "", ""); //Fleet, Planet, PopFaction, Sector, Leader, Country, Tile from vanilla use
        ScopedEffect("controller", [Scope.Planet], Scope.Country, EffectType.Both, "", ""); //Planet from vanilla use
        ScopedEffect("planet_owner", [Scope.Planet], Scope.Country, EffectType.Both, defaultDesc, "");
        ScopedEffect("last_created_country", allScopes, Scope.Country, EffectType.Both, defaultDesc, "");
        ScopedEffect("leader", [Scope.Ship; Scope.Planet; Scope.Country; Scope.PopFaction; Scope.Fleet; Scope.Sector], Scope.Leader, EffectType.Both, "", ""); //PopFaction, Fleet from vannilla use, Sector from Iyur
        ScopedEffect("last_created_leader", allScopes, Scope.Leader, EffectType.Both, defaultDesc, "");
        ScopedEffect("ruler", [Scope.Country], Scope.Leader, EffectType.Both, defaultDesc, "");
        ScopedEffect("heir", [], Scope.Leader, EffectType.Both, defaultDesc, "");
        ScopedEffect("solar_system", [Scope.Ship; Scope.Planet; Scope.Country; Scope.Fleet; Scope.AmbientObject; Scope.Tile; Scope.Starbase], Scope.GalacticObject, EffectType.Both, "", ""); //Fleet, Ambient object, Tile from vanilla use
        ScopedEffect("last_created_system", allScopes, Scope.GalacticObject, EffectType.Both, defaultDesc, "");
        ScopedEffect("planet", [Scope.Pop; Scope.Tile; Scope.Planet], Scope.Planet, EffectType.Both, defaultDesc, "");
        ScopedEffect("capital_scope", [Scope.Country], Scope.Planet, EffectType.Both, defaultDesc, "");
        ScopedEffect("orbit", [Scope.Fleet; Scope.Ship], Scope.Planet, EffectType.Both, defaultDesc, "");
        ScopedEffect("home_planet", [Scope.Country; Scope.Species; Scope.Planet], Scope.Planet, EffectType.Both, defaultDesc, "");
        ScopedEffect("star", [Scope.Planet; Scope.Ship; Scope.Fleet; Scope.AmbientObject; Scope.Megastructure; Scope.GalacticObject; Scope.Starbase], Scope.Planet, EffectType.Both, defaultDesc, "");
        ScopedEffect("last_created_ship", allScopes, Scope.Ship, EffectType.Both, defaultDesc, "");
        //ScopedEffect("spaceport", [Scope.Planet], Scope.Fleet, EffectType.Both, defaultDesc, ""); Removed in 2.0
        ScopedEffect("mining_station", [Scope.Planet], Scope.Fleet, EffectType.Both, defaultDesc, "");
        ScopedEffect("research_station", [Scope.Planet], Scope.Fleet, EffectType.Both, defaultDesc, "");
        ScopedEffect("last_created_fleet", allScopes, Scope.Fleet, EffectType.Both, defaultDesc, "");
        ScopedEffect("fleet", [Scope.Ship; Scope.Starbase], Scope.Fleet, EffectType.Both, defaultDesc, "");
        ScopedEffect("pop", [Scope.Tile], Scope.Pop, EffectType.Both, defaultDesc, "");
        ScopedEffect("last_created_pop", allScopes, Scope.Pop, EffectType.Both, defaultDesc, "");
        ScopedEffect("last_created_ambient_object", allScopes, Scope.AmbientObject, EffectType.Both, defaultDesc, "");
        ScopedEffect("last_created_army", allScopes, Scope.Army, EffectType.Both, defaultDesc, "");
        ScopedEffect("tile", [Scope.Pop], Scope.Tile, EffectType.Both, defaultDesc, "");
        ScopedEffect("orbital_deposit_tile", [Scope.Planet], Scope.Tile, EffectType.Both, defaultDesc, "");
        ScopedEffect("best_tile_for_pop", [Scope.Planet], Scope.Tile, EffectType.Both, defaultDesc, "");
        ScopedEffect("owner_species", [Scope.Country], Scope.Species, EffectType.Both, defaultDesc, "");
        ScopedEffect("last_created_species", allScopes, Scope.Species, EffectType.Both, defaultDesc, "");
        ScopedEffect("species", [Scope.Country; Scope.Ship; Scope.Leader; Scope.Pop], Scope.Species, EffectType.Both, defaultDesc, "");
        ScopedEffect("pop_faction", [Scope.Pop; Scope.Leader], Scope.PopFaction, EffectType.Both, defaultDesc, ""); //Leader from vanilla
        ScopedEffect("last_created_pop_faction", allScopes, Scope.PopFaction, EffectType.Both, defaultDesc, "");
        ScopedEffect("core_sector", [], Scope.Sector, EffectType.Both, defaultDesc, "");
        ScopedEffect("sector", [Scope.Planet; Scope.GalacticObject], Scope.Sector, EffectType.Both, defaultDesc, "");
        ScopedEffect("alliance", [], Scope.Alliance, EffectType.Both, defaultDesc, "");
        ScopedEffect("starbase", [Scope.GalacticObject; Scope.Planet], Scope.Starbase, EffectType.Both, defaultDesc, "");
        ScopedEffect("last_refugee_country", allScopes, Scope.Country, EffectType.Both, defaultDesc, "");
        ScopedEffect("system_star", [Scope.GalacticObject], Scope.Planet, EffectType.Both, defaultDesc, "")
    ]
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
    let effectInnerScopes =[ 
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
    let effectInnerScopeFunctions = [
        "if", id, []
        "while", id, []
        "any_neighbor_system", (fun _ -> Scope.GalacticObject), ["ignore_hyperlanes"];
        "every_neighbor_system", (fun _ -> Scope.GalacticObject), ["ignore_hyperlanes"];
        "random_neighbor_system", (fun _ -> Scope.GalacticObject), ["ignore_hyperlanes"];
    ]

    let addInnerScope (des : DocEffect list) =
        let withSimple =
             des |> List.map (fun de ->
                match effectInnerScopes |> List.tryPick (function | (n, t) when n = de.Name -> Some (fun _ -> t) |_ -> None) with
                | Some t -> ScopedEffect(de, t, true, []) :> DocEffect
                | None -> de) 
        withSimple |> List.map (fun de ->
                match effectInnerScopeFunctions |> List.tryPick (function | (n, t, s) when n = de.Name -> Some (t, s) |_ -> None) with
                | Some (t, s) -> ScopedEffect(de, t, false, s) :> DocEffect
                | None -> de)
        

    type ScopeContext =
        {
            Root : Scope
            From : Scope list
            Scopes : Scope list
        }
        member this.CurrentScope = match this.Scopes with |[] -> Scope.Any |x::_ -> x
        member this.PopScope = match this.Scopes with |[] -> [] |_::xs -> xs

    type ScopeResult =
        | NewScope of newScope : ScopeContext * ignoreKeys : string list
        | WrongScope of expected : Scope list
        | NotFound

    let oneToOneScopes = 
        let from = fun (s, change) -> {s with Scopes = Scope.Any::s.Scopes}, true
        let prev = fun (s, change) -> {s with Scopes = s.PopScope}, true
        [
        "THIS", id;
        "ROOT", fun (s, change) -> {s with Scopes = s.Root::s.Scopes}, true;
        "FROM", from; //TODO Make it actually use FROM
        "FROMFROM", from >> from;
        "FROMFROMFROM", from >> from >> from;
        "FROMFROMFROMFROM", from >> from >> from >> from;
        "PREV", prev;
        "PREVPREV", prev >> prev;
        "PREVPREVPREV", prev >> prev >> prev;
        "PREVPREVPREVPREV", prev >> prev >> prev >> prev
        "AND", id;
        "OR", id;
        "NOR", id;
        "NOT", id;
        "hidden_effect", id;
        "hidden_trigger", id;
    ]
    type EffectMap = Map<string, Effect, STLStringComparer>

    let changeScope (effects : EffectMap) (triggers : EffectMap) (key : string) (source : ScopeContext) = 
        let key = if key.StartsWith("hidden:", StringComparison.OrdinalIgnoreCase) then key.Substring(7) else key
        if key.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase) then NewScope ({ source with Scopes = Scope.Any::source.Scopes }, [])
        else
            let keys = key.Split('.')
            let inner ((context : ScopeContext), (changed : bool)) (nextKey : string) =
                let onetoone = oneToOneScopes |> List.tryFind (fun (k, _) -> k == nextKey)
                match onetoone with
                | Some (_, f) -> f (context, false), NewScope (f (context, false) |> fst, [])
                | None ->
                    let effectMatch = effects.TryFind nextKey |> Option.bind (function | :? ScopedEffect as e -> Some e |_ -> None)
                    let triggerMatch = triggers.TryFind nextKey |> Option.bind (function | :? ScopedEffect as e -> Some e |_ -> None)
                    // let effect = (effects @ triggers) 
                    //             |> List.choose (function | :? ScopedEffect as e -> Some e |_ -> None)
                    //             |> List.tryFind (fun e -> e.Name == nextKey)
                    match Option.orElse effectMatch triggerMatch with
                    | None -> (context, false), NotFound
                    | Some e -> 
                        let possibleScopes = e.Scopes
                        let exact = possibleScopes |> List.contains context.CurrentScope
                        match context.CurrentScope, possibleScopes, exact, e.IsScopeChange with
                        | Scope.Any, _, _, true -> ({context with Scopes = e.InnerScope context.CurrentScope::context.Scopes}, true), NewScope ({context with Scopes = e.InnerScope context.CurrentScope::context.Scopes}, e.IgnoreChildren)
                        | Scope.Any, _, _, false -> (context, false), NewScope (context, e.IgnoreChildren)
                        | _, [], _, _ -> (context, false), NotFound
                        | _, _, true, true -> ({context with Scopes = e.InnerScope context.CurrentScope::context.Scopes}, true), NewScope ({context with Scopes = e.InnerScope context.CurrentScope::context.Scopes}, e.IgnoreChildren)
                        | _, _, true, false -> (context, false), NewScope (context, e.IgnoreChildren)
                        | _, ss, false, _ -> (context, false), WrongScope ss
            let inner2 = fun a b -> inner a b |> (fun (c, d) -> c, Some d)
            let res = keys |> Array.fold (fun ((c,b), r) k -> match r with |None -> inner2 (c, b) k |Some (NewScope (x, i)) -> inner2 (x, b) k |Some x -> (c,b), Some x) ((source, false), None)// |> snd |> Option.defaultValue (NotFound)
            let res2 =
                match res with
                |(_, _), None -> NotFound
                |(_, true), Some r -> r |> function |NewScope (x, i) -> NewScope ({ source with Scopes = x.CurrentScope::source.Scopes }, i) |x -> x
                |(_, false), Some r -> r
            // let x = res |> function |NewScope x -> NewScope { source with Scopes = x.CurrentScope::source.Scopes } |x -> x
            // x
            res2

    // let sourceScope (scope : string) = scopes
    //                                 |> List.choose (function | (n, s, _) when n == scope -> Some s |_ -> None)
    //                                 |> (function |x when List.contains Scope.Any x -> Some allScopes |[] -> None |x -> Some x)
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
        keys |> List.fold (fun acc k -> match acc with |Some e -> Some e |None -> inner k) None |> Option.defaultValue allScopes
    
    type UsageScopeContext = Scope list
    type ContextResult =
    | Found of string * (Scope list)
    | LocNotFound of string
    //| Failed
    type LocEntry = {
        key : string
        value : char option
        desc : string
        position : FParsec.Position
        scopes : ContextResult list
        refs : string list
    }
    let scopedLocEffects = [
        ScopedEffect("capital", allScopes, Scope.Planet, EffectType.Both, defaultDesc, "");
        ScopedEffect("capital_scope", allScopes, Scope.Planet, EffectType.Both, defaultDesc, "");
        ScopedEffect("system", allScopes, Scope.GalacticObject, EffectType.Both, defaultDesc, "");
        ScopedEffect("solar_system", allScopes, Scope.GalacticObject, EffectType.Both, defaultDesc, "");
        ScopedEffect("owner", [Scope.Ship; Scope.Pop; Scope.Fleet; Scope.Planet; Scope.PopFaction; Scope.Sector; Scope.Leader; Scope.Country; Scope.Starbase; Scope.Tile; Scope.GalacticObject], Scope.Country, EffectType.Both, "", "");
        ScopedEffect("planet", [Scope.Pop; Scope.Tile; Scope.Planet], Scope.Planet, EffectType.Both, defaultDesc, "");
        ScopedEffect("leader", [Scope.Ship; Scope.Planet; Scope.Country; Scope.PopFaction; Scope.Fleet; Scope.Sector], Scope.Leader, EffectType.Both, "", "");
        ScopedEffect("species", [Scope.Country; Scope.Ship; Scope.Leader; Scope.Pop], Scope.Species, EffectType.Both, defaultDesc, "");
        ScopedEffect("fleet", [Scope.Ship; Scope.Starbase], Scope.Fleet, EffectType.Both, defaultDesc, "");
        ScopedEffect("ship", [Scope.Leader], Scope.Ship, EffectType.Both, defaultDesc, "");
        ScopedEffect("ruler", [Scope.Country], Scope.Leader, EffectType.Both, defaultDesc, "");
        ScopedEffect("sector", [Scope.Planet; Scope.GalacticObject], Scope.Sector, EffectType.Both, defaultDesc, "");
    ]
    let scopedLocEffectsMap = EffectMap.FromList(STLStringComparer(), scopedLocEffects |> List.map (fun se -> se.Name, se :> Effect))

    
    let locPrimaryScopes = 
        let from = fun (s, change) -> {s with Scopes = Scope.Any::s.Scopes}, true
        let prev = fun (s, change) -> {s with Scopes = s.PopScope}, true
        [
        "THIS", id;
        "ROOT", fun (s, change) -> {s with Scopes = s.Root::s.Scopes}, true;
        "FROM", from; //TODO Make it actually use FROM
        "FROMFROM", from >> from;
        "FROMFROMFROM", from >> from >> from;
        "FROMFROMFROMFROM", from >> from >> from >> from;
        "PREV", prev;
        "PREVPREV", prev >> prev;
        "PREVPREVPREV", prev >> prev >> prev;
        "PREVPREVPREVPREV", prev >> prev >> prev >> prev
        "Recipient", id;
        "Actor", id;
        "Third_party", id;
        ]
    
    let localisationCommandContext (commands : string list) (eventtargets : string list) (setvariables : string list) (entry : Entry) (command : string) =
        let keys = command.Split('.') |> List.ofArray
        let inner ((first : bool), (rootScope : string), (scopes : Scope list)) (nextKey : string) =
            let onetoone = locPrimaryScopes |> List.tryFind (fun (k, _) -> k == nextKey)
            match onetoone with
            | Some (_) -> Found (nextKey, []), false
            | None -> 
                let effectMatch = scopedLocEffectsMap.TryFind nextKey |> Option.bind (function | :? ScopedEffect as e -> Some e |_ -> None)
                match effectMatch with
                | Some e -> Found (rootScope, e.Scopes), false
                | None -> 
                    let matchedCommand = (commands)  |> List.tryFind (fun c -> c == nextKey)
                    match matchedCommand, first, nextKey.StartsWith("parameter:", StringComparison.OrdinalIgnoreCase) with
                    |Some _, _, _ -> Found (rootScope, scopes), false
                    | _, _, true -> Found (rootScope, scopes), false
                    |None, false, false -> 
                        match setvariables |> List.exists (fun sv -> sv == nextKey) with
                        | true -> Found (rootScope, scopes), false
                        | false -> LocNotFound (nextKey), false
                    |None, true, false ->
                        match eventtargets |> List.exists (fun et -> et == nextKey) with
                        | true -> Found (rootScope, scopes), false
                        | false -> LocNotFound (nextKey), false
        keys |> List.fold (fun r k -> match r with | (Found (r, s) , f) -> inner ((f, r, s)) k |LocNotFound s, _ -> LocNotFound s, false) (Found ("this", []), true) |> fst                
        //keys |> List.fold (fun (e, r, s) k -> if e then (e, r, s) else inner (e, r, s) k) (false, "this", [])
