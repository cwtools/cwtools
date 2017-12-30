namespace CWTools

open CWTools.Parser
open System.IO
open FParsec


type FileResult =
        |Pass of string * Statement list
        |Fail of string * string
    

type STLGame ( gameDirectory : string ) =
        let folders = [
                    "common/agendas";
                    "common/ambient_objects";
                    "common/anomalies";
                    "common/armies";
                    "common/army_attachments";
                    "common/attitudes";
                    "common/buildable_pops";
                    "common/buildings";
                    "common/colors";
                    "common/component_flags";
                    "common/component_sets";
                    "common/component_tags";
                    "common/component_templates";
                    "common/country_types";
                    "common/defines";
                    "common/deposits";
                    "common/diplo_phrases";
                    "common/diplomatic_actions";
                    "common/edicts";
                    "common/ethics";
                    "common/event_chains";
                    "common/fallen_empires";
                    "common/game_rules";
                    "common/global_ship_designs";
                    "common/governments";
                    "common/graphical_culture";
                    "common/mandates";
                    "common/name_lists";
                    "common/observation_station_missions";
                    "common/on_actions";
                    "common/opinion_modifiers";
                    "common/personalities";
                    "common/planet_classes";
                    "common/planet_modifiers";
                    "common/policies";
                    "common/pop_faction_types";
                    "common/precursor_civilizations";
                    "common/random_names";
                    "common/scripted_effects";
                    "common/scripted_triggers";
                    "common/section_templates";
                    "common/sector_types";
                    "common/ship_behaviors";
                    "common/ship_sizes";
                    "common/solar_system_initializers";
                    "common/spaceport_modules";
                    "common/special_projects";
                    "common/species_classes";
                    "common/start_screen_messages";
                    "common/static_modifiers";
                    "common/strategic_resources";
                    "common/subjects";
                    "common/technology";
                    "common/terraform";
                    "common/tile_blockers";
                    "common/traits";
                    "common/triggered_modifiers";
                    "common/war_demand_counters";
                    "common/war_demand_types";
                    "events";
                    "map/galaxy";
                    "map/setup_scenarios";
                    "prescripted_countries"
                    ]
        let allFiles = folders |> List.map (fun f -> f, if Directory.Exists f then Directory.EnumerateFiles (gameDirectory + "/" + f) else Seq.empty |> List.ofSeq)
                               |> List.collect (fun (f, fs) -> fs |> List.map (fun x -> f, x))
                               |> Map.ofList
        let parseResults = 
            let matchResult (k, f) = 
                match f with
                |Success(parsed, _, _) -> Pass(k, parsed)
                |Failure(msg, _,_) -> Fail(k, msg)
            allFiles |> Map.toList 
                |> List.map ((fun (k, f) -> k, CKParser.parseFile f) >> matchResult)

        member __.Results = parseResults
        