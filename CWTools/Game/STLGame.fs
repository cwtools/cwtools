namespace CWTools.Games

open CWTools.Parser
open System.IO
open FParsec
open CWTools.Process
open CWTools.Process.STLProcess
open CWTools.Validation.STLValidation
open CWTools.Validation.ValidationCore


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
                    //"common/defines";
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
        let allFiles = folders |> List.map (fun f -> gameDirectory + "/" + f)
                               |> List.map (fun f -> f, (if Directory.Exists f then Directory.EnumerateFiles f else Seq.empty )|> List.ofSeq)
                               |> List.collect (fun (f, fs) -> fs |> List.map (fun x -> f, x))
                               |> List.filter (fun (fl, f) -> Path.GetExtension(f) = ".txt")
                               |> Map.ofList
        let parseResults = 
            let matchResult (k, f) = 
                match f with
                |Success(parsed, _, _) -> Pass(k, parsed)
                |Failure(msg, _,_) -> Fail(k, msg)
            allFiles |> Map.toList 
                |> List.map ((fun (k, f) -> k, CKParser.parseFile f) >> matchResult)

        let entities = parseResults 
                        |> List.choose (function |Pass(_,parsed) -> Some parsed |_ -> None) 
                        |> List.collect id
                        |> STLProcess.shipProcess.ProcessNode<Node>() "root"
                        |> (fun n -> n.Children)

        let findDuplicates (sl : Statement list) =
            let node = ProcessCore.processNodeBasic "root" sl
            node.Children |> List.groupBy (fun c -> c.Key)
                          |> List.filter (fun (k,v) -> v.Length > 1)
                          |> List.map (fun (k,v) -> k)

        let validateDuplicates =
            parseResults |> List.choose (function |Pass(k,parsed) -> Some (k, parsed) |_ -> None)
                |> List.groupBy (fun (k,p) -> k)
                |> List.map (fun (k, vs) -> k, List.collect (fun (_, vs2) -> vs2) vs)
                |> List.map (fun (k, vs) -> k, findDuplicates vs)

        let validateShips = 
            let ships = entities |> List.choose (function | :? Ship as s -> Some s |_ -> None)
            ships |> List.map validateShip
                  |> List.choose (function |Invalid es -> Some es |_ -> None)
                  |> List.collect id



        member __.Results = parseResults
        member __.Duplicates = validateDuplicates
        member __.ValidationErrors = validateShips