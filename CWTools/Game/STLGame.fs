namespace CWTools.Games

open CWTools.Parser
open System.IO
open FParsec
open CWTools.Process
open CWTools.Process.STLProcess
open CWTools.Validation.STLValidation
open CWTools.Validation.ValidationCore
open System.Text
open FSharp.Collections.ParallelSeq


type FileResult =
    |Pass of string * Statement list * int64
    |Fail of string * string * Position * int64
    
type FilesScope =
    |All
    |Mods
    |Vanilla
type STLGame ( gameDirectory : string, scope : FilesScope, modFilter : string, triggers : Effect list, effects : Effect list ) =
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

        let duration f = 
            let timer = System.Diagnostics.Stopwatch()
            timer.Start()
            let returnValue = f()
            (returnValue  , timer.ElapsedMilliseconds)  

        let mods = if not (Directory.Exists (gameDirectory + "/mod")) then [] else
                    Directory.EnumerateFiles (gameDirectory + "/mod") 
                        |> List.ofSeq
                        |> List.filter (fun f -> Path.GetExtension(f) = ".mod")
                        |> List.map CKParser.parseFile
                        |> List.choose ( function | Success(p, _, _) -> Some p | _ -> None )
                        |> List.map ((ProcessCore.processNodeBasic "mod" Position.Empty) >> (fun s -> s.TagText "name", s.TagText "path"))

        //let modFolders = Directory.EnumerateDirectories (gameDirectory + "/mod") |> List.ofSeq
        let modFolders = mods |> List.filter (fun (n, _) -> n.Contains(modFilter)) |> List.map (fun (n, p) -> n, (gameDirectory + "/" + p))
        let allFolders = 
            match scope with
            |All -> gameDirectory :: (modFolders |> List.map snd)
            |Mods -> (modFolders |> List.map snd)
            |Vanilla -> [gameDirectory]
       
        let allFiles = 
            let getAllFiles path =
                folders |> List.map (fun f -> path + "/" + f)
                       |> List.map (fun f -> f, (if Directory.Exists f then Directory.EnumerateFiles f else Seq.empty )|> List.ofSeq)
                       |> List.collect (fun (f, fs) -> fs |> List.map (fun x -> f, x))
                       |> List.filter (fun (fl, f) -> Path.GetExtension(f) = ".txt")
            List.map getAllFiles allFolders |> List.collect id

        let parseResults = 
            let matchResult (k, (f, t)) = 
                match f with
                |Success(parsed, _, _) -> Pass(k, parsed, t)
                |Failure(msg, pe,_) -> Fail(k, msg, pe.Position, t)
            allFiles |> PSeq.map ((fun (k, f) -> f, (fun t -> duration (fun () -> CKParser.parseFile t)) (Path.GetFullPath(f))) >> matchResult)
                     |> PSeq.toList

        let entities = parseResults 
                        |> List.choose (function |Pass(f,parsed,t) -> Some (f,parsed,t) |_ -> None) 
                        //|> List.collect id
                        |> List.map (fun (f, parsed, _) -> (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) parsed))
        let flatEntities = entities |> List.map (fun n -> n.Children) |> List.collect id

        let scriptedTriggers = 
            let rawTriggers = 
                parseResults
                |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some (f, p, t) |_ -> None)
                //|> List.collect (fun (f, passed, t) -> List.map (fun p -> f, p, t) passed)
                |> List.map (fun (f, parsed, _) -> (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) parsed))
                |> List.collect (fun n -> n.Children)
                |> List.rev
            let firstPass = rawTriggers |> List.fold (fun ts t -> (STLProcess.getScriptedTriggerScope ts ts t)::ts) triggers
            let secondPass = rawTriggers |> List.fold (fun ts t -> (STLProcess.getScriptedTriggerScope ts ts t)::ts) firstPass
            secondPass


        let scriptedEffects =
            parseResults
            |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_effects") -> Some (f, p, t) |_ -> None)
            //|> List.collect (fun (f, passed, t) -> List.map (fun p -> f, p, t) passed)
            |> List.map (fun (f, parsed, _) -> (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) parsed))
            |> List.collect (fun n -> n.Children)
            |> List.rev
            |> List.fold (fun es e -> (STLProcess.getScriptedTriggerScope es scriptedTriggers e)::es) effects
                      

        let findDuplicates (sl : Statement list) =
            let node = ProcessCore.processNodeBasic "root" Position.Empty sl
            node.Children |> List.groupBy (fun c -> c.Key)
                          |> List.filter (fun (k,v) -> v.Length > 1)
                          |> List.map (fun (k,v) -> k)

        let validateDuplicates =
            parseResults |> List.choose (function |Pass(k,parsed,t) -> Some (k, parsed) |_ -> None)
                |> List.groupBy (fun (k,p) -> k)
                |> List.map (fun (k, vs) -> k, List.collect (fun (_, vs2) -> vs2) vs)
                |> List.map (fun (k, vs) -> k, findDuplicates vs)

        let validateShips = 
            let ships = flatEntities |> List.choose (function | :? Ship as s -> Some s |_ -> None)
            ships |> List.map validateShip
                  |> List.choose (function |Invalid es -> Some es |_ -> None)
                  |> List.collect id
                  |> List.map (fun (n, s) -> n :> Node , s)

        let parseErrors = parseResults
                        |> List.choose (function |Fail(f,e,p,t) -> Some (f,e,p) |_ -> None)
                        
        let validateFiles =
            entities |> List.map validateVariables
                     |> List.choose (function |Invalid es -> Some es |_ -> None)
                     |> List.collect id

        let validateEvents =
            let events = flatEntities |> List.choose (function | :? Event as e -> Some e |_ -> None)
            events |> List.map (fun e -> (valEventTriggers (triggers @ scriptedTriggers) (effects @ scriptedEffects) e) <&&> (valEventEffects (triggers @ scriptedTriggers) (effects @ scriptedEffects) e))
                   |> List.choose (function |Invalid es -> Some es |_ -> None)
                   |> List.collect id
                   |> List.map (fun (n, s) -> n :> Node, s)

        let validateAll = validateShips @ validateFiles @ validateEvents


        member __.Results = parseResults
        member __.Duplicates = validateDuplicates
        member __.ParserErrors = parseErrors
        member __.ValidationErrors = validateAll
        //member __.ValidationWarnings = warningsAll
        member __.Entities = entities
        member __.Folders = allFolders
        member __.AllFiles = parseResults |> List.map (function |Fail(f,_,_,t) -> (f, false, t) |Pass(f,_,t) -> (f, true, t))
        member __.ScripteTriggers = scriptedTriggers
        member __.ScriptedEffects = scriptedEffects
        //member __.ScriptedTriggers = parseResults |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some p |_ -> None) |> List.map (fun t -> )