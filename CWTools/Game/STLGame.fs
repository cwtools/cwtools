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
open System.Collections.Generic


type PassFileResult = {
    statements : Statement list
    parseTime : int64
}
type FailFileResult = {
    error : string
    position : Position
    parseTime : int64
}
type FileResult =
    |Pass of file : string * result : PassFileResult
    |Fail of file : string * result : FailFileResult
    
type FilesScope =
    |All
    |Mods
    |Vanilla

//type GameFile = GameFile of result : FileResult

type STLGame ( gameDirectory : string, scope : FilesScope, modFilter : string, triggers : Effect list, effects : Effect list ) =
        let scriptFolders = [
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

        let mutable files : Map<string, FileResult> = Map.empty
        let validFiles() = files |> Map.toList |> List.choose ( snd >> (function  | Pass (f, r) -> Some ((f, r)) |_ -> None))
        let invalidFiles() = files |> Map.toList |> List.choose ( snd >> (function  | Fail (f, r) -> Some ((f, r)) |_ -> None))


        let mods = if not (Directory.Exists (gameDirectory + "/mod")) then [] else
                    Directory.EnumerateFiles (gameDirectory + "/mod") 
                        |> List.ofSeq
                        |> List.filter (fun f -> Path.GetExtension(f) = ".mod")
                        |> List.map CKParser.parseFile
                        |> List.choose ( function | Success(p, _, _) -> Some p | _ -> None )
                        |> List.map ((ProcessCore.processNodeBasic "mod" Position.Empty) >> (fun s -> s.TagText "name", s.TagText "path"))

        let modFolders = mods |> List.filter (fun (n, _) -> n.Contains(modFilter)) |> List.map (fun (n, p) -> n, (gameDirectory + "/" + p))
        
        let allFolders = 
            match scope with
            |All -> gameDirectory :: (modFolders |> List.map snd)
            |Mods -> (modFolders |> List.map snd)
            |Vanilla -> [gameDirectory]
       
        let allFilesByPath = 
            let getAllFiles path =
                scriptFolders
                        |> List.map ((fun folder -> path + "/" + folder)
                        >> (fun folder -> folder, (if Directory.Exists folder then Directory.EnumerateFiles folder else Seq.empty )|> List.ofSeq))
                        |> List.collect (fun (folder, files) -> files )
                        |> List.filter (fun file -> Path.GetExtension(file) = ".txt")
            List.map getAllFiles allFolders |> List.collect id

        let parseResults files = 
            let matchResult (file, (parseResult, time)) = 
                match parseResult with
                |Success(parsed, _, _) -> Pass (file, {statements = parsed; parseTime = time})
                |Failure(msg, pe,_) -> Fail(file, {error = msg; position = pe.Position; parseTime =  time})
            files |> PSeq.map ((fun file -> file, (fun t -> duration (fun () -> CKParser.parseFile t)) (Path.GetFullPath(file))) >> matchResult)
                     |> PSeq.toList

        let entities() = validFiles()
                        |> List.choose (function |(file, parsed) -> Some (file, parsed.statements, parsed.parseTime) |_ -> None) 
                        //|> List.collect id
                        |> List.map (fun (f, parsed, _) -> (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) parsed))
        //let flatEntities() = entities() |> List.map (fun n -> n.Children) |> List.collect id

        let scriptedTriggers() = 
            let rawTriggers = 
                validFiles()
                |> List.choose (function |(file, parsed) when file.Contains("scripted_triggers") -> Some (file, parsed.statements, parsed.parseTime) |_ -> None)
                //|> List.collect (fun (f, passed, t) -> List.map (fun p -> f, p, t) passed)
                |> List.map (fun (f, parsed, _) -> (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) parsed))
                |> List.collect (fun n -> n.Children)
                |> List.rev
            let firstPass = rawTriggers |> List.fold (fun ts t -> (STLProcess.getScriptedTriggerScope ts ts t)::ts) triggers
            let secondPass = rawTriggers |> List.fold (fun ts t -> (STLProcess.getScriptedTriggerScope ts ts t)::ts) firstPass
            secondPass


        let scriptedEffects() =
            let scriptedTriggers = scriptedTriggers()
            validFiles()
            |> List.choose (function |(file, parsed) when file.Contains("scripted_effects") -> Some (file, parsed.statements, parsed.parseTime) |_ -> None)
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

        let validateDuplicates files =
            files |> List.choose (function |(file, parsed) -> Some (file, parsed.statements) |_ -> None)
                |> List.groupBy (fun (k,p) -> k)
                |> List.map ((fun (k, vs) -> k, List.collect (fun (_, vs2) -> vs2) vs)
                    >> (fun (k, vs) -> k, findDuplicates vs))

        let validateShips (entities : Node list) = 
            let ships = entities |> List.choose (function | :? Ship as s -> Some s |_ -> None)
            ships |> List.map validateShip
                  |> List.choose (function |Invalid es -> Some es |_ -> None)
                  |> List.collect id
                  |> List.map (fun (n, s) -> n :> Node , s)

        let parseErrors() = invalidFiles() |> List.map (fun (file, fail) -> (file, fail.error, fail.position))
                        
        let validateFiles (entities : Node list) =
            entities |> List.map validateVariables
                     |> List.choose (function |Invalid es -> Some es |_ -> None)
                     |> List.collect id

        let validateEvents (entities : Node list) =
            let events = entities |> List.choose (function | :? Event as e -> Some e |_ -> None)
            let scriptedTriggers = scriptedTriggers()
            let scriptedEffects = scriptedEffects()
            events |> List.map (fun e -> (valEventTriggers (triggers @ scriptedTriggers) (effects @ scriptedEffects) e) <&&> (valEventEffects (triggers @ scriptedTriggers) (effects @ scriptedEffects) e))
                   |> List.choose (function |Invalid es -> Some es |_ -> None)
                   |> List.collect id
                   |> List.map (fun (n, s) -> n :> Node, s)

        let validateAll()  = 
            let es = entities()
            let fes = es |> List.map (fun n -> n.Children) |> List.collect id
            (validateShips (fes)) @ (validateFiles (es)) @ (validateEvents (fes))

        do files <- parseResults allFilesByPath |> List.map (function |Pass (file, result) -> (file, Pass(file, result)) |Fail (file, result) -> (file, Fail(file, result))) |> Map.ofList
        member __.Results = parseResults
        member __.Duplicates = validateDuplicates
        member __.ParserErrors = parseErrors()
        member __.ValidationErrors = (validateAll())
        //member __.ValidationWarnings = warningsAll
        member __.Entities = entities
        member __.Folders = allFolders
        member __.AllFiles = files |> Map.toList |> List.map (snd >> (function |(Fail (file, result)) -> (file, false, result.parseTime) |Pass(file, result) -> (file, true, result.parseTime)))
        member __.ScripteTriggers = scriptedTriggers
        member __.ScriptedEffects = scriptedEffects
        //member __.ScriptedTriggers = parseResults |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some p |_ -> None) |> List.map (fun t -> )