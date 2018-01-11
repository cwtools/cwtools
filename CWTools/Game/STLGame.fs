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
open FSharpPlus


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
    |Embedded of file : string * statements : Statement list
    
type FilesScope =
    |All
    |Mods
    |Vanilla

//type GameFile = GameFile of result : FileResult

type STLGame ( scopeDirectory : string, scope : FilesScope, modFilter : string, triggers : Effect list, effects : Effect list, embeddedFiles : (string * string) list ) =
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

        let mutable scriptedTriggers : Effect list = []
        let mutable scriptedEffects : Effect list = []

        let rec getAllFolders dirs =
            if Seq.isEmpty dirs then Seq.empty else
                seq { yield! dirs |> Seq.collect Directory.EnumerateDirectories
                      yield! dirs |> Seq.collect Directory.EnumerateDirectories |> getAllFolders }
        let getAllFoldersUnion dirs =
            seq { 
                yield! dirs
                yield! getAllFolders dirs
            }
        let allDirs = if Directory.Exists scopeDirectory then getAllFoldersUnion [scopeDirectory] |> List.ofSeq |> List.map(fun folder -> folder, Path.GetFileName folder) else []
        let gameDirectory = 
            let dir = allDirs |> List.tryFind (fun (_, folder) -> folder.ToLower() = "stellaris") |> map fst >>= (fun f -> if Directory.Exists (f + (string Path.DirectorySeparatorChar) + "common") then Some f else None)
            match dir with
            |Some s -> eprintfn "Found stellaris directory at %s" s
            |None -> eprintfn "Couldn't find stellaris directory, falling back to embedded vanilla files"
            dir
        let mods = 
            let getModFiles modDir =
                Directory.EnumerateFiles (modDir) 
                        |> List.ofSeq
                        |> List.filter (fun f -> Path.GetExtension(f) = ".mod")
                        |> List.map CKParser.parseFile
                        |> List.choose ( function | Success(p, _, _) -> Some p | _ -> None )
                        |> List.map ((ProcessCore.processNodeBasic "mod" Position.Empty) >> (fun s -> s.TagText "name", s.TagText "path", modDir))


            let modDirs = allDirs |> List.filter(fun (path, folder) -> folder.ToLower() = "mod" || folder.ToLower() = "mods")
            match modDirs with
            | [] -> eprintfn "%s" "Didn't find any mod directories" 
            | x ->
                eprintfn "Found %i mod directories:" x.Length
                x |> List.iter (fun d -> eprintfn "%s" (fst d))
            let modFiles = modDirs |> List.collect (fst >> getModFiles)
            match modFiles with
            | [] -> eprintfn "%s" "Didn't find any mods"
            | x -> 
                eprintfn "Found %i mods:" x.Length
                x |> List.iter (fun (n, p, d) -> eprintfn "%s, %s" n p)
            modFiles
        let modFolders = 
            let folders = mods |> List.filter (fun (n, _, _) -> n.Contains(modFilter))
                                |> map (fun (n, p, r) -> if Path.IsPathRooted p then n, p else n, (Directory.GetParent(r).FullName + (string Path.DirectorySeparatorChar) + p))
            eprintfn "Mod folders"                            
            folders |> List.iter (fun (n, f) -> eprintfn "%s, %s" n f)
            folders

        let allFolders = 
            match gameDirectory, scope with
            |None, _ -> (modFolders |> List.map snd)
            |Some s, All -> s :: (modFolders |> List.map snd)
            |_, Mods -> (modFolders |> List.map snd)
            |Some s, Vanilla -> [s]
       
        let getEmbeddedFiles = embeddedFiles |> map (fun (fn, f) -> "embeddedfiles." + fn, f)
        let allFilesByPath = 
            let getAllFiles path =
                scriptFolders
                        |> List.map ((fun folder -> path + "/" + folder)
                        >> (fun folder -> folder, (if Directory.Exists folder then Directory.EnumerateFiles folder else Seq.empty )|> List.ofSeq))
                        |> List.collect (fun (folder, files) -> files )
                        |> List.filter (fun file -> Path.GetExtension(file) = ".txt")
            let allFiles = map getAllFiles allFolders |> List.collect id |> map (fun f -> f, File.ReadAllText f)
            allFiles

        let matchResult (file : string, (parseResult, time)) = 
                match file, parseResult with
                |file, Success(parsed, _, _) when file.Contains("embeddedfiles.") -> Embedded(file, parsed)
                |file, Failure(msg, pe, _) when file.Contains("embeddedfiles.") -> failwith ("Embedded file failed: " + file + msg)
                |file, Success(parsed, _, _) -> Pass (file, {statements = parsed; parseTime = time})
                |file, Failure(msg, pe,_) -> Fail(file, {error = msg; position = pe.Position; parseTime =  time})
        let parseResults files = 
            files |> PSeq.map ((fun (file, fileString) -> file, (fun (t, t2) -> duration (fun () -> CKParser.parseString t t2)) (file, fileString)) >> matchResult)
                     |> PSeq.toList
                
        let parseEntities validfiles =
            validfiles
            |> List.map (fun (file, parsed) -> (file, parsed.statements, parsed.parseTime))
            |> List.map (fun (f, parsed, _) -> (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) parsed))

        let embeddedParsed = parseResults getEmbeddedFiles
            

        // let entities() = validFiles()
        //                 |> List.choose (function |(file, parsed) -> Some (file, parsed.statements, parsed.parseTime) |_ -> None) 
        //                 //|> List.collect id
        //                 |> List.map (fun (f, parsed, _) -> (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) parsed))
        //let flatEntities() = entities() |> List.map (fun n -> n.Children) |> List.collect id

        let updateScriptedTriggers (validfiles : (string * Statement list) list ) = 
            let rawTriggers = 
                validfiles
                |> List.choose (function |(file, statements) when file.Contains("scripted_triggers") -> Some (file, statements) |_ -> None)
                //|> List.collect (fun (f, passed, t) -> List.map (fun p -> f, p, t) passed)
                |> List.map (fun (f, statements) -> (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) statements))
                |> List.collect (fun n -> n.Children)
                |> List.rev
            let firstPass = rawTriggers |> List.fold (fun ts t -> (STLProcess.getScriptedTriggerScope ts ts t)::ts) triggers
            let secondPass = rawTriggers |> List.fold (fun ts t -> (STLProcess.getScriptedTriggerScope ts ts t)::ts) firstPass
            scriptedTriggers <- secondPass


        let updateScriptedEffects (validfiles : (string * Statement list) list ) =
            let effects = 
                validfiles
                |> List.choose (function |(file, statements) when file.Contains("scripted_effects") -> Some (file, statements) |_ -> None)
                //|> List.collect (fun (f, passed, t) -> List.map (fun p -> f, p, t) passed)
                |> List.map (fun (f, statements) -> (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) statements))
                |> List.collect (fun n -> n.Children)
                |> List.rev
                |> List.fold (fun es e -> (STLProcess.getScriptedTriggerScope es scriptedTriggers e)::es) effects
            scriptedEffects <- effects
                      

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
            let scriptedTriggers = scriptedTriggers
            let scriptedEffects = scriptedEffects
            events |> List.map (fun e -> (valEventVals e) <&&> (valEventTriggers (triggers @ scriptedTriggers) (effects @ scriptedEffects) e) <&&> (valEventEffects (triggers @ scriptedTriggers) (effects @ scriptedEffects) e))
                   |> List.choose (function |Invalid es -> Some es |_ -> None)
                   |> List.collect id
                   |> List.map (fun (n, s) -> n :> Node, s)

        let validateAll (entities : (string * PassFileResult) list)  = 
            eprintfn "Validating %i files" (entities.Length)
            let es = entities |> parseEntities
            let fes = es |> List.map (fun n -> n.Children) |> List.collect id
            (validateShips (fes)) @ (validateFiles (es)) @ (validateEvents (fes))

        let updateFile filepath =
            eprintfn "%s" filepath
            let file = File.ReadAllText filepath
            let newFiles = parseResults [(filepath, file)] 
            files <- newFiles |> List.map (function |Pass (file, result) -> (file, Pass(file, result)) |Fail (file, result) -> (file, Fail(file, result))) |> List.fold (fun x s -> x.Add(s)) files
            let valid = newFiles |> List.choose  (function  | Pass (f, r) -> Some ((f, r)) |_ -> None)
            validateAll valid

        do 
            eprintfn "Parsing %i files" allFilesByPath.Length
            files <- parseResults allFilesByPath |> List.map (function |Pass (file, result) -> (file, Pass(file, result)) |Fail (file, result) -> (file, Fail(file, result))) |> Map.ofList
            let embedded =  (embeddedParsed |> List.choose (function |Embedded (f, s) -> Some (f, s) |_ -> None))
            let user = (validFiles() |> map (fun (fn, pf) -> fn, pf.statements))
            let coreFiles = 
                match gameDirectory with
                |Some _ -> user
                |None -> user @ embedded
            updateScriptedTriggers (coreFiles)
            updateScriptedEffects (coreFiles)
        member __.Results = parseResults
        member __.Duplicates = validateDuplicates
        member __.ParserErrors = parseErrors()
        member __.ValidationErrors = (validateAll (validFiles()))
        //member __.ValidationWarnings = warningsAll
        member __.Folders = allFolders
        member __.AllFiles = files |> Map.toList |> List.map (snd >> (function |(Fail (file, result)) -> (file, false, result.parseTime) |Pass(file, result) -> (file, true, result.parseTime)))
        member __.ScripteTriggers = scriptedTriggers
        member __.ScriptedEffects = scriptedEffects
        member __.UpdateFile file = updateFile file


        //member __.ScriptedTriggers = parseResults |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some p |_ -> None) |> List.map (fun t -> )