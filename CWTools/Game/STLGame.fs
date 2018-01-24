namespace CWTools.Games

open CWTools.Parser
open System.IO
open FParsec
open CWTools.Process
open CWTools.Process.STLProcess
open CWTools.Validation.STLValidation
open CWTools.Validation.ValidationCore
open FSharp.Collections.ParallelSeq
open FSharpPlus
open CWTools.Localisation
open CWTools.Localisation.STLLocalisation
open CWTools.Common
open CWTools.Common.STLConstants
open CWTools.Process.STLScopes
open DotNet.Globbing
open System.Collections.Specialized
open CWTools.Validation.STLLocalisationValidation


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
    |Pass of file : string * scope : string * result : PassFileResult
    |Fail of file : string * scope : string * result : FailFileResult
    |Embedded of file : string * statements : Statement list
    
type FilesScope =
    |All
    |Mods
    |Vanilla

//type GameFile = GameFile of result : FileResult

type STLGame ( scopeDirectory : string, scope : FilesScope, modFilter : string, triggers : DocEffect list, effects : DocEffect list, embeddedFiles : (string * string) list, langs : Lang list, validateVanilla : bool ) =
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
                    "common/traditions";
                    "common/tradition_categories";
                    "common/traits";
                    "common/triggered_modifiers";
                    "common/war_demand_counters";
                    "common/war_demand_types";
                    "events";
                    "map/galaxy";
                    "map/setup_scenarios";
                    "prescripted_countries";
                    "interface";
                    ]

        let duration f = 
            let timer = System.Diagnostics.Stopwatch()
            timer.Start()
            let returnValue = f()
            (returnValue  , timer.ElapsedMilliseconds) 

        let vanillaEffects = 
            let se = scopedEffects |> List.map (fun e -> e :> Effect)
            let ve = effects |> addInnerScope |> List.map (fun e -> e :> Effect)
            se @ ve
        let vanillaTriggers = 
            let se = scopedEffects |> List.map (fun e -> e :> Effect)
            let vt = triggers |> addInnerScope |> List.map (fun e -> e :> Effect)
            se @ vt

        let mutable files : Map<string, FileResult> = Map.empty
        let validFiles() = files |> Map.toList |> List.choose ( snd >> (function  | Pass (f, s, r) -> Some ((f, s, r)) |_ -> None))
        let invalidFiles() = files |> Map.toList |> List.choose ( snd >> (function  | Fail (f, s, r) -> Some ((f, s, r)) |_ -> None))
        let validatableFiles() = if validateVanilla then validFiles() else validFiles() |> List.filter (fun(_, s, _) -> s <> "vanilla")
        let mutable allEntities : Map<string, Node> = Map.empty
        let entitiesList() = allEntities |> Map.toList

        let mutable scriptedTriggers : Effect list = []
        let mutable scriptedEffects : Effect list = []
        let mutable localisationAPIs : ILocalisationAPI list = []

        let rec getAllFolders dirs =
            if Seq.isEmpty dirs then Seq.empty else
                seq { yield! dirs |> Seq.collect Directory.EnumerateDirectories
                      yield! dirs |> Seq.collect Directory.EnumerateDirectories |> getAllFolders }
        let getAllFoldersUnion dirs =
            seq { 
                yield! dirs
                yield! getAllFolders dirs
            }
        do eprintfn "%s %b" scopeDirectory (Directory.Exists scopeDirectory)
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


            let modDirs = allDirs |> List.filter(fun (_, folder) -> folder.ToLower() = "mod" || folder.ToLower() = "mods")
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
                x |> List.iter (fun (n, p, _) -> eprintfn "%s, %s" n p)
            modFiles
        let modFolders = 
            let folders = mods |> List.filter (fun (n, _, _) -> n.Contains(modFilter))
                                |> map (fun (n, p, r) -> if Path.IsPathRooted p then n, p else n, (Directory.GetParent(r).FullName + (string Path.DirectorySeparatorChar) + p))
            eprintfn "Mod folders"                            
            folders |> List.iter (fun (n, f) -> eprintfn "%s, %s" n f)
            folders
          

        let allFolders = 
            match gameDirectory, scope with
            |None, _ -> 
                if modFolders.Length > 0 then modFolders
                else 
                    eprintfn "Couldn't find the game directory or any mods"
                    let foundAnyFolders = Directory.EnumerateDirectories scopeDirectory |> List.ofSeq |> List.exists (fun f -> f.Contains("common") || f.Contains("events"))
                    match foundAnyFolders with
                    | true ->
                        eprintfn "I think you opened a mod folder directly"
                        [(Path.GetFileName scopeDirectory, scopeDirectory)]
                    | false ->
                        eprintfn "I don't think you opened a mod folder directly"
                        []
            |Some s, All -> ("vanilla", s) :: (modFolders)
            |_, Mods -> (modFolders)
            |Some s, Vanilla -> ["vanilla", s]
        let locFolders = allDirs |> List.filter(fun (_, folder) -> folder.ToLower() = "localisation")
        
        let getEmbeddedFiles = embeddedFiles |> map (fun (fn, f) -> "embedded", "embeddedfiles." + fn, f)
        let allFilesByPath = 
            let getAllFiles (scope, path) =
                scriptFolders
                        |> List.map ((fun folder -> scope, path + "/" + folder)
                        >> (fun (scope, folder) -> scope, folder, (if Directory.Exists folder then Directory.EnumerateFiles folder else Seq.empty )|> List.ofSeq))
                        |> List.collect (fun (scope, _, files) -> files |> List.map (fun f -> scope, f) )
                        |> List.filter (fun (_, file) -> List.contains (Path.GetExtension(file)) [".txt"; ".gui"; ".gfx"])
            let allFiles = map getAllFiles allFolders |> List.collect id |> map (fun( s, f) -> s, f, File.ReadAllText f)
            allFiles

        let matchResult (scope : string, file : string, (parseResult, time)) = 
                match file, parseResult with
                |file, Success(parsed, _, _) when file.Contains("embeddedfiles.") -> Embedded(file, parsed)
                |file, Failure(msg, _, _) when file.Contains("embeddedfiles.") -> failwith ("Embedded file failed: " + file + msg)
                |file, Success(parsed, _, _) -> Pass (file, scope, {statements = parsed; parseTime = time})
                |file, Failure(msg, pe,_) -> Fail(file, scope, {error = msg; position = pe.Position; parseTime =  time})
        let parseResults files = 
            files |> PSeq.map ((fun (scope, file, fileString) -> scope, file, (fun (t, t2) -> duration (fun () -> CKParser.parseString t t2)) (file, fileString)) >> matchResult)
                     |> PSeq.toList
                
        let parseEntities validfiles =
            validfiles
            |> List.map ((fun (file, _, parsed) -> (file, parsed.statements, parsed.parseTime))
            >> (fun (f, parsed, _) ->  f, (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) parsed)))

        let embeddedParsed = getEmbeddedFiles |> List.filter (fun (_, fn, _) -> fn.Contains("common")) |> parseResults
            

        // let entities() = validFiles()
        //                 |> List.choose (function |(file, parsed) -> Some (file, parsed.statements, parsed.parseTime) |_ -> None) 
        //                 //|> List.collect id
        //                 |> List.map (fun (f, parsed, _) -> (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) parsed))
        //let flatEntities() = entities() |> List.map (fun n -> n.Children) |> List.collect id
        let updateEntities() = 
            allEntities <- validFiles() |> parseEntities |> Map.ofList

        let updateScriptedTriggers (validfiles : (string * Statement list) list ) = 
            let rawTriggers = 
                validfiles
                |> List.choose (function |(file, statements) when file.Contains("scripted_triggers") -> Some (file, statements) |_ -> None)
                //|> List.collect (fun (f, passed, t) -> List.map (fun p -> f, p, t) passed)
                |> List.map (fun (f, statements) -> (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) statements))
                |> List.collect (fun n -> n.Children)
                |> List.rev
            let mutable final = vanillaTriggers
            let mutable i = 0
            let ff() = 
                i <- i + 1
                let before = final
                final <- rawTriggers |> List.fold (fun ts t -> (STLProcess.getScriptedTriggerScope EffectType.Trigger ts ts t) :> Effect::ts) final
                ((before |> Set.ofList) = (final |> Set.ofList)) || i > 10
            while (not (ff())) do ()
                //( before |> List.map (fun f -> f.Name, f.Scopes)) = (final |> List.map (fun f -> f.Name, f.Scopes))
            scriptedTriggers <- final


        let updateScriptedEffects (validfiles : (string * Statement list) list ) =
            let rawEffects = 
                validfiles
                |> List.choose (function |(file, statements) when file.Contains("scripted_effects") -> Some (file, statements) |_ -> None)
                //|> List.collect (fun (f, passed, t) -> List.map (fun p -> f, p, t) passed)
                |> List.map (fun (f, statements) -> (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) statements))
                |> List.collect (fun n -> n.Children)
                |> List.rev
            let mutable final = vanillaEffects
            let mutable i = 0
            let ff() = 
                i <- i + 1
                let before = final
                final <- rawEffects |>  List.fold (fun es e -> (STLProcess.getScriptedTriggerScope EffectType.Effect es scriptedTriggers e) :> Effect::es) final
                ((before |> Set.ofList) = (final |> Set.ofList)) || i > 10
            while (not (ff())) do ()
            scriptedEffects <- final
        
        let updateLocalisation() = 

            localisationAPIs <- 
                let allLocs = locFolders |> List.collect (fun (l, _) -> langs |> List.map (fun lang -> STLLocalisationService({ folder = l; language = lang}).Api))
                match gameDirectory with
                |Some _ -> allLocs
                |None ->  
                    allLocs @ (getEmbeddedFiles 
                    |> List.filter (fun (_, fn, _ )-> fn.Contains("localisation"))
                    |> List.map (fun (_, fn, f) -> (fn, f))
                    |> (fun files -> langs |> List.map (fun lang -> STLLocalisationService(files, lang).Api)))

            //TODO: Add loc from embedded
                
                      

        let findDuplicates (sl : Statement list) =
            let node = ProcessCore.processNodeBasic "root" Position.Empty sl
            node.Children |> List.groupBy (fun c -> c.Key)
                          |> List.filter (fun (_,v) -> v.Length > 1)
                          |> List.map (fun (k,_) -> k)

        let validateDuplicates files =
            files |> List.choose (function |(file, parsed) -> Some (file, parsed.statements))
                |> List.groupBy (fun (k,_) -> k)
                |> List.map ((fun (k, vs) -> k, List.collect (fun (_, vs2) -> vs2) vs)
                    >> (fun (k, vs) -> k, findDuplicates vs))

        let validateShips (entities : Node list) = 
            let ships = entities |> List.choose (function | :? Ship as s -> Some s |_ -> None)
            ships |> List.map validateShip
                  |> List.choose (function |Invalid es -> Some es |_ -> None)
                  |> List.collect id

        let parseErrors() = invalidFiles() |> List.map (fun (file, _, fail) -> (file, fail.error, fail.position))
                        
        let validateFiles (entities : Node list) =
            entities |> List.map validateVariables
                     |> List.choose (function |Invalid es -> Some es |_ -> None)
                     |> List.collect id

        let validateTechnology (entities : (string * Node) list) =
            let tech = entities |> List.filter (fun (f, _) -> f.Contains("common/technology/")) 
            tech |> List.iter (fun (f, t) -> eprintfn "%s" f)
            
        let validateEvents (entities : Node list) =
            let events = entities |> List.choose (function | :? Event as e -> Some e |_ -> None)
            let scriptedTriggers = scriptedTriggers
            let scriptedEffects = scriptedEffects
            events |> List.map (fun e -> (valEventVals e) <&&> (valEventTriggers (vanillaTriggers @ scriptedTriggers) (vanillaEffects @ scriptedEffects) e) <&&> (valEventEffects (vanillaTriggers @ scriptedTriggers) (vanillaEffects @ scriptedEffects) e))
                   |> List.choose (function |Invalid es -> Some es |_ -> None)
                   |> List.collect id
        let snood = snd
        let validateAll (entities : (string * string * PassFileResult) list)  = 
            eprintfn "Validating %i files" (entities.Length)
            let entitiesByFile = entities |> parseEntities
            let allEntitiesByFile = entitiesByFile |> List.map snood
            let flattened = allEntitiesByFile |> List.map (fun n -> n.Children) |> List.collect id
            (validateShips (flattened)) @ (validateFiles (allEntitiesByFile)) @ (validateEvents (flattened))
        
        let localisationCheck (entities : (string * string * PassFileResult) list) =
            eprintfn "Localisation check %i files" (entities.Length)
            let e1 = entities |> parseEntities
            let keys = localisationAPIs |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |>List.collect (fun ls -> ls.GetKeys) |> Set.ofList )
            
            let validators = [valTechLocs; valCompSetLocs; valCompTempLocs; valBuildingLocs; valTraditionLocCats]
            let oldEntities = EntitySet (entitiesList())
            let newEntities = EntitySet e1

            validators |> List.map (fun v -> v oldEntities keys newEntities) |> List.fold (<&&>) OK
                       |> (function |Invalid es -> es |_ -> [])
             

        let updateFile filepath =
            eprintfn "%s" filepath
            let file = File.ReadAllText filepath
            let newFiles = parseResults [("", filepath, file)] 
            files <- newFiles |> List.map (function |Pass (file, scope, result) -> (file, Pass(file, scope, result)) |Fail (file, scope, result) -> (file, Fail(file, scope, result))) |> List.fold (fun x s -> x.Add(s)) files
            let valid = newFiles |> List.choose  (function  | Pass (f, s, r) -> Some (f, s, r) |_ -> None)
            let newEntities = parseEntities valid
            allEntities <- newEntities |> List.fold (fun x s -> x.Add(s)) allEntities
            validateAll valid @ localisationCheck valid

        do 
            eprintfn "Parsing %i files" allFilesByPath.Length
            files <- parseResults allFilesByPath |> List.map (function |Pass (file, scope, result) -> (file, Pass(file, scope, result)) |Fail (file, scope, result) -> (file, Fail(file, scope, result))) |> Map.ofList
            let embedded =  (embeddedParsed |> List.choose (function |Embedded (f, s) -> Some (f, s) |_ -> None))
            let user = (validFiles() |> map (fun (fn, _, pf) -> fn, pf.statements))
            let coreFiles = 
                match gameDirectory with
                |Some _ -> user
                |None -> user @ embedded
            updateEntities()
            updateScriptedTriggers (coreFiles)
            updateScriptedEffects (coreFiles)
            updateLocalisation()

        member __.Results = parseResults
        member __.Duplicates = validateDuplicates
        member __.ParserErrors = parseErrors()
        member __.ValidationErrors = (validateAll (validatableFiles()))
        member __.LocalisationErrors= (localisationCheck (validatableFiles()))
        //member __.ValidationWarnings = warningsAll
        member __.Folders = allFolders
        member __.AllFiles = files |> Map.toList |> List.map (snd >> (function |(Fail (file, _, result)) -> (file, false, result.parseTime) |Pass(file, _, result) -> (file, true, result.parseTime)))
        member __.ScripteTriggers = scriptedTriggers
        member __.ScriptedEffects = scriptedEffects
        member __.UpdateFile file = updateFile file


        //member __.ScriptedTriggers = parseResults |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some p |_ -> None) |> List.map (fun t -> )