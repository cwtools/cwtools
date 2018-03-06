namespace CWTools.Games

open CWTools.Parser
open System.IO
open FParsec
open CWTools.Process
open CWTools.Process.STLProcess
open CWTools.Validation.STLValidation
open CWTools.Validation.ValidationCore
open FSharp.Collections.ParallelSeq
open CWTools.Localisation
open CWTools.Localisation.STLLocalisation
open CWTools.Common
open CWTools.Common.STLConstants
open CWTools.Process.STLScopes
open DotNet.Globbing
open System.Collections.Specialized
open CWTools.Validation.STLLocalisationValidation



    
type FilesScope =
    |All
    |Mods
    |Vanilla

//type GameFile = GameFile of result : FileResult

type STLGame ( scopeDirectory : string, scope : FilesScope, modFilter : string, triggers : DocEffect list, effects : DocEffect list, modifiers : Modifier list, embeddedFiles : (string * string) list, langs : Lang list, validateVanilla : bool ) =
        let scriptFolders = [
                    "common/agendas";
                    "common/ambient_objects";
                    "common/anomalies";
                    "common/armies";
                    "common/army_attachments";
                    "common/attitudes";
                    "common/buildable_pops";
                    "common/buildings";
                    "common/button_effects";
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
                    "common/governments/civics";
                    "common/graphical_culture";
                    "common/mandates";
                    "common/megastructures";
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
                    "common/scripted_variables";
                    "common/section_templates";
                    "common/sector_types";
                    "common/ship_behaviors";
                    "common/ship_sizes";
                    "common/solar_system_initializers";
                    "common/spaceport_modules";
                    "common/special_projects";
                    "common/species_classes";
                    "common/species_rights";
                    "common/species_names";
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
                    "gfx";
                    ]


        let vanillaEffects = 
            let se = scopedEffects |> List.map (fun e -> e :> Effect)
            let ve = effects |> addInnerScope |> List.map (fun e -> e :> Effect)
            se @ ve
        let vanillaTriggers = 
            let se = scopedEffects |> List.map (fun e -> e :> Effect)
            let vt = triggers |> addInnerScope |> List.map (fun e -> e :> Effect)
            se @ vt

        let resources = ResourceManager().Api

        let validatableFiles() = resources.ValidatableFiles
        let lookup = Lookup()
        // let mutable scriptedTriggers : Effect list = []
        // let mutable scriptedEffects : Effect list = []
        // let mutable staticModifiers : Modifier list = []
        let mutable localisationAPIs : ILocalisationAPI list = []
        let mutable localisationErrors : (string * Severity * CWTools.Parser.Position * int * string * string option) list option = None

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
            let dir = allDirs |> List.tryFind (fun (_, folder) -> folder.ToLower() = "stellaris") |> Option.map (fst) |> Option.bind (fun f -> if Directory.Exists (f + (string Path.DirectorySeparatorChar) + "common") then Some f else None)
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
                                |> List.map (fun (n, p, r) -> if Path.IsPathRooted p then n, p else n, (Directory.GetParent(r).FullName + (string Path.DirectorySeparatorChar) + p))
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
        let locFolders = allDirs |> List.filter(fun (_, folder) -> folder.ToLower() = "localisation" || folder.ToLower() = "localisation_synced")
        
        let getEmbeddedFiles = embeddedFiles |> List.map (fun (fn, f) -> "embedded", "embeddedfiles/" + fn, f)
        let allFilesByPath = 
            let getAllFiles (scope, path) =
                scriptFolders
                        |> List.map ((fun folder -> scope, Path.GetFullPath(Path.Combine(path, folder)))
                        >> (fun (scope, folder) -> scope, folder, (if Directory.Exists folder then getAllFoldersUnion [folder] |> Seq.collect Directory.EnumerateFiles else Seq.empty )|> List.ofSeq))
                        |> List.collect (fun (scope, _, files) -> files |> List.map (fun f -> scope, Path.GetFullPath(f)))
            let fileToResourceInput (scope, filepath) =
                match Path.GetExtension(filepath) with
                |".txt"
                |".gui"
                |".gfx" ->
                    Some (EntityResourceInput { scope = scope; filepath = filepath; filetext = File.ReadAllText filepath; validate = true})
                |".dds"
                |".tga"
                |".shader"
                |".lua"
                |".png" ->
                    Some (FileResourceInput { scope = scope; filepath = filepath })
                |_ -> None
            let allFiles = List.map getAllFiles allFolders |> List.collect id |> List.choose fileToResourceInput
            allFiles

        let updateScriptedTriggers () = 
            let rawTriggers = 
                resources.AllEntities()
                |> List.choose (function |f when f.filepath.Contains("scripted_triggers") -> Some (f.entity) |_ -> None)
                |> List.collect (fun n -> n.Children)
                |> List.rev
            let mutable final = vanillaTriggers
            let mutable i = 0
            let mutable first = true
            let ff() = 
                i <- i + 1
                let before = final
                final <- rawTriggers |> List.map (fun t -> (STLProcess.getScriptedTriggerScope first EffectType.Trigger final final t) :> Effect)
                first <- false
                ((before |> Set.ofList) = (final |> Set.ofList)) || i > 10
            while (not (ff())) do ()
            lookup.scriptedTriggers <- final @ vanillaTriggers


        let updateScriptedEffects () =
            let rawEffects = 
                resources.AllEntities()
                |> List.choose (function |f when f.filepath.Contains("scripted_effects") -> Some (f.entity) |_ -> None)
                |> List.collect (fun n -> n.Children)
                |> List.rev
            let mutable final = vanillaEffects
            let mutable i = 0
            let mutable first = true
            let ff() = 
                i <- i + 1
                let before = final
                final <- rawEffects |>  List.map (fun e -> (STLProcess.getScriptedTriggerScope first EffectType.Effect final lookup.scriptedTriggers e) :> Effect)
                first <- false
                ((before |> Set.ofList) = (final |> Set.ofList)) || i > 10
            while (not (ff())) do ()
            lookup.scriptedEffects <- final @ vanillaEffects

        let updateStaticodifiers () =
            let rawModifiers =
                resources.AllEntities()
                |> List.choose (function |f when f.filepath.Contains("static_modifiers") -> Some (f.entity) |_ -> None)
                |> List.collect (fun n -> n.Children)
                |> List.rev
            let newModifiers = rawModifiers |> List.map (fun e -> STLProcess.getStaticModifierCategory modifiers e)
            lookup.staticModifiers <- newModifiers
        
        let updateLocalisation() = 
            localisationAPIs <-
                let locs = locFolders |> List.map (fun (folder, _) -> STLLocalisationService({ folder = folder}))
                let allLocs = locs |> List.collect (fun l -> (STL STLLang.Default :: langs)|> List.map (fun lang -> l.Api(lang)))
                match gameDirectory with
                |Some _ -> allLocs
                |None ->  
                    allLocs @ (getEmbeddedFiles 
                    |> List.filter (fun (_, fn, _ )-> fn.Contains("localisation"))
                    |> List.map (fun (_, fn, f) -> (fn, f))
                    |> (fun files -> STLLocalisationService(files))
                    |> (fun l -> (STL STLLang.Default :: langs) |> List.map (fun lang -> l.Api(lang))))
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

        let parseErrors() = 
            resources.GetResources()
                |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None)
                |> List.choose (fun r -> r.result |> function |(Fail (result)) when r.validate -> Some (r.filepath, result.error, result.position)  |_ -> None)


        let validateTechnology (entities : (string * Node) list) =
            let tech = entities |> List.filter (fun (f, _) -> f.Contains("common/technology/")) 
            tech |> List.iter (fun (f, t) -> eprintfn "%s" f)
            
        let validateEvents (entities : Node list) =
            let events = entities |> List.choose (function | :? Event as e -> Some e |_ -> None)
            let scriptedTriggers = lookup.scriptedTriggers
            let scriptedEffects = lookup.scriptedEffects
            events |> List.map (fun e -> (valEventVals e) <&&> (valEventTriggers (vanillaTriggers @ scriptedTriggers) (vanillaEffects @ scriptedEffects) lookup.staticModifiers e) <&&> (valEventEffects (vanillaTriggers @ scriptedTriggers) (vanillaEffects @ scriptedEffects) lookup.staticModifiers e))
                   |> List.choose (function |Invalid es -> Some es |_ -> None)
                   |> List.collect id
        let snood = snd
        let validateAll (entities : Entity list)  = 
            eprintfn "Validating %i files" (entities.Length)
            let allEntitiesByFile = entities |> List.map (fun f -> f.entity)
            let flattened = allEntitiesByFile |> List.map (fun n -> n.Children) |> List.collect id

            let validators = [validateVariables; valTechnology; valButtonEffects; valSprites]
            let oldEntities = EntitySet (resources.AllEntities())
            let newEntities = EntitySet entities
            let res = validators |> List.map (fun v -> v oldEntities newEntities) |> List.fold (<&&>) OK
                       |> (function |Invalid es -> es |_ -> [])

            let fileValidators = [valSpriteFiles]
            let fres = fileValidators <&!&> (fun v -> v resources newEntities) |> (function |Invalid es -> es |_ -> [])

            (validateShips (flattened)) @ (validateEvents (flattened)) @ res @ fres
        
        let localisationCheck (entities : Entity list) =
            eprintfn "Localisation check %i files" (entities.Length)
            let keys = localisationAPIs |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |>List.collect (fun ls -> ls.GetKeys) |> Set.ofList )
            
            let validators = [valEventLocs; valTechLocs; valCompSetLocs; valCompTempLocs; valBuildingLocs; valTraditionLocCats; valArmiesLoc;
                                 valArmyAttachmentLocs; valDiploPhrases; valShipLoc; valFactionDemands; valSpeciesRightsLocs;
                                 valMapsLocs; valMegastructureLocs; valModifiers; valModules; valTraits; valGoverments; valPersonalities;
                                 valEthics; valPlanetClasses; valEdicts; valPolicies; valSectionTemplates; valSpeciesNames; valStratRes;
                                 valAmbient]
            let oldEntities = EntitySet (resources.AllEntities())
            let newEntities = EntitySet entities

            validators |> List.map (fun v -> v oldEntities keys newEntities) |> List.fold (<&&>) OK
                       |> (function |Invalid es -> es |_ -> [])
             

        let updateFile filepath =
            eprintfn "%s" filepath
            let file = File.ReadAllText filepath
            let newEntities = resources.UpdateFile (EntityResourceInput {scope = ""; filepath = filepath; filetext = file; validate = true})
            validateAll newEntities @ localisationCheck newEntities

        do 
            eprintfn "Parsing %i files" allFilesByPath.Length
            // let efiles = allFilesByPath |> List.filter (fun (_, f, _) -> not(f.EndsWith(".dds"))) 
            //             |> List.map (fun (s, f, ft) -> EntityResourceInput {scope = s; filepath = f; filetext = ft; validate = true})
            // let otherfiles = allFilesByPath |> List.filter (fun (_, f, _) -> f.EndsWith(".dds"))
            //                     |> List.map (fun (s, f, _) -> FileResourceInput {scope = s; filepath = f;})
            let files = allFilesByPath
            let filteredfiles = if validateVanilla then files else files |> List.choose (function |FileResourceInput f -> Some (FileResourceInput f) |EntityResourceInput f -> if f.scope = "vanilla" then Some (EntityResourceInput {f with validate = false}) else Some (EntityResourceInput f))
            resources.UpdateFiles(filteredfiles) |> ignore
            let embedded = embeddedFiles |> List.map (fun (f, ft) -> if ft = "" then FileResourceInput { scope = "embedded"; filepath = f } else EntityResourceInput {scope = "embedded"; filepath = f; filetext = ft; validate = false})
           
            match gameDirectory with
            |None -> resources.UpdateFiles(embedded) |> ignore
            | _ -> ()

            updateScriptedTriggers()
            updateScriptedEffects()
            updateStaticodifiers()
            updateLocalisation()

        //member __.Results = parseResults
        member __.Duplicates = validateDuplicates
        member __.ParserErrors = parseErrors()
        member __.ValidationErrors = (validateAll (resources.ValidatableEntities()))
        member __.LocalisationErrors= 
            match localisationErrors with
            |Some les -> les
            |None -> 
                let les = (localisationCheck (resources.ValidatableEntities()))
                localisationErrors <- Some les
                les
        //member __.ValidationWarnings = warningsAll
        member __.Folders = allFolders
        member __.AllFiles() = 
            resources.GetResources()
            // |> List.map 
            //     (function 
            //         |EntityResource (f, r) ->  r.result |> function |(Fail (result)) -> (r.filepath, false, result.parseTime) |Pass(result) -> (r.filepath, true, result.parseTime)
            //         |FileResource (f, r) ->  (r.filepath, false, 0L))
            //|> List.map (fun r -> r.result |> function |(Fail (result)) -> (r.filepath, false, result.parseTime) |Pass(result) -> (r.filepath, true, result.parseTime))
        member __.ScripteTriggers = lookup.scriptedTriggers
        member __.ScriptedEffects = lookup.scriptedEffects
        member __.StaticModifiers = lookup.staticModifiers
        member __.UpdateFile file = updateFile file
        member __.AllEntities = resources.AllEntities()
        member __.References = References(resources, lookup)
       

        //member __.ScriptedTriggers = parseResults |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some p |_ -> None) |> List.map (fun t -> )