namespace CWTools.Games

open CWTools.Parser
open System.IO
open FParsec
open CWTools.Process
open CWTools.Process.STLProcess
open CWTools.Validation.Stellaris.STLValidation
open CWTools.Validation.ValidationCore
open FSharp.Collections.ParallelSeq
open CWTools.Localisation
open CWTools.Localisation.STLLocalisation
open CWTools.Common
open CWTools.Common.STLConstants
open CWTools.Process.STLScopes
open DotNet.Globbing
open System.Collections.Specialized
open CWTools.Validation.Stellaris.STLLocalisationValidation
open CWTools.Validation.Stellaris.STLEventValidation
open CWTools.Process.ProcessCore
open CWTools.Parser.Types
open CWTools.Validation.Stellaris.STLLocalisationString
open CWTools.Utilities.Utils
open CWTools.Validation.Stellaris.Graphics
open CWTools.Game.Stellaris
open CWTools.Game.Stellaris.STLLookup
open Microsoft.FSharp.Compiler.Range



    
type FilesScope =
    |All
    |Mods
    |Vanilla

//type GameFile = GameFile of result : FileResult

type STLGame ( scopeDirectory : string, scope : FilesScope, modFilter : string, triggers : DocEffect list, effects : DocEffect list, modifiers : Modifier list, embeddedFiles : (string * string) list, langs : Lang list, validateVanilla : bool, experimental : bool ) =

        let normalisedScopeDirectory = scopeDirectory.Replace("/","\\").TrimStart('.')
        let scriptFolders = [
                    "common/agendas";
                    "common/ambient_objects";
                    "common/anomalies";
                    "common/armies";
                    "common/army_attachments"; //Removed in 2.0?
                    "common/ascension_perks";
                    "common/attitudes";
                    "common/bombardment_stances";
                    "common/buildable_pops";
                    "common/building_tags";
                    "common/buildings";
                    "common/button_effects";
                    "common/bypass";
                    "common/casus_belli";
                    "common/colors";
                    "common/component_flags"; //Removed in 2.0?
                    "common/component_sets";
                    "common/component_tags";
                    "common/component_templates";
                    "common/country_customization";
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
                    "common/map_modes";
                    "common/megastructures";
                    "common/name_lists";
                    "common/notification_modifiers";
                    "common/observation_station_missions";
                    "common/on_actions";
                    "common/opinion_modifiers";
                    "common/personalities";
                    "common/planet_classes";
                    "common/planet_modifiers";
                    "common/policies";
                    "common/pop_faction_types";
                    "common/precursor_civilizations";
                    //"common/random_names";
                    "common/scripted_effects";
                    "common/scripted_loc";
                    "common/scripted_triggers";
                    "common/scripted_variables";
                    "common/section_templates";
                    "common/sector_types";
                    "common/ship_behaviors";
                    "common/ship_sizes";
                    "common/solar_system_initializers";
                    "common/special_projects";
                    "common/species_archetypes";
                    "common/species_classes";
                    "common/species_names";
                    "common/species_rights";
                    "common/star_classes";
                    "common/starbase_buildings";
                    "common/starbase_levels";
                    "common/starbase_modules";
                    "common/starbase_types";
                    "common/spaceport_modules"; //Removed in 2.0
                    "common/start_screen_messages";
                    "common/static_modifiers";
                    "common/strategic_resources";
                    "common/subjects";
                    "common/system_types";
                    "common/technology";
                    "common/terraform";
                    "common/tile_blockers";
                    "common/tradition_categories";
                    "common/traditions";
                    "common/traits";
                    "common/triggered_modifiers"; //Removed in 2.0
                    "common/war_demand_counters"; //Removed in 2.0
                    "common/war_demand_types"; //Removed in 2.0
                    "common/war_goals";
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

        let resources = ResourceManager(computeSTLData).Api

        let validatableFiles() = resources.ValidatableFiles
        let lookup = Lookup()
        // let mutable scriptedTriggers : Effect list = []
        // let mutable scriptedEffects : Effect list = []
        // let mutable staticModifiers : Modifier list = []
        let mutable localisationAPIs : (bool * ILocalisationAPI) list = []
        let allLocalisation() = localisationAPIs |> List.map snd
        let validatableLocalisation() = localisationAPIs |> List.choose (fun (validate, api) -> if validate then Some api else None)
        let mutable localisationErrors : (string * Severity * range * int * string * string option) list option = None

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
                        |> List.map ((ProcessCore.processNodeBasic "mod" range.Zero) >> (fun s -> s.TagText "name", s.TagText "path", modDir))


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
                    let checkIsGameFolder = (fun (f : string) -> f.Contains "common" || f.Contains "events" || f.Contains "interface" || f.Contains "gfx" || f.Contains "localisation")
                    let foundAnyFolders = Directory.EnumerateDirectories scopeDirectory |> List.ofSeq |> List.exists checkIsGameFolder
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
        
        let getEmbeddedFiles() = embeddedFiles |> List.map (fun (fn, f) -> "embedded", "embeddedfiles/" + fn, f)

        let convertPathToLogicalPath =
            fun (path : string) ->
                let matches = 
                    [
                        if path.Contains "common" then let i = path.IndexOf "common" in yield i, path.Substring(i) else ();
                        if path.Contains "interface" then let i = path.IndexOf "interface" in yield i, path.Substring(i) else ();
                        if path.Contains "gfx" then let i = path.IndexOf "gfx" in yield i, path.Substring(i) else ();
                        if path.Contains "events" then let i = path.IndexOf "events" in yield i, path.Substring(i) else ();
                        if path.Contains "localisation" then let i = path.IndexOf "localisation" in yield i, path.Substring(i) else ();
                        if path.Contains "localisation_synced" then let i = path.IndexOf "localisation_synced" in yield i, path.Substring(i) else ();
                        if path.Contains "map" then let i = path.IndexOf "map" in yield i, path.Substring(i) else ();
                    ]
                if matches.IsEmpty then path else matches |> List.minBy fst |> snd
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
                |".gfx"
                |".asset" ->
                    let rootedpath = filepath.Substring(filepath.IndexOf(normalisedScopeDirectory) + (normalisedScopeDirectory.Length) + 1)
                    Some (EntityResourceInput { scope = scope; filepath = filepath; logicalpath = (convertPathToLogicalPath rootedpath); filetext = File.ReadAllText filepath; validate = true})
                |".dds"
                |".tga"
                |".shader"
                |".lua"
                |".png"
                |".mesh" ->
                    let rootedpath = filepath.Substring(filepath.IndexOf(normalisedScopeDirectory) + (normalisedScopeDirectory.Length) + 1)
                    
                    Some (FileResourceInput { scope = scope; filepath = filepath; logicalpath = convertPathToLogicalPath rootedpath })
                |_ -> None
            let allFiles = duration (fun _ -> PSeq.map getAllFiles allFolders |> PSeq.collect id |> PSeq.choose fileToResourceInput |> List.ofSeq ) "Load files"
            // let allFiles = List.map getAllFiles allFolders |> List.collect id |> List.choose fileToResourceInput
            allFiles

        let updateScriptedTriggers () =
            lookup.scriptedTriggers <- STLLookup.updateScriptedTriggers resources vanillaTriggers


        let updateScriptedEffects () =
            lookup.scriptedEffects <- STLLookup.updateScriptedEffects resources vanillaEffects (lookup.scriptedTriggers)

        let updateStaticodifiers () =
            let rawModifiers =
                resources.AllEntities()
                |> List.choose (function |struct (f, _) when f.filepath.Contains("static_modifiers") -> Some (f.entity) |_ -> None)
                |> List.collect (fun n -> n.Children)
                |> List.rev
            let newModifiers = rawModifiers |> List.map (fun e -> STLProcess.getStaticModifierCategory modifiers e)
            lookup.staticModifiers <- newModifiers

        let updateScriptedLoc () =
            let rawLocs =
                resources.AllEntities()
                |> List.choose (function |struct (f, _) when f.filepath.Contains("scripted_loc") -> Some (f.entity) |_ -> None)
                |> List.collect (fun n -> n.Children)
                |> List.map (fun l -> l.TagText "name")
            lookup.scriptedLoc <- rawLocs
        
        let updateLocalisation() = 
            localisationAPIs <-
                let locs = locFolders |> PSeq.ofList |> PSeq.map (fun (folder, _) -> STLLocalisationService({ folder = folder})) |> PSeq.toList
                let allLocs = locs |> List.collect (fun l -> (STL STLLang.Default :: langs)|> List.map (fun lang -> true, l.Api(lang)))
                match gameDirectory with
                |Some _ -> allLocs
                |None ->  
                    allLocs @ (getEmbeddedFiles() 
                    |> List.filter (fun (_, fn, _ )-> fn.Contains("localisation"))
                    |> List.map (fun (_, fn, f) -> (fn, f))
                    |> (fun files -> STLLocalisationService(files))
                    |> (fun l -> (STL STLLang.Default :: langs) |> List.map (fun lang -> false, l.Api(lang))))
            let taggedKeys = allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.GetKeys) |> List.fold (fun (s : LocKeySet) v -> s.Add v) (LocKeySet.Empty(STLStringComparer())) )
            let validatableEntries = validatableLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList) |> Map.ofList)
            lookup.proccessedLoc <- validatableEntries |> List.map (fun f -> processLocalisation lookup.scriptedEffects lookup.scriptedLoc lookup.definedScriptVariables (EntitySet (resources.AllEntities())) f taggedKeys)
            //TODO: Add loc from embedded

        let updateDefinedVariables() =
            lookup.definedScriptVariables <- (resources.AllEntities()) |> List.collect (fun struct (_, d) -> d.Force().setvariables)
                
        let updateModifiers() =
            lookup.coreModifiers <- addGeneratedModifiers modifiers (EntitySet (resources.AllEntities()))

        let updateTechnologies() =
            lookup.technologies <- getTechnologies (EntitySet (resources.AllEntities()))

        // let findDuplicates (sl : Statement list) =
        //     let node = ProcessCore.processNodeBasic "root" Position.Empty sl
        //     node.Children |> List.groupBy (fun c -> c.Key)
        //                   |> List.filter (fun (_,v) -> v.Length > 1)
        //                   |> List.map (fun (k,_) -> k)

        // let validateDuplicates files =
        //     files |> List.choose (function |(file, parsed) -> Some (file, parsed.statements))
        //         |> List.groupBy (fun (k,_) -> k)
        //         |> List.map ((fun (k, vs) -> k, List.collect (fun (_, vs2) -> vs2) vs)
        //             >> (fun (k, vs) -> k, findDuplicates vs))

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
            tech
            // tech |> List.iter (fun (f, t) -> eprintfn "%s" f)
            
        let validateEvents (entities : Node list) =
            let events = entities |> List.choose (function | :? Event as e -> Some e |_ -> None)
            events |> List.map (fun e -> (valEventVals e) )
                   |> List.choose (function |Invalid es -> Some es |_ -> None)
                   |> List.collect id
        let snood = snd
        let validateAll (entities : struct (Entity * Lazy<STLComputedData>) list)  = 
            eprintfn "Validating %i files" (entities.Length)
            let allEntitiesByFile = entities |> List.map (fun struct (f, _) -> f.entity)
            let flattened = allEntitiesByFile |> List.map (fun n -> n.Children) |> List.collect id

            let validators = [validateVariables, "var"; valTechnology, "tech"; validateTechnologies, "tech2"; valButtonEffects, "but"; valSprites, "sprite"; valVariables, "var2"; valEventCalls, "event"; 
                                validateAmbientGraphics, "ambient"; validateShipDesigns, "designs"; ]
            let experimentalvalidators = [valSectionGraphics, "sections"; valComponentGraphics, "component"; ]
            let oldEntities = EntitySet (resources.AllEntities())
            let newEntities = EntitySet entities
            let runValidators f (validators : (StructureValidator * string) list) =
                (validators <&!&> (fun (v, s) -> duration (fun _ -> f v) s) |> (function |Invalid es -> es |_ -> []))
                @ (if not experimental then [] else experimentalvalidators <&!&> (fun (v, s) -> duration (fun _ -> f v) s) |> (function |Invalid es -> es |_ -> []))
            eprintfn "Validating misc"
            //let res = validators |> List.map (fun v -> v oldEntities newEntities) |> List.fold (<&&>) OK
            let res = runValidators (fun f -> f oldEntities newEntities) validators
            //let res = validators <&!&> (fun v -> v oldEntities newEntities) |> (function |Invalid es -> es |_ -> [])
            eprintfn "Validating files"
            let fileValidators = [valSpriteFiles, "sprites"; valMeshFiles, "mesh"; valAssetFiles, "asset"; valComponentIcons, "compicon"]
            let fres = fileValidators <&!&> (fun (v, s) -> duration (fun _ -> v resources newEntities) s) |> (function |Invalid es -> es |_ -> [])
            eprintfn "Validating effects/triggers"
            let eres = duration (fun _ -> valAllEffects (lookup.scriptedTriggers) (lookup.scriptedEffects) (lookup.staticModifiers) newEntities  |> (function |Invalid es -> es |_ -> [])) "effects"
            let tres = duration (fun _ ->  valAllTriggers (lookup.scriptedTriggers) (lookup.scriptedEffects) (lookup.staticModifiers) newEntities  |> (function |Invalid es -> es |_ -> [])) "triggers"
            let wres = duration (fun _ ->  validateModifierBlocks (lookup.scriptedTriggers) (lookup.scriptedEffects) (lookup.staticModifiers) newEntities |> (function |Invalid es -> es |_ -> [])) "weights"
            let mres = duration (fun _ ->  valAllModifiers (lookup.coreModifiers) newEntities  |> (function |Invalid es -> es |_ -> [])) "modifiers"
            let evres = duration (fun _ ->  ( if experimental then getEventChains (lookup.scriptedEffects) oldEntities newEntities else OK) |> (function |Invalid es -> es |_ -> [])) "events"
            //let etres = getEventChains newEntities |> (function |Invalid es -> es |_ -> [])
            //(validateShips (flattened)) @ (validateEvents (flattened)) @ res @ fres @ eres
            (validateShips (flattened)) @ (validateEvents (flattened)) @ res @ fres @ eres @ tres @ mres @ evres @ wres
        let localisationCheck (entities : struct (Entity * Lazy<STLComputedData>) list) =
            eprintfn "Localisation check %i files" (entities.Length)
            let keys = allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |>List.collect (fun ls -> ls.GetKeys) |> Set.ofList )
            //let allEntries = allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList) |> Map.ofList)
            
            let validators = [valEventLocs; valTechLocs; valCompSetLocs; valCompTempLocs; valBuildingLocs; valTraditionLocCats; valArmiesLoc;
                                 valArmyAttachmentLocs; valDiploPhrases; valShipLoc; valFactionDemands; valSpeciesRightsLocs;
                                 valMapsLocs; valMegastructureLocs; valModifiers; valModules; valTraits; valGoverments; valPersonalities;
                                 valEthics; valPlanetClasses; valEdicts; valPolicies; valSectionTemplates; valSpeciesNames; valStratRes;
                                 valAmbient; valDeposits; valWarGoals; valEffectLocs; valTriggerLocs; valBuildingTags; valOpinionModifiers;
                                 valScriptedTriggers; valSpecialProjects]
            let newEntities = EntitySet entities
            let oldEntities = EntitySet (resources.AllEntities())
            let vs = (validators |> List.map (fun v -> v oldEntities keys newEntities) |> List.fold (<&&>) OK
                       |> (function |Invalid es -> es |_ -> []))
            vs
        
        let globalLocalisation () =
            let taggedKeys = allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.GetKeys) |> List.fold (fun (s : LocKeySet) v -> s.Add v) (LocKeySet.Empty(STLStringComparer())) )

            let validatableEntries = validatableLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList) |> Map.ofList)
            let oldEntities = EntitySet (resources.AllEntities())

            // let apiValidators = [validateLocalisation]
            // let apiVs = validatableEntries <&!&> (fun l -> apiValidators |> List.fold (fun s v -> s <&&> v lookup.scriptedEffects lookup.scriptedLoc lookup.definedScriptVariables oldEntities l taggedKeys) OK)
            //                  |> (function |Invalid es -> es |_ -> [])
            //apiVs    
            lookup.proccessedLoc |> validateProcessedLocalisation taggedKeys |> (function |Invalid es -> es |_ -> [])          

        let updateFile filepath =
            eprintfn "%s" filepath
            match filepath with
            |x when x.EndsWith (".yml") ->
                updateLocalisation()
                globalLocalisation()
            | _ ->
                let filepath = Path.GetFullPath(filepath)
                let file = File.ReadAllText filepath
                let rootedpath = filepath.Substring(filepath.IndexOf(normalisedScopeDirectory) + (normalisedScopeDirectory.Length))
                let logicalpath = convertPathToLogicalPath rootedpath
                //eprintfn "%s %s" logicalpath filepath
                let newEntities = resources.UpdateFile (EntityResourceInput {scope = ""; filepath = filepath; logicalpath = logicalpath; filetext = file; validate = true})
                match filepath with
                |x when x.Contains("scripted_triggers") -> updateScriptedTriggers()
                |x when x.Contains("scripted_effects") -> updateScriptedEffects()
                |x when x.Contains("static_modifiers") -> updateStaticodifiers()
                |_ -> ()
                updateDefinedVariables()
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
            let embedded = embeddedFiles |> List.map (fun (f, ft) -> if ft = "" then FileResourceInput { scope = "embedded"; filepath = f; logicalpath = (convertPathToLogicalPath f) } else EntityResourceInput {scope = "embedded"; filepath = f; logicalpath = (convertPathToLogicalPath f); filetext = ft; validate = false})
            match gameDirectory with
            |None -> resources.UpdateFiles(embedded) |> ignore
            | _ -> ()

            updateScriptedTriggers()
            updateScriptedEffects()
            updateStaticodifiers()
            updateScriptedLoc()
            updateDefinedVariables()
            updateModifiers()
            updateTechnologies()
            updateLocalisation()

        //member __.Results = parseResults
        member __.ParserErrors = parseErrors()
        member __.ValidationErrors = (validateAll (resources.ValidatableEntities()))
        member __.LocalisationErrors= 
            match localisationErrors with
            |Some les -> les
            |None -> 
                let les = (localisationCheck (resources.ValidatableEntities())) @ globalLocalisation()
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
        member __.References = References<STLComputedData>(resources, lookup, (localisationAPIs |> List.map snd))
       

        //member __.ScriptedTriggers = parseResults |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some p |_ -> None) |> List.map (fun t -> )