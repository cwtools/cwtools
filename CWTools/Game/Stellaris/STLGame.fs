namespace CWTools.Games.Stellaris

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
open CWTools.Games
open CWTools.Games.Stellaris
open CWTools.Games.Stellaris.STLLookup
open Microsoft.FSharp.Compiler.Range
open CWTools.Validation.Rules
open CWTools.Parser
open CWTools.Parser.ConfigParser
open FSharp.Data.Runtime
open CWTools.Validation.Stellaris.ScopeValidation
open Files

type EmbeddedSettings = {
    triggers : DocEffect list
    effects : DocEffect list
    modifiers : Modifier list
    embeddedFiles : (string * string) list
}

type RulesSettings = {
    ruleFiles : (string * string) list
    validateRules : bool
}

type ValidationSettings = {
    langs : Lang list
    validateVanilla : bool
    experimental : bool
}

type StellarisSettings = {
    rootDirectory : string
    scope : FilesScope
    modFilter : string option
    embedded : EmbeddedSettings
    validation : ValidationSettings
    rules : RulesSettings option
}

//type GameFile = GameFile of result : FileResult

type STLGame (settings : StellarisSettings) =
        let embeddedSettings = settings.embedded
        let validationSettings = settings.validation
        let useRules = settings.rules.IsSome

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

        let fileManager = FileManager(settings.rootDirectory, settings.modFilter, settings.scope, scriptFolders, "stellaris")
        let vanillaEffects =
            let se = scopedEffects |> List.map (fun e -> e :> Effect)
            let ve = settings.embedded.effects |> addInnerScope |> List.map (fun e -> e :> Effect)
            se @ ve
        let vanillaTriggers =
            let se = scopedEffects |> List.map (fun e -> e :> Effect)
            let vt = settings.embedded.triggers |> addInnerScope |> List.map (fun e -> e :> Effect)
            se @ vt

        let resourceManager = ResourceManager(computeSTLData)
        let resources = resourceManager.Api

        let validatableFiles() = resources.ValidatableFiles
        let lookup = Lookup()
        let mutable localisationAPIs : (bool * ILocalisationAPI) list = []
        let allLocalisation() = localisationAPIs |> List.map snd
        let validatableLocalisation() = localisationAPIs |> List.choose (fun (validate, api) -> if validate then Some api else None)
        let mutable localisationErrors : (string * Severity * range * int * string * string option) list option = None


        let getEmbeddedFiles() = settings.embedded.embeddedFiles |> List.map (fun (fn, f) -> "embedded", "embeddedfiles/" + fn, f)


        let updateScriptedTriggers () =
            lookup.scriptedTriggers <- STLLookup.updateScriptedTriggers resources vanillaTriggers


        let updateScriptedEffects () =
            lookup.scriptedEffects <- STLLookup.updateScriptedEffects resources vanillaEffects (lookup.scriptedTriggers)

        let updateStaticodifiers () =
            let rawModifiers =
                resources.AllEntities()
                |> List.choose (function |struct (f, _) when f.filepath.Contains("static_modifiers") -> Some (f.entity) |_ -> None)
                |> List.collect (fun n -> n.Children)
                //|> List.rev
            let newModifiers = rawModifiers |> List.map (fun e -> STLProcess.getStaticModifierCategory settings.embedded.modifiers e)
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
                let locs = fileManager.LocalisationFiles() |> PSeq.ofList |> PSeq.map (fun (folder, _) -> STLLocalisationService({ folder = folder})) |> PSeq.toList
                let allLocs = locs |> List.collect (fun l -> (STL STLLang.Default :: settings.validation.langs)|> List.map (fun lang -> true, l.Api(lang)))
                match fileManager.ShouldUseEmbedded with
                |false -> allLocs
                |true ->
                    allLocs @ (getEmbeddedFiles()
                    |> List.filter (fun (_, fn, _ )-> fn.Contains("localisation"))
                    |> List.map (fun (_, fn, f) -> (fn, f))
                    |> (fun files -> STLLocalisationService(files))
                    |> (fun l -> (STL STLLang.Default :: settings.validation.langs) |> List.map (fun lang -> false, l.Api(lang))))
            let taggedKeys = allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.GetKeys) |> List.fold (fun (s : LocKeySet) v -> s.Add v) (LocKeySet.Empty(InsensitiveStringComparer())) )
            let validatableEntries = validatableLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList) |> Map.ofList)
            lookup.proccessedLoc <- validatableEntries |> List.map (fun f -> processLocalisation lookup.scriptedEffects lookup.scriptedLoc lookup.definedScriptVariables (EntitySet (resources.AllEntities())) f taggedKeys)

            //TODO: Add loc from embedded

        let updateDefinedVariables() =
            lookup.definedScriptVariables <- (resources.AllEntities()) |> List.collect (fun struct (_, d) -> d.Force().setvariables)

        let updateModifiers() =
            lookup.coreModifiers <- addGeneratedModifiers settings.embedded.modifiers (EntitySet (resources.AllEntities()))

        let updateTechnologies() =
            lookup.technologies <- getTechnologies (EntitySet (resources.AllEntities()))

        let updateTypeDef() =
            match settings.rules with
            |Some rulesSettings ->
                let rules, types, enums, complexenums = rulesSettings.ruleFiles |> List.fold (fun (rs, ts, es, ces) (fn, ft) -> let r2, t2, e2, ce2 = parseConfig fn ft in rs@r2, ts@t2, es@e2, ces@ce2) ([], [], [], [])
                let rulesWithMod = rules @ (lookup.coreModifiers |> List.map (fun c -> AliasRule ("modifier", Rule(c.tag, {min = 0; max = 100; leafvalue = false; description = None; pushScope = None}, ValueField (ValueType.Float (-100000.0, 100000.0))))))
                let complexEnumDefs = getEnumsFromComplexEnums complexenums (resources.AllEntities() |> List.map (fun struct(e,_) -> e))
                let allEnums = enums @ complexEnumDefs
                lookup.configRules <- rulesWithMod
                lookup.typeDefs <- types
                lookup.enumDefs <- allEnums |> Map.ofList
                let loc = allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |>List.collect (fun ls -> ls.GetKeys) |> Set.ofList )
                let files = resources.GetResources() |> List.choose (function |FileResource (_, f) -> Some f.logicalpath |EntityResource (_, f) -> Some f.logicalpath) |> Set.ofList
                let ruleApplicator = RuleApplicator(lookup.configRules, lookup.typeDefs, lookup.typeDefInfo, lookup.enumDefs, loc, files, lookup.scriptedTriggers, lookup.scriptedEffects)
                lookup.typeDefInfo <- getTypesFromDefinitions ruleApplicator types (resources.AllEntities() |> List.map (fun struct(e,_) -> e))
            |None -> ()

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
        let mutable ruleApplicator : RuleApplicator option = None
        let validateAll (shallow : bool) (entities : struct (Entity * Lazy<STLComputedData>) list)  =
            //let ruleApplicator = RuleApplicator(lookup.configRules, lookup.typeDefs, lookup.typeDefInfo, lookup.enumDefs, loc, files, lookup.scriptedTriggers, lookup.scriptedEffects)
            eprintfn "Validating %i files" (entities.Length)
            let allEntitiesByFile = entities |> List.map (fun struct (f, _) -> f.entity)
            let flattened = allEntitiesByFile |> List.map (fun n -> n.Children) |> List.collect id

            let validators = [validateVariables, "var"; valTechnology, "tech"; validateTechnologies, "tech2"; valButtonEffects, "but"; valSprites, "sprite"; valVariables, "var2"; valEventCalls, "event";
                                validateAmbientGraphics, "ambient"; validateShipDesigns, "designs"; validateMixedBlocks, "mixed"; validateSolarSystemInitializers, "solar"; validateAnomaly210, "anom";
                                validateIfElse210, "ifelse"; validateIfElse, "ifelse2"; validatePlanetKillers, "pk"; validateRedundantAND, "AND"]
            let validators = if useRules && ruleApplicator.IsSome then (ruleApplicator.Value.RuleValidate, "rules")::validators else validators
            let experimentalvalidators = [valSectionGraphics, "sections"; valComponentGraphics, "component"; ]
            let oldEntities = EntitySet (resources.AllEntities())
            let newEntities = EntitySet entities
            let runValidators f (validators : (StructureValidator * string) list) =
                (validators <&!!&> (fun (v, s) -> duration (fun _ -> f v) s) |> (function |Invalid es -> es |_ -> []))
                @ (if not settings.validation.experimental then [] else experimentalvalidators <&!&> (fun (v, s) -> duration (fun _ -> f v) s) |> (function |Invalid es -> es |_ -> []))
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
            let evres = duration (fun _ ->  ( if settings.validation.experimental && (not shallow) then getEventChains (lookup.scriptedEffects) oldEntities newEntities else OK) |> (function |Invalid es -> es |_ -> [])) "events"
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
                                 valScriptedTriggers; valSpecialProjects; valStarbaseType; valTileBlockers; valAnomalies]
            let newEntities = EntitySet entities
            let oldEntities = EntitySet (resources.AllEntities())
            let vs = (validators |> List.map (fun v -> v oldEntities keys newEntities) |> List.fold (<&&>) OK
                       |> (function |Invalid es -> es |_ -> []))
            vs

        let globalLocalisation () =
            let taggedKeys = allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.GetKeys) |> List.fold (fun (s : LocKeySet) v -> s.Add v) (LocKeySet.Empty(InsensitiveStringComparer())) )

            let validatableEntries = validatableLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList) |> Map.ofList)
            let oldEntities = EntitySet (resources.AllEntities())

            // let apiValidators = [validateLocalisation]
            // let apiVs = validatableEntries <&!&> (fun l -> apiValidators |> List.fold (fun s v -> s <&&> v lookup.scriptedEffects lookup.scriptedLoc lookup.definedScriptVariables oldEntities l taggedKeys) OK)
            //                  |> (function |Invalid es -> es |_ -> [])
            //apiVs
            lookup.proccessedLoc |> validateProcessedLocalisation taggedKeys |> (function |Invalid es -> es |_ -> [])

        let updateFile filepath (filetext : string option) =
            eprintfn "%s" filepath
            let timer = new System.Diagnostics.Stopwatch()
            timer.Start()
            let res =
                match filepath with
                |x when x.EndsWith (".yml") ->
                    updateLocalisation()
                    let les = (localisationCheck (resources.ValidatableEntities())) @ globalLocalisation()
                    localisationErrors <- Some les
                    globalLocalisation()
                | _ ->
                    let filepath = Path.GetFullPath(filepath)
                    let file = filetext |> Option.defaultWith (fun () -> File.ReadAllText filepath)
                    let rootedpath = filepath.Substring(filepath.IndexOf(fileManager.ScopeDirectory) + (fileManager.ScopeDirectory.Length))
                    let logicalpath = fileManager.ConvertPathToLogicalPath rootedpath
                    //eprintfn "%s %s" logicalpath filepath
                    let newEntities = resources.UpdateFile (EntityResourceInput {scope = ""; filepath = filepath; logicalpath = logicalpath; filetext = file; validate = true})
                    match filepath with
                    |x when x.Contains("scripted_triggers") -> updateScriptedTriggers()
                    |x when x.Contains("scripted_effects") -> updateScriptedEffects()
                    |x when x.Contains("static_modifiers") -> updateStaticodifiers()
                    |_ -> ()
                    updateDefinedVariables()
                    validateAll true newEntities @ localisationCheck newEntities
            eprintfn "Update Time: %i" timer.ElapsedMilliseconds
            res
        let mutable completionService : CompletionService option = None
        let mutable infoService : FoldRules option = None
        let completion (pos : pos) (filepath : string) (filetext : string) =
            // let filepath = Path.GetFullPath(filepath).Replace("/","\\")
            // match resources.AllEntities() |> List.tryFind (fun struct (e, _) -> e.filepath == filepath) with
            // |Some struct (e, _) ->
            //     let completion = CompletionService([ConfigParser.building; ConfigParser.shipsize])
            //     completion.Complete(pos, e.entity)
            // |None -> []
            let split = filetext.Split('\n')
            let filetext = split |> Array.mapi (fun i s -> if i = (pos.Line - 1) then eprintfn "%s" s; s.Insert(pos.Column, "x") else s) |> String.concat "\n"
            match resourceManager.ManualProcess (fileManager.ConvertPathToLogicalPath filepath) filetext, completionService, infoService with
            |Some e, Some completion, Some info ->
                eprintfn "completion %A %A" (fileManager.ConvertPathToLogicalPath filepath) filepath
                eprintfn "scope at cursor %A" (getScopeContextAtPos pos lookup.scriptedTriggers lookup.scriptedEffects e)
                completion.Complete(pos, e)
            |_, _, _ -> []

        let getInfoAtPos (pos : pos) (filepath : string) (filetext : string) =
            match resourceManager.ManualProcess (fileManager.ConvertPathToLogicalPath filepath) filetext, infoService with
            |Some e, Some info ->
                eprintfn "getInfo %A %A" (fileManager.ConvertPathToLogicalPath filepath) filepath
                match info.GetInfo(pos, e) with
                |Some (_, Some (t, tv)) -> lookup.typeDefInfo.[t] |> List.tryPick (fun (n, v) -> if n = tv then Some v else None)
                |_ -> None
            |_, _ -> None


        let scopesAtPos (pos : pos) (filepath : string) (filetext : string) =
            let split = filetext.Split('\n')
            let filetext = split |> Array.mapi (fun i s -> if i = (pos.Line - 1) then eprintfn "%s" s; s.Insert(pos.Column, "x") else s) |> String.concat "\n"
            match resourceManager.ManualProcess (fileManager.ConvertPathToLogicalPath filepath) filetext, infoService with
            |Some e, Some info ->
                match info.GetInfo(pos, e) with
                |Some (ctx, _) when ctx.scopes <> { Root = Scope.Any; From = []; Scopes = [] } -> Some (ctx.scopes)
                |_ -> getScopeContextAtPos pos lookup.scriptedTriggers lookup.scriptedEffects e
            |Some e, _ -> getScopeContextAtPos pos lookup.scriptedTriggers lookup.scriptedEffects e
            |_ -> None


        do
            eprintfn "Parsing %i files" (fileManager.AllFilesByPath().Length)
            // let efiles = allFilesByPath |> List.filter (fun (_, f, _) -> not(f.EndsWith(".dds")))
            //             |> List.map (fun (s, f, ft) -> EntityResourceInput {scope = s; filepath = f; filetext = ft; validate = true})
            // let otherfiles = allFilesByPath |> List.filter (fun (_, f, _) -> f.EndsWith(".dds"))
            //                     |> List.map (fun (s, f, _) -> FileResourceInput {scope = s; filepath = f;})
            let files = fileManager.AllFilesByPath()
            let filteredfiles = if settings.validation.validateVanilla then files else files |> List.choose (function |FileResourceInput f -> Some (FileResourceInput f) |EntityResourceInput f -> if f.scope = "vanilla" then Some (EntityResourceInput {f with validate = false}) else Some (EntityResourceInput f))
            resources.UpdateFiles(filteredfiles) |> ignore
            let embedded = settings.embedded.embeddedFiles |> List.map (fun (f, ft) -> if ft = "" then FileResourceInput { scope = "embedded"; filepath = f; logicalpath = (fileManager.ConvertPathToLogicalPath f) } else EntityResourceInput {scope = "embedded"; filepath = f; logicalpath = (fileManager.ConvertPathToLogicalPath f); filetext = ft; validate = false})
            if fileManager.ShouldUseEmbedded then resources.UpdateFiles(embedded) |> ignore else ()

            updateScriptedTriggers()
            updateScriptedEffects()
            updateStaticodifiers()
            updateScriptedLoc()
            updateDefinedVariables()
            updateModifiers()
            updateTechnologies()
            updateLocalisation()
            updateTypeDef()

            let loc = allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |>List.collect (fun ls -> ls.GetKeys) |> Set.ofList )
            let files = resources.GetResources() |> List.choose (function |FileResource (_, f) -> Some f.logicalpath |EntityResource (_, f) -> Some f.logicalpath) |> Set.ofList
            completionService <- Some (CompletionService(lookup.configRules, lookup.typeDefs, lookup.typeDefInfo, lookup.enumDefs))
            ruleApplicator <- Some (RuleApplicator(lookup.configRules, lookup.typeDefs, lookup.typeDefInfo, lookup.enumDefs, loc, files, lookup.scriptedTriggers, lookup.scriptedEffects))
            infoService <- Some (FoldRules(lookup.configRules, lookup.typeDefs, lookup.typeDefInfo, lookup.enumDefs, loc, files, lookup.scriptedTriggers, lookup.scriptedEffects, ruleApplicator.Value))
        interface IGame<STLComputedData> with
        //member __.Results = parseResults
            member __.ParserErrors() = parseErrors()
            member __.ValidationErrors() = (validateAll false (resources.ValidatableEntities()))
            member __.LocalisationErrors(force : bool) =
                let generate =
                    let les = (localisationCheck (resources.ValidatableEntities())) @ globalLocalisation()
                    localisationErrors <- Some les
                    les
                match localisationErrors with
                |Some les -> if force then generate else les
                |None -> generate

            //member __.ValidationWarnings = warningsAll
            member __.Folders() = fileManager.AllFolders()
            member __.AllFiles() =
                resources.GetResources()
                // |> List.map
                //     (function
                //         |EntityResource (f, r) ->  r.result |> function |(Fail (result)) -> (r.filepath, false, result.parseTime) |Pass(result) -> (r.filepath, true, result.parseTime)
                //         |FileResource (f, r) ->  (r.filepath, false, 0L))
                //|> List.map (fun r -> r.result |> function |(Fail (result)) -> (r.filepath, false, result.parseTime) |Pass(result) -> (r.filepath, true, result.parseTime))
            member __.ScriptedTriggers() = lookup.scriptedTriggers
            member __.ScriptedEffects() = lookup.scriptedEffects
            member __.StaticModifiers() = lookup.staticModifiers
            member __.UpdateFile file text = updateFile file text
            member __.AllEntities() = resources.AllEntities()
            member __.References() = References<STLComputedData>(resources, lookup, (localisationAPIs |> List.map snd))
            member __.Complete pos file text = completion pos file text
            member __.ScopesAtPos pos file text = scopesAtPos pos file text
            member __.GoToType pos file text = getInfoAtPos pos file text


            //member __.ScriptedTriggers = parseResults |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some p |_ -> None) |> List.map (fun t -> )