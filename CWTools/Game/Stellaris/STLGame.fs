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
open CWTools.Utilities.Position
open CWTools.Validation.Rules
open CWTools.Validation.Stellaris.STLRules
open CWTools.Parser
open CWTools.Parser.ConfigParser
open FSharp.Data.Runtime
open CWTools.Validation.Stellaris.ScopeValidation
open Files
open CWTools.Validation.Stellaris
open CWTools.Validation.Common.CommonValidation
open CWTools.Validation
open CWTools.Process.Scopes

type EmbeddedSettings = {
    triggers : DocEffect list
    effects : DocEffect list
    modifiers : Modifier list
    embeddedFiles : (string * string) list
    cachedResourceData : (Resource * Entity) list
}

type RulesSettings = {
    ruleFiles : (string * string) list
    validateRules : bool
    debugRulesOnly : bool
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


        let fileManager = FileManager(settings.rootDirectory, settings.modFilter, settings.scope, scriptFolders, "stellaris")
        let vanillaEffects =
            let se = scopedEffects |> List.map (fun e -> e :> Effect)
            let ve = settings.embedded.effects |> addInnerScope |> List.map (fun e -> e :> Effect)
            se @ ve
        let vanillaTriggers =
            let se = scopedEffects |> List.map (fun e -> e :> Effect)
            let vt = settings.embedded.triggers |> addInnerScope |> List.map (fun e -> e :> Effect)
            se @ vt
        let mutable completionService = None
        let mutable infoService = None

        let resourceManager = ResourceManager(STLCompute.computeSTLData (fun () -> infoService))
        let resources = resourceManager.Api

        let validatableFiles() = resources.ValidatableFiles
        let lookup = Lookup()
        let mutable localisationAPIs : (bool * ILocalisationAPI) list = []
        let allLocalisation() = localisationAPIs |> List.map snd
        let validatableLocalisation() = localisationAPIs |> List.choose (fun (validate, api) -> if validate then Some api else None)
        let mutable localisationKeys = []
        let mutable taggedLocalisationKeys = []
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
            localisationKeys <-allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |>List.collect (fun ls -> ls.GetKeys) |> Set.ofList )
            taggedLocalisationKeys <- allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.GetKeys) |> List.fold (fun (s : LocKeySet) v -> s.Add v) (LocKeySet.Empty(InsensitiveStringComparer())) )
            let validatableEntries = validatableLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList) |> Map.ofList)
            lookup.proccessedLoc <- validatableEntries |> List.map (fun f -> processLocalisation lookup.scriptedEffects lookup.scriptedLoc lookup.definedScriptVariables (EntitySet (resources.AllEntities())) f taggedLocalisationKeys)

            //TODO: Add loc from embedded

        let updateDefinedVariables() =
            lookup.definedScriptVariables <- (resources.AllEntities()) |> List.collect (fun struct (_, d) -> d.Force().Setvariables)

        let updateModifiers() =
            lookup.coreModifiers <- addGeneratedModifiers settings.embedded.modifiers (EntitySet (resources.AllEntities()))

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

        // let validateShips (entities : Node list) =
        //     let ships = entities |> List.choose (function | :? Ship as s -> Some s |_ -> None)
        //     ships |> List.map validateShip
        //           |> List.choose (function |Invalid es -> Some es |_ -> None)
        //           |> List.collect id

        let parseErrors() =
            resources.GetResources()
                |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None)
                |> List.choose (fun r -> r.result |> function |(Fail (result)) when r.validate -> Some (r.filepath, result.error, result.position)  |_ -> None)


        let validateTechnology (entities : (string * Node) list) =
            let tech = entities |> List.filter (fun (f, _) -> f.Contains("common/technology/"))
            tech
            // tech |> List.iter (fun (f, t) -> eprintfn "%s" f)

        // let validateEvents (entities : Node list) =
        //     let events = entities |> List.choose (function | :? Event as e -> Some e |_ -> None)
        //     events |> List.map (fun e -> (valEventVals e) )
        //            |> List.choose (function |Invalid es -> Some es |_ -> None)
        //            |> List.collect id
        let mutable ruleApplicator : CWTools.Validation.RuleApplicator<Scope, _> option = None
        let validationSettings = {
            validators = [validateVariables, "var"; valTechnology, "tech"; validateTechnologies, "tech2"; valButtonEffects, "but"; valSprites, "sprite"; valVariables, "var2"; valEventCalls, "event";
                                validateAmbientGraphics, "ambient"; validateShipDesigns, "designs"; validateMixedBlocks, "mixed"; validateSolarSystemInitializers, "solar"; validateAnomaly210, "anom";
                                validateIfElse210, "ifelse"; validateIfElse, "ifelse2"; validatePlanetKillers, "pk"; validateRedundantAND, "AND"; valFlags, "flags"; valMegastructureGraphics, "megastructure";
                                valPlanetClassGraphics, "pcg"; validateDeprecatedSetName, "setname"; validateShips, "ships"; validateEvents, "eventsSimple"]
            experimentalValidators = [valSectionGraphics, "sections"; valComponentGraphics, "component"]
            heavyExperimentalValidators = [getEventChains, "event chains"]
            experimental = settings.validation.experimental
            fileValidators = [valSpriteFiles, "sprites"; valMeshFiles, "mesh"; valAssetFiles, "asset"; valComponentIcons, "compicon"]
            resources = resources
            lookup = lookup
            lookupValidators = [valAllEffects, "effects"; valAllTriggers, "triggers"; validateModifierBlocks, "mod blocks"; valAllModifiers, "mods"]
            ruleApplicator = ruleApplicator
            useRules = useRules
            debugRulesOnly = settings.rules |> Option.map (fun o -> o.debugRulesOnly) |> Option.defaultValue false
    // validators : (StructureValidator<'T> * string) list
    // experimentalValidators : (StructureValidator<'T> * string) list
    // heavyExperimentalValidators : (LookupValidator<'T> * string) list
    // experimental : bool
    // fileValidators : (FileValidator<'T> * string) list
    // resources : IResourceAPI<'T>
    // lookup : Lookup
    // lookupValidators : (LookupValidator<'T> * string) list
        }

        let mutable validationManager = ValidationManager(validationSettings)
        let validateAll shallow newEntities = validationManager.Validate(shallow, newEntities)
        // let validateAll (shallow : bool) (entities : struct (Entity * Lazy<STLComputedData>) list)  =
        //     //let ruleApplicator = RuleApplicator(lookup.configRules, lookup.typeDefs, lookup.typeDefInfo, lookup.enumDefs, loc, files, lookup.scriptedTriggers, lookup.scriptedEffects)
        //     eprintfn "Validating %i files" (entities.Length)
        //     let allEntitiesByFile = entities |> List.map (fun struct (f, _) -> f.entity)
        //     let flattened = allEntitiesByFile |> List.map (fun n -> n.Children) |> List.collect id

        //     let validators = [validateVariables, "var"; valTechnology, "tech"; validateTechnologies, "tech2"; valButtonEffects, "but"; valSprites, "sprite"; valVariables, "var2"; valEventCalls, "event";
        //                         validateAmbientGraphics, "ambient"; validateShipDesigns, "designs"; validateMixedBlocks, "mixed"; validateSolarSystemInitializers, "solar"; validateAnomaly210, "anom";
        //                         validateIfElse210, "ifelse"; validateIfElse, "ifelse2"; validatePlanetKillers, "pk"; validateRedundantAND, "AND"; valFlags, "flags"; valMegastructureGraphics, "megastructure";
        //                         valPlanetClassGraphics, "pcg"; validateDeprecatedSetName, "setname"; validateShips, "ships"; validateEvents, "eventsSimple"]
        //     let validators = if useRules && ruleApplicator.IsSome then (ruleApplicator.Value.RuleValidate, "rules")::validators else validators
        //     let experimentalvalidators = [valSectionGraphics, "sections"; valComponentGraphics, "component"]
        //     let oldEntities = EntitySet (resources.AllEntities())
        //     let newEntities = EntitySet entities
        //     let runValidators f (validators : (STLStructureValidator * string) list) =
        //         (validators <&!!&> (fun (v, s) -> duration (fun _ -> f v) s) |> (function |Invalid es -> es |_ -> []))
        //         @ (if not settings.validation.experimental then [] else experimentalvalidators <&!&> (fun (v, s) -> duration (fun _ -> f v) s) |> (function |Invalid es -> es |_ -> []))
        //     eprintfn "Validating misc"
        //     //let res = validators |> List.map (fun v -> v oldEntities newEntities) |> List.fold (<&&>) OK
        //     let res = runValidators (fun f -> f oldEntities newEntities) validators
        //     //let res = validators <&!&> (fun v -> v oldEntities newEntities) |> (function |Invalid es -> es |_ -> [])
        //     eprintfn "Validating files"
        //     let STLFileValidators = [valSpriteFiles, "sprites"; valMeshFiles, "mesh"; valAssetFiles, "asset"; valComponentIcons, "compicon"]
        //     let fres = STLFileValidators <&!&> (fun (v, s) -> duration (fun _ -> v resources newEntities) s) |> (function |Invalid es -> es |_ -> [])
        //     eprintfn "Validating effects/triggers"
        //     let eres = duration (fun _ -> valAllEffects (lookup.scriptedTriggers) (lookup.scriptedEffects) (lookup.staticModifiers) newEntities  |> (function |Invalid es -> es |_ -> [])) "effects"
        //     let tres = duration (fun _ ->  valAllTriggers (lookup) oldEntities newEntities  |> (function |Invalid es -> es |_ -> [])) "triggers"
        //     let wres = duration (fun _ ->  validateModifierBlocks (lookup.scriptedTriggers) (lookup.scriptedEffects) (lookup.staticModifiers) newEntities |> (function |Invalid es -> es |_ -> [])) "weights"
        //     let mres = duration (fun _ ->  valAllModifiers (lookup.coreModifiers) newEntities  |> (function |Invalid es -> es |_ -> [])) "modifiers"
        //     let evres = duration (fun _ ->  ( if settings.validation.experimental && (not(shallow)) then getEventChains (lookup.scriptedEffects) oldEntities newEntities else OK) |> (function |Invalid es -> es |_ -> [])) "events"
        //     //let etres = getEventChains newEntities |> (function |Invalid es -> es |_ -> [])
        //     //(validateShips (flattened)) @ (validateEvents (flattened)) @ res @ fres @ eres
        //     let shallow = (validateShips (flattened)) @ (validateEvents (flattened)) @ res @ fres @ eres @ tres @ mres @ wres
        //     let deep = evres
        //     shallow, deep
        let localisationCheck (entities : struct (Entity * Lazy<STLComputedData>) list) =
            eprintfn "Localisation check %i files" (entities.Length)
            //let keys = allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |>List.collect (fun ls -> ls.GetKeys) |> Set.ofList )
            //let allEntries = allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList) |> Map.ofList)

            let validators = [valEventLocs; valTechLocs; valCompSetLocs; valCompTempLocs; valBuildingLocs; valTraditionLocCats; valArmiesLoc;
                                 valArmyAttachmentLocs; valDiploPhrases; valShipLoc; valFactionDemands; valSpeciesRightsLocs;
                                 valMapsLocs; valMegastructureLocs; valModifiers; valModules; valTraits; valGoverments; valPersonalities;
                                 valEthics; valPlanetClasses; valEdicts; valPolicies; valSectionTemplates; valSpeciesNames; valStratRes;
                                 valAmbient; valDeposits; valWarGoals; valEffectLocs; valTriggerLocs; valBuildingTags; valOpinionModifiers;
                                 valScriptedTriggers; valSpecialProjects; valStarbaseType; valTileBlockers; valAnomalies]
            let newEntities = EntitySet entities
            let oldEntities = EntitySet (resources.AllEntities())
            let vs = (validators |> List.map (fun v -> v oldEntities localisationKeys newEntities) |> List.fold (<&&>) OK)
            ((vs) |> (function |Invalid es -> es |_ -> []))

        let globalLocalisation () =
            // let taggedKeys = allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.GetKeys) |> List.fold (fun (s : LocKeySet) v -> s.Add v) (LocKeySet.Empty(InsensitiveStringComparer())) )

            // let validatableEntries = validatableLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList) |> Map.ofList)
            // let oldEntities = EntitySet (resources.AllEntities())

            // let apiValidators = [validateLocalisation]
            // let apiVs = validatableEntries <&!&> (fun l -> apiValidators |> List.fold (fun s v -> s <&&> v lookup.scriptedEffects lookup.scriptedLoc lookup.definedScriptVariables oldEntities l taggedKeys) OK)
            //                  |> (function |Invalid es -> es |_ -> [])
            //apiVs
            let locFileValidation = (fileManager.LocalisationFiles() |> List.map fst) <&!&> validateLocalisationFiles
            lookup.proccessedLoc |> validateProcessedLocalisation taggedLocalisationKeys <&&> locFileValidation |> (function |Invalid es -> es |_ -> [])

        let makeEntityResourceInput filepath filetext  =
            let filepath = Path.GetFullPath(filepath)
            let indexOfScope = filepath.IndexOf(fileManager.ScopeDirectory)
            let rootedpath =
                if indexOfScope = -1
                then filepath
                else filepath.Substring(indexOfScope + (fileManager.ScopeDirectory.Length))
            let logicalpath = fileManager.ConvertPathToLogicalPath rootedpath
            EntityResourceInput {scope = ""; filepath = filepath; logicalpath = logicalpath; filetext = filetext; validate = true}
        let mutable errorCache = Map.empty
        let updateFile (shallow : bool) filepath (filetext : string option) =
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
                    let file = filetext |> Option.defaultWith (fun () -> File.ReadAllText filepath)
                    let resource = makeEntityResourceInput filepath file
                    // let filepath = Path.GetFullPath(filepath)
                    // let rootedpath = filepath.Substring(filepath.IndexOf(fileManager.ScopeDirectory) + (fileManager.ScopeDirectory.Length))
                    // let logicalpath = fileManager.ConvertPathToLogicalPath rootedpath
                    //eprintfn "%s %s" logicalpath filepath
                    let newEntities = resources.UpdateFile (resource) |> List.map snd
                    match filepath with
                    |x when x.Contains("scripted_triggers") -> updateScriptedTriggers()
                    |x when x.Contains("scripted_effects") -> updateScriptedEffects()
                    |x when x.Contains("scripted_loc") -> updateScriptedLoc()
                    |x when x.Contains("static_modifiers") -> updateStaticodifiers()
                    |_ -> ()
                    updateDefinedVariables()
                    match shallow with
                    |true ->
                        let (shallowres, _) = (validateAll shallow newEntities)
                        let shallowres = shallowres @ (localisationCheck newEntities)
                        let deep = errorCache |> Map.tryFind filepath |> Option.defaultValue []
                        shallowres @ deep
                    |false ->
                        let (shallowres, deepres) = (validateAll shallow newEntities)
                        let shallowres = shallowres @ (localisationCheck newEntities)
                        errorCache <- errorCache.Add(filepath, deepres)
                        shallowres @ deepres
                    //validateAll shallow newEntities @ localisationCheck newEntities
            eprintfn "Update Time: %i" timer.ElapsedMilliseconds
            res
        let completion (pos : pos) (filepath : string) (filetext : string) =
            let split = filetext.Split('\n')
            let filetext = split |> Array.mapi (fun i s -> if i = (pos.Line - 1) then eprintfn "%s" s; s.Insert(pos.Column, "x") else s) |> String.concat "\n"
            let resource = makeEntityResourceInput filepath filetext
            match resourceManager.ManualProcessResource resource, completionService with
            |Some e, Some completion ->
                eprintfn "completion %A %A" (fileManager.ConvertPathToLogicalPath filepath) filepath
                //eprintfn "scope at cursor %A" (getScopeContextAtPos pos lookup.scriptedTriggers lookup.scriptedEffects e.entity)
                completion(pos, e)
            |_, _ -> []


        let getInfoAtPos (pos : pos) (filepath : string) (filetext : string) =
            let resource = makeEntityResourceInput filepath filetext
            match resourceManager.ManualProcessResource resource, infoService with
            |Some e, Some info ->
                eprintfn "getInfo %A %A" (fileManager.ConvertPathToLogicalPath filepath) filepath
                match (info |> fst)(pos, e) with
                |Some (_, Some (t, tv)) ->
                    lookup.typeDefInfo.[t] |> List.tryPick (fun (n, v) -> if n = tv then Some v else None)
                |_ -> None
            |_, _ -> None

        let findAllRefsFromPos (pos : pos) (filepath : string) (filetext : string) =
            let resource = makeEntityResourceInput filepath filetext
            match resourceManager.ManualProcessResource resource, infoService with
            |Some e, Some info ->
                eprintfn "findRefs %A %A" (fileManager.ConvertPathToLogicalPath filepath) filepath
                match (info |> fst)(pos, e) with
                |Some (_, Some ((t : string), tv)) ->
                    //eprintfn "tv %A %A" t tv
                    let t = t.Split('.').[0]
                    resources.ValidatableEntities() |> List.choose (fun struct(e, l) -> let x = l.Force().Referencedtypes in if x.IsSome then (x.Value.TryFind t) else ((info |> snd) e).TryFind t)
                                   |> List.collect id
                                   |> List.choose (fun (tvk, r) -> if tvk == tv then Some r else None)
                                   |> Some
                |_ -> None
            |_, _ -> None

        let scopesAtPos (pos : pos) (filepath : string) (filetext : string) =
            let resource = makeEntityResourceInput filepath filetext
            match resourceManager.ManualProcessResource resource, infoService with
            |Some e, Some info ->
                // match info.GetInfo(pos, e) with
                match (info |> fst)(pos, e) with
                |Some (ctx, _) when ctx <> { Root = Scope.Any; From = []; Scopes = [] } ->
                    eprintfn "true scopes"
                    Some (ctx)
                |_ ->
                    eprintfn "fallback scopes"
                    getScopeContextAtPos pos lookup.scriptedTriggers lookup.scriptedEffects e.entity |> Option.map (fun s -> {From = s.From; Root = s.Root; Scopes = s.Scopes})
            |Some e, _ -> getScopeContextAtPos pos lookup.scriptedTriggers lookup.scriptedEffects e.entity |> Option.map (fun s -> {From = s.From; Root = s.Root; Scopes = s.Scopes})
            |_ -> None

        let findAllRefsFromPos (pos : pos) (filepath : string) (filetext : string) =
            let resource = makeEntityResourceInput filepath filetext
            match resourceManager.ManualProcessResource resource, infoService with
            |Some e, Some info ->
                eprintfn "findRefs %A %A" (fileManager.ConvertPathToLogicalPath filepath) filepath
                match (info |> fst)(pos, e) with
                |Some (_, Some ((t : string), tv)) ->
                    //eprintfn "tv %A %A" t tv
                    let t = t.Split('.').[0]
                    resources.ValidatableEntities() |> List.choose (fun struct(e, l) -> let x = l.Force().Referencedtypes in if x.IsSome then (x.Value.TryFind t) else ((info |> snd) e).TryFind t)
                                   |> List.collect id
                                   |> List.choose (fun (tvk, r) -> if tvk == tv then Some r else None)
                                   |> Some
                |_ -> None
            |_, _ -> None

        let updateTypeDef =
            let mutable simpleEnums = []
            let mutable complexEnums = []
            let mutable tempTypes = []
            let mutable tempTypeMap = [("", StringSet.Empty(InsensitiveStringComparer()))] |> Map.ofList
            let mutable tempEnumMap = [("", StringSet.Empty(InsensitiveStringComparer()))] |> Map.ofList
            (fun rulesSettings ->
                let timer = new System.Diagnostics.Stopwatch()
                timer.Start()
                match rulesSettings with
                |Some rulesSettings ->
                    let rules, types, enums, complexenums = rulesSettings.ruleFiles |> List.fold (fun (rs, ts, es, ces) (fn, ft) -> let r2, t2, e2, ce2 = parseConfig parseScope allScopes Scope.Any fn ft in rs@r2, ts@t2, es@e2, ces@ce2) ([], [], [], [])
                    lookup.typeDefs <- types
                    let rulesWithMod = rules @ (lookup.coreModifiers |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(specificField c.tag, ValueField (ValueType.Float (-1E+12, 1E+12))), {min = 0; max = 100; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = []}))))
                    lookup.configRules <- rulesWithMod
                    simpleEnums <- enums
                    complexEnums <- complexenums
                    tempTypes <- types
                    eprintfn "Update config rules def: %i" timer.ElapsedMilliseconds; timer.Restart()
                |None -> ()
                let complexEnumDefs = getEnumsFromComplexEnums complexEnums (resources.AllEntities() |> List.map (fun struct(e,_) -> e))
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                let allEnums = simpleEnums @ complexEnumDefs
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                lookup.enumDefs <- allEnums |> Map.ofList
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                tempEnumMap <- lookup.enumDefs |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (s))) |> Map.ofSeq
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                let loc = localisationKeys
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                let files = resources.GetResources() |> List.choose (function |FileResource (_, f) -> Some f.logicalpath |EntityResource (_, f) -> Some f.logicalpath) |> Set.ofList
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                let tempRuleApplicator : RuleApplicator<_,_> = ruleApplicatorCreator(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, Scope.Any, changeScope, defaultContext, STL STLLang.Default)
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                let allentities = resources.AllEntities() |> List.map (fun struct(e,_) -> e)
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                lookup.typeDefInfo <- getTypesFromDefinitions tempRuleApplicator tempTypes allentities
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                tempTypeMap <- lookup.typeDefInfo |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))) |> Map.ofSeq
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                completionService <- Some (completionServiceCreator(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, changeScope, defaultContext, Scope.Any, STL STLLang.Default))
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                ruleApplicator <- Some (ruleApplicatorCreator(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, Scope.Any, changeScope, defaultContext, STL STLLang.Default))
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                infoService <- Some (foldRules(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, ruleApplicator.Value, changeScope, defaultContext, Scope.Any, STL STLLang.Default))
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                validationManager <- ValidationManager({validationSettings with ruleApplicator = ruleApplicator})
            )
        let refreshRuleCaches(rules) =
            updateTypeDef(rules)
            // let timer = new System.Diagnostics.Stopwatch()
            // timer.Start()
            // let loc = allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |>List.collect (fun ls -> ls.GetKeys) |> Set.ofList )
            // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            // let files = resources.GetResources() |> List.choose (function |FileResource (_, f) -> Some f.logicalpath |EntityResource (_, f) -> Some f.logicalpath) |> Set.ofList
            // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()


        do
            eprintfn "Parsing %i files" (fileManager.AllFilesByPath().Length)
            let timer = new System.Diagnostics.Stopwatch()
            timer.Start()
            // let efiles = allFilesByPath |> List.filter (fun (_, f, _) -> not(f.EndsWith(".dds")))
            //             |> List.map (fun (s, f, ft) -> EntityResourceInput {scope = s; filepath = f; filetext = ft; validate = true})
            // let otherfiles = allFilesByPath |> List.filter (fun (_, f, _) -> f.EndsWith(".dds"))
            //                     |> List.map (fun (s, f, _) -> FileResourceInput {scope = s; filepath = f;})
            let files = fileManager.AllFilesByPath()
            let filteredfiles = if settings.validation.validateVanilla then files else files |> List.choose (function |FileResourceInput f -> Some (FileResourceInput f) |EntityResourceInput f -> if f.scope = "vanilla" then Some (EntityResourceInput {f with validate = false}) else Some (EntityResourceInput f))
            resources.UpdateFiles(filteredfiles) |> ignore
            let embeddedFiles =
                settings.embedded.embeddedFiles
                |> List.map (fun (f, ft) -> f.Replace("\\","/"), ft)
                |> List.choose (fun (f, ft) -> if ft = "" then Some (FileResourceInput { scope = "embedded"; filepath = f; logicalpath = (fileManager.ConvertPathToLogicalPath f) }) else None)
            let disableValidate (r, e) : Resource * Entity =
                match r with
                |EntityResource (s, er) -> EntityResource (s, { er with validate = false; scope = "embedded" })
                |x -> x
                , {e with validate = false}
            let cached = settings.embedded.cachedResourceData |> List.map (fun (r, e) -> CachedResourceInput (disableValidate (r, e)))
            let embedded = embeddedFiles @ cached
            if fileManager.ShouldUseEmbedded then resources.UpdateFiles(embedded) |> ignore else ()

            eprintfn "Parsing complete in %i" (timer.Elapsed.Seconds)
            timer.Restart()
            updateScriptedTriggers()
            updateScriptedEffects()
            updateStaticodifiers()
            updateScriptedLoc()
            updateDefinedVariables()
            updateModifiers()
            updateTechnologies()
            updateLocalisation()
            updateTypeDef(settings.rules)
            eprintfn "Initial cache complete in %i" (timer.Elapsed.Seconds)

            // let loc = allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |>List.collect (fun ls -> ls.GetKeys) |> Set.ofList )
            // let files = resources.GetResources() |> List.choose (function |FileResource (_, f) -> Some f.logicalpath |EntityResource (_, f) -> Some f.logicalpath) |> Set.ofList
            // completionService <- Some (CompletionService(lookup.configRules, lookup.typeDefs, lookup.typeDefInfo, lookup.enumDefs, loc, files, lookup.scriptedTriggers, lookup.scriptedEffects))
            // ruleApplicator <- Some (RuleApplicator(lookup.configRules, lookup.typeDefs, lookup.typeDefInfo, lookup.enumDefs, loc, files, lookup.scriptedTriggers, lookup.scriptedEffects))
            // infoService <- Some (FoldRules(lookup.configRules, lookup.typeDefs, lookup.typeDefInfo, lookup.enumDefs, loc, files, lookup.scriptedTriggers, lookup.scriptedEffects, ruleApplicator.Value))
            //resources.ForceRecompute()
        interface IGame<STLComputedData, Scope> with
        //member __.Results = parseResults
            member __.ParserErrors() = parseErrors()
            member __.ValidationErrors() = let (s, d) = (validateAll false (resources.ValidatableEntities())) in s @ d
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
            member __.UpdateFile shallow file text = updateFile shallow file text
            member __.AllEntities() = resources.AllEntities()
            member __.References() = References<STLComputedData, _>(resources, lookup, (localisationAPIs |> List.map snd))
            member __.Complete pos file text = completion pos file text
            member __.ScopesAtPos pos file text = scopesAtPos pos file text |> Option.map (fun sc -> { OutputScopeContext.From = sc.From; Scopes = sc.Scopes; Root = sc.Root})
            member __.GoToType pos file text = getInfoAtPos pos file text
            member __.FindAllRefs pos file text = findAllRefsFromPos pos file text
            member __.ReplaceConfigRules rules = refreshRuleCaches(Some { ruleFiles = rules; validateRules = true; debugRulesOnly = false})
            member __.RefreshCaches() = refreshRuleCaches None
            member __.ForceRecompute() = resources.ForceRecompute()

            //member __.ScriptedTriggers = parseResults |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some p |_ -> None) |> List.map (fun t -> )