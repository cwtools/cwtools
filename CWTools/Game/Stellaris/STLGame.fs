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
open System.Text
open CWTools.Validation.Rules
open CWTools.Games.LanguageFeatures

// type EmbeddedSettings = {
//     triggers : DocEffect list
//     effects : DocEffect list
//     modifiers : Modifier list
//     embeddedFiles : (string * string) list
//     cachedResourceData : (Resource * Entity) list
// }

// type RulesSettings = {
//     ruleFiles : (string * string) list
//     validateRules : bool
//     debugRulesOnly : bool
// }

// type ValidationSettings = {
//     langs : Lang list
//     validateVanilla : bool
//     experimental : bool
// }

// type StellarisSettings = {
//     rootDirectory : string
//     scope : FilesScope
//     modFilter : string option
//     embedded : EmbeddedSettings
//     validation : ValidationSettings
//     rules : RulesSettings option
//     scriptFolders : string list option
// }
module STLGameFunctions =
    let processLocalisationFunction (lookup : Lookup<Scope, Modifier>) =
        let eventtargets = lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst
        processLocalisation eventtargets lookup.scriptedLoc lookup.definedScriptVariables

type StellarisSettings = GameSettings<Modifier, Scope>
//type GameFile = GameFile of result : FileResult

type STLGame (settings : StellarisSettings) =
        let game = GameObject<Scope, Modifier, STLComputedData>
                    (settings, "stellaris", scriptFolders, STLCompute.computeSTLData,
                     (STLLocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
                     STLGameFunctions.processLocalisationFunction,
                     Encoding.UTF8)
        let lookup = game.Lookup
        let resources = game.Resources
        let fileManager = game.FileManager

        let useRules = settings.rules.IsSome
        // let scriptFolders = settings.scriptFolders |> Option.defaultValue scriptFolders

        // let fileManager = FileManager(settings.rootDirectory, settings.modFilter, settings.scope, scriptFolders, "stellaris", Encoding.UTF8)
        let vanillaEffects =
            let se = scopedEffects |> List.map (fun e -> e :> Effect)
            let ve = settings.embedded.effects |> addInnerScope |> List.map (fun e -> e :> Effect)
            se @ ve
        let vanillaTriggers =
            let se = scopedEffects |> List.map (fun e -> e :> Effect)
            let vt = settings.embedded.triggers |> addInnerScope |> List.map (fun e -> e :> Effect)
            se @ vt
        // let mutable completionService : CompletionService<_> option = None
        // let mutable infoService = None

        // let resourceManager = ResourceManager(STLCompute.computeSTLData (fun () -> infoService))
        // let resources = resourceManager.Api

        // let validatableFiles() = resources.ValidatableFiles
        // let lookup = Lookup<Scope, Modifier>()
        // let mutable localisationAPIs : (bool * ILocalisationAPI) list = []
        // let allLocalisation() = localisationAPIs |> List.map snd
        // let validatableLocalisation() = localisationAPIs |> List.choose (fun (validate, api) -> if validate then Some api else None)
        // let mutable localisationKeys = []
        // let mutable taggedLocalisationKeys = []
        // let mutable localisationErrors : (string * Severity * range * int * string * string option) list option = None


        // let getEmbeddedFiles() = settings.embedded.embeddedFiles |> List.map (fun (fn, f) -> "embedded", "embeddedfiles/" + fn, f)


        let updateScriptedTriggers () =
            game.Lookup.scriptedTriggers <- STLLookup.updateScriptedTriggers resources vanillaTriggers


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

        // let updateLocalisation() =
        //     localisationAPIs <-
        //         let locs = resources.GetResources()
        //                     |> List.choose (function |FileWithContentResource (_, e) -> Some e |_ -> None)
        //                     |> List.filter (fun f -> f.overwrite <> Overwritten && f.extension = ".yml")
        //                     |> List.groupBy (fun f -> f.validate)
        //                     |> List.map (fun (b, fs) -> b, fs |> List.map (fun f -> f.filepath, f.filetext) |> STLLocalisationService)
        //         let allLocs = locs |> List.collect (fun (b,l) -> (STL STLLang.Default :: settings.validation.langs)|> List.map (fun lang -> b, l.Api(lang)))
        //         allLocs
        //     localisationKeys <-allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |>List.collect (fun ls -> ls.GetKeys) |> Set.ofList )
        //     taggedLocalisationKeys <- allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.GetKeys) |> List.fold (fun (s : LocKeySet) v -> s.Add v) (LocKeySet.Empty(InsensitiveStringComparer())) )
        //     let validatableEntries = validatableLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList) |> Map.ofList)
        //     let eventtargets = lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst
        //     lookup.proccessedLoc <- validatableEntries |> List.map (fun f -> processLocalisation eventtargets lookup.scriptedLoc lookup.definedScriptVariables f taggedLocalisationKeys)


        let updateDefinedVariables() =
            lookup.definedScriptVariables <- (resources.AllEntities()) |> List.collect (fun struct (_, d) -> d.Force().Setvariables)

        let updateModifiers() =
            lookup.coreModifiers <- addGeneratedModifiers settings.embedded.modifiers (EntitySet (resources.AllEntities()))

        let updateTechnologies() =
            lookup.technologies <- getTechnologies (EntitySet (resources.AllEntities()))

        let parseErrors() =
            resources.GetResources()
                |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None)
                |> List.choose (fun r -> r.result |> function |(Fail (result)) when r.validate -> Some (r.filepath, result.error, result.position)  |_ -> None)


        let validateTechnology (entities : (string * Node) list) =
            let tech = entities |> List.filter (fun (f, _) -> f.Contains("common/technology/"))
            tech
            // tech |> List.iter (fun (f, t) -> eprintfn "%s" f)

        let mutable ruleApplicator : RuleApplicator<Scope> option = None
        let validationSettings = {
            validators = [validateVariables, "var"; valTechnology, "tech"; validateTechnologies, "tech2"; valButtonEffects, "but"; valSprites, "sprite"; valVariables, "var2"; valEventCalls, "event";
                                validateAmbientGraphics, "ambient"; validateShipDesigns, "designs"; validateMixedBlocks, "mixed"; validateSolarSystemInitializers, "solar"; validateAnomaly210, "anom";
                                validateIfElse210, "ifelse"; validateIfElse, "ifelse2"; validatePlanetKillers, "pk"; validateRedundantAND, "AND"; valMegastructureGraphics, "megastructure";
                                valPlanetClassGraphics, "pcg"; validateDeprecatedSetName, "setname"; validateShips, "ships"; validateEvents, "eventsSimple"]
            experimentalValidators = [valSectionGraphics, "sections"; valComponentGraphics, "component"]
            heavyExperimentalValidators = [getEventChains, "event chains"]
            experimental = settings.validation.experimental
            fileValidators = [valSpriteFiles, "sprites"; valMeshFiles, "mesh"; valAssetFiles, "asset"; valComponentIcons, "compicon"]
            resources = resources
            lookup = lookup
            lookupValidators = [validateModifierBlocks, "mod blocks"; valAllModifiers, "mods"]
            ruleApplicator = ruleApplicator
            foldRules = game.infoService
            useRules = useRules
            debugRulesOnly = settings.rules |> Option.map (fun o -> o.debugRulesOnly) |> Option.defaultValue false
            localisationKeys = (fun () -> game.LocalisationManager.localisationKeys)
            localisationValidators = [valEventLocs; valTechLocs; valCompSetLocs; valCompTempLocs; valBuildingLocs; valTraditionLocCats; valArmiesLoc;
                                 valArmyAttachmentLocs; valDiploPhrases; valShipLoc; valFactionDemands; valSpeciesRightsLocs;
                                 valMapsLocs; valMegastructureLocs; valModifiers; valModules; valTraits; valGoverments; valPersonalities;
                                 valEthics; valPlanetClasses; valEdicts; valPolicies; valSectionTemplates; valSpeciesNames; valStratRes;
                                 valAmbient; valDeposits; valWarGoals; valEffectLocs; valTriggerLocs; valBuildingTags; valOpinionModifiers;
                                 valScriptedTriggers; valSpecialProjects; valStarbaseType; valTileBlockers; valAnomalies]

        }

        let mutable validationManager = ValidationManager(validationSettings)
        let validateAll shallow newEntities = validationManager.Validate(shallow, newEntities)
        let localisationCheck (entities : struct (Entity * Lazy<STLComputedData>) list) = validationManager.ValidateLocalisation(entities)

        let globalLocalisation () =
            let locfiles =  resources.GetResources()
                            |> List.choose (function |FileWithContentResource (_, e) -> Some e |_ -> None)
                            |> List.filter (fun f -> f.overwrite <> Overwritten && f.extension = ".yml" && f.validate)
                            |> List.map (fun f -> f.filepath)
            let locFileValidation = validateLocalisationFiles locfiles
            let globalTypeLoc = validationManager.ValidateGlobalLocalisation()
            lookup.proccessedLoc |> validateProcessedLocalisation game.LocalisationManager.taggedLocalisationKeys <&&> locFileValidation <&&> globalTypeLoc |> (function |Invalid es -> es |_ -> [])

        let mutable errorCache = Map.empty
        let updateFile (shallow : bool) filepath (filetext : string option) =
            eprintfn "%s" filepath
            let timer = new System.Diagnostics.Stopwatch()
            timer.Start()
            let res =
                match filepath with
                |x when x.EndsWith (".yml") ->
                    let file = filetext |> Option.defaultWith (fun () -> File.ReadAllText filepath)
                    let resource = makeFileWithContentResourceInput fileManager filepath file
                    resources.UpdateFile(resource) |> ignore
                    game.LocalisationManager.UpdateAllLocalisation()
                    let les = (localisationCheck (resources.ValidatableEntities())) @ globalLocalisation()
                    game.LocalisationManager.localisationErrors <- Some les
                    globalLocalisation()
                | _ ->
                    let file = filetext |> Option.defaultWith (fun () -> File.ReadAllText filepath)
                    let resource = makeEntityResourceInput fileManager filepath file
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

        let addTriggerDocsScopes (rules : RootRule<_> list) =
            let addRequiredScopesE s (o : ConfigParser.Options<_>) =
                let newScopes =
                    match o.requiredScopes with
                    |[] ->
                        lookup.scriptedEffectsMap.TryFind(s)
                            |> Option.map(fun se -> se.Scopes)
                            |> Option.defaultValue []
                    |x -> x
                { o with requiredScopes = newScopes}
            let addRequiredScopesT s (o : ConfigParser.Options<_>) =
                let newScopes =
                    match o.requiredScopes with
                    |[] ->
                        lookup.scriptedTriggersMap.TryFind(s)
                            |> Option.map(fun se -> se.Scopes)
                            |> Option.defaultValue []
                    |x -> x
                { o with requiredScopes = newScopes}
            rules |> List.map (
                    function
                    |AliasRule ("effect", (LeafRule(ValueField(ValueType.Specific s),r), o)) ->
                        AliasRule ("effect", (LeafRule(ValueField(ValueType.Specific s),r), addRequiredScopesE s o))
                    |AliasRule ("trigger", (LeafRule(ValueField(ValueType.Specific s),r), o)) ->
                        AliasRule ("trigger", (LeafRule(ValueField(ValueType.Specific s),r), addRequiredScopesT s o))
                    |AliasRule ("effect", (NodeRule(ValueField(ValueType.Specific s),r), o)) ->
                        AliasRule ("effect", (NodeRule(ValueField(ValueType.Specific s),r), addRequiredScopesE s o))
                    |AliasRule ("trigger", (NodeRule(ValueField(ValueType.Specific s),r), o)) ->
                        AliasRule ("trigger", (NodeRule(ValueField(ValueType.Specific s),r), addRequiredScopesT s o))
                    |AliasRule ("effect", (LeafValueRule(ValueField(ValueType.Specific s)), o)) ->
                        AliasRule ("effect", (LeafValueRule(ValueField(ValueType.Specific s)), addRequiredScopesE s o))
                    |AliasRule ("trigger", (LeafValueRule(ValueField(ValueType.Specific s)), o)) ->
                        AliasRule ("trigger", (LeafValueRule(ValueField(ValueType.Specific s)), addRequiredScopesT s o))
                    |x -> x)
        let addModifiersWithScopes () =
            let modifierOptions (modifier : Modifier) =
                let requiredScopes =
                    modifier.categories |> List.choose (fun c -> modifierCategoryToScopesMap.TryFind c)
                                        |> List.map Set.ofList
                                        |> (fun l -> if List.isEmpty l then [] else l |> List.reduce (Set.intersect) |> Set.toList)
                {min = 0; max = 100; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = requiredScopes}
            lookup.coreModifiers
                |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(specificField c.tag, ValueField (ValueType.Float (-1E+12, 1E+12))), modifierOptions c)))

        let updateTypeDef =
            let mutable simpleEnums = []
            let mutable complexEnums = []
            let mutable tempTypes = []
            let mutable tempValues = Map.empty
            let mutable tempTypeMap = [("", StringSet.Empty(InsensitiveStringComparer()))] |> Map.ofList
            let mutable tempEnumMap = [("", StringSet.Empty(InsensitiveStringComparer()))] |> Map.ofList
            (fun rulesSettings ->
                let timer = new System.Diagnostics.Stopwatch()
                timer.Start()
                match rulesSettings with
                |Some rulesSettings ->
                    let rules, types, enums, complexenums, values = rulesSettings.ruleFiles |> List.fold (fun (rs, ts, es, ces, vs) (fn, ft) -> let r2, t2, e2, ce2, v2 = parseConfig parseScope allScopes Scope.Any fn ft in rs@r2, ts@t2, es@e2, ces@ce2, vs@v2) ([], [], [], [], [])
                    lookup.typeDefs <- types
                    let rulesWithMod = rules @ addModifiersWithScopes()
                    let rulesWithEmbeddedScopes = addTriggerDocsScopes rulesWithMod
                    lookup.configRules <- rulesWithEmbeddedScopes
                    simpleEnums <- enums
                    complexEnums <- complexenums
                    tempTypes <- types
                    tempValues <- values |> List.map (fun (s, sl) -> s, (sl |> List.map (fun s2 -> s2, range.Zero))) |> Map.ofList
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
                let loc = game.LocalisationManager.localisationKeys
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                let files = resources.GetResources() |> List.choose (function |FileResource (_, f) -> Some f.logicalpath |EntityResource (_, f) -> Some f.logicalpath |FileWithContentResource (_, f) -> Some f.logicalpath) |> Set.ofList
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                let tempRuleApplicator : RuleApplicator<_> = RuleApplicator<Scope>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, Collections.Map.empty, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, Scope.Any, changeScope, defaultContext, STL STLLang.Default)
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                let allentities = resources.AllEntities() |> List.map (fun struct(e,_) -> e)
                eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                lookup.typeDefInfo <- getTypesFromDefinitions tempRuleApplicator tempTypes allentities
                eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                let tempFoldRules = (FoldRules<Scope>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, Collections.Map.empty, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, tempRuleApplicator, changeScope, defaultContext, Scope.Any, STL STLLang.Default))

                let results = resources.AllEntities() |> PSeq.map (fun struct(e, l) -> (l.Force().Definedvariables |> (Option.defaultWith (fun () -> tempFoldRules.GetDefinedVariables e))))
                                |> Seq.fold (fun m map -> Map.toList map |>  List.fold (fun m2 (n,k) -> if Map.containsKey n m2 then Map.add n (k@m2.[n]) m2 else Map.add n k m2) m) tempValues

                lookup.varDefInfo <- results
                eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                let varMap = lookup.varDefInfo |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (List.map fst s))) |> Map.ofSeq
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                tempTypeMap <- lookup.typeDefInfo |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))) |> Map.ofSeq
                eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                game.completionService <- Some (CompletionService(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, changeScope, defaultContext, Scope.Any, oneToOneScopesNames, STL STLLang.Default))
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                ruleApplicator <- Some (RuleApplicator<Scope>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, Scope.Any, changeScope, defaultContext, STL STLLang.Default))
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                game.infoService <- Some (FoldRules<Scope>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, ruleApplicator.Value, changeScope, defaultContext, Scope.Any, STL STLLang.Default))
                // eprintfn "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
                validationManager <- ValidationManager({validationSettings with ruleApplicator = ruleApplicator; foldRules = game.infoService})
            )
        let refreshRuleCaches(rules) =
            updateModifiers()
            updateTypeDef(rules)

        let scopesAtPosSTL pos file text =
            let resource = makeEntityResourceInput fileManager file text
            match game.ResourceManager.ManualProcessResource resource, game.infoService with
            |Some e, Some info ->
                // match info.GetInfo(pos, e) with
                match (info.GetInfo)(pos, e) with
                |Some (ctx, _) when ctx <> { Root = Scope.Any; From = []; Scopes = [] } ->
                    eprintfn "true scopes"
                    Some (ctx)
                |_ ->
                    eprintfn "fallback scopes"
                    getScopeContextAtPos pos lookup.scriptedTriggers lookup.scriptedEffects e.entity |> Option.map (fun s -> {From = s.From; Root = s.Root; Scopes = s.Scopes})
            |Some e, _ -> getScopeContextAtPos pos lookup.scriptedTriggers lookup.scriptedEffects e.entity |> Option.map (fun s -> {From = s.From; Root = s.Root; Scopes = s.Scopes})
            |_ -> None

        do
            eprintfn "Parsing %i files" (fileManager.AllFilesByPath().Length)
            let timer = new System.Diagnostics.Stopwatch()
            timer.Start()
            // let efiles = allFilesByPath |> List.filter (fun (_, f, _) -> not(f.EndsWith(".dds")))
            //             |> List.map (fun (s, f, ft) -> EntityResourceInput {scope = s; filepath = f; filetext = ft; validate = true})
            // let otherfiles = allFilesByPath |> List.filter (fun (_, f, _) -> f.EndsWith(".dds"))
            //                     |> List.map (fun (s, f, _) -> FileResourceInput {scope = s; filepath = f;})
            let files = fileManager.AllFilesByPath()
            let filteredfiles =
                if settings.validation.validateVanilla
                then files
                else files |> List.choose (function
                    |FileResourceInput f -> Some (FileResourceInput f)
                    |FileWithContentResourceInput f -> Some (FileWithContentResourceInput f)
                    |EntityResourceInput f -> (if f.scope = "vanilla" then Some (EntityResourceInput {f with validate = false}) else Some (EntityResourceInput f)) |_ -> None)
            resources.UpdateFiles(filteredfiles) |> ignore
            let embeddedFiles =
                settings.embedded.embeddedFiles
                |> List.map (fun (f, ft) -> f.Replace("\\","/"), ft)
                |> List.choose (fun (f, ft) ->
                    if ft = ""
                    then Some (FileResourceInput { scope = "embedded"; filepath = f; logicalpath = (fileManager.ConvertPathToLogicalPath f) })
                    else Some (FileWithContentResourceInput { scope = "embedded"; filepath = f; logicalpath = (fileManager.ConvertPathToLogicalPath f); filetext = ft; validate = false}))
            let disableValidate (r, e) : Resource * Entity =
                match r with
                |EntityResource (s, er) -> EntityResource (s, { er with validate = false; scope = "embedded" })
                |x -> x
                , {e with validate = false}
            let cached = settings.embedded.cachedResourceData |> List.map (fun (r, e) -> CachedResourceInput (disableValidate (r, e)))
            let embedded = embeddedFiles @ cached
            if fileManager.ShouldUseEmbedded then resources.UpdateFiles(embedded) |> ignore else ()

            let log = true
            eprintfn "Parsing complete in %i" (timer.Elapsed.Seconds)
            timer.Restart()
            updateScriptedTriggers()
            updateScriptedEffects()
            updateStaticodifiers()
            updateScriptedLoc()
            updateDefinedVariables()
            updateModifiers()
            updateTechnologies()
            if log then eprintfn "time %i" (timer.ElapsedMilliseconds); timer.Restart() else ()
            game.LocalisationManager.UpdateAllLocalisation()
            if log then eprintfn "time %i" (timer.ElapsedMilliseconds); timer.Restart() else ()
            updateTypeDef(settings.rules)
            game.LocalisationManager.UpdateAllLocalisation()
            eprintfn "Initial cache complete in %i" (timer.Elapsed.Seconds)

            // let loc = allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |>List.collect (fun ls -> ls.GetKeys) |> Set.ofList )
            // let files = resources.GetResources() |> List.choose (function |FileResource (_, f) -> Some f.logicalpath |EntityResource (_, f) -> Some f.logicalpath) |> Set.ofList
            // completionService <- Some (CompletionService(lookup.configRules, lookup.typeDefs, lookup.typeDefInfo, lookup.enumDefs, loc, files, lookup.scriptedTriggers, lookup.scriptedEffects))
            // ruleApplicator <- Some (RuleApplicator(lookup.configRules, lookup.typeDefs, lookup.typeDefInfo, lookup.enumDefs, loc, files, lookup.scriptedTriggers, lookup.scriptedEffects))
            // infoService <- Some (FoldRules(lookup.configRules, lookup.typeDefs, lookup.typeDefInfo, lookup.enumDefs, loc, files, lookup.scriptedTriggers, lookup.scriptedEffects, ruleApplicator.Value))
            //resources.ForceRecompute()
        interface IGame<STLComputedData, Scope, Modifier> with
        //member __.Results = parseResults
            member __.ParserErrors() = parseErrors()
            member __.ValidationErrors() = let (s, d) = (validateAll false (resources.ValidatableEntities())) in s @ d
            member __.LocalisationErrors(force : bool) =
                let generate =
                    let les = (localisationCheck (resources.ValidatableEntities())) @ globalLocalisation()
                    game.LocalisationManager.localisationErrors <- Some les
                    les
                match game.LocalisationManager.localisationErrors with
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
            member __.References() = References<_, _, Modifier>(resources, lookup, (game.LocalisationManager.localisationAPIs |> List.map snd))
            member __.Complete pos file text = completion fileManager game.completionService game.ResourceManager pos file text
            member __.ScopesAtPos pos file text =
                scopesAtPosSTL pos file text
                |> Option.map (fun sc -> { OutputScopeContext.From = sc.From; Scopes = sc.Scopes; Root = sc.Root})
            member __.GoToType pos file text = getInfoAtPos fileManager game.ResourceManager game.infoService lookup pos file text
            member __.FindAllRefs pos file text = findAllRefsFromPos fileManager game.ResourceManager game.infoService pos file text
            member __.ReplaceConfigRules rules = refreshRuleCaches(Some { ruleFiles = rules; validateRules = true; debugRulesOnly = false})
            member __.RefreshCaches() = refreshRuleCaches None
            member __.ForceRecompute() = resources.ForceRecompute()

            //member __.ScriptedTriggers = parseResults |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some p |_ -> None) |> List.map (fun t -> )