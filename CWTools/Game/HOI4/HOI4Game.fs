namespace CWTools.Games.HOI4
open CWTools.Common.HOI4Constants
open CWTools.Localisation
open CWTools.Validation.ValidationCore
open CWTools.Games.Files
open CWTools.Games
open CWTools.Common
open FSharp.Collections.ParallelSeq
open CWTools.Localisation.HOI4Localisation
open CWTools.Utilities.Utils
open CWTools.Utilities.Position
open CWTools.Utilities
open System.IO
open CWTools.Validation.Common.CommonValidation
// open CWTools.Validation.Rules
open CWTools.Parser.ConfigParser
open CWTools.Common.HOI4Constants
//open CWTools.Validation.HOI4.HOI4Rules
open CWTools.Validation.Rules
open CWTools.Process.HOI4Scopes
open CWTools.Common
open CWTools.Process.Scopes
open CWTools.Validation.HOI4
open System.Text
open CWTools.Games.LanguageFeatures
open System
open CWTools.Validation.HOI4.HOI4LocalisationString
open CWTools.Games

module HOI4GameFunctions =
    type GameObject = GameObject<Scope, Modifier, HOI4ComputedData>
    let processLocalisationFunction (localisationCommands : ((string * Scope list) list)) (lookup : Lookup<Scope, Modifier>) =
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.typeDefInfo.TryFind "province_id" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.enumDefs.TryFind "country_tags" |> Option.map snd |> Option.defaultValue [])
        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.varDefInfo.TryFind "saved_name" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.varDefInfo.TryFind "exiled_ruler" |> Option.defaultValue [] |> List.map fst)
        processLocalisation localisationCommands eventtargets lookup.scriptedLoc definedvars
    let validateLocalisationCommandFunction (localisationCommands : ((string * Scope list) list)) (lookup : Lookup<Scope, Modifier>) =
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.typeDefInfo.TryFind "province_id" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.enumDefs.TryFind "country_tags" |> Option.map snd |> Option.defaultValue [])
        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.varDefInfo.TryFind "saved_name" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.varDefInfo.TryFind "exiled_ruler" |> Option.defaultValue [] |> List.map fst)
        validateLocalisationCommand localisationCommands eventtargets lookup.scriptedLoc definedvars
    let globalLocalisation (game : GameObject) =
        // let locfiles =  resources.GetResources()
        //                 |> List.choose (function |FileWithContentResource (_, e) -> Some e |_ -> None)
        //                 |> List.filter (fun f -> f.overwrite <> Overwritten && f.extension = ".yml" && f.validate)
        //                 |> List.map (fun f -> f.filepath)
        // let locFileValidation = validateLocalisationFiles locfiles
        let globalTypeLoc = game.ValidationManager.ValidateGlobalLocalisation()
        // lookup.proccessedLoc |> validateProcessedLocalisation taggedLocalisationKeys <&&>
        // locFileValidation <&&>
        globalTypeLoc |> (function |Invalid es -> es |_ -> [])

    let updateModifiers (game : GameObject) =
        game.Lookup.coreModifiers <- game.Settings.embedded.modifiers

    let updateProvinces (game : GameObject) =
        let provinceFile =
            game.Resources.GetResources()
            |> List.choose (function |FileWithContentResource (_, e) -> Some e |_ -> None)
            |> List.tryFind (fun f -> f.overwrite <> Overwritten && Path.GetFileName(f.filepath) = "definition.csv")
        match provinceFile with
        |None -> ()
        |Some pf ->
            let lines = pf.filetext.Split(([|"\r\n"; "\r"; "\n"|]), StringSplitOptions.None)
            let provinces = lines |> Array.choose (fun l -> l.Split([|';'|], 2, StringSplitOptions.RemoveEmptyEntries) |> Array.tryHead) |> List.ofArray
            log (sprintf "%A" provinces)
            game.Lookup.HOI4provinces <- provinces

    let updateScriptedEffects(rules :RootRule<Scope> list) (states : string list) (countries : string list) =
        let effects =
            rules |> List.choose (function |AliasRule("effect", r) -> Some r |_ -> None)
        let ruleToEffect(r,o) =
            let name =
                match r with
                |LeafRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n
                |NodeRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n
                |_ -> ""
            DocEffect(name, o.requiredScopes, EffectType.Effect, o.description |> Option.defaultValue "", "")
        let stateEffects =  states |> List.map (fun p -> ScopedEffect(p, allScopes, Scope.State, EffectType.Both, defaultDesc, "", true));
        let countryEffects =  countries |> List.map (fun p -> ScopedEffect(p, allScopes, Scope.Country, EffectType.Both, defaultDesc, "", true));
        (effects |> List.map ruleToEffect  |> List.map (fun e -> e :> Effect)) @ (scopedEffects |> List.map (fun e -> e :> Effect))
        @ (stateEffects |> List.map (fun e -> e :> Effect)) @ (countryEffects |> List.map (fun e -> e :> Effect))

    let updateScriptedTriggers(rules :RootRule<Scope> list) states countries =
        let effects =
            rules |> List.choose (function |AliasRule("trigger", r) -> Some r |_ -> None)
        let ruleToTrigger(r,o) =
            let name =
                match r with
                |LeafRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n
                |NodeRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n
                |_ -> ""
            DocEffect(name, o.requiredScopes, EffectType.Trigger, o.description |> Option.defaultValue "", "")
        let stateEffects =  states |> List.map (fun p -> ScopedEffect(p, allScopes, Scope.State, EffectType.Both, defaultDesc, "", true));
        let countryEffects =  countries |> List.map (fun p -> ScopedEffect(p, allScopes, Scope.Country, EffectType.Both, defaultDesc, "", true));
        (effects |> List.map ruleToTrigger |> List.map (fun e -> e :> Effect)) @ (scopedEffects |> List.map (fun e -> e :> Effect))
        @ (stateEffects |> List.map (fun e -> e :> Effect)) @ (countryEffects |> List.map (fun e -> e :> Effect))
    let addModifiersWithScopes (game : GameObject) =
        let modifierOptions (modifier : Modifier) =
            let requiredScopes =
                modifier.categories |> List.choose (fun c -> modifierCategoryToScopesMap.TryFind c)
                                    |> List.map Set.ofList
                                    |> (fun l -> if List.isEmpty l then [] else l |> List.reduce (Set.intersect) |> Set.toList)
            {min = 0; max = 100; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = requiredScopes}
        game.Lookup.coreModifiers
            |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(specificField c.tag, ValueField (ValueType.Float (-1E+12, 1E+12))), modifierOptions c)))

    let updateTypeDef =
        let mutable simpleEnums = []
        let mutable complexEnums = []
        let mutable tempTypes = []
        let mutable tempValues = Map.empty
        let mutable tempTypeMap = [("", StringSet.Empty(InsensitiveStringComparer()))] |> Map.ofList
        let mutable tempEnumMap = [("", ("", StringSet.Empty(InsensitiveStringComparer())))] |> Map.ofList
        let mutable rulesDataGenerated = false
        (fun  (game : GameObject) rulesSettings ->
            let lookup = game.Lookup
            let resources = game.Resources
            let timer = new System.Diagnostics.Stopwatch()
            timer.Start()
            match rulesSettings with
            |Some rulesSettings ->
                let rules, types, enums, complexenums, values = rulesSettings.ruleFiles |> List.fold (fun (rs, ts, es, ces, vs) (fn, ft) -> let r2, t2, e2, ce2, v2 = parseConfig parseScope allScopes Scope.Any fn ft in rs@r2, ts@t2, es@e2, ces@ce2, vs@v2) ([], [], [], [], [])
                lookup.typeDefs <- types
                let rulesWithMod = rules @ addModifiersWithScopes game
                lookup.configRules <- rulesWithMod
                simpleEnums <- enums
                complexEnums <- complexenums
                tempTypes <- types
                tempValues <- values |> List.map (fun (s, sl) -> s, (sl |> List.map (fun s2 -> s2, range.Zero))) |> Map.ofList
                rulesDataGenerated <- false
                log (sprintf "Update config rules def: %i" timer.ElapsedMilliseconds); timer.Restart()
            |None -> ()
            let complexEnumDefs = CWTools.Validation.Rules.getEnumsFromComplexEnums complexEnums (resources.AllEntities() |> List.map (fun struct(e,_) -> e))
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let allEnums = simpleEnums @ complexEnumDefs @ [{ key = "provinces"; description = "provinces"; values = lookup.HOI4provinces}]
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            lookup.enumDefs <- allEnums |> List.map (fun e -> (e.key, (e.description, e.values))) |> Map.ofList
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            tempEnumMap <- lookup.enumDefs |> Map.toSeq |> PSeq.map (fun (k, (d, s)) -> k, (d, StringSet.Create(InsensitiveStringComparer(), (s)))) |> Map.ofSeq
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let loc = game.LocalisationManager.localisationKeys
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let files = resources.GetFileNames() |> Set.ofList
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let tempRuleApplicator = RuleApplicator<Scope>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, Collections.Map.empty, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, Scope.Any, changeScope, defaultContext, HOI4 HOI4Lang.Default)
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let allentities = resources.AllEntities() |> List.map (fun struct(e,_) -> e)
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let typeDefInfo = getTypesFromDefinitions tempRuleApplicator tempTypes allentities
            lookup.typeDefInfoForValidation <- typeDefInfo |> Map.map (fun _ v -> v |> List.choose (fun (v, t, r) -> if v then Some (t, r) else None))
            lookup.typeDefInfo <- typeDefInfo |> Map.map (fun _ v -> v |> List.map (fun (_, t, r) -> (t, r)))
            tempTypeMap <- lookup.typeDefInfo |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))) |> Map.ofSeq

            let states = lookup.typeDefInfo.TryFind "state"
                            |> Option.map (fun sl -> sl |> List.map fst)
                            |> Option.defaultValue []
            let countries = lookup.enumDefs.TryFind "country_tag"
                            |> Option.map snd
                            |> Option.defaultValue []
            lookup.scriptedEffects <- updateScriptedEffects lookup.configRules states countries
            lookup.scriptedTriggers <- updateScriptedTriggers lookup.configRules states countries

            let tempFoldRules = (FoldRules<Scope>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, Collections.Map.empty, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, tempRuleApplicator, changeScope, defaultContext, Scope.Any, STL STLLang.Default))
            game.InfoService <- Some tempFoldRules
            if not rulesDataGenerated then resources.ForceRulesDataGenerate(); rulesDataGenerated <- true else ()

            let results = resources.AllEntities() |> PSeq.map (fun struct(e, l) -> (l.Force().Definedvariables |> (Option.defaultWith (fun () -> tempFoldRules.GetDefinedVariables e))))
                            |> Seq.fold (fun m map -> Map.toList map |>  List.fold (fun m2 (n,k) -> if Map.containsKey n m2 then Map.add n (k@m2.[n]) m2 else Map.add n k m2) m) tempValues

            lookup.varDefInfo <- results
            let varMap = lookup.varDefInfo |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (List.map fst s))) |> Map.ofSeq

            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            game.completionService <- Some (CompletionService(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, [], changeScope, defaultContext, Scope.Any, oneToOneScopesNames, HOI4 HOI4Lang.Default))
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            game.RuleApplicator <- Some (RuleApplicator<Scope>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, Scope.Any, changeScope, defaultContext, HOI4 HOI4Lang.Default))
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            game.InfoService <- Some (FoldRules<Scope>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, game.RuleApplicator.Value, changeScope, defaultContext, Scope.Any, HOI4 HOI4Lang.Default))
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            game.RefreshValidationManager()
            // validationManager <- ValidationManager({validationSettings with ruleApplicator = game.ruleApplicator; foldRules = game.infoService })
        )
    let afterInit (game : GameObject) =
        // updateScriptedTriggers()
        // updateScriptedEffects()
        // updateStaticodifiers()
        // updateScriptedLoc()
        // updateDefinedVariables()
        updateModifiers(game)
        updateProvinces(game)
        // updateTechnologies()
        game.LocalisationManager.UpdateAllLocalisation()
        updateTypeDef game game.Settings.rules
        game.LocalisationManager.UpdateAllLocalisation()

type HOI4Settings = GameSettings<Modifier, Scope>
open HOI4GameFunctions
type HOI4Game(settings : HOI4Settings) =
    let validationSettings = {
        validators = [ validateMixedBlocks, "mixed"; ]
        experimentalValidators = []
        heavyExperimentalValidators = []
        experimental = settings.validation.experimental
        fileValidators = []
        lookupValidators = []
        useRules = true
        debugRulesOnly = false
        localisationValidators = []
    }
    let settings = { settings with embedded = { settings.embedded with localisationCommands = settings.embedded.localisationCommands |> (fun l -> if l.Length = 0 then locCommands else l )}}

    let game = GameObject<Scope, Modifier, HOI4ComputedData>.CreateGame
                (settings, "hearts of iron iv", scriptFolders, HOI4Compute.computeHOI4Data,
                HOI4Compute.computeHOI4DataUpdate,
                 (HOI4LocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
                 HOI4GameFunctions.processLocalisationFunction (settings.embedded.localisationCommands),
                 HOI4GameFunctions.validateLocalisationCommandFunction (settings.embedded.localisationCommands),
                 defaultContext,
                 noneContext,
                 Encoding.UTF8,
                 Encoding.GetEncoding(1252),
                 validationSettings,
                 globalLocalisation,
                 (fun _ _ -> ()))
                 afterInit
    let lookup = game.Lookup
    let resources = game.Resources
    let fileManager = game.FileManager

    let getEmbeddedFiles() = settings.embedded.embeddedFiles |> List.map (fun (fn, f) -> "embedded", "embeddedfiles/" + fn, f)

    // let lookup = Lookup<Scope, Modifier>()


    // let mutable validationManager = ValidationManager(validationSettings)
    // let validateAll shallow newEntities = validationManager.Validate(shallow, newEntities)

    // let localisationCheck (entities : struct (Entity * Lazy<HOI4ComputedData>) list) = validationManager.ValidateLocalisation(entities)


    let refreshRuleCaches(rules) =
        updateTypeDef(rules)



    let parseErrors() =
        resources.GetResources()
            |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None)
            |> List.choose (fun r -> r.result |> function |(Fail (result)) when r.validate -> Some (r.filepath, result.error, result.position)  |_ -> None)
    // let mutable errorCache = Map.empty

    // let updateFile (shallow : bool) filepath (filetext : string option) =
    //     log "%s" filepath
    //     let timer = new System.Diagnostics.Stopwatch()
    //     timer.Start()
    //     let res =
    //         match filepath with
    //         |x when x.EndsWith (".yml") ->
    //             let file = filetext |> Option.defaultWith (fun () -> File.ReadAllText filepath)
    //             let resource = makeFileWithContentResourceInput fileManager filepath file
    //             resources.UpdateFile(resource) |> ignore
    //             game.LocalisationManager.UpdateAllLocalisation()
    //             let les = (localisationCheck (resources.ValidatableEntities())) @ globalLocalisation()
    //             game.LocalisationManager.localisationErrors <- Some les
    //             globalLocalisation()
    //         | _ ->
    //             let filepath = Path.GetFullPath(filepath)
    //             let file = filetext |> Option.defaultWith (fun () -> File.ReadAllText(filepath, Encoding.GetEncoding(1252)))
    //             let rootedpath = filepath.Substring(filepath.IndexOf(fileManager.ScopeDirectory) + (fileManager.ScopeDirectory.Length))
    //             let logicalpath = fileManager.ConvertPathToLogicalPath rootedpath
    //             let resource = makeEntityResourceInput fileManager filepath file
    //             //log "%s %s" logicalpath filepath
    //             let newEntities = resources.UpdateFile (resource) |> List.map snd
    //             // match filepath with
    //             // |x when x.Contains("scripted_triggers") -> updateScriptedTriggers()
    //             // |x when x.Contains("scripted_effects") -> updateScriptedEffects()
    //             // |x when x.Contains("static_modifiers") -> updateStaticodifiers()
    //             // |_ -> ()
    //             // updateDefinedVariables()
    //             // validateAll true newEntities @ localisationCheck newEntities
    //             match shallow with
    //                 |true ->
    //                     let (shallowres, _) = (validateAll shallow newEntities)
    //                     let shallowres = shallowres @ (localisationCheck newEntities)
    //                     let deep = errorCache |> Map.tryFind filepath |> Option.defaultValue []
    //                     shallowres @ deep
    //                 |false ->
    //                     let (shallowres, deepres) = (validateAll shallow newEntities)
    //                     let shallowres = shallowres @ (localisationCheck newEntities)
    //                     errorCache <- errorCache.Add(filepath, deepres)
    //                     shallowres @ deepres

        // log "Update Time: %i" timer.ElapsedMilliseconds
        // res

    // do
    //     log "Parsing %i files" (fileManager.AllFilesByPath().Length)
    //     let files = fileManager.AllFilesByPath()
    //     let filteredfiles = if settings.validation.validateVanilla then files else files |> List.choose (function |FileResourceInput f -> Some (FileResourceInput f) |EntityResourceInput f -> (if f.scope = "vanilla" then Some (EntityResourceInput {f with validate = false}) else Some (EntityResourceInput f) )|_ -> None)
    //     resources.UpdateFiles(filteredfiles) |> ignore
    //     let embeddedFiles =
    //         settings.embedded.embeddedFiles
    //         |> List.map (fun (f, ft) -> f.Replace("\\","/"), ft)
    //         |> List.choose (fun (f, ft) -> if ft = "" then Some (FileResourceInput { scope = "embedded"; filepath = f; logicalpath = (fileManager.ConvertPathToLogicalPath f) }) else None)
    //     let disableValidate (r, e) : Resource * Entity =
    //         match r with
    //         |EntityResource (s, er) -> EntityResource (s, { er with validate = false; scope = "embedded" })
    //         |x -> x
    //         , {e with validate = false}

    //     // let embeddedFiles = settings.embedded.embeddedFiles |> List.map (fun (f, ft) -> if ft = "" then FileResourceInput { scope = "embedded"; filepath = f; logicalpath = (fileManager.ConvertPathToLogicalPath f) } else EntityResourceInput {scope = "embedded"; filepath = f; logicalpath = (fileManager.ConvertPathToLogicalPath f); filetext = ft; validate = false})
    //     let cached = settings.embedded.cachedResourceData |> List.map (fun (r, e) -> CachedResourceInput (disableValidate (r, e)))
    //     let embedded = embeddedFiles @ cached
    //     if fileManager.ShouldUseEmbedded then resources.UpdateFiles(embedded) |> ignore else ()

    interface IGame<HOI4ComputedData, Scope, Modifier> with
    //member __.Results = parseResults
        member __.ParserErrors() = parseErrors()
        member __.ValidationErrors() = let (s, d) = (game.ValidationManager.Validate(false, (resources.ValidatableEntities()))) in s @ d
        member __.LocalisationErrors(force : bool, forceGlobal : bool) =
            let genGlobal() =
                let ges = (globalLocalisation(game))
                game.LocalisationManager.globalLocalisationErrors <- Some ges
                ges
            let genAll() =
                let les = (game.ValidationManager.ValidateLocalisation (resources.ValidatableEntities()))
                game.LocalisationManager.localisationErrors <- Some les
                les
            match game.LocalisationManager.localisationErrors, game.LocalisationManager.globalLocalisationErrors with
            |Some les, Some ges -> (if force then genAll() else les) @ (if forceGlobal then genGlobal() else ges)
            |None, Some ges -> (genAll()) @ (if forceGlobal then genGlobal() else ges)
            |Some les, None -> (if force then genAll() else les) @ (genGlobal())
            |None, None -> (genAll()) @ (genGlobal())

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
        member __.StaticModifiers() = [] //lookup.staticModifiers
        member __.UpdateFile shallow file text = game.UpdateFile shallow file text
        member __.AllEntities() = resources.AllEntities()
        member __.References() = References<HOI4ComputedData, Scope, _>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))
        member __.Complete pos file text = completion fileManager game.completionService game.ResourceManager pos file text
        member __.ScopesAtPos pos file text = scopesAtPos fileManager game.ResourceManager game.InfoService Scope.Any pos file text |> Option.map (fun sc -> { OutputScopeContext.From = sc.From; Scopes = sc.Scopes; Root = sc.Root})
        member __.GoToType pos file text = getInfoAtPos fileManager game.ResourceManager game.InfoService lookup pos file text
        member __.FindAllRefs pos file text = findAllRefsFromPos fileManager game.ResourceManager game.InfoService pos file text
        member __.InfoAtPos pos file text = game.InfoAtPos pos file text
        member __.ReplaceConfigRules rules = refreshRuleCaches game (Some { ruleFiles = rules; validateRules = true; debugRulesOnly = false})
        member __.RefreshCaches() = refreshRuleCaches game None
        member __.ForceRecompute() = resources.ForceRecompute()
        member __.Types() = game.Lookup.typeDefInfo

            //member __.ScriptedTriggers = parseResults |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some p |_ -> None) |> List.map (fun t -> )