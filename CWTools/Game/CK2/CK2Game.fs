namespace CWTools.Games.CK2
open CWTools.Localisation
open CWTools.Validation.ValidationCore
open CWTools.Games.Files
open CWTools.Games
open CWTools.Common
open FSharp.Collections.ParallelSeq
open CWTools.Localisation.CK2Localisation
open CWTools.Utilities.Utils
open CWTools.Utilities.Position
open CWTools.Utilities
open System.IO
open CWTools.Validation.Common.CommonValidation
// open CWTools.Validation.Rules
open CWTools.Parser.ConfigParser
open CWTools.Common.CK2Constants
open CWTools.Validation.Rules
open CWTools.Process.CK2Scopes
open CWTools.Common
open CWTools.Process.Scopes
open CWTools.Validation.CK2
open System.Text
open CWTools.Validation.Rules
open CWTools.Games.LanguageFeatures
open CWTools.Validation.CK2.CK2LocalisationString
open CWTools.Validation.LocalisationString
open CWTools.Process
open CWTools.Process.ProcessCore

module CK2GameFunctions =
    type GameObject = GameObject<Scope, Modifier, CK2ComputedData>
    let processLocalisationFunction (localisationCommands : ((string * Scope list) list)) (lookup : Lookup<Scope, Modifier>) =
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst)
            // @
            // (lookup.typeDefInfo.TryFind "province_id" |> Option.defaultValue [] |> List.map fst)
            // @
            // (lookup.enumDefs.TryFind "country_tags" |> Option.map snd |> Option.defaultValue [])
        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
            // @
            // (lookup.varDefInfo.TryFind "saved_name" |> Option.defaultValue [] |> List.map fst)
            // @
            // (lookup.varDefInfo.TryFind "exiled_ruler" |> Option.defaultValue [] |> List.map fst)
        processLocalisation localisationCommands eventtargets lookup.scriptedLoc definedvars

    let validateLocalisationCommandFunction (localisationCommands : ((string * Scope list) list)) (lookup : Lookup<Scope, Modifier>) =
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst)
            // @
            // (lookup.typeDefInfo.TryFind "province_id" |> Option.defaultValue [] |> List.map fst)
            // @
            // (lookup.enumDefs.TryFind "country_tags" |> Option.map snd |> Option.defaultValue [])
        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
            // @
            // (lookup.varDefInfo.TryFind "saved_name" |> Option.defaultValue [] |> List.map fst)
            // @
            // (lookup.varDefInfo.TryFind "exiled_ruler" |> Option.defaultValue [] |> List.map fst)
        validateLocalisationCommand localisationCommands eventtargets lookup.scriptedLoc definedvars

    let globalLocalisation (game : GameObject) =
        // let locfiles =  resources.GetResources()
        //                 |> List.choose (function |FileWithContentResource (_, e) -> Some e |_ -> None)
        //                 |> List.filter (fun f -> f.overwrite <> Overwritten && f.extension = ".yml" && f.validate)
        //                 |> List.map (fun f -> f.filepath)
        // let locFileValidation = validateLocalisationFiles locfiles
        let locParseErrors = game.LocalisationManager.LocalisationAPIs() <&!&> (fun (b, api) -> if b then validateLocalisationSyntax api.Results else OK)
        let globalTypeLoc = game.ValidationManager.ValidateGlobalLocalisation()
        game.Lookup.proccessedLoc |> validateProcessedLocalisation game.LocalisationManager.taggedLocalisationKeys <&&>
        locParseErrors <&&>
        globalTypeLoc |> (function |Invalid es -> es |_ -> [])
    let updateScriptedLoc (game : GameObject) =
        let rawLocs =
            game.Resources.AllEntities()
            |> List.choose (function |struct (f, _) when f.filepath.Contains("customizable_localisation") -> Some (f.entity) |_ -> None)
            |> List.collect (fun n -> n.Children)
            |> List.map (fun l -> l.TagText "name")
        game.Lookup.scriptedLoc <- rawLocs

    let updateModifiers (game : GameObject) =
        game.Lookup.coreModifiers <- game.Settings.embedded.modifiers

    let addModifiersWithScopes (game : GameObject) =
        let modifierOptions (modifier : Modifier) =
            let requiredScopes =
                modifier.categories |> List.choose (fun c -> modifierCategoryToScopesMap.TryFind c)
                                    |> List.map Set.ofList
                                    |> (fun l -> if List.isEmpty l then [] else l |> List.reduce (Set.intersect) |> Set.toList)
            {min = 0; max = 100; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = requiredScopes}
        game.Lookup.coreModifiers
            |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(specificField c.tag, ValueField (ValueType.Float (-1E+12, 1E+12))), modifierOptions c)))

    let updateLandedTitles (game : GameObject) =
        let fNode =
            fun (t : Node) result ->
                match t.Key with
                | x when x.StartsWith "e_" -> (Empire, x)::result
                | x when x.StartsWith "k_" -> (Kingdom, x)::result
                | x when x.StartsWith "d_" -> (Duchy, x)::result
                | x when x.StartsWith "c_" -> (County, x)::result
                | x when x.StartsWith "b_" -> (Barony, x)::result
                | _ -> result
        let titleEntities = (EntitySet (game.Resources.AllEntities())).GlobMatchChildren("**/landed_titles/**/*.txt")
        let titles = titleEntities |> List.collect (foldNode7 fNode)
        let inner (es, ks, ds, cs, bs) (k : TitleType, value : string) =
             match k with
             | Empire -> (value::es, ks, ds, cs, bs)
             | Kingdom -> (es, value::ks, ds, cs, bs)
             | Duchy -> (es, ks, value::ds, cs, bs)
             | County -> (es, ks, ds, value::cs, bs)
             | Barony -> (es, ks, ds, cs, value::bs)
        let (es, ks, ds, cs, bs) = titles |> List.fold inner ([], [], [], [], [])
        game.Lookup.CK2LandedTitles <- (Empire, es)::(Kingdom, ks)::(Duchy, ds)::(County, cs)::[(Barony, bs)] |> Map.ofList
    let createLandedTitleTypes(game : GameObject)(map : Map<_,_>) =
        let es = game.Lookup.CK2LandedTitles.[Empire] |> List.map (fun e -> (false, e, range.Zero))
        let ks = game.Lookup.CK2LandedTitles.[Kingdom] |> List.map (fun e -> (false, e, range.Zero))
        let ds = game.Lookup.CK2LandedTitles.[Duchy] |> List.map (fun e -> (false, e, range.Zero))
        let cs = game.Lookup.CK2LandedTitles.[County] |> List.map (fun e -> (false, e, range.Zero))
        let bs = game.Lookup.CK2LandedTitles.[Barony] |> List.map (fun e -> (false, e, range.Zero))
        map |> Map.add "title.empire" es |> Map.add "title.kingdom" ks |> Map.add "title.duchy" ds |> Map.add "title.county" cs
            |> Map.add "title.barony" bs |> Map.add "title" (es@ks@ds@cs@bs)

    let updateScriptedEffects(rules :RootRule<Scope> list) =
        let effects =
            rules |> List.choose (function |AliasRule("effect", r) -> Some r |_ -> None)
        let ruleToEffect(r,o) =
            let name =
                match r with
                |LeafRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n
                |NodeRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n
                |_ -> ""
            DocEffect(name, o.requiredScopes, EffectType.Effect, o.description |> Option.defaultValue "", "")
        (effects |> List.map ruleToEffect  |> List.map (fun e -> e :> Effect)) @ (scopedEffects |> List.map (fun e -> e :> Effect))

    let updateScriptedTriggers(rules :RootRule<Scope> list) =
        let effects =
            rules |> List.choose (function |AliasRule("trigger", r) -> Some r |_ -> None)
        let ruleToTrigger(r,o) =
            let name =
                match r with
                |LeafRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n
                |NodeRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n
                |_ -> ""
            DocEffect(name, o.requiredScopes, EffectType.Trigger, o.description |> Option.defaultValue "", "")
        (effects |> List.map ruleToTrigger |> List.map (fun e -> e :> Effect)) @ (scopedEffects |> List.map (fun e -> e :> Effect))

    let addModifiersAsTypes (game : GameObject) (typesMap : Map<string,(string * range) list>) =
        // let createType (modifier : Modifier) =
        typesMap.Add("modifier", game.Lookup.coreModifiers |> List.map (fun m -> (m.tag, range.Zero)))

    let updateTypeDef  =
        let mutable simpleEnums = []
        let mutable complexEnums = []
        let mutable tempTypes = []
        let mutable tempValues = Map.empty
        let mutable tempTypeMap = [("", StringSet.Empty(InsensitiveStringComparer()))] |> Map.ofList
        let mutable tempEnumMap = [("", ("", StringSet.Empty(InsensitiveStringComparer())))] |> Map.ofList
        let mutable rulesDataGenerated = false
        (fun (game : GameObject) rulesSettings ->
            let lookup = game.Lookup
            let resources = game.Resources
            let timer = new System.Diagnostics.Stopwatch()
            timer.Start()
            match rulesSettings with
            |Some rulesSettings ->
                let rules, types, enums, complexenums, values = rulesSettings.ruleFiles |> List.fold (fun (rs, ts, es, ces, vs) (fn, ft) -> let r2, t2, e2, ce2, v2 = parseConfig parseScope allScopes Scope.Any fn ft in rs@r2, ts@t2, es@e2, ces@ce2, vs@v2) ([], [], [], [], [])
                lookup.scriptedEffects <- updateScriptedEffects rules
                lookup.scriptedTriggers <- updateScriptedTriggers rules
                lookup.typeDefs <- types
                let rulesWithMod = rules @ addModifiersWithScopes(game)
                lookup.configRules <- rulesWithMod
                simpleEnums <- enums
                complexEnums <- complexenums
                tempTypes <- types
                tempValues <- values |> List.map (fun (s, sl) -> s, (sl |> List.map (fun s2 -> s2, range.Zero))) |> Map.ofList
                rulesDataGenerated <- false
                log (sprintf "Update config rules def: %i" timer.ElapsedMilliseconds); timer.Restart()
            |None -> ()
            let complexEnumDefs = CWTools.Validation.Rules.getEnumsFromComplexEnums complexEnums (resources.AllEntities() |> List.map (fun struct(e,_) -> e))
            let modifierEnums = { key = "modifiers"; values = lookup.coreModifiers |> List.map (fun m -> m.tag); description = "Modifiers" }
            let allEnums = simpleEnums @ complexEnumDefs @ [modifierEnums]
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            lookup.enumDefs <- allEnums |> List.map (fun e -> (e.key, (e.description, e.values))) |> Map.ofList
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            tempEnumMap <- lookup.enumDefs |> Map.toSeq |> PSeq.map (fun (k, (d, s)) -> k, (d, StringSet.Create(InsensitiveStringComparer(), (s)))) |> Map.ofSeq
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let loc = game.LocalisationManager.localisationKeys
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let files = resources.GetFileNames() |> Set.ofList
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let tempRuleApplicator = RuleApplicator<Scope>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, Collections.Map.empty, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, Scope.Any, changeScope, defaultContext, CK2 CK2Lang.Default)
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let allentities = resources.AllEntities() |> List.map (fun struct(e,_) -> e)
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let typeDefInfo = getTypesFromDefinitions tempRuleApplicator tempTypes allentities
            let typeDefInfo = createLandedTitleTypes game typeDefInfo
            lookup.typeDefInfoForValidation <- typeDefInfo |> Map.map (fun _ v -> v |> List.choose (fun (v, t, r) -> if v then Some (t, r) else None))
            lookup.typeDefInfo <- typeDefInfo |> Map.map (fun _ v -> v |> List.map (fun (_, t, r) -> (t, r)))
            lookup.typeDefInfo <- addModifiersAsTypes game lookup.typeDefInfo
            tempTypeMap <- lookup.typeDefInfo |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))) |> Map.ofSeq
            let tempFoldRules = (FoldRules<Scope>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, Collections.Map.empty, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, tempRuleApplicator, changeScope, defaultContext, Scope.Any, STL STLLang.Default))
            game.InfoService <- Some tempFoldRules
            if not rulesDataGenerated then resources.ForceRulesDataGenerate(); rulesDataGenerated <- true else ()

            let results = resources.AllEntities() |> PSeq.map (fun struct(e, l) -> (l.Force().Definedvariables |> (Option.defaultWith (fun () -> tempFoldRules.GetDefinedVariables e))))
                            |> Seq.fold (fun m map -> Map.toList map |>  List.fold (fun m2 (n,k) -> if Map.containsKey n m2 then Map.add n (k@m2.[n]) m2 else Map.add n k m2) m) tempValues

            lookup.varDefInfo <- results

            let varMap = lookup.varDefInfo |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (List.map fst s))) |> Map.ofSeq

            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            game.completionService <- Some (CompletionService(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, [], changeScope, defaultContext, Scope.Any, oneToOneScopesNames, CK2 CK2Lang.Default))
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            game.RuleApplicator <- Some (RuleApplicator<Scope>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, Scope.Any, changeScope, defaultContext, CK2 CK2Lang.Default))
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            game.InfoService <- Some (FoldRules<Scope>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.scriptedTriggersMap, lookup.scriptedEffectsMap, game.RuleApplicator.Value, changeScope, defaultContext, Scope.Any, CK2 CK2Lang.Default))
            // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
            game.RefreshValidationManager()
            // validationManager <- ValidationManager({validationSettings with ruleApplicator = game.ruleApplicator; foldRules = game.infoService})
        )
    let afterInit (game : GameObject) =
        // updateScriptedTriggers()
        // updateScriptedEffects()
        // updateStaticodifiers()
        // updateScriptedLoc(game)
        // updateDefinedVariables()
        updateModifiers(game)
        // updateLegacyGovernments(game)
        // updateTechnologies()
        updateLandedTitles(game)
        game.LocalisationManager.UpdateAllLocalisation()
        updateTypeDef game game.Settings.rules
        game.LocalisationManager.UpdateAllLocalisation()
type CK2Settings = GameSettings<Modifier, Scope>
open CK2GameFunctions
type CK2Game(settings : CK2Settings) =
    let validationSettings = {
        validators = [ validateMixedBlocks, "mixed";]
        experimentalValidators = []
        heavyExperimentalValidators = []
        experimental = false
        fileValidators = []
        lookupValidators = []
        useRules = true
        debugRulesOnly = false
        localisationValidators = []
    }

    let settings = { settings with embedded = { settings.embedded with localisationCommands = settings.embedded.localisationCommands |> (fun l -> if l.Length = 0 then locCommands else l )}}

    let game = GameObject<Scope, Modifier, CK2ComputedData>.CreateGame
                ((settings, "crusader kings ii", scriptFolders, CK2Compute.computeCK2Data,
                    CK2Compute.computeCK2DataUpdate,
                     (CK2LocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
                     CK2GameFunctions.processLocalisationFunction (settings.embedded.localisationCommands),
                     CK2GameFunctions.validateLocalisationCommandFunction (settings.embedded.localisationCommands),
                     defaultContext,
                     noneContext,
                     Encoding.UTF8,
                     Encoding.GetEncoding(1252),
                     validationSettings,
                     globalLocalisation,
                     (fun _ _ -> ()),
                     ".csv"))
                     afterInit
    let lookup = game.Lookup
    let resources = game.Resources
    let fileManager = game.FileManager


    // let mutable validationManager = ValidationManager(validationSettings)
    // let validateAll shallow newEntities = validationManager.Validate(shallow, newEntities)

    // let localisationCheck (entities : struct (Entity * Lazy<CK2ComputedData>) list) = validationManager.ValidateLocalisation(entities)



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
    //             let resource = makeFileWithContentResourceInput game.FileManager filepath file
    //             resources.UpdateFile(resource) |> ignore
    //             game.LocalisationManager.UpdateAllLocalisation()
    //             let les = (localisationCheck (resources.ValidatableEntities())) @ globalLocalisation()
    //             game.LocalisationManager.localisationErrors <- Some les
    //             globalLocalisation()
    //         | _ ->
    //             let filepath = Path.GetFullPath(filepath)
    //             let file = filetext |> Option.defaultWith (fun () -> File.ReadAllText(filepath, Encoding.GetEncoding(1252)))
    //             let rootedpath = filepath.Substring(filepath.IndexOf(game.FileManager.ScopeDirectory) + (game.FileManager.ScopeDirectory.Length))
    //             let logicalpath = game.FileManager.ConvertPathToLogicalPath rootedpath
    //             let resource = makeEntityResourceInput game.FileManager filepath file

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


    //     log "Update Time: %i" timer.ElapsedMilliseconds
    //     res

    // do
    //     log (sprintf "Parsing %i files" (fileManager.AllFilesByPath().Length))
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


    interface IGame<CK2ComputedData, Scope, Modifier> with
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
        member __.AllLoadedLocalisation() = game.LocalisationManager.LocalisationFileNames()
            // |> List.map
            //     (function
            //         |EntityResource (f, r) ->  r.result |> function |(Fail (result)) -> (r.filepath, false, result.parseTime) |Pass(result) -> (r.filepath, true, result.parseTime)
            //         |FileResource (f, r) ->  (r.filepath, false, 0L))
            //|> List.map (fun r -> r.result |> function |(Fail (result)) -> (r.filepath, false, result.parseTime) |Pass(result) -> (r.filepath, true, result.parseTime))
        member __.ScriptedTriggers() = lookup.scriptedTriggers
        member __.ScriptedEffects() = lookup.scriptedEffects
        member __.StaticModifiers() = [] //lookup.staticModifiers
        member __.UpdateFile shallow file text =game.UpdateFile shallow file text
        member __.AllEntities() = resources.AllEntities()
        member __.References() = References<_, Scope, _>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))
        member __.Complete pos file text = completion fileManager game.completionService game.InfoService game.ResourceManager pos file text
        member __.ScopesAtPos pos file text = scopesAtPos fileManager game.ResourceManager game.InfoService Scope.Any pos file text |> Option.map (fun sc -> { OutputScopeContext.From = sc.From; Scopes = sc.Scopes; Root = sc.Root})
        member __.GoToType pos file text = getInfoAtPos fileManager game.ResourceManager game.InfoService lookup pos file text
        member __.FindAllRefs pos file text = findAllRefsFromPos fileManager game.ResourceManager game.InfoService pos file text
        member __.InfoAtPos pos file text = game.InfoAtPos pos file text
        member __.ReplaceConfigRules rules = refreshRuleCaches game (Some { ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})
        member __.RefreshCaches() = refreshRuleCaches game None
        member __.RefreshLocalisationCaches() = game.LocalisationManager.UpdateProcessedLocalisation()
        member __.ForceRecompute() = resources.ForceRecompute()
        member __.Types() = game.Lookup.typeDefInfo

            //member __.ScriptedTriggers = parseResults |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some p |_ -> None) |> List.map (fun t -> )