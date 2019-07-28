namespace CWTools.Games.HOI4
open CWTools.Common.HOI4Constants
open CWTools.Localisation
open CWTools.Validation
open CWTools.Games
open CWTools.Common
open CWTools.Localisation.HOI4
open CWTools.Utilities
open System.IO
open CWTools.Validation.Common.CommonValidation
// open CWTools.Rules.Rules
open CWTools.Rules
open CWTools.Process.Scopes.HOI4
open CWTools.Process.Scopes
open CWTools.Validation.HOI4
open System.Text
open CWTools.Games.LanguageFeatures
open System
open CWTools.Validation.HOI4.HOI4LocalisationString
open CWTools.Games.Helpers
open CWTools.Parser
open CWTools.Common.NewScope

module HOI4GameFunctions =
    type GameObject = GameObject<Scope, Modifier, HOI4ComputedData, HOI4Lookup>
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
        processLocalisation() localisationCommands eventtargets lookup.scriptedLoc definedvars
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
        validateLocalisationCommand() localisationCommands eventtargets lookup.scriptedLoc definedvars
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
            game.Lookup.HOI4provinces <- provinces

    let updateScriptedEffects(rules :RootRule<Scope> list) (states : string list) (countries : string list) =
        let effects =
            rules |> List.choose (function |AliasRule("effect", r) -> Some r |_ -> None)
        let ruleToEffect(r,o) =
            let name =
                match r with
                |LeafRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |NodeRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |_ -> ""
            DocEffect(name, o.requiredScopes, o.pushScope, EffectType.Effect, o.description |> Option.defaultValue "", "")
        let stateEffects =  states |> List.map (fun p -> ScopedEffect(p, scopeManager.AllScopes, Some (scopeManager.ParseScope() "State"), EffectType.Link, defaultDesc, "", true));
        let countryEffects =  countries |> List.map (fun p -> ScopedEffect(p, scopeManager.AllScopes, Some (scopeManager.ParseScope() "Country"), EffectType.Link, defaultDesc, "", true));
        (effects |> List.map ruleToEffect  |> List.map (fun e -> e :> Effect)) @ (scopedEffects() |> List.map (fun e -> e :> Effect))
        @ (stateEffects |> List.map (fun e -> e :> Effect)) @ (countryEffects |> List.map (fun e -> e :> Effect))

    let updateScriptedTriggers(rules :RootRule<Scope> list) states countries =
        let effects =
            rules |> List.choose (function |AliasRule("trigger", r) -> Some r |_ -> None)
        let ruleToTrigger(r,o) =
            let name =
                match r with
                |LeafRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |NodeRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |_ -> ""
            DocEffect(name, o.requiredScopes, o.pushScope, EffectType.Trigger, o.description |> Option.defaultValue "", "")
        let stateEffects =  states |> List.map (fun p -> ScopedEffect(p, scopeManager.AllScopes, Some (scopeManager.ParseScope() "State"), EffectType.Link, defaultDesc, "", true));
        let countryEffects =  countries |> List.map (fun p -> ScopedEffect(p, scopeManager.AllScopes, Some (scopeManager.ParseScope() "Country"), EffectType.Link, defaultDesc, "", true));
        (effects |> List.map ruleToTrigger |> List.map (fun e -> e :> Effect)) @ (scopedEffects() |> List.map (fun e -> e :> Effect))
        @ (stateEffects |> List.map (fun e -> e :> Effect)) @ (countryEffects |> List.map (fun e -> e :> Effect))
    let addModifiersWithScopes (lookup : Lookup<_,_>) =
        let modifierOptions (modifier : Modifier) =
            let requiredScopes =
                modifier.categories |> List.choose (fun c -> modifierCategoryToScopesMap().TryFind c)
                                    |> List.map Set.ofList
                                    |> (fun l -> if List.isEmpty l then [] else l |> List.reduce (Set.intersect) |> Set.toList)
            {min = 0; max = 100; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = requiredScopes; comparison = false}
        lookup.coreModifiers
            |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(CWTools.Rules.RulesParser.specificField c.tag, ValueField (ValueType.Float (-1E+12, 1E+12))), modifierOptions c)))

    let loadConfigRulesHook rules (lookup : Lookup<_,_>) embedded =
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ updateEventTargetLinks embedded
        rules @ addModifiersWithScopes lookup

    let refreshConfigBeforeFirstTypesHook (lookup : HOI4Lookup) _ _ =
        let provinceEnums = { key = "provinces"; description = "provinces"; values = lookup.HOI4provinces}
        lookup.enumDefs <-
            lookup.enumDefs |> Map.add provinceEnums.key (provinceEnums.description, provinceEnums.values)

    let refreshConfigAfterFirstTypesHook (lookup : Lookup<_,_>) _ (embeddedSettings : EmbeddedSettings<_,_>) =
        let states = lookup.typeDefInfo.TryFind "state"
                            |> Option.map (fun sl -> sl |> List.map fst)
                            |> Option.defaultValue []
        let countries = lookup.enumDefs.TryFind "country_tag"
                            |> Option.map snd
                            |> Option.defaultValue []
        let ts = updateScriptedTriggers lookup.configRules states countries
        let es = updateScriptedEffects lookup.configRules states countries
        let ls = updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings false
        lookup.allCoreLinks <- ts @ es @ ls

    let refreshConfigAfterVarDefHook (lookup : Lookup<_,_>) (resources : IResourceAPI<_>) (embeddedSettings : EmbeddedSettings<_,_>) =
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings false

    let afterInit (game : GameObject) =
        updateModifiers(game)
        updateProvinces(game)

    let createEmbeddedSettings embeddedFiles cachedResourceData (configs : (string * string) list) =
        let scopeDefinitions =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
                            |> (fun f -> UtilityParser.initializeScopes f (Some defaultScopeInputs) )
        let hoi4Mods =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifiers.cwt")
                    |> Option.map (fun (fn, ft) -> HOI4Parser.loadModifiers fn ft)
                    |> Option.defaultValue []
        let hoi4LocCommands =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "localisation.cwt")
                    |> Option.map (fun (fn, ft) -> HOI4Parser.loadLocCommands fn ft)
                    |> Option.defaultValue []


        let triggers, effects = ([], [])

        let eventTargetLinks =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "links.cwt")
                    |> Option.map (fun (fn, ft) -> UtilityParser.loadEventTargetLinks scopeManager.AnyScope (scopeManager.ParseScope()) scopeManager.AllScopes fn ft)
                    |> Option.defaultValue (CWTools.Process.Scopes.HOI4.scopedEffects() |> List.map SimpleLink)

        {
            triggers = triggers
            effects = effects
            modifiers = hoi4Mods
            embeddedFiles = embeddedFiles
            cachedResourceData = cachedResourceData
            localisationCommands = hoi4LocCommands
            eventTargetLinks = eventTargetLinks
            scopeDefinitions = scopeDefinitions
        }

type HOI4Settings = GameSetupSettings<Modifier, Scope, HOI4Lookup>
open HOI4GameFunctions
type HOI4Game(setupSettings : HOI4Settings) =
    let validationSettings = {
        validators = [ validateMixedBlocks, "mixed"; validateIfWithNoEffect, "ifnoeffect" ]
        experimentalValidators = []
        heavyExperimentalValidators = []
        experimental = setupSettings.validation.experimental
        fileValidators = []
        lookupValidators = [valUniqueTypes, "uniques"]
        useRules = true
        debugRulesOnly = false
        localisationValidators = []
    }
    let embeddedSettings =
        match setupSettings.embedded with
        | FromConfig (ef, crd) ->
            createEmbeddedSettings ef crd (setupSettings.rules |> Option.map (fun r -> r.ruleFiles) |> Option.defaultValue [])
        | ManualSettings e -> e

    let settings = {
        rootDirectories = setupSettings.rootDirectories
        excludeGlobPatterns = setupSettings.excludeGlobPatterns
        embedded = embeddedSettings
        GameSettings.rules = setupSettings.rules
        validation = setupSettings.validation
        scriptFolders = setupSettings.scriptFolders
        modFilter = setupSettings.modFilter
        initialLookup = HOI4Lookup()

    }
    do if settings.embedded.scopeDefinitions = [] then eprintfn "%A has no scopes" (settings.rootDirectories |> List.head) else ()

    let settings = { settings with
                        embedded = { settings.embedded with localisationCommands = settings.embedded.localisationCommands |> (fun l -> if l.Length = 0 then locCommands() else l )}
                        initialLookup = HOI4Lookup()}

    let rulesManagerSettings = {
        rulesSettings = settings.rules
        parseScope = scopeManager.ParseScope()
        allScopes = scopeManager.AllScopes
        anyScope = scopeManager.AnyScope
        changeScope = changeScope
        defaultContext = defaultContext
        defaultLang = HOI4 HOI4Lang.Default
        oneToOneScopesNames = oneToOneScopesNames
        loadConfigRulesHook = loadConfigRulesHook
        refreshConfigBeforeFirstTypesHook = refreshConfigBeforeFirstTypesHook
        refreshConfigAfterFirstTypesHook = refreshConfigAfterFirstTypesHook
        refreshConfigAfterVarDefHook = refreshConfigAfterVarDefHook
    }

    let game = GameObject.CreateGame
                (settings, "hearts of iron iv", scriptFolders, Compute.computeHOI4Data,
                Compute.computeHOI4DataUpdate,
                 (HOI4LocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
                 HOI4GameFunctions.processLocalisationFunction (settings.embedded.localisationCommands),
                 HOI4GameFunctions.validateLocalisationCommandFunction (settings.embedded.localisationCommands),
                 defaultContext,
                 noneContext,
                 Encoding.UTF8,
                 Encoding.GetEncoding(1252),
                 validationSettings,
                 globalLocalisation,
                 (fun _ _ -> ()),
                 ".yml",
                 rulesManagerSettings)
                afterInit
    let lookup = game.Lookup
    let resources = game.Resources
    let fileManager = game.FileManager

    let getEmbeddedFiles() = settings.embedded.embeddedFiles |> List.map (fun (fn, f) -> "embedded", "embeddedfiles/" + fn, f)

    let parseErrors() =
        resources.GetResources()
            |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None)
            |> List.choose (fun r -> r.result |> function |(Fail (result)) when r.validate -> Some (r.filepath, result.error, result.position)  |_ -> None)

    interface IGame<ComputedData, Scope, Modifier> with
    //member __.Results = parseResults
        member __.ParserErrors() = parseErrors()
        member __.ValidationErrors() = let (s, d) = (game.ValidationManager.Validate(false, (resources.ValidatableEntities()))) in s @ d
        member __.LocalisationErrors(force : bool, forceGlobal : bool) =
            getLocalisationErrors game globalLocalisation (force, forceGlobal)

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
        member __.ScriptedTriggers() = lookup.triggers
        member __.ScriptedEffects() = lookup.effects
        member __.StaticModifiers() = [] //lookup.staticModifiers
        member __.UpdateFile shallow file text = game.UpdateFile shallow file text
        member __.AllEntities() = resources.AllEntities()
        member __.References() = References<HOI4ComputedData, Scope, _>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))
        member __.Complete pos file text = completion fileManager game.completionService game.InfoService game.ResourceManager pos file text
        member __.ScopesAtPos pos file text = scopesAtPos fileManager game.ResourceManager game.InfoService scopeManager.AnyScope pos file text
        member __.GoToType pos file text = getInfoAtPos fileManager game.ResourceManager game.InfoService lookup pos file text
        member __.FindAllRefs pos file text = findAllRefsFromPos fileManager game.ResourceManager game.InfoService pos file text
        member __.InfoAtPos pos file text = game.InfoAtPos pos file text
        member __.ReplaceConfigRules rules = game.ReplaceConfigRules(({ ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})) //refreshRuleCaches game (Some { ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})
        member __.RefreshCaches() = game.RefreshCaches()
        member __.RefreshLocalisationCaches() = game.LocalisationManager.UpdateProcessedLocalisation()
        member __.ForceRecompute() = resources.ForceRecompute()
        member __.Types() = game.Lookup.typeDefInfo
        member __.GetPossibleCodeEdits file text = []
        member __.GetCodeEdits file text = None //getFastTrigger fileManager game.ResourceManager file text
