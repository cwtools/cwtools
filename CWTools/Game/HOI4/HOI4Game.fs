namespace CWTools.Games.HOI4
open CWTools.Common.HOI4Constants
open CWTools.Localisation
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Games
open CWTools.Common
open CWTools.Localisation.HOI4
open CWTools.Utilities
open System.IO
open CWTools.Validation.Common.CommonValidation
open CWTools.Rules
open CWTools.Process.Scopes.HOI4
open CWTools.Process.Scopes.Scopes
open System.Text
open CWTools.Games.LanguageFeatures
open System
open CWTools.Validation.HOI4.HOI4LocalisationString
open CWTools.Games.Helpers
open CWTools.Parser
open CWTools.Process.Localisation

module HOI4GameFunctions =
    type GameObject = GameObject<HOI4ComputedData, HOI4Lookup>
    let createLocDynamicSettings(lookup : Lookup) =
        //eprintfn "clds %A" (lookup.enumDefs.TryFind "country_tags")
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.varDefInfo.TryFind "global_event_target" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.typeDefInfo.TryFind "state" |> Option.defaultValue [] |> List.map (fun tdi -> tdi.id))
            @
            (lookup.enumDefs.TryFind "country_tags" |> Option.map snd |> Option.defaultValue [])
        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.varDefInfo.TryFind "saved_name" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.varDefInfo.TryFind "exiled_ruler" |> Option.defaultValue [] |> List.map fst)
        {
            scriptedLocCommands = lookup.scriptedLoc |> List.map (fun s -> s, [scopeManager.AnyScope])
            eventTargets = eventtargets |> List.map (fun s -> s, scopeManager.AnyScope)
            setVariables = definedvars
        }
    let processLocalisationFunction (commands, variableCommands) (lookup : Lookup) =
        processLocalisation commands variableCommands (createLocDynamicSettings(lookup))

    let validateLocalisationCommandFunction (commands, variableCommands) (lookup : Lookup) =
        validateLocalisationCommand commands variableCommands (createLocDynamicSettings(lookup))

    let globalLocalisation (game : GameObject) =
        let globalTypeLoc = game.ValidationManager.ValidateGlobalLocalisation()
        game.Lookup.proccessedLoc |> validateProcessedLocalisation game.LocalisationManager.taggedLocalisationKeys <&&>
        globalTypeLoc |> (function |Invalid (_, es) -> es |_ -> [])

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

    let updateScriptedLoc (game : GameObject) =
        let rawLocs =
            game.Resources.AllEntities()
            |> List.choose (function |struct (f, _) when f.filepath.Contains("scripted_localisation") -> Some (f.entity) |_ -> None)
            |> List.collect (fun n -> n.Children)
            |> List.map (fun l -> l.TagText "name")
        game.Lookup.embeddedScriptedLoc <- game.Settings.embedded.cachedRuleMetadata |> Option.map (fun crm -> crm.scriptedLoc) |> Option.defaultValue []
        game.Lookup.scriptedLoc <- rawLocs

    let updateScriptedEffects(rules :RootRule list) (states : string list) (countries : string list) =
        let effects =
            rules |> List.choose (function |AliasRule("effect", r) -> Some r |_ -> None)
        let ruleToEffect(r,o) =
            let name =
                match r with
                |LeafRule(SpecificField(SpecificValue n),_) -> StringResource.stringManager.GetStringForID n.normal
                |NodeRule(SpecificField(SpecificValue n),_) -> StringResource.stringManager.GetStringForID n.normal
                |_ -> ""
            DocEffect(name, o.requiredScopes, o.pushScope, EffectType.Effect, o.description |> Option.defaultValue "", "")
        let stateEffects =  states |> List.map (fun p -> ScopedEffect(p, scopeManager.AllScopes, Some (scopeManager.ParseScope() "State"), EffectType.Link, defaultDesc, "", true));
        let countryEffects =  countries |> List.map (fun p -> ScopedEffect(p, scopeManager.AllScopes, Some (scopeManager.ParseScope() "Country"), EffectType.Link, defaultDesc, "", true));
        (effects |> List.map ruleToEffect  |> List.map (fun e -> e :> Effect)) @ (scopedEffects() |> List.map (fun e -> e :> Effect))
        @ (stateEffects |> List.map (fun e -> e :> Effect)) @ (countryEffects |> List.map (fun e -> e :> Effect))

    let updateScriptedTriggers(rules :RootRule list) states countries =
        let effects =
            rules |> List.choose (function |AliasRule("trigger", r) -> Some r |_ -> None)
        let ruleToTrigger(r,o) =
            let name =
                match r with
                |LeafRule(SpecificField(SpecificValue n),_) -> StringResource.stringManager.GetStringForID n.normal
                |NodeRule(SpecificField(SpecificValue n),_) -> StringResource.stringManager.GetStringForID n.normal
                |_ -> ""
            DocEffect(name, o.requiredScopes, o.pushScope, EffectType.Trigger, o.description |> Option.defaultValue "", "")
        let stateEffects =  states |> List.map (fun p -> ScopedEffect(p, scopeManager.AllScopes, Some (scopeManager.ParseScope() "State"), EffectType.Link, defaultDesc, "", true));
        let countryEffects =  countries |> List.map (fun p -> ScopedEffect(p, scopeManager.AllScopes, Some (scopeManager.ParseScope() "Country"), EffectType.Link, defaultDesc, "", true));
        (effects |> List.map ruleToTrigger |> List.map (fun e -> e :> Effect)) @ (scopedEffects() |> List.map (fun e -> e :> Effect))
        @ (stateEffects |> List.map (fun e -> e :> Effect)) @ (countryEffects |> List.map (fun e -> e :> Effect))
    let addModifiersWithScopes (lookup : Lookup) =
        let modifierOptions (modifier : ActualModifier) =
            let requiredScopes =
                modifierCategoryManager.SupportedScopes modifier.category
            {min = 0; max = 100; strictMin = true; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = requiredScopes; comparison = false; referenceDetails = None; keyRequiredQuotes = false; valueRequiredQuotes = false}
        lookup.coreModifiers
            |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(CWTools.Rules.RulesParser.specificField c.tag, ValueField (ValueType.Float (-1E+12M, 1E+12M))), modifierOptions c)))

    let loadConfigRulesHook rules (lookup : Lookup) embedded =
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ updateEventTargetLinks embedded
        rules @ addModifiersWithScopes lookup

    let refreshConfigBeforeFirstTypesHook (lookup : HOI4Lookup) _ _ =
        let provinceEnums = { key = "provinces"; description = "provinces"; values = lookup.HOI4provinces}
        lookup.enumDefs <-
            lookup.enumDefs |> Map.add provinceEnums.key (provinceEnums.description, provinceEnums.values)

    let refreshConfigAfterFirstTypesHook (lookup : Lookup) _ (embeddedSettings : EmbeddedSettings) =
        let states = lookup.typeDefInfo.TryFind "state"
                            |> Option.map (fun sl -> sl |> List.map (fun tdi -> tdi.id))
                            |> Option.defaultValue []
        let countries = lookup.enumDefs.TryFind "country_tag"
                            |> Option.map snd
                            |> Option.defaultValue []
        let ts = updateScriptedTriggers lookup.configRules states countries
        let es = updateScriptedEffects lookup.configRules states countries
        let ls = updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings false
        lookup.allCoreLinks <- ts @ es @ ls

    let refreshConfigAfterVarDefHook (lookup : Lookup) (resources : IResourceAPI<_>) (embeddedSettings : EmbeddedSettings) =
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings false

    let afterInit (game : GameObject) =
        updateModifiers(game)
        updateProvinces(game)
        updateScriptedLoc(game)

    let createEmbeddedSettings embeddedFiles cachedResourceData (configs : (string * string) list) (cachedRuleMetadata : CachedRuleMetadata option) =
        let scopeDefinitions =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
                            |> (fun f -> UtilityParser.initializeScopes f (Some defaultScopeInputs) )

        configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifier_categories.cwt")
                |> (fun f -> UtilityParser.initializeModifierCategories f (Some (defaultModifiersInputs())) )


        let hoi4Mods =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifiers.cwt")
                    |> Option.map (fun (fn, ft) -> UtilityParser.loadModifiers fn ft)
                    |> Option.defaultValue []
        let hoi4LocCommands =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "localisation.cwt")
                    |> Option.map (fun (fn, ft) -> HOI4Parser.loadLocCommands fn ft)
                    |> Option.defaultValue ([], [])


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
            localisationCommands = Legacy hoi4LocCommands
            eventTargetLinks = eventTargetLinks
            cachedRuleMetadata = cachedRuleMetadata
        }

type HOI4Settings = GameSetupSettings<HOI4Lookup>
open HOI4GameFunctions
type HOI4Game(setupSettings : HOI4Settings) =
    let validationSettings = {
        validators = [ validateIfWithNoEffect, "ifnoeffect"; CWTools.Validation.Stellaris.STLValidation.validateRedundantANDWithNOT, "AND"; ]
        experimentalValidators = []
        heavyExperimentalValidators = []
        experimental = setupSettings.validation.experimental
        fileValidators = []
        lookupValidators = [valUniqueTypes, "uniques"]
        lookupFileValidators = []
        useRules = true
        debugRulesOnly = false
        localisationValidators = []
    }
    let embeddedSettings =
        match setupSettings.embedded with
        | FromConfig (ef, crd) ->
            createEmbeddedSettings ef crd (setupSettings.rules |> Option.map (fun r -> r.ruleFiles) |> Option.defaultValue []) None
        | Metadata cmd ->
            createEmbeddedSettings [] [] (setupSettings.rules |> Option.map (fun r -> r.ruleFiles) |> Option.defaultValue []) (Some cmd)
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
        maxFileSize = setupSettings.maxFileSize

    }
    do if scopeManager.Initialized |> not then eprintfn "%A has no scopes" (settings.rootDirectories |> List.head) else ()

    let locSettings = settings.embedded.localisationCommands |> function |Legacy (l, v) -> (if l.Length = 0 then Legacy (locCommands()) else Legacy (l, v)) |_ -> Legacy (locCommands())
    let settings = { settings with
                        embedded = { settings.embedded with localisationCommands = locSettings }
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
        processLocalisation = HOI4GameFunctions.processLocalisationFunction (settings.embedded.localisationCommands |> function |Legacy (c, v) -> c, v |_ -> ([], []))
        validateLocalisation = HOI4GameFunctions.validateLocalisationCommandFunction (settings.embedded.localisationCommands |> function |Legacy (c, v) -> c, v |_ -> ([], []))
    }

    let game = GameObject.CreateGame
                (settings, "hearts of iron iv", scriptFolders, Compute.computeHOI4Data,
                Compute.computeHOI4DataUpdate,
                 (HOI4LocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
                 HOI4GameFunctions.processLocalisationFunction (settings.embedded.localisationCommands |> function |Legacy (c, v) -> c, v |_ -> ([], [])),
                 HOI4GameFunctions.validateLocalisationCommandFunction (settings.embedded.localisationCommands |> function |Legacy (c, v) -> c, v |_ -> ([], [])),
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
    let references = References<_>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))

    let getEmbeddedFiles() = settings.embedded.embeddedFiles |> List.map (fun (fn, f) -> "embedded", "embeddedfiles/" + fn, f)

    let parseErrors() =
        resources.GetResources()
            |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None)
            |> List.choose (fun r -> r.result |> function |(Fail (result)) when r.validate -> Some (r.filepath, result.error, result.position)  |_ -> None)

    interface IGame<ComputedData> with
        member __.ParserErrors() = parseErrors()
        member __.ValidationErrors() = let (s, d) = (game.ValidationManager.Validate(false, (resources.ValidatableEntities()))) in s @ d
        member __.LocalisationErrors(force : bool, forceGlobal : bool) =
            getLocalisationErrors game globalLocalisation (force, forceGlobal)

        member __.Folders() = fileManager.AllFolders()
        member __.AllFiles() =
            resources.GetResources()
        member __.AllLoadedLocalisation() = game.LocalisationManager.LocalisationFileNames()
        member __.ScriptedTriggers() = lookup.triggers
        member __.ScriptedEffects() = lookup.effects
        member __.StaticModifiers() = [] //lookup.staticModifiers
        member __.UpdateFile shallow file text = game.UpdateFile shallow file text
        member __.AllEntities() = resources.AllEntities()
        member __.References() = References<HOI4ComputedData>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))
        member __.Complete pos file text = completion fileManager game.completionService game.InfoService game.ResourceManager pos file text
        member __.ScopesAtPos pos file text = scopesAtPos fileManager game.ResourceManager game.InfoService scopeManager.AnyScope pos file text
        member __.GoToType pos file text = getInfoAtPos fileManager game.ResourceManager game.InfoService game.LocalisationManager lookup (HOI4 HOI4Lang.English) pos file text
        member __.FindAllRefs pos file text = findAllRefsFromPos fileManager game.ResourceManager game.InfoService pos file text
        member __.InfoAtPos pos file text = game.InfoAtPos pos file text
        member __.ReplaceConfigRules rules = game.ReplaceConfigRules(({ ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})) //refreshRuleCaches game (Some { ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})
        member __.RefreshCaches() = game.RefreshCaches()
        member __.RefreshLocalisationCaches() = game.LocalisationManager.UpdateProcessedLocalisation()
        member __.ForceRecompute() = resources.ForceRecompute()
        member __.Types() = game.Lookup.typeDefInfo
        member __.TypeDefs() = game.Lookup.typeDefs
        member __.GetPossibleCodeEdits file text = []
        member __.GetCodeEdits file text = None //getFastTrigger fileManager game.ResourceManager file text
        member __.GetEventGraphData : GraphDataRequest = (fun files gameType depth -> graphEventDataForFiles references game.ResourceManager lookup files gameType depth)
        member __.GetEmbeddedMetadata() = getEmbeddedMetadata lookup game.LocalisationManager game.ResourceManager