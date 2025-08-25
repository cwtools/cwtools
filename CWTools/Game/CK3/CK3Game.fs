namespace CWTools.Games.CK3

open CWTools.Game
open CWTools.Localisation
open CWTools.Games
open CWTools.Common
open System.IO
open CWTools.Validation.Common.CommonValidation
open CWTools.Process.Scopes
open System.Text
open CWTools.Games.LanguageFeatures
open CWTools.Games.Helpers
open CWTools.Parser

module CK3GameFunctions =
    type GameObject = GameObject<JominiComputedData, JominiLookup>

    let updateModifiers (game: GameObject) =
        game.Lookup.coreModifiers <- game.Settings.embedded.modifiers

    let afterInit (game: GameObject) = updateModifiers (game)

    let createEmbeddedSettings embeddedFiles cachedResourceData (configs: (string * string) list) cachedRuleMetadata =
        let scopeDefinitions =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
            |> (fun f -> UtilityParser.initializeScopes f (Some []))

        configs
        |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifier_categories.cwt")
        |> (fun f -> UtilityParser.initializeModifierCategories f (Some []))

        let irMods =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifiers.cwt")
            |> Option.map (fun (fn, ft) -> UtilityParser.loadModifiers fn ft)
            |> Option.defaultValue []

        let irLocCommands =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "localisation.cwt")
            |> Option.map (fun (fn, ft) -> UtilityParser.loadLocCommands fn ft)
            |> Option.defaultValue ([], [], [])

        let jominiLocDataTypes =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "data_types.log")
            |> Option.map (fun (fn, ft) ->
                DataTypeParser.parseDataTypesStreamRes (
                    new MemoryStream(System.Text.Encoding.GetEncoding(1252).GetBytes(ft))
                ))
            |> Option.defaultValue
                { DataTypeParser.JominiLocDataTypes.promotes = Map.empty
                  confidentFunctions = Map.empty
                  DataTypeParser.JominiLocDataTypes.functions = Map.empty
                  DataTypeParser.JominiLocDataTypes.dataTypes = Map.empty
                  DataTypeParser.JominiLocDataTypes.dataTypeNames = Set.empty }

        let irEventTargetLinks =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "links.cwt")
            |> Option.map (fun (fn, ft) ->
                UtilityParser.loadEventTargetLinks
                    scopeManager.AnyScope
                    (scopeManager.ParseScope())
                    scopeManager.AllScopes
                    fn
                    ft)
            |> Option.defaultValue (CWTools.Process.Scopes.IR.scopedEffects |> List.map SimpleLink)

        let irEffects =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "effects.log")
            |> Option.bind (fun (fn, ft) ->
                JominiParser.parseEffectStreamRes (
                    new MemoryStream(System.Text.Encoding.GetEncoding(1252).GetBytes(ft))
                ))
            |> Option.map (JominiParser.processEffects scopeManager.ParseScopes)
            |> Option.defaultWith (fun () ->
                eprintfn "effects.log was not found in ck3 config"
                [])

        let irTriggers =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "triggers.log")
            |> Option.bind (fun (fn, ft) ->
                JominiParser.parseTriggerStreamRes (
                    new MemoryStream(System.Text.Encoding.GetEncoding(1252).GetBytes(ft))
                ))
            |> Option.map (JominiParser.processTriggers scopeManager.ParseScopes)
            |> Option.defaultWith (fun () ->
                eprintfn "triggers.log was not found in ck3 config"
                [])

        let featureSettings =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "settings.cwt")
            |> Option.bind (fun (fn, ft) -> UtilityParser.loadSettingsFile fn ft)
            |> Option.defaultValue CWTools.Parser.UtilityParser.FeatureSettings.Default


        { triggers = irTriggers
          effects = irEffects
          modifiers = irMods
          embeddedFiles = embeddedFiles
          cachedResourceData = cachedResourceData
          localisationCommands = Jomini jominiLocDataTypes
          eventTargetLinks = irEventTargetLinks
          cachedRuleMetadata = cachedRuleMetadata
          featureSettings = featureSettings }

type CK3Settings = GameSetupSettings<JominiLookup>

open CK3GameFunctions
open CWTools.Localisation.CK3

type CK3Game(setupSettings: CK3Settings) =
    let validationSettings =
        { validators = [ validateIfWithNoEffect, "ifnoeffect" ]
          experimentalValidators = []
          heavyExperimentalValidators = []
          experimental = false
          fileValidators = []
          lookupValidators = commonValidationRules
          lookupFileValidators = []
          useRules = true
          debugRulesOnly = false
          localisationValidators = [] }

    let embeddedSettings =
        match setupSettings.embedded with
        | FromConfig(ef, crd) ->
            createEmbeddedSettings
                ef
                crd
                (setupSettings.rules
                 |> Option.map (fun r -> r.ruleFiles)
                 |> Option.defaultValue [])
                None
        | Metadata cmd ->
            createEmbeddedSettings
                []
                []
                (setupSettings.rules
                 |> Option.map (fun r -> r.ruleFiles)
                 |> Option.defaultValue [])
                (Some cmd)
        | ManualSettings e -> e

    let settings =
        { rootDirectories = setupSettings.rootDirectories
          excludeGlobPatterns = setupSettings.excludeGlobPatterns
          embedded = embeddedSettings
          GameSettings.rules = setupSettings.rules
          validation = setupSettings.validation
          scriptFolders = setupSettings.scriptFolders
          modFilter = setupSettings.modFilter
          initialLookup = JominiLookup()
          maxFileSize = setupSettings.maxFileSize
          enableInlineScripts = false

        }

    do
        if scopeManager.Initialized |> not then
            eprintfn "%A has no scopes" (settings.rootDirectories |> List.head)
        else
            ()

    let locCommands () = []

    let settings =
        { settings with
            initialLookup = JominiLookup() }

    let changeScope =
        Scopes.createJominiChangeScope
            CWTools.Process.Scopes.CK3.oneToOneScopes
            (Scopes.complexVarPrefixFun "variable:from:" "variable:")

    let jominiLocDataTypes =
        settings.embedded.localisationCommands
        |> function
            | Jomini dts -> Some dts
            | _ -> None

    let processLocalisationFunction lookup =
        (createJominiLocalisationFunctions jominiLocDataTypes lookup)

    // let validationLocalisationCommandFunction lookup =
    // createJominiLocalisationFunctions jominiLocDataTypes lookup |> snd

    let rulesManagerSettings =
        { rulesSettings = settings.rules
          useFormulas = true
          stellarisScopeTriggers = false
          parseScope = scopeManager.ParseScope()
          allScopes = scopeManager.AllScopes
          anyScope = scopeManager.AnyScope
          scopeGroups = scopeManager.ScopeGroups
          changeScope = changeScope
          defaultContext = CWTools.Process.Scopes.Scopes.defaultContext
          defaultLang = CK3 CK3Lang.English
          oneToOneScopesNames = CWTools.Process.Scopes.CK3.oneToOneScopesNames
          loadConfigRulesHook = Hooks.loadConfigRulesHook
          refreshConfigBeforeFirstTypesHook = Hooks.refreshConfigBeforeFirstTypesHook
          refreshConfigAfterFirstTypesHook = Hooks.refreshConfigAfterFirstTypesHook true
          refreshConfigAfterVarDefHook = Hooks.refreshConfigAfterVarDefHook true
          locFunctions = processLocalisationFunction }

    let scriptFolders = []

    let game =
        GameObject<JominiComputedData, JominiLookup>.CreateGame
            ((settings,
              "crusader kings iii",
              scriptFolders,
              Compute.Jomini.computeJominiData,
              Compute.Jomini.computeJominiDataUpdate,
              (CK3LocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
              processLocalisationFunction,
              CWTools.Process.Scopes.Scopes.defaultContext,
              CWTools.Process.Scopes.Scopes.noneContext,
              Encoding.UTF8,
              Encoding.GetEncoding(1252),
              validationSettings,
              Hooks.globalLocalisation,
              (fun _ _ -> ()),
              ".yml",
              rulesManagerSettings,
              setupSettings.debugSettings))
            afterInit

    let lookup = game.Lookup
    let resources = game.Resources
    let fileManager = game.FileManager

    let references =
        References<_>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))


    let parseErrors () =
        resources.GetResources()
        |> List.choose (function
            | EntityResource(_, e) -> Some e
            | _ -> None)
        |> List.choose (fun r ->
            r.result
            |> function
                | Fail result when r.validate -> Some(r.filepath, result.error, result.position)
                | _ -> None)

    interface IGame<JominiComputedData> with
        member _.ParserErrors() = parseErrors ()

        member _.ValidationErrors() =
            let s, d = game.ValidationManager.Validate(false, resources.ValidatableEntities()) in s @ d

        member _.LocalisationErrors(force: bool, forceGlobal: bool) =
            getLocalisationErrors game Hooks.globalLocalisation (force, forceGlobal)

        member _.Folders() = fileManager.AllFolders()
        member _.AllFiles() = resources.GetResources()

        member _.AllLoadedLocalisation() =
            game.LocalisationManager.LocalisationFileNames()

        member _.ScriptedTriggers() = lookup.triggers
        member _.ScriptedEffects() = lookup.effects
        member _.StaticModifiers() = [] //lookup.staticModifiers
        member _.UpdateFile shallow file text = game.UpdateFile shallow file text
        member _.AllEntities() = resources.AllEntities()

        member _.References() =
            References<_>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))

        member _.Complete pos file text =
            completion fileManager game.completionService game.InfoService game.ResourceManager pos file text

        member _.ScopesAtPos pos file text =
            scopesAtPos fileManager game.ResourceManager game.InfoService scopeManager.AnyScope pos file text

        member _.GoToType pos file text =
            getInfoAtPos
                fileManager
                game.ResourceManager
                game.InfoService
                game.LocalisationManager
                lookup
                (CK3 CK3Lang.English)
                pos
                file
                text

        member _.FindAllRefs pos file text =
            findAllRefsFromPos fileManager game.ResourceManager game.InfoService pos file text

        member _.InfoAtPos pos file text = game.InfoAtPos pos file text

        member _.ReplaceConfigRules rules =
            game.ReplaceConfigRules
                { ruleFiles = rules
                  validateRules = true
                  debugRulesOnly = false
                  debugMode = false } //refreshRuleCaches game (Some { ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})

        member _.RefreshCaches() = game.RefreshCaches()

        member _.RefreshLocalisationCaches() =
            game.LocalisationManager.UpdateProcessedLocalisation()

        member _.ForceRecompute() = resources.ForceRecompute()
        member _.Types() = game.Lookup.typeDefInfo
        member _.TypeDefs() = game.Lookup.typeDefs
        member _.GetPossibleCodeEdits file text = []
        member _.GetCodeEdits file text = None

        member _.GetEventGraphData: GraphDataRequest =
            (fun files gameType depth ->
                graphEventDataForFiles references game.ResourceManager lookup files gameType depth)

        member _.GetEmbeddedMetadata() =
            getEmbeddedMetadata lookup game.LocalisationManager game.ResourceManager
