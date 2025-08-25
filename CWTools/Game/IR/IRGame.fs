namespace CWTools.Games.IR

open CWTools.Game
open CWTools.Localisation
open CWTools.Games
open CWTools.Common
open CWTools.Localisation.IR
open System.IO
open CWTools.Validation.Common.CommonValidation
open CWTools.Rules
open CWTools.Common.IRConstants
open CWTools.Process.Scopes.IR
open CWTools.Process.Scopes.Scopes
open System.Text
open CWTools.Games.LanguageFeatures
open System
open CWTools.Games.Helpers
open CWTools.Parser

module IRGameFunctions =
    type GameObject = GameObject<IRComputedData, IRLookup>

    let updateModifiers (game: GameObject) =
        game.Lookup.coreModifiers <- game.Settings.embedded.modifiers


    let updateProvinces (game: GameObject) =
        let provinceFile =
            game.Resources.GetResources()
            |> List.choose (function
                | FileWithContentResource(_, e) -> Some e
                | _ -> None)
            |> List.tryFind (fun f -> f.overwrite <> Overwrite.Overwritten && Path.GetFileName(f.filepath) = "definition.csv")

        match provinceFile with
        | None -> ()
        | Some pf ->
            let lines = pf.filetext.Split([| "\r\n"; "\r"; "\n" |], StringSplitOptions.None)

            let provinces =
                lines
                |> Array.choose (fun l ->
                    if l.StartsWith('#') then
                        None
                    else
                        l.Split(';', 2, StringSplitOptions.RemoveEmptyEntries) |> Array.tryHead)

            game.Lookup.IRprovinces <- provinces

    let updateCharacters (game: GameObject) =
        let characterFile =
            game.Resources.GetResources()
            |> List.choose (function
                | FileWithContentResource(_, e) -> Some e
                | _ -> None)
            |> List.tryFind (fun f ->
                f.overwrite <> Overwrite.Overwritten
                && Path.GetFileName(f.filepath) = "character_setup.csv")

        match characterFile with
        | None -> ()
        | Some pf ->
            let lines = pf.filetext.Split([| "\r\n"; "\r"; "\n" |], StringSplitOptions.None)

            let chars =
                lines
                |> Array.choose (fun l ->
                    if l.StartsWith('#') then
                        None
                    else
                        l.Split(',', 3, StringSplitOptions.RemoveEmptyEntries)
                        |> (fun a ->
                            if a.Length > 1 then
                                a |> Array.skip 1 |> Array.tryHead
                            else
                                None))

            game.Lookup.IRcharacters <- chars


    let refreshConfigBeforeFirstTypesHook (lookup: IRLookup) _ _ =
        let modifierEnums =
            { key = "modifiers"
              values = lookup.coreModifiers |> List.map (fun m -> m.tag) |> List.toArray
              description = "Modifiers"
              valuesWithRange = lookup.coreModifiers |> List.map (fun m -> m.tag, None) |> List.toArray }

        let provinceEnums =
            { key = "provinces"
              description = "provinces"
              values = lookup.IRprovinces
              valuesWithRange = lookup.IRprovinces |> Array.map (fun x -> x, None) }

        let charEnums =
            { key = "character_ids"
              description = "character_ids"
              values = lookup.IRcharacters
              valuesWithRange = lookup.IRcharacters |> Array.map (fun x -> x, None) }

        lookup.enumDefs <-
            lookup.enumDefs
            |> Map.add modifierEnums.key (modifierEnums.description, modifierEnums.valuesWithRange)
            |> Map.add provinceEnums.key (provinceEnums.description, provinceEnums.valuesWithRange)
            |> Map.add charEnums.key (charEnums.description, charEnums.valuesWithRange)


    let afterInit (game: GameObject) =
        updateProvinces (game)
        updateCharacters (game)
        updateModifiers (game)

    let createEmbeddedSettings embeddedFiles cachedResourceData (configs: (string * string) list) cachedRuleMetadata =
        let scopeDefinitions =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
            |> (fun f -> UtilityParser.initializeScopes f (Some defaultScopeInputs))

        configs
        |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifier_categories.cwt")
        |> (fun f -> UtilityParser.initializeModifierCategories f (Some(defaultModifiersInputs ())))


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
                eprintfn "effects.log was not found in ir config"
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
                eprintfn "triggers.log was not found in ir config"
                [])

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

type IRSettings = GameSetupSettings<IRLookup>
open IRGameFunctions

type IRGame(setupSettings: IRSettings) =
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
          initialLookup = IRLookup()
          maxFileSize = setupSettings.maxFileSize
          enableInlineScripts = false }

    do
        if scopeManager.Initialized |> not then
            eprintfn "%A has no scopes" (settings.rootDirectories |> List.head)
        else
            ()

    let jominiLocDataTypes =
        settings.embedded.localisationCommands
        |> function
            | Jomini dts -> Some dts
            | _ -> None

    let processLocalisationFunction lookup =
        (createJominiLocalisationFunctions jominiLocDataTypes lookup)

    let rulesManagerSettings =
        { rulesSettings = settings.rules
          useFormulas = true
          stellarisScopeTriggers = false
          parseScope = scopeManager.ParseScope()
          allScopes = scopeManager.AllScopes
          anyScope = scopeManager.AnyScope
          scopeGroups = scopeManager.ScopeGroups
          changeScope = changeScope
          defaultContext = defaultContext
          defaultLang = IR IRLang.English
          oneToOneScopesNames = oneToOneScopesNames
          loadConfigRulesHook = Hooks.loadConfigRulesHook
          refreshConfigBeforeFirstTypesHook = refreshConfigBeforeFirstTypesHook
          refreshConfigAfterFirstTypesHook = Hooks.refreshConfigAfterFirstTypesHook false
          refreshConfigAfterVarDefHook = Hooks.refreshConfigAfterVarDefHook false
          locFunctions = processLocalisationFunction }

    let game =
        GameObject<IRComputedData, IRLookup>.CreateGame
            ((settings,
              "imperator",
              scriptFolders,
              Compute.Jomini.computeJominiData,
              Compute.Jomini.computeJominiDataUpdate,
              (IRLocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
              processLocalisationFunction,
              defaultContext,
              noneContext,
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

    interface IGame<IRComputedData> with
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
                (IR IRLang.English)
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
