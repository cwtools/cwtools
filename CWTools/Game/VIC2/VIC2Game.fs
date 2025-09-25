namespace CWTools.Games.VIC2

open CWTools.Game
open CWTools.Localisation
open CWTools.Utilities.Utils2
open CWTools.Games
open CWTools.Common
open CWTools.Localisation.VIC2Localisation
open System.IO
open CWTools.Validation.Common.CommonValidation
open CWTools.Rules
open CWTools.Common.VIC2Constants
open CWTools.Process.Scopes.VIC2
open CWTools.Process.Scopes.Scopes
open System.Text
open CWTools.Games.LanguageFeatures
open System
open CWTools.Games.Helpers
open CWTools.Parser
open CWTools.Process.Localisation
open Microsoft.FSharp.Collections

module VIC2GameFunctions =
    type GameObject = GameObject<VIC2ComputedData, VIC2Lookup>

    let createLocDynamicSettings (lookup: Lookup) =
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target"
             |> Option.defaultValue [||]
             |> Array.map fst)

        let definedvars =
            (lookup.varDefInfo.TryFind "variable"
             |> Option.defaultValue [||]
             |> Array.map fst)

        { scriptedLocCommands = lookup.scriptedLoc |> List.map (fun s -> s, [ scopeManager.AnyScope ])
          eventTargets = eventtargets |> Array.map (fun s -> s, scopeManager.AnyScope)
          setVariables = definedvars |> IgnoreCaseStringSet }


    let updateModifiers (game: GameObject) =
        game.Lookup.coreModifiers <- game.Settings.embedded.modifiers

    let updateProvinces (game: GameObject) =
        let provinceFile =
            game.Resources.GetResources()
            |> List.choose (function
                | FileWithContentResource(_, e) -> Some e
                | _ -> None)
            |> List.tryFind (fun f ->
                f.overwrite <> Overwrite.Overwritten
                && Path.GetFileName(f.filepath) = "definition.csv")

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

            game.Lookup.VIC2provinces <- provinces



    let refreshConfigBeforeFirstTypesHook (lookup: VIC2Lookup) _ _ =
        let modifierEnums =
            { key = "modifiers"
              values = lookup.coreModifiers |> Array.map _.tag
              description = "Modifiers"
              valuesWithRange = lookup.coreModifiers |> Array.map (fun m -> m.tag, None) }

        let provinceEnums =
            { key = "provinces"
              description = "provinces"
              values = lookup.VIC2provinces
              valuesWithRange = lookup.VIC2provinces |> Array.map (fun x -> x, None) }

        lookup.enumDefs <-
            lookup.enumDefs
            |> Map.add modifierEnums.key (modifierEnums.description, modifierEnums.valuesWithRange)
            |> Map.add provinceEnums.key (provinceEnums.description, provinceEnums.valuesWithRange)

    let afterInit (game: GameObject) =
        updateProvinces (game)
        updateModifiers (game)

    let createEmbeddedSettings embeddedFiles cachedResourceData (configs: (string * string) list) cachedRuleMetadata =
        initializeScopesAndModifierCategories configs defaultScopeInputs defaultModifiersInputs

        let vic2Mods = getActualModifiers configs

        let vic2LocCommands =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "localisation.cwt")
            |> Option.map (fun (fn, ft) -> UtilityParser.loadLocCommands fn ft)
            |> Option.defaultValue ([], [], [])

        let vic2EventTargetLinks =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "links.cwt")
            |> Option.map (fun (fn, ft) ->
                UtilityParser.loadEventTargetLinks
                    scopeManager.AnyScope
                    (scopeManager.ParseScope())
                    scopeManager.AllScopes
                    fn
                    ft)
            |> Option.defaultValue []

        let featureSettings = getFeatureSettings configs

        { triggers = []
          effects = []
          modifiers = vic2Mods
          embeddedFiles = embeddedFiles
          cachedResourceData = cachedResourceData
          localisationCommands = Legacy vic2LocCommands
          eventTargetLinks = vic2EventTargetLinks
          cachedRuleMetadata = cachedRuleMetadata
          featureSettings = featureSettings }

type VIC2Settings = GameSetupSettings<VIC2Lookup>
open VIC2GameFunctions

type VIC2Game(setupSettings: VIC2Settings) =
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
          initialLookup = VIC2Lookup()
          maxFileSize = setupSettings.maxFileSize
          enableInlineScripts = false }

    do
        if scopeManager.Initialized |> not then
            eprintfn "%A has no scopes" (settings.rootDirectories |> List.head)
        else
            ()

    let locSettings =
        settings.embedded.localisationCommands
        |> function
            | Legacy(l, v, links) ->
                (if l.Length = 0 then
                     Legacy([], [], [])
                 else
                     Legacy(l, v, links))
            | _ -> Legacy([], [], [])

    let settings =
        { settings with
            embedded =
                { settings.embedded with
                    localisationCommands = locSettings }
            initialLookup = VIC2Lookup() }

    let legacyLocDataTypes =
        settings.embedded.localisationCommands
        |> function
            | Legacy(c, v, links) -> (c, v, links)
            | _ -> ([], [], [])

    let processLocalisationFunction lookup =
        (createLocalisationFunctions VIC2.locStaticSettings createLocDynamicSettings legacyLocDataTypes lookup)

    let rulesManagerSettings =
        { rulesSettings = settings.rules
          useFormulas = false
          stellarisScopeTriggers = false
          parseScope = scopeManager.ParseScope()
          allScopes = scopeManager.AllScopes
          anyScope = scopeManager.AnyScope
          scopeGroups = scopeManager.ScopeGroups
          changeScope = changeScope
          defaultContext = defaultContext
          defaultLang = VIC2 VIC2Lang.English
          oneToOneScopesNames = oneToOneScopesNames
          loadConfigRulesHook = Hooks.loadConfigRulesHook
          refreshConfigBeforeFirstTypesHook = refreshConfigBeforeFirstTypesHook
          refreshConfigAfterFirstTypesHook = Hooks.refreshConfigAfterFirstTypesHook false
          refreshConfigAfterVarDefHook = Hooks.refreshConfigAfterVarDefHook false
          locFunctions = processLocalisationFunction }

    let game =
        GameObject<VIC2ComputedData, VIC2Lookup>.CreateGame
            ((settings,
              "victoria 2",
              scriptFolders,
              Compute.computeVIC2Data,
              Compute.computeVIC2DataUpdate,
              (VIC2LocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
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
        References<_>(resources, lookup, game.LocalisationManager.GetCleanLocalisationAPIs())


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

    interface IGame<VIC2ComputedData> with
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
        member _.StaticModifiers() = [||] //lookup.staticModifiers
        member _.UpdateFile shallow file text = game.UpdateFile shallow file text
        member _.AllEntities() = resources.AllEntities()

        member _.References() =
            References<_>(resources, lookup, game.LocalisationManager.GetCleanLocalisationAPIs())

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
                (VIC2 VIC2Lang.English)
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
        member _.GetCodeEdits file text = None //getFastTrigger fileManager game.ResourceManager file text

        member _.GetEventGraphData: GraphDataRequest =
            (fun files gameType depth ->
                graphEventDataForFiles references game.ResourceManager lookup files gameType depth)

        member _.GetEmbeddedMetadata() =
            getEmbeddedMetadata lookup game.LocalisationManager game.ResourceManager
