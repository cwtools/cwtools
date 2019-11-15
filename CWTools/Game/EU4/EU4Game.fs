namespace CWTools.Games.EU4
open CWTools.Localisation
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Games
open CWTools.Common
open FSharp.Collections.ParallelSeq
open CWTools.Localisation.EU4
open CWTools.Utilities.Utils
open CWTools.Utilities.Position
open CWTools.Utilities
open CWTools.Validation.Common.CommonValidation
open CWTools.Rules
open CWTools.Common.EU4Constants
open CWTools.Process.Scopes.EU4
open CWTools.Process.Scopes.Scopes
open System.Text
open CWTools.Games.LanguageFeatures
open CWTools.Validation.EU4.EU4Validation
open CWTools.Validation.EU4.EU4LocalisationString
open CWTools.Games.Helpers
open CWTools.Games.Compute.EU4
open CWTools.Parser
open System.IO
open CWTools.Process.Localisation

module EU4GameFunctions =
    type GameObject = GameObject<EU4ComputedData, EU4Lookup>
    let createLocDynamicSettings(lookup : Lookup) =
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.typeDefInfo.TryFind "province_id" |> Option.defaultValue [] |> List.map (fun tdi -> tdi.id))
            @
            (lookup.enumDefs.TryFind "country_tags" |> Option.map snd |> Option.defaultValue [])
        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.varDefInfo.TryFind "exiled_ruler" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.varDefInfo.TryFind "saved_name" |> Option.defaultValue [] |> List.map fst)
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
    let updateScriptedLoc (game : GameObject) =
        let rawLocs =
            game.Resources.AllEntities()
            |> List.choose (function |struct (f, _) when f.filepath.Contains("customizable_localization") -> Some (f.entity) |_ -> None)
            |> List.collect (fun n -> n.Children)
            |> List.map (fun l -> l.TagText "name")
        game.Lookup.scriptedLoc <- rawLocs
    let updateModifiers (game : GameObject) =
            game.Lookup.coreModifiers <- addGeneratedModifiers game.Settings.embedded.modifiers (EntitySet (game.Resources.AllEntities()))

    let updateLegacyGovernments (game : GameObject) =
        let es = game.Resources.AllEntities() |> EntitySet
        let allReforms = es.GlobMatchChildren("**/government_reforms/*.txt")
        let legacies = allReforms |> List.choose (fun n -> if n.TagText "legacy_government" == "yes" then Some n.Key else None) |> Set.ofList
        let legacyRefs = allReforms |> List.choose (fun n -> if n.Has "legacy_equivalent" then Some (n.TagText "legacy_equivalent") else None) |> Set.ofList
        let legacyOnly = Set.difference legacies legacyRefs |> Set.toList
        game.Lookup.EU4TrueLegacyGovernments <- legacyOnly

    let addModifiersWithScopes (lookup : Lookup) =
        (lookup.coreModifiers |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(CWTools.Rules.RulesParser.specificField c.tag, ValueField (ValueType.Float (-1E+12M, 1E+12M))), {min = 0; max = 100; strictMin = true; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = []; comparison = false; referenceDetails = None}))))

    let updateScriptedEffects(rules :RootRule list) =
        let effects =
            rules |> List.choose (function |AliasRule("effect", r) -> Some r |_ -> None)
        let ruleToEffect(r,o) =
            let name =
                match r with
                |LeafRule(SpecificField(SpecificValue n),_) -> StringResource.stringManager.GetStringForID n.normal
                |NodeRule(SpecificField(SpecificValue n),_) -> StringResource.stringManager.GetStringForID n.normal
                |_ -> ""
            DocEffect(name, o.requiredScopes, o.pushScope, EffectType.Effect, o.description |> Option.defaultValue "", "")
        (effects |> List.map ruleToEffect  |> List.map (fun e -> e :> Effect)) @ (scopedEffects() |> List.map (fun e -> e :> Effect))

    let updateScriptedTriggers(rules :RootRule list) =
        let effects =
            rules |> List.choose (function |AliasRule("trigger", r) -> Some r |_ -> None)
        let ruleToTrigger(r,o) =
            let name =
                match r with
                |LeafRule(SpecificField(SpecificValue n),_) -> StringResource.stringManager.GetStringForID n.normal
                |NodeRule(SpecificField(SpecificValue n),_) -> StringResource.stringManager.GetStringForID n.normal
                |_ -> ""
            DocEffect(name, o.requiredScopes, o.pushScope, EffectType.Trigger, o.description |> Option.defaultValue "", "")
        (effects |> List.map ruleToTrigger |> List.map (fun e -> e :> Effect)) @ (scopedEffects() |> List.map (fun e -> e :> Effect))

    let addModifiersAsTypes (lookup : Lookup) (typesMap : Map<string,TypeDefInfo list>) =
        typesMap.Add("modifier", lookup.coreModifiers |> List.map (fun m -> createTypeDefInfo false m.tag range.Zero [] []))

    let loadConfigRulesHook rules (lookup : Lookup) embedded =
        let ts = updateScriptedTriggers rules
        let es = updateScriptedEffects rules
        let ls = updateEventTargetLinks embedded
        lookup.allCoreLinks <- ts @ es @ ls
        rules @ addModifiersWithScopes lookup

    let refreshConfigBeforeFirstTypesHook (lookup : EU4Lookup) (resources : IResourceAPI<EU4ComputedData>) _ =
        lookup.EU4ScriptedEffectKeys <- "scaled_skill" :: (resources.AllEntities() |> PSeq.map (fun struct(e, l) -> (l.Force().ScriptedEffectParams |> (Option.defaultWith (fun () -> getScriptedEffectParamsEntity e))))
                                            |> List.ofSeq |> List.collect id)
        let scriptedEffectParmas = { key = "scripted_effect_params"; description = "Scripted effect parameter"; values = lookup.EU4ScriptedEffectKeys }
        let scriptedEffectParmasD =  { key = "scripted_effect_params_dollar"; description = "Scripted effect parameter"; values = lookup.EU4ScriptedEffectKeys |> List.map (fun k -> sprintf "$%s$" k)}
        let modifierEnums = { key = "modifiers"; values = lookup.coreModifiers |> List.map (fun m -> m.tag); description = "Modifiers" }
        let legacyGovEnums = { key = "hardcoded_legacy_only_governments"; values = lookup.EU4TrueLegacyGovernments; description = "Legacy only government"}
        lookup.enumDefs <-
            lookup.enumDefs |> Map.add scriptedEffectParmas.key (scriptedEffectParmas.description, scriptedEffectParmas.values)
                            |> Map.add scriptedEffectParmasD.key (scriptedEffectParmasD.description, scriptedEffectParmasD.values)
                            |> Map.add modifierEnums.key (modifierEnums.description, modifierEnums.values)
                            |> Map.add legacyGovEnums.key (legacyGovEnums.description, legacyGovEnums.values)

    let refreshConfigAfterFirstTypesHook (lookup : Lookup) _ (embeddedSettings : EmbeddedSettings) =
        lookup.typeDefInfo <-
            (lookup.typeDefInfo)
            |> addModifiersAsTypes lookup
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings false

    let refreshConfigAfterVarDefHook (lookup : Lookup) (resources : IResourceAPI<_>) (embeddedSettings : EmbeddedSettings) =
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings false

    let afterInit (game : GameObject) =
        updateScriptedLoc(game)
        updateModifiers(game)
        updateLegacyGovernments(game)

    let createEmbeddedSettings embeddedFiles cachedResourceData (configs : (string * string) list) =
        let scopeDefinitions =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
                            |> (fun f -> UtilityParser.initializeScopes f (Some defaultScopeInputs) )
        configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifier_categories.cwt")
                |> (fun f -> UtilityParser.initializeModifierCategories f (Some (defaultModifiersInputs())) )


        let triggers, effects = ([], [])

        let modifiers =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifiers.cwt")
                    |> Option.map (fun (fn, ft) -> EU4Parser.loadModifiers fn ft)
                    |> Option.defaultValue []

        let eu4LocCommands =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "localisation.cwt")
                    |> Option.map (fun (fn, ft) -> UtilityParser.loadLocCommands fn ft)
                    |> Option.defaultValue ([], [])

        let eu4EventTargetLinks =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "links.cwt")
                    |> Option.map (fun (fn, ft) -> UtilityParser.loadEventTargetLinks scopeManager.AnyScope (scopeManager.ParseScope()) scopeManager.AllScopes fn ft)
                    |> Option.defaultValue (CWTools.Process.Scopes.EU4.scopedEffects() |> List.map SimpleLink)

        {
            triggers = triggers
            effects = effects
            modifiers = modifiers
            embeddedFiles = embeddedFiles
            cachedResourceData = cachedResourceData
            localisationCommands = Legacy eu4LocCommands
            eventTargetLinks = eu4EventTargetLinks
        }

type EU4Settings = GameSetupSettings<EU4Lookup>
open EU4GameFunctions
type EU4Game(setupSettings : EU4Settings) =
    let validationSettings = {
        validators = [ validateMixedBlocks, "mixed"; validateEU4NaiveNot, "not"; validateIfWithNoEffect, "ifnoeffect"]
        experimentalValidators = []
        heavyExperimentalValidators = []
        experimental = false
        fileValidators = []
        lookupValidators = [valUniqueTypes, "uniques"]
        lookupFileValidators = [valScriptedEffectParams, "scripted_effects"]
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
        initialLookup = EU4Lookup()
        maxFileSize = setupSettings.maxFileSize

    }

    do if scopeManager.Initialized |> not then eprintfn "%A has no scopes" (settings.rootDirectories |> List.head) else ()

    let locSettings = settings.embedded.localisationCommands |> function |Legacy (l, v) -> (if l.Length = 0 then Legacy (locCommands()) else Legacy (l, v)) |_ -> Legacy (locCommands())
    let settings = { settings with
                        embedded = { settings.embedded with localisationCommands = locSettings }
                        initialLookup = EU4Lookup()
                        }

    let rulesManagerSettings = {
        rulesSettings = settings.rules
        parseScope = scopeManager.ParseScope()
        allScopes = scopeManager.AllScopes
        anyScope = scopeManager.AnyScope
        changeScope = changeScope
        defaultContext = defaultContext
        defaultLang = EU4 EU4Lang.Default
        oneToOneScopesNames = oneToOneScopesNames
        loadConfigRulesHook = loadConfigRulesHook
        refreshConfigBeforeFirstTypesHook = refreshConfigBeforeFirstTypesHook
        refreshConfigAfterFirstTypesHook = refreshConfigAfterFirstTypesHook
        refreshConfigAfterVarDefHook = refreshConfigAfterVarDefHook
        processLocalisation = EU4GameFunctions.processLocalisationFunction (settings.embedded.localisationCommands |> function |Legacy l -> l |_ -> [])
        validateLocalisation = EU4GameFunctions.validateLocalisationCommandFunction (settings.embedded.localisationCommands |> function |Legacy l -> l |_ -> [])
    }

    let game = GameObject.CreateGame
                ((settings, "europa universalis iv", scriptFolders, Compute.EU4.computeEU4Data,
                    Compute.EU4.computeEU4DataUpdate,
                     (EU4LocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
                     EU4GameFunctions.processLocalisationFunction (settings.embedded.localisationCommands |> function |Legacy (c, v) -> c, v |_ -> ([], [])),
                     EU4GameFunctions.validateLocalisationCommandFunction (settings.embedded.localisationCommands |> function |Legacy (c, v) -> c, v |_ -> ([], [])),
                     defaultContext,
                     noneContext,
                     Encoding.UTF8,
                     Encoding.GetEncoding(1252),
                     validationSettings,
                     globalLocalisation,
                     (fun _ _ -> ()),
                     ".yml",
                     rulesManagerSettings))
                     afterInit
    let lookup = game.Lookup
    let resources = game.Resources
    let fileManager = game.FileManager
    let references = References<_>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))

    let parseErrors() =
        resources.GetResources()
            |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None)
            |> List.choose (fun r -> r.result |> function |(Fail (result)) when r.validate -> Some (r.filepath, result.error, result.position)  |_ -> None)

    interface IGame<EU4ComputedData> with
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
        member __.UpdateFile shallow file text =game.UpdateFile shallow file text
        member __.AllEntities() = resources.AllEntities()
        member __.References() = References<_>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))
        member __.Complete pos file text = completion fileManager game.completionService game.InfoService game.ResourceManager pos file text
        member __.ScopesAtPos pos file text = scopesAtPos fileManager game.ResourceManager game.InfoService scopeManager.AnyScope pos file text
        member __.GoToType pos file text = getInfoAtPos fileManager game.ResourceManager game.InfoService game.LocalisationManager lookup (EU4 EU4Lang.English) pos file text
        member __.FindAllRefs pos file text = findAllRefsFromPos fileManager game.ResourceManager game.InfoService pos file text
        member __.InfoAtPos pos file text = game.InfoAtPos pos file text
        member __.ReplaceConfigRules rules = game.ReplaceConfigRules(({ ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})) //refreshRuleCaches game (Some { ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})
        member __.RefreshCaches() = game.RefreshCaches()
        member __.RefreshLocalisationCaches() = game.LocalisationManager.UpdateProcessedLocalisation()
        member __.ForceRecompute() = resources.ForceRecompute()
        member __.Types() = game.Lookup.typeDefInfo
        member __.TypeDefs() = game.Lookup.typeDefs
        member __.GetPossibleCodeEdits file text = []
        member __.GetCodeEdits file text = None
        member __.GetEventGraphData : GraphDataRequest = (fun files gameType depth -> graphEventDataForFiles references game.ResourceManager lookup files gameType depth)
