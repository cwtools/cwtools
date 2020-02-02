namespace CWTools.Games.Stellaris

open CWTools.Parser
open CWTools.Process
open CWTools.Validation.Stellaris.STLValidation
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Localisation
open CWTools.Localisation.STL
open CWTools.Common
open CWTools.Common.STLConstants
open CWTools.Process.Scopes.STL
open CWTools.Validation.Stellaris.STLLocalisationValidation
open CWTools.Validation.Stellaris.STLEventValidation
open CWTools.Validation.Stellaris.STLLocalisationString
open CWTools.Utilities
open CWTools.Validation.Stellaris.Graphics
open CWTools.Games
open CWTools.Games.Stellaris
open CWTools.Games.Stellaris.STLLookup
open CWTools.Rules
open CWTools.Validation.Stellaris
open CWTools.Validation.Common.CommonValidation
open CWTools.Process.Scopes.Scopes
open System.Text
open CWTools.Games.LanguageFeatures
open CWTools.Validation.LocalisationString
open CWTools.Games.Helpers
open FSharp.Collections.ParallelSeq
open System.IO
open CWTools.Process.Localisation

module STLGameFunctions =
    type GameObject = GameObject<STLComputedData, STLLookup>

    let createLocDynamicSettings(lookup : Lookup) =
        let eventtargets = (lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst)
                           @ (lookup.varDefInfo.TryFind "global_event_target" |> Option.defaultValue [] |> List.map fst)
        let definedVariables = (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
        {
            scriptedLocCommands = lookup.scriptedLoc |> List.map (fun s -> s, [scopeManager.AnyScope])
            eventTargets = eventtargets |> List.map (fun s -> s, scopeManager.AnyScope)
            setVariables = definedVariables
        }

    let updateScriptedTriggers (game : GameObject) =
        let vanillaTriggers =
            let se = scopedEffects() |> List.map (fun e -> e :> Effect)
            let vt = game.Settings.embedded.triggers |> addInnerScope |> List.map (fun e -> e :> Effect)
            se @ vt
        let sts, ts = STLLookup.updateScriptedTriggers game.Resources vanillaTriggers
        game.Lookup.onlyScriptedTriggers <- sts
        sts @ ts

    let updateScriptedEffects (game : GameObject) =
        let vanillaEffects =
            let se = scopedEffects() |> List.map (fun e -> e :> Effect)
            let ve = game.Settings.embedded.effects |> addInnerScope |> List.map (fun e -> e :> Effect)
            se @ ve
        let ses, es = STLLookup.updateScriptedEffects game.Resources vanillaEffects (game.Lookup.triggers)
        game.Lookup.onlyScriptedEffects <- ses
        ses @ es

    let updateStaticodifiers (game : GameObject) =
        let rawModifiers =
            game.Resources.AllEntities()
            |> List.choose (function |struct (f, _) when f.filepath.Contains("static_modifiers") -> Some (f.entity) |_ -> None)
            |> List.collect (fun n -> n.Children)
        let newModifiers = rawModifiers |> List.map (fun e -> STLProcess.getStaticModifierCategory game.Settings.embedded.modifiers e)
        game.Lookup.staticModifiers <- newModifiers

    let updateScriptedLoc (game : GameObject) =
        let rawLocs =
            game.Resources.AllEntities()
            |> List.choose (function |struct (f, _) when f.filepath.Contains("scripted_loc") -> Some (f.entity) |_ -> None)
            |> List.collect (fun n -> n.Children)
            |> List.map (fun l -> l.TagText "name")
        game.Lookup.embeddedScriptedLoc <- game.Settings.embedded.cachedRuleMetadata |> Option.map (fun crm -> crm.scriptedLoc) |> Option.defaultValue []
        game.Lookup.scriptedLoc <- rawLocs
    let updateDefinedVariables(game : GameObject) =
        game.Lookup.definedScriptVariables <- (game.Resources.AllEntities()) |> List.collect (fun struct (_, d) -> d.Force().Setvariables)

    let afterUpdateFile (game : GameObject<STLComputedData,STLLookup>) (filepath : string) =
        match filepath with
        |x when x.Contains("scripted_triggers") -> updateScriptedTriggers(game) |> ignore
        |x when x.Contains("scripted_effects") -> updateScriptedEffects(game) |> ignore
        |x when x.Contains("scripted_loc") -> updateScriptedLoc(game)
        |x when x.Contains("static_modifiers") -> updateStaticodifiers(game)
        |_ -> ()
        updateDefinedVariables(game)

    let globalLocalisation (game : GameObject) =
        let locfiles =  game.Resources.GetResources()
                        |> List.choose (function |FileWithContentResource (_, e) -> Some e |_ -> None)
                        |> List.filter (fun f -> f.overwrite <> Overwritten && f.extension = ".yml" && f.validate)
                        |> List.map (fun f -> f.filepath)
        let locFileValidation = validateLocalisationFiles locfiles
        let locParseErrors = game.LocalisationManager.LocalisationAPIs() <&!&> (fun (b, api) -> if b then validateLocalisationSyntax api.Results else OK)
        let globalTypeLoc = game.ValidationManager.ValidateGlobalLocalisation()
        game.Lookup.proccessedLoc |> validateProcessedLocalisation game.LocalisationManager.taggedLocalisationKeys <&&> locFileValidation <&&> globalTypeLoc <&&> locParseErrors |> (function |Invalid (_, es) -> es |_ -> [])

    let updateModifiers(game : GameObject) =
        game.Lookup.coreModifiers <- addGeneratedModifiers game.Settings.embedded.modifiers (EntitySet (game.Resources.AllEntities()))

    let updateTechnologies(game : GameObject) =
        game.Lookup.technologies <- getTechnologies (EntitySet (game.Resources.AllEntities()))

    let addModifiersWithScopes (lookup : Lookup) =
        let modifierOptions (modifier : ActualModifier) =
            let requiredScopes =
                modifierCategoryManager.SupportedScopes modifier.category
            {min = 0; max = 100; strictMin = true; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = requiredScopes; comparison = false; referenceDetails = None; keyRequiredQuotes = false; valueRequiredQuotes = false}
        lookup.coreModifiers
            |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(CWTools.Rules.RulesParser.specificField c.tag, ValueField (ValueType.Float (-1E+12M, 1E+12M))), modifierOptions c)))

    let addTriggerDocsScopes (lookup : Lookup) (rules : RootRule list) =
            let scriptedOptions (effect : ScriptedEffect) =
                {min = 0; max = 100; strictMin = true; leafvalue = false; description = Some effect.Comments; pushScope = None; replaceScopes = None; severity = None; requiredScopes = effect.Scopes; comparison = false; referenceDetails = None; keyRequiredQuotes = false; valueRequiredQuotes = false}
            let getAllScriptedEffects =
                lookup.onlyScriptedEffects |> List.choose (function | :? ScriptedEffect as se -> Some se |_ -> None)
                                                |> List.map (fun se -> AliasRule("effect", NewRule(LeafRule(CWTools.Rules.RulesParser.specificField se.Name, ValueField(ValueType.Bool)), scriptedOptions se)))
            let getAllScriptedTriggers =
                lookup.onlyScriptedTriggers |> List.choose (function | :? ScriptedEffect as se -> Some se |_ -> None)
                                                |> List.map (fun se -> AliasRule("trigger", NewRule(LeafRule(CWTools.Rules.RulesParser.specificField se.Name, ValueField(ValueType.Bool)), scriptedOptions se)))
            let addRequiredScopesE (s : StringTokens) (o : Options) =
                let newScopes =
                    match o.requiredScopes with
                    |[] ->
                        lookup.effectsMap.TryFind((StringResource.stringManager.GetStringForID s.normal))
                            |> Option.map(fun se -> se.Scopes)
                            |> Option.defaultValue []
                    |x -> x
                { o with requiredScopes = newScopes}
            let addRequiredScopesT (s : StringTokens) (o : Options) =
                let newScopes =
                    match o.requiredScopes with
                    |[] ->
                        lookup.triggersMap.TryFind((StringResource.stringManager.GetStringForID s.normal))
                            |> Option.map(fun se -> se.Scopes)
                            |> Option.defaultValue []
                    |x -> x
                { o with requiredScopes = newScopes}
            rules |> List.collect (
                    function
                    |AliasRule ("effect", (LeafRule(SpecificField(SpecificValue s),r), o)) ->
                        [AliasRule ("effect", (LeafRule(SpecificField(SpecificValue s),r), addRequiredScopesE s o))]
                    |AliasRule ("trigger", (LeafRule(SpecificField(SpecificValue s),r), o)) ->
                        [AliasRule ("trigger", (LeafRule(SpecificField(SpecificValue s),r), addRequiredScopesT s o))]
                    |AliasRule ("effect", (NodeRule(SpecificField(SpecificValue s),r), o)) ->
                        [AliasRule ("effect", (NodeRule(SpecificField(SpecificValue s),r), addRequiredScopesE s o))]
                    |AliasRule ("trigger", (NodeRule(SpecificField(SpecificValue s),r), o)) ->
                        [AliasRule ("trigger", (NodeRule(SpecificField(SpecificValue s),r), addRequiredScopesT s o))]
                    |AliasRule ("effect", (LeafValueRule(SpecificField(SpecificValue s)), o)) ->
                        [AliasRule ("effect", (LeafValueRule(SpecificField(SpecificValue s)), addRequiredScopesE s o))]
                    |AliasRule ("trigger", (LeafValueRule(SpecificField(SpecificValue s)), o)) ->
                        [AliasRule ("trigger", (LeafValueRule(SpecificField(SpecificValue s)), addRequiredScopesT s o))]
                    |AliasRule ("effect", (LeafRule(TypeField(TypeType.Simple "scripted_effect"), o), _)) ->
                        getAllScriptedEffects
                    |AliasRule ("trigger", (LeafRule(TypeField(TypeType.Simple "scripted_trigger"), o), _)) ->
                        getAllScriptedTriggers
                    |x -> [x])


    let loadConfigRulesHook rules (lookup : Lookup) embedded =
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ updateEventTargetLinks embedded //@ addDataEventTargetLinks lookup embedded
        let rulesWithMod = rules @ addModifiersWithScopes(lookup)
        let rulesWithEmbeddedScopes = addTriggerDocsScopes lookup rulesWithMod
        rulesWithEmbeddedScopes

    let refreshConfigBeforeFirstTypesHook (lookup : STLLookup) (resources : IResourceAPI<STLComputedData>) (embeddedSettings : EmbeddedSettings)  =
        lookup.STLScriptedEffectKeys <- "scaled_skill" :: (resources.AllEntities() |> PSeq.map (fun struct(e, l) -> (l.Force().ScriptedEffectParams |> (Option.defaultWith (fun () -> CWTools.Games.Compute.EU4.getScriptedEffectParamsEntity e))))
                                            |> List.ofSeq |> List.collect id)
        let scriptedEffectParmas = { key = "scripted_effect_params"; description = "Scripted effect parameter"; values = lookup.STLScriptedEffectKeys }
        let scriptedEffectParmasD =  { key = "scripted_effect_params_dollar"; description = "Scripted effect parameter"; values = lookup.STLScriptedEffectKeys |> List.map (fun k -> sprintf "$%s$" k)}
        lookup.enumDefs <-
            lookup.enumDefs |> Map.add scriptedEffectParmas.key (scriptedEffectParmas.description, scriptedEffectParmas.values)
                            |> Map.add scriptedEffectParmasD.key (scriptedEffectParmasD.description, scriptedEffectParmasD.values)


    let refreshConfigAfterFirstTypesHook (lookup : Lookup) (resources : IResourceAPI<_>) (embeddedSettings : EmbeddedSettings) =
        lookup.globalScriptedVariables <- (EntitySet (resources.AllEntities())).GlobMatch "**/common/scripted_variables/*.txt" |> List.collect STLValidation.getDefinedVariables
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ (updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings false)

    let refreshConfigAfterVarDefHook (lookup : Lookup) (resources : IResourceAPI<_>) (embeddedSettings : EmbeddedSettings) =
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ (updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings false)
    let afterInit (game : GameObject) =
            let ts = updateScriptedTriggers(game)
            game.Lookup.allCoreLinks <- ts @ game.Lookup.effects @ game.Lookup.eventTargetLinks
            let es = updateScriptedEffects(game)
            game.Lookup.allCoreLinks <- game.Lookup.triggers @ es @ game.Lookup.eventTargetLinks
            updateStaticodifiers(game)
            updateScriptedLoc(game)
            updateDefinedVariables(game)
            updateModifiers(game)
            updateTechnologies(game)

    let createEmbeddedSettings embeddedFiles cachedResourceData (configs : (string * string) list) cachedRuleMetadata =
        let scopeDefinitions =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
                            |> (fun f -> UtilityParser.initializeScopes f (Some defaultScopeInputs) )
        configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifier_categories.cwt")
                |> (fun f -> UtilityParser.initializeModifierCategories f (Some (defaultModifiersInputs())) )


        let triggers, effects =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "trigger_docs.log")
                    |> Option.map (fun (fn, ft) -> DocsParser.parseDocsFile fn)
                    |> Option.bind ((function |FParsec.CharParsers.ParserResult.Success(p, _, _) -> Some (DocsParser.processDocs scopeManager.ParseScopes p) |FParsec.CharParsers.ParserResult.Failure(e, _, _) -> eprintfn "%A" e; None))
                    |> Option.defaultWith (fun () -> Utils.logError "trigger_docs.log was not found in stellaris config"; ([], []))
        let modifiers =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "setup.log")
                    |> Option.map (fun (fn, ft) -> SetupLogParser.parseLogsFile fn)
                    |> Option.bind ((function |FParsec.CharParsers.ParserResult.Success(p, _, _) -> Some (SetupLogParser.processLogs p) |FParsec.CharParsers.ParserResult.Failure(e, _, _) -> None))
                    |> Option.defaultWith (fun () -> Utils.logError "setup.log was not found in stellaris config"; ([]))
        let stlLocCommands =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "localisation.cwt")
                    |> Option.map (fun (fn, ft) -> UtilityParser.loadLocCommands fn ft)
                    |> Option.defaultValue ([], [])
        let stlEventTargetLinks =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "links.cwt")
                    |> Option.map (fun (fn, ft) -> UtilityParser.loadEventTargetLinks scopeManager.AnyScope (scopeManager.ParseScope()) scopeManager.AllScopes fn ft)
                    |> Option.defaultValue ([])

        {
            triggers = triggers
            effects = effects
            modifiers = modifiers
            embeddedFiles = embeddedFiles
            cachedResourceData = cachedResourceData
            localisationCommands = Legacy stlLocCommands
            eventTargetLinks = stlEventTargetLinks
            cachedRuleMetadata = cachedRuleMetadata
        }


type StellarisSettings = GameSetupSettings<STLLookup>

open STLGameFunctions
type STLGame (setupSettings : StellarisSettings) =
    let validationSettings = {
        validators = [validateVariables, "var"; valTechnology, "tech"; validateTechnologies, "tech2"; valButtonEffects, "but"; valSprites, "sprite"; valVariables, "var2"; valEventCalls, "event";
                            validateAmbientGraphics, "ambient"; validateShipDesigns, "designs"; validateSolarSystemInitializers, "solar";
                             validateIfElse, "ifelse2"; validatePlanetKillers, "pk"; validateRedundantAND, "AND"; valMegastructureGraphics, "megastructure";
                            valPlanetClassGraphics, "pcg"; validateDeprecatedSetName, "setname"; validateShips, "ships"; validateEvents, "eventsSimple"; validateNOTMultiple, "not"; validatePreTriggers, "pre";
                            validateIfWithNoEffect, "ifnoeffect";]
        experimentalValidators = [valSectionGraphics, "sections"; valComponentGraphics, "component"]
        heavyExperimentalValidators = [getEventChains, "event chains"]
        experimental = setupSettings.validation.experimental
        fileValidators = [valSpriteFiles, "sprites"; valMeshFiles, "mesh"; valAssetFiles, "asset"; valComponentIcons, "compicon"]
        lookupValidators = [valAllModifiers, "mods"; valUniqueTypes, "uniques"; validateEconomicCatAIBudget, "aibudget"]
        lookupFileValidators = [valScriptedEffectParams, "scripted_effects"]
        useRules = setupSettings.rules |> Option.map (fun o -> o.validateRules) |> Option.defaultValue false
        debugRulesOnly = setupSettings.rules |> Option.map (fun o -> o.debugRulesOnly) |> Option.defaultValue false
        localisationValidators = [valEventLocs; valTechLocs; valCompSetLocs; valCompTempLocs; valTraditionLocCats

                                ; valPolicies; valEffectLocs; valTriggerLocs;]

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
            initialLookup = STLLookup()
            maxFileSize = setupSettings.maxFileSize
        }
        do if scopeManager.Initialized |> not then Utils.logError (sprintf "%A has no scopes" (settings.rootDirectories |> List.head)) else ()
        let locSettings = settings.embedded.localisationCommands |> function |Legacy (l, v) -> (if l.Length = 0 then Legacy ([],[]) else Legacy (l, v)) |_ -> Legacy ([],[])

        let settings = { settings with validation = { settings.validation with langs = STL STLLang.Default::settings.validation.langs }
                                       embedded = { settings.embedded with localisationCommands = locSettings }
                                       initialLookup = STLLookup()}

        let legacyLocDataTypes = settings.embedded.localisationCommands |> function | Legacy (c, v) -> (c, v)| _ -> ([], [])
        let processLocalisationFunction lookup = (createLocalisationFunctions STL.locStaticSettings createLocDynamicSettings legacyLocDataTypes  lookup) |> fst
        let validationLocalisationCommandFunction lookup = (createLocalisationFunctions STL.locStaticSettings createLocDynamicSettings legacyLocDataTypes  lookup) |> snd


        let rulesManagerSettings = {
            rulesSettings = settings.rules
            parseScope = scopeManager.ParseScope()
            allScopes = scopeManager.AllScopes
            anyScope = scopeManager.AnyScope
            changeScope = changeScope
            defaultContext = defaultContext
            defaultLang = STL STLLang.Default
            oneToOneScopesNames = oneToOneScopesNames
            loadConfigRulesHook = loadConfigRulesHook
            refreshConfigBeforeFirstTypesHook = refreshConfigBeforeFirstTypesHook
            refreshConfigAfterFirstTypesHook = refreshConfigAfterFirstTypesHook
            refreshConfigAfterVarDefHook = refreshConfigAfterVarDefHook
            processLocalisation = processLocalisationFunction
            validateLocalisation = validationLocalisationCommandFunction
        }

        let game = GameObject<STLComputedData, STLLookup>.CreateGame
                    (settings, "stellaris", scriptFolders, Compute.STL.computeSTLData,
                    Compute.STL.computeSTLDataUpdate,
                     (STLLocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
                     processLocalisationFunction,
                     validationLocalisationCommandFunction,
                     defaultContext,
                     noneContext,
                     Encoding.UTF8,
                     Encoding.GetEncoding(1252),
                     validationSettings,
                     STLGameFunctions.globalLocalisation,
                     STLGameFunctions.afterUpdateFile,
                     ".yml",
                     rulesManagerSettings)
                     afterInit

        let lookup = game.Lookup
        let resources = game.Resources
        let fileManager = game.FileManager
        let references = References<_>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))

        let useRules = settings.rules.IsSome
        let parseErrors() =
            resources.GetResources()
                |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None)
                |> List.choose (fun r -> r.result |> function |(Fail (result)) when r.validate -> Some (r.filepath, result.error, result.position)  |_ -> None)


        let validateTechnology (entities : (string * Node) list) =
            let tech = entities |> List.filter (fun (f, _) -> f.Contains("common/technology/"))
            tech

        member __.Lookup = lookup
        interface IGame<STLComputedData> with
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
            member __.StaticModifiers() = lookup.staticModifiers
            member __.UpdateFile shallow file text = game.UpdateFile shallow file text
            member __.AllEntities() = resources.AllEntities()
            member __.References() = References<_>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))
            member __.Complete pos file text = completion fileManager game.completionService game.InfoService game.ResourceManager pos file text
            member __.ScopesAtPos pos file text = scopesAtPos fileManager game.ResourceManager game.InfoService scopeManager.AnyScope pos file text
            member __.GoToType pos file text = getInfoAtPos fileManager game.ResourceManager game.InfoService game.LocalisationManager lookup (STL STLLang.English) pos file text
            member __.FindAllRefs pos file text = findAllRefsFromPos fileManager game.ResourceManager game.InfoService pos file text
            member __.InfoAtPos pos file text = game.InfoAtPos pos file text
            member __.ReplaceConfigRules rules = game.ReplaceConfigRules(({ ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})) //refreshRuleCaches game (Some { ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})
            member __.RefreshCaches() = updateModifiers(game); game.RefreshCaches()
            member __.RefreshLocalisationCaches() = game.LocalisationManager.UpdateProcessedLocalisation()
            member __.ForceRecompute() = resources.ForceRecompute()
            member __.Types() = game.Lookup.typeDefInfo
            member __.TypeDefs() = game.Lookup.typeDefs
            member __.GetPossibleCodeEdits file text = getPreTriggerPossible fileManager game.ResourceManager file text
            member __.GetCodeEdits file text = getFastTrigger fileManager game.ResourceManager file text
            member __.GetEventGraphData : GraphDataRequest = (fun files gameType depth -> graphEventDataForFiles references game.ResourceManager lookup files gameType depth)
            member __.GetEmbeddedMetadata() = getEmbeddedMetadata lookup game.LocalisationManager game.ResourceManager