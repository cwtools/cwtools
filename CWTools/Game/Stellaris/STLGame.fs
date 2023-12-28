namespace CWTools.Games.Stellaris

open CWTools.Game
open CWTools.Parser
open CWTools.Process
open CWTools.Utilities.Position
open CWTools.Utilities.Utils2
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
open CWTools.Rules
open CWTools.Validation.Common.CommonValidation
open CWTools.Process.Scopes.Scopes
open System.Text
open CWTools.Games.LanguageFeatures
open CWTools.Validation.LocalisationString
open CWTools.Games.Helpers
open System.IO
open CWTools.Process.Localisation

module STLGameFunctions =
    type GameObject = GameObject<STLComputedData, STLLookup>

    let createLocDynamicSettings (lookup: Lookup) =
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target"
             |> Option.defaultValue []
             |> List.map fst)
            @ (lookup.varDefInfo.TryFind "global_event_target"
               |> Option.defaultValue []
               |> List.map fst)

        let definedVariables =
            (lookup.varDefInfo.TryFind "variable"
             |> Option.defaultValue []
             |> Seq.map fst
             |> LowerCaseStringSet)

        { scriptedLocCommands = lookup.scriptedLoc |> List.map (fun s -> s, [ scopeManager.AnyScope ])
          eventTargets = eventtargets |> List.map (fun s -> s, scopeManager.AnyScope)
          setVariables = definedVariables }

    let updateScriptedTriggers (game: GameObject) =
        let vanillaTriggers =
            let se = game.Lookup.triggers
            let vt = game.Settings.embedded.triggers |> List.map (fun e -> e :> Effect)
            se @ vt

        let sts, ts = STLLookup.updateScriptedTriggers game.Resources vanillaTriggers
        game.Lookup.onlyScriptedTriggers <- sts
        sts @ ts

    let updateScriptedEffects (game: GameObject) =
        let vanillaEffects =
            let se = game.Lookup.effects
            let ve = game.Settings.embedded.effects |> List.map (fun e -> e :> Effect)
            se @ ve

        let ses, es =
            STLLookup.updateScriptedEffects game.Resources vanillaEffects game.Lookup.triggers

        game.Lookup.onlyScriptedEffects <- ses
        ses @ es

    let updateStaticodifiers (game: GameObject) =
        let rawModifiers =
            game.Resources.AllEntities()
            |> List.choose (function
                | struct (f, _) when f.filepath.Contains("static_modifiers") -> Some f.entity
                | _ -> None)
            |> List.collect (fun n -> n.Children)

        let modifiers2 =
            System.Linq.Enumerable.ToLookup(game.Settings.embedded.modifiers, (fun x -> x.tag), (fun x -> x.category))

        let newModifiers =
            rawModifiers
            |> List.map (fun e -> STLProcess.getStaticModifierCategory modifiers2 e)

        game.Lookup.staticModifiers <- newModifiers

    let updateScriptedLoc (game: GameObject) =
        let rawLocs =
            game.Resources.AllEntities()
            |> List.choose (function
                | struct (f, _) when f.filepath.Contains("scripted_loc") -> Some f.entity
                | _ -> None)
            |> List.collect (fun n -> n.Children)
            |> List.map (fun l -> l.TagText "name")

        game.Lookup.embeddedScriptedLoc <-
            game.Settings.embedded.cachedRuleMetadata
            |> Option.map (fun crm -> crm.scriptedLoc)
            |> Option.defaultValue []

        game.Lookup.scriptedLoc <- rawLocs

    let afterUpdateFile (game: GameObject<STLComputedData, STLLookup>) (filepath: string) =
        match filepath with
        | x when x.Contains("scripted_triggers") -> updateScriptedTriggers (game) |> ignore
        | x when x.Contains("scripted_effects") -> updateScriptedEffects (game) |> ignore
        | x when x.Contains("scripted_loc") -> updateScriptedLoc (game)
        | x when x.Contains("static_modifiers") -> updateStaticodifiers (game)
        | _ -> ()

    let globalLocalisation (game: GameObject) =
        let locfiles =
            game.Resources.GetResources()
            |> List.choose (function
                | FileWithContentResource(_, e) -> Some e
                | _ -> None)
            |> List.filter (fun f -> f.overwrite <> Overwritten && f.extension = ".yml" && f.validate)
            |> List.map (fun f -> f.filepath)

        let locFileValidation = validateLocalisationFiles locfiles

        let locParseErrors =
            game.LocalisationManager.LocalisationAPIs()
            <&!&> (fun (b, api) -> if b then validateLocalisationSyntax api.Results else OK)

        let globalTypeLoc = game.ValidationManager.ValidateGlobalLocalisation()

        game.Lookup.proccessedLoc
        |> validateProcessedLocalisation game.LocalisationManager.taggedLocalisationKeys
        <&&> locFileValidation
        <&&> globalTypeLoc
        <&&> locParseErrors
        |> (function
        | Invalid(_, es) -> es
        | _ -> [])

    let addModifiersFromCoreAndTypes
        (lookup: Lookup)
        (embeddedSettings: EmbeddedSettings)
        (resources: IResourceAPI<_>)
        =
        let typeGeneratedModifiers =
            RulesHelpers.generateModifiersFromTypes lookup.typeDefs lookup.typeDefInfo

        let current = embeddedSettings.modifiers @ typeGeneratedModifiers
        lookup.coreModifiers <- addGeneratedModifiers current (EntitySet(resources.AllEntities()))

    // let updateModifiers(game : GameObject) =
    // game.Lookup.coreModifiers <- addGeneratedModifiers game.Settings.embedded.modifiers (EntitySet (game.Resources.AllEntities()))

    let updateTechnologies (game: GameObject) =
        game.Lookup.technologies <- getTechnologies (EntitySet(game.Resources.AllEntities()))

    let addModifiersWithScopes (lookup: Lookup) =
        let modifierOptions (modifier: ActualModifier) =
            let requiredScopes = modifierCategoryManager.SupportedScopes modifier.category

            { Options.DefaultOptions with
                requiredScopes = requiredScopes }

        let processField =
            RulesParser.processTagAsField (scopeManager.ParseScope()) scopeManager.AnyScope scopeManager.ScopeGroups

        (lookup.coreModifiers
         |> List.map (fun c ->
             AliasRule(
                 "modifier",
                 NewRule(LeafRule(processField c.tag, ValueField(ValueType.Float(-1E+12M, 1E+12M))), modifierOptions c)
             )))
        @ RulesHelpers.generateModifierRulesFromTypes lookup.typeDefs

    let addTriggerDocsScopes (lookup: Lookup) (rules: RootRule list) =
        let scriptedOptions (scripted: string) (effect: ScriptedEffect) =
            { Options.DefaultOptions with
                description = Some effect.Comments
                requiredScopes = effect.Scopes
                typeHint = Some(scripted, true) }

        let getAllScriptedEffects =
            lookup.onlyScriptedEffects
            |> List.choose (function
                | :? ScriptedEffect as se -> Some se
                | _ -> None)
            |> List.map (fun se ->
                AliasRule(
                    "effect",
                    NewRule(
                        LeafRule(CWTools.Rules.RulesParser.specificFieldFromId se.Name, ValueField(ValueType.Bool)),
                        scriptedOptions "scripted_effect" se
                    )
                ))

        let getAllScriptedTriggers =
            lookup.onlyScriptedTriggers
            |> List.choose (function
                | :? ScriptedEffect as se -> Some se
                | _ -> None)
            |> List.map (fun se ->
                AliasRule(
                    "trigger",
                    NewRule(
                        LeafRule(CWTools.Rules.RulesParser.specificFieldFromId se.Name, ValueField(ValueType.Bool)),
                        scriptedOptions "scripted_trigger" se
                    )
                ))

        let addRequiredScopesE (s: StringTokens) (o: Options) =
            let newScopes =
                match o.requiredScopes with
                | [] ->
                    lookup.effectsMap.TryFind(StringResource.stringManager.GetStringForID s.normal)
                    |> Option.map (fun se -> se.Scopes)
                    |> Option.defaultValue []
                | x -> x

            { o with requiredScopes = newScopes }

        let addRequiredScopesT (s: StringTokens) (o: Options) =
            let newScopes =
                match o.requiredScopes with
                | [] ->
                    lookup.triggersMap.TryFind(StringResource.stringManager.GetStringForID s.normal)
                    |> Option.map (fun se -> se.Scopes)
                    |> Option.defaultValue []
                | x -> x

            { o with requiredScopes = newScopes }

        rules
        |> List.collect (function
            | AliasRule("effect", (LeafRule(SpecificField(SpecificValue s), r), o)) ->
                [ AliasRule("effect", (LeafRule(SpecificField(SpecificValue s), r), addRequiredScopesE s o)) ]
            | AliasRule("trigger", (LeafRule(SpecificField(SpecificValue s), r), o)) ->
                [ AliasRule("trigger", (LeafRule(SpecificField(SpecificValue s), r), addRequiredScopesT s o)) ]
            | AliasRule("effect", (NodeRule(SpecificField(SpecificValue s), r), o)) ->
                [ AliasRule("effect", (NodeRule(SpecificField(SpecificValue s), r), addRequiredScopesE s o)) ]
            | AliasRule("trigger", (NodeRule(SpecificField(SpecificValue s), r), o)) ->
                [ AliasRule("trigger", (NodeRule(SpecificField(SpecificValue s), r), addRequiredScopesT s o)) ]
            | AliasRule("effect", (LeafValueRule(SpecificField(SpecificValue s)), o)) ->
                [ AliasRule("effect", (LeafValueRule(SpecificField(SpecificValue s)), addRequiredScopesE s o)) ]
            | AliasRule("trigger", (LeafValueRule(SpecificField(SpecificValue s)), o)) ->
                [ AliasRule("trigger", (LeafValueRule(SpecificField(SpecificValue s)), addRequiredScopesT s o)) ]
            | AliasRule("effect", (LeafRule(TypeField(TypeType.Simple "scripted_effect"), o), _)) ->
                getAllScriptedEffects
            | AliasRule("trigger", (LeafRule(TypeField(TypeType.Simple "scripted_trigger"), o), _)) ->
                getAllScriptedTriggers
            | x -> [ x ])

    let addValueTriggersToTriggers rules (lookup: Lookup) =
        let triggers =
            rules
            |> List.choose (function
                | AliasRule("trigger", (LeafRule(SpecificField(SpecificValue n), _), o)) ->
                    Some(StringResource.stringManager.GetStringForID n.normal, o)
                | _ -> None)
            |> Map.ofList

        let inline triggerAugment (trigger: Effect) =
            match trigger, triggers |> Map.tryFind (trigger.Name.GetString()) with
            | :? DocEffect as doc, Some options when options.comparison ->
                [ DocEffect(
                      "trigger:" + doc.Name.GetString(),
                      doc.Scopes,
                      doc.Target,
                      EffectType.ValueTrigger,
                      doc.Desc,
                      doc.Usage,
                      doc.RefHint
                  )
                  :> Effect
                  doc :> Effect ]
            | trigger, _ -> [ trigger ]

        lookup.triggers |> List.collect triggerAugment
    //        lookup.triggers |> List.

    let loadConfigRulesHook rules (lookup: Lookup) embedded =
        let triggersWithValueTriggers = addValueTriggersToTriggers rules lookup
        lookup.allCoreLinks <- triggersWithValueTriggers @ lookup.effects @ updateEventTargetLinks embedded //@ addDataEventTargetLinks lookup embedded
        lookup.coreModifiers <- embedded.modifiers
        let rulesWithMod = rules @ addModifiersWithScopes (lookup)
        let rulesWithEmbeddedScopes = addTriggerDocsScopes lookup rulesWithMod
        rulesWithEmbeddedScopes



    let addModifiersAsTypes (lookup: Lookup) (typesMap: Map<string, TypeDefInfo list>) =
        typesMap.Add(
            "modifier",
            lookup.coreModifiers
            |> List.map (fun m -> createTypeDefInfo false m.tag range.Zero [] [])
        )

    let refreshConfigAfterFirstTypesHook
        (lookup: Lookup)
        (resources: IResourceAPI<_>)
        (embeddedSettings: EmbeddedSettings)
        =
        addModifiersFromCoreAndTypes lookup embeddedSettings resources
        lookup.typeDefInfo <- lookup.typeDefInfo |> addModifiersAsTypes lookup

        lookup.allCoreLinks <-
            lookup.triggers
            @ lookup.effects
            @ (updateEventTargetLinks embeddedSettings
               @ addDataEventTargetLinks lookup embeddedSettings false)

    let refreshConfigAfterVarDefHook
        (lookup: Lookup)
        (resources: IResourceAPI<_>)
        (embeddedSettings: EmbeddedSettings)
        =
        lookup.allCoreLinks <-
            lookup.triggers
            @ lookup.effects
            @ (updateEventTargetLinks embeddedSettings
               @ addDataEventTargetLinks lookup embeddedSettings false)

    let afterInit (game: GameObject) =
        let ts = updateScriptedTriggers (game)
        game.Lookup.allCoreLinks <- ts @ game.Lookup.effects @ game.Lookup.eventTargetLinks
        let es = updateScriptedEffects (game)
        game.Lookup.allCoreLinks <- game.Lookup.triggers @ es @ game.Lookup.eventTargetLinks
        updateStaticodifiers (game)
        updateScriptedLoc (game)
        // updateModifiers(game)
        updateTechnologies (game)

    let createEmbeddedSettings embeddedFiles cachedResourceData (configs: (string * string) list) cachedRuleMetadata =
        let scopeDefinitions =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
            |> (fun f -> UtilityParser.initializeScopes f (Some defaultScopeInputs))

        configs
        |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifier_categories.cwt")
        |> (fun f -> UtilityParser.initializeModifierCategories f (Some(defaultModifiersInputs ())))


        let triggers, effects =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "trigger_docs.log")
            |> Option.map (fun (fn, ft) -> DocsParser.parseDocsFile fn)
            |> Option.bind (function
                | FParsec.CharParsers.ParserResult.Success(p, _, _) ->
                    Some(DocsParser.processDocs scopeManager.ParseScopes p)
                | FParsec.CharParsers.ParserResult.Failure(e, _, _) ->
                    eprintfn "%A" e
                    None)
            |> Option.defaultWith (fun () ->
                Utils.logError "trigger_docs.log was not found in stellaris config"
                ([], []))


        let stlSetupModifiers =
            if configs |> List.exists (fun (fn, _) -> Path.GetFileName fn = "modifiers.log") then
                configs
                |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifiers.log")
                |> Option.map (fun (fn, ft) -> StellarisModifierParser.parseLogsFile fn)
                |> Option.bind (function
                    | FParsec.CharParsers.ParserResult.Success(p, _, _) -> Some(StellarisModifierParser.processLogs p)
                    | FParsec.CharParsers.ParserResult.Failure(e, _, _) -> None)
                |> Option.defaultWith (fun () ->
                    Utils.logError "modifiers.log was not found in stellaris config"
                    [])
            else
                configs
                |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "setup.log")
                |> Option.map (fun (fn, ft) -> SetupLogParser.parseLogsFile fn)
                |> Option.bind (function
                    | FParsec.CharParsers.ParserResult.Success(p, _, _) -> Some(SetupLogParser.processLogs p)
                    | FParsec.CharParsers.ParserResult.Failure(e, _, _) -> None)
                |> Option.defaultWith (fun () ->
                    Utils.logError "setup.log was not found in stellaris config"
                    [])

        let stlRulesMods =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifiers.cwt")
            |> Option.map (fun (fn, ft) -> UtilityParser.loadModifiers fn ft)
            |> Option.defaultValue []

        let stlLocCommands =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "localisation.cwt")
            |> Option.map (fun (fn, ft) -> UtilityParser.loadLocCommands fn ft)
            |> Option.defaultValue ([], [], [])

        let stlEventTargetLinks =
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

        let featureSettings =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "settings.cwt")
            |> Option.bind (fun (fn, ft) -> UtilityParser.loadSettingsFile fn ft)
            |> Option.defaultValue CWTools.Parser.UtilityParser.FeatureSettings.Default

        { triggers = triggers
          effects = effects
          modifiers = stlSetupModifiers @ stlRulesMods
          embeddedFiles = embeddedFiles
          cachedResourceData = cachedResourceData
          localisationCommands = Legacy stlLocCommands
          eventTargetLinks = stlEventTargetLinks
          cachedRuleMetadata = cachedRuleMetadata
          featureSettings = featureSettings }


type StellarisSettings = GameSetupSettings<STLLookup>

open STLGameFunctions

type STLGame(setupSettings: StellarisSettings) =
    let validationSettings =
        { validators =
            [ validateVariables, "var"
              valTechnology, "tech"
              validateShipDesigns, "designs"
              validateIfElse, "ifelse2"
              validatePlanetKillers, "pk"
              validateRedundantANDWithNOR, "AND"
              valMegastructureGraphics, "megastructure"
              valPlanetClassGraphics, "pcg"
              validateDeprecatedSetName, "setname"
              validateEvents, "eventsSimple"
              validateNOTMultiple, "not"
              validatePreTriggers, "pre"
              validateIfWithNoEffect, "ifnoeffect" ]
          experimentalValidators = [ valSectionGraphics, "sections"; valComponentGraphics, "component" ]
          heavyExperimentalValidators = [ getEventChains, "event chains" ]
          experimental = setupSettings.validation.experimental
          fileValidators = [ validateTechnologies, "tech2" ]
          lookupValidators = (validateEconomicCatAIBudget, "aibudget") :: commonValidationRules
          lookupFileValidators =
            [ valScriptedEffectParams, "scripted_effects"
              valScriptValueParams, "script_values" ]
          useRules =
            setupSettings.rules
            |> Option.map (fun o -> o.validateRules)
            |> Option.defaultValue false
          debugRulesOnly =
            setupSettings.rules
            |> Option.map (fun o -> o.debugRulesOnly)
            |> Option.defaultValue false
          localisationValidators = [ valTechLocs; valPolicies ]

        }

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
          initialLookup = STLLookup()
          maxFileSize = setupSettings.maxFileSize
          enableInlineScripts = true }

    do
        if scopeManager.Initialized |> not then
            Utils.logError (sprintf "%A has no scopes" (settings.rootDirectories |> List.head))
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
            initialLookup = STLLookup() }

    let legacyLocDataTypes =
        settings.embedded.localisationCommands
        |> function
            | Legacy(c, v, links) -> (c, v, (links, settings.embedded.eventTargetLinks))
            | _ -> ([], [], ([], []))

    let processLocalisationFunction lookup =
        (createLocalisationFunctions STL.locStaticSettings createLocDynamicSettings legacyLocDataTypes lookup)


    let rulesManagerSettings =
        { rulesSettings = settings.rules
          useFormulas = false
          stellarisScopeTriggers = true
          parseScope = scopeManager.ParseScope()
          allScopes = scopeManager.AllScopes
          anyScope = scopeManager.AnyScope
          scopeGroups = scopeManager.ScopeGroups
          changeScope = changeScope
          defaultContext = defaultContext
          defaultLang = STL STLLang.Default
          oneToOneScopesNames = oneToOneScopesNames
          loadConfigRulesHook = loadConfigRulesHook
          refreshConfigBeforeFirstTypesHook = Hooks.refreshConfigBeforeFirstTypesHook
          refreshConfigAfterFirstTypesHook = refreshConfigAfterFirstTypesHook
          refreshConfigAfterVarDefHook = refreshConfigAfterVarDefHook
          locFunctions = processLocalisationFunction }

    let game =
        GameObject<STLComputedData, STLLookup>.CreateGame
            (settings,
             "stellaris",
             scriptFolders,
             Compute.STL.computeSTLData,
             Compute.STL.computeSTLDataUpdate,
             (STLLocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
             processLocalisationFunction,
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

    let references =
        References<_>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))

    let useRules = settings.rules.IsSome

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


    let validateTechnology (entities: (string * Node) list) =
        let tech = entities |> List.filter (fun (f, _) -> f.Contains("common/technology/"))
        tech

    member __.Lookup = lookup

    interface IGame<STLComputedData> with
        member __.ParserErrors() = parseErrors ()

        member __.ValidationErrors() =
            let s, d = game.ValidationManager.Validate(false, resources.ValidatableEntities()) in s @ d

        member __.LocalisationErrors(force: bool, forceGlobal: bool) =
            getLocalisationErrors game globalLocalisation (force, forceGlobal)

        member __.Folders() = fileManager.AllFolders()
        member __.AllFiles() = resources.GetResources()

        member __.AllLoadedLocalisation() =
            game.LocalisationManager.LocalisationFileNames()

        member __.ScriptedTriggers() = lookup.triggers
        member __.ScriptedEffects() = lookup.effects
        member __.StaticModifiers() = lookup.staticModifiers
        member __.UpdateFile shallow file text = game.UpdateFile shallow file text
        member __.AllEntities() = resources.AllEntities()

        member __.References() =
            References<_>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))

        member __.Complete pos file text =
            completion fileManager game.completionService game.InfoService game.ResourceManager pos file text

        member __.ScopesAtPos pos file text =
            scopesAtPos fileManager game.ResourceManager game.InfoService scopeManager.AnyScope pos file text

        member __.GoToType pos file text =
            getInfoAtPos
                fileManager
                game.ResourceManager
                game.InfoService
                game.LocalisationManager
                lookup
                (STL STLLang.English)
                pos
                file
                text

        member __.FindAllRefs pos file text =
            findAllRefsFromPos fileManager game.ResourceManager game.InfoService pos file text

        member __.InfoAtPos pos file text = game.InfoAtPos pos file text

        member __.ReplaceConfigRules rules =
            game.ReplaceConfigRules
                { ruleFiles = rules
                  validateRules = true
                  debugRulesOnly = false
                  debugMode = false } //refreshRuleCaches game (Some { ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})

        member __.RefreshCaches() = game.RefreshCaches()

        member __.RefreshLocalisationCaches() =
            game.LocalisationManager.UpdateProcessedLocalisation()

        member __.ForceRecompute() = resources.ForceRecompute()
        member __.Types() = game.Lookup.typeDefInfo
        member __.TypeDefs() = game.Lookup.typeDefs

        member __.GetPossibleCodeEdits file text =
            getPreTriggerPossible fileManager game.ResourceManager file text

        member __.GetCodeEdits file text =
            getFastTrigger fileManager game.ResourceManager file text

        member __.GetEventGraphData: GraphDataRequest =
            (fun files gameType depth ->
                graphEventDataForFiles references game.ResourceManager lookup files gameType depth)

        member __.GetEmbeddedMetadata() =
            getEmbeddedMetadata lookup game.LocalisationManager game.ResourceManager
