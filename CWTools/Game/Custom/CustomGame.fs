namespace CWTools.Games.Custom
open CWTools.Localisation
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Games
open CWTools.Common
open CWTools.Utilities.Position
open CWTools.Utilities
open System.IO
open CWTools.Validation.Common.CommonValidation
open CWTools.Rules
open CWTools.Process.Scopes
open System.Text
open CWTools.Games.LanguageFeatures
open CWTools.Validation.LocalisationString
open CWTools.Process
open CWTools.Games.Helpers
open CWTools.Parser
open CWTools.Utilities.Utils
open FSharp.Collections.ParallelSeq
open System
open CWTools.Process.Localisation.ChangeLocScope
open CWTools.Process.Localisation

module CustomGameFunctions =
    type GameObject = GameObject<JominiComputedData, JominiLookup>
    let defaultContext =
        { Root = scopeManager.AnyScope; From = []; Scopes = [] }
    let noneContext =
        { Root = scopeManager.InvalidScope; From = []; Scopes = [scopeManager.InvalidScope] }

    let processLocalisationFunction (localisationSettings : LocalisationEmbeddedSettings) (lookup : Lookup) =
        let dataTypes = localisationSettings |> function | Jomini dts -> dts | _ -> { promotes = Map.empty; functions = Map.empty; dataTypes = Map.empty; dataTypeNames = Set.empty }
        let localisationCommandValidator() = createJominiLocalisationCommandValidator dataTypes
        let processLocalisation() = processJominiLocalisationBase (localisationCommandValidator()) defaultContext
        let validateLocalisationCommand() = validateJominiLocalisationCommandsBase (localisationCommandValidator())
        let eventtargets =
            lookup.savedEventTargets |> Seq.map (fun (a, _, c) -> (a, c)) |> List.ofSeq
                                     |> List.distinct
                                     |> List.fold (fun map (k, s) -> if Map.containsKey k map then Map.add k (s::map.[k]) map else Map.add k ([s]) map) Map.empty
        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
        processLocalisation() eventtargets definedvars

    let validateLocalisationCommandFunction (localisationSettings : LocalisationEmbeddedSettings) (lookup : Lookup) =
        let dataTypes = localisationSettings |> function | Jomini dts -> dts | _ -> { promotes = Map.empty; functions = Map.empty; dataTypes = Map.empty; dataTypeNames = Set.empty }
        let localisationCommandValidator() = createJominiLocalisationCommandValidator dataTypes
        let validateLocalisationCommand() = validateJominiLocalisationCommandsBase (localisationCommandValidator())
        let eventtargets =
            lookup.savedEventTargets |> Seq.map (fun (a, _, c) -> (a, c)) |> List.ofSeq
                                     |> List.distinct
                                     |> List.fold (fun map (k, s) -> if Map.containsKey k map then Map.add k (s::map.[k]) map else Map.add k ([s]) map) Map.empty
        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
        validateLocalisationCommand() eventtargets definedvars

    let globalLocalisation (game : GameObject) =
        let validateProcessedLocalisation : ((Lang * LocKeySet) list -> (Lang * Map<string,LocEntry>) list -> ValidationResult) = validateProcessedLocalisationBase []

        let locParseErrors = game.LocalisationManager.LocalisationAPIs() <&!&> (fun (b, api) -> if b then validateLocalisationSyntax api.Results else OK)
        let globalTypeLoc = game.ValidationManager.ValidateGlobalLocalisation()
        game.Lookup.proccessedLoc |> validateProcessedLocalisation game.LocalisationManager.taggedLocalisationKeys <&&>
        locParseErrors <&&>
        globalTypeLoc |> (function |Invalid (_, es) -> es |_ -> [])
    let updateScriptedLoc (game : GameObject) = ()

    let updateModifiers (game : GameObject) =
        game.Lookup.coreModifiers <- game.Settings.embedded.modifiers

    let addModifiersWithScopes (lookup : Lookup) =
        let modifierCategoryToScopesMap() = Map.empty
        let modifierOptions (modifier : ActualModifier) =
            let requiredScopes =
                modifierCategoryManager.SupportedScopes modifier.category
            {min = 0; max = 100; strictMin = true; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = requiredScopes; comparison = false; referenceDetails = None}
        lookup.coreModifiers
            |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(CWTools.Rules.RulesParser.specificField c.tag, ValueField (ValueType.Float (-1E+12M, 1E+12M))), modifierOptions c)))

    let updateScriptedTriggers (lookup : Lookup) (rules :RootRule list) (embeddedSettings : EmbeddedSettings) =
        let vanillaTriggers =
            let se = [] |> List.map (fun e -> e :> Effect)
            let vt = embeddedSettings.triggers  |> List.map (fun e -> e :> Effect)
            se @ vt
        let vanillaTriggerNames = vanillaTriggers |> List.map (fun vt -> StringResource.stringManager.InternIdentifierToken vt.Name) |> Set.ofList
        let effects =
            rules |> List.choose (function |AliasRule("trigger", r) -> Some r |_ -> None)
        let inline ruleToTrigger(r,o) =
            let name =
                match r with
                |LeafRule(SpecificField(SpecificValue n),_) when not (Set.contains n vanillaTriggerNames) -> Some (StringResource.stringManager.GetStringForID n.normal)
                |NodeRule(SpecificField(SpecificValue n),_) when not (Set.contains n vanillaTriggerNames) -> Some (StringResource.stringManager.GetStringForID n.normal)
                |_ -> None
            let effectType = if o.comparison then EffectType.ValueTrigger else EffectType.Trigger
            name |> Option.map (fun name -> DocEffect(name, o.requiredScopes, o.pushScope, effectType, o.description |> Option.defaultValue "", ""))
        let extraFromRules = (effects |> List.choose ruleToTrigger |> List.map (fun e -> e :> Effect))
        vanillaTriggers @ extraFromRules

    let updateScriptedEffects (lookup : Lookup) (rules :RootRule list) (embeddedSettings : EmbeddedSettings) =
        let vanillaEffects =
            let se = [] |> List.map (fun e -> e :> Effect)
            let ve = embeddedSettings.effects |> List.map (fun e -> e :> Effect)
            se @ ve
        vanillaEffects


    let addModifiersAsTypes (lookup : Lookup) (typesMap : Map<string,TypeDefInfo list>) =
        typesMap.Add("modifier", lookup.coreModifiers |> List.map (fun m -> createTypeDefInfo false m.tag range.Zero [] []))

    let addScriptFormulaLinks (lookup : Lookup) =
        match lookup.typeDefInfo |> Map.tryFind "script_value" with
        | Some vs ->
            let values = vs |> List.map (fun tdi -> tdi.id)
            values |> List.map (fun v -> Effect(v, [scopeManager.AnyScope], EffectType.ValueTrigger))
        | None -> []

    let addTriggerDocsScopes (lookup : Lookup) (rules : RootRule list) =
            let addRequiredScopesE (s : StringTokens) (o : Options) =
                let newScopes =
                    match o.requiredScopes with
                    |[] ->
                        lookup.effectsMap.TryFind((StringResource.stringManager.GetStringForID s.normal))
                            |> Option.map(fun se -> se.Scopes)
                            |> Option.defaultValue []
                    |x -> x
                let innerScope =
                    match o.pushScope with
                    | None ->
                        lookup.effectsMap.TryFind((StringResource.stringManager.GetStringForID s.normal))
                            |> Option.bind (function | :? DocEffect as se -> Some se | _ -> None)
                            |> Option.bind(fun se -> se.Target)
                    | x -> x
                { o with requiredScopes = newScopes; pushScope = innerScope}
            let addRequiredScopesT (s : StringTokens) (o : Options) =
                let newScopes =
                    match o.requiredScopes with
                    |[] ->
                        lookup.triggersMap.TryFind((StringResource.stringManager.GetStringForID s.normal))
                            |> Option.map(fun se -> se.Scopes)
                            |> Option.defaultValue []
                    |x -> x
                let innerScope =
                    match o.pushScope with
                    | None ->
                        lookup.triggersMap.TryFind((StringResource.stringManager.GetStringForID s.normal))
                            |> Option.bind (function | :? DocEffect as se -> Some se | _ -> None)
                            |> Option.bind(fun se -> se.Target)
                    | x -> x

                { o with requiredScopes = newScopes; pushScope = innerScope }
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
                    |x -> [x])



    let loadConfigRulesHook rules (lookup : Lookup) embedded =
        let ts = updateScriptedTriggers lookup rules embedded
        let es = updateScriptedEffects lookup rules embedded
        let ls = updateEventTargetLinks embedded
        lookup.allCoreLinks <- ts @ es @ ls
        // eprintfn "crh %A" ts
        addTriggerDocsScopes lookup (rules @ addModifiersWithScopes lookup)

    let refreshConfigBeforeFirstTypesHook (lookup : JominiLookup) (resources : IResourceAPI<JominiComputedData>) _ =
        let modifierEnums = { key = "modifiers"; values = lookup.coreModifiers |> List.map (fun m -> m.tag); description = "Modifiers" }
        lookup.ScriptedEffectKeys <- (resources.AllEntities() |> PSeq.map (fun struct(e, l) -> (l.Force().ScriptedEffectParams |> (Option.defaultWith (fun () -> CWTools.Games.Compute.EU4.getScriptedEffectParamsEntity e))))
                                        |> List.ofSeq |> List.collect id)
        let scriptedEffectParmas = { key = "scripted_effect_params"; description = "Scripted effect parameter"; values = lookup.ScriptedEffectKeys }
        let scriptedEffectParmasD =  { key = "scripted_effect_params_dollar"; description = "Scripted effect parameter"; values = lookup.ScriptedEffectKeys |> List.map (fun k -> sprintf "$%s$" k)}
        lookup.enumDefs <-
            lookup.enumDefs |> Map.add scriptedEffectParmas.key (scriptedEffectParmas.description, scriptedEffectParmas.values)
                            |> Map.add scriptedEffectParmasD.key (scriptedEffectParmasD.description, scriptedEffectParmasD.values)
                            |> Map.add modifierEnums.key (modifierEnums.description, modifierEnums.values)

    let refreshConfigAfterFirstTypesHook (lookup : Lookup) _ (embedded : EmbeddedSettings) =
        lookup.typeDefInfo <-
            (lookup.typeDefInfo)
            |> addModifiersAsTypes lookup
        let ts = updateScriptedTriggers lookup lookup.configRules embedded @ addScriptFormulaLinks lookup
        let es = updateScriptedEffects lookup lookup.configRules embedded
        let ls = updateEventTargetLinks embedded @ addDataEventTargetLinks lookup embedded true
        lookup.allCoreLinks <- ts @ es @ ls

    let refreshConfigAfterVarDefHook (lookup : Lookup) (resources : IResourceAPI<_>) (embedded : EmbeddedSettings) =
        let ts = updateScriptedTriggers lookup lookup.configRules embedded @ addScriptFormulaLinks lookup
        let es = updateScriptedEffects lookup lookup.configRules embedded
        let ls = updateEventTargetLinks embedded @ addDataEventTargetLinks lookup embedded false
        lookup.allCoreLinks <- ts @ es @ ls

    let afterInit (game : GameObject) =
        // updateScriptedTriggers()
        // updateScriptedEffects()
        // updateStaticodifiers()
        // updateScriptedLoc(game)
        // updateDefinedVariables()
        // updateProvinces(game)
        // updateCharacters(game)
        updateModifiers(game)

        // updateLegacyGovernments(game)
        // updateTechnologies()
        // game.LocalisationManager.UpdateAllLocalisation()
        // updateTypeDef game game.Settings.rules
        // game.LocalisationManager.UpdateAllLocalisation()


    let createEmbeddedSettings embeddedFiles cachedResourceData (configs : (string * string) list) =
        let scopeDefinitions =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
                            |> (fun f -> UtilityParser.initializeScopes f (Some []) )

            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifier_categories.cwt")
                |> (fun f -> UtilityParser.initializeModifierCategories f (Some []) )

        let irMods =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifiers.cwt")
                    |> Option.map (fun (fn, ft) -> IRParser.loadModifiers fn ft)
                    |> Option.defaultValue []

        let irLocCommands =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "localisation.cwt")
                    |> Option.map (fun (fn, ft) -> UtilityParser.loadLocCommands fn ft)
                    |> Option.defaultValue ([], [])
        let jominiLocDataTypes =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "data_types.log")
                    |> Option.map (fun (fn, ft) -> DataTypeParser.parseDataTypesStreamRes (new MemoryStream(System.Text.Encoding.GetEncoding(1252).GetBytes(ft))))
                    |> Option.defaultValue { DataTypeParser.JominiLocDataTypes.promotes = Map.empty; DataTypeParser.JominiLocDataTypes.functions = Map.empty; DataTypeParser.JominiLocDataTypes.dataTypes = Map.empty; DataTypeParser.JominiLocDataTypes.dataTypeNames = Set.empty }
        let irEventTargetLinks =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "links.cwt")
                    |> Option.map (fun (fn, ft) -> UtilityParser.loadEventTargetLinks scopeManager.AnyScope (scopeManager.ParseScope()) scopeManager.AllScopes fn ft)
                    |> Option.defaultValue (CWTools.Process.Scopes.IR.scopedEffects |> List.map SimpleLink)

        let irEffects =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "effects.log")
                    |> Option.bind (fun (fn, ft) -> JominiParser.parseEffectStreamRes (new MemoryStream(System.Text.Encoding.GetEncoding(1252).GetBytes(ft))))
                    |> Option.map (JominiParser.processEffects (scopeManager.ParseScopes))
                    |> Option.defaultWith (fun () -> eprintfn "effects.log was not found in ir config"; ([]))

        let irTriggers =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "triggers.log")
                    |> Option.bind (fun (fn, ft) -> JominiParser.parseTriggerStreamRes (new MemoryStream(System.Text.Encoding.GetEncoding(1252).GetBytes(ft))))
                    |> Option.map (JominiParser.processTriggers scopeManager.ParseScopes)
                    |> Option.defaultWith (fun () -> eprintfn "triggers.log was not found in ir config"; ([]))


        {
            triggers = irTriggers
            effects = irEffects
            modifiers = irMods
            embeddedFiles = embeddedFiles
            cachedResourceData = cachedResourceData
            localisationCommands = Jomini jominiLocDataTypes
            eventTargetLinks = irEventTargetLinks
            cachedRuleMetadata = None
        }

type CustomSettings = GameSetupSettings<JominiLookup>
open CustomGameFunctions
open CWTools.Localisation.Custom
type CustomGame(setupSettings : CustomSettings, gameFolderName : string) =
    let validationSettings = {
        validators = [ validateMixedBlocks, "mixed"; validateIfWithNoEffect, "ifnoeffect"]
        experimentalValidators = []
        heavyExperimentalValidators = []
        experimental = false
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
        initialLookup = JominiLookup()
        maxFileSize = setupSettings.maxFileSize

    }
    do if scopeManager.Initialized |> not then eprintfn "%A has no scopes" (settings.rootDirectories |> List.head) else ()
    let locCommands() = []
    let settings = {
        settings with
            initialLookup = JominiLookup()
            }
    let changeScope = Scopes.createJominiChangeScope CWTools.Process.Scopes.IR.oneToOneScopes (Scopes.complexVarPrefixFun "variable:from:" "variable:")

    let rulesManagerSettings = {
        rulesSettings = settings.rules
        parseScope = scopeManager.ParseScope()
        allScopes = scopeManager.AllScopes
        anyScope = scopeManager.AnyScope
        changeScope = changeScope
        defaultContext = defaultContext
        defaultLang = Custom CustomLang.English
        oneToOneScopesNames = CWTools.Process.Scopes.IR.oneToOneScopesNames
        loadConfigRulesHook = loadConfigRulesHook
        refreshConfigBeforeFirstTypesHook = refreshConfigBeforeFirstTypesHook
        refreshConfigAfterFirstTypesHook = refreshConfigAfterFirstTypesHook
        refreshConfigAfterVarDefHook = refreshConfigAfterVarDefHook
        processLocalisation = CustomGameFunctions.processLocalisationFunction (settings.embedded.localisationCommands)
        validateLocalisation = CustomGameFunctions.validateLocalisationCommandFunction (settings.embedded.localisationCommands)
    }
    let scriptFolders = []

    let game = GameObject<JominiComputedData, JominiLookup>.CreateGame
                ((settings, gameFolderName, scriptFolders, Compute.Jomini.computeJominiData,
                    Compute.Jomini.computeJominiDataUpdate,
                     (CustomLocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
                     CustomGameFunctions.processLocalisationFunction (settings.embedded.localisationCommands),
                     CustomGameFunctions.validateLocalisationCommandFunction (settings.embedded.localisationCommands),
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

    interface IGame<JominiComputedData> with
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
        member __.GoToType pos file text = getInfoAtPos fileManager game.ResourceManager game.InfoService game.LocalisationManager lookup (Custom CustomLang.English) pos file text
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
        member __.GetEmbeddedMetadata() = getEmbeddedMetadata lookup game.LocalisationManager game.ResourceManager