namespace CWTools.Games.IR
open CWTools.Localisation
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Games
open CWTools.Common
open CWTools.Localisation.IR
open CWTools.Utilities.Position
open CWTools.Utilities
open System.IO
open CWTools.Validation.Common.CommonValidation
// open CWTools.Rules.Rules
open CWTools.Rules
open CWTools.Common.IRConstants
open CWTools.Process.Scopes.IR
open CWTools.Process.Scopes.Scopes
open CWTools.Validation.IR
open System.Text
open CWTools.Games.LanguageFeatures
open CWTools.Validation.IR.IRLocalisationString
open CWTools.Validation.LocalisationString
open CWTools.Process
open System
open CWTools.Games.Helpers
open CWTools.Parser
open CWTools.Common.NewScope

module IRGameFunctions =
    type GameObject = GameObject<Modifier, IRComputedData, IRLookup>
    let processLocalisationFunction (localisationCommands : ((string * Scope list) list)) (lookup : Lookup<Modifier>) =
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst)
        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
        processLocalisation() localisationCommands eventtargets lookup.scriptedLoc definedvars

    let validateLocalisationCommandFunction (localisationCommands : ((string * Scope list) list)) (lookup : Lookup<Modifier>) =
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst)
        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
        validateLocalisationCommand() localisationCommands eventtargets lookup.scriptedLoc definedvars

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
    let updateScriptedLoc (game : GameObject) = ()
        // let rawLocs =
        //     game.Resources.AllEntities()
        //     |> List.choose (function |struct (f, _) when f.filepath.Contains("customizable_localisation") -> Some (f.entity) |_ -> None)
        //     |> List.collect (fun n -> n.Children)
        //     |> List.map (fun l -> l.TagText "name")
        // game.Lookup.scriptedLoc <- rawLocs

    let updateModifiers (game : GameObject) =
        game.Lookup.coreModifiers <- game.Settings.embedded.modifiers

    let addModifiersWithScopes (lookup : Lookup<_>) =
        let modifierOptions (modifier : Modifier) =
            let requiredScopes =
                modifier.categories |> List.choose (fun c -> modifierCategoryToScopesMap().TryFind c)
                                    |> List.map Set.ofList
                                    |> (fun l -> if List.isEmpty l then [] else l |> List.reduce (Set.intersect) |> Set.toList)
            {min = 0; max = 100; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = requiredScopes; comparison = false}
        lookup.coreModifiers
            |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(CWTools.Rules.RulesParser.specificField c.tag, ValueField (ValueType.Float (-1E+12, 1E+12))), modifierOptions c)))

    let updateScriptedTriggers (lookup : IRLookup) (rules :RootRule<Scope> list) (embeddedSettings : EmbeddedSettings<_,_>) =
        let vanillaTriggers =
            let se = scopedEffects |> List.map (fun e -> e :> Effect)
            let vt = embeddedSettings.triggers  |> List.map (fun e -> e :> Effect)
            se @ vt
        let vanillaTriggerNames = vanillaTriggers |> List.map (fun vt -> StringResource.stringManager.InternIdentifierToken vt.Name)
        let effects =
            rules |> List.choose (function |AliasRule("trigger", r) -> Some r |_ -> None)
        let ruleToTrigger(r,o) =
            let name =
                match r with
                |LeafRule(SpecificField(SpecificValue n),_) when not (List.contains n vanillaTriggerNames) -> Some (StringResource.stringManager.GetStringForID n.normal)
                |NodeRule(SpecificField(SpecificValue n),_) when not (List.contains n vanillaTriggerNames) -> Some (StringResource.stringManager.GetStringForID n.normal)
                |_ -> None
            let effectType = if o.comparison then EffectType.ValueTrigger else EffectType.Trigger
            name |> Option.map (fun name -> DocEffect(name, o.requiredScopes, o.pushScope, effectType, o.description |> Option.defaultValue "", ""))
        // let simpleEventTargetLinks = embeddedSettings.eventTargetLinks |> List.choose (function | SimpleLink l -> Some (l :> Effect) | _ -> None)
        let extraFromRules = (effects |> List.choose ruleToTrigger |> List.map (fun e -> e :> Effect))
        // let sts, ts = STLLookup.updateScriptedTriggers game.Resources vanillaTriggers
        // game.Lookup.triggers <- sts @ ts
        // game.Lookup.onlyScriptedTriggers <- sts
        vanillaTriggers @ extraFromRules

    let updateScriptedEffects (lookup : IRLookup) (rules :RootRule<Scope> list) (embeddedSettings : EmbeddedSettings<_,_>) =
        let vanillaEffects =
            let se = scopedEffects |> List.map (fun e -> e :> Effect)
            let ve = embeddedSettings.effects |> List.map (fun e -> e :> Effect)
            se @ ve
        // let ses, es = STLLookup.updateScriptedEffects game.Resources vanillaEffects (game.Lookup.triggers)
        // game.Lookup.effects <- ses @ es
        // game.Lookup.onlyScriptedEffects <- ses
        vanillaEffects


    // let updateScriptedEffects (lookup : Lookup<_,_>) (rules :RootRule<Scope> list) (embeddedSettings : EmbeddedSettings<_,_>) =
    //     let effects =
    //         rules |> List.choose (function |AliasRule("effect", r) -> Some r |_ -> None)
    //     let ruleToEffect(r,o) =
    //         let name =
    //             match r with
    //             |LeafRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
    //             |NodeRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
    //             |_ -> ""
    //         DocEffect(name, o.requiredScopes, EffectType.Effect, o.description |> Option.defaultValue "", "")
    //     // let simpleEventTargetLinks = embeddedSettings.eventTargetLinks |> List.choose (function | SimpleLink l -> Some (l :> Effect) | _ -> None)
    //     (effects |> List.map ruleToEffect  |> List.map (fun e -> e :> Effect))

    // let updateScriptedTriggers (lookup : Lookup<_,_>) (rules :RootRule<Scope> list) (embeddedSettings : EmbeddedSettings<_,_>) =
    //     let effects =
    //         rules |> List.choose (function |AliasRule("trigger", r) -> Some r |_ -> None)
    //     let ruleToTrigger(r,o) =
    //         let name =
    //             match r with
    //             |LeafRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
    //             |NodeRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
    //             |_ -> ""
    //         DocEffect(name, o.requiredScopes, EffectType.Trigger, o.description |> Option.defaultValue "", "")
    //     let test = ScopedEffect("test_value_trigger", allScopes, Scope.NoneScope, EffectType.Trigger, "", "", false, true) :> Effect
    //     // let simpleEventTargetLinks = embeddedSettings.eventTargetLinks |> List.choose (function | SimpleLink l -> Some (l :> Effect) | _ -> None)
    //     test::(effects |> List.map ruleToTrigger |> List.map (fun e -> e :> Effect))


    let addModifiersAsTypes (lookup : Lookup<_>) (typesMap : Map<string,TypeDefInfo list>) =
        // let createType (modifier : Modifier) =
        typesMap.Add("modifier", lookup.coreModifiers |> List.map (fun m -> createTypeDefInfo false m.tag range.Zero [] []))

    let updateProvinces (game : GameObject) =
        let provinceFile =
            game.Resources.GetResources()
            |> List.choose (function |FileWithContentResource (_, e) -> Some e |_ -> None)
            |> List.tryFind (fun f -> f.overwrite <> Overwritten && Path.GetFileName(f.filepath) = "definition.csv")
        match provinceFile with
        |None -> ()
        |Some pf ->
            let lines = pf.filetext.Split(([|"\r\n"; "\r"; "\n"|]), StringSplitOptions.None)
            let provinces = lines |> Array.choose (fun l -> if l.StartsWith("#", StringComparison.OrdinalIgnoreCase) then None else l.Split([|';'|], 2, StringSplitOptions.RemoveEmptyEntries) |> Array.tryHead) |> List.ofArray
            game.Lookup.IRprovinces <- provinces

    let updateCharacters (game : GameObject) =
        let characterFile =
            game.Resources.GetResources()
            |> List.choose (function |FileWithContentResource (_, e) -> Some e |_ -> None)
            |> List.tryFind (fun f -> f.overwrite <> Overwritten && Path.GetFileName(f.filepath) = "character_setup.csv")
        match characterFile with
        |None -> ()
        |Some pf ->
            let lines = pf.filetext.Split(([|"\r\n"; "\r"; "\n"|]), StringSplitOptions.None)
            let chars = lines |> Array.choose (fun l -> if l.StartsWith("#", StringComparison.OrdinalIgnoreCase) then None else l.Split([|','|], 3, StringSplitOptions.RemoveEmptyEntries) |> (fun a -> if a.Length > 1 then a |> Array.skip 1 |> Array.tryHead else None)) |> List.ofArray
            game.Lookup.IRcharacters <- chars

    let addScriptFormulaLinks (lookup : IRLookup) =
        match lookup.typeDefInfo |> Map.tryFind "script_value" with
        | Some vs ->
            let values = vs |> List.map (fun tdi -> tdi.id)
            values |> List.map (fun v -> Effect(v, [], EffectType.ValueTrigger))
        | None -> []

    let addTriggerDocsScopes (lookup : IRLookup) (rules : RootRule<_> list) =
            // let scriptedOptions (effect : ScriptedEffect) =
            //     {min = 0; max = 100; leafvalue = false; description = Some effect.Comments; pushScope = None; replaceScopes = None; severity = None; requiredScopes = effect.Scopes; comparison = false}
            // let getAllScriptedEffects =
            //     lookup.onlyScriptedEffects |> List.choose (function | :? ScriptedEffect as se -> Some se |_ -> None)
            //                                     |> List.map (fun se -> AliasRule("effect", NewRule(LeafRule(specificField se.Name, ValueField(ValueType.Bool)), scriptedOptions se)))
            // let getAllScriptedTriggers =
            //     lookup.onlyScriptedTriggers |> List.choose (function | :? ScriptedEffect as se -> Some se |_ -> None)
            //                                     |> List.map (fun se -> AliasRule("trigger", NewRule(LeafRule(specificField se.Name, ValueField(ValueType.Bool)), scriptedOptions se)))
            let addRequiredScopesE (s : StringTokens) (o : Options<_>) =
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
            let addRequiredScopesT (s : StringTokens) (o : Options<_>) =
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
                    // |AliasRule ("effect", (LeafRule(TypeField(TypeType.Simple "scripted_effect"), o), _)) ->
                    //     getAllScriptedEffects
                    // |AliasRule ("trigger", (LeafRule(TypeField(TypeType.Simple "scripted_trigger"), o), _)) ->
                    //     getAllScriptedTriggers
                    |x -> [x])



    let loadConfigRulesHook rules (lookup : IRLookup) embedded =
        let ts = updateScriptedTriggers lookup rules embedded
        let es = updateScriptedEffects lookup rules embedded
        let ls = updateEventTargetLinks embedded
        lookup.allCoreLinks <- ts @ es @ ls
        // eprintfn "crh %A" ts
        addTriggerDocsScopes lookup (rules @ addModifiersWithScopes lookup)

    let refreshConfigBeforeFirstTypesHook (lookup : IRLookup) _ _ =
        let modifierEnums = { key = "modifiers"; values = lookup.coreModifiers |> List.map (fun m -> m.tag); description = "Modifiers" }
        let provinceEnums = { key = "provinces"; description = "provinces"; values = lookup.IRprovinces}
        let charEnums = { key = "character_ids"; description = "character_ids"; values = lookup.IRcharacters}
        lookup.enumDefs <-
            lookup.enumDefs |> Map.add modifierEnums.key (modifierEnums.description, modifierEnums.values)
                            |> Map.add provinceEnums.key (provinceEnums.description, provinceEnums.values)
                            |> Map.add charEnums.key (charEnums.description, charEnums.values)

    let refreshConfigAfterFirstTypesHook (lookup : IRLookup) _ (embedded : EmbeddedSettings<_,_>) =
        lookup.typeDefInfo <-
            (lookup.typeDefInfo)
            |> addModifiersAsTypes lookup
        let ts = updateScriptedTriggers lookup lookup.configRules embedded @ addScriptFormulaLinks lookup
        let es = updateScriptedEffects lookup lookup.configRules embedded
        let ls = updateEventTargetLinks embedded @ addDataEventTargetLinks lookup embedded true
        lookup.allCoreLinks <- ts @ es @ ls

    let refreshConfigAfterVarDefHook (lookup : IRLookup) (resources : IResourceAPI<_>) (embedded : EmbeddedSettings<_,_>) =
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
        updateProvinces(game)
        updateCharacters(game)
        updateModifiers(game)

        // updateLegacyGovernments(game)
        // updateTechnologies()
        // game.LocalisationManager.UpdateAllLocalisation()
        // updateTypeDef game game.Settings.rules
        // game.LocalisationManager.UpdateAllLocalisation()


    let createEmbeddedSettings embeddedFiles cachedResourceData (configs : (string * string) list) =
        let scopeDefinitions =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
                            |> (fun f -> UtilityParser.initializeScopes f (Some defaultScopeInputs) )
        let irMods =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifiers.cwt")
                    |> Option.map (fun (fn, ft) -> IRParser.loadModifiers fn ft)
                    |> Option.defaultValue []

        let irLocCommands =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "localisation.cwt")
                    |> Option.map (fun (fn, ft) -> IRParser.loadLocCommands fn ft)
                    |> Option.defaultValue []

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
            localisationCommands = Legacy irLocCommands
            eventTargetLinks = irEventTargetLinks
            scopeDefinitions = scopeDefinitions
        }

type IRSettings = GameSetupSettings<Modifier, Scope, IRLookup>
open IRGameFunctions
type IRGame(setupSettings : IRSettings) =
    let validationSettings = {
        validators = [ validateMixedBlocks, "mixed"; validateIfWithNoEffect, "ifnoeffect"]
        experimentalValidators = []
        heavyExperimentalValidators = []
        experimental = false
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
        initialLookup = IRLookup()

    }
    do if settings.embedded.scopeDefinitions = [] then eprintfn "%A has no scopes" (settings.rootDirectories |> List.head) else ()

    let locSettings = settings.embedded.localisationCommands |> function |Legacy l -> (if l.Length = 0 then Legacy (locCommands()) else Legacy l) |_ -> Legacy (locCommands())
    let settings =
            { settings with
                embedded = { settings.embedded with localisationCommands = locSettings }
                initialLookup = IRLookup()
            }

    let rulesManagerSettings = {
        rulesSettings = settings.rules
        parseScope = scopeManager.ParseScope()
        allScopes = scopeManager.AllScopes
        anyScope = scopeManager.AnyScope
        changeScope = changeScope
        defaultContext = defaultContext
        defaultLang = IR IRLang.English
        oneToOneScopesNames = oneToOneScopesNames
        loadConfigRulesHook = loadConfigRulesHook
        refreshConfigBeforeFirstTypesHook = refreshConfigBeforeFirstTypesHook
        refreshConfigAfterFirstTypesHook = refreshConfigAfterFirstTypesHook
        refreshConfigAfterVarDefHook = refreshConfigAfterVarDefHook
    }
    let game = GameObject<Modifier, IRComputedData, IRLookup>.CreateGame
                ((settings, "imperator", scriptFolders, Compute.computeIRData,
                    Compute.computeIRDataUpdate,
                     (IRLocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
                     IRGameFunctions.processLocalisationFunction (settings.embedded.localisationCommands |> function |Legacy l -> l |_ -> []),
                     IRGameFunctions.validateLocalisationCommandFunction (settings.embedded.localisationCommands |> function |Legacy l -> l |_ -> []),
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
    let references = References<_, _>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))


    let parseErrors() =
        resources.GetResources()
            |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None)
            |> List.choose (fun r -> r.result |> function |(Fail (result)) when r.validate -> Some (r.filepath, result.error, result.position)  |_ -> None)

    interface IGame<IRComputedData, Scope, Modifier> with
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
        member __.UpdateFile shallow file text =game.UpdateFile shallow file text
        member __.AllEntities() = resources.AllEntities()
        member __.References() = References<_, _>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))
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
        member __.GetCodeEdits file text = None
        member __.GetEventGraphData : GraphDataRequest = (fun files -> graphEventDataForFiles references game.ResourceManager lookup files ["event"])

        //getFastTrigger fileManager game.ResourceManager file text
            //member __.ScriptedTriggers = parseResults |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some p |_ -> None) |> List.map (fun t -> )