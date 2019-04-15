namespace CWTools.Games.IR
open CWTools.Localisation
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Games
open CWTools.Common
open CWTools.Localisation.IRLocalisation
open CWTools.Utilities.Position
open CWTools.Utilities
open System.IO
open CWTools.Validation.Common.CommonValidation
// open CWTools.Rules.Rules
open CWTools.Rules
open CWTools.Common.IRConstants
open CWTools.Process.IRScopes
open CWTools.Process.Scopes
open CWTools.Validation.IR
open System.Text
open CWTools.Games.LanguageFeatures
open CWTools.Validation.IR.IRLocalisationString
open CWTools.Validation.LocalisationString
open CWTools.Process
open System
open CWTools.Games.Helpers

module IRGameFunctions =
    type GameObject = GameObject<Scope, Modifier, IRComputedData>
    let processLocalisationFunction (localisationCommands : ((string * Scope list) list)) (lookup : Lookup<Scope, Modifier>) =
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst)
        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
        processLocalisation localisationCommands eventtargets lookup.scriptedLoc definedvars

    let validateLocalisationCommandFunction (localisationCommands : ((string * Scope list) list)) (lookup : Lookup<Scope, Modifier>) =
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst)
        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
        validateLocalisationCommand localisationCommands eventtargets lookup.scriptedLoc definedvars

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

    let addModifiersWithScopes (lookup : Lookup<_,_>) =
        let modifierOptions (modifier : Modifier) =
            let requiredScopes =
                modifier.categories |> List.choose (fun c -> modifierCategoryToScopesMap.TryFind c)
                                    |> List.map Set.ofList
                                    |> (fun l -> if List.isEmpty l then [] else l |> List.reduce (Set.intersect) |> Set.toList)
            {min = 0; max = 100; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = requiredScopes; comparison = false}
        lookup.coreModifiers
            |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(CWTools.Rules.RulesParser.specificField c.tag, ValueField (ValueType.Float (-1E+12, 1E+12))), modifierOptions c)))

    let updateScriptedTriggers (lookup : Lookup<_,_>) (rules :RootRule<Scope> list) (embeddedSettings : EmbeddedSettings<_,_>) =
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
                |LeafRule(ValueField(Specific n),_) when not (List.contains n vanillaTriggerNames) -> Some (StringResource.stringManager.GetStringForID n.normal)
                |NodeRule(ValueField(Specific n),_) when not (List.contains n vanillaTriggerNames) -> Some (StringResource.stringManager.GetStringForID n.normal)
                |_ -> None
            let effectType = if o.comparison then EffectType.ValueTrigger else EffectType.Trigger
            name |> Option.map (fun name -> DocEffect(name, o.requiredScopes, o.pushScope, effectType, o.description |> Option.defaultValue "", ""))
        // let simpleEventTargetLinks = embeddedSettings.eventTargetLinks |> List.choose (function | SimpleLink l -> Some (l :> Effect) | _ -> None)
        let extraFromRules = (effects |> List.choose ruleToTrigger |> List.map (fun e -> e :> Effect))
        // let sts, ts = STLLookup.updateScriptedTriggers game.Resources vanillaTriggers
        // game.Lookup.triggers <- sts @ ts
        // game.Lookup.onlyScriptedTriggers <- sts
        vanillaTriggers @ extraFromRules

    let updateScriptedEffects (lookup : Lookup<_,_>) (rules :RootRule<Scope> list) (embeddedSettings : EmbeddedSettings<_,_>) =
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


    let addModifiersAsTypes (lookup : Lookup<_,_>) (typesMap : Map<string,(bool * string * range) list>) =
        // let createType (modifier : Modifier) =
        typesMap.Add("modifier", lookup.coreModifiers |> List.map (fun m -> (false, m.tag, range.Zero)))

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

    let addScriptFormulaLinks (lookup : Lookup<'S,'M>) =
        match lookup.typeDefInfo |> Map.tryFind "script_value" with
        | Some vs ->
            let values = vs |> List.map fst
            values |> List.map (fun v -> Effect(v, [], EffectType.ValueTrigger))
        | None -> []

    let addTriggerDocsScopes (lookup : Lookup<_,_>) (rules : RootRule<_> list) =
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
                    |AliasRule ("effect", (LeafRule(ValueField(ValueType.Specific s),r), o)) ->
                        [AliasRule ("effect", (LeafRule(ValueField(ValueType.Specific s),r), addRequiredScopesE s o))]
                    |AliasRule ("trigger", (LeafRule(ValueField(ValueType.Specific s),r), o)) ->
                        [AliasRule ("trigger", (LeafRule(ValueField(ValueType.Specific s),r), addRequiredScopesT s o))]
                    |AliasRule ("effect", (NodeRule(ValueField(ValueType.Specific s),r), o)) ->
                        [AliasRule ("effect", (NodeRule(ValueField(ValueType.Specific s),r), addRequiredScopesE s o))]
                    |AliasRule ("trigger", (NodeRule(ValueField(ValueType.Specific s),r), o)) ->
                        [AliasRule ("trigger", (NodeRule(ValueField(ValueType.Specific s),r), addRequiredScopesT s o))]
                    |AliasRule ("effect", (LeafValueRule(ValueField(ValueType.Specific s)), o)) ->
                        [AliasRule ("effect", (LeafValueRule(ValueField(ValueType.Specific s)), addRequiredScopesE s o))]
                    |AliasRule ("trigger", (LeafValueRule(ValueField(ValueType.Specific s)), o)) ->
                        [AliasRule ("trigger", (LeafValueRule(ValueField(ValueType.Specific s)), addRequiredScopesT s o))]
                    // |AliasRule ("effect", (LeafRule(TypeField(TypeType.Simple "scripted_effect"), o), _)) ->
                    //     getAllScriptedEffects
                    // |AliasRule ("trigger", (LeafRule(TypeField(TypeType.Simple "scripted_trigger"), o), _)) ->
                    //     getAllScriptedTriggers
                    |x -> [x])



    let loadConfigRulesHook rules (lookup : Lookup<_,_>) embedded =
        let ts = updateScriptedTriggers lookup rules embedded
        let es = updateScriptedEffects lookup rules embedded
        let ls = updateEventTargetLinks embedded
        lookup.allCoreLinks <- ts @ es @ ls
        // eprintfn "crh %A" ts
        addTriggerDocsScopes lookup (rules @ addModifiersWithScopes lookup)

    let refreshConfigBeforeFirstTypesHook (lookup : Lookup<_,_>) _ _ =
        let modifierEnums = { key = "modifiers"; values = lookup.coreModifiers |> List.map (fun m -> m.Tag); description = "Modifiers" }
        let provinceEnums = { key = "provinces"; description = "provinces"; values = lookup.IRprovinces}
        let charEnums = { key = "character_ids"; description = "character_ids"; values = lookup.IRcharacters}
        lookup.enumDefs <-
            lookup.enumDefs |> Map.add modifierEnums.key (modifierEnums.description, modifierEnums.values)
                            |> Map.add provinceEnums.key (provinceEnums.description, provinceEnums.values)
                            |> Map.add charEnums.key (charEnums.description, charEnums.values)

    let refreshConfigAfterFirstTypesHook (lookup : Lookup<_,_>) _ (embedded : EmbeddedSettings<_,_>) =
        lookup.typeDefInfoRaw <-
            (lookup.typeDefInfoRaw)
            |> addModifiersAsTypes lookup
        let ts = updateScriptedTriggers lookup lookup.configRules embedded @ addScriptFormulaLinks lookup
        let es = updateScriptedEffects lookup lookup.configRules embedded
        let ls = updateEventTargetLinks embedded @ addDataEventTargetLinks lookup embedded true
        lookup.allCoreLinks <- ts @ es @ ls

    let refreshConfigAfterVarDefHook (lookup : Lookup<_,_>) (resources : IResourceAPI<_>) (embedded : EmbeddedSettings<_,_>) =
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
type IRSettings = GameSettings<Modifier, Scope>
open IRGameFunctions
type IRGame(settings : IRSettings) =
    let validationSettings = {
        validators = [ validateMixedBlocks, "mixed";]
        experimentalValidators = []
        heavyExperimentalValidators = []
        experimental = false
        fileValidators = []
        lookupValidators = []
        useRules = true
        debugRulesOnly = false
        localisationValidators = []
    }

    let settings = { settings with embedded = { settings.embedded with localisationCommands = settings.embedded.localisationCommands |> (fun l -> if l.Length = 0 then locCommands else l )}}

    let rulesManagerSettings = {
        rulesSettings = settings.rules
        parseScope = parseScope
        allScopes = allScopes
        anyScope = Scope.Any
        changeScope = changeScope
        defaultContext = defaultContext
        defaultLang = IR IRLang.English
        oneToOneScopesNames = oneToOneScopesNames
        loadConfigRulesHook = loadConfigRulesHook
        refreshConfigBeforeFirstTypesHook = refreshConfigBeforeFirstTypesHook
        refreshConfigAfterFirstTypesHook = refreshConfigAfterFirstTypesHook
        refreshConfigAfterVarDefHook = refreshConfigAfterVarDefHook
    }
    let game = GameObject<Scope, Modifier, IRComputedData>.CreateGame
                ((settings, "imperator", scriptFolders, Compute.computeIRData,
                    Compute.computeIRDataUpdate,
                     (IRLocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
                     IRGameFunctions.processLocalisationFunction (settings.embedded.localisationCommands),
                     IRGameFunctions.validateLocalisationCommandFunction (settings.embedded.localisationCommands),
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


    let parseErrors() =
        resources.GetResources()
            |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None)
            |> List.choose (fun r -> r.result |> function |(Fail (result)) when r.validate -> Some (r.filepath, result.error, result.position)  |_ -> None)

    interface IGame<IRComputedData, Scope, Modifier> with
    //member __.Results = parseResults
        member __.ParserErrors() = parseErrors()
        member __.ValidationErrors() = let (s, d) = (game.ValidationManager.Validate(false, (resources.ValidatableEntities()))) in s @ d
        member __.LocalisationErrors(force : bool, forceGlobal : bool) =
            let genGlobal() =
                let ges = (globalLocalisation(game))
                game.LocalisationManager.globalLocalisationErrors <- Some ges
                ges
            let genAll() =
                let les = (game.ValidationManager.ValidateLocalisation (resources.ValidatableEntities()))
                game.LocalisationManager.localisationErrors <- Some les
                les
            match game.LocalisationManager.localisationErrors, game.LocalisationManager.globalLocalisationErrors with
            |Some les, Some ges -> (if force then genAll() else les) @ (if forceGlobal then genGlobal() else ges)
            |None, Some ges -> (genAll()) @ (if forceGlobal then genGlobal() else ges)
            |Some les, None -> (if force then genAll() else les) @ (genGlobal())
            |None, None -> (genAll()) @ (genGlobal())

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
        member __.References() = References<_, Scope, _>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))
        member __.Complete pos file text = completion fileManager game.completionService game.InfoService game.ResourceManager pos file text
        member __.ScopesAtPos pos file text = scopesAtPos fileManager game.ResourceManager game.InfoService Scope.Any pos file text
        member __.GoToType pos file text = getInfoAtPos fileManager game.ResourceManager game.InfoService lookup pos file text
        member __.FindAllRefs pos file text = findAllRefsFromPos fileManager game.ResourceManager game.InfoService pos file text
        member __.InfoAtPos pos file text = game.InfoAtPos pos file text
        member __.ReplaceConfigRules rules = game.ReplaceConfigRules(({ ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})) //refreshRuleCaches game (Some { ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})
        member __.RefreshCaches() = game.RefreshCaches()
        member __.RefreshLocalisationCaches() = game.LocalisationManager.UpdateProcessedLocalisation()
        member __.ForceRecompute() = resources.ForceRecompute()
        member __.Types() = game.Lookup.typeDefInfo

            //member __.ScriptedTriggers = parseResults |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some p |_ -> None) |> List.map (fun t -> )