namespace CWTools.Games.EU4
open CWTools.Localisation
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Games
open CWTools.Common
open FSharp.Collections.ParallelSeq
open CWTools.Localisation.EU4Localisation
open CWTools.Utilities.Utils
open CWTools.Utilities.Position
open CWTools.Utilities
open CWTools.Validation.Common.CommonValidation
// open CWTools.Rules.Rules
open CWTools.Rules
open CWTools.Common.EU4Constants
// open CWTools.Validation.EU4.EU4Rules
open CWTools.Process.EU4Scopes
open CWTools.Process.Scopes
open CWTools.Validation.EU4
open System.Text
open CWTools.Games.LanguageFeatures
open CWTools.Validation.EU4.EU4Validation
open CWTools.Validation.EU4.EU4LocalisationString
open CWTools.Games.Helpers
open CWTools.Games.Compute.EU4

module EU4GameFunctions =
    type GameObject = GameObject<Scope, Modifier, EU4ComputedData>
    let processLocalisationFunction (localisationCommands : ((string * Scope list) list)) (lookup : Lookup<Scope, Modifier>) =
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.typeDefInfo.TryFind "province_id" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.enumDefs.TryFind "country_tags" |> Option.map snd |> Option.defaultValue [])
        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.varDefInfo.TryFind "saved_name" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.varDefInfo.TryFind "exiled_ruler" |> Option.defaultValue [] |> List.map fst)
        processLocalisation localisationCommands eventtargets lookup.scriptedLoc definedvars

    let validateLocalisationCommandFunction (localisationCommands : ((string * Scope list) list)) (lookup : Lookup<Scope, Modifier>) =
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.typeDefInfo.TryFind "province_id" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.enumDefs.TryFind "country_tags" |> Option.map snd |> Option.defaultValue [])
        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.varDefInfo.TryFind "saved_name" |> Option.defaultValue [] |> List.map fst)
            @
            (lookup.varDefInfo.TryFind "exiled_ruler" |> Option.defaultValue [] |> List.map fst)
        validateLocalisationCommand localisationCommands eventtargets lookup.scriptedLoc definedvars

    let globalLocalisation (game : GameObject) =
        // let locfiles =  resources.GetResources()
        //                 |> List.choose (function |FileWithContentResource (_, e) -> Some e |_ -> None)
        //                 |> List.filter (fun f -> f.overwrite <> Overwritten && f.extension = ".yml" && f.validate)
        //                 |> List.map (fun f -> f.filepath)
        // let locFileValidation = validateLocalisationFiles locfiles
        let globalTypeLoc = game.ValidationManager.ValidateGlobalLocalisation()
        game.Lookup.proccessedLoc |> validateProcessedLocalisation game.LocalisationManager.taggedLocalisationKeys <&&>
        // locFileValidation <&&>
        globalTypeLoc |> (function |Invalid es -> es |_ -> [])
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

    let addModifiersWithScopes (lookup : Lookup<_,_>) =
        (lookup.coreModifiers |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(CWTools.Rules.RulesParser.specificField c.tag, ValueField (ValueType.Float (-1E+12, 1E+12))), {min = 0; max = 100; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = []; comparison = false}))))
        // let modifierOptions (modifier : Modifier) =
        //     let requiredScopes =
        //         modifier.categories |> List.choose (fun c -> modifierCategoryToScopesMap.TryFind c)
        //                             |> List.map Set.ofList
        //                             |> (fun l -> if List.isEmpty l then [] else l |> List.reduce (Set.intersect) |> Set.toList)
        //     {min = 0; max = 100; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = requiredScopes}
        // lookup.coreModifiers
        //     |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(specificField c.tag, ValueField (ValueType.Float (-1E+12, 1E+12))), modifierOptions c)))

    let updateScriptedEffects(rules :RootRule<Scope> list) =
        let effects =
            rules |> List.choose (function |AliasRule("effect", r) -> Some r |_ -> None)
        let ruleToEffect(r,o) =
            let name =
                match r with
                |LeafRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |NodeRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |_ -> ""
            DocEffect(name, o.requiredScopes, o.pushScope, EffectType.Effect, o.description |> Option.defaultValue "", "")
        (effects |> List.map ruleToEffect  |> List.map (fun e -> e :> Effect)) @ (scopedEffects |> List.map (fun e -> e :> Effect))

    let updateScriptedTriggers(rules :RootRule<Scope> list) =
        let effects =
            rules |> List.choose (function |AliasRule("trigger", r) -> Some r |_ -> None)
        let ruleToTrigger(r,o) =
            let name =
                match r with
                |LeafRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |NodeRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |_ -> ""
            DocEffect(name, o.requiredScopes, o.pushScope, EffectType.Trigger, o.description |> Option.defaultValue "", "")
        (effects |> List.map ruleToTrigger |> List.map (fun e -> e :> Effect)) @ (scopedEffects |> List.map (fun e -> e :> Effect))

    let addModifiersAsTypes (lookup : Lookup<_,_>) (typesMap : Map<string,(bool * string * range) list>) =
        // let createType (modifier : Modifier) =
        typesMap.Add("modifier", lookup.coreModifiers |> List.map (fun m -> (false, m.tag, range.Zero)))

    let loadConfigRulesHook rules (lookup : Lookup<_,_>) embedded =
        let ts = updateScriptedTriggers rules
        let es = updateScriptedEffects rules
        let ls = updateEventTargetLinks embedded
        lookup.allCoreLinks <- ts @ es @ ls
        rules @ addModifiersWithScopes lookup

    let refreshConfigBeforeFirstTypesHook (lookup : Lookup<_,_>) (resources : IResourceAPI<EU4ComputedData>) _ =
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

    let refreshConfigAfterFirstTypesHook (lookup : Lookup<_,_>) _ (embeddedSettings : EmbeddedSettings<_,_>) =
        lookup.typeDefInfoRaw <-
            (lookup.typeDefInfoRaw)
            |> addModifiersAsTypes lookup
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings false

    let refreshConfigAfterVarDefHook (lookup : Lookup<_,_>) (resources : IResourceAPI<_>) (embeddedSettings : EmbeddedSettings<_,_>) =
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings false

    let afterInit (game : GameObject) =
        updateScriptedLoc(game)
        updateModifiers(game)
        updateLegacyGovernments(game)
type EU4Settings = GameSettings<Modifier, Scope>
open EU4GameFunctions
type EU4Game(settings : EU4Settings) =
    let validationSettings = {
        validators = [ validateMixedBlocks, "mixed"; validateEU4NaiveNot, "not"]
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
        defaultLang = EU4 EU4Lang.Default
        oneToOneScopesNames = oneToOneScopesNames
        loadConfigRulesHook = loadConfigRulesHook
        refreshConfigBeforeFirstTypesHook = refreshConfigBeforeFirstTypesHook
        refreshConfigAfterFirstTypesHook = refreshConfigAfterFirstTypesHook
        refreshConfigAfterVarDefHook = refreshConfigAfterVarDefHook
    }

    let game = GameObject<Scope, Modifier, EU4ComputedData>.CreateGame
                ((settings, "europa universalis iv", scriptFolders, Compute.EU4.computeEU4Data,
                    Compute.EU4.computeEU4DataUpdate,
                     (EU4LocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
                     EU4GameFunctions.processLocalisationFunction (settings.embedded.localisationCommands),
                     EU4GameFunctions.validateLocalisationCommandFunction (settings.embedded.localisationCommands),
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

    interface IGame<EU4ComputedData, Scope, Modifier> with
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