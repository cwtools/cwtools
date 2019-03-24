namespace CWTools.Games.HOI4
open CWTools.Common.HOI4Constants
open CWTools.Localisation
open CWTools.Validation.ValidationCore
open CWTools.Games.Files
open CWTools.Games
open CWTools.Common
open FSharp.Collections.ParallelSeq
open CWTools.Localisation.HOI4Localisation
open CWTools.Utilities.Utils
open CWTools.Utilities.Position
open CWTools.Utilities
open System.IO
open CWTools.Validation.Common.CommonValidation
// open CWTools.Validation.Rules
open CWTools.Parser.ConfigParser
open CWTools.Common.HOI4Constants
//open CWTools.Validation.HOI4.HOI4Rules
open CWTools.Validation.Rules
open CWTools.Process.HOI4Scopes
open CWTools.Common
open CWTools.Process.Scopes
open CWTools.Validation.HOI4
open System.Text
open CWTools.Games.LanguageFeatures
open System
open CWTools.Validation.HOI4.HOI4LocalisationString
open CWTools.Games
open CWTools.Games.Helpers

module HOI4GameFunctions =
    type GameObject = GameObject<Scope, Modifier, HOI4ComputedData>
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
        // lookup.proccessedLoc |> validateProcessedLocalisation taggedLocalisationKeys <&&>
        // locFileValidation <&&>
        globalTypeLoc |> (function |Invalid es -> es |_ -> [])

    let updateModifiers (game : GameObject) =
        game.Lookup.coreModifiers <- game.Settings.embedded.modifiers

    let updateProvinces (game : GameObject) =
        let provinceFile =
            game.Resources.GetResources()
            |> List.choose (function |FileWithContentResource (_, e) -> Some e |_ -> None)
            |> List.tryFind (fun f -> f.overwrite <> Overwritten && Path.GetFileName(f.filepath) = "definition.csv")
        match provinceFile with
        |None -> ()
        |Some pf ->
            let lines = pf.filetext.Split(([|"\r\n"; "\r"; "\n"|]), StringSplitOptions.None)
            let provinces = lines |> Array.choose (fun l -> l.Split([|';'|], 2, StringSplitOptions.RemoveEmptyEntries) |> Array.tryHead) |> List.ofArray
            game.Lookup.HOI4provinces <- provinces

    let updateScriptedEffects(rules :RootRule<Scope> list) (states : string list) (countries : string list) =
        let effects =
            rules |> List.choose (function |AliasRule("effect", r) -> Some r |_ -> None)
        let ruleToEffect(r,o) =
            let name =
                match r with
                |LeafRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |NodeRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |_ -> ""
            DocEffect(name, o.requiredScopes, EffectType.Effect, o.description |> Option.defaultValue "", "")
        let stateEffects =  states |> List.map (fun p -> ScopedEffect(p, allScopes, Scope.State, EffectType.Link, defaultDesc, "", true));
        let countryEffects =  countries |> List.map (fun p -> ScopedEffect(p, allScopes, Scope.Country, EffectType.Link, defaultDesc, "", true));
        (effects |> List.map ruleToEffect  |> List.map (fun e -> e :> Effect)) @ (scopedEffects |> List.map (fun e -> e :> Effect))
        @ (stateEffects |> List.map (fun e -> e :> Effect)) @ (countryEffects |> List.map (fun e -> e :> Effect))

    let updateScriptedTriggers(rules :RootRule<Scope> list) states countries =
        let effects =
            rules |> List.choose (function |AliasRule("trigger", r) -> Some r |_ -> None)
        let ruleToTrigger(r,o) =
            let name =
                match r with
                |LeafRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |NodeRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |_ -> ""
            DocEffect(name, o.requiredScopes, EffectType.Trigger, o.description |> Option.defaultValue "", "")
        let stateEffects =  states |> List.map (fun p -> ScopedEffect(p, allScopes, Scope.State, EffectType.Link, defaultDesc, "", true));
        let countryEffects =  countries |> List.map (fun p -> ScopedEffect(p, allScopes, Scope.Country, EffectType.Link, defaultDesc, "", true));
        (effects |> List.map ruleToTrigger |> List.map (fun e -> e :> Effect)) @ (scopedEffects |> List.map (fun e -> e :> Effect))
        @ (stateEffects |> List.map (fun e -> e :> Effect)) @ (countryEffects |> List.map (fun e -> e :> Effect))
    let addModifiersWithScopes (lookup : Lookup<_,_>) =
        let modifierOptions (modifier : Modifier) =
            let requiredScopes =
                modifier.categories |> List.choose (fun c -> modifierCategoryToScopesMap.TryFind c)
                                    |> List.map Set.ofList
                                    |> (fun l -> if List.isEmpty l then [] else l |> List.reduce (Set.intersect) |> Set.toList)
            {min = 0; max = 100; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = requiredScopes}
        lookup.coreModifiers
            |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(specificField c.tag, ValueField (ValueType.Float (-1E+12, 1E+12))), modifierOptions c)))

    let loadConfigRulesHook rules (lookup : Lookup<_,_>) embedded =
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ updateEventTargetLinks embedded
        rules @ addModifiersWithScopes lookup

    let refreshConfigBeforeFirstTypesHook (lookup : Lookup<_,_>) _ _ =
        let provinceEnums = { key = "provinces"; description = "provinces"; values = lookup.HOI4provinces}
        lookup.enumDefs <-
            lookup.enumDefs |> Map.add provinceEnums.key (provinceEnums.description, provinceEnums.values)

    let refreshConfigAfterFirstTypesHook (lookup : Lookup<_,_>) _ (embeddedSettings : EmbeddedSettings<_,_>) =
        let states = lookup.typeDefInfo.TryFind "state"
                            |> Option.map (fun sl -> sl |> List.map fst)
                            |> Option.defaultValue []
        let countries = lookup.enumDefs.TryFind "country_tag"
                            |> Option.map snd
                            |> Option.defaultValue []
        let ts = updateScriptedTriggers lookup.configRules states countries
        let es = updateScriptedEffects lookup.configRules states countries
        let ls = updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings
        lookup.allCoreLinks <- ts @ es @ ls

    let refreshConfigAfterVarDefHook (lookup : Lookup<_,_>) (resources : IResourceAPI<_>) (embeddedSettings : EmbeddedSettings<_,_>) =
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings

    let afterInit (game : GameObject) =
        updateModifiers(game)
        updateProvinces(game)

type HOI4Settings = GameSettings<Modifier, Scope>
open HOI4GameFunctions
type HOI4Game(settings : HOI4Settings) =
    let validationSettings = {
        validators = [ validateMixedBlocks, "mixed"; ]
        experimentalValidators = []
        heavyExperimentalValidators = []
        experimental = settings.validation.experimental
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
        defaultLang = HOI4 HOI4Lang.Default
        oneToOneScopesNames = oneToOneScopesNames
        loadConfigRulesHook = loadConfigRulesHook
        refreshConfigBeforeFirstTypesHook = refreshConfigBeforeFirstTypesHook
        refreshConfigAfterFirstTypesHook = refreshConfigAfterFirstTypesHook
        refreshConfigAfterVarDefHook = refreshConfigAfterVarDefHook
    }

    let game = GameObject<Scope, Modifier, HOI4ComputedData>.CreateGame
                (settings, "hearts of iron iv", scriptFolders, HOI4Compute.computeHOI4Data,
                HOI4Compute.computeHOI4DataUpdate,
                 (HOI4LocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
                 HOI4GameFunctions.processLocalisationFunction (settings.embedded.localisationCommands),
                 HOI4GameFunctions.validateLocalisationCommandFunction (settings.embedded.localisationCommands),
                 defaultContext,
                 noneContext,
                 Encoding.UTF8,
                 Encoding.GetEncoding(1252),
                 validationSettings,
                 globalLocalisation,
                 (fun _ _ -> ()),
                 ".yml",
                 rulesManagerSettings)
                afterInit
    let lookup = game.Lookup
    let resources = game.Resources
    let fileManager = game.FileManager

    let getEmbeddedFiles() = settings.embedded.embeddedFiles |> List.map (fun (fn, f) -> "embedded", "embeddedfiles/" + fn, f)

    let parseErrors() =
        resources.GetResources()
            |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None)
            |> List.choose (fun r -> r.result |> function |(Fail (result)) when r.validate -> Some (r.filepath, result.error, result.position)  |_ -> None)

    interface IGame<HOI4ComputedData, Scope, Modifier> with
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
        member __.UpdateFile shallow file text = game.UpdateFile shallow file text
        member __.AllEntities() = resources.AllEntities()
        member __.References() = References<HOI4ComputedData, Scope, _>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))
        member __.Complete pos file text = completion fileManager game.completionService game.InfoService game.ResourceManager pos file text
        member __.ScopesAtPos pos file text = scopesAtPos fileManager game.ResourceManager game.InfoService Scope.Any pos file text |> Option.map (fun sc -> { OutputScopeContext.From = sc.From; Scopes = sc.Scopes; Root = sc.Root})
        member __.GoToType pos file text = getInfoAtPos fileManager game.ResourceManager game.InfoService lookup pos file text
        member __.FindAllRefs pos file text = findAllRefsFromPos fileManager game.ResourceManager game.InfoService pos file text
        member __.InfoAtPos pos file text = game.InfoAtPos pos file text
        member __.ReplaceConfigRules rules = game.ReplaceConfigRules(({ ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})) //refreshRuleCaches game (Some { ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})
        member __.RefreshCaches() = game.RefreshCaches()
        member __.RefreshLocalisationCaches() = game.LocalisationManager.UpdateProcessedLocalisation()
        member __.ForceRecompute() = resources.ForceRecompute()
        member __.Types() = game.Lookup.typeDefInfo
