namespace CWTools.Games.CK2
open CWTools.Localisation
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Games
open CWTools.Common
open CWTools.Localisation.CK2Localisation
open CWTools.Utilities.Utils
open CWTools.Utilities.Position
open CWTools.Utilities
open System.IO
open CWTools.Validation.Common.CommonValidation
// open CWTools.Rules.Rules
open CWTools.Common.CK2Constants
open CWTools.Process.Scopes.CK2
open CWTools.Process.Scopes.Scopes
open CWTools.Validation.CK2
open System.Text
open CWTools.Games.LanguageFeatures
open CWTools.Validation.CK2.CK2LocalisationString
open CWTools.Validation.LocalisationString
open CWTools.Process
open CWTools.Process.ProcessCore
open System
open CWTools.Games.Helpers
open CWTools.Rules
open CWTools.Parser
open CWTools.Common.CK2Constants
open CWTools.Common.NewScope

module CK2GameFunctions =
    type GameObject = GameObject<Scope, Modifier, ComputedData, CK2Lookup>
    let processLocalisationFunction (localisationCommands : ((string * Scope list) list)) (lookup : Lookup<Scope, Modifier>) =
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst)
        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
        processLocalisation() localisationCommands eventtargets lookup.scriptedLoc definedvars

    let validateLocalisationCommandFunction (localisationCommands : ((string * Scope list) list)) (lookup : Lookup<Scope, Modifier>) =
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
    let updateScriptedLoc (game : GameObject) =
        let rawLocs =
            game.Resources.AllEntities()
            |> List.choose (function |struct (f, _) when f.filepath.Contains("customizable_localisation") -> Some (f.entity) |_ -> None)
            |> List.collect (fun n -> n.Children)
            |> List.map (fun l -> l.TagText "name")
        game.Lookup.scriptedLoc <- rawLocs

    let updateModifiers (game : GameObject) =
        game.Lookup.coreModifiers <- game.Settings.embedded.modifiers

    let addModifiersWithScopes (lookup : Lookup<_,_>) =
        let modifierOptions (modifier : Modifier) =
            let requiredScopes =
                modifier.categories |> List.choose (fun c -> modifierCategoryToScopesMap().TryFind c)
                                    |> List.map Set.ofList
                                    |> (fun l -> if List.isEmpty l then [] else l |> List.reduce (Set.intersect) |> Set.toList)
            {min = 0; max = 100; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = requiredScopes; comparison = false}
        lookup.coreModifiers
            |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(CWTools.Rules.RulesParser.specificField c.tag, ValueField (ValueType.Float (-1E+12, 1E+12))), modifierOptions c)))

    let updateLandedTitles (game : GameObject) =
        let fNode =
            fun (t : Node) result ->
                match t.Key with
                | x when x.StartsWith "e_" && t.TagText "landless" == "yes" -> ((Empire, true), x)::result
                | x when x.StartsWith "e_" -> ((Empire, false), x)::result
                | x when x.StartsWith "k_" && t.TagText "landless" == "yes" -> ((Kingdom, true), x)::result
                | x when x.StartsWith "k_" -> ((Kingdom, false), x)::result
                | x when x.StartsWith "d_" ->
                    match t.TagText "landless", t.TagText "mercenary", t.TagText "holy_order" with
                    | "yes", "yes", _
                    | "yes", _, "yes" ->
                        ((Duchy_Hired, true), x)::result
                    | "yes", _, _ ->
                        ((Duchy_Normal, true), x)::result
                    | _, "yes", _
                    | _, _, "yes" ->
                        ((Duchy_Hired, false), x)::result
                    | _ ->
                        ((Duchy_Normal, false), x)::result
                | x when x.StartsWith "c_" && t.TagText "landless" == "yes" -> ((County, true), x)::result
                | x when x.StartsWith "c_" -> ((County, false), x)::result
                | x when x.StartsWith "b_" && t.TagText "landless" == "yes"-> ((Barony, true), x)::result
                | x when x.StartsWith "b_" -> ((Barony, false), x)::result
                | _ -> result
        let titleEntities = (EntitySet (game.Resources.AllEntities())).GlobMatchChildren("**/landed_titles/**/*.txt")
        let titles = titleEntities |> List.collect (foldNode7 fNode)
        let inner (ells, es, klls, ks, dnlls, dns, dhlls, dhs, clls, cs, blls, bs) (k : TitleType * bool, value : string) =
             match k with
             | Empire, true -> (value::ells, es, klls, ks, dnlls, dns, dhlls, dhs, clls, cs, blls, bs)
             | Empire, false -> (ells, value::es, klls, ks, dnlls, dns, dhlls, dhs, clls, cs, blls, bs)
             | Kingdom, true -> (ells, es, value::klls, ks, dnlls, dns, dhlls, dhs, clls, cs, blls, bs)
             | Kingdom, false -> (ells, es, klls, value::ks, dnlls, dns, dhlls, dhs, clls, cs, blls, bs)
             | Duchy_Normal, true -> (ells, es, klls, ks, value::dnlls, dns, dhlls, dhs, clls, cs, blls, bs)
             | Duchy_Normal, false -> (ells, es, klls, ks, dnlls, value::dns, dhlls, dhs, clls, cs, blls, bs)
             | Duchy_Hired, true -> (ells, es, klls, ks, dnlls, dns, value::value::dhlls, dhs, clls, cs, blls, bs)
             | Duchy_Hired, false -> (ells, es, klls, ks, dnlls, dns, dhlls, dhs, clls, cs, blls, bs)
             | County, true -> (ells, es, klls, ks, dnlls, dns, dhlls, dhs,value:: clls, cs, blls, bs)
             | County, false -> (ells, es, klls, ks, dnlls, dns, dhlls, dhs, clls, value::cs, blls, bs)
             | Barony, true -> (ells, es, klls, ks, dnlls, dns, dhlls, dhs, clls, cs, value::blls, bs)
             | Barony, false -> (ells, es, klls, ks, dnlls, dns, dhlls, dhs, clls, cs, blls, value::bs)
        let (ells, es, klls, ks, dnlls, dns, dhlls, dhs, clls, cs, blls, bs) = titles |> List.fold inner ([], [], [], [], [], [], [], [], [], [], [], [])
        game.Lookup.CK2LandedTitles <-
            ((Empire, true), ells)::((Empire, false), es)::((Kingdom, true), klls)::((Kingdom, false), ks)
                ::((Duchy_Normal, true), dnlls)::((Duchy_Normal, false), dns)
                ::((Duchy_Hired, true), dhlls)::((Duchy_Hired, false), dhs)
                ::((County, true), clls)::((County, false), cs)
                ::((Barony, true), blls)::[((Barony, false), bs)] |> Map.ofList
    let createLandedTitleTypes(lookup : CK2Lookup)(map : Map<_,_>) =
        let ells = lookup.CK2LandedTitles.[Empire, true] |> List.map (fun e -> (false, e, range.Zero))
        let es = lookup.CK2LandedTitles.[Empire, false] |> List.map (fun e -> (false, e, range.Zero))
        let klls = lookup.CK2LandedTitles.[Kingdom, true] |> List.map (fun e -> (false, e, range.Zero))
        let ks = lookup.CK2LandedTitles.[Kingdom, false] |> List.map (fun e -> (false, e, range.Zero))
        let dllns = lookup.CK2LandedTitles.[Duchy_Normal, true] |> List.map (fun e -> (false, e, range.Zero))
        let dns = lookup.CK2LandedTitles.[Duchy_Normal, false] |> List.map (fun e -> (false, e, range.Zero))
        let dllhs = lookup.CK2LandedTitles.[Duchy_Hired, true] |> List.map (fun e -> (false, e, range.Zero))
        let dhs = lookup.CK2LandedTitles.[Duchy_Hired, false] |> List.map (fun e -> (false, e, range.Zero))
        let clls = lookup.CK2LandedTitles.[County, true] |> List.map (fun e -> (false, e, range.Zero))
        let cs = lookup.CK2LandedTitles.[County, false] |> List.map (fun e -> (false, e, range.Zero))
        let blls = lookup.CK2LandedTitles.[Barony, true] |> List.map (fun e -> (false, e, range.Zero))
        let bs = lookup.CK2LandedTitles.[Barony, false] |> List.map (fun e -> (false, e, range.Zero))
        map |> Map.add "title.empire" (es@ells)
            |> Map.add "title.kingdom" (ks@klls)
            |> Map.add "title.duchy" (dllns@dns@dllhs@dhs)
            |> Map.add "title.duchy_hired" (dllhs@dhs)
            |> Map.add "title.duchy_normal" (dllns@dns)
            |> Map.add "title.county" (clls@cs)
            |> Map.add "title.barony" (blls@bs)
            |> Map.add "title.landless" (ells@klls@dllns@dllhs@clls@blls)
            |> Map.add "title.landed" (es@ks@dns@dhs@cs@bs)
            |> Map.add "title" (es@ells@ks@klls@dns@dllns@dhs@dllhs@cs@clls@bs@blls)

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
            game.Lookup.CK2provinces <- provinces

    let updateScriptedEffects (lookup : Lookup<_,_>) (rules :RootRule<Scope> list) (embeddedSettings : EmbeddedSettings<_,_>) =
        let effects =
            rules |> List.choose (function |AliasRule("effect", r) -> Some r |_ -> None)
        let ruleToEffect(r,o) =
            let name =
                match r with
                |LeafRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |NodeRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |_ -> ""
            DocEffect(name, o.requiredScopes, o.pushScope, EffectType.Effect, o.description |> Option.defaultValue "", "")
        // let simpleEventTargetLinks = embeddedSettings.eventTargetLinks |> List.choose (function | SimpleLink l -> Some (l :> Effect) | _ -> None)
        (effects |> List.map ruleToEffect  |> List.map (fun e -> e :> Effect))

    let updateScriptedTriggers (lookup : Lookup<_,_>) (rules :RootRule<Scope> list) (embeddedSettings : EmbeddedSettings<_,_>) =
        let effects =
            rules |> List.choose (function |AliasRule("trigger", r) -> Some r |_ -> None)
        let ruleToTrigger(r,o) =
            let name =
                match r with
                |LeafRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |NodeRule(ValueField(Specific n),_) -> StringResource.stringManager.GetStringForID n.normal
                |_ -> ""
            DocEffect(name, o.requiredScopes, o.pushScope, EffectType.Trigger, o.description |> Option.defaultValue "", "")
        // let simpleEventTargetLinks = embeddedSettings.eventTargetLinks |> List.choose (function | SimpleLink l -> Some (l :> Effect) | _ -> None)
        (effects |> List.map ruleToTrigger |> List.map (fun e -> e :> Effect))

    let addModifiersAsTypes (lookup : Lookup<_,_>) (typesMap : Map<string,(bool * string * range) list>) =
        // let createType (modifier : Modifier) =
        typesMap.Add("modifier", lookup.coreModifiers |> List.map (fun m -> (false, m.tag, range.Zero)))


    let loadConfigRulesHook rules (lookup : Lookup<_,_>) embedded =
        let ts = updateScriptedTriggers lookup rules embedded
        let es = updateScriptedEffects lookup rules embedded
        let ls = updateEventTargetLinks embedded
        lookup.allCoreLinks <- ts @ es @ ls
        rules @ addModifiersWithScopes lookup

    let refreshConfigBeforeFirstTypesHook (lookup : CK2Lookup) _ _ =
        let modifierEnums = { key = "modifiers"; values = lookup.coreModifiers |> List.map (fun m -> m.tag); description = "Modifiers" }
        let provinceEnums = { key = "provinces"; description = "provinces"; values = lookup.CK2provinces}
        lookup.enumDefs <-
            lookup.enumDefs |> Map.add modifierEnums.key (modifierEnums.description, modifierEnums.values)
                            |> Map.add provinceEnums.key (provinceEnums.description, provinceEnums.values)

    let refreshConfigAfterFirstTypesHook (lookup : CK2Lookup) _ (embeddedSettings : EmbeddedSettings<_,_>) =
        lookup.typeDefInfoRaw <-
            createLandedTitleTypes lookup (lookup.typeDefInfoRaw)
            |> addModifiersAsTypes lookup
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings false

    let refreshConfigAfterVarDefHook (lookup : Lookup<_,_>) (resources : IResourceAPI<_>) (embeddedSettings : EmbeddedSettings<_,_>) =
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings false


    let afterInit (game : GameObject) =
        // updateScriptedTriggers()
        // updateScriptedEffects()
        // updateStaticodifiers()
        // updateScriptedLoc(game)
        // updateDefinedVariables()
        updateModifiers(game)
        // updateLegacyGovernments(game)
        // updateTechnologies()
        updateLandedTitles(game)
        updateProvinces(game)
        // game.LocalisationManager.UpdateAllLocalisation()
        // updateTypeDef game game.Settings.rules
        // game.LocalisationManager.UpdateAllLocalisation()

    let createEmbeddedSettings embeddedFiles cachedResourceData (configs : (string * string) list) =
        let scopeDefinitions =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
                            |> (fun f -> UtilityParser.initializeScopes f (Some defaultScopeInputs) )


        let ck2Mods =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifiers.cwt")
                    |> Option.map (fun (fn, ft) -> CK2Parser.loadModifiers fn ft)
                    |> Option.defaultValue []

        let ck2LocCommands =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "localisation.cwt")
                    |> Option.map (fun (fn, ft) -> CK2Parser.loadLocCommands fn ft)
                    |> Option.defaultValue []

        let ck2EventTargetLinks =
            configs |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "links.cwt")
                    |> Option.map (fun (fn, ft) -> UtilityParser.loadEventTargetLinks scopeManager.AnyScope (scopeManager.ParseScope()) scopeManager.AllScopes fn ft)
                    |> Option.defaultValue (CWTools.Process.Scopes.CK2.scopedEffects() |> List.map SimpleLink)

        let triggers, effects = ([], [])

        {
            triggers = triggers
            effects = effects
            modifiers = ck2Mods
            embeddedFiles = embeddedFiles
            cachedResourceData = cachedResourceData
            localisationCommands = ck2LocCommands
            eventTargetLinks = ck2EventTargetLinks
            scopeDefinitions = scopeDefinitions
        }

type CK2Settings = GameSetupSettings<Modifier, Scope, CK2Lookup>
open CK2GameFunctions
type CK2Game(setupSettings : CK2Settings) =
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
        initialLookup = CK2Lookup()

    }
    do if settings.embedded.scopeDefinitions = [] then eprintfn "%A has no scopes" (settings.rootDirectories |> List.head) else ()

    let settings = { settings with
                        embedded = { settings.embedded with localisationCommands = settings.embedded.localisationCommands |> (fun l -> if l.Length = 0 then locCommands else l )}
                        initialLookup = CK2Lookup()
                        }

    let rulesManagerSettings = {
        rulesSettings = settings.rules
        parseScope = scopeManager.ParseScope()
        allScopes = scopeManager.AllScopes
        anyScope = scopeManager.AnyScope
        changeScope = changeScope
        defaultContext = defaultContext
        defaultLang = CK2 CK2Lang.Default
        oneToOneScopesNames = oneToOneScopesNames
        loadConfigRulesHook = loadConfigRulesHook
        refreshConfigBeforeFirstTypesHook = refreshConfigBeforeFirstTypesHook
        refreshConfigAfterFirstTypesHook = refreshConfigAfterFirstTypesHook
        refreshConfigAfterVarDefHook = refreshConfigAfterVarDefHook
    }
    let game = GameObject.CreateGame
                ((settings, "crusader kings ii", scriptFolders, Compute.computeCK2Data,
                    Compute.computeCK2DataUpdate,
                     (CK2LocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
                     CK2GameFunctions.processLocalisationFunction (settings.embedded.localisationCommands),
                     CK2GameFunctions.validateLocalisationCommandFunction (settings.embedded.localisationCommands),
                     defaultContext,
                     noneContext,
                     Encoding.UTF8,
                     Encoding.GetEncoding(1252),
                     validationSettings,
                     globalLocalisation,
                     (fun _ _ -> ()),
                     ".csv",
                     rulesManagerSettings))
                     afterInit
    let lookup = game.Lookup
    let resources = game.Resources
    let fileManager = game.FileManager


    let parseErrors() =
        resources.GetResources()
            |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None)
            |> List.choose (fun r -> r.result |> function |(Fail (result)) when r.validate -> Some (r.filepath, result.error, result.position)  |_ -> None)

    interface IGame<CK2ComputedData, Scope, Modifier> with
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
        member __.References() = References<_, Scope, _>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))
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
        member __.GetCodeEdits file text = None //getFastTrigger fileManager game.ResourceManager file text
            //member __.ScriptedTriggers = parseResults |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some p |_ -> None) |> List.map (fun t -> )
