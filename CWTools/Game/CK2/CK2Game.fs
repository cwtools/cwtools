namespace CWTools.Games.CK2

open CWTools.Localisation
open CWTools.Utilities.Utils2
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
open CWTools.Common.CK2Constants
open CWTools.Process.Scopes.CK2
open CWTools.Process.Scopes.Scopes
open System.Text
open CWTools.Games.LanguageFeatures
open CWTools.Validation.LocalisationString
open CWTools.Process
open CWTools.Process.ProcessCore
open System
open CWTools.Games.Helpers
open CWTools.Rules
open CWTools.Parser
open CWTools.Process.Localisation

module CK2GameFunctions =
    type GameObject = GameObject<ComputedData, CK2Lookup>

    let createLocDynamicSettings (lookup: Lookup) =
        let eventtargets =
            (lookup.varDefInfo.TryFind "event_target"
             |> Option.defaultValue []
             |> List.map fst)

        let definedvars =
            (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)

        { scriptedLocCommands = lookup.scriptedLoc |> List.map (fun s -> s, [ scopeManager.AnyScope ])
          eventTargets = eventtargets |> List.map (fun s -> s, scopeManager.AnyScope)
          setVariables = definedvars |> LowerCaseStringSet }

    let globalLocalisation (game: GameObject) =
        let locParseErrors =
            game.LocalisationManager.LocalisationAPIs()
            <&!&> (fun (b, api) -> if b then validateLocalisationSyntax api.Results else OK)

        let globalTypeLoc = game.ValidationManager.ValidateGlobalLocalisation()

        game.Lookup.proccessedLoc
        |> validateProcessedLocalisation game.LocalisationManager.taggedLocalisationKeys
        <&&> locParseErrors
        <&&> globalTypeLoc
        |> (function
        | Invalid(_, es) -> es
        | _ -> [])

    let updateScriptedLoc (game: GameObject) =
        let rawLocs =
            game.Resources.AllEntities()
            |> List.choose (function
                | struct (f, _) when f.filepath.Contains("customizable_localisation") -> Some f.entity
                | _ -> None)
            |> List.collect (fun n -> n.Children)
            |> List.map (fun l -> l.TagText "name")

        game.Lookup.embeddedScriptedLoc <-
            game.Settings.embedded.cachedRuleMetadata
            |> Option.map (fun crm -> crm.scriptedLoc)
            |> Option.defaultValue []

        game.Lookup.scriptedLoc <- rawLocs

    let updateModifiers (game: GameObject) =
        game.Lookup.coreModifiers <- game.Settings.embedded.modifiers

    let addModifiersWithScopes (lookup: Lookup) =
        let modifierOptions (modifier: ActualModifier) =
            let requiredScopes = modifierCategoryManager.SupportedScopes modifier.category

            { Options.DefaultOptions with
                requiredScopes = requiredScopes }

        let processField =
            RulesParser.processTagAsField (scopeManager.ParseScope()) scopeManager.AnyScope scopeManager.ScopeGroups

        lookup.coreModifiers
        |> List.map (fun c ->
            AliasRule(
                "modifier",
                NewRule(LeafRule(processField c.tag, ValueField(ValueType.Float(-1E+12M, 1E+12M))), modifierOptions c)
            ))

    let updateLandedTitles (game: GameObject) =
        let fNode =
            fun (t: Node) result ->
                match t.Key with
                | x when x.StartsWith "e_" && t.TagText "landless" == "yes" -> ((Empire, true), x) :: result
                | x when x.StartsWith "e_" -> ((Empire, false), x) :: result
                | x when x.StartsWith "k_" && t.TagText "landless" == "yes" -> ((Kingdom, true), x) :: result
                | x when x.StartsWith "k_" -> ((Kingdom, false), x) :: result
                | x when x.StartsWith "d_" ->
                    match t.TagText "landless", t.TagText "mercenary", t.TagText "holy_order" with
                    | "yes", "yes", _
                    | "yes", _, "yes" -> ((Duchy_Hired, true), x) :: result
                    | "yes", _, _ -> ((Duchy_Normal, true), x) :: result
                    | _, "yes", _
                    | _, _, "yes" -> ((Duchy_Hired, false), x) :: result
                    | _ -> ((Duchy_Normal, false), x) :: result
                | x when x.StartsWith "c_" && t.TagText "landless" == "yes" -> ((County, true), x) :: result
                | x when x.StartsWith "c_" -> ((County, false), x) :: result
                | x when x.StartsWith "b_" && t.TagText "landless" == "yes" -> ((Barony, true), x) :: result
                | x when x.StartsWith "b_" -> ((Barony, false), x) :: result
                | _ -> result

        let titleEntities =
            (EntitySet(game.Resources.AllEntities()))
                .GlobMatchChildren("**/landed_titles/**/*.txt")

        let titles = titleEntities |> List.collect (foldNode7 fNode)

        let inner
            (ells, es, klls, ks, dnlls, dns, dhlls, dhs, clls, cs, blls, bs)
            (k: TitleType * bool, value: string)
            =
            match k with
            | Empire, true -> (value :: ells, es, klls, ks, dnlls, dns, dhlls, dhs, clls, cs, blls, bs)
            | Empire, false -> (ells, value :: es, klls, ks, dnlls, dns, dhlls, dhs, clls, cs, blls, bs)
            | Kingdom, true -> (ells, es, value :: klls, ks, dnlls, dns, dhlls, dhs, clls, cs, blls, bs)
            | Kingdom, false -> (ells, es, klls, value :: ks, dnlls, dns, dhlls, dhs, clls, cs, blls, bs)
            | Duchy_Normal, true -> (ells, es, klls, ks, value :: dnlls, dns, dhlls, dhs, clls, cs, blls, bs)
            | Duchy_Normal, false -> (ells, es, klls, ks, dnlls, value :: dns, dhlls, dhs, clls, cs, blls, bs)
            | Duchy_Hired, true -> (ells, es, klls, ks, dnlls, dns, value :: value :: dhlls, dhs, clls, cs, blls, bs)
            | Duchy_Hired, false -> (ells, es, klls, ks, dnlls, dns, dhlls, dhs, clls, cs, blls, bs)
            | County, true -> (ells, es, klls, ks, dnlls, dns, dhlls, dhs, value :: clls, cs, blls, bs)
            | County, false -> (ells, es, klls, ks, dnlls, dns, dhlls, dhs, clls, value :: cs, blls, bs)
            | Barony, true -> (ells, es, klls, ks, dnlls, dns, dhlls, dhs, clls, cs, value :: blls, bs)
            | Barony, false -> (ells, es, klls, ks, dnlls, dns, dhlls, dhs, clls, cs, blls, value :: bs)

        let ells, es, klls, ks, dnlls, dns, dhlls, dhs, clls, cs, blls, bs =
            titles |> List.fold inner ([], [], [], [], [], [], [], [], [], [], [], [])

        game.Lookup.CK2LandedTitles <-
            ((Empire, true), ells)
            :: ((Empire, false), es)
            :: ((Kingdom, true), klls)
            :: ((Kingdom, false), ks)
            :: ((Duchy_Normal, true), dnlls)
            :: ((Duchy_Normal, false), dns)
            :: ((Duchy_Hired, true), dhlls)
            :: ((Duchy_Hired, false), dhs)
            :: ((County, true), clls)
            :: ((County, false), cs)
            :: ((Barony, true), blls)
            :: [ ((Barony, false), bs) ]
            |> Map.ofList

    let createLandedTitleTypes (lookup: CK2Lookup) (map: Map<_, _>) =
        let ells =
            lookup.CK2LandedTitles.[Empire, true]
            |> List.map (fun e -> createTypeDefInfo false e range.Zero [] [ "empire"; "landless" ])

        let es =
            lookup.CK2LandedTitles.[Empire, false]
            |> List.map (fun e -> createTypeDefInfo false e range.Zero [] [ "empire"; "landed" ])

        let klls =
            lookup.CK2LandedTitles.[Kingdom, true]
            |> List.map (fun e -> createTypeDefInfo false e range.Zero [] [ "kingdom"; "landless" ])

        let ks =
            lookup.CK2LandedTitles.[Kingdom, false]
            |> List.map (fun e -> createTypeDefInfo false e range.Zero [] [ "kingdom"; "landed" ])

        let dllns =
            lookup.CK2LandedTitles.[Duchy_Normal, true]
            |> List.map (fun e -> createTypeDefInfo false e range.Zero [] [ "duchy"; "duchy_normal"; "landless" ])

        let dns =
            lookup.CK2LandedTitles.[Duchy_Normal, false]
            |> List.map (fun e -> createTypeDefInfo false e range.Zero [] [ "duchy"; "duchy_normal"; "landed" ])

        let dllhs =
            lookup.CK2LandedTitles.[Duchy_Hired, true]
            |> List.map (fun e -> createTypeDefInfo false e range.Zero [] [ "duchy"; "duchy_hired"; "landless" ])

        let dhs =
            lookup.CK2LandedTitles.[Duchy_Hired, false]
            |> List.map (fun e -> createTypeDefInfo false e range.Zero [] [ "duchy"; "duchy_hired"; "landed" ])

        let clls =
            lookup.CK2LandedTitles.[County, true]
            |> List.map (fun e -> createTypeDefInfo false e range.Zero [] [ "county"; "landless" ])

        let cs =
            lookup.CK2LandedTitles.[County, false]
            |> List.map (fun e -> createTypeDefInfo false e range.Zero [] [ "county"; "landed" ])

        let blls =
            lookup.CK2LandedTitles.[Barony, true]
            |> List.map (fun e -> createTypeDefInfo false e range.Zero [] [ "barony"; "landless" ])

        let bs =
            lookup.CK2LandedTitles.[Barony, false]
            |> List.map (fun e -> createTypeDefInfo false e range.Zero [] [ "barony"; "landed" ])

        map
        |> Map.add "title.empire" (es @ ells)
        |> Map.add "title.kingdom" (ks @ klls)
        |> Map.add "title.duchy" (dllns @ dns @ dllhs @ dhs)
        |> Map.add "title.duchy_hired" (dllhs @ dhs)
        |> Map.add "title.duchy_normal" (dllns @ dns)
        |> Map.add "title.county" (clls @ cs)
        |> Map.add "title.barony" (blls @ bs)
        |> Map.add "title.landless" (ells @ klls @ dllns @ dllhs @ clls @ blls)
        |> Map.add "title.landed" (es @ ks @ dns @ dhs @ cs @ bs)
        |> Map.add "title" (es @ ells @ ks @ klls @ dns @ dllns @ dhs @ dllhs @ cs @ clls @ bs @ blls)

    let updateProvinces (game: GameObject) =
        let provinceFile =
            game.Resources.GetResources()
            |> List.choose (function
                | FileWithContentResource(_, e) -> Some e
                | _ -> None)
            |> List.tryFind (fun f -> f.overwrite <> Overwritten && Path.GetFileName(f.filepath) = "definition.csv")

        match provinceFile with
        | None -> ()
        | Some pf ->
            let lines = pf.filetext.Split([| "\r\n"; "\r"; "\n" |], StringSplitOptions.None)

            let provinces =
                lines
                |> Array.choose (fun l -> l.Split([| ';' |], 2, StringSplitOptions.RemoveEmptyEntries) |> Array.tryHead)
                |> List.ofArray

            game.Lookup.CK2provinces <- provinces

    let updateScriptedEffects (lookup: Lookup) (rules: RootRule list) (embeddedSettings: EmbeddedSettings) =
        let effects =
            rules
            |> List.choose (function
                | AliasRule("effect", r) -> Some r
                | _ -> None)

        let ruleToEffect (r, o) =
            let name =
                match r with
                | LeafRule(SpecificField(SpecificValue n), _) -> StringResource.stringManager.GetStringForID n.normal
                | NodeRule(SpecificField(SpecificValue n), _) -> StringResource.stringManager.GetStringForID n.normal
                | _ -> ""

            DocEffect(
                name,
                o.requiredScopes,
                o.pushScope,
                EffectType.Effect,
                o.description |> Option.defaultValue "",
                ""
            )

        (effects |> List.map ruleToEffect |> List.map (fun e -> e :> Effect))

    let updateScriptedTriggers (lookup: Lookup) (rules: RootRule list) (embeddedSettings: EmbeddedSettings) =
        let effects =
            rules
            |> List.choose (function
                | AliasRule("trigger", r) -> Some r
                | _ -> None)

        let ruleToTrigger (r, o) =
            let name =
                match r with
                | LeafRule(SpecificField(SpecificValue n), _) -> StringResource.stringManager.GetStringForID n.normal
                | NodeRule(SpecificField(SpecificValue n), _) -> StringResource.stringManager.GetStringForID n.normal
                | _ -> ""

            DocEffect(
                name,
                o.requiredScopes,
                o.pushScope,
                EffectType.Trigger,
                o.description |> Option.defaultValue "",
                ""
            )

        (effects |> List.map ruleToTrigger |> List.map (fun e -> e :> Effect))

    let addModifiersAsTypes (lookup: Lookup) (typesMap: Map<string, TypeDefInfo list>) =
        typesMap.Add(
            "modifier",
            lookup.coreModifiers
            |> List.map (fun m -> createTypeDefInfo false m.tag range.Zero [] [])
        )


    let loadConfigRulesHook rules (lookup: Lookup) embedded =
        let ts = updateScriptedTriggers lookup rules embedded
        let es = updateScriptedEffects lookup rules embedded
        let ls = updateEventTargetLinks embedded
        lookup.allCoreLinks <- ts @ es @ ls
        rules @ addModifiersWithScopes lookup

    let refreshConfigBeforeFirstTypesHook (lookup: CK2Lookup) _ _ =
        let modifierEnums =
            { key = "modifiers"
              values = lookup.coreModifiers |> List.map (fun m -> m.tag)
              description = "Modifiers"
              valuesWithRange = lookup.coreModifiers |> List.map (fun m -> m.tag, None) }

        let provinceEnums =
            { key = "provinces"
              description = "provinces"
              values = lookup.CK2provinces
              valuesWithRange = lookup.CK2provinces |> List.map (fun x -> x, None) }

        lookup.enumDefs <-
            lookup.enumDefs
            |> Map.add modifierEnums.key (modifierEnums.description, modifierEnums.valuesWithRange)
            |> Map.add provinceEnums.key (provinceEnums.description, provinceEnums.valuesWithRange)

    let refreshConfigAfterFirstTypesHook (lookup: CK2Lookup) _ (embeddedSettings: EmbeddedSettings) =
        lookup.typeDefInfo <- createLandedTitleTypes lookup lookup.typeDefInfo |> addModifiersAsTypes lookup

        lookup.allCoreLinks <-
            lookup.triggers
            @ lookup.effects
            @ updateEventTargetLinks embeddedSettings
            @ addDataEventTargetLinks lookup embeddedSettings false

    let refreshConfigAfterVarDefHook
        (lookup: Lookup)
        (resources: IResourceAPI<_>)
        (embeddedSettings: EmbeddedSettings)
        =
        lookup.allCoreLinks <-
            lookup.triggers
            @ lookup.effects
            @ updateEventTargetLinks embeddedSettings
            @ addDataEventTargetLinks lookup embeddedSettings false


    let afterInit (game: GameObject) =
        // updateScriptedTriggers()
        // updateScriptedEffects()
        // updateStaticodifiers()
        // updateScriptedLoc(game)
        // updateDefinedVariables()
        updateModifiers (game)
        // updateLegacyGovernments(game)
        // updateTechnologies()
        updateLandedTitles (game)
        updateProvinces (game)
    // game.LocalisationManager.UpdateAllLocalisation()
    // updateTypeDef game game.Settings.rules
    // game.LocalisationManager.UpdateAllLocalisation()

    let createEmbeddedSettings embeddedFiles cachedResourceData (configs: (string * string) list) cachedRuleMetadata =
        configs
        |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
        |> (fun f -> UtilityParser.initializeScopes f (Some defaultScopeInputs))

        configs
        |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifier_categories.cwt")
        |> (fun f -> UtilityParser.initializeModifierCategories f (Some(defaultModifiersInputs ())))

        let ck2Mods =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifiers.cwt")
            |> Option.map (fun (fn, ft) -> UtilityParser.loadModifiers fn ft)
            |> Option.defaultValue []

        let ck2LocCommands =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "localisation.cwt")
            |> Option.map (fun (fn, ft) -> UtilityParser.loadLocCommands fn ft)
            |> Option.defaultValue ([], [], [])

        let ck2EventTargetLinks =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "links.cwt")
            |> Option.map (fun (fn, ft) ->
                UtilityParser.loadEventTargetLinks
                    scopeManager.AnyScope
                    (scopeManager.ParseScope())
                    scopeManager.AllScopes
                    fn
                    ft)
            |> Option.defaultValue (CWTools.Process.Scopes.CK2.scopedEffects () |> List.map SimpleLink)

        let featureSettings =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "settings.cwt")
            |> Option.bind (fun (fn, ft) -> UtilityParser.loadSettingsFile fn ft)
            |> Option.defaultValue CWTools.Parser.UtilityParser.FeatureSettings.Default

        let triggers, effects = ([], [])

        { triggers = triggers
          effects = effects
          modifiers = ck2Mods
          embeddedFiles = embeddedFiles
          cachedResourceData = cachedResourceData
          localisationCommands = Legacy ck2LocCommands
          eventTargetLinks = ck2EventTargetLinks
          cachedRuleMetadata = cachedRuleMetadata
          featureSettings = featureSettings }

type CK2Settings = GameSetupSettings<CK2Lookup>
open CK2GameFunctions

type CK2Game(setupSettings: CK2Settings) =
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
          initialLookup = CK2Lookup()
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
            initialLookup = CK2Lookup() }

    let legacyLocDataTypes =
        settings.embedded.localisationCommands
        |> function
            | Legacy(c, v, links) -> (c, v, links)
            | _ -> ([], [], [])

    let processLocalisationFunction lookup =
        (createLocalisationFunctions CK2.locStaticSettings createLocDynamicSettings legacyLocDataTypes lookup)
        |> fst

    let validationLocalisationCommandFunction lookup =
        (createLocalisationFunctions CK2.locStaticSettings createLocDynamicSettings legacyLocDataTypes lookup)
        |> snd


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
          defaultLang = CK2 CK2Lang.Default
          oneToOneScopesNames = oneToOneScopesNames
          loadConfigRulesHook = loadConfigRulesHook
          refreshConfigBeforeFirstTypesHook = refreshConfigBeforeFirstTypesHook
          refreshConfigAfterFirstTypesHook = refreshConfigAfterFirstTypesHook
          refreshConfigAfterVarDefHook = refreshConfigAfterVarDefHook
          processLocalisation = processLocalisationFunction
          validateLocalisation = validationLocalisationCommandFunction }

    let game =
        GameObject.CreateGame
            ((settings,
              "crusader kings ii",
              scriptFolders,
              Compute.computeCK2Data,
              Compute.computeCK2DataUpdate,
              (CK2LocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
              processLocalisationFunction,
              validationLocalisationCommandFunction,
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

    let references =
        References<_>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))

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

    interface IGame<CK2ComputedData> with
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
        member __.StaticModifiers() = [] //lookup.staticModifiers
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
                (CK2 CK2Lang.English)
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
        member __.GetPossibleCodeEdits file text = []
        member __.GetCodeEdits file text = None

        member __.GetEventGraphData: GraphDataRequest =
            (fun files gameType depth ->
                graphEventDataForFiles references game.ResourceManager lookup files gameType depth)

        member __.GetEmbeddedMetadata() =
            getEmbeddedMetadata lookup game.LocalisationManager game.ResourceManager
