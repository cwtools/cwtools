namespace CWTools.Games.HOI4

open CWTools.Common.HOI4Constants
open CWTools.Game
open CWTools.Localisation
open CWTools.Utilities.Utils2
open CWTools.Games
open CWTools.Common
open CWTools.Localisation.HOI4
open CWTools.Utilities
open System.IO
open CWTools.Validation.Common.CommonValidation
open CWTools.Rules
open CWTools.Process.Scopes.HOI4
open CWTools.Process.Scopes.Scopes
open System.Text
open CWTools.Games.LanguageFeatures
open System
open CWTools.Games.Helpers
open CWTools.Parser
open CWTools.Process.Localisation
open System.Linq

module HOI4GameFunctions =
    type GameObject = GameObject<HOI4ComputedData, HOI4Lookup>

    let createLocDynamicSettings (lookup: Lookup) =
        //eprintfn "clds %A" (lookup.enumDefs.TryFind "country_tags")
        let eventtargets =
            seq {

                yield!
                    lookup.varDefInfo.TryFind "event_target"
                    |> Option.defaultValue [||]
                    |> Seq.map fst

                yield!
                    lookup.varDefInfo.TryFind "global_event_target"
                    |> Option.defaultValue [||]
                    |> Seq.map fst

                yield! lookup.typeDefInfo.TryFind "state" |> Option.defaultValue [||] |> Seq.map _.id

                yield!
                    lookup.enumDefs.TryFind "country_tags"
                    |> Option.map (fun x -> (snd x) |> Seq.map fst)
                    |> Option.defaultValue Seq.empty
            }

        let definedvars =
            seq {
                yield! (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [||] |> Seq.map fst)

                yield!
                    (lookup.varDefInfo.TryFind "saved_name"
                     |> Option.defaultValue [||]
                     |> Seq.map fst)

                yield!
                    (lookup.varDefInfo.TryFind "exiled_ruler"
                     |> Option.defaultValue [||]
                     |> Seq.map fst)
            }
            |> IgnoreCaseStringSet

        { scriptedLocCommands =
            lookup.scriptedLoc
            |> Seq.map (fun s -> s, [ scopeManager.AnyScope ])
            |> Array.ofSeq
          eventTargets = eventtargets |> Seq.map (fun s -> s, scopeManager.AnyScope) |> Array.ofSeq
          setVariables = definedvars }

    let updateModifiers (game: GameObject) =
        game.Lookup.coreModifiers <- game.Settings.embedded.modifiers

    let updateProvinces (game: GameObject) =
        let provinceFile =
            game.Resources.GetResources()
            |> List.choose (function
                | FileWithContentResource(_, e) -> Some e
                | _ -> None)
            |> List.tryFind (fun f ->
                f.overwrite <> Overwrite.Overwritten
                && Path.GetFileName(f.filepath.AsSpan()).Equals("definition.csv", StringComparison.OrdinalIgnoreCase))

        match provinceFile with
        | None -> ()
        | Some pf ->
            let lines = pf.filetext.Split([| "\r\n"; "\r"; "\n" |], StringSplitOptions.None)

            let provinces =
                lines
                |> Array.choose (fun l -> l.Split(';', 2, StringSplitOptions.RemoveEmptyEntries) |> Array.tryHead)


            game.Lookup.HOI4provinces <- provinces

    let updateScriptedLoc (game: GameObject) =
        let rawLocs =
            game.Resources.AllEntities()
            |> List.choose (function
                | struct (f, _) when f.filepath.Contains("scripted_localisation") -> Some f.entity
                | _ -> None)
            |> List.collect (fun n -> n.Children)
            |> List.map (fun l -> l.TagText "name")

        game.Lookup.embeddedScriptedLoc <-
            game.Settings.embedded.cachedRuleMetadata
            |> Option.map (fun crm -> crm.scriptedLoc)
            |> Option.defaultValue []

        game.Lookup.scriptedLoc <- rawLocs

    let ruleToEffect (rule, effectType) =
        let r, o = rule

        let name =
            match r with
            | LeafRule(SpecificField(SpecificValue n), _) -> StringResource.stringManager.GetStringForID n.normal
            | NodeRule(SpecificField(SpecificValue n), _) -> StringResource.stringManager.GetStringForID n.normal
            | _ -> ""

        DocEffect(name, o.requiredScopes, o.pushScope, effectType, o.description |> Option.defaultValue "", "")
        :> Effect

    let updateScriptedEffects (rules: RootRule array) (states: _ array) (countries: _ array) =
        let effects =
            rules
            |> Array.choose (function
                | AliasRule("effect", r) -> Some(ruleToEffect (r, EffectType.Effect))
                | _ -> None)

        let stateEffects =
            states
            |> Array.map (fun p ->
                ScopedEffect(
                    p,
                    scopeManager.AllScopes,
                    Some(scopeManager.ParseScope () "State"),
                    EffectType.Link,
                    defaultDesc,
                    "",
                    true
                )
                :> Effect)

        let countryEffects =
            countries
            |> Array.map (fun p ->
                ScopedEffect(
                    p,
                    scopeManager.AllScopes,
                    Some(scopeManager.ParseScope () "Country"),
                    EffectType.Link,
                    defaultDesc,
                    "",
                    true
                )
                :> Effect)

        Array.concat [| effects; stateEffects; countryEffects |]

    let updateScriptedTriggers (rules: RootRule array) (states: _ array) (countries: _ array) =
        let effects =
            rules
            |> Array.choose (function
                | AliasRule("trigger", r) -> Some(ruleToEffect (r, EffectType.Trigger))
                | _ -> None)

        let stateEffects =
            (states
             |> Array.map (fun p ->
                 ScopedEffect(
                     p,
                     scopeManager.AllScopes,
                     Some(scopeManager.ParseScope () "State"),
                     EffectType.Link,
                     defaultDesc,
                     "",
                     true
                 )
                 :> Effect))

        let countryEffects =
            (countries
             |> Array.map (fun p ->
                 ScopedEffect(
                     p,
                     scopeManager.AllScopes,
                     Some(scopeManager.ParseScope () "Country"),
                     EffectType.Link,
                     defaultDesc,
                     "",
                     true
                 )
                 :> Effect))

        Array.concat [| effects; stateEffects; countryEffects |]

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

    let loadConfigRulesHook (rules: RootRule array) (lookup: Lookup) embedded =
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ updateEventTargetLinks embedded
        rules.Concat(addModifiersWithScopes lookup).ToArray()

    let refreshConfigBeforeFirstTypesHook (lookup: HOI4Lookup) _ _ =
        let provinceEnums =
            { key = "provinces"
              description = "provinces"
              values = lookup.HOI4provinces
              valuesWithRange = lookup.HOI4provinces |> Array.map (fun x -> x, None) }

        lookup.enumDefs <-
            lookup.enumDefs
            |> Map.add provinceEnums.key (provinceEnums.description, provinceEnums.valuesWithRange)

    let refreshConfigAfterFirstTypesHook (lookup: Lookup) _ (embeddedSettings: EmbeddedSettings) =
        let states =
            lookup.typeDefInfo.TryFind "state"
            |> Option.map (fun sl ->
                sl
                |> Array.map (fun tdi -> StringResource.stringManager.InternIdentifierToken tdi.id))
            |> Option.defaultValue [||]

        let countries =
            lookup.enumDefs.TryFind "country_tag"
            |> Option.map (fun x -> (snd x) |> Array.map (fst >> StringResource.stringManager.InternIdentifierToken))
            |> Option.defaultValue [||]

        let ts = updateScriptedTriggers lookup.configRules states countries
        let es = updateScriptedEffects lookup.configRules states countries

        let ls =
            updateEventTargetLinks embeddedSettings
            @ addDataEventTargetLinks lookup embeddedSettings false

        lookup.allCoreLinks <- ts.Concat(es).Concat(ls) |> List.ofSeq

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
        updateModifiers (game)
        updateProvinces (game)
        updateScriptedLoc (game)

    let createEmbeddedSettings
        embeddedFiles
        cachedResourceData
        (configs: (string * string) list)
        (cachedRuleMetadata: CachedRuleMetadata option)
        =

        initializeScopesAndModifierCategories configs defaultScopeInputs defaultModifiersInputs

        let hoi4Mods = getActualModifiers configs

        let hoi4LocCommands =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "localisation.cwt")
            |> Option.map (fun (fn, ft) -> UtilityParser.loadLocCommands fn ft)
            |> Option.defaultValue ([], [], [])

        let triggers, effects = ([], [])

        let eventTargetLinks =
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

        let featureSettings = getFeatureSettings configs

        { triggers = triggers
          effects = effects
          modifiers = hoi4Mods
          embeddedFiles = embeddedFiles
          cachedResourceData = cachedResourceData
          localisationCommands = Legacy hoi4LocCommands
          eventTargetLinks = eventTargetLinks
          cachedRuleMetadata = cachedRuleMetadata
          featureSettings = featureSettings }

type HOI4Settings = GameSetupSettings<HOI4Lookup>
open HOI4GameFunctions

type HOI4Game(setupSettings: HOI4Settings) =
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

    let validationSettings =
        { validators =
            [ validateIfWithNoEffect, "ifnoeffect"
              validateRedundantANDWithNOT, "AND"
              validateOptimisations embeddedSettings.featureSettings.ListMergeOptimisations, "opt" ]
          experimentalValidators = []
          heavyExperimentalValidators = []
          experimental = setupSettings.validation.experimental
          fileValidators = []
          lookupValidators = commonValidationRules
          lookupFileValidators = []
          useRules = true
          debugRulesOnly = false
          localisationValidators = [] }

    let settings =
        { rootDirectories = setupSettings.rootDirectories
          excludeGlobPatterns = setupSettings.excludeGlobPatterns
          embedded = embeddedSettings
          GameSettings.rules = setupSettings.rules
          validation = setupSettings.validation
          scriptFolders = setupSettings.scriptFolders
          modFilter = setupSettings.modFilter
          initialLookup = HOI4Lookup()
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
            initialLookup = HOI4Lookup() }


    let legacyLocDataTypes =
        settings.embedded.localisationCommands
        |> function
            | Legacy(c, v, links) -> (c, v, links)
            | _ -> ([], [], [])

    let processLocalisationFunction lookup =
        (createLocalisationFunctions HOI4.locStaticSettings createLocDynamicSettings legacyLocDataTypes lookup)

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
          defaultLang = HOI4 HOI4Lang.Default
          oneToOneScopesNames = oneToOneScopesNames
          loadConfigRulesHook = loadConfigRulesHook
          refreshConfigBeforeFirstTypesHook = refreshConfigBeforeFirstTypesHook
          refreshConfigAfterFirstTypesHook = refreshConfigAfterFirstTypesHook
          refreshConfigAfterVarDefHook = refreshConfigAfterVarDefHook
          locFunctions = processLocalisationFunction }

    let game =
        GameObject.CreateGame
            (settings,
             "hearts of iron iv",
             scriptFolders,
             Compute.computeHOI4Data,
             Compute.computeHOI4DataUpdate,
             (HOI4LocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
             processLocalisationFunction,
             defaultContext,
             noneContext,
             Encoding.UTF8,
             Encoding.GetEncoding(1252),
             validationSettings,
             Hooks.globalLocalisation,
             (fun _ _ -> ()),
             ".yml",
             rulesManagerSettings,
             setupSettings.debugSettings)
            afterInit

    let lookup = game.Lookup
    let resources = game.Resources
    let fileManager = game.FileManager

    let references =
        References<_>(resources, lookup, game.LocalisationManager.GetCleanLocalisationAPIs())

    let getEmbeddedFiles () =
        settings.embedded.embeddedFiles
        |> List.map (fun (fn, f) -> "embedded", "embeddedfiles/" + fn, f)

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

    member _.Lookup = lookup

    interface IGame<ComputedData> with
        member _.ParserErrors() = parseErrors ()

        member _.ValidationErrors() =
            let s, d = game.ValidationManager.Validate(false, resources.ValidatableEntities()) in s @ d

        member _.LocalisationErrors(force: bool, forceGlobal: bool) =
            getLocalisationErrors game Hooks.globalLocalisation (force, forceGlobal)

        member _.Folders() = fileManager.AllFolders()
        member _.AllFiles() = resources.GetResources()

        member _.AllLoadedLocalisation() =
            game.LocalisationManager.LocalisationFileNames()

        member _.ScriptedTriggers() = lookup.triggers
        member _.ScriptedEffects() = lookup.effects
        member _.StaticModifiers() = [] //lookup.staticModifiers
        member _.UpdateFile shallow file text = game.UpdateFile shallow file text
        member _.AllEntities() = resources.AllEntities()

        member _.References() =
            References<HOI4ComputedData>(resources, lookup, game.LocalisationManager.GetCleanLocalisationAPIs())

        member _.Complete pos file text =
            completion fileManager game.completionService game.InfoService game.ResourceManager pos file text

        member _.ScopesAtPos pos file text =
            scopesAtPos fileManager game.ResourceManager game.InfoService scopeManager.AnyScope pos file text

        member _.GoToType pos file text =
            getInfoAtPos
                fileManager
                game.ResourceManager
                game.InfoService
                game.LocalisationManager
                lookup
                (HOI4 HOI4Lang.English)
                pos
                file
                text

        member _.FindAllRefs pos file text =
            findAllRefsFromPos fileManager game.ResourceManager game.InfoService pos file text

        member _.InfoAtPos pos file text = game.InfoAtPos pos file text

        member _.ReplaceConfigRules rules =
            game.ReplaceConfigRules
                { ruleFiles = rules
                  validateRules = true
                  debugRulesOnly = false
                  debugMode = false } //refreshRuleCaches game (Some { ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})

        member _.RefreshCaches() = game.RefreshCaches()

        member _.RefreshLocalisationCaches() =
            game.LocalisationManager.UpdateProcessedLocalisation()

        member _.ForceRecompute() = resources.ForceRecompute()
        member _.Types() = game.Lookup.typeDefInfo
        member _.TypeDefs() = game.Lookup.typeDefs
        member _.GetPossibleCodeEdits file text = []
        member _.GetCodeEdits file text = None //getFastTrigger fileManager game.ResourceManager file text

        member _.GetEventGraphData: GraphDataRequest =
            (fun files gameType depth ->
                graphEventDataForFiles references game.ResourceManager lookup files gameType depth)

        member _.GetEmbeddedMetadata() =
            getEmbeddedMetadata lookup game.LocalisationManager game.ResourceManager
