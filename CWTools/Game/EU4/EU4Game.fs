namespace CWTools.Games.EU4

open CWTools.Localisation
open CWTools.Utilities.Utils2
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Games
open CWTools.Common
open FSharp.Collections.ParallelSeq
open CWTools.Localisation.EU4
open CWTools.Utilities.Utils
open CWTools.Utilities.Position
open CWTools.Utilities
open CWTools.Validation.Common.CommonValidation
open CWTools.Rules
open CWTools.Common.EU4Constants
open CWTools.Process.Scopes.EU4
open CWTools.Process.Scopes.Scopes
open System.Text
open CWTools.Games.LanguageFeatures
open CWTools.Validation.EU4.EU4Validation
open CWTools.Games.Helpers
open CWTools.Games.Compute.EU4
open CWTools.Parser
open System.IO
open CWTools.Process.Localisation
open System.Linq

module EU4GameFunctions =
    type GameObject = GameObject<EU4ComputedData, EU4Lookup>

    let createLocDynamicSettings (lookup: Lookup) =
        let eventTargets = ResizeArray<string>(32)

        eventTargets.AddRange(
            lookup.varDefInfo.TryFind "event_target"
            |> Option.defaultValue [||]
            |> Seq.map fst
        )

        eventTargets.AddRange(
            lookup.varDefInfo.TryFind "global_event_target"
            |> Option.defaultValue [||]
            |> Array.map fst
        )

        eventTargets.AddRange(
            lookup.typeDefInfo.TryFind "province_id"
            |> Option.defaultValue [||]
            |> Seq.map _.id
        )

        eventTargets.AddRange(
            lookup.enumDefs.TryFind "country_tags"
            |> Option.map (fun x -> (snd x) |> Seq.map fst)
            |> Option.defaultValue [||]
        )

        let definedVars =
            (lookup.varDefInfo.TryFind "variable"
             |> Option.defaultValue [||]
             |> Array.map fst)
                .Concat(
                    lookup.varDefInfo.TryFind "exiled_ruler"
                    |> Option.defaultValue [||]
                    |> Seq.map fst
                )
                .Concat(
                    lookup.varDefInfo.TryFind "saved_name"
                    |> Option.defaultValue [||]
                    |> Seq.map fst
                )


        { scriptedLocCommands = lookup.scriptedLoc |> Array.map (fun s -> s, [ scopeManager.AnyScope ])
          eventTargets = eventTargets.Select(fun s -> s, scopeManager.AnyScope).ToArray()
          setVariables = definedVars |> IgnoreCaseStringSet }

    let globalLocalisation (game: GameObject) =
        let globalTypeLoc = game.ValidationManager.ValidateGlobalLocalisation()

        game.Lookup.proccessedLoc
        |> validateProcessedLocalisation game.LocalisationManager.taggedLocalisationKeys
        <&&> globalTypeLoc
        |> (function
        | Invalid(_, es) -> es
        | _ -> [])

    let updateScriptedLoc (game: GameObject) =
        let rawLocs =
            game.Resources.AllEntities()
            |> Seq.choose (function
                | struct (f, _) when f.filepath.Contains("customizable_localization") -> Some f.entity
                | _ -> None)
            |> Seq.collect (fun n -> n.Nodes)
            |> Seq.map (fun l -> l.TagText "name")
            |> Seq.toArray

        game.Lookup.embeddedScriptedLoc <-
            game.Settings.embedded.cachedRuleMetadata
            |> Option.map _.scriptedLoc
            |> Option.defaultValue [||]

        game.Lookup.scriptedLoc <- rawLocs

    let updateModifiers (game: GameObject) =
        game.Lookup.coreModifiers <-
            addGeneratedModifiers game.Settings.embedded.modifiers (EntitySet(game.Resources.AllEntities()))

    let updateLegacyGovernments (game: GameObject) =
        let es = game.Resources.AllEntities() |> EntitySet
        let allReforms = es.GlobMatchChildren("**/government_reforms/*.txt")

        let legacies =
            allReforms
            |> List.choose (fun n ->
                if n.TagText "legacy_government" == "yes" then
                    Some n.Key
                else
                    None)
            |> Set.ofList

        let legacyRefs =
            allReforms
            |> List.choose (fun n ->
                if n.Has "legacy_equivalent" then
                    Some(n.TagText "legacy_equivalent")
                else
                    None)
            |> Set.ofList

        let legacyOnly = Set.difference legacies legacyRefs |> Set.toArray
        game.Lookup.EU4TrueLegacyGovernments <- legacyOnly

    let addModifiersWithScopes (lookup: Lookup) =
        let processField =
            RulesParser.processTagAsField (scopeManager.ParseScope()) scopeManager.AnyScope scopeManager.ScopeGroups

        lookup.coreModifiers
        |> Array.map (fun c ->
            AliasRule(
                "modifier",
                NewRule(
                    LeafRule(processField c.tag, ValueField(ValueType.Float(-1E+12M, 1E+12M))),
                    Options.DefaultOptions
                )
            ))

    let updateScriptedEffects (rules: RootRule seq) =
        let effects =
            rules
            |> Seq.choose (function
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

        (effects |> Seq.map ruleToEffect |> Seq.cast<Effect> |> Seq.toList)
        @ (scopedEffects () |> List.map (fun e -> e :> Effect))

    let updateScriptedTriggers (rules: RootRule seq) =
        let effects =
            rules
            |> Seq.choose (function
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

        (effects |> Seq.map ruleToTrigger |> Seq.cast<Effect> |> Seq.toList)
        @ (scopedEffects () |> List.map (fun e -> e :> Effect))

    let addModifiersAsTypes (lookup: Lookup) (typesMap: Map<string, TypeDefInfo array>) =
        typesMap.Add(
            "modifier",
            lookup.coreModifiers
            |> Array.map (fun m -> createTypeDefInfo false m.tag range.Zero [] [])
        )

    let loadConfigRulesHook (rules: RootRule array) (lookup: Lookup) embedded =
        let ts = updateScriptedTriggers rules
        let es = updateScriptedEffects rules
        let ls = updateEventTargetLinks embedded
        lookup.allCoreLinks <- ts @ es @ ls
        rules.Concat(addModifiersWithScopes lookup).ToArray()

    let refreshConfigBeforeFirstTypesHook (lookup: EU4Lookup) (resources: IResourceAPI<EU4ComputedData>) _ =
        lookup.EU4ScriptedEffectKeys <-
            Array.append
                [| "scaled_skill" |]
                (resources.AllEntities()
                 |> PSeq.map (fun struct (e, l) ->
                     (l.Force().ScriptedEffectParams
                      |> (Option.defaultWith (fun () -> getScriptedEffectParamsEntity e))))
                 |> Seq.collect id
                 |> Seq.toArray)

        let scriptedEffectParmas =
            { key = "scripted_effect_params"
              description = "Scripted effect parameter"
              values = lookup.EU4ScriptedEffectKeys
              valuesWithRange = lookup.EU4ScriptedEffectKeys |> Array.map (fun x -> x, None) }

        let paramsDValues = lookup.EU4ScriptedEffectKeys |> Array.map (fun k -> $"$%s{k}$")

        let scriptedEffectParmasD =
            { key = "scripted_effect_params_dollar"
              description = "Scripted effect parameter"
              values = paramsDValues
              valuesWithRange = paramsDValues |> Array.map (fun x -> x, None) }

        let modifierEnums =
            { key = "modifiers"
              values = lookup.coreModifiers |> Array.map _.tag
              description = "Modifiers"
              valuesWithRange = lookup.coreModifiers |> Array.map (fun m -> m.tag, None) }

        let legacyGovEnums =
            { key = "hardcoded_legacy_only_governments"
              values = lookup.EU4TrueLegacyGovernments
              description = "Legacy only government"
              valuesWithRange = lookup.EU4TrueLegacyGovernments |> Array.map (fun x -> x, None) }

        lookup.enumDefs <-
            lookup.enumDefs
            |> Map.add scriptedEffectParmas.key (scriptedEffectParmas.description, scriptedEffectParmas.valuesWithRange)
            |> Map.add
                scriptedEffectParmasD.key
                (scriptedEffectParmasD.description, scriptedEffectParmasD.valuesWithRange)
            |> Map.add modifierEnums.key (modifierEnums.description, modifierEnums.valuesWithRange)
            |> Map.add legacyGovEnums.key (legacyGovEnums.description, legacyGovEnums.valuesWithRange)

    let refreshConfigAfterFirstTypesHook (lookup: Lookup) _ (embeddedSettings: EmbeddedSettings) =
        lookup.typeDefInfo <- lookup.typeDefInfo |> addModifiersAsTypes lookup

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
        updateScriptedLoc (game)
        updateModifiers (game)
        updateLegacyGovernments (game)

    let createEmbeddedSettings embeddedFiles cachedResourceData (configs: (string * string) list) cachedRuleMetadata =
        initializeScopesAndModifierCategories configs defaultScopeInputs defaultModifiersInputs

        let triggers, effects = ([], [])
        let modifiers = getActualModifiers configs

        let eu4LocCommands =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "localisation.cwt")
            |> Option.map (fun (fn, ft) -> UtilityParser.loadLocCommands fn ft)
            |> Option.defaultValue ([], [], [])

        let eu4EventTargetLinks =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "links.cwt")
            |> Option.map (fun (fn, ft) ->
                UtilityParser.loadEventTargetLinks
                    scopeManager.AnyScope
                    (scopeManager.ParseScope())
                    scopeManager.AllScopes
                    fn
                    ft)
            |> Option.defaultValue (CWTools.Process.Scopes.EU4.scopedEffects () |> List.map SimpleLink)

        let featureSettings = getFeatureSettings configs

        { triggers = triggers
          effects = effects
          modifiers = modifiers
          embeddedFiles = embeddedFiles
          cachedResourceData = cachedResourceData
          localisationCommands = Legacy eu4LocCommands
          eventTargetLinks = eu4EventTargetLinks
          cachedRuleMetadata = cachedRuleMetadata
          featureSettings = featureSettings }

type EU4Settings = GameSetupSettings<EU4Lookup>
open EU4GameFunctions

type EU4Game(setupSettings: EU4Settings) =
    let validationSettings =
        { validators = [ validateEU4NaiveNot, "not"; validateIfWithNoEffect, "ifnoeffect" ]
          experimentalValidators = []
          heavyExperimentalValidators = []
          experimental = false
          fileValidators = []
          lookupValidators = commonValidationRules
          lookupFileValidators = [ valScriptedEffectParams, "scripted_effects" ]
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
          initialLookup = EU4Lookup()
          maxFileSize = setupSettings.maxFileSize
          enableInlineScripts = false }

    do
        if scopeManager.Initialized |> not then
            eprintfn "%A has no scopes" (settings.rootDirectories |> Array.head)
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
            initialLookup = EU4Lookup() }


    let legacyLocDataTypes =
        settings.embedded.localisationCommands
        |> function
            | Legacy(c, v, links) -> (c, v, links)
            | _ -> ([], [], [])

    let processLocalisationFunction lookup =
        (createLocalisationFunctions EU4.locStaticSettings createLocDynamicSettings legacyLocDataTypes lookup)


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
          defaultLang = EU4 EU4Lang.Default
          oneToOneScopesNames = oneToOneScopesNames
          loadConfigRulesHook = loadConfigRulesHook
          refreshConfigBeforeFirstTypesHook = refreshConfigBeforeFirstTypesHook
          refreshConfigAfterFirstTypesHook = refreshConfigAfterFirstTypesHook
          refreshConfigAfterVarDefHook = refreshConfigAfterVarDefHook
          locFunctions = processLocalisationFunction }

    let game =
        GameObject.CreateGame
            ((settings,
              "europa universalis iv",
              scriptFolders,
              Compute.EU4.computeEU4Data,
              Compute.EU4.computeEU4DataUpdate,
              (EU4LocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
              processLocalisationFunction,
              defaultContext,
              noneContext,
              Encoding.UTF8,
              Encoding.GetEncoding(1252),
              validationSettings,
              globalLocalisation,
              (fun _ _ -> ()),
              ".yml",
              rulesManagerSettings,
              setupSettings.debugSettings))
            afterInit

    let lookup = game.Lookup
    let resources = game.Resources
    let fileManager = game.FileManager

    let references =
        References<_>(resources, lookup, game.LocalisationManager.GetCleanLocalisationAPIs())

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

    interface IGame<EU4ComputedData> with
        member _.ParserErrors() = parseErrors ()

        member _.ValidationErrors() =
            let s, d = game.ValidationManager.Validate(false, resources.ValidatableEntities()) in s @ d

        member _.LocalisationErrors(force: bool, forceGlobal: bool) =
            getLocalisationErrors game globalLocalisation (force, forceGlobal)

        member _.Folders() = fileManager.AllFolders()
        member _.AllFiles() = resources.GetResources()

        member _.AllLoadedLocalisation() =
            game.LocalisationManager.LocalisationFileNames()

        member _.ScriptedTriggers() = lookup.triggers
        member _.ScriptedEffects() = lookup.effects
        member _.StaticModifiers() = [||] //lookup.staticModifiers
        member _.UpdateFile shallow file text = game.UpdateFile shallow file text
        member _.AllEntities() = resources.AllEntities()

        member _.References() =
            References<_>(resources, lookup, game.LocalisationManager.GetCleanLocalisationAPIs())

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
                (EU4 EU4Lang.English)
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
        member _.GetCodeEdits file text = None

        member _.GetEventGraphData: GraphDataRequest =
            (fun files gameType depth ->
                graphEventDataForFiles references game.ResourceManager lookup files gameType depth)

        member _.GetEmbeddedMetadata() =
            getEmbeddedMetadata lookup game.LocalisationManager game.ResourceManager
