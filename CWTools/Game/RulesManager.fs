namespace CWTools.Games

open System
open System.Collections.Generic
open System.Linq
open CWTools.Rules
open CWTools.Common
open CWTools.Utilities.Position
open FSharp.Collections.ParallelSeq
open CWTools.Process.Localisation
open CWTools.Process.Scopes
open CWTools.Utilities.Utils
open CWTools.Utilities.Utils2
open CWTools.Rules.RulesHelpers
open System.IO
open System.Collections.Frozen
open CWTools.Parser.UtilityParser
open CWTools.Rules.RulesWrapper

type RulesSettings =
    { ruleFiles: (string * string) list
      validateRules: bool
      debugRulesOnly: bool
      debugMode: bool }

type LocalisationEmbeddedSettings =
    | Legacy of (string * Scope list) list * string list * (string * Scope list * Scope) list
    | Jomini of CWTools.Parser.DataTypeParser.JominiLocDataTypes

type EmbeddedSettings =
    { triggers: DocEffect list
      effects: DocEffect list
      embeddedFiles: (string * string) list
      modifiers: ActualModifier array
      cachedResourceData: (Resource * Entity) list
      localisationCommands: LocalisationEmbeddedSettings
      eventTargetLinks: EventTargetLink list
      cachedRuleMetadata: CachedRuleMetadata option
      featureSettings: FeatureSettings }

type RuleManagerSettings<'T, 'L when 'T :> ComputedData and 'L :> Lookup> =
    { rulesSettings: RulesSettings option
      useFormulas: bool
      stellarisScopeTriggers: bool
      parseScope: string -> Scope
      allScopes: Scope list
      anyScope: Scope
      scopeGroups: Collections.Map<string, Scope list>
      changeScope: ChangeScope
      defaultContext: ScopeContext
      defaultLang: Lang
      oneToOneScopesNames: string list
      loadConfigRulesHook: RootRule array -> 'L -> EmbeddedSettings -> RootRule array
      refreshConfigBeforeFirstTypesHook: 'L -> IResourceAPI<'T> -> EmbeddedSettings -> unit
      refreshConfigAfterFirstTypesHook: 'L -> IResourceAPI<'T> -> EmbeddedSettings -> unit
      refreshConfigAfterVarDefHook: 'L -> IResourceAPI<'T> -> EmbeddedSettings -> unit
      locFunctions:
          'L
              -> ((Lang * Collections.Map<string, CWTools.Localisation.Entry>
                  -> Lang * Collections.Map<string, LocEntry>) *
              (LocEntry -> ScopeContext -> CWTools.Validation.ValidationResult)) }

type RulesManager<'T, 'L when 'T :> ComputedData and 'L :> Lookup>
    (
        resources: IResourceAPI<'T>,
        lookup: 'L,
        settings: RuleManagerSettings<'T, 'L>,
        localisation: LocalisationManager<'T>,
        embeddedSettings: EmbeddedSettings,
        languages: Lang array,
        debugMode: bool
    ) =

    let addEmbeddedTypeDefData =
        match embeddedSettings.cachedRuleMetadata with
        | None -> id
        | Some md ->
            fun (newMap: Map<string, array<TypeDefInfo>>) ->
                Map.fold
                    (fun s k v ->
                        match Map.tryFind k s with
                        | Some v' -> Map.add k (Array.append v v') s
                        | None -> Map.add k v s)
                    newMap
                    md.typeDefs

    let addEmbeddedEnumDefData =
        match embeddedSettings.cachedRuleMetadata with
        | None -> id
        | Some md ->
            fun (newMap: Map<string, string * (string * range option) array>) ->
                let mdAdjusted =
                    md.enumDefs
                    |> Map.map (fun _ (s, sl) -> s, (sl |> Array.map (fun x -> x, None)))

                let res =
                    Map.fold
                        (fun s k (d, v) ->
                            match Map.tryFind k s with
                            | Some(d', v') -> Map.add k (d, Array.append v v') s
                            | None -> Map.add k (d, v) s)
                        newMap
                        mdAdjusted

                res
    // res |> Map.map (fun _ (s, sl) -> s, (sl |> List.map (fun x -> x, None)))

    let addEmbeddedVarDefData =
        match embeddedSettings.cachedRuleMetadata with
        | None -> id
        | Some md ->
            fun (newMap: Map<string, array<string * range>>) ->
                Map.fold
                    (fun s k v ->
                        match Map.tryFind k s with
                        | Some v' -> Map.add k (Array.append v v') s
                        | None -> Map.add k v s)
                    newMap
                    md.varDefs

    let addEmbeddedLoc (langs: Lang array) : (Lang * Set<string>) array -> (Lang * Set<string>) array =
        match embeddedSettings.cachedRuleMetadata with
        | None -> id
        | Some md ->
            fun (newList: (Lang * Set<string>) array) ->
                let newMap = newList |> Map.ofArray
                let oldList = md.loc |> Array.filter (fun (l, _) -> Array.contains l langs)
                let embeddedMap = oldList |> Map.ofArray

                let res =
                    Map.fold
                        (fun s k v ->
                            match Map.tryFind k s with
                            | Some v' -> Map.add k (Set.union v v') s
                            | None -> Map.add k v s)
                        newMap
                        embeddedMap

                res |> Map.toArray

    let addEmbeddedFiles =
        match embeddedSettings.cachedRuleMetadata with
        | None -> id
        | Some md ->
            fun (newSet: HashSet<string>) ->
                newSet.UnionWith(md.files)
                newSet

    let mutable simpleEnums = []
    let mutable complexEnums = []
    let mutable tempTypes = []
    let mutable tempValues = Map.empty

    let mutable tempTypeMap = [ ("", PrefixOptimisedStringSet()) ] |> Map.ofList

    let mutable tempEnumMap: FrozenDictionary<string, string * PrefixOptimisedStringSet> =
        FrozenDictionary.Empty

    let mutable rulesDataGenerated = false


    let loadBaseConfig (rulesSettings: RulesSettings) =
        let rules, types, enums, complexenums, values =
            rulesSettings.ruleFiles
            |> List.filter (fun (fn, _) ->
                Path.GetExtension(fn.AsSpan()).Equals(".cwt", StringComparison.OrdinalIgnoreCase))
            |> RulesParser.parseConfigs
                settings.parseScope
                settings.allScopes
                settings.anyScope
                settings.scopeGroups
                settings.useFormulas
                settings.stellarisScopeTriggers
        // tempEffects <- updateScriptedEffects game rules
        // effects <- tempEffects
        // tempTriggers <- updateScriptedTriggers game rules
        // _triggers <- tempTriggers
        lookup.typeDefs <- types
        // let rulesWithMod = rules @ addModifiersWithScopes(game)
        let rulesPostHook = settings.loadConfigRulesHook rules lookup embeddedSettings

        if rulesSettings.debugMode then
            RulesConsistencyValidation.checkForUndefinedTypes rulesPostHook lookup.typeDefs
        // lookup.configRules <- rulesWithMod
        lookup.configRules <- rulesPostHook
        simpleEnums <- enums
        complexEnums <- complexenums
        tempTypes <- types
        tempValues <- values |> Map.ofList //|> List.map (fun (s, sl) -> s, (sl |> List.map (fun s2 -> s2, range.Zero))) |> Map.ofList
        rulesDataGenerated <- false
    // log (sprintf "Update config rules def: %i" timer.ElapsedMilliseconds); timer.Restart()

    let refreshConfig () =
        let timer = System.Diagnostics.Stopwatch()
        let endToEndTimer = System.Diagnostics.Stopwatch()
        timer.Start()
        endToEndTimer.Start()
        let rulesWrapper = RulesWrapper(lookup.configRules)

        /// Enums
        let complexEnumDefs =
            getEnumsFromComplexEnums complexEnums (resources.AllEntities() |> Seq.map structFst)

        let allEnums = simpleEnums @ complexEnumDefs

        let newEnumDefs =
            allEnums
            |> Seq.map (fun e -> (e.key, (e.description, e.valuesWithRange)))
            |> Map.ofSeq

        lookup.enumDefs <- addEmbeddedEnumDefData newEnumDefs

        settings.refreshConfigBeforeFirstTypesHook lookup resources embeddedSettings

        tempEnumMap <-
            (lookup.enumDefs
             |> Map.toSeq
             |> PSeq.map (fun (k, (d, s)) -> KeyValuePair(k, (d, s |> Array.map fst |> createStringSet))))
                .ToFrozenDictionary()

        /// First pass type defs
        let loc = addEmbeddedLoc languages localisation.localisationKeys
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let files = addEmbeddedFiles(resources.GetFileNames().ToHashSet()).ToFrozenSet()
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()

        let refreshTypeInfo () =
            let processLoc, validateLoc = settings.locFunctions lookup

            let tempRuleValidationService =
                RuleValidationService(
                    rulesWrapper,
                    lookup.typeDefs,
                    tempTypeMap.ToFrozenDictionary(),
                    tempEnumMap,
                    FrozenDictionary.Empty,
                    loc,
                    files,
                    lookup.eventTargetLinksMap,
                    lookup.valueTriggerMap,
                    settings.anyScope,
                    settings.changeScope,
                    settings.defaultContext,
                    settings.defaultLang,
                    processLoc,
                    validateLoc
                )

            let allEntities = resources.AllEntities() |> Seq.map structFst
            let typeDefInfo =
                getTypesFromDefinitions (Some tempRuleValidationService) tempTypes allEntities

            lookup.typeDefInfo <- addEmbeddedTypeDefData typeDefInfo // |> Map.map (fun _ v -> v |> List.map (fun (_, t, r) -> (t, r)))

            let newTypeMap =
                lookup.typeDefInfo
                |> Map.toSeq
                |> PSeq.map (fun (k, s) -> k, s |> Seq.map _.id |> createStringSet)
                |> Map.ofSeq

            newTypeMap

        logDiag $"Pre-refresh types time: %0.3f{float timer.ElapsedMilliseconds / 1000.0}"
        timer.Restart()
        let mutable i = 0
        let mutable beforeCount = tempTypeMap.Values |> Seq.sumBy _.Count

        let step () =
            //log "%A" current
            i <- i + 1
            //TODO: Only refresh the types which have subtypes that depend on other types
            tempTypeMap <- refreshTypeInfo ()
            logDiag $"Refresh types time: %0.3f{float timer.ElapsedMilliseconds / 1000.0}"
            timer.Restart()
            let afterCount = tempTypeMap.Values |> Seq.sumBy _.Count
            let complete = beforeCount = afterCount || i > 5
            beforeCount <- afterCount
            complete

        // TODO check this actually stops early
        while not (step ()) do
            ()

        let processLoc, validateLoc = settings.locFunctions lookup

        let tempRuleValidationService =
            RuleValidationService(
                rulesWrapper,
                lookup.typeDefs,
                tempTypeMap.ToFrozenDictionary(),
                tempEnumMap,
                FrozenDictionary.Empty,
                loc,
                files,
                lookup.eventTargetLinksMap,
                lookup.valueTriggerMap,
                settings.anyScope,
                settings.changeScope,
                settings.defaultContext,
                settings.defaultLang,
                processLoc,
                validateLoc
            )

        lookup.typeDefInfoForValidation <-
            lookup.typeDefInfo
            |> Map.map (fun _ v ->
                v
                |> Array.choose (fun tdi ->
                    if tdi.validate then
                        Some(struct (tdi.id, tdi.range))
                    else
                        None))

        settings.refreshConfigAfterFirstTypesHook lookup resources embeddedSettings

        tempTypeMap <-
            lookup.typeDefInfo
            |> Map.toSeq
            |> PSeq.map (fun (k, s) -> k, s |> Seq.map _.id |> createStringSet)
            |> Map.ofSeq

        let processLoc, validateLoc = settings.locFunctions lookup

        let tempInfoService =
            InfoService(
                rulesWrapper,
                lookup.typeDefs,
                tempTypeMap.ToFrozenDictionary(),
                tempEnumMap,
                FrozenDictionary.Empty,
                loc,
                files,
                lookup.eventTargetLinksMap,
                lookup.valueTriggerMap,
                tempRuleValidationService,
                settings.changeScope,
                settings.defaultContext,
                settings.anyScope,
                settings.defaultLang,
                processLoc,
                validateLoc
            )


        //let infoService = tempInfoService
        // game.InfoService <- Some tempInfoService
        if not rulesDataGenerated then
            resources.ForceRulesDataGenerate()
            rulesDataGenerated <- true
        else
            ()

        let predefValues =
            tempValues
            |> Map.map (fun k vs -> (expandPredefinedValues tempTypeMap lookup.enumDefs vs))
            |> Map.toList
            |> List.map (fun (s, sl) -> s, (sl |> Seq.map (fun s2 -> s2, range.Zero) |> Array.ofSeq))
            |> Map.ofList

        let results =
            resources.AllEntities()
            |> PSeq.map (fun struct (e, l) ->
                (l.Force().Definedvariables
                 |> (Option.defaultWith (fun () -> tempInfoService.GetDefinedVariables e))))
            |> Seq.fold
                (fun m map ->
                    Map.toList map
                    |> List.fold
                        (fun m2 (n, k) ->
                            if Map.containsKey n m2 then
                                Map.add n (Array.append (k.ToArray()) m2[n]) m2
                            else
                                Map.add n (k.ToArray()) m2)
                        m)
                predefValues

        lookup.varDefInfo <- addEmbeddedVarDefData results
        // eprintfn "vdi %A" results
        let results =
            resources.AllEntities()
            |> PSeq.map (fun struct (e, l) ->
                (l.Force().SavedEventTargets
                 |> (Option.defaultWith (fun () -> tempInfoService.GetSavedEventTargets e))))
            |> Seq.fold
                (fun (acc: ResizeArray<_>) e ->
                    acc.AddRange e
                    acc)
                (new ResizeArray<_>())

        lookup.savedEventTargets <- results
        //|> Seq.fold (fun m map -> Map.toList map |>  List.fold (fun m2 (n,k) -> if Map.containsKey n m2 then Map.add n ((k |> List.ofSeq)@m2.[n]) m2 else Map.add n (k |> List.ofSeq) m2) m) tempValues
        settings.refreshConfigAfterVarDefHook lookup resources embeddedSettings

        let varMap: FrozenDictionary<string, PrefixOptimisedStringSet> =
            (lookup.varDefInfo
             |> Map.toSeq
             |> PSeq.map (fun (k, s) -> KeyValuePair(k, s |> Seq.map fst |> createStringSet)))
                .ToFrozenDictionary()

        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let dataTypes =
            embeddedSettings.localisationCommands
            |> function
                | Jomini dts -> dts
                | _ ->
                    { promotes = Map.empty
                      confidentFunctions = Map.empty
                      functions = Map.empty
                      dataTypes = Map.empty
                      dataTypeNames = Set.empty }

        let processLoc, validateLoc = settings.locFunctions lookup

        let completionService =
            CompletionService(
                rulesWrapper,
                lookup.typeDefs,
                tempTypeMap.ToFrozenDictionary(),
                tempEnumMap,
                varMap,
                loc,
                files,
                lookup.eventTargetLinksMap,
                lookup.valueTriggerMap,
                [],
                settings.changeScope,
                settings.defaultContext,
                settings.anyScope,
                settings.oneToOneScopesNames,
                settings.defaultLang,
                dataTypes,
                processLoc,
                validateLoc
            )
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let ruleValidationService =
            RuleValidationService(
                rulesWrapper,
                lookup.typeDefs,
                tempTypeMap.ToFrozenDictionary(),
                tempEnumMap,
                varMap,
                loc,
                files,
                lookup.eventTargetLinksMap,
                lookup.valueTriggerMap,
                settings.anyScope,
                settings.changeScope,
                settings.defaultContext,
                settings.defaultLang,
                processLoc,
                validateLoc
            )
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let infoService =
            InfoService(
                rulesWrapper,
                lookup.typeDefs,
                tempTypeMap.ToFrozenDictionary(),
                tempEnumMap,
                varMap,
                loc,
                files,
                lookup.eventTargetLinksMap,
                lookup.valueTriggerMap,
                ruleValidationService,
                settings.changeScope,
                settings.defaultContext,
                settings.anyScope,
                settings.defaultLang,
                processLoc,
                validateLoc
            )
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        // game.RefreshValidationManager()
        logInfo $"Refresh all lookups: %0.3f{float endToEndTimer.ElapsedMilliseconds / 1000.0}s"
        ruleValidationService, infoService, completionService

    member _.LoadBaseConfig(rulesSettings) = loadBaseConfig rulesSettings
    member _.RefreshConfig() = refreshConfig ()
