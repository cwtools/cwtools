namespace CWTools.Games
open CWTools.Rules
open CWTools.Common
open CWTools.Utilities.Position
open FSharp.Collections.ParallelSeq
open CWTools.Process.Scopes
open CWTools.Utilities.Utils
open CWTools.Rules.RulesHelpers
open System.IO
open CWTools.Common.NewScope
type RulesSettings = {
    ruleFiles : (string * string) list
    validateRules : bool
    debugRulesOnly : bool
    debugMode : bool
}

type LocalisationEmbeddedSettings =
| Legacy of (string * (Scope list)) list
| Jomini of CWTools.Parser.DataTypeParser.JominiLocDataTypes

type EmbeddedSettings = {
    triggers : DocEffect list
    effects : DocEffect list
    embeddedFiles : (string * string) list
    modifiers : ActualModifier list
    cachedResourceData : (Resource * Entity) list
    localisationCommands : LocalisationEmbeddedSettings
    eventTargetLinks : EventTargetLink list
}

type RuleManagerSettings<'T, 'L when 'T :> ComputedData and 'L :> Lookup> = {
    rulesSettings : RulesSettings option
    parseScope : string -> Scope
    allScopes : Scope list
    anyScope : Scope
    changeScope : ChangeScope
    defaultContext : ScopeContext
    defaultLang : Lang
    oneToOneScopesNames : string list
    loadConfigRulesHook : RootRule list -> 'L -> EmbeddedSettings -> RootRule list
    refreshConfigBeforeFirstTypesHook : 'L -> IResourceAPI<'T> -> EmbeddedSettings -> unit
    refreshConfigAfterFirstTypesHook : 'L -> IResourceAPI<'T> -> EmbeddedSettings -> unit
    refreshConfigAfterVarDefHook : 'L -> IResourceAPI<'T> -> EmbeddedSettings -> unit
}

type RulesManager<'T, 'L when 'T :> ComputedData and 'L :> Lookup>
    (resources : IResourceAPI<'T>, lookup : 'L,
     settings : RuleManagerSettings<'T, 'L>,
     localisation : LocalisationManager<'T>,
     embeddedSettings : EmbeddedSettings,
     debugMode : bool) =

    let mutable tempEffects = []
    let mutable tempTriggers = []
    let mutable simpleEnums = []
    let mutable complexEnums = []
    let mutable tempTypes = []
    let mutable tempValues = Map.empty
    let mutable tempTypeMap = [("", StringSet.Empty(InsensitiveStringComparer()))] |> Map.ofList
    let mutable tempEnumMap = [("", ("", StringSet.Empty(InsensitiveStringComparer())))] |> Map.ofList
    let mutable rulesDataGenerated = false

    let loadBaseConfig(rulesSettings : RulesSettings) =
        let rules, types, enums, complexenums, values =
            rulesSettings.ruleFiles
                |> List.filter (fun (fn, ft) -> Path.GetExtension fn == ".cwt" )
                |> CWTools.Rules.RulesParser.parseConfigs settings.parseScope settings.allScopes settings.anyScope
        // tempEffects <- updateScriptedEffects game rules
        // effects <- tempEffects
        // tempTriggers <- updateScriptedTriggers game rules
        // _triggers <- tempTriggers
        lookup.typeDefs <- types
        // let rulesWithMod = rules @ addModifiersWithScopes(game)
        let rulesPostHook = settings.loadConfigRulesHook rules lookup embeddedSettings
        // lookup.configRules <- rulesWithMod
        lookup.configRules <- rulesPostHook
        simpleEnums <- enums
        complexEnums <- complexenums
        tempTypes <- types
        tempValues <- values |> List.map (fun (s, sl) -> s, (sl |> List.map (fun s2 -> s2, range.Zero))) |> Map.ofList
        rulesDataGenerated <- false
        // log (sprintf "Update config rules def: %i" timer.ElapsedMilliseconds); timer.Restart()

    let debugChecks() =
        if debugMode
        then
            // let filesWithoutTypes = getEntitiesWithoutTypes lookup.typeDefs (resources.AllEntities() |> List.map (fun struct(e,_) -> e))
            // filesWithoutTypes |> List.iter (eprintfn "File without type %s")
            ()
        else ()
    let refreshConfig() =
        /// Enums
        let complexEnumDefs = getEnumsFromComplexEnums complexEnums (resources.AllEntities() |> List.map (fun struct(e,_) -> e))
        // let modifierEnums = { key = "modifiers"; values = lookup.coreModifiers |> List.map (fun m -> m.Tag); description = "Modifiers" }
        let allEnums = simpleEnums @ complexEnumDefs// @ [modifierEnums] @ [{ key = "provinces"; description = "provinces"; values = lookup.CK2provinces}]

        lookup.enumDefs <- allEnums |> List.map (fun e -> (e.key, (e.description, e.values))) |> Map.ofList

        settings.refreshConfigBeforeFirstTypesHook lookup resources embeddedSettings

        tempEnumMap <- lookup.enumDefs |> Map.toSeq |> PSeq.map (fun (k, (d, s)) -> k, (d, StringSet.Create(InsensitiveStringComparer(), (s)))) |> Map.ofSeq

        /// First pass type defs
        let loc = localisation.localisationKeys
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let files = resources.GetFileNames() |> Set.ofList
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let allentities = resources.AllEntities() |> List.map (fun struct(e,_) -> e)
        let refreshTypeInfo() =
            let tempRuleValidationService = RuleValidationService(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, Collections.Map.empty, loc, files, lookup.eventTargetLinksMap, lookup.valueTriggerMap , settings.anyScope, settings.changeScope, settings.defaultContext, settings.defaultLang)
            let typeDefInfo = getTypesFromDefinitions tempRuleValidationService tempTypes allentities
            lookup.typeDefInfo <- typeDefInfo// |> Map.map (fun _ v -> v |> List.map (fun (_, t, r) -> (t, r)))
            tempTypeMap <- lookup.typeDefInfo |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (s |> List.map (fun tdi -> tdi.id)))) |> Map.ofSeq
            tempTypeMap
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()
        let mutable i = 0
        let step() =
            //log "%A" current
            i <- i + 1
            let before = tempTypeMap
            tempTypeMap <- refreshTypeInfo()
            log (sprintf "Refresh types time: %i" timer.ElapsedMilliseconds); timer.Restart()
            tempTypeMap = before || i > 5
        while (not(step())) do ()

        let tempRuleValidationService = RuleValidationService(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, Collections.Map.empty, loc, files, lookup.eventTargetLinksMap, lookup.valueTriggerMap , settings.anyScope, settings.changeScope, settings.defaultContext, settings.defaultLang)

        lookup.typeDefInfoForValidation <- lookup.typeDefInfo |> Map.map (fun _ v -> v |> List.choose (fun (tdi) -> if tdi.validate then Some (tdi.id, tdi.range) else None))
        settings.refreshConfigAfterFirstTypesHook lookup resources embeddedSettings
        let tempInfoService = (InfoService(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, Collections.Map.empty, loc, files, lookup.eventTargetLinksMap, lookup.valueTriggerMap, tempRuleValidationService, settings.changeScope, settings.defaultContext, settings.anyScope, settings.defaultLang))


        //let infoService = tempInfoService
        // game.InfoService <- Some tempInfoService
        if not rulesDataGenerated then resources.ForceRulesDataGenerate(); rulesDataGenerated <- true else ()

        let results = resources.AllEntities() |> PSeq.map (fun struct(e, l) -> (l.Force().Definedvariables |> (Option.defaultWith (fun () -> tempInfoService.GetDefinedVariables e))))
                        |> Seq.fold (fun m map -> Map.toList map |>  List.fold (fun m2 (n,k) -> if Map.containsKey n m2 then Map.add n ((k |> List.ofSeq)@m2.[n]) m2 else Map.add n (k |> List.ofSeq) m2) m) tempValues

        lookup.varDefInfo <- results
        let results = resources.AllEntities() |> PSeq.map (fun struct(e, l) -> (l.Force().SavedEventTargets |> (Option.defaultWith (fun () -> tempInfoService.GetSavedEventTargets e))))
                        |> Seq.fold (fun (acc : ResizeArray<_>) e -> acc.AddRange((e)); acc ) (new ResizeArray<_>())
        lookup.savedEventTargets <- results
                        //|> Seq.fold (fun m map -> Map.toList map |>  List.fold (fun m2 (n,k) -> if Map.containsKey n m2 then Map.add n ((k |> List.ofSeq)@m2.[n]) m2 else Map.add n (k |> List.ofSeq) m2) m) tempValues
        settings.refreshConfigAfterVarDefHook lookup resources embeddedSettings

        let varMap = lookup.varDefInfo |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (List.map fst s))) |> Map.ofSeq
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let dataTypes = embeddedSettings.localisationCommands |> function | Jomini dts -> dts | _ -> { promotes = Map.empty; functions = Map.empty; dataTypes = Map.empty; dataTypeNames = Set.empty }

        let completionService = (CompletionService(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.eventTargetLinksMap, lookup.valueTriggerMap, [], settings.changeScope, settings.defaultContext, settings.anyScope, settings.oneToOneScopesNames, settings.defaultLang, dataTypes))
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let ruleValidationService =  (RuleValidationService(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.eventTargetLinksMap, lookup.valueTriggerMap, settings.anyScope, settings.changeScope, settings.defaultContext, settings.defaultLang))
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let infoService = (InfoService(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.eventTargetLinksMap, lookup.valueTriggerMap, ruleValidationService, settings.changeScope, settings.defaultContext, settings.anyScope, settings.defaultLang))
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        // game.RefreshValidationManager()
        debugChecks()
        ruleValidationService, infoService, completionService

    member __.LoadBaseConfig(rulesSettings) = loadBaseConfig rulesSettings
    member __.RefreshConfig() = refreshConfig()