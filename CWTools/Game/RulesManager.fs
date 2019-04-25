namespace CWTools.Games
open CWTools.Rules
open CWTools.Common
open CWTools.Utilities.Position
open FSharp.Collections.ParallelSeq
open CWTools.Process.Scopes
open CWTools.Utilities.Utils
open CWTools.Rules.RulesHelpers
type RulesSettings = {
    ruleFiles : (string * string) list
    validateRules : bool
    debugRulesOnly : bool
    debugMode : bool
}


type EmbeddedSettings<'S,'M when 'S : comparison> = {
    triggers : DocEffect<'S> list
    effects : DocEffect<'S> list
    embeddedFiles : (string * string) list
    modifiers : 'M list
    cachedResourceData : (Resource * Entity) list
    localisationCommands : (string * ('S list)) list
    eventTargetLinks : EventTargetLink<'S> list
}

type RuleManagerSettings<'S, 'M, 'T, 'L when 'S :> IScope<'S> and 'S : comparison and 'M :> IModifier and 'T :> ComputedData and 'L :> Lookup<'S, 'M>> = {
    rulesSettings : RulesSettings option
    parseScope : string -> 'S
    allScopes : 'S list
    anyScope : 'S
    changeScope : ChangeScope<'S>
    defaultContext : ScopeContext<'S>
    defaultLang : Lang
    oneToOneScopesNames : string list
    loadConfigRulesHook : RootRule<'S> list -> 'L -> EmbeddedSettings<'S, 'M> -> RootRule<'S> list
    refreshConfigBeforeFirstTypesHook : 'L -> IResourceAPI<'T> -> EmbeddedSettings<'S, 'M> -> unit
    refreshConfigAfterFirstTypesHook : 'L -> IResourceAPI<'T> -> EmbeddedSettings<'S, 'M> -> unit
    refreshConfigAfterVarDefHook : 'L -> IResourceAPI<'T> -> EmbeddedSettings<'S, 'M> -> unit
}

type RulesManager<'T, 'S, 'M, 'L when 'T :> ComputedData and 'S :> IScope<'S> and 'S : comparison and 'M :> IModifier and 'L :> Lookup<'S,'M>>
    (resources : IResourceAPI<'T>, lookup : 'L,
     settings : RuleManagerSettings<'S, 'M, 'T, 'L>,
     localisation : LocalisationManager<'S, 'T, 'M>,
     embeddedSettings : EmbeddedSettings<'S, 'M>) =

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
        let rules, types, enums, complexenums, values = rulesSettings.ruleFiles |> CWTools.Rules.RulesParser.parseConfigs settings.parseScope settings.allScopes settings.anyScope
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
        let tempRuleValidationService = RuleValidationService<'S>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, Collections.Map.empty, loc, files, lookup.eventTargetLinksMap, lookup.valueTriggerMap , settings.anyScope, settings.changeScope, settings.defaultContext, settings.defaultLang)
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let allentities = resources.AllEntities() |> List.map (fun struct(e,_) -> e)
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let typeDefInfo = getTypesFromDefinitions tempRuleValidationService tempTypes allentities


        // let typeDefInfo = createLandedTitleTypes game typeDefInfo
        lookup.typeDefInfoRaw <- typeDefInfo// |> Map.map (fun _ v -> v |> List.map (fun (_, t, r) -> (t, r)))
        // lookup.typeDefInfo <- addModifiersAsTypes game lookup.typeDefInfo

        settings.refreshConfigAfterFirstTypesHook lookup resources embeddedSettings
        lookup.typeDefInfoForValidation <- typeDefInfo |> Map.map (fun _ v -> v |> List.choose (fun (v, t, r) -> if v then Some (t, r) else None))

        // lookup.scriptedEffects <- tempEffects @ addDataEventTargetLinks game
        // lookup.scriptedTriggers <- tempTriggers @ addDataEventTargetLinks game
        tempTypeMap <- lookup.typeDefInfo |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))) |> Map.ofSeq
        let tempInfoService = (InfoService<'S>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, Collections.Map.empty, loc, files, lookup.eventTargetLinksMap, lookup.valueTriggerMap, tempRuleValidationService, settings.changeScope, settings.defaultContext, settings.anyScope, settings.defaultLang))

        let infoService = tempInfoService
        // game.InfoService <- Some tempInfoService
        if not rulesDataGenerated then resources.ForceRulesDataGenerate(); rulesDataGenerated <- true else ()

        let results = resources.AllEntities() |> PSeq.map (fun struct(e, l) -> (l.Force().Definedvariables |> (Option.defaultWith (fun () -> tempInfoService.GetDefinedVariables e))))
                        |> Seq.fold (fun m map -> Map.toList map |>  List.fold (fun m2 (n,k) -> if Map.containsKey n m2 then Map.add n (k@m2.[n]) m2 else Map.add n k m2) m) tempValues

        lookup.varDefInfo <- results
        settings.refreshConfigAfterVarDefHook lookup resources embeddedSettings

        let varMap = lookup.varDefInfo |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (List.map fst s))) |> Map.ofSeq
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let completionService = (CompletionService(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.eventTargetLinksMap, lookup.valueTriggerMap, [], settings.changeScope, settings.defaultContext, settings.anyScope, settings.oneToOneScopesNames, settings.defaultLang))
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let ruleValidationService =  (RuleValidationService<'S>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.eventTargetLinksMap, lookup.valueTriggerMap, settings.anyScope, settings.changeScope, settings.defaultContext, settings.defaultLang))
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let infoService = (InfoService<'S>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.eventTargetLinksMap, lookup.valueTriggerMap, ruleValidationService, settings.changeScope, settings.defaultContext, settings.anyScope, settings.defaultLang))
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        // game.RefreshValidationManager()
        ruleValidationService, infoService, completionService

    member __.LoadBaseConfig(rulesSettings) = loadBaseConfig rulesSettings
    member __.RefreshConfig() = refreshConfig()