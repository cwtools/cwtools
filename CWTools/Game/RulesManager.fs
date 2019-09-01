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

type LocalisationEmbeddedSettings<'S when 'S : comparison> =
| Legacy of (string * ('S list)) list
| Jomini of CWTools.Parser.DataTypeParser.JominiLocDataTypes

type EmbeddedSettings<'S,'M when 'S : comparison> = {
    triggers : DocEffect<'S> list
    effects : DocEffect<'S> list
    embeddedFiles : (string * string) list
    modifiers : 'M list
    cachedResourceData : (Resource * Entity) list
    localisationCommands : LocalisationEmbeddedSettings<'S>
    eventTargetLinks : EventTargetLink<'S> list
    scopeDefinitions : ScopeInput list
}

type RuleManagerSettings<'S, 'M, 'T, 'L when 'S :> IScope<'S> and 'S : comparison and 'M :> IModifier and 'T :> ComputedData and 'L :> Lookup<'M>> = {
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

type RulesManager<'T, 'M, 'L when 'T :> ComputedData and 'M :> IModifier and 'L :> Lookup<'M>>
    (resources : IResourceAPI<'T>, lookup : 'L,
     settings : RuleManagerSettings<Scope, 'M, 'T, 'L>,
     localisation : LocalisationManager<'T, 'M>,
     embeddedSettings : EmbeddedSettings<Scope, 'M>,
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
        let tempRuleValidationService = RuleValidationService<Scope>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, Collections.Map.empty, loc, files, lookup.eventTargetLinksMap, lookup.valueTriggerMap , settings.anyScope, settings.changeScope, settings.defaultContext, settings.defaultLang)
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let allentities = resources.AllEntities() |> List.map (fun struct(e,_) -> e)
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let typeDefInfo = getTypesFromDefinitions tempRuleValidationService tempTypes allentities


        // let typeDefInfo = createLandedTitleTypes game typeDefInfo
        lookup.typeDefInfo <- typeDefInfo// |> Map.map (fun _ v -> v |> List.map (fun (_, t, r) -> (t, r)))
        // lookup.typeDefInfo <- addModifiersAsTypes game lookup.typeDefInfo

        settings.refreshConfigAfterFirstTypesHook lookup resources embeddedSettings
        lookup.typeDefInfoForValidation <- typeDefInfo |> Map.map (fun _ v -> v |> List.choose (fun (tdi) -> if tdi.validate then Some (tdi.id, tdi.range) else None))

        // lookup.scriptedEffects <- tempEffects @ addDataEventTargetLinks game
        // lookup.scriptedTriggers <- tempTriggers @ addDataEventTargetLinks game
        tempTypeMap <- lookup.typeDefInfo |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (s |> List.map (fun tdi -> tdi.id)))) |> Map.ofSeq
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
        let completionService = (CompletionService(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.eventTargetLinksMap, lookup.valueTriggerMap, [], settings.changeScope, settings.defaultContext, settings.anyScope, settings.oneToOneScopesNames, settings.defaultLang))
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let ruleValidationService =  (RuleValidationService<Scope>(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.eventTargetLinksMap, lookup.valueTriggerMap, settings.anyScope, settings.changeScope, settings.defaultContext, settings.defaultLang))
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        let infoService = (InfoService(lookup.configRules, lookup.typeDefs, tempTypeMap, tempEnumMap, varMap, loc, files, lookup.eventTargetLinksMap, lookup.valueTriggerMap, ruleValidationService, settings.changeScope, settings.defaultContext, settings.anyScope, settings.defaultLang))
        // log "Refresh rule caches time: %i" timer.ElapsedMilliseconds; timer.Restart()
        // game.RefreshValidationManager()
        debugChecks()
        ruleValidationService, infoService, completionService

    member __.LoadBaseConfig(rulesSettings) = loadBaseConfig rulesSettings
    member __.RefreshConfig() = refreshConfig()