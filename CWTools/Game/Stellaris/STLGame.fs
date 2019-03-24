namespace CWTools.Games.Stellaris

open CWTools.Parser
open System.IO
open FParsec
open CWTools.Process
open CWTools.Process.STLProcess
open CWTools.Validation.Stellaris.STLValidation
open CWTools.Validation.ValidationCore
open FSharp.Collections.ParallelSeq
open CWTools.Localisation
open CWTools.Localisation.STLLocalisation
open CWTools.Common
open CWTools.Common.STLConstants
open CWTools.Process.STLScopes
open DotNet.Globbing
open System.Collections.Specialized
open CWTools.Validation.Stellaris.STLLocalisationValidation
open CWTools.Validation.Stellaris.STLEventValidation
open CWTools.Process.ProcessCore
open CWTools.Parser.Types
open CWTools.Validation.Stellaris.STLLocalisationString
open CWTools.Utilities.Utils
open CWTools.Utilities
open CWTools.Validation.Stellaris.Graphics
open CWTools.Games
open CWTools.Games.Stellaris
open CWTools.Games.Stellaris.STLLookup
open CWTools.Utilities.Position
open CWTools.Validation.Rules
open CWTools.Validation.Stellaris.STLRules
open CWTools.Parser
open CWTools.Parser.ConfigParser
open FSharp.Data.Runtime
open CWTools.Validation.Stellaris.ScopeValidation
open Files
open CWTools.Validation.Stellaris
open CWTools.Validation.Common.CommonValidation
open CWTools.Validation
open CWTools.Process.Scopes
open System.Text
open CWTools.Validation.Rules
open CWTools.Games.LanguageFeatures
open CWTools.Common
open CWTools.Validation.LocalisationString
open CWTools.Games.Helpers

module STLGameFunctions =
    type GameObject = GameObject<Scope, Modifier, STLComputedData>

    let processLocalisationFunction (localisationCommands : ((string * Scope list) list)) (lookup : Lookup<Scope, Modifier>) =
        let eventtargets = lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst
        let globaleventtargets = lookup.varDefInfo.TryFind "global_event_target" |> Option.defaultValue [] |> List.map fst
        let definedVariables = (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
        processLocalisation localisationCommands (eventtargets @ globaleventtargets) lookup.scriptedLoc definedVariables
    let validateLocalisationCommandFunction (localisationCommands : ((string * Scope list) list)) (lookup : Lookup<Scope, Modifier>) =
        let eventtargets = lookup.varDefInfo.TryFind "event_target" |> Option.defaultValue [] |> List.map fst
        let globaleventtargets = lookup.varDefInfo.TryFind "global_event_target" |> Option.defaultValue [] |> List.map fst
        let definedVariables = (lookup.varDefInfo.TryFind "variable" |> Option.defaultValue [] |> List.map fst)
        validateLocalisationCommand localisationCommands (eventtargets @ globaleventtargets) lookup.scriptedLoc definedVariables
    let updateScriptedTriggers (game : GameObject) =
        let vanillaTriggers =
            let se = scopedEffects |> List.map (fun e -> e :> Effect)
            let vt = game.Settings.embedded.triggers |> addInnerScope |> List.map (fun e -> e :> Effect)
            se @ vt
        let sts, ts = STLLookup.updateScriptedTriggers game.Resources vanillaTriggers
        // game.Lookup.triggers <- sts @ ts
        game.Lookup.onlyScriptedTriggers <- sts
        sts @ ts

    let updateScriptedEffects (game : GameObject) =
        let vanillaEffects =
            let se = scopedEffects |> List.map (fun e -> e :> Effect)
            let ve = game.Settings.embedded.effects |> addInnerScope |> List.map (fun e -> e :> Effect)
            se @ ve
        let ses, es = STLLookup.updateScriptedEffects game.Resources vanillaEffects (game.Lookup.triggers)
        // game.Lookup.effects <- ses @ es
        game.Lookup.onlyScriptedEffects <- ses
        ses @ es

    let updateStaticodifiers (game : GameObject) =
        let rawModifiers =
            game.Resources.AllEntities()
            |> List.choose (function |struct (f, _) when f.filepath.Contains("static_modifiers") -> Some (f.entity) |_ -> None)
            |> List.collect (fun n -> n.Children)
            //|> List.rev
        let newModifiers = rawModifiers |> List.map (fun e -> STLProcess.getStaticModifierCategory game.Settings.embedded.modifiers e)
        game.Lookup.staticModifiers <- newModifiers

    let updateScriptedLoc (game : GameObject) =
        let rawLocs =
            game.Resources.AllEntities()
            |> List.choose (function |struct (f, _) when f.filepath.Contains("scripted_loc") -> Some (f.entity) |_ -> None)
            |> List.collect (fun n -> n.Children)
            |> List.map (fun l -> l.TagText "name")
        game.Lookup.scriptedLoc <- rawLocs
    let updateDefinedVariables(game : GameObject) =
        game.Lookup.definedScriptVariables <- (game.Resources.AllEntities()) |> List.collect (fun struct (_, d) -> d.Force().Setvariables)

    let afterUpdateFile (game : GameObject<Scope, Modifier,STLComputedData>) (filepath : string) =
        match filepath with
        |x when x.Contains("scripted_triggers") -> updateScriptedTriggers(game) |> ignore
        |x when x.Contains("scripted_effects") -> updateScriptedEffects(game) |> ignore
        |x when x.Contains("scripted_loc") -> updateScriptedLoc(game)
        |x when x.Contains("static_modifiers") -> updateStaticodifiers(game)
        |_ -> ()
        updateDefinedVariables(game)

    let globalLocalisation (game : GameObject) =
        let locfiles =  game.Resources.GetResources()
                        |> List.choose (function |FileWithContentResource (_, e) -> Some e |_ -> None)
                        |> List.filter (fun f -> f.overwrite <> Overwritten && f.extension = ".yml" && f.validate)
                        |> List.map (fun f -> f.filepath)
        let locFileValidation = validateLocalisationFiles locfiles
        let locParseErrors = game.LocalisationManager.LocalisationAPIs() <&!&> (fun (b, api) -> if b then validateLocalisationSyntax api.Results else OK)
        let globalTypeLoc = game.ValidationManager.ValidateGlobalLocalisation()
        game.Lookup.proccessedLoc |> validateProcessedLocalisation game.LocalisationManager.taggedLocalisationKeys <&&> locFileValidation <&&> globalTypeLoc <&&> locParseErrors |> (function |Invalid es -> es |_ -> [])

    let updateModifiers(game : GameObject) =
        game.Lookup.coreModifiers <- addGeneratedModifiers game.Settings.embedded.modifiers (EntitySet (game.Resources.AllEntities()))

    let updateTechnologies(game : GameObject) =
        game.Lookup.technologies <- getTechnologies (EntitySet (game.Resources.AllEntities()))

    let addModifiersWithScopes (lookup : Lookup<_,_>) =
        let modifierOptions (modifier : Modifier) =
            let requiredScopes =
                modifier.categories |> List.choose (fun c -> modifierCategoryToScopesMap.TryFind c)
                                    |> List.map Set.ofList
                                    |> (fun l -> if List.isEmpty l then [] else l |> List.reduce (Set.intersect) |> Set.toList)
            {min = 0; max = 100; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = requiredScopes}
        lookup.coreModifiers
            |> List.map (fun c -> AliasRule ("modifier", NewRule(LeafRule(specificField c.tag, ValueField (ValueType.Float (-1E+12, 1E+12))), modifierOptions c)))

    let addTriggerDocsScopes (lookup : Lookup<_,_>) (rules : RootRule<_> list) =
            let scriptedOptions (effect : ScriptedEffect) =
                {min = 0; max = 100; leafvalue = false; description = Some effect.Comments; pushScope = None; replaceScopes = None; severity = None; requiredScopes = effect.Scopes}
            let getAllScriptedEffects =
                lookup.onlyScriptedEffects |> List.choose (function | :? ScriptedEffect as se -> Some se |_ -> None)
                                                |> List.map (fun se -> AliasRule("effect", NewRule(LeafRule(specificField se.Name, ValueField(ValueType.Bool)), scriptedOptions se)))
            let getAllScriptedTriggers =
                lookup.onlyScriptedTriggers |> List.choose (function | :? ScriptedEffect as se -> Some se |_ -> None)
                                                |> List.map (fun se -> AliasRule("trigger", NewRule(LeafRule(specificField se.Name, ValueField(ValueType.Bool)), scriptedOptions se)))
            let addRequiredScopesE (s : StringTokens) (o : ConfigParser.Options<_>) =
                let newScopes =
                    match o.requiredScopes with
                    |[] ->
                        lookup.effectsMap.TryFind((StringResource.stringManager.GetStringForID s.normal))
                            |> Option.map(fun se -> se.Scopes)
                            |> Option.defaultValue []
                    |x -> x
                { o with requiredScopes = newScopes}
            let addRequiredScopesT (s : StringTokens) (o : ConfigParser.Options<_>) =
                let newScopes =
                    match o.requiredScopes with
                    |[] ->
                        lookup.triggersMap.TryFind((StringResource.stringManager.GetStringForID s.normal))
                            |> Option.map(fun se -> se.Scopes)
                            |> Option.defaultValue []
                    |x -> x
                { o with requiredScopes = newScopes}
            rules |> List.collect (
                    function
                    |AliasRule ("effect", (LeafRule(ValueField(ValueType.Specific s),r), o)) ->
                        [AliasRule ("effect", (LeafRule(ValueField(ValueType.Specific s),r), addRequiredScopesE s o))]
                    |AliasRule ("trigger", (LeafRule(ValueField(ValueType.Specific s),r), o)) ->
                        [AliasRule ("trigger", (LeafRule(ValueField(ValueType.Specific s),r), addRequiredScopesT s o))]
                    |AliasRule ("effect", (NodeRule(ValueField(ValueType.Specific s),r), o)) ->
                        [AliasRule ("effect", (NodeRule(ValueField(ValueType.Specific s),r), addRequiredScopesE s o))]
                    |AliasRule ("trigger", (NodeRule(ValueField(ValueType.Specific s),r), o)) ->
                        [AliasRule ("trigger", (NodeRule(ValueField(ValueType.Specific s),r), addRequiredScopesT s o))]
                    |AliasRule ("effect", (LeafValueRule(ValueField(ValueType.Specific s)), o)) ->
                        [AliasRule ("effect", (LeafValueRule(ValueField(ValueType.Specific s)), addRequiredScopesE s o))]
                    |AliasRule ("trigger", (LeafValueRule(ValueField(ValueType.Specific s)), o)) ->
                        [AliasRule ("trigger", (LeafValueRule(ValueField(ValueType.Specific s)), addRequiredScopesT s o))]
                    |AliasRule ("effect", (LeafRule(TypeField(TypeType.Simple "scripted_effect"), o), _)) ->
                        getAllScriptedEffects
                    |AliasRule ("trigger", (LeafRule(TypeField(TypeType.Simple "scripted_trigger"), o), _)) ->
                        getAllScriptedTriggers
                    |x -> [x])


    let loadConfigRulesHook rules (lookup : Lookup<_,_>) embedded =
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ updateEventTargetLinks embedded //@ addDataEventTargetLinks lookup embedded
        let rulesWithMod = rules @ addModifiersWithScopes(lookup)
        let rulesWithEmbeddedScopes = addTriggerDocsScopes lookup rulesWithMod
        rulesWithEmbeddedScopes

    let refreshConfigBeforeFirstTypesHook (lookup : Lookup<_,_>) _ (embeddedSettings : EmbeddedSettings<_,_>)  =
        ()

    let refreshConfigAfterFirstTypesHook (lookup : Lookup<_,_>) (resources : IResourceAPI<_>) (embeddedSettings : EmbeddedSettings<_,_>) =
        lookup.globalScriptedVariables <- (EntitySet (resources.AllEntities())).GlobMatch "**/common/scripted_variables/*.txt" |> List.collect STLValidation.getDefinedVariables
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ (updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings)

    let refreshConfigAfterVarDefHook (lookup : Lookup<_,_>) (resources : IResourceAPI<_>) (embeddedSettings : EmbeddedSettings<_,_>) =
        lookup.allCoreLinks <- lookup.triggers @ lookup.effects @ (updateEventTargetLinks embeddedSettings @ addDataEventTargetLinks lookup embeddedSettings)
    let afterInit (game : GameObject) =
            let ts = updateScriptedTriggers(game)
            game.Lookup.allCoreLinks <- ts @ game.Lookup.effects @ game.Lookup.eventTargetLinks
            let es = updateScriptedEffects(game)
            game.Lookup.allCoreLinks <- game.Lookup.triggers @ es @ game.Lookup.eventTargetLinks
            updateStaticodifiers(game)
            updateScriptedLoc(game)
            updateDefinedVariables(game)
            updateModifiers(game)
            updateTechnologies(game)



type StellarisSettings = GameSettings<Modifier, Scope>

open STLGameFunctions
type STLGame (settings : StellarisSettings) =
    let validationSettings = {
        validators = [validateVariables, "var"; valTechnology, "tech"; validateTechnologies, "tech2"; valButtonEffects, "but"; valSprites, "sprite"; valVariables, "var2"; valEventCalls, "event";
                            validateAmbientGraphics, "ambient"; validateShipDesigns, "designs"; validateMixedBlocks, "mixed"; validateSolarSystemInitializers, "solar"; validateAnomaly210, "anom";
                            validateIfElse210, "ifelse"; validateIfElse, "ifelse2"; validatePlanetKillers, "pk"; validateRedundantAND, "AND"; valMegastructureGraphics, "megastructure";
                            valPlanetClassGraphics, "pcg"; validateDeprecatedSetName, "setname"; validateShips, "ships"; validateEvents, "eventsSimple"; validateNOTMultiple, "not"]
        experimentalValidators = [valSectionGraphics, "sections"; valComponentGraphics, "component"]
        heavyExperimentalValidators = [getEventChains, "event chains"]
        experimental = settings.validation.experimental
        fileValidators = [valSpriteFiles, "sprites"; valMeshFiles, "mesh"; valAssetFiles, "asset"; valComponentIcons, "compicon"]
        lookupValidators = [validateModifierBlocks, "mod blocks"; valAllModifiers, "mods"]
        useRules = settings.rules |> Option.map (fun o -> o.validateRules) |> Option.defaultValue false
        debugRulesOnly = settings.rules |> Option.map (fun o -> o.debugRulesOnly) |> Option.defaultValue false
        localisationValidators = [valEventLocs; valTechLocs; valCompSetLocs; valCompTempLocs; valTraditionLocCats

                                ; valPolicies; valEffectLocs; valTriggerLocs;]

    }
        let settings = { settings with validation = { settings.validation with langs = STL STLLang.Default::settings.validation.langs }
                                       embedded = { settings.embedded with localisationCommands = settings.embedded.localisationCommands |> (fun l -> if l.Length = 0 then locCommands else l )}}


        let rulesManagerSettings = {
            rulesSettings = settings.rules
            parseScope = parseScope
            allScopes = allScopes
            anyScope = Scope.Any
            changeScope = changeScope
            defaultContext = defaultContext
            defaultLang = STL STLLang.Default
            oneToOneScopesNames = oneToOneScopesNames
            loadConfigRulesHook = loadConfigRulesHook
            refreshConfigBeforeFirstTypesHook = refreshConfigBeforeFirstTypesHook
            refreshConfigAfterFirstTypesHook = refreshConfigAfterFirstTypesHook
            refreshConfigAfterVarDefHook = refreshConfigAfterVarDefHook
        }

        let game = GameObject<Scope, Modifier, STLComputedData>.CreateGame
                    (settings, "stellaris", scriptFolders, STLCompute.computeSTLData,
                    STLCompute.computeSTLDataUpdate,
                     (STLLocalisationService >> (fun f -> f :> ILocalisationAPICreator)),
                     STLGameFunctions.processLocalisationFunction (settings.embedded.localisationCommands),
                     STLGameFunctions.validateLocalisationCommandFunction (settings.embedded.localisationCommands),
                     defaultContext,
                     defaultContext,
                     Encoding.UTF8,
                     Encoding.GetEncoding(1252),
                     validationSettings,
                     STLGameFunctions.globalLocalisation,
                     STLGameFunctions.afterUpdateFile,
                     ".yml",
                     rulesManagerSettings)
                     afterInit

        let lookup = game.Lookup
        let resources = game.Resources
        let fileManager = game.FileManager

        let useRules = settings.rules.IsSome
        let parseErrors() =
            resources.GetResources()
                |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None)
                |> List.choose (fun r -> r.result |> function |(Fail (result)) when r.validate -> Some (r.filepath, result.error, result.position)  |_ -> None)


        let validateTechnology (entities : (string * Node) list) =
            let tech = entities |> List.filter (fun (f, _) -> f.Contains("common/technology/"))
            tech

        let scopesAtPosSTL pos file text =
            let resource = makeEntityResourceInput fileManager file text
            match game.ResourceManager.ManualProcessResource resource, game.InfoService with
            |Some e, Some info ->
                // match info.GetInfo(pos, e) with
                match (info.GetInfo)(pos, e) with
                |Some (ctx, _) when ctx <> { Root = Scope.Any; From = []; Scopes = [] } ->
                    Some (ctx)
                |_ ->
                    getScopeContextAtPos pos lookup.triggers lookup.effects e.entity |> Option.map (fun s -> {From = s.From; Root = s.Root; Scopes = s.Scopes})
            |Some e, _ -> getScopeContextAtPos pos lookup.triggers lookup.effects e.entity |> Option.map (fun s -> {From = s.From; Root = s.Root; Scopes = s.Scopes})
            |_ -> None


        interface IGame<STLComputedData, Scope, Modifier> with
        //member __.Results = parseResults
            member __.ParserErrors() = parseErrors()
            member __.ValidationErrors() = let (s, d) = (game.ValidationManager.Validate(false, (resources.ValidatableEntities()))) in s @ d
            member __.LocalisationErrors(force : bool, forceGlobal : bool) =
                let rulesLocErrors = game.ValidationManager.CachedRuleErrors(resources.ValidatableEntities() |> List.map (fun struct (e, _) -> e)) |> List.filter (fun (id, _, _, _, _, _) -> id = "CW100")
                let genGlobal() =
                    let ges = (globalLocalisation(game))
                    game.LocalisationManager.globalLocalisationErrors <- Some ges
                    ges
                let genAll() =
                    let les = (game.ValidationManager.ValidateLocalisation (resources.ValidatableEntities()))
                    game.LocalisationManager.localisationErrors <- Some les
                    les
                rulesLocErrors @
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
            member __.StaticModifiers() = lookup.staticModifiers
            member __.UpdateFile shallow file text = game.UpdateFile shallow file text
            member __.AllEntities() = resources.AllEntities()
            member __.References() = References<_, _, Modifier>(resources, lookup, (game.LocalisationManager.LocalisationAPIs() |> List.map snd))
            member __.Complete pos file text = completion fileManager game.completionService game.InfoService game.ResourceManager pos file text
            member __.ScopesAtPos pos file text =
                scopesAtPosSTL pos file text
                |> Option.map (fun sc -> { OutputScopeContext.From = sc.From; Scopes = sc.Scopes; Root = sc.Root})
            member __.GoToType pos file text = getInfoAtPos fileManager game.ResourceManager game.InfoService lookup pos file text
            member __.FindAllRefs pos file text = findAllRefsFromPos fileManager game.ResourceManager game.InfoService pos file text
            member __.InfoAtPos pos file text = game.InfoAtPos pos file text
            member __.ReplaceConfigRules rules = game.ReplaceConfigRules(({ ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})) //refreshRuleCaches game (Some { ruleFiles = rules; validateRules = true; debugRulesOnly = false; debugMode = false})
            member __.RefreshCaches() = updateModifiers(game); game.RefreshCaches()
            member __.RefreshLocalisationCaches() = game.LocalisationManager.UpdateProcessedLocalisation()
            member __.ForceRecompute() = resources.ForceRecompute()
            member __.Types() = game.Lookup.typeDefInfo
