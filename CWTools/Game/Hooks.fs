module CWTools.Game.Hooks

open CWTools.Common
open CWTools.Games
open CWTools.Games.Helpers
open CWTools.Rules
open CWTools.Utilities
open CWTools.Utilities.Position
open FSharp.Collections.ParallelSeq
open CWTools.Validation.LocalisationString
open CWTools.Validation
open CWTools.Validation.ValidationCore

let globalLocalisation (game: GameObject<_, _>) =
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

let private updateScriptedTriggers (lookup: Lookup) (rules: RootRule list) (embeddedSettings: EmbeddedSettings) =
    let vanillaTriggers =
        let se = [] |> List.map (fun e -> e :> Effect)
        let vt = embeddedSettings.triggers |> List.map (fun e -> e :> Effect)
        se @ vt

    let vanillaTriggerNames = vanillaTriggers |> List.map _.Name |> Set.ofList

    let effects =
        rules
        |> List.choose (function
            | AliasRule("trigger", r) -> Some r
            | _ -> None)

    let inline ruleToTrigger (r, o) =
        let name =
            match r with
            | LeafRule(SpecificField(SpecificValue n), _) when not (Set.contains n vanillaTriggerNames) ->
                Some(StringResource.stringManager.GetStringForID n.normal)
            | NodeRule(SpecificField(SpecificValue n), _) when not (Set.contains n vanillaTriggerNames) ->
                Some(StringResource.stringManager.GetStringForID n.normal)
            | _ -> None

        let effectType =
            if o.comparison then
                EffectType.ValueTrigger
            else
                EffectType.Trigger

        name
        |> Option.map (fun name ->
            DocEffect(name, o.requiredScopes, o.pushScope, effectType, o.description |> Option.defaultValue "", ""))

    let extraFromRules =
        (effects |> List.choose ruleToTrigger |> List.map (fun e -> e :> Effect))

    vanillaTriggers @ extraFromRules

let private updateScriptedEffects (lookup: Lookup) (rules: RootRule list) (embeddedSettings: EmbeddedSettings) =
    let vanillaEffects =
        let se = [] |> List.map (fun e -> e :> Effect)
        let ve = embeddedSettings.effects |> List.map (fun e -> e :> Effect)
        se @ ve

    vanillaEffects

let refreshConfigAfterHook
    (addScriptFormulaScope: bool)
    (wildcardLinks: bool)
    (lookup: Lookup)
    (embedded: EmbeddedSettings)
    =
    let scriptFormulaScopes =
        if addScriptFormulaScope then
            [ scopeManager.AnyScope ]
        else
            []

    let addScriptFormulaLinks (lookup: Lookup) =
        match lookup.typeDefInfo |> Map.tryFind "script_value" with
        | Some vs ->
            let values = vs |> List.map (fun tdi -> tdi.id)

            values
            |> List.map (fun v ->
                Effect(v, scriptFormulaScopes, EffectType.ValueTrigger, Some(TypeRef("script_value", v))))
        | None -> []



    let addModifiersAsTypes (lookup: Lookup) (typesMap: Map<string, TypeDefInfo list>) =
        typesMap.Add(
            "modifier",
            lookup.coreModifiers
            |> List.map (fun m -> createTypeDefInfo false m.tag range.Zero [] [])
        )

    lookup.typeDefInfo <- lookup.typeDefInfo |> addModifiersAsTypes lookup

    let ts =
        updateScriptedTriggers lookup lookup.configRules embedded
        @ addScriptFormulaLinks lookup

    let es = updateScriptedEffects lookup lookup.configRules embedded

    let ls =
        updateEventTargetLinks embedded
        @ addDataEventTargetLinks lookup embedded wildcardLinks

    lookup.allCoreLinks <- ts @ es @ ls

let loadConfigRulesHook (addModifiersWithScopes: Lookup -> RootRule list) rules (lookup: Lookup) embedded =
    let addTriggerDocsScopes (lookup: Lookup) (rules: RootRule list) =
        let addRequiredScopesE (s: StringTokens) (o: Options) =
            let newScopes =
                match o.requiredScopes with
                | [] ->
                    lookup.effectsMap.TryFind(StringResource.stringManager.GetStringForID s.normal)
                    |> Option.map (fun se -> se.Scopes)
                    |> Option.defaultValue []
                | x -> x

            let innerScope =
                match o.pushScope with
                | None ->
                    lookup.effectsMap.TryFind(StringResource.stringManager.GetStringForID s.normal)
                    |> Option.bind (function
                        | :? DocEffect as se -> Some se
                        | _ -> None)
                    |> Option.bind (fun se -> se.Target)
                | x -> x

            { o with
                requiredScopes = newScopes
                pushScope = innerScope }

        let addRequiredScopesT (s: StringTokens) (o: Options) =
            let newScopes =
                match o.requiredScopes with
                | [] ->
                    lookup.triggersMap.TryFind(StringResource.stringManager.GetStringForID s.normal)
                    |> Option.map (fun se -> se.Scopes)
                    |> Option.defaultValue []
                | x -> x

            let innerScope =
                match o.pushScope with
                | None ->
                    lookup.triggersMap.TryFind(StringResource.stringManager.GetStringForID s.normal)
                    |> Option.bind (function
                        | :? DocEffect as se -> Some se
                        | _ -> None)
                    |> Option.bind (fun se -> se.Target)
                | x -> x

            { o with
                requiredScopes = newScopes
                pushScope = innerScope }

        rules
        |> List.collect (function
            | AliasRule("effect", (LeafRule(SpecificField(SpecificValue s), r), o)) ->
                [ AliasRule("effect", (LeafRule(SpecificField(SpecificValue s), r), addRequiredScopesE s o)) ]
            | AliasRule("trigger", (LeafRule(SpecificField(SpecificValue s), r), o)) ->
                [ AliasRule("trigger", (LeafRule(SpecificField(SpecificValue s), r), addRequiredScopesT s o)) ]
            | AliasRule("effect", (NodeRule(SpecificField(SpecificValue s), r), o)) ->
                [ AliasRule("effect", (NodeRule(SpecificField(SpecificValue s), r), addRequiredScopesE s o)) ]
            | AliasRule("trigger", (NodeRule(SpecificField(SpecificValue s), r), o)) ->
                [ AliasRule("trigger", (NodeRule(SpecificField(SpecificValue s), r), addRequiredScopesT s o)) ]
            | AliasRule("effect", (LeafValueRule(SpecificField(SpecificValue s)), o)) ->
                [ AliasRule("effect", (LeafValueRule(SpecificField(SpecificValue s)), addRequiredScopesE s o)) ]
            | AliasRule("trigger", (LeafValueRule(SpecificField(SpecificValue s)), o)) ->
                [ AliasRule("trigger", (LeafValueRule(SpecificField(SpecificValue s)), addRequiredScopesT s o)) ]
            | x -> [ x ])


    // let addModifiersWithScopes (lookup: Lookup) =
    //     let modifierOptions (modifier: ActualModifier) =
    //         let requiredScopes = modifierCategoryManager.SupportedScopes modifier.category
    //
    //         { Options.DefaultOptions with
    //             requiredScopes = requiredScopes }
    //
    //     let processField =
    //         RulesParser.processTagAsField (scopeManager.ParseScope()) scopeManager.AnyScope scopeManager.ScopeGroups
    //
    //     lookup.coreModifiers
    //     |> List.map (fun c ->
    //         AliasRule(
    //             "modifier",
    //             NewRule(LeafRule(processField c.tag, ValueField(ValueType.Float(-1E+12M, 1E+12M))), modifierOptions c)
    //         ))
    //
    let ts = updateScriptedTriggers lookup rules embedded
    let es = updateScriptedEffects lookup rules embedded
    let ls = updateEventTargetLinks embedded
    lookup.allCoreLinks <- ts @ es @ ls
    lookup.coreModifiers <- embedded.modifiers
    // eprintfn "crh %A" ts
    addTriggerDocsScopes lookup (rules @ addModifiersWithScopes lookup)


let refreshConfigBeforeFirstTypesHook (lookup: Lookup) (resources: IResourceAPI<ScriptedEffectComputedData>) _ =
    let modifierEnums =
        { key = "modifiers"
          values = lookup.coreModifiers |> List.map (fun m -> m.tag)
          description = "Modifiers"
          valuesWithRange = lookup.coreModifiers |> List.map (fun m -> m.tag, None) }

    let scriptedEffectKeys =
        (resources.AllEntities()
         |> PSeq.map (fun struct (e, l) ->
             (l.Force().ScriptedEffectParams
              |> (Option.defaultWith (fun () -> CWTools.Games.Compute.EU4.getScriptedEffectParamsEntity e))))
         |> List.ofSeq
         |> List.collect id)

    let scriptedEffectParmas =
        { key = "scripted_effect_params"
          description = "Scripted effect parameter"
          values = scriptedEffectKeys
          valuesWithRange = scriptedEffectKeys |> List.map (fun x -> x, None) }

    let paramsDValues = scriptedEffectKeys |> List.map (fun k -> sprintf "$%s$" k)

    let scriptedEffectParmasD =
        { key = "scripted_effect_params_dollar"
          description = "Scripted effect parameter"
          values = paramsDValues
          valuesWithRange = paramsDValues |> List.map (fun x -> x, None) }

    lookup.enumDefs <-
        lookup.enumDefs
        |> Map.add scriptedEffectParmas.key (scriptedEffectParmas.description, scriptedEffectParmas.valuesWithRange)
        |> Map.add scriptedEffectParmasD.key (scriptedEffectParmasD.description, scriptedEffectParmasD.valuesWithRange)
        |> Map.add modifierEnums.key (modifierEnums.description, modifierEnums.valuesWithRange)

let refreshConfigAfterVarDefHook (addScriptFormulaScope: bool) (lookup: Lookup) _ (embedded: EmbeddedSettings) =
    refreshConfigAfterHook addScriptFormulaScope false lookup embedded

let refreshConfigAfterFirstTypesHook (addScriptFormulaScope: bool) (lookup: Lookup) _ (embedded: EmbeddedSettings) =
    refreshConfigAfterHook addScriptFormulaScope true lookup embedded
