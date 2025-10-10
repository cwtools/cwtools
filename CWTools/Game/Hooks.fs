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
open System.Linq

let globalLocalisation (game: GameObject<_, _>) =
    let locParseErrors =
        game.LocalisationManager.GetLocalisationAPIs()
        <&!&> (fun struct (b, api) -> if b then validateLocalisationSyntax api.Results else OK)

    let globalTypeLoc = game.ValidationManager.ValidateGlobalLocalisation()

    game.Lookup.proccessedLoc
    |> validateProcessedLocalisation game.LocalisationManager.taggedLocalisationKeys
    <&&> locParseErrors
    <&&> globalTypeLoc
    |> (function
    | Invalid(_, es) -> es
    | _ -> [])

let private updateScriptedTriggers (rules: RootRule array) (embeddedSettings: EmbeddedSettings) =
    let vanillaTriggers = embeddedSettings.triggers |> Seq.cast<Effect> |> Seq.toArray
    let vanillaTriggerNames = vanillaTriggers |> Array.map _.Name |> Set.ofArray

    let effects =
        rules
        |> Seq.choose (function
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
        (effects |> Seq.choose ruleToTrigger |> Seq.cast<Effect> |> Seq.toArray)

    Array.append vanillaTriggers extraFromRules

let private updateScriptedEffects (embeddedSettings: EmbeddedSettings) =
    let vanillaEffects =
        let ve = embeddedSettings.effects |> Seq.cast<Effect> |> Seq.toArray
        ve

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
        | Some infos ->
            infos
            |> Seq.map (fun info ->
                Effect(info.id, scriptFormulaScopes, EffectType.ValueTrigger, Some(TypeRef("script_value", info.id))))
            |> Seq.toArray
        | None -> [||]

    let addModifiersAsTypes (lookup: Lookup) (typesMap: Map<string, TypeDefInfo array>) =
        typesMap.Add(
            "modifier",
            lookup.coreModifiers
            |> Array.map (fun m -> createTypeDefInfo false m.tag range.Zero [] [])
        )

    lookup.typeDefInfo <- lookup.typeDefInfo |> addModifiersAsTypes lookup

    let ts =
        Array.append (updateScriptedTriggers lookup.configRules embedded) (addScriptFormulaLinks lookup)

    let es = updateScriptedEffects embedded

    let ls =
        updateEventTargetLinks embedded
        @ addDataEventTargetLinks lookup embedded wildcardLinks
        |> Array.ofList

    lookup.allCoreLinks <- Array.concat [| ts; es; ls |] |> List.ofArray

let private addModifiersWithScopes (lookup: Lookup) =
    let modifierOptions (modifier: ActualModifier) =
        let requiredScopes = modifierCategoryManager.SupportedScopes modifier.category

        { Options.DefaultOptions with
            requiredScopes = requiredScopes }

    let processField =
        RulesParser.processTagAsField (scopeManager.ParseScope()) scopeManager.AnyScope scopeManager.ScopeGroups

    (lookup.coreModifiers
     |> Seq.map (fun c ->
         AliasRule(
             "modifier",
             NewRule(LeafRule(processField c.tag, ValueField(ValueType.Float(-1E+12M, 1E+12M))), modifierOptions c)
         )))
        .Concat(RulesHelpers.generateModifierRulesFromTypes (lookup.typeDefs))
    |> Array.ofSeq

let loadConfigRulesHook (rules: RootRule array) (lookup: Lookup) embedded =
    let addTriggerDocsScopes (lookup: Lookup) (rules: RootRule array) =
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
        |> Array.collect (function
            | AliasRule("effect", (LeafRule(SpecificField(SpecificValue s), r), o)) ->
                [| AliasRule("effect", (LeafRule(SpecificField(SpecificValue s), r), addRequiredScopesE s o)) |]
            | AliasRule("trigger", (LeafRule(SpecificField(SpecificValue s), r), o)) ->
                [| AliasRule("trigger", (LeafRule(SpecificField(SpecificValue s), r), addRequiredScopesT s o)) |]
            | AliasRule("effect", (NodeRule(SpecificField(SpecificValue s), r), o)) ->
                [| AliasRule("effect", (NodeRule(SpecificField(SpecificValue s), r), addRequiredScopesE s o)) |]
            | AliasRule("trigger", (NodeRule(SpecificField(SpecificValue s), r), o)) ->
                [| AliasRule("trigger", (NodeRule(SpecificField(SpecificValue s), r), addRequiredScopesT s o)) |]
            | AliasRule("effect", (LeafValueRule(SpecificField(SpecificValue s)), o)) ->
                [| AliasRule("effect", (LeafValueRule(SpecificField(SpecificValue s)), addRequiredScopesE s o)) |]
            | AliasRule("trigger", (LeafValueRule(SpecificField(SpecificValue s)), o)) ->
                [| AliasRule("trigger", (LeafValueRule(SpecificField(SpecificValue s)), addRequiredScopesT s o)) |]
            | x -> [| x |])


    let ts = updateScriptedTriggers rules embedded
    let es = updateScriptedEffects embedded
    let ls = (updateEventTargetLinks embedded) |> Array.ofList
    lookup.allCoreLinks <- Array.concat [| ts; es; ls |] |> List.ofArray
    lookup.coreModifiers <- embedded.modifiers
    // eprintfn "crh %A" ts
    addTriggerDocsScopes lookup (Array.append rules (addModifiersWithScopes (lookup)))

let refreshConfigBeforeFirstTypesHook (lookup: Lookup) (resources: IResourceAPI<ScriptedEffectComputedData>) _ =
    let modifierEnums =
        { key = "modifiers"
          values = lookup.coreModifiers |> Array.map _.tag
          description = "Modifiers"
          valuesWithRange = lookup.coreModifiers |> Array.map (fun m -> m.tag, None) }

    let scriptedEffectKeys =
        (resources.AllEntities()
         |> PSeq.map (fun struct (e, l) ->
             (l.Force().ScriptedEffectParams
              |> (Option.defaultWith (fun () -> Compute.EU4.getScriptedEffectParamsEntity e))))
         |> Seq.collect id
         |> Seq.toArray)

    let scriptedEffectParmas =
        { key = "scripted_effect_params"
          description = "Scripted effect parameter"
          values = scriptedEffectKeys
          valuesWithRange = scriptedEffectKeys |> Array.map (fun x -> x, None) }

    let paramsDValues = scriptedEffectKeys |> Array.map (fun k -> sprintf "$%s$" k)

    let scriptedEffectParmasD =
        { key = "scripted_effect_params_dollar"
          description = "Scripted effect parameter"
          values = paramsDValues
          valuesWithRange = paramsDValues |> Array.map (fun x -> x, None) }

    lookup.enumDefs <-
        lookup.enumDefs
        |> Map.add scriptedEffectParmas.key (scriptedEffectParmas.description, scriptedEffectParmas.valuesWithRange)
        |> Map.add scriptedEffectParmasD.key (scriptedEffectParmasD.description, scriptedEffectParmasD.valuesWithRange)
        |> Map.add modifierEnums.key (modifierEnums.description, modifierEnums.valuesWithRange)

let refreshConfigAfterVarDefHook (addScriptFormulaScope: bool) (lookup: Lookup) _ (embedded: EmbeddedSettings) =
    refreshConfigAfterHook addScriptFormulaScope false lookup embedded

let refreshConfigAfterFirstTypesHook (addScriptFormulaScope: bool) (lookup: Lookup) _ (embedded: EmbeddedSettings) =
    refreshConfigAfterHook addScriptFormulaScope true lookup embedded
