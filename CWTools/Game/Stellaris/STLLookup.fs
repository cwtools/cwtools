namespace CWTools.Games.Stellaris
open CWTools.Process
open CWTools.Games
open CWTools.Common

module STLLookup =
    type FlagType = |Country |Planet |Fleet |Ship |Pop |Global |Star |Relation |Leader |AmbientObject |Species |Megastructure |PopFaction


    type STLComputedData(eventids, setvariables, setflags, savedeventtargets, referencedtypes, hastechs, definedvariable, withRulesData, effectBlocks, triggersBlocks, scriptedeffectparams, savedEventTargets) =
        inherit ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks, savedEventTargets)
        member __.Eventids : string list = eventids
        member __.Setvariables : string list = setvariables
        member __.Setflags : (FlagType * string) list = setflags
        member __.Savedeventtargets : string list = savedeventtargets
        member __.Hastechs : string list = hastechs
        member __.ScriptedEffectParams : (string * bool) list option = scriptedeffectparams

    let getChildrenWithComments (root : Node) =
        let findComment t s (a : Child) =
            match (s, a) with
            | ((b, c), _) when b -> (b, c)
            | ((_, c), CommentC (_, nc)) -> (false, nc::c)
            | ((_, c), NodeC n) when n.Key = t -> (true, c)
            | ((_, _), _) -> (false, [])
        root.Children |> List.map (fun e -> e, root.All |> List.fold (findComment e.Key) (false, []) |> snd)

    let updateScriptedTriggers (resources : IResourceAPI<STLComputedData>) (vanillaTriggers : Effect list) =
        let rawTriggers =
            resources.AllEntities()
            |> List.choose (function |struct (f, _) when f.filepath.Contains("scripted_triggers") -> Some (f.entity) |_ -> None)
            |> List.collect getChildrenWithComments
        let mutable final = vanillaTriggers
        let mutable i = 0
        let mutable first = true
        let ff() =
            i <- i + 1
            let before = final
            final <- rawTriggers |> List.map (fun t -> (STLProcess.getScriptedTriggerScope first EffectType.Trigger (final) (vanillaTriggers @ final) t) :> Effect)
            first <- false
            before = final || i > 10
        while (not (ff())) do ()

        final, vanillaTriggers

    let manualEffectScopeOverrides =
        [
            "set_variable", [scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "GalacticObject"; scopeManager.ParseScope() "Leader"; scopeManager.ParseScope() "Federation"]
            "change_variable", [scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "GalacticObject"; scopeManager.ParseScope() "Leader"; scopeManager.ParseScope() "Federation"]
            "subtract_variable", [scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "GalacticObject"; scopeManager.ParseScope() "Leader"; scopeManager.ParseScope() "Federation"]
            "multiply_variable", [scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "GalacticObject"; scopeManager.ParseScope() "Leader"; scopeManager.ParseScope() "Federation"]
            "divide_variable", [scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "GalacticObject"; scopeManager.ParseScope() "Leader"; scopeManager.ParseScope() "Federation"]
        ] |> Map.ofList

    let updateScriptedEffects (resources : IResourceAPI<STLComputedData>) (vanillaEffects : Effect list) (scriptedTriggers : Effect list) =
        let rawEffects =
            resources.AllEntities()
            |> List.choose (function |struct (f, _) when f.filepath.Contains("scripted_effects") -> Some (f.entity) |_ -> None)
            |> List.collect getChildrenWithComments
        let mutable final = vanillaEffects
        let mutable i = 0
        let mutable first = true
        let ff() =
            i <- i + 1
            let before = final
            final <- rawEffects |>  List.map (fun e -> (STLProcess.getScriptedTriggerScope first EffectType.Effect (final @ vanillaEffects) scriptedTriggers e) :> Effect)
            first <- false
            before = final || i > 10
        while (not (ff())) do ()
        let adjustedEffects =
            vanillaEffects
            |> List.map (function | :? DocEffect as ve when manualEffectScopeOverrides.ContainsKey ve.Name ->
                                    let newScopes = manualEffectScopeOverrides.[ve.Name]
                                    (DocEffect(ve.Name, newScopes, ve.Target, ve.Type, ve.Desc, ve.Usage)) :> Effect
                                  | x -> x)
        final, adjustedEffects
