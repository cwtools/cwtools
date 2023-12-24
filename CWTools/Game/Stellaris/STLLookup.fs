namespace CWTools.Games.Stellaris

open CWTools.Process
open CWTools.Games
open CWTools.Common
open FSharp.Collections.ParallelSeq

module STLLookup =
    type STLComputedData
        (
            eventids,
            referencedtypes,
            hastechs,
            definedvariable,
            withRulesData,
            effectBlocks,
            triggersBlocks,
            scriptedeffectparams,
            savedEventTargets
        ) =
        inherit
            ComputedData(
                referencedtypes,
                definedvariable,
                withRulesData,
                effectBlocks,
                triggersBlocks,
                savedEventTargets
            )

        member __.Eventids: string list = eventids
        member __.Hastechs: string list = hastechs
        member __.ScriptedEffectParams: string list option = scriptedeffectparams

    let getChildrenWithComments (root: Node) =
        let findComment t s (a: Child) =
            match (s, a) with
            | (b, c), _ when b -> (b, c)
            | (_, c), CommentC(_, nc) -> (false, nc :: c)
            | (_, c), NodeC n when n.Key = t -> (true, c)
            | (_, _), _ -> (false, [])

        root.Children
        |> List.map (fun e -> e, root.All |> List.fold (findComment e.Key) (false, []) |> snd)

    let updateScriptedTriggers (resources: IResourceAPI<STLComputedData>) (vanillaTriggers: Effect list) =
        let rawTriggers =
            resources.AllEntities()
            |> List.choose (function
                | struct (f, _) when f.filepath.Contains("scripted_triggers") -> Some f.entity
                | _ -> None)
            |> List.collect getChildrenWithComments

        let mutable final = vanillaTriggers
        let mutable i = 0
        let mutable first = true

        let ff () =
            i <- i + 1
            let before = final
            let vanillaAndFinal = Seq.append vanillaTriggers final
                                    |> Array.ofSeq
            let scopedEffects = final |> Seq.choose (function | :? ScopedEffect as e -> Some e |_ -> None)
                                |> Array.ofSeq
            let effectsInput = final
            let triggersInput = vanillaAndFinal
            let triggerAndEffectMap =
                Seq.append effectsInput triggersInput
                |> Seq.map (fun e -> (e.Name.normal, e.ScopesSet))
                |> Map.ofSeq

            // let mergedTriggers = 
            final <-
                rawTriggers
                |> PSeq.map (fun t ->
                    (STLProcess.getScriptedTriggerScope first EffectType.Trigger effectsInput triggersInput scopedEffects triggerAndEffectMap t)
                    :> Effect)
                |> List.ofSeq

            first <- false
            before = final || i > 10

        while (not (ff ())) do
            ()

        final, vanillaTriggers

    let updateScriptedEffects
        (resources: IResourceAPI<STLComputedData>)
        (vanillaEffects: Effect list)
        (scriptedTriggers: Effect list)
        =
        let rawEffects =
            resources.AllEntities()
            |> List.choose (function
                | struct (f, _) when f.filepath.Contains("scripted_effects") -> Some f.entity
                | _ -> None)
            |> List.collect getChildrenWithComments

        let mutable final = vanillaEffects
        let mutable i = 0
        let mutable first = true

        let ff () =
            i <- i + 1
            let before = final
            let vanillaAndFinal = Seq.append final vanillaEffects
                                    |> Array.ofSeq

            let scopedEffects = vanillaAndFinal |> Seq.choose (function | :? ScopedEffect as e -> Some e |_ -> None)
                                |> Array.ofSeq
            let effectsInput = vanillaAndFinal
            let triggersInput = scriptedTriggers
            
            let triggerAndEffectMap =
                Seq.append effectsInput triggersInput
                |> Seq.map (fun e -> (e.Name.normal, e.ScopesSet))
                |> Map.ofSeq
            final <-
                rawEffects
                |> PSeq.map (fun e ->
                    (STLProcess.getScriptedTriggerScope
                        first
                        EffectType.Effect
                        effectsInput
                        triggersInput
                        scopedEffects
                        triggerAndEffectMap
                        e)
                    :> Effect)
                |> List.ofSeq

            first <- false
            before = final || i > 10

        while (not (ff ())) do
            ()
            
        final, vanillaEffects
