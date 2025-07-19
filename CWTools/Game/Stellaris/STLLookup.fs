namespace CWTools.Games.Stellaris

open System.Collections.Generic
open CWTools.Process
open CWTools.Games
open CWTools.Common
open CWTools.Utilities
open FSharp.Collections.ParallelSeq

module STLLookup =
    let getChildrenWithComments (root: Node) =
        let findComment t s (a: Child) =
            match (s, a) with
            | (b, c), _ when b -> (b, c)
            | (_, c), CommentC(comment) -> (false, comment.Comment :: c)
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

        let scopedEffects =
            vanillaTriggers
            |> Seq.choose (function
                | :? ScopedEffect as e -> Some e
                | _ -> None)
            |> Seq.map (fun x -> (x.Name.lower, x.Scopes))
            |> Map.ofSeq

        let vanillaTriggerDictionary = Dictionary<StringToken, Scope list>()

        vanillaTriggers
        |> Seq.iter (fun x -> vanillaTriggerDictionary[x.Name.normal] <- x.Scopes)
        // let vanillaTriggerMap =
        // vanillaTriggers
        // |> Seq.map (fun e -> (e.Name.normal, e.ScopesSet))
        let mutable final: Effect list = List.empty
        let mutable i = 0
        let mutable first = true

        let ff () =
            i <- i + 1
            let before = final
            // let vanillaAndFinal = Seq.append vanillaTriggers final
            // |> Array.ofSeq
            let triggerAndEffectMap =
                final
                // Seq.append effectsInput triggersInput
                |> Seq.map (fun e -> (e.Name.normal, e.Scopes))
                |> Map.ofSeq

            // let mergedTriggers =
            final <-
                rawTriggers
                |> PSeq.map (fun t ->
                    (STLProcess.getScriptedTriggerScope
                        first
                        EffectType.Trigger
                        scopedEffects
                        vanillaTriggerDictionary
                        triggerAndEffectMap
                        t)
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

        let scopedEffects =
            vanillaEffects
            |> Seq.choose (function
                | :? ScopedEffect as e -> Some e
                | _ -> None)
            |> Seq.map (fun x -> (x.Name.lower, x.Scopes))
            |> Map.ofSeq

        let vanillaBothDictionary = Dictionary<StringToken, Scope list>()

        Seq.append vanillaEffects scriptedTriggers
        |> Seq.iter (fun x -> vanillaBothDictionary[x.Name.normal] <- x.Scopes)
        // let vanillaBothMap =
        // |> Seq.map (fun e -> (e.Name.normal, e.ScopesSet))
        // |> Map.ofSeq
        let mutable final: Effect list = List.empty
        let mutable i = 0
        let mutable first = true

        let ff () =
            i <- i + 1
            let before = final
            // let vanillaAndFinal = Seq.append final
            // |> Array.ofSeq

            // let effectsInput = vanillaAndFinal
            // let triggersInput = scriptedTriggers

            let newFoundMap = final |> Seq.map (fun e -> (e.Name.normal, e.Scopes)) |> Map.ofSeq

            final <-
                rawEffects
                |> PSeq.map (fun e ->
                    (STLProcess.getScriptedTriggerScope
                        first
                        EffectType.Effect
                        scopedEffects
                        vanillaBothDictionary
                        newFoundMap
                        e)
                    :> Effect)
                |> List.ofSeq

            first <- false
            before = final || i > 10

        while (not (ff ())) do
            ()

        final, vanillaEffects
