namespace CWTools.Games.Stellaris
open CWTools.Process.ProcessCore
open CWTools.Parser.Types
open CWTools.Localisation
open CWTools.Process.ProcessCore
open CWTools.Process
open CWTools.Process.STLScopes
open CWTools.Common.STLConstants
open DotNet.Globbing
open System
open CWTools.Utilities.Utils
open CWTools.Games
open Microsoft.FSharp.Compiler.Range

module STLLookup =
    type FlagType = |Country |Planet |Fleet |Ship |Pop |Global |Star |Relation |Leader |AmbientObject |Species |Megastructure |PopFaction

    type STLComputedData = {
        eventids : string list
        setvariables : string list
        setflags : (FlagType * string) list
        savedeventtargets : string list
        referencedtypes : Map<string, (string  * range) list> option
        hastechs : string list
    }

    let getChildrenWithComments (root : Node) =
        let findComment t s (a : Child) =
            match (s, a) with
            | ((b, c), _) when b -> (b, c)
            | ((_, c), CommentC nc) -> (false, nc::c)
            | ((_, c), NodeC n) when n.Key = t -> (true, c)
            | ((_, _), _) -> (false, [])
        root.Children |> List.map (fun e -> e, root.All |> List.fold (findComment e.Key) (false, []) |> snd)

    let updateScriptedTriggers (resources : IResourceAPI<STLComputedData>) (vanillaTriggers : Effect list) =
        let rawTriggers =
            resources.AllEntities()
            |> List.choose (function |struct (f, _) when f.filepath.Contains("scripted_triggers") -> Some (f.entity) |_ -> None)
            |> List.collect getChildrenWithComments
            //|> List.rev
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

        final @ vanillaTriggers

    let manualEffectScopeOverrides =
        [
            "set_variable", [Scope.Planet; Scope.Country; Scope.Fleet; Scope.GalacticObject; Scope.Leader]
            "change_variable", [Scope.Planet; Scope.Country; Scope.Fleet; Scope.GalacticObject; Scope.Leader]
            "subtract_variable", [Scope.Planet; Scope.Country; Scope.Fleet; Scope.GalacticObject; Scope.Leader]
            "multiply_variable", [Scope.Planet; Scope.Country; Scope.Fleet; Scope.GalacticObject; Scope.Leader]
            "divide_variable", [Scope.Planet; Scope.Country; Scope.Fleet; Scope.GalacticObject; Scope.Leader]
        ] |> Map.ofList

    let updateScriptedEffects (resources : IResourceAPI<STLComputedData>) (vanillaEffects : Effect list) (scriptedTriggers : Effect list) =
        let rawEffects =
            resources.AllEntities()
            |> List.choose (function |struct (f, _) when f.filepath.Contains("scripted_effects") -> Some (f.entity) |_ -> None)
            |> List.collect getChildrenWithComments
            //|> List.rev
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
                                    (DocEffect(ve.Name, newScopes, ve.Type, ve.Desc, ve.Usage)) :> Effect
                                  | x -> x)
        final @ adjustedEffects
