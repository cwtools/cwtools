namespace CWTools.Validation.Stellaris
open CWTools.Validation.ValidationCore
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser.Types
open CWTools.Process.STLScopes
open CWTools.Common
open CWTools.Common.STLConstants
open DotNet.Globbing
open CWTools.Games
open Newtonsoft.Json.Linq
open CWTools.Utilities.Utils
open System
open Microsoft.FSharp.Collections.Tagged
open System.Collections
open CWTools.Games.Stellaris.STLLookup
open CWTools.Validation.Stellaris.STLValidation
open CWTools.Utilities.Position

module ScopeValidation =
    let valTriggerLeafUsage (modifiers : Modifier list) (scopes : ScopeContext) (leaf : Leaf) =
        match leaf.Key with
        | "has_modifier" -> valStaticModifier modifiers scopes (leaf.Value.ToRawString()) leaf
        | _ -> OK

    let valTriggerNodeUsage (modifiers : Modifier list) (scopes : ScopeContext) (node : Node) =
        match node.Key with
        | "NOT" -> valNotUsage node
        | _ -> OK
    let valEffectLeafUsage (modifiers : Modifier list) (scopes : ScopeContext) (leaf : Leaf) =
        match leaf.Key with
        | "remove_modifier" -> valStaticModifier modifiers scopes (leaf.Value.ToRawString()) leaf
        | _ -> OK

    let valEffectNodeUsage (modifiers : Modifier list) (scopes : ScopeContext) (node : Node) =
        match node.Key with
        | "add_modifier" -> valStaticModifier modifiers scopes (node.TagText "modifier") node
        | _ -> OK

    let eventScope (event : Event) =
        match event.Key with
        |"country_event" -> Scope.Country
        |"fleet_event" -> Scope.Fleet
        |"ship_event" -> Scope.Ship
        |"pop_faction_event" -> Scope.PopFaction
        |"pop_event" -> Scope.Pop
        |"planet_event" -> Scope.Planet
        |_ -> Scope.Army

    let inline handleUnknownTrigger (root : ^a) (key : string) =
        match STLProcess.ignoreKeys |> List.tryFind (fun k -> k == key) with
        |Some _ -> OK //Do better
        |None -> if key.StartsWith("@", StringComparison.OrdinalIgnoreCase) then OK else Invalid [inv (ErrorCodes.UndefinedTrigger key) root]

    let inline handleUnknownEffect root (key : string) =
        match STLProcess.ignoreKeys |> List.tryFind (fun k -> k == key) with
        |Some _ -> OK //Do better
        |None -> if key.StartsWith("@", StringComparison.OrdinalIgnoreCase) then OK else Invalid [inv (ErrorCodes.UndefinedEffect key) root]

    let valTriggerLeaf (triggers : EffectMap) (modifiers : Modifier list) (scopes : ScopeContext) (leaf : Leaf) =
        match triggers.TryFind leaf.Key with
        |Some (:? ScopedEffect as e) -> Invalid [inv (ErrorCodes.IncorrectScopeAsLeaf (e.Name) (leaf.Value.ToRawString())) leaf]
        |Some e ->
            if e.Scopes |> List.contains(scopes.CurrentScope) || scopes.CurrentScope = Scope.Any
            then valTriggerLeafUsage modifiers scopes leaf
            else Invalid [inv (ErrorCodes.IncorrectTriggerScope leaf.Key (scopes.CurrentScope.ToString()) (e.Scopes |> List.map (fun f -> f.ToString()) |> String.concat ", ")) leaf]
        |None -> handleUnknownTrigger leaf leaf.Key

    let valEffectLeaf (effects : EffectMap) (modifiers : Modifier list) (scopes : ScopeContext) (leaf : Leaf) =
        match effects.TryFind leaf.Key with
            |Some (:? ScopedEffect as e) -> Invalid [inv (ErrorCodes.IncorrectScopeAsLeaf (e.Name) (leaf.Value.ToRawString())) leaf]
            |Some e ->
                if e.Scopes |> List.contains(scopes.CurrentScope) || scopes.CurrentScope = Scope.Any
                then valEffectLeafUsage modifiers scopes leaf
                else Invalid [inv (ErrorCodes.IncorrectEffectScope leaf.Key (scopes.CurrentScope.ToString()) (e.Scopes |> List.map (fun f -> f.ToString()) |> String.concat ", ")) leaf]
            |None -> handleUnknownEffect leaf leaf.Key

    let rec valEventTrigger (root : Node) (triggers : EffectMap) (effects : EffectMap) (modifiers : Modifier list) (scopes : ScopeContext) (effect : Child) =
        match effect with
        |LeafC leaf -> valTriggerLeaf triggers modifiers scopes leaf
        |NodeC node ->
            match node.Key with
            |x when STLProcess.toTriggerBlockKeys |> List.exists (fun t -> t == x) ->
                valNodeTriggers root triggers effects modifiers scopes [] node
            |x when ["else"] |> List.exists (fun t -> t == x) ->
                valNodeTriggers node triggers effects modifiers scopes [] node

            // |x when STLProcess.isTargetKey x ->
            //     valNodeTriggers root triggers effects Scope.Any node
            // |x when x.Contains("event_target:") ->

            //     OK //Handle later
            |x when x.Contains("parameter:") ->
                OK //Handle later
            |x ->
                match changeScope false effects triggers x scopes with
                |NewScope (s, ignores) ->
                    valNodeTriggers root triggers effects modifiers s ignores node
                    <&&>
                    valTriggerNodeUsage modifiers scopes node
                |WrongScope (_,_,ss) -> Invalid [inv (ErrorCodes.IncorrectScopeScope x (scopes.CurrentScope.ToString()) (ss |> List.map (fun s -> s.ToString()) |> String.concat ", ")) node]
                |NotFound ->
                    match triggers.TryFind x with
                    |Some e ->
                        if e.Scopes |> List.contains(scopes.CurrentScope) || scopes.CurrentScope = Scope.Any
                        then valTriggerNodeUsage modifiers scopes node
                        else Invalid [inv (ErrorCodes.IncorrectTriggerScope x (scopes.CurrentScope.ToString()) (e.Scopes |> List.map (fun f -> f.ToString()) |> String.concat ", ")) node]
                    // |Some (_, true) -> OK
                    // |Some (t, false) -> Invalid [inv S.Error node (sprintf "%s trigger used in incorrect scope. In %A but expected %s" x scopes.CurrentScope (t.Scopes |> List.map (fun f -> f.ToString()) |> String.concat ", "))]
                    |None -> handleUnknownTrigger node x
        |_ -> OK

    and valEventEffect (root : Node) (triggers : EffectMap) (effects : EffectMap) (modifiers : Modifier list) (scopes : ScopeContext) (effect : Child) =
        match effect with
        |LeafC leaf -> valEffectLeaf effects modifiers scopes leaf
        |NodeC node ->
            match node.Key with
            |x when STLProcess.toTriggerBlockKeys |> List.exists (fun t -> t == x) ->
                valNodeTriggers root triggers effects modifiers scopes [] node
            |x when ["else"] |> List.exists (fun t -> t == x) ->
                valNodeEffects node triggers effects modifiers scopes [] node
            // |x when STLProcess.isTargetKey x ->
            //     OK //Handle later
            // |x when x.Contains("event_target:") ->
            //     OK //Handle later
            |x when x.Contains("parameter:") ->
                OK //Handle later
            |x ->
                match changeScope false effects triggers x scopes with
                |NewScope (s, ignores) -> valNodeEffects node triggers effects modifiers s ignores node
                |WrongScope (_,_,ss) -> Invalid [inv (ErrorCodes.IncorrectScopeScope x (scopes.CurrentScope.ToString()) (ss |> List.map (fun s -> s.ToString()) |> String.concat ", ")) node]
                |NotFound ->
                    match effects.TryFind x with
                    |Some e ->
                        if e.Scopes |> List.contains(scopes.CurrentScope) || scopes.CurrentScope = Scope.Any
                        then valEffectNodeUsage modifiers scopes node
                        else Invalid [inv (ErrorCodes.IncorrectEffectScope x (scopes.CurrentScope.ToString()) (e.Scopes  |> List.map (fun f -> f.ToString()) |> String.concat ", ")) node]
                    // |Some(_, true) -> OK
                    // |Some (t, false) -> Invalid [inv S.Error node (sprintf "%s effect used in incorrect scope. In %A but expected %s" x scopes.CurrentScope (t.Scopes  |> List.map (fun f -> f.ToString()) |> String.concat ", "))]
                    |None -> handleUnknownEffect node x
        |_ -> OK

    and valNodeTriggers (root : Node) (triggers : EffectMap) (effects : EffectMap) (modifiers : Modifier list) (scopes : ScopeContext) (ignores : string list) (node : Node) =
        // let scopedTriggers = triggers |> List.map (fun (e, _) -> e, scopes.CurrentScope = Scope.Any || e.Scopes |> List.exists (fun s -> s = scopes.CurrentScope))
        // let scopedEffects = effects |> List.map (fun (e, _) -> e,  scopes.CurrentScope = Scope.Any || e.Scopes |> List.exists (fun s -> s = scopes.CurrentScope))
        let filteredAll =
            node.All
            |> List.filter (function |NodeC c -> not (List.exists (fun i -> i == c.Key) ignores) |LeafC c -> not (List.exists (fun i -> i == c.Key) ignores) |_ -> false)
        List.map (valEventTrigger root triggers effects modifiers scopes) filteredAll |> List.fold (<&&>) OK

    and valNodeEffects (root : Node) (triggers : EffectMap) (effects : EffectMap) (modifiers : Modifier list) (scopes : ScopeContext) (ignores : string list) (node : Node) =
        //let scopedTriggers = triggers |> List.map (fun (e, _) -> e, scopes.CurrentScope = Scope.Any || e.Scopes |> List.exists (fun s -> s = scopes.CurrentScope))
        //let scopedEffects = effects |> List.map (fun (e, _) -> e, scopes.CurrentScope = Scope.Any || e.Scopes |> List.exists (fun s -> s = scopes.CurrentScope))
        let filteredAll =
            node.All
            |> List.filter (function |NodeC c -> not (List.exists (fun i -> i == c.Key) ignores) |LeafC c -> not (List.exists (fun i -> i == c.Key) ignores) |_ -> false)
        List.map (valEventEffect root triggers effects modifiers scopes) filteredAll |> List.fold (<&&>) OK

    //let applyTriggerToScope (triggers : EffectMap) (effects : EffectMap) (scopes : ScopeContext) (key : string) =

    let applyEffectToScope (triggers : EffectMap) (effects : EffectMap) (scopes : ScopeContext) (key : string) =
        match key with
        |x when STLProcess.toTriggerBlockKeys |> List.exists (fun t -> t == x) -> true, scopes
        |x when ["else"] |> List.exists (fun t -> t == x) -> false, scopes
        |x when x.Contains("parameter:") -> false, scopes
        |x ->
            match changeScope false effects triggers x scopes with
                |NewScope (s, _) -> false, s
                |WrongScope _ -> false, scopes
                |NotFound -> false, scopes

    let getScopeContextAtPos (targetpos : pos) (triggers : Effect list) (effects : Effect list) (root : Node) =
        let triggerMap = triggers |> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))
        let effectMap = effects |> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))

        let rec getBlock (pos : pos) (node : Node) =
            match node.Children |> List.tryFind (fun c -> rangeContainsPos c.Position pos) with
            | Some (:? EffectBlock as e) -> Some (false, e :> Node, e.Scope)
            | Some (:? TriggerBlock as e) -> Some (true, e :> Node, e.Scope)
            | Some (:? WeightModifierBlock as e) -> Some (true, e :> Node, e.Scope)
            | Some (:? Option as e) -> Some (true, e.AsEffectBlock :> Node, e.Scope)
            | Some c -> getBlock pos c
            | None -> None
        let rec getPathFromEffectBlockAndPos (pos : pos) (stack : string list) (node : Node) =
            match node.Children |> List.tryFind (fun c -> rangeContainsPos c.Position pos) with
            | Some c -> getPathFromEffectBlockAndPos pos (c.Key :: stack) c
            | None -> stack
        let rec getScopeFromPath (stack : string list) (scopes : ScopeContext) (isTrigger : bool) =
            match stack with
            |[] -> scopes
            |head::tail ->
                let trig, newscopes =
                    match isTrigger with
                    |true -> applyEffectToScope triggerMap effectMap scopes head
                    |false -> applyEffectToScope triggerMap effectMap scopes head
                getScopeFromPath tail newscopes trig
        match getBlock targetpos root with
        |Some (isTrigger, startingBlock, scope) ->
            let path = getPathFromEffectBlockAndPos targetpos [] startingBlock |> List.rev
            Some (getScopeFromPath path {Root = scope; Scopes = [scope]; From = []} isTrigger)
        |None -> None
        // let rec getScopeFromPath (stack : string list) (scopes : ScopeContext) (node : Node) =
        //     match stack with
        //     |[] -> scopes
        //     |head::tail ->
        //         match node.Child head with
        //         |Some c -> applyEffectToScope ()
        //         |None -> scopes

    let valOption (root : Node) (triggers : EffectMap) (effects : EffectMap) (modifiers : Modifier list) (scopes : ScopeContext) (node : Node) =
        let optionTriggers = ["trigger"; "allow"]
        let optionEffects = ["tooltip"; "hidden_effect"]
        let optionExcludes = ["name"; "custom_tooltip"; "response_text"; "is_dialog_only"; "sound"; "ai_chance"; "custom_gui"; "default_hide_option"]
        let filterFunction =
            function
            | NodeC n -> optionExcludes |> List.exists (fun f -> n.Key == f)
            | LeafC l -> optionExcludes |> List.exists (fun f -> l.Key == f)
            | _ -> false
        let leaves = node.Values |> List.filter (fun l -> not (optionExcludes |> List.exists (fun f -> l.Key == f)))
        let children = node.Children |> List.filter (fun c -> not (optionExcludes |> List.exists (fun f -> c.Key == f)))
        let lres = leaves <&!&> (valEffectLeaf effects modifiers scopes)
        let tres = children |> List.filter (fun c -> optionTriggers |> List.exists (fun f -> c.Key == f))
                    <&!&> (NodeC >> valEventTrigger root triggers effects modifiers scopes)
        let effectPos = children |> List.filter (fun c -> not (optionTriggers |> List.exists (fun f -> c.Key == f)))
        let eres = effectPos |> List.filter (fun c -> not (optionEffects |> List.exists (fun f -> c.Key == f)))
                    <&!&> (NodeC >> valEventEffect root triggers effects modifiers scopes)
        let esres = effectPos |> List.filter (fun c -> optionEffects |> List.exists (fun f -> c.Key == f))
                    <&!&> (valNodeEffects root triggers effects modifiers scopes [])
        lres <&&> tres <&&> eres <&&> esres
        //children |> List.map (valEventEffect node triggers effects scopes) |> List.fold (<&&>) OK


    // let valEventTriggers  (triggers : (Effect) list) (effects : (Effect) list) (modifiers : Modifier list) (event : Event) =
    //     let eventScope = { Root = eventScope event; From = []; Scopes = [eventScope event]}
    //     //let eventScope = Seq.append [eventScope event] (Seq.initInfinite (fun _ -> Scope.Any))
    //     // let scopedTriggers = triggers |> List.map (fun e -> e, e.Scopes |> List.exists (fun s -> s = eventScope.CurrentScope))
    //     // let scopedEffects = effects |> List.map (fun e -> e, e.Scopes |> List.exists (fun s -> s = eventScope.CurrentScope))
    //     match event.Child "trigger" with
    //     |Some n ->
    //         let v = List.map (valEventTrigger event triggers effects modifiers eventScope) n.All
    //         v |> List.fold (<&&>) OK
    //     |None -> OK
    /// Flawed, caqn't deal with DU
    let rec copy source target =
        let properties (x:obj) = x.GetType().GetProperties()
        query {
            for s in properties source do
            join t in properties target on (s.Name = t.Name)
            where t.CanWrite
            select s }
        |> Seq.iter (fun s ->
            let value = s.GetValue(source,null)
            if value.GetType().FullName.StartsWith("System.")
            then s.SetValue(target, value, null)
            else copy value (s.GetValue(target,null))
        )



    let valEffectsNew (triggers : EffectMap) (effects : EffectMap) (modifiers : Modifier list) (effectBlock : Node) =
        let scope = { Root = effectBlock.Scope; From = []; Scopes = [effectBlock.Scope]}
        effectBlock.All <&!&> valEventEffect effectBlock triggers effects modifiers scope

    let valAllEffects : LookupValidator<STLComputedData> =
        (fun lu _ es ->
            let triggers = lu.scriptedTriggers
            let effects = lu.scriptedEffects
            let modifiers = lu.staticModifiers
            let effectMap = effects |> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))
            let triggerMap = triggers |> List.map (fun t -> t.Name, t) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))
            let fNode = (fun (x : Node) children ->
                match x with
                | (:? EffectBlock as x) -> valEffectsNew triggerMap effectMap modifiers x
                | (:? Option as x) -> x |> filterOptionToEffects |> (fun o -> valEffectsNew triggerMap effectMap modifiers o)
                | _ -> OK
                <&&> children)
            let fseNode = (fun (x : Node) ->

                let scope = { Root = Scope.Any; From = []; Scopes = [Scope.Any]}
                x.Children <&!&> (fun f -> valEventEffect x triggerMap effectMap modifiers scope (NodeC f)))
            let fstNode = (fun (x : Node)  ->
                let scope = { Root = Scope.Any; From = []; Scopes = [Scope.Any]}
                x.Children <&!&> (fun f -> valEventTrigger x triggerMap effectMap modifiers scope (NodeC f)))

            let fCombine = (<&&>)
            // let opts = es.All |> List.collect (foldNode2 foNode (@) []) |> List.map filterOptionToEffects
            // es.All @ opts <&!&> foldNode2 fNode fCombine OK
            //<&&>
            let b = (es.All <&!!&> foldNode2 fNode fCombine OK)
            //<&&>
            // let c = (es.AllOfTypeChildren EntityType.ScriptedEffects <&!&> fseNode)
            // //<&&>
            // let d = (es.AllOfTypeChildren EntityType.ScriptedTriggers <&!&> fstNode)
            b) //<&&> c <&&> d


    let valTriggersNew (triggers : EffectMap) (effects : EffectMap) (modifiers : Modifier list) (effectBlock : Node) =
        let scope = { Root = effectBlock.Scope; From = []; Scopes = [effectBlock.Scope]}
        effectBlock.All <&!&> valEventTrigger effectBlock triggers effects modifiers scope


    let valAllTriggers : LookupValidator<STLComputedData> =
        (fun lu _ es ->
            let triggers = lu.scriptedTriggers
            let effects = lu.scriptedEffects
            let modifiers = lu.staticModifiers
            let effectMap = effects |> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))
            let triggerMap = triggers |> List.map (fun t -> t.Name, t) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))
            let fNode = (fun (x : Node) children ->
                match x with
                | (:? TriggerBlock as x) when not x.InEffectBlock -> valTriggersNew triggerMap effectMap modifiers x
                | _ -> OK
                <&&> children)
            let fCombine = (<&&>)
            es.All <&!!&> foldNode2 fNode fCombine OK)

    let filterWeightBlockToTriggers (wmb : STLProcess.WeightModifierBlock) =
        let excludes = ["factor"; "add"; "weight"]
        let newWmb = WeightModifierBlock(wmb.Key, wmb.Position)
        copy wmb newWmb
        newWmb.All <- newWmb.All |> List.filter (function |LeafC l -> (not (List.contains l.Key excludes)) | _ -> true)
        newWmb.All <- newWmb.All |> List.filter (function |NodeC l -> (not (List.contains l.Key excludes)) | _ -> true)
        newWmb.Scope <- wmb.Scope
        // eprintfn "%A" (wmb.Values |> List.map (fun  c -> c.Key))
        // eprintfn "%A" (newWmb.Values |> List.map (fun  c -> c.Key))
        newWmb :> Node

    let validateModifierBlocks : LookupValidator<STLComputedData> =
        (fun lu _ es ->
            let triggers = lu.scriptedTriggers
            let effects = lu.scriptedEffects
            let modifiers = lu.staticModifiers
            let effectMap = effects |> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))
            let triggerMap = triggers |> List.map (fun t -> t.Name, t) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))
            let foNode = (fun (x : Node) children ->
                match x with
                | (:? WeightModifierBlock as x) -> x |> filterWeightBlockToTriggers |> (fun o -> valTriggersNew triggerMap effectMap modifiers o)
                | _ -> OK
                <&&> children)
            let fCombine = (<&&>)
            es.All <&!!&> foldNode2 foNode fCombine OK)