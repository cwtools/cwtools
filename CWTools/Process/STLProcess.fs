namespace CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser.Types
open CWTools.Process
open CWTools.Common.STLConstants
open System
open CWTools.Utilities.Utils
open CWTools.Common
open CWTools.Common.NewScope

module STLProcess =

    let rec scriptedTriggerScope (strict : bool) (effects : (Effect list)) (triggers : (Effect list)) (root : string) (node : Node) =
        let anyBlockKeys = ["OR"; "AND"; "NOR"; "NAND"; "NOT"; "if"; "else"; "hidden_effect"]
        let triggerBlockKeys = ["limit"] //@ targetKeys
        let nodeScopes = node.Children
                        |> List.map (
                            function
                            | x when x.Key = root ->
                                scopeManager.AllScopes |> Set.ofList
                            | x when (x.Key.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase)) ->
                                scopeManager.AllScopes |> Set.ofList
                            // | x when targetKeys |> List.exists (fun y -> y == x.Key) ->
                            //     allScopes
                            | x when anyBlockKeys |> List.exists (fun y -> y == x.Key) ->
                                scriptedTriggerScope strict effects triggers root x
                            | x when triggerBlockKeys |> List.exists (fun y -> y == x.Key) ->
                                scriptedTriggerScope strict triggers triggers root x
                            | x -> Scopes.STL.sourceScope effects x.Key |> Set.ofList
                                // match STLScopes.sourceScope x.Key with
                                // | Some v -> v
                                // | None -> effects |> List.filter (fun (n, _) -> n = x.Key) |> List.map (fun (_, ss) -> ss) |> List.collect id
                        )
        let valueScopes = node.Values
                        //|> List.filter (fun v -> v.Key.StartsWith("@"))
                        |> List.map (
                            function
                            | x when x.Key.StartsWith("@", StringComparison.OrdinalIgnoreCase) -> scopeManager.AllScopes |> Set.ofList
                            | x when x.Key = root -> scopeManager.AllScopes |> Set.ofList
                            | x -> (effects @ triggers) |> List.tryFind (fun e -> e.Name = x.Key) |> (function |Some e -> e.ScopesSet |None -> Set.empty)
                           )
        let combinedScopes = nodeScopes @ valueScopes |> List.map (function | x when Set.isEmpty x -> (if strict then Set.empty else scopeManager.AllScopes |> Set.ofList) |x -> x)
        combinedScopes |> List.fold Set.intersect (scopeManager.AllScopes |> Set.ofList)
        //combinedScopes |> List.fold (fun a b -> Set.intersect (Set.ofList a) (Set.ofList b) |> Set.toList) allScopes

    let findAllUsedEventTargets (event : Node) =
        let fNode = (fun (x : Node) children ->
                        let targetFromString (k : string) = k.Substring(13).Split('.').[0]
                        let inner (leaf : Leaf) = if leaf.Value.ToRawString().StartsWith("event_target:", StringComparison.OrdinalIgnoreCase) then Some (leaf.Value.ToRawString() |> targetFromString) else None
                        match x.Key with
                        |k when k.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase) ->
                           targetFromString k :: ((x.Values |> List.choose inner) @ children)
                        |_ ->
                            ((x.Values |> List.choose inner) @ children)

                        )
        let fCombine = (@)
        event |> (foldNode2 fNode fCombine []) |> Set.ofList

    let findAllSavedEventTargets (event : Node) =
        let fNode = (fun (x : Node) children ->
                        let inner (leaf : Leaf) = if leaf.Key == "save_event_target_as" then Some (leaf.Value.ToRawString()) else None
                        (x.Values |> List.choose inner) @ children
                        )
        let fCombine = (@)
        event |> (foldNode2 fNode fCombine []) |> Set.ofList

    let findAllExistsEventTargets (event : Node) =
        let fNode = (fun (x : Node) children ->
                        let inner (leaf : Leaf) = if leaf.Key == "exists" && leaf.Value.ToRawString().StartsWith("event_target:", StringComparison.OrdinalIgnoreCase) then Some (leaf.Value.ToRawString().Substring(13).Split('.').[0]) else None
                        (x.Values |> List.choose inner) @ children
                        )
        let fCombine = (@)
        event |> (foldNode2 fNode fCombine []) |> Set.ofList

    let findAllSavedGlobalEventTargets (event : Node) =
        let fNode = (fun (x : Node) children ->
                        let inner (leaf : Leaf) = if leaf.Key == "save_global_event_target_as" then Some (leaf.Value.ToRawString()) else None
                        (x.Values |> List.choose inner) @ children
                        )
        let fCombine = (@)
        event |> (foldNode2 fNode fCombine []) |> Set.ofList

    let getScriptedTriggerScope (firstRun: bool) (effectType : EffectType) (effects : Effect list) (triggers : Effect list) ((node, comments) : Node * string list) =
        let scopes = scriptedTriggerScope (not firstRun) effects triggers node.Key node
        let commentString = comments |> List.truncate 5 |> String.concat("\n")
        let globals = findAllSavedGlobalEventTargets node
        let savetargets = findAllSavedEventTargets node
        let usedtargets = findAllUsedEventTargets node
        ScriptedEffect(node.Key, scopes |> Set.toList, effectType, commentString, globals |> Set.toList, savetargets |> Set.toList, usedtargets |> Set.toList)

    let rec cloneNode (source : Node) =
        let rec mapChild =
            function
            | NodeC n ->
                let newNode = Node(n.Key, n.Position)
                newNode.AllArray <- n.AllArray |> Array.map mapChild
                NodeC newNode
            | LeafC l ->
                LeafC (Leaf(l.Key, l.Value, l.Position, l.Operator))
            | LeafValueC lv ->
                LeafValueC (LeafValue(lv.Value, lv.Position))
            | ValueClauseC vc ->
                let newVC = ValueClause([||], vc.Position)
                newVC.AllArray <- vc.AllArray |> Array.map mapChild
                newVC.Keys <- vc.Keys
                ValueClauseC newVC
            | CommentC c ->
                CommentC c
        mapChild (NodeC source) |> function |NodeC node -> node

    let shipProcess = BaseProcess()
    let simpleProcess = BaseProcess()

    let staticModifierCategory (modifiers : System.Linq.ILookup<string,ModifierCategory>) (node : Node) =
        node.Values |> Seq.filter (fun v -> v.Key <> "icon" && v.Key <> "icon_frame")
                    |> Seq.filter (fun v -> modifiers.Contains v.Key)
                    |> Seq.collect (fun v -> modifiers[v.Key])
                    |> Seq.distinct

    let getStaticModifierCategory modifierMap (node : Node) =
        let categories = staticModifierCategory modifierMap node |> List.ofSeq
        {tag = node.Key; categories = categories }


