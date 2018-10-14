namespace CWTools.Process
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
open CWTools.Common

module rec STLProcess =
    let toTriggerBlockKeys = ["limit"; "trigger"; "allow"]
    let _targetKeys = ["THIS"; "ROOT"; "PREV"; "FROM"; "OWNER"; "CONTROLLER"; "CAPITAL"; "SOLAR_SYSTEM"; "LEADER"; "RANDOM"; "FROMFROM"; "FROMFROMFROM"; "FROMFROMFROMFROM"; "PREVPREV"; "PREVPREVPREV"; "PREVPREVPREVPREV";
                        "CAPITAL_SCOPE"]//Added used in STH]
    let targetKeys = _targetKeys |> List.sortByDescending (fun k -> k.Length)
    let toEffectBlockKeys = ["if"; "else"; "tooltip"]
    let ignoreKeys = ["count"; "min_steps"; "max_steps"]


    let rec isTargetKey =
        function
        |"" -> true
        |x ->
            match targetKeys |> List.tryFind (fun f -> x.StartsWith(f, StringComparison.OrdinalIgnoreCase))  with
            |Some s -> if s.Length = x.Length then true else isTargetKey (x.Substring(s.Length + 1))
            |None -> false

    let rec scriptedTriggerScope (strict : bool) (effects : (Effect list)) (triggers : (Effect list)) (root : string) (node : Node) =
        let targetKeys = ["THIS"; "ROOT"; "PREV"; "FROM"; "OWNER"; "CONTROLLER"; "CAPITAL"; "SOLAR_SYSTEM"; "LEADER"; "RANDOM"; "FROMFROM"; "FROMFROMFROM"; "FROMFROMFROMFROM"; "PREVPREV"; "PREVPREVPREV"; "PREVPREVPREVPREV"]
        let anyBlockKeys = ["OR"; "AND"; "NOR"; "NAND"; "NOT"; "if"; "else"; "hidden_effect"]
        let triggerBlockKeys = ["limit"] //@ targetKeys
        let nodeScopes = node.Children
                        |> List.map (
                            function
                            | x when x.Key = root ->
                                allScopesSet
                            | x when (x.Key.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase)) ->
                                allScopesSet
                            // | x when targetKeys |> List.exists (fun y -> y == x.Key) ->
                            //     allScopes
                            | x when anyBlockKeys |> List.exists (fun y -> y == x.Key) ->
                                scriptedTriggerScope strict effects triggers root x
                            | x when triggerBlockKeys |> List.exists (fun y -> y == x.Key) ->
                                scriptedTriggerScope strict triggers triggers root x
                            | x -> STLScopes.sourceScope effects x.Key |> Set.ofList
                                // match STLScopes.sourceScope x.Key with
                                // | Some v -> v
                                // | None -> effects |> List.filter (fun (n, _) -> n = x.Key) |> List.map (fun (_, ss) -> ss) |> List.collect id
                        )
        let valueScopes = node.Values
                        //|> List.filter (fun v -> v.Key.StartsWith("@"))
                        |> List.map (
                            function
                            | x when x.Key.StartsWith("@", StringComparison.OrdinalIgnoreCase) -> allScopesSet
                            | x when x.Key = root -> allScopesSet
                            | x -> (effects @ triggers) |> List.tryFind (fun e -> e.Name = x.Key) |> (function |Some e -> e.ScopesSet |None -> Set.empty)
                           )
        let combinedScopes = nodeScopes @ valueScopes |> List.map (function | x when Set.isEmpty x -> (if strict then Set.empty else allScopesSet) |x -> x)
        combinedScopes |> List.fold Set.intersect allScopesSet
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
    // let nodePickler : (ResizeArray<Child> -> ResizeArray<Child>) =
    //     let mkPickler (resolver : IPicklerResolver) =
    //         let arrayPickler = resolver.Resolve<Leaf array> ()
    //         let writer (w : WriteState) (ns : Lazy<Leaf array>) =
    //             arrayPickler.Write w "value" (ns.Force())
    //         let reader (r : ReadState) =
    //             let v = arrayPickler.Read r "value" in Lazy<Leaf array>.CreateFromValue v
    //         let cloner (c : CloneState) (ns : Lazy<Leaf array>) =
    //             lazy (arrayPickler.Clone c (ns.Force()))
    //         Pickler.FromPrimitives(reader, writer, cloner = cloner)
    //     let registry = new CustomPicklerRegistry()
    //     do registry.RegisterFactory mkPickler
    //     registry.DeclareSerializable<FParsec.Position>()
    //     let cache = PicklerCache.FromCustomPicklerRegistry registry
    //     let binarySerializer = FsPickler.CreateBinarySerializer(picklerResolver = cache)
    //     //(fun (input : ResizeArray<Child>) -> binarySerializer.Pickle input |> binarySerializer.UnPickle)
    //     // (fun (input) -> FsPickler.Clone(input, pickler = (cache.GeneratePickler<ResizeArray<Child>>())))
    //     let pickler = cache.GeneratePickler<ResizeArray<Child>>()
    //     let t = typeof<CloneState>
    //     (fun (input) ->
    //         let state = t.GetConstructors(BindingFlags.Instance ||| BindingFlags.NonPublic).[0].Invoke([|cache; null; null; null|]) :?> CloneState
    //         // let state = t.Assembly.CreateInstance(
    //         //                 t.FullName, false,
    //         //                 BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.,
    //         //                 null, [|cache|], null, null) :?> CloneState
    //         //let state = new CloneState(cache)
    //         pickler.Clone state input

    //     )

    let optionTriggers = ["trigger"; "allow"; "exclusive_trigger"]
    let optionEffects = ["tooltip";]
    let optionExcludes = ["name"; "custom_tooltip"; "response_text"; "is_dialog_only"; "sound"; "ai_chance"; "custom_gui"; "default_hide_option"] @ optionTriggers @ optionEffects
    let optionExcludeSet = optionExcludes |> Set.ofList
    let copyChild (a : Child) =
        match a with
        |NodeC n when Set.contains n.Key optionExcludeSet -> None
        |NodeC n ->
            let children = n.AllChildren |> Seq.choose copyChild //|> Array.ofSeq
            let n2 = (Node(n.Key, n.Position))
            n2.AllChildren <- children |> ResizeArray<Child>
            Some (NodeC n2)
        |LeafC l when Set.contains l.Key optionExcludeSet -> None
        |x -> Some x
        // |LeafValueC lv -> LeafValueC (LeafValue(lv.Value, lv.Position))
        // |LeafC l -> LeafC (Leaf(l.Key, l.Value, l.Position))
        // |CommentC c -> CommentC c
    let filterOptionToEffects (o : Option) =
        let newO = EffectBlock(o.Key, o.Position)
        //let newChildren = FsPickler.Clone(o.AllChildren, pickler = nodePickler)
        newO.AllChildren <- (o.AllChildren |> Seq.choose copyChild |> ResizeArray<Child>)
        //copy o newO
        // newO.All <- newO.All |> List.filter (function |LeafC l -> (not (Set.contains l.Key optionExcludeSet)) | _ -> true)
        // newO.All <- newO.All |> List.filter (function |NodeC l -> (not (Set.contains l.Key optionExcludeSet)) | _ -> true)
        newO.Scope <- o.Scope
        newO
        //newO :> Node

    type Ship (key, pos) =
        inherit Node(key, pos)
        member this.Name = this.TagText "name"
        member this.ShipSize = this.TagText "ship_size"

    type ShipSection (key, pos) =
        inherit Node(key, pos)
        member this.Template = this.TagText "template"
        member this.Slot = this.TagText "slot"

    type ShipComponent (key, pos) =
        inherit Node(key, pos)
        member this.Template = this.TagText "template"
        member this.Slot = this.TagText "slot"

    type Button_Effect (key, pos) =
        inherit Node(key, pos)

    type Event(key, pos) =
        inherit Node(key, pos)
        member this.ID = this.TagText "id"
        member this.Desc = this.TagText "desc"
        member this.Hidden = this.Tag "hide_window" |> (function | Some (Bool b) -> b | _ -> false)

    type EffectBlock(key, pos) = inherit Node(key, pos)
    type TriggerBlock(key, pos) =
        inherit Node(key, pos)
        member val InEffectBlock : bool = false with get, set
    type Option(key, pos) =
        inherit Node(key, pos)
        member this.AsEffectBlock = filterOptionToEffects this
    type ModifierBlock(key, pos) = inherit Node(key, pos)
    type WeightBlock(key, pos) = inherit Node(key, pos)
    type WeightModifierBlock(key, pos) = inherit Node(key, pos)

    let scopedProcessNode<'T when 'T :> Node> (lookup : LookupContext) =
        match lookup.scope with
        |"planet" -> processNode<'T> (fun n -> n.Scope <- Scope.Planet; n)
        |"country" -> processNode<'T> (fun n -> n.Scope <- Scope.Country; n)
        |"fleet" -> processNode<'T> (fun n -> n.Scope <- Scope.Fleet; n)
        |"ship" -> processNode<'T> (fun n -> n.Scope <- Scope.Ship; n)
        |"pop_faction" -> processNode<'T> (fun n -> n.Scope <- Scope.PopFaction; n)
        |"pop" -> processNode<'T> (fun n -> n.Scope <- Scope.Pop; n)
        |"leader" -> processNode<'T> (fun n -> n.Scope <- Scope.Leader; n)
        |_ -> processNode<'T> (fun n -> n.Scope <- Scope.Any; n)

    let specificScopeProcessNode<'T when 'T :> Node> (scope : Scope) (lookup : LookupContext) =
        processNode<'T> (fun n -> n.Scope <- scope; n)

    // let modifierScopeProcessNode (modifier : ModifierCategory) (lookup : LookupContext) =
    //     processNode<ModifierBlock> (fun n -> n.ModifierCategory <- modifier; n)

    // let triggerProcessNode (lookup : LookupContext) =
    //     let postinit =
    //         match lookup.scope with
    //             |"planet" ->  (fun (n : TriggerBlock) -> n.Scope <- Scope.Planet; n)
    //             |"country" ->  (fun n -> n.Scope <- Scope.Country; n)
    //             |"fleet" ->  (fun n -> n.Scope <- Scope.Fleet; n)
    //             |"ship" ->  (fun n -> n.Scope <- Scope.Ship; n)
    //             |"pop_faction" ->  (fun n -> n.Scope <- Scope.PopFaction; n)
    //             |"pop" ->  (fun n -> n.Scope <- Scope.Pop; n)
    //             |_ ->  (fun n -> n.Scope <- Scope.Any; n)
    //     match lookup.parents with
    //     | "effectblock"::_ -> processNode<TriggerBlock> (postinit >> (fun n -> n.InEffectBlock <- true; n))
    //     | _ -> processNode<TriggerBlock> postinit

    let triggerInEffectProcessNode (lookup : LookupContext) =
        processNode<TriggerBlock> (fun n -> n.InEffectBlock <- true; n)

    let stellarisFunction =
        function
        |("ship_design", _, _) -> processNodeSimple<Ship>, "ship", id
        |("section", _, _) -> processNodeSimple<ShipSection>, "shipsection", id;
        |("component", _, _) -> processNodeSimple<ShipComponent>, "shipcomponent", id;
        |("planet_event", _, _) -> processNodeSimple<Event>, "event", (fun c -> {c with scope = "planet"});
        |("country_event", _, _) -> processNodeSimple<Event>, "event", (fun c -> {c with scope = "country"});
        |("fleet_event", _, _) -> processNodeSimple<Event>, "event", (fun c -> {c with scope = "fleet"});
        |("ship_event", _, _) -> processNodeSimple<Event>, "event", (fun c -> {c with scope = "ship"});
        |("pop_faction_event", _, _) -> processNodeSimple<Event>, "event", (fun c -> {c with scope = "pop_faction"});
        |("pop_event", _, _) -> processNodeSimple<Event>, "event", (fun c -> {c with scope = "pop"});
        |("event", _, _) -> processNodeSimple<Event>, "event", id;
        //Events
        |("trigger", _, {parents = "event"::_}) ->  scopedProcessNode<TriggerBlock>, "triggerblock", id;
        |("immediate", _, { parents = "event"::_ }) ->  scopedProcessNode<EffectBlock>, "effectblock", id;
        |("option", _, {parents = "event"::_}) ->  scopedProcessNode<Option>, "option", id;
        |("tooltip", _, {parents = "option"::_; previous = "option"}) ->  scopedProcessNode<EffectBlock>, "effectblock", id;
        |("allow", _, {parents = "option"::_; previous = "option"}) ->  scopedProcessNode<TriggerBlock>, "triggerblock", id;
        |("trigger", _, {parents = "option"::_; previous = "option"}) ->  scopedProcessNode<TriggerBlock>, "triggerblock", id;
        |("exclusive_trigger", _, {parents = "option"::_; previous = "option"}) ->  scopedProcessNode<TriggerBlock>, "triggerblock", id;
        |("desc", _, {parents = "event"::_}) ->  scopedProcessNode<Node>, "eventdesc", id;
        |("trigger", _, {parents = "eventdesc"::"event"::_}) ->  scopedProcessNode<TriggerBlock>, "triggerblock", id;
        |("after", _, {parents = "event"::_}) ->  scopedProcessNode<EffectBlock>, "effectblock", id;
        |("limit", _, {parents = "effectblock"::_}) ->  triggerInEffectProcessNode, "triggerblock", id;
        |("limit", _, {parents = "option"::_}) ->  processNodeSimple<TriggerBlock>, "triggerblock", id;
        |("ai_chance", _, {parents = "option"::_}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"option"::_}) ->  scopedProcessNode<WeightModifierBlock>, "weightmodifierblock", id;
        |("queue_actions",_, {parents = "effectblock"::_}) -> processNodeSimple<Node>, "fleet_action", id;
        // |("while", _, {parents = "fleet_action"::_}) -> processNodeSimple<TriggerBlock>, "triggerblock", id;
        // |("trigger", _, {parents = "fleet_action"::_}) -> processNodeSimple<TriggerBlock>, "triggerblock", id;

        //Buildings
        |(_, p, {complete = false; entityType = EntityType.Buildings}) ->  processNodeSimple<Node>, "building",  (fun c -> { c with complete = true});
        |("potential", _, {parents = "building"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Tile, "triggerblock", id;
        |("allow", _, {parents = "building"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Tile, "triggerblock", id;
        |("ai_allow", _, {parents = "building"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Tile, "triggerblock", id;
        |("destroy_if", _, {parents = "building"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Tile, "triggerblock", id;
        |("active", _, {parents = "building"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Pop, "triggerblock", id;

        |("planet_modifier_with_pop_trigger", _, {parents = "building"::_}) ->  processNodeSimple<Node>, "planetmodpop", id;
        |("potential", _, {parents = "planetmodpop"::"building"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Pop, "triggerblock", id;
        |("modifier", _, {parents = "planetmodpop"::"building"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Planet, "triggerblock", id;

        |("triggered_planet_modifier", _, {parents = "building"::_}) ->  processNodeSimple<Node>, "triggeredplanetmod", id;
        |("potential", _, {parents = "triggeredplanetmod"::"building"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Planet, "triggerblock", id;
        |("modifier", _, {parents = "triggeredplanetmod"::"building"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Planet, "triggerblock", id;

        |("planet_modifier", _, {parents = "building"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Planet, "modifierblock", id;
        |("army_modifier", _, {parents = "building"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Army, "modifierblock", id;
        |("country_modifier", _, {parents = "building"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;

        |("ai_weight", _, {parents = "building"::_}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"building"::_}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Tile, "weightmodifierblock", id;

        //Agendas
        |(_, p, {complete = false; entityType = EntityType.Agenda}) ->  processNodeSimple<Node>, "agenda",  (fun c -> { c with complete = true});
        |("weight_modifier", _, {parents = "agenda"::_}) ->  processNodeSimple<WeightBlock>, "weightmodifier", id;
        |("modifier", _, {parents = "weightmodifier"::"agenda"::_}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Leader, "modifierblock", id;
        |("modifier", _, {parents = "agenda"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        //Armies
        |(_, _, {complete = false; entityType = EntityType.Armies})  ->  processNodeSimple<Node>, "army",  (fun c -> { c with complete = true});
        |("potential", _, {parents = "army"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Planet, "triggerblock", id;
        |("allow", _, {parents = "army"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Planet, "triggerblock", id;
        |("show_tech_unlock_if", _, {parents = "army"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("on_queued", _, {parents = "army"::_}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("on_unqueued", _, {parents = "army"::_}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        //Anomalies
        |(_, _, {complete = false; entityType = EntityType.Anomalies})  ->  processNodeSimple<Node>, "anomalycat",  (fun c -> { c with complete = true});
        |("spawn_chance", _, {parents = "anomalycat"::_}) ->  processNodeSimple<WeightBlock> , "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"anomalycat"::_}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Planet , "weightmodifierblock", id;
        |("on_spawn", _, {parents = "anomalycat"::_}) ->  specificScopeProcessNode<EffectBlock> Scope.Planet, "effectblock", id;
        |("on_success", _, {parents = "anomalycat"::_}) ->  processNodeSimple<Node>, "onsuccess", id;
        |("modifier", _, {parents = "onsuccess"::"anomalycat"::_}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Ship , "weightmodifierblock", id;
        //
        // |("anomaly", _, {complete = false; entityType = EntityType.Anomalies}) ->  processNodeSimple<Node>, "anomaly",  (fun c -> { c with complete = true});
        // |("potential", _, {parents = "anomaly"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Ship, "triggerblock", id;
        // |("anomaly_category", _, {complete = false; entityType = EntityType.Anomalies}) -> processNodeSimple<Node>, "anomalycat",  (fun c -> { c with complete = true});
        // |("potential", _, {parents = "anomalycat"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Planet, "triggerblock", id;
        // |("on_spawn", _, {parents = "anomalycat"::_}) ->  specificScopeProcessNode<EffectBlock> Scope.Planet, "effectblock", id;
        // |("on_success", _, {parents = "anomalycat"::_}) ->  specificScopeProcessNode<EffectBlock> Scope.Ship, "effectblock", id;
        // |("on_fail", _, {parents = "anomalycat"::_}) ->  specificScopeProcessNode<EffectBlock> Scope.Ship, "effectblock", id;
        // |("on_critical_fail", _, {parents = "anomalycat"::_}) ->  specificScopeProcessNode<EffectBlock> Scope.Ship, "effectblock", id;
        // |("spawn_chance", _, {parents = "anomalycat"::_}) ->  processNodeSimple<WeightBlock> , "weightblock", id;
        // |("modifier", _, {parents = "weightblock"::"anomalycat"::_}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Planet , "weightmodifierblock", id;
        //Ascension perks
        |(_, _, {complete = false; entityType = EntityType.AscensionPerks})  ->  processNodeSimple<Node>, "ascension",  (fun c -> { c with complete = true});
        |("potential", _, {parents = "ascension"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("possible", _, {parents = "ascension"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("on_enabled", _, {parents = "ascension"::_}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("ai_weight", _, {parents = "ascension"::_}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"ascension"::_}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Country, "weightmodifierblock", id;
        |("modifier", _, {parents = "ascension"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        //Bombardment stances
        |(_, _, {complete = false; entityType = EntityType.BombardmentStances})  ->  processNodeSimple<Node>, "bombard",  (fun c -> { c with complete = true});
        |("trigger", _, {parents = "bombard"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Fleet, "triggerblock", id;
        |("ai_weight", _, {parents = "bombard"::_}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"bombard"::_}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Fleet, "weightmodifierblock", id;
        //Buildable pops
        |(_, _, {complete = false; entityType = EntityType.BuildablePops})  ->  processNodeSimple<Node>, "buildpops",  (fun c -> { c with complete = true});
        |("potential_build", _, {parents = "buildpops"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("allow", _, {parents = "buildpops"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("show_tech_unlock_if", _, {parents = "buildpops"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        //Button effects #todo effects
        |(_, _, {complete = false; entityType = EntityType.ButtonEffects})  ->  processNodeSimple<Button_Effect>, "buttoneffect",  (fun c -> { c with complete = true});
        //Bypass
        |(_, _, {complete = false; entityType = EntityType.Bypass})  ->  processNodeSimple<Node>, "bypass",  (fun c -> { c with complete = true});
        |("on_pre_explore", _, {parents = "bypass"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Fleet, "effectblock", id;
        |("country_can_use", _, {parents = "bypass"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        //Casus belli
        |(_, _, {complete = false; entityType = EntityType.CasusBelli})  ->  processNodeSimple<Node>, "casusbelli",  (fun c -> { c with complete = true});
        |("potential", _, {parents = "casusbelli"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("is_valid", _, {parents = "casusbelli"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("destroy_if", _, {parents = "casusbelli"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        //Component templates
        |(_, _, {complete = false; entityType = EntityType.ComponentTemplates})  ->  processNodeSimple<Node>, "componenttemplate",  (fun c -> { c with complete = true});
        |("ai_weight", _, {parents = "componenttemplate"::_}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"componenttemplate"::_}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Country, "weightblock", id;
        |("friendly_aura", _, {parents = "componenttemplate"::_;}) ->  processNodeSimple<Node>, "componentaura", id;
        |("hostile_aura", _, {parents = "componenttemplate"::_;}) ->  processNodeSimple<Node>, "componentaura", id;
        |("modifier", _, {parents = "componentaura"::"componenttemplate"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Ship, "modifierblock", id;
        |("ship_modifier", _, {parents = "componenttemplate"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Ship, "modifierblock", id;
        |("modifier", _, {parents = "componenttemplate"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Ship, "modifierblock", id;
        //Country customization
        |(_, _, {complete = false; entityType = EntityType.CountryCustomization})  ->  processNodeSimple<Node>, "countrycustomization",  (fun c -> { c with complete = true});
        |("weight", _, {parents = "countrycustomization"::_}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"countrycustomization"::_}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Country, "weightmodifierblock", id;
        //Country type
        |(_, _, {complete = false; entityType = EntityType.CountryTypes})  ->  processNodeSimple<Node>, "countrytype",  (fun c -> { c with complete = true});
        |("fraction", _, {parents = "countrytype"::_}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"countrytype"::_}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Country, "weightmodifierblock", id;
        //Deposits
        |(_, _, {complete = false; entityType = EntityType.Deposits})  ->  processNodeSimple<Node>, "deposit",  (fun c -> { c with complete = true});
        |("orbital_weight", _, {parents = "deposit"::_}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("drop_weight", _, {parents = "deposit"::_}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"deposit"::_}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Tile, "weightmodifierblock", id;
        //Diplomatic actions
        |(_, _, {complete = false; entityType = EntityType.DiplomaticActions})  ->  processNodeSimple<Node>, "diploact",  (fun c -> { c with complete = true});
        |("potential", _, {parents = "diploact"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("possible", _, {parents = "diploact"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("proposable", _, {parents = "diploact"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("on_accept", _, {parents = "diploact"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("on_decline", _, {parents = "diploact"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        //Edicts
        |("country_edict", _, {complete = false; entityType = EntityType.Edicts}) -> processNodeSimple<Node>, "edict",  (fun c -> { c with complete = true; scope = "country"});
        |("planet_edict", _, {complete = false; entityType = EntityType.Edicts}) -> processNodeSimple<Node>, "edict",  (fun c -> { c with complete = true; scope = "planet"});
        |("potential", _, {parents = "edict"::_;}) ->  scopedProcessNode<TriggerBlock>, "triggerblock", id;
        |("allow", _, {parents = "edict"::_;}) ->  scopedProcessNode<TriggerBlock>, "triggerblock", id;
        |("effect", _, {parents = "edict"::_;}) ->  scopedProcessNode<EffectBlock>, "effectblock", id;
        |("modifier", _, {parents = "edict"::_; previous = "country_edict"}) ->  scopedProcessNode<ModifierBlock>, "modifierblock", id;
        |("modifier", _, {parents = "edict"::_; previous = "planet_edict"}) ->  scopedProcessNode<ModifierBlock>, "modifierblock", id;
        |("ai_weight", _, {parents = "edict"::_;}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"edict"::_;}) ->  scopedProcessNode<WeightModifierBlock>, "weightmodifierblock", id;
        //Ethics
        |(_, _, {complete = false; entityType = EntityType.Ethics})  ->  processNodeSimple<Node>, "ethic",  (fun c -> { c with complete = true;});
        |("playable", _, {parents = "ethic"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("pop_attraction_tag", _, {parents = "ethic"::_;}) ->  processNodeSimple<Node>, "popattractiontag", id;
        |("trigger", _, {parents = "popattractiontag"::"ethic"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("country_modifier", _, {parents = "ethic"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        |("country_attraction", _, {parents = "ethic"::_;}) ->  processNodeSimple<WeightBlock>, "weightblock",  (fun c -> { c with complete = true; scope = "country"});
        |("pop_attraction", _, {parents = "ethic"::_;}) ->  processNodeSimple<WeightBlock>, "weightblock",  (fun c -> { c with complete = true; scope = "pop"});
        |("modifier", _, {parents = "weightblock"::"ethic"::_;}) ->  scopedProcessNode<WeightModifierBlock>, "weightmodifierblock", id;
        //Fallen empires
        |(_, _, {complete = false; entityType = EntityType.FallenEmpires})  ->  processNodeSimple<Node>, "fallenempire",  (fun c -> { c with complete = true;});
        |("create_country_effect", _, {parents = "fallenempire"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Any, "effectblock", id;
        //Government
        |(_, _, {complete = false; entityType = EntityType.Governments})  ->  processNodeSimple<Node>, "government",  (fun c -> { c with complete = true;});
        |("weight", _, {parents = "government"::_;}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"government"::_;}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Country, "weightmodifierblock", id;
        //Government, authoritity
        |(_, _, {complete = false; entityType = EntityType.Authorities})  ->  processNodeSimple<Node>, "authority",  (fun c -> { c with complete = true;});
        |("country_modifier", _, {parents = "authority"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        //Government, civics
        |(_, _, {complete = false; entityType = EntityType.Civics})  ->  processNodeSimple<Node>, "civic",  (fun c -> { c with complete = true;});
        |("random_weight", _, {parents = "civic"::_;}) ->  processNodeSimple<Node>, "randomweight", id;
        |("modifier", _, {parents = "civic"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        //Mandates
        |(_, _, {complete = false; entityType = EntityType.Mandates})  ->  processNodeSimple<Node>, "mandate",  (fun c -> { c with complete = true;});
        |("valid", _, {parents = "mandate"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Leader, "triggerblock", id;
        |("on_term_started", _, {parents = "mandate"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Leader, "effectblock", id;
        |("on_term_ended", _, {parents = "mandate"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Leader, "effectblock", id;
        //Megastrucutres
        |(_, _, {complete = false; entityType = EntityType.Megastructures})  ->  processNodeSimple<Node>, "megastructure",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "megastructure"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("possible", _, {parents = "megastructure"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.GalacticObject, "triggerblock", id;
        |("placement_rules", _, {parents = "megastructure"::_;}) ->  processNodeSimple<Node>, "megastructureplacement", id;
        |("planet_possible", _, {parents = "megastructureplacement"::"megastructure"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Planet, "triggerblock", id;
        |("on_build_start", _, {parents = "megastructure"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.GalacticObject, "effectblock", id;
        |("on_build_cancel", _, {parents = "megastructure"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.GalacticObject, "effectblock", id;
        |("on_build_complete", _, {parents = "megastructure"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.GalacticObject, "effectblock", id;
        |("ai_weight", _, {parents = "megastructure"::_;}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"megastructure"::_;}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.GalacticObject, "weightmodifierblock", id;
        //Observation station missions
        |(_, _, {complete = false; entityType = EntityType.ObservationStationMissions})  ->  processNodeSimple<Node>, "obsstation",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "obsstation"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("valid", _, {parents = "obsstation"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("ai_weight", _, {parents = "obsstation"::_;}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"obsstation"::_;}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Country, "weightmodifierblock", id;
        // Opinion modifiers
        |(_, _, {complete = false; entityType = EntityType.OpinionModifiers})  ->  processNodeSimple<Node>, "opinionmodifier",  (fun c -> { c with complete = true;});
        |("trigger", _, {parents = "opinionmodifier"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("opinion", _, {parents = "opinionmodifier"::_;}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"opinionmodifier"::_;}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Country, "weightmodifierblock", id;
        //Personalities
        |(_, _, {complete = false; entityType = EntityType.Personalities})  ->  processNodeSimple<Node>, "personality",  (fun c -> { c with complete = true;});
        |("allow", _, {parents = "personality"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("weight_modifier", _, {parents = "personality"::_;}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"personality"::_;}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Country, "weightmodifierblock", id;
        //Planet modifiers
        |(_, _, {complete = false; entityType = EntityType.PlanetModifiers})  ->  processNodeSimple<Node>, "planetmod",  (fun c -> { c with complete = true;});
        |("spawn_chance", _, {parents = "planetmod"::_;}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"planetmod"::_;}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Planet, "weightmodifierblock", id;
        //Policies
        |(_, _, {complete = false; entityType = EntityType.Policies})  ->  processNodeSimple<Node>, "policy",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "policy"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("allow", _, {parents = "policy"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("option", _, {parents = "policy"::_;}) ->  processNodeSimple<Node>, "policyoption", id;
        |("valid", _, {parents = "policyoption"::"policy"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "policyoption", id;
        |("on_enabled", _, {parents = "policyoption"::"policy"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("on_disabled", _, {parents = "policyoption"::"policy"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("AI_weight", _, {parents = "policyoption"::"policy"::_;}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "policyoption"::"policy"::_; previous = "option"}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        |("modifier", _, {parents = "weightblock"::"policyoption"::"policy"::_;}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Country, "weightmodifierblock", id;
        //Pop faction types
        |(_, _, {complete = false; entityType = EntityType.PopFactionTypes})  ->  processNodeSimple<Node>, "popfaction",  (fun c -> { c with complete = true;});
        |("is_potential", _, {parents = "popfaction"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("can_join_faction", _, {parents = "popfaction"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Pop, "triggerblock", id;
        |("demand", _, {parents = "popfaction"::_;}) ->  processNodeSimple<Node>, "popfactiondemand", id;
        |("potential", _, {parents = "popfactiondemand"::"popfaction"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.PopFaction, "triggerblock", id;
        |("trigger", _, {parents = "popfactiondemand"::"popfaction"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.PopFaction, "triggerblock", id;
        |("on_create", _, {parents = "popfaction"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.PopFaction, "effectblock", id;
        |("on_destroy", _, {parents = "popfaction"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("actions", _, {parents = "popfaction"::_;}) ->  processNodeSimple<Node>, "popfactionaction", id;
        |("embrace_faction", _, {parents = "popfactionaction"::"popfaction"::_;}) ->  processNodeSimple<Node>, "popfactionaction2", id;
        |("promote_faction", _, {parents = "popfactionaction"::"popfaction"::_;}) ->  processNodeSimple<Node>, "popfactionaction2", id;
        |("cancel_promote_faction", _, {parents = "popfactionaction"::"popfaction"::_;}) ->  processNodeSimple<Node>, "popfactionaction2", id;
        |("suppress_faction", _, {parents = "popfactionaction"::"popfaction"::_;}) ->  processNodeSimple<Node>, "popfactionaction2", id;
        |("cancel_suppress_faction", _, {parents = "popfactionaction"::"popfaction"::_;}) ->  processNodeSimple<Node>, "popfactionaction2", id;
        |("potential", _, {parents = "popfactionaction2"::"popfactionaction"::"popfaction"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.PopFaction, "triggerblock", id;
        |("effect", _, {parents = "popfactionaction2"::"popfactionaction"::"popfaction"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.PopFaction, "effectblock", id;
        |("valid", _, {parents = "popfactionaction2"::"popfactionaction"::"popfaction"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.PopFaction, "triggerblock", id;
        |("ai_weight", _, {parents = "popfactionaction2"::"popfactionaction"::"popfaction"::_;}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"popfactionaction2"::"popfactionaction"::"popfaction"::_;}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.PopFaction, "weightmodifierblock", id;
        |("attraction", _, {parents = "popfaction"::_;}) ->  processNodeSimple<WeightBlock>, "weightblock",  (fun c -> { c with complete = true; scope = "pop"});
        |("leader", _, {parents = "popfaction"::_;}) ->  processNodeSimple<WeightBlock>, "weightblock",  (fun c -> { c with complete = true; scope = "leader"});
        |("modifier", _, {parents = "weightblock"::"popfaction"::_;}) ->  scopedProcessNode<WeightModifierBlock>, "weightmodifierblock", id;
        //Scripted effects
        |(_, _, {complete = false; entityType = EntityType.ScriptedEffects})  ->  specificScopeProcessNode<EffectBlock> Scope.Any, "",  (fun c -> { c with complete = true;});
        //Scripted triggers
        |(_, _, {complete = false; entityType = EntityType.ScriptedTriggers})  ->  specificScopeProcessNode<TriggerBlock> Scope.Any, "",  (fun c -> { c with complete = true;});

        //Section templates
        |(_, _, {complete = false; entityType = EntityType.SectionTemplates})  ->  processNodeSimple<Node>, "sectiontemplate",  (fun c -> { c with complete = true;});
        |("ai_weight", _, {parents = "sectiontemplate"::_;}) ->  processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier", _, {parents = "weightblock"::"sectiontemplate"::_;}) ->  specificScopeProcessNode<WeightModifierBlock> Scope.Country, "weightmodifierblock", id;
        ////TODO: Sector types
        //Ship_sizes
        |(_, _, {complete = false; entityType = EntityType.ShipSizes})  ->  processNodeSimple<Node>, "shipsize",  (fun c -> { c with complete = true;});
        |("possible_starbase", _, {parents = "shipsize"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Starbase, "triggerblock", id;
        ////TODO: improve, Starbase so that both ship and starbase modifiers work
        |("modifier", _, {parents = "shipsize"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Starbase, "modifierblock", id;

        //Solar system
         //|(_, p, c) when not c.complete && globCheckPosition("**\\common\\solar_system_initializers\\**\\*.txt") p ->  processNodeSimple<Node>, "solarsystem",  (fun c -> { c with complete = true;});
        //|(_, _, {complete = false; entityType = EntityType.solar_system_initializers/**})  ->  processNodeSimple<Node>, "solarsystem",  (fun c -> { c with complete = true;});
        |(_, _, {complete = false; entityType = EntityType.SolarSystemInitializers})  ->  processNodeSimple<Node>, "solarsystem",  (fun c -> { c with complete = true;});
        |("init_effect", _, {parents = "solarsystem"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.GalacticObject, "effectblock", id;
        |("planet", _, {parents = "solarsystem"::_;}) ->  processNodeSimple<Node>, "solarplanet", id;
        |("init_effect", _, {parents = "solarplanet"::"solarsystem"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Planet, "effectblock", id;
        |("moon", _, {parents = "solarplanet"::"solarsystem"::_;}) ->  processNodeSimple<Node>, "solarmoon", id;
        |("init_effect", _, {parents = "solarmoon"::"solarplanet"::"solarsystem"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Planet, "effectblock", id;
        |("neighbor_system", _, {parents = "solarsystem"::_;}) ->  processNodeSimple<Node>, "solarneighbor", id;
        |("trigger", _, {parents = "solarneighbor"::"solarsystem"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.GalacticObject, "triggerblock", id;
        //Special projects
        |(_, _, {complete = false; entityType = EntityType.SpecialProjects})  ->  processNodeSimple<Node>, "specialproject",  (fun c -> { c with complete = true;});
        |("on_success", _, {parents = "specialproject"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Any, "effectblock", id;
        |("on_start", _, {parents = "specialproject"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Any, "effectblock", id;
        |("on_progress_25", _, {parents = "specialproject"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Any, "effectblock", id;
        |("on_progress_50", _, {parents = "specialproject"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Any, "effectblock", id;
        |("on_progress_75", _, {parents = "specialproject"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Any, "effectblock", id;
        |("on_fail", _, {parents = "specialproject"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("abort_trigger", _, {parents = "specialproject"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "effectblock", id;
        ///////TODO special project
        //////
        //Species classes
        |(_, _, {complete = false; entityType = EntityType.SpeciesClasses})  ->  processNodeSimple<Node>, "specialclass",  (fun c -> { c with complete = true;});
        //////TODO species classes
        //Species rights
        |(_, _, {complete = false; entityType = EntityType.SpeciesRights})  ->  processNodeSimple<Node>, "speciesrights",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "speciesrights"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Species, "triggerblock", id;
        |("allow", _, {parents = "speciesrights"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Species, "triggerblock", id;
        |("pop_modifier", _, {parents = "speciesrights"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        ////TODO work out scope
        |("ai_will_do"), _, {parents = "speciesrights"::_;} -> processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier"), _, {parents = "weightblock"::"speciesrights"::_;} -> specificScopeProcessNode<WeightModifierBlock> Scope.Species, "weightmodifierblock", id;
        |("modifier", _, {parents = "speciesrights"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Any, "modifierblock", id;
        //Starbase buildings
        |(_, _, {complete = false; entityType = EntityType.StarbaseBuilding})  ->  processNodeSimple<Node>, "starbasebuilding",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "starbasebuilding"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Starbase, "triggerblock", id;
        |("possible", _, {parents = "starbasebuilding"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Starbase, "triggerblock", id;
        |("orbit_modifier", _, {parents = "starbasebuilding"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Ship, "modifierblock", id;
        |("country_modifier", _, {parents = "starbasebuilding"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        |("station_modifier", _, {parents = "starbasebuilding"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Starbase, "modifierblock", id;
        |("ai_weight"), _, {parents = "starbasebuilding"::_;} -> processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier"), _, {parents = "weightblock"::"starbasebuilding"::_;} -> specificScopeProcessNode<WeightModifierBlock> Scope.Starbase, "weightmodifierblock", id;
        //Starbase modules
        |(_, _, {complete = false; entityType = EntityType.StarbaseModules})  ->  processNodeSimple<Node>, "starbasemodule",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "starbasemodule"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Starbase, "triggerblock", id;
        |("possible", _, {parents = "starbasemodule"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Starbase, "triggerblock", id;
        |("country_modifier", _, {parents = "starbasemodule"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        |("station_modifier", _, {parents = "starbasemodule"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Starbase, "modifierblock", id;
        |("triggered_country_modifier", _, {parents = "starbasemodule"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        |("ai_weight"), _, {parents = "starbasemodule"::_;} -> processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier"), _, {parents = "weightblock"::"starbasemodule"::_;} -> specificScopeProcessNode<WeightModifierBlock> Scope.Starbase, "weightmodifierblock", id;
        //Starbase type
        |(_, _, {complete = false; entityType = EntityType.StarbaseTypes})  ->  processNodeSimple<Node>, "starbasetype",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "starbasetype"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Starbase, "triggerblock", id;
        |("weight_modifier"), _, {parents = "starbasetype"::_;} -> processNodeSimple<WeightBlock>, "weightblock", id;
        |("ratio"), _, {parents = "starbasetype"::_;} -> processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier"), _, {parents = "weightblock"::"starbasetype"::_;} -> specificScopeProcessNode<WeightModifierBlock> Scope.Starbase, "weightmodifierblock", id;
        //Start screen messages
        |(_, _, {complete = false; entityType = EntityType.StartScreenMessages})  ->  processNodeSimple<Node>, "startscreen",  (fun c -> { c with complete = true;});
        |("trigger", _, {parents = "startscreen"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        //Strategic resources
        |(_, _, {complete = false; entityType = EntityType.StrategicResources})  ->  processNodeSimple<Node>, "stratres",  (fun c -> { c with complete = true;});
        |("modifier", _, {parents = "stratres"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        //Subjects
        |(_, _, {complete = false; entityType = EntityType.Subjects})  ->  processNodeSimple<Node>, "subject",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "subject"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("effect", _, {parents = "subject"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        //System types
        |(_, _, {complete = false; entityType = EntityType.SystemTypes})  ->  processNodeSimple<Node>, "systemtype",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "systemtype"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.GalacticObject, "triggerblock", id;
        //Technologies
        |(_, _, {complete = false; entityType = EntityType.Technology})  ->  processNodeSimple<Node>, "technology",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "technology"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("modifier", _, {parents = "technology"::_; previous="technology"}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        |("weight_modifier"), _, {parents = "technology"::_;} -> processNodeSimple<WeightBlock>, "weightblock", id;
        |("ai_weight"), _, {parents = "technology"::_;} -> processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier"), _, {parents = "weightblock"::"technology"::_;} -> specificScopeProcessNode<WeightModifierBlock> Scope.Country, "weightmodifierblock", id;
        //Terraform
        |(_, _, {complete = false; entityType = EntityType.Terraform})  ->  processNodeSimple<Node>, "terraform",  (fun c -> { c with complete = true;});
        |("condition", _, {parents = "terraform"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("ai_weight"), _, {parents = "terraform"::_;} -> processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier"), _, {parents = "weightblock"::"terraform"::_;} -> specificScopeProcessNode<WeightModifierBlock> Scope.Country, "weightmodifierblock", id;
        //Tile blockers
        ////I believe from in deposits and tile blockers is planet
        |(_, _, {complete = false; entityType = EntityType.TileBlockers})  ->  processNodeSimple<Node>, "tileblocker",  (fun c -> { c with complete = true;});
        |("spawn_chance"), _, {parents = "tileblocker"::_;} -> processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier"), _, {parents = "weightblock"::"tileblocker"::_;} -> specificScopeProcessNode<WeightModifierBlock> Scope.Tile, "weightmodifierblock", id;

        //Tradition category
        |(_, _, {complete = false; entityType = EntityType.TraditionCategories})  ->  processNodeSimple<Node>, "tradcat",  (fun c -> { c with complete = true;});
        |("tradition_swap", _, {parents = "tradcat"::_;}) ->  processNodeSimple<Node>, "tradcatswap", id;
        |("modifier", _, {parents = "tradcat"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        |("trigger", _, {parents = "tradcatswap"::"tradcat"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("modifier", _, {parents = "tradcatswap"::"tradcat"::_; previous= "tradition_swap"}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        //Tradition
        |(_, _, {complete = false; entityType = EntityType.Traditions})  ->  processNodeSimple<Node>, "trad",  (fun c -> { c with complete = true;});
        |("tradition_swap", _, {parents = "trad"::_;}) ->  processNodeSimple<Node>, "tradswap", id;
        |("modifier", _, {parents = "trad"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        |("trigger", _, {parents = "tradswap"::"trad"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("modifier", _, {parents = "tradswap"::"trad"::_; previous = "tradition_swap"}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        |("ai_weight"), _, {parents = "trad"::_;} -> processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier"), _, {parents = "weightblock"::"trad"::_;} -> specificScopeProcessNode<WeightModifierBlock> Scope.Country, "weightmodifierblock", id;
        |("on_enabled"), _, {parents = "trad"::_;} -> specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("on_enabled"), _, {parents = "tradswap"::"trad"::_;} -> specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        //Traits
        ////TODO: traits ai_weight
        |(_, _, {complete = false; entityType = EntityType.Traits})  ->  processNodeSimple<Node>, "trait",  (fun c -> { c with complete = true;});
        |("leader_potential_add", _, {parents = "trait"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Leader, "triggerblock", id;
        |("species_potential_add", _, {parents = "trait"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Species, "triggerblock", id;
        |("species_potential_remove", _, {parents = "trait"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Species, "triggerblock", id;
        |("modifier", _, {parents = "trait"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        |("ai_weight"), _, {parents = "trait"::_;} -> processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier"), _, {parents = "weightblock"::"trait"::_;} -> specificScopeProcessNode<WeightModifierBlock> Scope.Country, "weightmodifierblock", id;
        //War goals
        |(_, _, {complete = false; entityType = EntityType.WarGoals})  ->  processNodeSimple<Node>, "wargoal",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "wargoal"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("possible", _, {parents = "wargoal"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("on_status_quo", _, {parents = "wargoal"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("on_accept", _, {parents = "wargoal"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("on_wargoal_set", _, {parents = "wargoal"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("ai_weight"), _, {parents = "wargoal"::_;} -> processNodeSimple<WeightBlock>, "weightblock", id;
        |("modifier"), _, {parents = "weightblock"::"wargoal"::_;} -> specificScopeProcessNode<WeightModifierBlock> Scope.Country, "weightmodifierblock", id;
        |_ -> processNodeSimple<Node>, "", (fun c -> {c with complete = true;});

    let shipProcess = BaseProcess(stellarisFunction)
    let simpleProcess = BaseProcess(fun _ -> processNodeSimple<Node>, "", id)

    let staticModifierCategory (modifiers : (string * ModifierCategory list) list) (node : Node) =
        node.Values |> List.filter (fun v -> v.Key <> "icon" && v.Key <> "icon_frame")
                    |> List.map (fun v -> List.tryPick (function |(m, c) when m = v.Key -> Some c |_ -> None) modifiers)
                    |> List.choose id
                    |> List.collect id
                    |> List.distinct

    let getStaticModifierCategory (modifiers : Modifier list) (node : Node) =
        let modifiers2 = modifiers |> List.map (fun t -> t.tag, t.categories)
        let category = staticModifierCategory modifiers2 node
        {tag = node.Key; categories = category; core = false}


