namespace CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser
open CWTools.Localisation
open CWTools.Process.ProcessCore
open CWTools.Process
open CWTools.Process.STLScopes
open CWTools.Common.STLConstants
open DotNet.Globbing
open System
open CWTools.Utilities.Utils

module STLProcess =
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

    let rec scriptedTriggerScope (strict : bool) (effects : (string * Scope list) list) (triggers : (string * Scope list) list) (root : string) (node : Node) =
        let targetKeys = ["THIS"; "ROOT"; "PREV"; "FROM"; "OWNER"; "CONTROLLER"; "CAPITAL"; "SOLAR_SYSTEM"; "LEADER"; "RANDOM"; "FROMFROM"; "FROMFROMFROM"; "FROMFROMFROMFROM"; "PREVPREV"; "PREVPREVPREV"; "PREVPREVPREVPREV"]
        let anyBlockKeys = ["OR"; "AND"; "NOR"; "NAND"; "NOT"; "if"; "else"; "hidden_effect"]
        let triggerBlockKeys = ["limit"] //@ targetKeys
        let nodeScopes = node.Children 
                        |> List.map (
                            function
                            | x when x.Key = root -> 
                                allScopes
                            | x when (x.Key.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase)) ->
                                allScopes
                            | x when targetKeys |> List.exists (fun y -> y == x.Key) ->
                                allScopes
                            | x when anyBlockKeys |> List.exists (fun y -> y == x.Key) ->
                                scriptedTriggerScope strict effects triggers root x
                            | x when triggerBlockKeys |> List.exists (fun y -> y == x.Key) -> 
                                scriptedTriggerScope strict triggers triggers root x
                            | x ->
                                match STLScopes.sourceScope x.Key with
                                | Some v -> v
                                | None -> effects |> List.filter (fun (n, _) -> n = x.Key) |> List.map (fun (_, ss) -> ss) |> List.collect id
                        )
        let valueScopes = node.Values 
                        //|> List.filter (fun v -> v.Key.StartsWith("@"))
                        |> List.map (
                            function
                            | x when x.Key.StartsWith("@") -> allScopes
                            | x when x.Key = root -> allScopes
                            | x -> effects |> List.tryFind (fun (n, _) -> n = x.Key) |> (function |Some (_,s) -> s |None -> [])
                           )
        let combinedScopes = nodeScopes @ valueScopes |> List.map (function | [] -> (if strict then [] else allScopes) |x -> x)
        combinedScopes |> List.fold (fun a b -> Set.intersect (Set.ofList a) (Set.ofList b) |> Set.toList) allScopes
        // let valueTriggers = node.Values |> List.choose (fun v -> if List.contains v.Key anyBlockKeys then None else Some v.Key)
        // //valueTriggers |> List.iter (fun f -> printfn "%A" f)
        // let nodeScopeChanges = node.Children |> List.choose (fun v -> sourceScope v.Key) //|> List.map (fun x -> [x])
        // let nodeSameScope = node.Children |> List.choose (fun v -> match sourceScope v.Key with |Some s -> None |None -> Some v)
        // let nodeTriggers = nodeSameScope |> List.choose (fun v -> if List.contains v.Key anyBlockKeys then None else Some v.Key)
        // let nodeLimit = nodeSameScope |> List.choose (fun v -> if List.contains v.Key triggerBlockKeys then Some v else None)
        // let valueScopes = (valueTriggers @ nodeTriggers)
        //                 |> List.map (fun v -> 
        //                     effects 
        //                     |> List.filter (fun (n, ss) -> n = v)
        //                     |> List.map (fun (n, ss) -> ss)
        //                     |> List.collect id)
        // let nodeRecTriggers = nodeSameScope |> List.choose (fun v -> if List.contains v.Key anyBlockKeys && not (List.contains v.Key triggerBlockKeys) then Some v else None)
        
        // let nodeScopes =
        //     nodeRecTriggers
        //     |> List.map (scriptedTriggerScope effects triggers)
        // let limitScopes = nodeLimit |> List.map (scriptedTriggerScope triggers triggers)
        //nodeScopes @ valueScopes @ nodeScopeChanges @ limitScopes
        //        |> List.fold (fun a b -> Set.intersect (Set.ofList a) (Set.ofList b) |> Set.toList) allScopes
    let findAllUsedEventTargets (event : Node) =
        let fNode = (fun (x : Node) children ->
                        let targetFromString (k : string) = k.Substring(13).Split('.').[0]
                        let inner (leaf : Leaf) = if leaf.Value.ToRawString().StartsWith("event_target:") then Some (leaf.Value.ToRawString() |> targetFromString) else None
                        match x.Key with
                        |k when k.StartsWith("event_target:") -> 
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
                        let inner (leaf : Leaf) = if leaf.Key == "exists" && leaf.Value.ToRawString().StartsWith("event_target:") then Some (leaf.Value.ToRawString().Substring(13).Split('.').[0]) else None
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
        let effects2 = effects |> List.map (fun t -> t.Name, t.Scopes)
        let triggers2 = triggers |> List.map (fun t -> t.Name, t.Scopes)
        let scopes = scriptedTriggerScope firstRun effects2 triggers2 node.Key node
        let commentString = comments |> List.truncate 5 |> String.concat("\n")
        let globals = findAllSavedGlobalEventTargets node
        globals |> Set.toList |> List.iter (fun g -> eprintf "%s" g )
        let savetargets = findAllSavedEventTargets node
        let usedtargets = findAllUsedEventTargets node
        ScriptedEffect(node.Key, scopes, effectType, commentString, globals |> Set.toList, savetargets |> Set.toList, usedtargets |> Set.toList)

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
    type Option(key, pos) = inherit Node(key, pos)
    type ModifierBlock(key, pos) = inherit Node(key, pos)

    let scopedProcessNode<'T when 'T :> Node> (lookup : LookupContext) =
        match lookup.scope with
        |"planet" -> processNode<'T> (fun n -> n.Scope <- Scope.Planet; n)
        |"country" -> processNode<'T> (fun n -> n.Scope <- Scope.Country; n)
        |"fleet" -> processNode<'T> (fun n -> n.Scope <- Scope.Fleet; n)
        |"ship" -> processNode<'T> (fun n -> n.Scope <- Scope.Ship; n)
        |"pop_faction" -> processNode<'T> (fun n -> n.Scope <- Scope.PopFaction; n)
        |"pop" -> processNode<'T> (fun n -> n.Scope <- Scope.Pop; n)
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
        |("desc", _, {parents = "event"::_}) ->  scopedProcessNode<Node>, "eventdesc", id;
        |("trigger", _, {parents = "eventdesc"::"event"::_}) ->  scopedProcessNode<TriggerBlock>, "triggerblock", id;
        |("after", _, {parents = "event"::_}) ->  scopedProcessNode<EffectBlock>, "effectblock", id;
        |("limit", _, {parents = "effectblock"::_}) ->  triggerInEffectProcessNode, "triggerblock", id;
        |("limit", _, {parents = "option"::_}) ->  processNodeSimple<TriggerBlock>, "triggerblock", id;
        //Buildings
        |(_, p, {complete = false; entityType = EntityType.Buildings}) ->  processNodeSimple<Node>, "building",  (fun c -> { c with complete = true});
        |("potential", _, {parents = "building"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Tile, "triggerblock", id;
        |("allow", _, {parents = "building"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Tile, "triggerblock", id;
        |("ai_allow", _, {parents = "building"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Tile, "triggerblock", id;
        |("destroy_if", _, {parents = "building"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Tile, "triggerblock", id;
        |("active", _, {parents = "building"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Tile, "triggerblock", id;
        
        |("planet_modifier_with_pop_trigger", _, {parents = "building"::_}) ->  processNodeSimple<Node>, "planetmodpop", id;
        |("potential", _, {parents = "planetmodpop"::"building"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Pop, "triggerblock", id;
        |("modifier", _, {parents = "planetmodpop"::"building"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Planet, "triggerblock", id;
        
        |("triggered_planet_modifier", _, {parents = "building"::_}) ->  processNodeSimple<Node>, "triggeredplanetmod", id;
        |("potential", _, {parents = "triggeredplanetmod"::"building"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Planet, "triggerblock", id;
        |("modifier", _, {parents = "triggeredplanetmod"::"building"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Planet, "triggerblock", id;

        |("planet_modifier", _, {parents = "building"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Planet, "modifierblock", id;
        |("army_modifier", _, {parents = "building"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Army, "modifierblock", id;
        |("country_modifier", _, {parents = "building"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        //Agendas
        |(_, p, {complete = false; entityType = EntityType.Agenda}) ->  processNodeSimple<Node>, "agenda",  (fun c -> { c with complete = true});
        |("weight_modifier", _, {parents = "agenda"::_}) ->  processNodeSimple<Node>, "weightmodifier", id;
        |("modifier", _, {parents = "agenda"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        //Armies
        |(_, _, {complete = false; entityType = EntityType.Armies})  ->  processNodeSimple<Node>, "army",  (fun c -> { c with complete = true});
        |("potential", _, {parents = "army"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Planet, "triggerblock", id;
        |("allow", _, {parents = "army"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Planet, "triggerblock", id;
        |("show_tech_unlock_if", _, {parents = "army"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("on_queued", _, {parents = "army"::_}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("on_unqueued", _, {parents = "army"::_}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        //Anomalies
        |("anomaly", _, {complete = false; entityType = EntityType.Anomalies}) ->  processNodeSimple<Node>, "anomaly",  (fun c -> { c with complete = true});
        |("potential", _, {parents = "anomaly"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Ship, "triggerblock", id;
        |("anomaly_category", _, {complete = false; entityType = EntityType.Anomalies}) -> processNodeSimple<Node>, "anomalycat",  (fun c -> { c with complete = true});
        |("potential", _, {parents = "anomalycat"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Planet, "triggerblock", id;
        |("on_spawn", _, {parents = "anomalycat"::_}) ->  specificScopeProcessNode<EffectBlock> Scope.Planet, "effectblock", id;
        |("on_success", _, {parents = "anomalycat"::_}) ->  specificScopeProcessNode<EffectBlock> Scope.Ship, "effectblock", id;
        |("on_fail", _, {parents = "anomalycat"::_}) ->  specificScopeProcessNode<EffectBlock> Scope.Ship, "effectblock", id;
        |("on_critical_fail", _, {parents = "anomalycat"::_}) ->  specificScopeProcessNode<EffectBlock> Scope.Ship, "effectblock", id;
        //Ascension perks
        |(_, _, {complete = false; entityType = EntityType.AscensionPerks})  ->  processNodeSimple<Node>, "ascension",  (fun c -> { c with complete = true});
        |("potential", _, {parents = "ascension"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("possible", _, {parents = "ascension"::_}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("on_enabled", _, {parents = "ascension"::_}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("ai_weight", _, {parents = "ascension"::_}) ->  processNodeSimple<Node>, "ai_weight", id;
        |("modifier", _, {parents = "ascension"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        //Bombardment stances
        |(_, _, {complete = false; entityType = EntityType.BombardmentStances})  ->  processNodeSimple<Node>, "bombard",  (fun c -> { c with complete = true});
        |("trigger", _, {parents = "bombard"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Fleet, "triggerblock", id;
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
        |("ai_weight", _, {parents = "componenttemplate"::_}) ->  processNodeSimple<Node>, "ai_weight", id;
        |("friendly_aura", _, {parents = "componenttemplate"::_;}) ->  processNodeSimple<Node>, "componentaura", id;
        |("hostile_aura", _, {parents = "componenttemplate"::_;}) ->  processNodeSimple<Node>, "componentaura", id;
        |("modifier", _, {parents = "componentaura"::"componenttemplate"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Ship, "modifierblock", id;
        |("ship_modifier", _, {parents = "componenttemplate"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Ship, "modifierblock", id;
        |("modifier", _, {parents = "componenttemplate"::_}) ->  specificScopeProcessNode<ModifierBlock> Scope.Ship, "modifierblock", id;
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
        //Ethics
        |(_, _, {complete = false; entityType = EntityType.Ethics})  ->  processNodeSimple<Node>, "ethic",  (fun c -> { c with complete = true;});
        |("playable", _, {parents = "ethic"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("pop_attraction_tag", _, {parents = "ethic"::_;}) ->  processNodeSimple<Node>, "popattractiontag", id;
        |("trigger", _, {parents = "popattractiontag"::"ethic"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("country_modifier", _, {parents = "ethic"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
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
        //Observation station missions
        |(_, _, {complete = false; entityType = EntityType.ObservationStationMissions})  ->  processNodeSimple<Node>, "obsstation",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "obsstation"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("valid", _, {parents = "obsstation"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        //Personalities
        |(_, _, {complete = false; entityType = EntityType.Personalities})  ->  processNodeSimple<Node>, "personality",  (fun c -> { c with complete = true;});
        |("allow", _, {parents = "personality"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        //Policies
        |(_, _, {complete = false; entityType = EntityType.Policies})  ->  processNodeSimple<Node>, "policy",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "policy"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("allow", _, {parents = "policy"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("option", _, {parents = "policy"::_;}) ->  processNodeSimple<Node>, "policyoption", id;
        |("valid", _, {parents = "policyoption"::"policy"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "policyoption", id;
        |("on_enabled", _, {parents = "policyoption"::"policy"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("on_disabled", _, {parents = "policyoption"::"policy"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("ai_weight", _, {parents = "policyoption"::"policy"::_;}) ->  processNodeSimple<Node>, "aiweight", id;
        |("modifier", _, {parents = "policyoption"::"policy"::_; previous = "option"}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
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
        |("ai_will_do"), _, {parents = "speciesrights"::_;} -> processNodeSimple<Node>, "dk", id;
        |("modifier", _, {parents = "speciesrights"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Any, "modifierblock", id;
        //Starbase buildings
        |(_, _, {complete = false; entityType = EntityType.StarbaseBuilding})  ->  processNodeSimple<Node>, "starbasebuilding",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "starbasebuilding"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Starbase, "triggerblock", id;
        |("possible", _, {parents = "starbasebuilding"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Starbase, "triggerblock", id;
        |("orbit_modifier", _, {parents = "starbasebuilding"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Ship, "modifierblock", id;
        |("country_modifier", _, {parents = "starbasebuilding"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        |("station_modifier", _, {parents = "starbasebuilding"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Starbase, "modifierblock", id;
        //Starbase modules
        |(_, _, {complete = false; entityType = EntityType.StarbaseModules})  ->  processNodeSimple<Node>, "starbasemodule",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "starbasemodule"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Starbase, "triggerblock", id;
        |("possible", _, {parents = "starbasemodule"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Starbase, "triggerblock", id;
        |("country_modifier", _, {parents = "starbasemodule"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        |("station_modifier", _, {parents = "starbasemodule"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Starbase, "modifierblock", id;
        |("triggered_country_modifier", _, {parents = "starbasemodule"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        //Starbase type
        |(_, _, {complete = false; entityType = EntityType.StarbaseTypes})  ->  processNodeSimple<Node>, "starbasetype",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "starbasetype"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Starbase, "triggerblock", id;
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
        //Terraform
        |(_, _, {complete = false; entityType = EntityType.Terraform})  ->  processNodeSimple<Node>, "terraform",  (fun c -> { c with complete = true;});
        |("condition", _, {parents = "terraform"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        //Tradition category
        |(_, _, {complete = false; entityType = EntityType.TraditionCategories})  ->  processNodeSimple<Node>, "tradcat",  (fun c -> { c with complete = true;});
        |("tradition_swap", _, {parents = "tradcat"::_;}) ->  processNodeSimple<Node>, "tradcatswap", id;
        |("modifier", _, {parents = "tradcat"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        |("trigger", _, {parents = "tradcatswap"::"tradcat"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("modifier", _, {parents = "tradcatswap"::"tradcat"::_; previous= "tradition_swap"}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        //Tradition
        |(_, _, {complete = false; entityType = EntityType.Traditions})  ->  processNodeSimple<Node>, "trad",  (fun c -> { c with complete = true;});
        |("tradition_swap", _, {parents = "trad"::_;}) ->  processNodeSimple<Node>, "tradswap", id;
        |("ai_weight", _, {parents = "trad"::_;}) ->  processNodeSimple<Node>, "aiweight", id;
        |("modifier", _, {parents = "trad"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        |("trigger", _, {parents = "tradswap"::"trad"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("modifier", _, {parents = "tradswap"::"trad"::_; previous = "tradition_swap"}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        //Traits
        |(_, _, {complete = false; entityType = EntityType.Traits})  ->  processNodeSimple<Node>, "trait",  (fun c -> { c with complete = true;});
        |("leader_potential_add", _, {parents = "trait"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Leader, "triggerblock", id;
        |("species_potential_add", _, {parents = "trait"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Species, "triggerblock", id;
        |("species_potential_remove", _, {parents = "trait"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Species, "triggerblock", id;
        |("modifier", _, {parents = "trait"::_;}) ->  specificScopeProcessNode<ModifierBlock> Scope.Country, "modifierblock", id;
        //War goals
        |(_, _, {complete = false; entityType = EntityType.WarGoals})  ->  processNodeSimple<Node>, "wargoal",  (fun c -> { c with complete = true;});
        |("potential", _, {parents = "wargoal"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("possible", _, {parents = "wargoal"::_;}) ->  specificScopeProcessNode<TriggerBlock> Scope.Country, "triggerblock", id;
        |("on_status_quo", _, {parents = "wargoal"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("on_accept", _, {parents = "wargoal"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |("on_wargoal_set", _, {parents = "wargoal"::_;}) ->  specificScopeProcessNode<EffectBlock> Scope.Country, "effectblock", id;
        |_ -> processNodeSimple<Node>, "", (fun c -> {c with complete = true;});
            
    let shipProcess = BaseProcess(stellarisFunction)

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

    
