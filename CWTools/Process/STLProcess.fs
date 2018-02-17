namespace CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser
open CWTools.Localisation
open CWTools.Process.ProcessCore
open CWTools.Process
open CWTools.Process.STLScopes
open CWTools.Common.STLConstants
open DotNet.Globbing

module STLProcess =
    let toTriggerKeys = ["OR"; "AND"; "NOR"; "NAND"; "NOT";]
    let toTriggerBlockKeys = ["limit"; "trigger"; "allow"]
    let _targetKeys = ["THIS"; "ROOT"; "PREV"; "FROM"; "OWNER"; "CONTROLLER"; "CAPITAL"; "SOLAR_SYSTEM"; "LEADER"; "RANDOM"; "FROMFROM"; "FROMFROMFROM"; "FROMFROMFROMFROM"; "PREVPREV"; "PREVPREVPREV"; "PREVPREVPREVPREV";
                        "CAPITAL_SCOPE"]//Added used in STH]
    let targetKeys = _targetKeys |> List.sortByDescending (fun k -> k.Length)
    let toEffectBlockKeys = ["hidden_effect"; "if"; "else"; "tooltip"]
    let ignoreKeys = ["count"; "min_steps"; "max_steps"]


    let rec isTargetKey =
        function
        |"" -> true
        |x ->
            match targetKeys |> List.tryFind (fun f -> x.ToLower().StartsWith(f.ToLower()))  with
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
                            | x when (x.Key.ToLower().StartsWith("event_target:")) ->
                                allScopes
                            | x when targetKeys |> List.exists (fun y -> y.ToLower() = x.Key.ToLower()) ->
                                allScopes
                            | x when anyBlockKeys |> List.exists (fun y -> y.ToLower() = x.Key.ToLower()) ->
                                scriptedTriggerScope strict effects triggers root x
                            | x when triggerBlockKeys |> List.exists (fun y -> y.ToLower() = x.Key.ToLower()) -> 
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

    let getScriptedTriggerScope (firstRun: bool) (effectType : EffectType) (effects : Effect list) (triggers : Effect list) (node : Node) =
        let effects2 = effects |> List.map (fun t -> t.Name, t.Scopes)
        let triggers2 = triggers |> List.map (fun t -> t.Name, t.Scopes)
        let scopes = scriptedTriggerScope firstRun effects2 triggers2 node.Key node
        ScriptedEffect(node.Key, scopes, effectType)

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
   
    let globCheckPosition (pattern : string) =
        let glob = Glob.Parse(pattern)
        (fun (p : Position) ->
            let p2 = Position.UnConv(p)
            glob.IsMatch(p2.StreamName))

    
    let shipMap =
        [
            fst3 >> ((=) "ship_design"), processNode<Ship>, id;
            fst3 >> ((=) "section"), processNode<ShipSection>, id;
            fst3 >> ((=) "component"), processNode<ShipComponent>, id;
            fst3 >> ((=) "planet_event"), processNode<Event>, id;
            fst3 >> ((=) "country_event"), processNode<Event>, id;
            fst3 >> ((=) "fleet_event"), processNode<Event>, id;
            fst3 >> ((=) "ship_event"), processNode<Event>, id;
            fst3 >> ((=) "pop_faction_event"), processNode<Event>, id;
            fst3 >> ((=) "pop_event"), processNode<Event>, id;
            (fun (_, p, c) -> (globCheckPosition("**/common/button_effects/*.txt") p) && not c.complete), processNode<Button_Effect>, (fun c -> { c with complete = true});
       ]
    let shipProcess = BaseProcess(shipMap)

    

    
