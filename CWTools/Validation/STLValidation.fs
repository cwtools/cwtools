namespace CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser
open CWTools.Process.STLScopes
open System.ComponentModel.Design.Serialization
open CWTools.Localisation

module STLValidation =
    let shipName (ship : Ship) = if ship.Name = "" then Invalid [(ship, "must have name")] else OK
    let shipSize (ship : Ship) = if ship.ShipSize = "" then Invalid [(ship, "must have size")] else OK

    let validateShip : Validator<Ship>  = shipName <&> shipSize


    let getDefinedVariables (node : Node) =
        node.Values |> List.filter (fun kv -> kv.Key.StartsWith("@"))
                    |> List.map (fun kv -> kv.Key)

    let getUsedVariables (node : Node) =
        let fNode = (fun (x:Node) children ->
                        let values = x.Values |> List.choose (fun v -> match v.Value with |String s when s.StartsWith("@") -> Some s |_ -> None)
                        match values with
                        | [] -> children
                        | x -> x@children)
        let fCombine = (@)
        ([node] |> List.collect (foldNode2 fNode fCombine []))
    
    

    let validateVariables : Validator<Node> =
        (fun node -> 
            let defined = getDefinedVariables node |> Set.ofList
            let used = getUsedVariables node |> Set.ofList
            let undefined = Set.difference used defined
            let errors = 
                match undefined |> Set.toList with
                | [] -> OK
                | x -> 
                    let errors = List.map (fun v -> (node, (v + " is not defined"))) x
                    Invalid errors
            let unused = Set.difference defined used
            let warnings =
                match unused |> Set.toList with
                | [] -> OK
                | x -> 
                    let errors = List.map (fun v -> (node, (v + " is not used"))) x
                    Invalid errors

            errors //<&&> warnings
        )

    let eventScope (event : Event) =
        match event.Key with
        |"country_event" -> Scope.Country
        |"fleet_event" -> Scope.Fleet
        |"ship_event" -> Scope.Ship
        |"pop_faction_event" -> Scope.PopFaction
        |"pop_event" -> Scope.Pop
        |"planet_event" -> Scope.Planet
        |_ -> Scope.Army

    let handleUnknownTrigger root (key : string) =
        match STLProcess.ignoreKeys |> List.tryFind (fun k -> k.ToLower() = key.ToLower()) with
        |Some s -> OK //Do better
        |None -> Invalid [root, (sprintf "unknown trigger %s used." key)]
    
    let handleUnknownEffect root (key : string) =
        match STLProcess.ignoreKeys |> List.tryFind (fun k -> k.ToLower() = key.ToLower()) with
        |Some s -> OK //Do better
        |None -> Invalid [root, (sprintf "unknown effect %s used." key)]
    

    let rec valEventTrigger (root : Node) (triggers : (Effect * bool) list) (effects : (Effect * bool) list) (scope : Scope) (effect : Both) =
        match effect with
        |LeafI leaf ->
            match List.tryFind (fun (e, b) -> e.name = leaf.Key ) triggers with
            |Some (_, true) -> OK
            |Some (t, false) -> Invalid [root, sprintf "%s trigger used in incorrect scope. In %A but expected %s" leaf.Key scope (t.scopes |> String.concat ", ")]
            |None -> handleUnknownTrigger root leaf.Key
        |NodeI node ->
            match node.Key with
            |x when STLProcess.toTriggerKeys @ STLProcess.toTriggerBlockKeys |> List.contains x ->
                valNodeTriggers root triggers effects scope node
            |x when STLProcess.isTargetKey x ->
                OK //Handle later
            |x when x.StartsWith("event_target:") ->
                OK //Handle later
            |x ->
                match changeScope x scope with
                |NewScope s -> valNodeTriggers root triggers effects s node
                |WrongScope ss -> Invalid [node, sprintf "%s scope command used in incorrect scope. In %A but expected %s" x scope (ss |> List.map (fun s -> s.ToString()) |> String.concat ", ")]
                |NotFound ->
                    match List.tryFind (fun (e, b) -> e.name.ToLower() = x.ToLower() ) triggers with
                    |Some (_, true) -> OK
                    |Some (t, false) -> Invalid [node, sprintf "%s trigger used in incorrect scope. In %A but expected %s" x scope (t.scopes |> String.concat ", ")]
                    |None -> handleUnknownTrigger node x
        |_ -> OK

    and valEventEffect (root : Node) (triggers : (Effect * bool) list) (effects : (Effect * bool) list) (scope : Scope) (effect : Both) =
        match effect with
        |LeafI leaf ->
            match List.tryFind (fun (e, b) -> e.name = leaf.Key) effects with
            |Some(_, true) -> OK
            |Some (t, false) -> Invalid [root, sprintf "%s effect used in incorrect scope. In %A but expected %s" leaf.Key scope (t.scopes |> String.concat ", ")]
            |None -> handleUnknownEffect root leaf.Key
        |NodeI node ->
            match node.Key with
            |x when STLProcess.toTriggerKeys @ STLProcess.toTriggerBlockKeys |> List.contains x ->
                valNodeTriggers root triggers effects scope node
            |x when STLProcess.isTargetKey x ->
                OK //Handle later
            |x when x.StartsWith("event_target:") ->
                OK //Handle later
            |x ->
                match changeScope x scope with
                |NewScope s -> valNodeEffects root triggers effects s node
                |WrongScope ss -> Invalid [node, sprintf "%s scope command used in incorrect scope. In %A but expected %s" x scope (ss |> List.map (fun s -> s.ToString()) |> String.concat ", ")]
                |NotFound ->
                    match List.tryFind (fun (e, b) -> e.name.ToLower() = x.ToLower()) effects with
                    |Some(_, true) -> OK
                    |Some (t, false) -> Invalid [node, sprintf "%s effect used in incorrect scope. In %A but expected %s" x scope (t.scopes |> String.concat ", ")]
                    |None -> handleUnknownEffect node x
        |_ -> OK
    
    and valNodeTriggers (root : Node) (triggers : (Effect * bool) list) (effects : (Effect * bool) list) (scope : Scope) (node : Node) =
        let scopedTriggers = List.map (fun (e, b) -> e, e.scopes |> List.exists (fun s -> parseScope s = scope || s = "all")) triggers
        let scopedEffects = List.map (fun (e, b) -> e, e.scopes |> List.exists (fun s -> parseScope s = scope || s = "all")) effects
        List.map (valEventTrigger root scopedTriggers scopedEffects scope) node.All |> List.fold (<&&>) OK

    and valNodeEffects (root : Node) (triggers : (Effect * bool) list) (effects : (Effect * bool) list) (scope : Scope) (node : Node) =
        let scopedTriggers = List.map (fun (e, b) -> e, e.scopes |> List.exists (fun s -> parseScope s = scope || s = "all")) triggers
        let scopedEffects = List.map (fun (e, b) -> e, e.scopes |> List.exists (fun s -> parseScope s = scope || s = "all")) effects
        List.map (valEventEffect root scopedTriggers scopedEffects scope) node.All |> List.fold (<&&>) OK
    
    let valEventTriggers  (triggers : (Effect) list) (effects : (Effect) list) (event : Event) =
        let eventScope = eventScope event
        let scopedTriggers = List.map (fun e -> e, e.scopes |> List.exists (fun s -> parseScope s = eventScope || s = "all")) triggers
        let scopedEffects = List.map (fun e -> e, e.scopes |> List.exists (fun s -> parseScope s = eventScope || s = "all")) effects
        match event.Child "trigger" with
        |Some n -> 
            let v = List.map (valEventTrigger event scopedTriggers scopedEffects eventScope) n.All
            v |> List.fold (<&&>) OK
        |None -> OK
    let valEventEffects (triggers : (Effect) list) (effects : (Effect) list) (event : Event) =
        let eventScope = eventScope event
        let scopedTriggers = List.map (fun e -> e, e.scopes |> List.exists (fun s -> parseScope s = eventScope || s = "all")) triggers
        let scopedEffects = List.map (fun e -> e, e.scopes |> List.exists (fun s -> parseScope s = eventScope || s = "all")) effects
        let imm = match event.Child "immediate" with
            |Some n -> 
                let v = List.map (valEventEffect event scopedTriggers scopedEffects eventScope) n.All
                v |> List.fold (<&&>) OK
            |None -> OK
        let aft = match event.Child "after" with
            |Some n -> 
                let v = List.map (valEventEffect event scopedTriggers scopedEffects eventScope) n.All
                v |> List.fold (<&&>) OK
            |None -> OK
        imm <&&> aft

    /// Make sure an event either has a mean_time_to_happen or is stopped from checking all the time
    /// Not mandatory, but performance reasons, suggested by Caligula
    /// Check "mean_time_to_happen", "is_triggered_only", "fire_only_once" and "trigger = { always = no }".
    /// Create issue if none are true
    let valEventVals (event : Event) =
        let isMTTH = event.Has "mean_time_to_happen"
        let isTrig = event.Has "is_triggered_only"
        let isOnce = event.Has "fire_only_once"
        let isAlwaysNo = 
            match event.Child "trigger" with
            | Some t -> 
                match t.Tag "always" with
                | Some (Bool b) when b = false -> true
                | _ -> false
            | None -> false
        match isMTTH || isTrig || isOnce || isAlwaysNo with
        | false -> Invalid [(event :> Node), "This event should be explicitely marked as 'is_triggered_only', 'fire_only_once' or 'mean_time_to_happen'"]
        | true -> OK


    let checkLocKey (event : Event) key (keys : string list) =
        match key = "" || key.Contains(" "), List.contains key keys with
        | true, _ -> OK
        | _, true -> OK
        | _, false -> Invalid [(event :> Node), sprintf "Localisation key %s is not defined" key]
    let valEventLocs (event : Event) (loc : ILocalisationAPI) =
        let title = event.TagText "title"
        let desc = event.TagText "desc"
        let keys = loc.GetKeys
        checkLocKey event title keys <&&> checkLocKey event desc keys
        