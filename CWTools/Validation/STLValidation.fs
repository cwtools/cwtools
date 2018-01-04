namespace CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser
open CWTools.Process.STLScopes

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

    let scopeParse =
        function
        |"country" -> Scope.Country
        |"fleet" -> Scope.Fleet
        |"ship" -> Scope.Ship
        |"pop_faction" -> Scope.PopFaction
        |"pop" -> Scope.Pop
        |"planet" -> Scope.Planet
        |_ -> Scope.Army

    let valEventTrigger (node : Node) (triggers : (Effect * bool) list) (scope : string) (leaf : Leaf) =
        match List.tryFind (fun (e, b) -> e.name = leaf.Key ) triggers with
        |Some (_, true) -> OK
        |Some (t, false) -> Invalid [node, sprintf "%s trigger used in incorrect scope. In %s but expected %s" leaf.Key scope (t.scopes |> List.fold (+) "")]
        //Invalid [node, (leaf.Key + " trigger used in incorrect scope.")] //In " + scope + " and expected :"+ (triggers |> List.map (fun t -> t.ToString()) |> (List.fold (+) "")))]
        |None -> Invalid [node, (sprintf "unknown trigger %s used." leaf.Key)]
    let valEventTriggers  (triggers : Effect list) (event : Event) =
        let eventScope = eventScope event
        let scopedTriggers = List.map (fun e -> e, e.scopes |> List.exists (fun s -> scopeParse s = eventScope || s = "all")) triggers
        match event.Child "trigger" with
        |Some n -> 
            let v = List.map (valEventTrigger event scopedTriggers (eventScope.ToString())) n.Values
            v |> List.fold (<&&>) OK
        |None -> OK

    let valEventEffect (root : Node) (effects : (Effect * bool) list) (scope : string) (effect : Both) =
        match effect with
        |LeafI leaf ->
            match List.tryFind (fun (e, b) -> e.name = leaf.Key) effects with
            |Some(_, true) -> OK
            |Some (t, false) -> Invalid [root, sprintf "%s effect used in incorrect scope. In %s but expected %s" leaf.Key scope (t.scopes |> List.fold (+) "")]
            |None -> Invalid [root, (sprintf "unknown effect %s used." leaf.Key)]
        |NodeI node ->
            match List.tryFind (fun (e, b) -> e.name = node.Key) effects with
            |Some(_, true) -> OK
            |Some (t, false) -> Invalid [node, sprintf "%s effect used in incorrect scope. In %s but expected %s" node.Key scope (t.scopes |> List.fold (+) "")]
            |None -> OK//Invalid [node, (sprintf "unknown effect %s used." node.Key)]
        |_ -> OK
    let valEventEffects (effects : Effect list) (event : Event) =
        let eventScope = eventScope event
        let scopedEffects = List.map (fun e -> e, e.scopes |> List.exists (fun s -> scopeParse s = eventScope || s = "all")) effects
        let imm = match event.Child "immediate" with
            |Some n -> 
                let v = List.map (valEventEffect event scopedEffects (eventScope.ToString())) n.All
                v |> List.fold (<&&>) OK
            |None -> OK
        let aft = match event.Child "after" with
            |Some n -> 
                let v = List.map (valEventEffect event scopedEffects (eventScope.ToString())) n.All
                v |> List.fold (<&&>) OK
            |None -> OK
        imm <&&> aft