namespace CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser

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

            errors <&&> warnings
        )