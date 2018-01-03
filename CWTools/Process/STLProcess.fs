namespace CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser
open CWTools.Localisation
open CWTools.Process.ProcessCore
open CWTools.Process

module STLProcess =
    type Scope =
        |Planet
        |Country
        |Fleet
        |Pop
        |GalacticObject
        |Leader
        |Species
        |PopFaction
        |Sector
        |Ship
        |Army
    let allScopes = ["planet";"country";
        "fleet";
        "pop";
        "galactic_object";
        "leader";
        "species";
        "pop_faction";
        "sector";
        "ship";
        "army";]

    let rec scriptedTriggerScope (triggers : Effect list) (node : Node) =
        let anyBlockKeys = ["OR"; "AND"; "NOR"; "NAND"; "NOT"]
        let valueTriggers = node.Values |> List.choose (fun v -> if List.contains v.Key anyBlockKeys then None else Some v.Key)
        let nodeRecTriggers = node.Children |> List.choose (fun v -> if List.contains v.Key anyBlockKeys then Some v else None)
        let nodeTriggers = node.Children |> List.choose (fun v -> if List.contains v.Key anyBlockKeys then None else Some v.Key)
        let valueScopes = 
            valueTriggers @ nodeTriggers 
            |> List.map (fun v -> 
                triggers 
                |> List.tryPick (fun t -> if t.name = v then Some t.scopes else None))
            |> List.choose (function |Some s -> Some s |None -> None)
        
        let nodeScopes =
            nodeRecTriggers
            |> List.map (scriptedTriggerScope triggers)
        if List.length (nodeScopes @ valueScopes) = 0 then allScopes else
            nodeScopes @ valueScopes 
                |> List.fold (fun a b -> Set.intersect (Set.ofList a) (Set.ofList b) |> Set.toList) allScopes

    let getScriptedTriggerScope (triggers : Effect list) (node : Node) =
        let scopes = scriptedTriggerScope triggers node
        {name = node.Key; desc = ""; usage = ""; scopes = scopes ; targets = []}

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

    type Event(key, pos) =
        inherit Node(key, pos)
        member this.ID = this.TagText "id"
        member this.Desc = this.TagText "desc"
        member this.Hidden = this.Tag "hide_window" |> (function | Some (Bool b) -> b | _ -> false)
   
    let shipMap =
        [
            (=) "ship_design", processNode<Ship>;
            (=) "section", processNode<ShipSection>;
            (=) "component", processNode<ShipComponent>;
            (=) "planet_event", processNode<Event>;
            (=) "country_event", processNode<Event>;
            (=) "fleet_event", processNode<Event>;
            (=) "ship_event", processNode<Event>;
            (=) "pop_faction_event", processNode<Event>;
            (=) "pop_event", processNode<Event>;
       ]
    let shipProcess = BaseProcess(shipMap)

    

    
