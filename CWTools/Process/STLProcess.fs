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

    

    
