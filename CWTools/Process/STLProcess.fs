namespace CWTools.Process
open CWTools.Process.ProcessCore

module STLProcess =
    type Ship (key) =
        inherit Node(key)
        member this.Name = this.TagText "name"
        member this.ShipSize = this.TagText "ship_size"
    
    type ShipSection (key) =
        inherit Node(key)
        member this.Template = this.TagText "template"
        member this.Slot = this.TagText "slot"
    
    type ShipComponent (key) =
        inherit Node(key)
        member this.Template = this.TagText "template"
        member this.Slot = this.TagText "slot"
   
    let shipMap =
        [
            (=) "ship_design", processNode<Ship>;
            (=) "section", processNode<ShipSection>;
            (=) "component", processNode<ShipComponent>;
        ]
    let shipProcess = BaseProcess(shipMap)

    

    
