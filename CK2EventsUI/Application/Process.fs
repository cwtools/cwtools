namespace CK2_Events.Application

open CK2_Events.Application.CKParser
open System.Net.NetworkInformation

module Process =
    type Node () =
        member val AllTags : KeyValueItem list = List.empty with get, set
        member val Children : Node list = List.empty with get, set
        member val Comments : string list = List.empty with get, set
        member this.Tag x = this.AllTags |> List.pick (function |KeyValueItem(ID(y), v) when x=y -> Some v |_ -> None)
    type Option() = 
        inherit Node()
        member val Name = "" with get, set      
    type Event() =
        inherit Node()
        member val ID = "" with get, set
        member val Desc = "" with get, set

    type Root() =
        inherit Node()
        member val Namespace = "" with get, set
        member val Events : Event list = List.empty with get, set

    let addTag (event : Event) tag =
        event.AllTags <- tag::event.AllTags
        match tag with
            | KeyValueItem(ID("desc"), String(v)) -> event.Desc <- v 
            | KeyValueItem(ID("id"), String(v)) -> event.ID <- v
            | _ -> ()
    
    let addTagNode (node : Node) tag =
        node.AllTags <- tag::node.AllTags
    
    let rec processNode sl =
        let node = Node()
        sl |> List.iter (processNodeInner node)
        let tags = sl |> List.choose (function |KeyValue kv -> Some kv |_ -> None)
        let comments = sl |> List.choose (function |Comment c -> Some c |_ -> None)
        List.iter (fun t -> addTagNode node t) tags
        node.Comments <- comments 
        node

    and processNodeInner (node : Node) statement =
        match statement with
            | KeyValue(KeyValueItem(_ , Block(sl))) -> node.Children <- (processNode sl)::node.Children
            //| KeyValue(KeyValueItem(ID("namespace"), String(v))) -> root.Namespace <- v
            | KeyValue(kv) -> node.AllTags <- kv::node.AllTags
            | Comment(c) -> node.Comments <- c::node.Comments
            | _ -> ()

    let processEventInner (event : Event) statement =
        match statement with
            | KeyValue(KeyValueItem(_ , Block(sl))) -> event.Children <- (processNode sl)::event.Children
            //| KeyValue(KeyValueItem(ID("namespace"), String(v))) -> root.Namespace <- v
            | KeyValue(kv) -> event.AllTags <- kv::event.AllTags
            | Comment(c) -> event.Comments <- c::event.Comments
            | _ -> ()
    
    let processEvent sl =
        let event = Event()
        sl |> List.iter (processEventInner event)
        let tags = sl |> List.choose (function |KeyValue kv -> Some kv |_ -> None)
        let comments = sl |> List.choose (function |Comment c -> Some c |_ -> None)
        //let event = Event()
        List.iter (fun t -> addTag event t) tags
        event.Comments <- comments 
        event

 


    let processRoot (root : Root) statement =
        //root.AllTags <- statement::root.AllTags
        match statement with
            | KeyValue(KeyValueItem(_ , Block(sl))) -> root.Events <- (processEvent sl)::root.Events
            | KeyValue(KeyValueItem(ID("namespace"), String(v))) -> root.Namespace <- v
            | KeyValue(kv) -> root.AllTags <- kv::root.AllTags
            | Comment(c) -> root.Comments <- c::root.Comments
            | _ -> ()


    let processEventFile (ev : Statement list) =
        let root = Root()
        ev |> List.iter (processRoot root)
        root
        // ev
        // |> List.filter (function |Comment _ -> false |_ -> true)
        // |> List.map processRoot

    let rec foldbackNode fNode generator (node : Node) :'r =
        let recurse = foldbackNode fNode  
        // let newGenerator innerVal =
        //     let newInnerVal = fNode innerVal node
        //     generator newInnerVal
        let newGenerator = fNode generator node
        node.Children |> List.fold recurse newGenerator
        // let newGenerator innerVal =
        //     let newInnerVal = fNode innerVal node 
        //     generator newInnerVal
        // recurse newGenerator (fNode node.Children node)

    let testNode node =
        let fNode innerCount (node:Node) = node.Children.Length::innerCount
        let initialAcc = []
        foldbackNode fNode initialAcc node  