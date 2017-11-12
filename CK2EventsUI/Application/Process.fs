namespace CK2_Events.Application

open CK2_Events.Application.CKParser
open System.Net.NetworkInformation
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis

module Process =
    type Node () =
        member val AllTags : KeyValueItem list = List.empty with get, set
        member val Children : Node list = List.empty with get, set
        member val Comments : string list = List.empty with get, set
        member this.Tag x = this.AllTags |> List.tryPick (function |KeyValueItem(ID(y), v) when x=y -> Some v |_ -> None)
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

    let rec foldNode fNode acc (node : Node) :'r =
        let recurse = foldNode fNode
        let newAcc = fNode acc node
        node.Children |> List.fold recurse newAcc

    let foldNode2 fNode fCombine acc (node:Node) =
        let rec loop nodes cont =
            match nodes with
            | (x : Node)::tail ->
                loop x.Children (fun accChildren ->
                    let resNode = fNode x accChildren
                    loop tail (fun accTail ->
                        cont(fCombine resNode accTail) ))
            | [] -> cont acc
        loop [node] (fun x -> x)
    
    let rec cata fNode (node:Node) :'r =
        let recurse = cata fNode
        fNode node (node.Children |> List.map recurse)

    let testNode (node:Node) =
        let fNode = (fun (x:Node) children -> 
                        let cString =
                            match children with
                            | [] -> ""
                            | [x] -> x
                            | x -> ", {" + List.reduce (fun a b -> a + ", " + b) x + "}"
                        match x.Tag "id" with
                        | Some v -> "{" + v.ToString() + cString + "}"
                        | None -> cString)
        let fCombine = (fun child children -> if child = "" then children else children @ [child])
        foldNode2 fNode fCombine [] node

    let getTriggeredEvents (event:Event) =
        let fNode = (fun (x:Node) children ->
                        match x.Tag "id" with
                        | Some v -> v.ToString()::children
                        | None -> children)
        let fCombine = (@)
        (event.ID, event.Children |> List.collect (foldNode2 fNode fCombine []))

    let getTriggeredEventsAll (root:Root) =
        List.map getTriggeredEvents root.Events
    // let testNode (node:Node) =
    //     let fNode (inEvent, ) (node:Node) = 
    //         match node.Tag "character_event" with
    //         | Some v -> v.ToString() + (List.fold (+) "" children)
    //         | None -> (List.fold (+) "" children)
    //         //(node.Children.Length, children)
    //     foldNode fNode node

    
    // let rec foldbackNode fNode (node : Node) generator :'r =
    //     let recurse = foldbackNode fNode  
    //     // let newGenerator innerVal =
    //     //     let newInnerVal = fNode innerVal node
    //     //     generator newInnerVal
    //     let newGenerator innerVal = 
    //         let newInnerVal = fNode innerVal
    //         generator newInnerVal
    //     node.Children |> List.fold (fun c -> recurse c newGenerator)
    //     // fNode generator node
    //     // node.Children |> List.fold recurse newGenerator
    //     // let newGenerator innerVal =
    //     //     let newInnerVal = fNode innerVal node 
    //     //     generator newInnerVal
    //     // recurse newGenerator (fNode node.Children node)

    // let testNode node =
    //     let fNode innerCount (node:Node) = (node.Children.Length, innerCount)
    //     let initialAcc = ()
    //     foldbackNode fNode initialAcc node