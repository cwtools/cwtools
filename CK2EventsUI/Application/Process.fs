namespace CK2_Events.Application

open CK2_Events.Application.CKParser
open Microsoft.CodeAnalysis
open System.Xml.Serialization

module Process =
    type Node (key : string) =
        member val Key = key
        member val AllTags : KeyValueItem list = List.empty with get, set
        member val Children : Node list = List.empty with get, set
        member val Comments : string list = List.empty with get, set
        member this.Tag x = this.AllTags |> List.tryPick (function |KeyValueItem(ID(y), v) when x=y -> Some v |_ -> None)
        member val Raw : Statement list = List.empty with get, set
    type Option() = 
        inherit Node("option")
        member val Name = "" with get, set      
    type Event(key) =
        inherit Node(key)
        member val ID = "" with get, set
        member val Desc = "" with get, set
        member val Hidden = false with get, set

    type Root() =
        inherit Node("root")
        member val Namespace = "" with get, set
        member val Events : Event list = List.empty with get, set

    let addTag (event : Event) tag =
        event.AllTags <- tag::event.AllTags
        match tag with
            | KeyValueItem(ID("desc"), String(v)) -> event.Desc <- v 
            | KeyValueItem(ID("id"), String(v)) -> event.ID <- v
            | KeyValueItem(ID("hide_window"), Bool(v)) -> event.Hidden <- v
            | _ -> ()
    
    let addTagNode (node : Node) tag =
        node.AllTags <- tag::node.AllTags

    let addTagOption (option : Option) tag =
        option.AllTags <- tag::option.AllTags
        match tag with
            | KeyValueItem(ID("name"), String(v)) -> option.Name <- v
            | _ -> ()
    
    let rec processNode k sl =
        let node = Node(k)
        sl |> List.iter (processNodeInner node)
        let tags = sl |> List.choose (function |KeyValue kv -> Some kv |_ -> None)
        let comments = sl |> List.choose (function |Comment c -> Some c |_ -> None)
        List.iter (fun t -> addTagNode node t) tags
        node.Comments <- comments 
        node
    
    and processOption sl =
        let option = Option()
        sl |> List.iter (processNodeInner option)
        let tags = sl |> List.choose (function |KeyValue kv -> Some kv |_ -> None)
        let comments = sl |> List.choose (function |Comment c -> Some c |_ -> None)
        List.iter (fun t -> addTagOption option t) tags
        option.Comments <- comments
        option


    and processNodeInner (node : Node) statement =
        match statement with
            | KeyValue(KeyValueItem(ID("option"), Block(sl))) -> node.Children <- (upcast processOption sl)::node.Children
            | KeyValue(KeyValueItem(ID(k) , Block(sl))) -> node.Children <- (processNode k sl)::node.Children
            //| KeyValue(KeyValueItem(ID("namespace"), String(v))) -> root.Namespace <- v
            | KeyValue(kv) -> node.AllTags <- kv::node.AllTags
            | Comment(c) -> node.Comments <- c::node.Comments
            | _ -> ()

    let processEventInner (event : Event) statement =
        match statement with
            | KeyValue(KeyValueItem(ID("option"), Block(sl))) -> event.Children <- (upcast processOption sl)::event.Children
            | KeyValue(KeyValueItem(ID(k) , Block(sl))) -> event.Children <- (processNode k sl)::event.Children
            //| KeyValue(KeyValueItem(ID("namespace"), String(v))) -> root.Namespace <- v
            | KeyValue(kv) -> event.AllTags <- kv::event.AllTags
            | Comment(c) -> event.Comments <- c::event.Comments
            | _ -> ()
    
    let processEvent k sl =
        let event = Event(k)
        sl |> List.iter (processEventInner event)
        let tags = sl |> List.choose (function |KeyValue kv -> Some kv |_ -> None)
        let comments = sl |> List.choose (function |Comment c -> Some c |_ -> None)
        event.Raw <- sl
        //let event = Event()
        List.iter (fun t -> addTag event t) tags
        event.Comments <- comments 
        event

 


    let processRoot savedComments (root : Root) statement =
        //root.AllTags <- statement::root.AllTags
        let mutable comments = savedComments
        match statement with
            | KeyValue(KeyValueItem(ID(k) , Block(sl))) -> 
                let e = (processEvent k sl)
                e.Comments <- savedComments@e.Comments
                comments <- []
                root.Events <- e::root.Events
            | KeyValue(KeyValueItem(ID("namespace"), String(v))) -> root.Namespace <- v
            | KeyValue(kv) -> root.AllTags <- kv::root.AllTags
            | Comment(c) -> 
                root.Comments <- c::root.Comments
                comments <- c::comments
            | _ -> ()
        comments


    let processEventFile (ev : Statement list) =
        let root = Root()
        ev |> List.fold (fun s e -> processRoot s root e) [] |> ignore
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
    
    let foldNode3 fNode fOption fEvent fCombine acc (node:Node) =
        let rec loop nodes cont =
            let inner (x : #Node) tail fInner = loop x.Children (fun accChildren ->
                let resNode = fInner x accChildren
                loop tail (fun accTail ->
                    cont(fCombine resNode accTail)))
            match nodes with
            | (:? Option as x)::tail -> inner x tail fOption
            | (:? Event as x)::tail -> inner x tail fEvent
            | (x : Node)::tail -> inner x tail fNode
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

    let getIDs (node:Node) =
        let fNode = (fun (x:Node) children ->
                        match x.Tag "id" with
                        | Some v -> v.ToString()::children
                        | None -> children)
        let fCombine = (@)
        foldNode2 fNode fCombine [] node

    let addLocalisedDesc (node:Node) =
        let fNode = (fun (x:Node) children -> 
                        match x with
                        | :? Event as e -> e.Desc <- Localization.GetDesc e.Desc
                        | :? Option as o -> o.Name <- Localization.GetDesc o.Name
                        | _ -> ()
                        )
        let fCombine = (fun x c -> ())
        foldNode2 fNode fCombine () node
    
    let addLocalisedDescAll (root:Root) =
        root.Events |> List.iter addLocalisedDesc
        root

    // let getOptions (event:Node) =
    //     let fNode = (fun (x:Node) children ->
    //                     let name = x.Tag "name"
    //                     match name with
    //                     | Some n -> ((n.ToString()), children)
    //                     | _ -> ("", children)
    //     )
    //     let fCombine = (fun x c -> c)
    //     foldNode2 fNode fCombine ("", "") event
    let getOption (option:Option) =
        let fNode = (fun (x:Node) (d,i) ->
                        match x.Tag "id" with
                        |Some v -> (d, (v.ToString()) :: i)
                        |_ -> (d, i)
                        )
        let fOption = (fun (x:Option) (d,i) ->
                        match x.Name with
                        |"" -> (d,i)
                        |v -> (v,i)
                        )
        let fEvent = (fun x c -> c)
        let fCombine    x c =
            match x, c with
            |("",[]), c -> c
            |("",l), (a,b) -> (a, l@b)
            |(d,l), (_,b) -> (d,l@b)
        foldNode3 fNode fOption fEvent fCombine ("",[]) option
    
    let getOptions (event : Event) =
        event.Children |> List.choose (function | :? Option as o -> Some o |_ -> None)
                       |> List.map getOption

    let getEventsOptions (root : Root) =
        root.Events |> List.map (fun e -> (e.ID, getOptions e))

    let getImmediate (event : Event)=
        event.Children |> List.filter (fun c -> c.Key = "immediate")
                       |> List.map getIDs
    
    let getAllImmediates (root : Root) =
        root.Events |> List.map (fun e -> (e.ID, getImmediate e))
                        
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