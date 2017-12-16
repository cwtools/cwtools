namespace CK2Events.Application

open CK2Events.Application.CKParser
open Localisation
open System.Collections.Generic
open FParsec.CharParsers
open Newtonsoft.Json

module Process =
    open ParserDomain
    type Leaf(keyvalueitem : KeyValueItem) =
        let (KeyValueItem (Key(key), value)) = keyvalueitem
        member val Key = key with get, set
        member val Value = value with get, set
        member this.ToRaw = KeyValueItem(Key(this.Key), this.Value)
    type Node (key : string) =
        member val Key = key
        member val All : Both list = List.empty with get, set
        member this.Children = this.All |> List.choose (function |NodeI n -> Some n |_ -> None)
        member this.Values = this.All |> List.choose (function |LeafI l -> Some l |_ -> None)
        member this.Comments = this.All |> List.choose (function |CommentI c -> Some c |_ -> None)
        member this.Tag x = this.Values |> List.tryFind (fun l -> l.Key = x)
        [<JsonIgnore>]
        member this.ToRaw : Statement list = this.All |> List.rev |>
                                             List.map (function 
                                               |NodeI n -> KeyValue(KeyValueItem(Key n.Key, Clause n.ToRaw))
                                               |LeafI l -> KeyValue l.ToRaw 
                                               |CommentI c -> (Comment c))
        
    and Both = |NodeI of Node | LeafI of Leaf |CommentI of string

    type Option() = 
        inherit Node("option")
        member val Name = "" with get, set      
    type Event(key) =
        inherit Node(key)
        member this.ID = this.Tag "id" |> (function |Some s -> s.Value.ToString() |None -> "")
        member val Desc = "" with get, set
        member val Hidden = false with get, set

    type Root() =
        inherit Node("root")
        member this.Namespace = this.Tag "namespace" |> (function |Some s -> s.Value.ToString() |None -> "")
        member val test = base.All |> List.choose (function |NodeI n -> Some n |_ -> None)
        member __.Events : Event list = base.All |> List.choose (function |NodeI n -> Some n |_ -> None) |> List.choose (function | :? Event as e -> Some e |_ -> None)

    let addTag (event : Event) tag =
        //event.All <- LeafI (new Leaf(tag))::event.All
        match tag with
            | KeyValueItem(Key("desc"), String(v)) -> event.Desc <- v 
            //| KeyValueItem(Key("id"), String(v)) -> event.ID <- v
            | KeyValueItem(Key("hide_window"), Bool(v)) -> event.Hidden <- v
            | _ -> ()
    
    let addTagNode (node : Node) tag = ()
        //node.All <- LeafI (new Leaf(tag))::node.All

    let addTagOption (option : Option) tag =
        //option.All <- LeafI (new Leaf(tag))::option.All
        match tag with
            | KeyValueItem(Key("name"), String(v)) -> option.Name <- v
            | _ -> ()
    
    let rec processNode k sl =
        let node = Node(k)
        sl |> List.iter (processNodeInner node)
        let tags = sl |> List.choose (function |KeyValue kv -> Some kv |_ -> None)
        //let comments = sl |> List.choose (function |Comment c -> Some c |_ -> None)
        List.iter (fun t -> addTagNode node t) tags
        //node.Comments <- comments 
        node
    
    and processOption sl =
        let option = Option()
        sl |> List.iter (processNodeInner option)
        let tags = sl |> List.choose (function |KeyValue kv -> Some kv |_ -> None)
        //let comments = sl |> List.choose (function |Comment c -> Some c |_ -> None)
        List.iter (fun t -> addTagOption option t) tags
        //option.Comments <- comments
        option


    and processNodeInner (node : Node) statement =
        match statement with
            | KeyValue(KeyValueItem(Key("option"), Clause(sl))) -> node.All <- NodeI(processOption sl)::node.All
            | KeyValue(KeyValueItem(Key(k) , Clause(sl))) -> node.All <- NodeI(processNode k sl)::node.All
            //| KeyValue(KeyValueItem(ID("namespace"), String(v))) -> root.Namespace <- v
            | KeyValue(kv) -> node.All <- LeafI(new Leaf(kv))::node.All
            | Comment(c) -> node.All <- CommentI c::node.All
            | _ -> ()

    let processEventInner (event : Event) statement =
        match statement with
            | KeyValue(KeyValueItem(Key("option"), Clause(sl))) -> event.All <- NodeI(processOption sl)::event.All
            | KeyValue(KeyValueItem(Key(k) , Clause(sl))) -> event.All <- NodeI(processNode k sl)::event.All
            //| KeyValue(KeyValueItem(ID("namespace"), String(v))) -> root.Namespace <- v
            | KeyValue(kv) -> event.All <- LeafI(new Leaf(kv))::event.All
            | Comment(c) -> event.All <- CommentI c::event.All
            | _ -> ()
    
    let processEvent k sl =
        let event = Event(k)
        sl |> List.iter (processEventInner event)
        let tags = sl |> List.choose (function |KeyValue kv -> Some kv |_ -> None)
        //let comments = sl |> List.choose (function |Comment c -> Some c |_ -> None)
        //event.Raw <- sl
        //let event = Event()
        List.iter (fun t -> addTag event t) tags
        //event.Comments <- comments 
        event

 


    let processRoot savedComments (root : Root) statement =
        //root.AllTags <- statement::root.AllTags
        let mutable comments = savedComments
        match statement with
            | KeyValue(KeyValueItem(Key(k) , Clause(sl))) -> 
                let e = (processEvent k sl)
                //e.All <- comments@e.All
                //comments <- []
                root.All <- NodeI e::root.All
            //| KeyValue(KeyValueItem(Key("namespace"), String(v))) -> root.Namespace <- v
            | KeyValue(kv) -> root.All <- LeafI (Leaf kv)::root.All
            | Comment(c) -> 
                root.All <- CommentI c::root.All
                //comments <- CommentI c::comments
            | _ -> ()
        comments


    let processEventFile (ev : EventFile) =
        let root = Root()
        let (EventFile evs) = ev
        evs |> List.fold (fun s e -> processRoot s root e) [] |> ignore
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
        loop [node] Operators.id
    
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
        loop [node] Operators.id

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

    let addLocalisedDesc (localisation : LocalisationService) (node:Node) =
        let getDesc x = 
            match localisation.Values.ContainsKey(x) with
            | true -> localisation.Values.[x]
            | false -> x
        let fNode = (fun (x:Node) _ -> 
                        match x with
                        | :? Event as e -> e.Desc <- getDesc e.Desc
                        | :? Option as o -> o.Name <- getDesc o.Name
                        | _ -> ()
                        )
        let fCombine = (fun _ _ -> ())
        foldNode2 fNode fCombine () node
    
    let addLocalisedDescAll (root:Root) (localisation : LocalisationService) =
        root.Events |> List.iter (addLocalisedDesc localisation)
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
        let fEvent = (fun _ c -> c)
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