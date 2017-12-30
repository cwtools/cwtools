namespace CWTools.Process

open CWTools.Parser
open CWTools.Localisation
open Newtonsoft.Json
open System

module List =
  let replace f sub xs = 
    let rec finish acc = function
      | [] -> acc
      | x::xs -> finish (x::acc) xs
    let rec search acc = function
      | [] -> None
      | x::xs -> 
        if f x then Some(finish ((sub x)::xs) acc)
        else search (x::acc) xs
    search [] xs
  let replaceOrAdd f sub add xs =
    let result = replace f sub xs
    match result with
    | Some ls -> ls
    | None -> add::xs
    
type Leaf(keyvalueitem : KeyValueItem) =
    let (KeyValueItem (Key(key), value)) = keyvalueitem
    member val Key = key with get, set
    member val Value = value with get, set
    [<JsonIgnore>]
    member this.ToRaw = KeyValueItem(Key(this.Key), this.Value)
type LeafValue(value : Value) =
    member val Value = value with get, set
    [<JsonIgnore>]
    member this.ToRaw = Value(this.Value)
type Node (key : string) =
    let bothFind x = function |NodeI n when n.Key = x -> true |LeafI l when l.Key = x -> true |_ -> false

    member val Key = key
    member val All : Both list = List.empty with get, set
    member this.Children = this.All |> List.choose (function |NodeI n -> Some n |_ -> None)
    member this.Values = this.All |> List.choose (function |LeafI l -> Some l |_ -> None)
    member this.Comments = this.All |> List.choose (function |CommentI c -> Some c |_ -> None)
    member this.Has x = this.All |> (List.exists (bothFind x))
    member this.Tag x = this.Values |> List.tryPick (function |l when l.Key = x -> Some l.Value |_ -> None)
    member this.SetTag x v = this.All <- this.All |> List.replaceOrAdd (bothFind x) (fun _ -> v) v
    member this.Child x = this.Children |> List.tryPick (function |c when c.Key = x -> Some c |_ -> None)

    [<JsonIgnore>]
    member this.ToRaw : Statement list = this.All |> List.rev |>
                                         List.map (function 
                                           |NodeI n -> KeyValue(KeyValueItem(Key n.Key, Clause n.ToRaw))
                                           |LeafValueI lv -> lv.ToRaw
                                           |LeafI l -> KeyValue l.ToRaw 
                                           |CommentI c -> (Comment c))
    
and Both = |NodeI of Node | LeafI of Leaf |CommentI of string |LeafValueI of LeafValue

type Option(key) = 
    inherit Node(key)
    member this.Name = this.Tag "name" |> (function | Some (String s) -> s | Some (QString s) -> s | _ -> "")
type Event(key) =
    inherit Node(key)
    member this.ID = this.Tag "id" |> (function | Some (String s) -> s | _ -> "")
    member this.Desc = this.Tag "desc" |> (function | Some (String s) -> s | Some (QString s) -> s | _ -> "")
    member this.Hidden = this.Tag "hide_window" |> (function | Some (Bool b) -> b | _ -> false)

type EventRoot() =
    inherit Node("events")
    member __.Events : Event list = base.All |> List.choose (function |NodeI n -> Some n |_ -> None) |> List.choose (function | :? Event as e -> Some e |_ -> None)
    member this.Namespace = this.Tag "namespace" |> (function |Some (String s) -> s | _ -> "")

type ArtifactFile() =
    inherit Node("artifacts")
    member this.Slots = this.Child "slots" |> (function |Some c -> c.ToRaw | _ -> [])
    member __.Weapons = base.All |> List.choose (function |NodeI n when n.Key <> "slots" -> Some n |_ -> None)

module Process =    

    let maps =
        [
            "option", typeof<Option>;
            "character_event", typeof<Event>;
            "province_event", typeof<Event>;
            "letter_event", typeof<Event>;
        ]

    let rec processNodeInner (node : Node) statement =
        match statement with
            | KeyValue(KeyValueItem(Key("option"), Clause(sl))) -> node.All <- NodeI(processNode<Option> "option" sl)::node.All
            | KeyValue(KeyValueItem(Key(k) , Clause(sl))) -> node.All <- NodeI(processNode<Node> k sl)::node.All
            | KeyValue(kv) -> node.All <- LeafI(Leaf(kv))::node.All
            | Comment(c) -> node.All <- CommentI c::node.All
            | Value(v) -> node.All <- LeafValueI(LeafValue(v))::node.All

    and processNode< 'T when 'T :> Node > (key : string) (sl : Statement list) : Node =
        let node = match key with
                    |"" -> Activator.CreateInstance(typeof<'T>) :?> Node
                    |x -> Activator.CreateInstance(typeof<'T>, x) :?> Node
        sl |> List.iter (fun e -> processNodeInner node e) |> ignore
        node

    let processEventRoot (root : EventRoot) statement =
        match statement with
            | KeyValue(KeyValueItem(Key(k) , Clause(sl))) -> root.All <- NodeI (processNode<Event> k sl)::root.All
            | KeyValue(kv) -> root.All <- LeafI (Leaf kv)::root.All
            | Comment(c) -> root.All <- CommentI c::root.All 
            | _ -> ()

    let processEventFile (ev : EventFile) = 
        let (EventFile e) = ev
        processNode<EventRoot> "" e

    let processArtifact = processNode<ArtifactFile> "" >> (fun n -> n :?> ArtifactFile) 
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
        loop [node] id
    
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
        loop [node] id

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

    
    let getTriggeredEventsAll (root:EventRoot) =
        List.map getTriggeredEvents root.Events

    let getIDs (node:Node) =
        let fNode = (fun (x:Node) children ->
                        match x.Tag "id" with
                        | Some v -> v.ToString()::children
                        | None -> children)
        let fCombine = (@)
        foldNode2 fNode fCombine [] node

    let getAllLocalisationKeys (localisation : ILocalisationAPI) (node:Node) =
        let getDesc x = 
            match localisation.Values.ContainsKey(x) with
            | true -> localisation.Values.[x]
            | false -> x
        let fNode = (fun (x:Node) keys ->
                        match x with
                        | :? Event as e -> e.Desc::keys
                        | :? Option as o -> o.Name::keys
                        | _ -> keys)
        let fCombine = (@)
        let keys = foldNode2 fNode fCombine [] node
        keys |> List.map (fun k -> k, getDesc k)

    let addLocalisedDesc (localisation : ILocalisationAPI) (node:Node) =
        let getDesc x = localisation.GetDesc x
          //  match localisation.values.ContainsKey(x) with
          //  | true -> localisation.values.[x]
         //   | false -> x
        let fNode = (fun (x:Node) _ -> 
                        match x with
                        | :? Event as e -> e.SetTag "desc" (LeafI (Leaf (KeyValueItem(Key("desc"), Value.String (getDesc e.Desc)))))
                        | :? Option as o -> o.SetTag "name" (LeafI (Leaf (KeyValueItem(Key("name"), Value.String (getDesc o.Name)))))
                        | _ -> ()
                        )
        let fCombine = (fun _ _ -> ())
        foldNode2 fNode fCombine () node
    
    let addLocalisedDescAll (root:EventRoot) (localisation : ILocalisationAPI) =
        root.Events |> List.iter (addLocalisedDesc localisation)
        root

    let getOption (localisation : ILocalisationAPI) (option:Option) =
        let fNode = (fun (x:Node) (d,i) ->
                        match x.Tag "id" with
                        |Some v -> (d, (v.ToString()) :: i)
                        |_ -> (d, i)
                        )
        let fOption = (fun (x:Option) (d,i) ->
                        match x.Name with
                        |"" -> (d,i)
                        |v -> (localisation.GetDesc(v),i)
                        )
        let fEvent = (fun _ c -> c)
        let fCombine    x c =
            match x, c with
            |("",[]), c -> c
            |("",l), (a,b) -> (a, l@b)
            |(d,l), (_,b) -> (d,l@b)
        foldNode3 fNode fOption fEvent fCombine ("",[]) option
    
    let getOptions (localisation : ILocalisationAPI) (event : Event) =
        event.Children |> List.choose (function | :? Option as o -> Some o |_ -> None)
                       |> List.map (getOption localisation)

    let getEventsOptions (localisation : ILocalisationAPI) (root : EventRoot) =
        root.Events |> List.map (fun e -> (e.ID, getOptions localisation e))

    let getEventComments (root : EventRoot) =
        let findComment t s (a : Both) =
            match (s, a) with
            | ((b, c), _) when b -> (b, c)
            | ((_, _), CommentI nc) -> (false, nc)
            | ((_, c), NodeI (:? Event as n)) when n.ID = t -> (true, c)
            | ((_, _), _) -> (false, "")
        root.Events |> List.map (fun e -> e.ID, root.All |> List.fold (findComment e.ID) (false, "") |> snd)

    let getImmediate (event : Event)=
        event.Children |> List.filter (fun c -> c.Key = "immediate")
                       |> List.map getIDs
    
    let getAllImmediates (root : EventRoot) =
        root.Events |> List.map (fun e -> (e.ID, getImmediate e))

    type eventView = { ID :string; Desc:string; Hidden:bool; Key:string}

    let getEventViews (root : EventRoot) =
        let getView (e : Event) =
            //let etype = e.Tag "type" |> (function | Some (String s) -> s | _ -> "")
            {ID = e.ID; Desc = e.Desc; Hidden = e.Hidden; Key = e.Key}
        root.Events |> List.map getView