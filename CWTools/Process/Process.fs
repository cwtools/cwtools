namespace CWTools.Process

open CWTools.Parser
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

 
type Leaf(keyvalueitem : KeyValueItem, ?pos : Position) =
    let (KeyValueItem (Key(key), value)) = keyvalueitem
    member val Key = key with get, set
    member val Value = value with get, set
    member val Position = defaultArg pos Position.Empty
    [<JsonIgnore>]
    member this.ToRaw = KeyValueItem(Key(this.Key), this.Value)
    new(key : string, value : Value) = Leaf(KeyValueItem(Key(key), value))
    static member Create key value = LeafC(Leaf(KeyValueItem(Key(key), value)))
and LeafValue(value : Value) =
    member val Value = value with get, set
    [<JsonIgnore>]
    member this.ToRaw = Value(this.Value)
    static member Create value = LeafValue value
   
and Child = |NodeC of Node | LeafC of Leaf |CommentC of string |LeafValueC of LeafValue
and Node (key : string, pos : Position) =
    let bothFind x = function |NodeC n when n.Key = x -> true |LeafC l when l.Key = x -> true |_ -> false
    new(key : string) = Node(key, Position.Empty)
    member val Key = key
    member val Position = pos
    member val AllChildren : ResizeArray<Child> = new ResizeArray<Child>() with get, set
    member this.All with get () = this.AllChildren |> List.ofSeq
    member this.All with set(value : Child list) = this.AllChildren <- (value |> ResizeArray<Child>)
    member this.Nodes = this.All |> Seq.choose (function |NodeC n -> Some n |_ -> None)
    member this.Children = this.Nodes |> List.ofSeq
    member this.Leaves = this.All |> Seq.choose (function |LeafC l -> Some l |_ -> None)
    member this.Values = this.Leaves |> List.ofSeq
    member this.Comments = this.All |> Seq.choose (function |CommentC c -> Some c |_ -> None)
    member this.Has x = this.All |> (Seq.exists (bothFind x))
    member this.Tag x = this.Values |> Seq.tryPick (function |l when l.Key = x -> Some l.Value |_ -> None)
    member this.Leafs x = this.Values |> Seq.choose (function |l when l.Key = x -> Some l |_ -> None)
    member this.Tags x = this.Values |> Seq.choose (function |l when l.Key = x -> Some l.Value |_ -> None)
    member this.TagText x = this.Tag x |> function |Some (QString s) -> s |Some s -> s.ToString() |None -> ""
    member this.TagsText x = this.Tags x |> Seq.map (function |(QString s) -> s |s -> s.ToString())
    member this.SetTag x v = this.All <- this.All |> List.ofSeq |>  List.replaceOrAdd (bothFind x) (fun _ -> v) v
    member this.Child x = this.Children |> Seq.tryPick (function |c when c.Key = x -> Some c |_ -> None)
    member this.Childs x = this.Children |> Seq.choose (function |c when c.Key = x -> Some c |_ -> None)

    [<JsonIgnore>]
    member this.ToRaw : Statement list = this.All |> List.ofSeq |> List.rev |>
                                         List.map (function 
                                           |NodeC n -> KeyValue(PosKeyValue(n.Position, KeyValueItem(Key n.Key, Clause n.ToRaw)))
                                           |LeafValueC lv -> lv.ToRaw
                                           |LeafC l -> KeyValue(PosKeyValue (l.Position, l.ToRaw))
                                           |CommentC c -> (Comment c))


    static member Create key = Node(key)                               
    


module ProcessCore =

    let processNode< 'T when 'T :> Node > inner (key : string) (pos : Position) (sl : Statement list) : Node =
        // let node = match key with
        //             |"" -> Activator.CreateInstance(typeof<'T>) :?> Node
        //             |x -> Activator.CreateInstance(typeof<'T>, x) :?> Node
        //let paramList : obj[] = [|key; pos|]
        //let bindingFlags = BindingFlags.CreateInstance ||| BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.OptionalParamBinding
        //let node = Activator.CreateInstance(typeof<'T>, bindingFlags ,null, paramList, null) :?> Node
        let node = Activator.CreateInstance(typeof<'T>, key, pos) :?> Node
        sl |> List.iter (fun e -> inner node e) |> ignore
        node
    type LookupContext = { complete : bool}
    type NodeTypeMap = ((string * Position * LookupContext) -> bool) * ((Node -> Statement -> unit) -> string -> Position -> Statement list -> Node) * (LookupContext -> LookupContext)
    
    let fst3 (x, _, _) = x
    let snd3 (_, x, _) = x
    let tri3 (_, _, x) = x
    type BaseProcess (maps : NodeTypeMap list ) =
        let rec lookup =
            (fun (key : string) (pos : Position) (context : LookupContext) ->
                match maps |> List.tryFind (fun (a, _, _) -> a (key, pos, context)) with
                |Some (_,t, c) -> t (processNodeInner (c context)) key pos
                |None -> processNode<Node> (processNodeInner context) key pos
                ) >> (fun f a b c -> NodeC (f a b c))
        and processNodeInner (c : LookupContext) (node : Node) statement =
            match statement with
            | KeyValue(PosKeyValue(pos, KeyValueItem(Key(k) , Clause(sl)))) -> node.All <- lookup k pos c sl::node.All
            | KeyValue(PosKeyValue(pos, kv)) -> node.All <- LeafC(Leaf(kv, pos))::node.All
            | Comment(c) -> node.All <- CommentC c::node.All
            | Value(v) -> node.All <- LeafValueC(LeafValue(v))::node.All
        member __.ProcessNode<'T when 'T :> Node >() = processNode<'T> (processNodeInner { complete = false})

    let baseMap = []
    let processNodeBasic = BaseProcess(baseMap).ProcessNode()

    let rec foldNode fNode acc (node : Node) :'r =
        let recurse = foldNode fNode
        let newAcc = fNode acc node
        node.Children |> Seq.fold recurse newAcc

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
    
    let rec cata fNode (node:Node) :'r =
        let recurse = cata fNode
        fNode node (node.Children |> Seq.map recurse)
    
   