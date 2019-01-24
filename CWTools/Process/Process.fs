namespace CWTools.Process

open CWTools.Parser
open System
open CWTools.Common.STLConstants
open CWTools.Parser.Types
open CWTools.Utilities.Position
open CWTools.Utilities.Utils
open CWTools.Utilities


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



type IKeyPos =
    abstract member Key : string
    abstract member Position : range
[<Struct>]
type Leaf =
    val mutable KeyId : StringToken
    // val mutable Key : string
    val mutable private _valueId : StringToken
    val mutable private _value : Value
    val mutable Position : range

    member this.Key
        with get () = StringResource.stringManager.GetStringForID(this.KeyId)
        and set (value) = this.KeyId <- StringResource.stringManager.InternIdentifierToken(value)
    member this.ValueId
        with get () = this._valueId
    member this.Value
        with get () = this._value
        and set (value) = this._value <- value; this._valueId <- StringResource.stringManager.InternIdentifierToken(value.ToRawString())
    member this.ToRaw = KeyValueItem(Key(this.Key), this.Value)
    new(key : string, value : Value, pos : range) =
        {
            KeyId = StringResource.stringManager.InternIdentifierToken(key);
            _valueId = StringResource.stringManager.InternIdentifierToken(value.ToRawString())
            _value = value;
            Position = pos
            }
    //new(key : string, value : Value) = Leaf(key, value, Position.Empty)
    new(keyvalueitem : KeyValueItem, ?pos : range) =
        let (KeyValueItem (Key(key), value)) = keyvalueitem
        Leaf(key, value, pos |> Option.defaultValue range.Zero)
    static member Create key value = LeafC(Leaf(key, value))
    interface IKeyPos with
        member this.Key = this.Key
        member this.Position = this.Position
and LeafValue(value : Value, ?pos : range) =

    member val ValueId = StringResource.stringManager.InternIdentifierToken(value.ToRawString()) with get, set
    member val private _value = value with get, set
    // val mutable private _value : Value = value
    member this.Value
        with get () = this._value
        and set (value) = this._value <- value; this.ValueId <- StringResource.stringManager.InternIdentifierToken(value.ToRawString())

    member this.Key = StringResource.stringManager.GetStringForID(this.ValueId)
    member val Position = defaultArg pos range.Zero
    member this.ToRaw = Value(this.Position, this._value)
    static member Create value = LeafValue value
    interface IKeyPos with
        member this.Key = this.Key
        member this.Position = this.Position

and [<Struct>] Child = |NodeC of node : Node | LeafC of leaf : Leaf |CommentC of comment : string |LeafValueC of lefavalue : LeafValue
and Node (key : string, pos : range) =
    let bothFind (x : string) = function |NodeC n when n.Key == x -> true |LeafC l when l.Key == x -> true |_ -> false
    let mutable all : Child array = Array.empty
    let mutable _leaves : Lazy<Leaf array> = lazy ( Array.empty )
    let reset() =
        _leaves <- lazy (all |> Array.choose (function |LeafC l -> Some l |_ -> None))
    let leaves() = _leaves.Force()

    do reset()

    new(key : string) = Node(key, range.Zero)

    member val KeyId = StringResource.stringManager.InternIdentifierToken(key) with get, set

    member this.Key
        with get () = StringResource.stringManager.GetStringForID(this.KeyId)
        and set (value) = this.KeyId <- StringResource.stringManager.InternIdentifierToken(value)

    member val Position = pos
    member val Scope : Scope = Scope.Any with get, set
    member __.AllChildren with get () = all |> ResizeArray<Child>
    member __.AllChildren with set(value : ResizeArray<Child>) = all <- (value |> Seq.toArray); reset()
    member this.All with get () = all |> List.ofSeq
    member this.All with set(value : Child list) = all <- (value |> List.toArray); reset()
    member this.Nodes = this.AllChildren |> Seq.choose (function |NodeC n -> Some n |_ -> None)
    member this.Children = this.Nodes |> List.ofSeq
    member this.Leaves = this.AllChildren |> Seq.choose (function |LeafC l -> Some l |_ -> None)
    member this.Values = this.Leaves |> List.ofSeq
    member this.Comments = this.AllChildren |> Seq.choose (function |CommentC c -> Some c |_ -> None)
    member this.LeafValues = this.AllChildren |> Seq.choose(function |LeafValueC lv -> Some lv |_ -> None)
    member this.Has x = this.AllChildren |> (Seq.exists (bothFind x))
    member __.Tag x = leaves() |> Array.tryPick (function |l when l.Key == x -> Some l.Value |_ -> None)
    member __.Leafs x = leaves() |> Array.choose (function |l when l.Key == x -> Some l |_ -> None) |> Array.toSeq
    member __.Tags x = leaves() |> Array.choose (function |l when l.Key == x -> Some l.Value |_ -> None) |> Array.toSeq
    member this.TagText x = this.Tag x |> function |Some (QString s) -> s |Some s -> s.ToString() |None -> ""
    member this.TagsText x = this.Tags x |> Seq.map (function |(QString s) -> s |s -> s.ToString())
    member this.SetTag x v = this.All <- this.AllChildren |> List.ofSeq |>  List.replaceOrAdd (bothFind x) (fun _ -> v) v
    member this.Child x = this.Nodes |> Seq.tryPick (function |c when c.Key == x -> Some c |_ -> None)
    member this.Childs x = this.Nodes |> Seq.choose (function |c when c.Key == x -> Some c |_ -> None)

    member this.ToRaw : Statement list = this.All |>
                                         List.map (function
                                           |NodeC n -> KeyValue(PosKeyValue(n.Position, KeyValueItem(Key n.Key, Clause n.ToRaw)))
                                           |LeafValueC lv -> lv.ToRaw
                                           |LeafC l -> KeyValue(PosKeyValue (l.Position, l.ToRaw))
                                           |CommentC c -> (Comment c))

    static member Create key = Node(key)
    interface IKeyPos with
        member this.Key = this.Key
        member this.Position = this.Position

module ProcessCore =

    let processNode (postinit : Node -> Node) inner (key : string) (pos : range) (sl : Statement list) : Node =
        // let node = match key with
        //             |"" -> Activator.CreateInstance(typeof<'T>) :?> Node
        //             |x -> Activator.CreateInstance(typeof<'T>, x) :?> Node
        //let paramList : obj[] = [|key; pos|]
        //let bindingFlags = BindingFlags.CreateInstance ||| BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.OptionalParamBinding
        //let node = Activator.CreateInstance(typeof<'T>, bindingFlags ,null, paramList, null) :?> Node
        // let node =  Activator.CreateInstance(typeof<'T>, key, pos) :?> 'T |> postinit :> Node//  |> postinit// :?> Node |> postinit
        let node =  Node(key, pos) |> postinit//  |> postinit// :?> Node |> postinit
        sl |> List.iter (fun e -> inner node e) |> ignore
        node

    type LookupContext = { complete : bool; parents : string list; scope : string; previous : string; entityType : EntityType }
    let processNodeSimple _ = processNode id
    type NodeTypeMap = ((string * range * LookupContext)) -> (LookupContext -> ((Node -> Statement -> unit) -> string -> range -> Statement list -> Node)) * string * (LookupContext -> LookupContext)

    let fst3 (x, _, _) = x
    let snd3 (_, x, _) = x
    let tri3 (_, _, x) = x

    let updateContext f n key context =
        match n with
        |"" -> f { context with previous = key }
        |_ -> f { context with parents = n::context.parents; previous = key }

    type BaseProcess (maps : NodeTypeMap ) =
        let rec lookup =
            (fun (key : string) (pos : range) (context : LookupContext) ->
                match maps (key, pos, context) with
                |(t, n, c) -> t context (processNodeInner (updateContext c n key context)) key pos
                // match maps |> List.tryFind (fun (a, _, _, _) -> a (key, pos, context)) with
                // |Some (_,t, n, c) -> t context (processNodeInner (updateContext c n key context)) key pos
                // |None -> processNode<Node> id (processNodeInner {context with previous = key}) key pos
                ) >> (fun f a b c -> NodeC (f a b c |> (fun n -> n.All <- n.All |> List.rev; n)))
        and processNodeInner (c : LookupContext) (node : Node) statement =
            //log "%A" node.Key
            match statement with
            | KeyValue(PosKeyValue(pos, KeyValueItem(Key(k) , Clause(sl)))) -> node.All <- lookup k pos c sl::node.All
            | KeyValue(PosKeyValue(pos, kv)) -> node.All <- LeafC(Leaf(kv, pos))::node.All
            | Comment(c) -> node.All <- CommentC c::node.All
            | Value(pos, v) -> node.All <- LeafValueC(LeafValue(v, pos))::node.All
        member __.ProcessNode() = processNode id (processNodeInner { complete = false; parents = []; scope = ""; previous = ""; entityType = EntityType.Other})
        member __.ProcessNode(entityType : EntityType) = processNode id (processNodeInner { complete = false; parents = []; scope = ""; previous = ""; entityType = entityType})

    let baseMap = fun _ -> processNodeSimple, "", id;
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

    let foldNode3 fNode (node : Node) =
        let rec loop nodes cont =
            //Rewrite to use a parralel seq for each child
            match nodes with
            | (x : Node)::tail ->
                let y = loop x.Children (fun a ->
                    let resNode = fNode x
                    loop tail (fun accTail ->
                        cont(seq {yield! resNode; yield! accTail})))
                y
                        // cont(fCombine resNode accTail) ))
            | [] ->
                let x = cont Seq.empty
                x

        loop [node] id |> List.ofSeq

    let foldNode4 fNode (node : Node) =
        let rec loop (nodes : seq<Node>) =
            seq {
                yield! nodes |> Seq.collect fNode
                let x = nodes |> Seq.collect (fun n -> n.Nodes)
                yield! loop x
            }
        loop (seq { yield node}) |> List.ofSeq


    let foldNode5 fNode (node:Node) =
        let rec loop nodes cont =
            match nodes with
            | (x : Node)::tail ->
                loop x.Children (fun a ->
                    let resNode = fNode x
                    loop tail (fun accTail ->
                        cont(seq {yield! a; yield! resNode; yield! accTail}) ))
            | [] -> cont Seq.empty
        loop [node] id

    let foldNode6 fNode (node:Node) =
        let rec loop nodes cont =
            match nodes with
            | (x : Node)::tail ->
                loop x.Children (fun accChildren ->
                    let resNode = fNode x
                    loop tail (fun accTail ->
                        cont(resNode::accTail) ))
            | [] -> cont []
        loop [node] id //|> List.collect

    let foldNode7 fNode (node : Node) =
        let rec loop acc nodes  =
            match nodes with
            | (x : Node) ->
                let resNode = fNode x acc
                x.Children |> List.fold loop resNode
            //| [] -> acc
        loop [] node

    let foldNode8 fNode fCombine acc (node : Node) =
        let rec loop acc nodes  =
            match nodes with
            | (x : Node) ->
                let resNode = fNode x acc
                x.Children |> List.map (loop resNode) |> fCombine
            //| [] -> acc
        loop acc node

    let rec foldNodeWithState fNode acc (node : Node) =
        let recurse = foldNodeWithState fNode
        let newAcc, res = fNode acc node
        match res with
        |None -> (node.Children |> List.collect (recurse newAcc))
        |Some e -> e ::(node.Children |> List.collect (recurse newAcc))
        //res::(node.Children |> List.collect (recurse newAcc))

    let rec cata fNode (node:Node) :'r =
        let recurse = cata fNode
        fNode node (node.Children |> Seq.map recurse)

