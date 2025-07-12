namespace CWTools.Process

open System
open CWTools.Common.STLConstants
open CWTools.Parser.Types
open CWTools.Utilities.Position
open CWTools.Utilities.Utils
open CWTools.Utilities
open CWTools.Common.NewScope


module List =
    let replace f sub xs =
        let rec finish acc =
            function
            | [] -> acc
            | x :: xs -> finish (x :: acc) xs

        let rec search acc =
            function
            | [] -> None
            | x :: xs ->
                if f x then
                    Some(finish ((sub x) :: xs) acc)
                else
                    search (x :: acc) xs

        search [] xs

    let replaceOrAdd f sub add xs =
        let result = replace f sub xs

        match result with
        | Some ls -> ls
        | None -> add :: xs

type Trivia = { originalSource: range option }

type IKeyPos =
    abstract member Key: string
    abstract member KeyId: StringTokens
    abstract member Position: range
    abstract member Trivia: Trivia option with get, set

type IClause =
    inherit IKeyPos
    abstract member Nodes: Node seq
    abstract member LeafValues: LeafValue seq
    abstract member Leaves: Leaf seq
    abstract member ValueClauses: ValueClause seq
    abstract member AllArray: Child array
    abstract member Clauses: IClause seq
    abstract member ClauseList: IClause list
    abstract member Tag: string -> Value option
    abstract member TagText: string -> string



and Leaf =
    val mutable KeyId: StringTokens
    // val mutable Key : string
    val mutable private _valueId: StringTokens
    val mutable private _value: Value
    val mutable Position: range
    val mutable Operator: Operator
    val mutable Trivia: Trivia option


    member this.Key
        with get () = StringResource.stringManager.GetStringForID(this.KeyId.normal).Trim quoteCharArray
        and set value = this.KeyId <- StringResource.stringManager.InternIdentifierToken(value)

    member this.ValueId = this._valueId

    member this.Value
        with get () = this._value
        and set value =
            this._value <- value
            this._valueId <- StringResource.stringManager.InternIdentifierToken(value.ToString())

    member this.ValueText =
        StringResource.stringManager.GetStringForID(this.ValueId.normal).Trim quoteCharArray

    member this.ToRaw =
        KeyValue(PosKeyValue(this.Position, KeyValueItem(Key(this.Key), this.Value, this.Operator)))

    new(key: string, value: Value, pos: range, op: Operator) =
        { KeyId = StringResource.stringManager.InternIdentifierToken(key)
          _valueId = StringResource.stringManager.InternIdentifierToken(value.ToString())
          _value = value
          Position = pos
          Operator = op
          Trivia = None }

    new(keyvalueitem: KeyValueItem, ?pos: range) =
        let (KeyValueItem(Key(key), value, op)) = keyvalueitem
        Leaf(key, value, pos |> Option.defaultValue range.Zero, op)

    static member Create key value = LeafC(Leaf(key, value))

    interface IKeyPos with
        member this.Key = this.Key
        member this.KeyId = this.KeyId
        member this.Position = this.Position

        member this.Trivia
            with get () = this.Trivia
            and set v = this.Trivia <- v

and LeafValue(value: Value, ?pos: range) =
    member val Trivia: Trivia option = None with get, set
    member val ValueId = StringResource.stringManager.InternIdentifierToken(value.ToString()) with get, set
    member val private _value = value with get, set

    member this.Value
        with get () = this._value
        and set value =
            this._value <- value
            this.ValueId <- StringResource.stringManager.InternIdentifierToken(value.ToString())

    member this.ValueText =
        StringResource.stringManager.GetStringForID(this.ValueId.normal).Trim quoteCharArray

    member this.Key =
        StringResource.stringManager.GetStringForID(this.ValueId.normal).Trim quoteCharArray

    member val Position = defaultArg pos range.Zero
    member this.ToRaw = Value(this.Position, this._value)
    static member Create value = LeafValue value

    interface IKeyPos with
        member this.Key = this.Key
        member this.KeyId = this.ValueId
        member this.Position = this.Position

        member this.Trivia
            with get () = this.Trivia
            and set v = this.Trivia <- v


and [<Struct>] Child =
    | NodeC of node: Node
    | LeafC of leaf: Leaf
    | CommentC of comment: (range * string)
    | LeafValueC of leafvalue: LeafValue
    | ValueClauseC of valueclause: ValueClause

and ValueClause(keys: Value[], pos: range) =
    let mutable _keys =
        keys
        |> Array.map (fun v -> StringResource.stringManager.InternIdentifierToken(v.ToString()))

    let bothFind (x: string) =
        function
        | NodeC n when n.Key == x -> true
        | LeafC l when l.Key == x -> true
        | _ -> false

    let mutable all: Child array = Array.empty
    let mutable _leaves: Lazy<Leaf array> = lazy Array.empty

    let reset () =
        _leaves <-
            lazy
                (all
                 |> Array.choose (function
                     | LeafC l -> Some l
                     | _ -> None))

    let leaves () = _leaves.Force()

    do reset ()

    new() = ValueClause([||], range.Zero)

    member val Position = pos
    member val Scope: Scope = scopeManager.AnyScope with get, set
    member val Trivia: Trivia option = None with get, set
    member __.AllChildren = all |> ResizeArray<Child>

    member __.AllChildren
        with set (value: ResizeArray<Child>) =
            all <- (value |> Seq.toArray)
            reset ()

    member __.AllArray = all

    member __.AllArray
        with set value =
            all <- value
            reset ()

    member this.All = all |> List.ofSeq

    member this.All
        with set (value: Child list) =
            all <- (value |> List.toArray)
            reset ()

    member this.Nodes =
        seq {
            for child in all do
                match child with
                | NodeC node -> yield node
                | _ -> ()
        }

    member this.Children = this.Nodes |> List.ofSeq

    member this.Leaves =
        seq {
            for child in all do
                match child with
                | LeafC leaf -> yield leaf
                | _ -> ()
        }

    member this.Values = this.Leaves |> List.ofSeq

    member this.Comments =
        seq {
            for child in all do
                match child with
                | CommentC comment -> yield comment
                | _ -> ()
        }

    member this.LeafValues =
        seq {
            for child in all do
                match child with
                | LeafValueC leafValue -> yield leafValue
                | _ -> ()
        }

    member this.ValueClauses =
        seq {
            for child in all do
                match child with
                | ValueClauseC valueClause -> yield valueClause
                | _ -> ()
        }

    member this.Clauses =
        seq {
            for child in all do
                match child with
                | ValueClauseC vc -> yield vc :> IClause
                | NodeC n -> yield n :> IClause
                | _ -> ()
        }

    member this.Has x = all |> (Seq.exists (bothFind x))

    member __.Tag x =
        leaves ()
        |> Array.tryPick (function
            | l when l.Key == x -> Some l.Value
            | _ -> None)

    member __.Leafs x =
        leaves ()
        |> Array.choose (function
            | l when l.Key == x -> Some l
            | _ -> None)
        |> Array.toSeq

    member __.Tags x =
        leaves ()
        |> Array.choose (function
            | l when l.Key == x -> Some l.Value
            | _ -> None)
        |> Array.toSeq

    member this.TagText x =
        this.Tag x
        |> function
            | Some(QString s) -> s.GetString()
            | Some s -> s.ToString()
            | None -> ""

    member this.TagsText x =
        this.Tags x
        |> Seq.map (function
            | QString s -> s.GetString()
            | s -> s.ToString())

    member this.SetTag x v =
        this.All <- this.AllChildren |> List.ofSeq |> List.replaceOrAdd (bothFind x) (fun _ -> v) v

    member this.Child x =
        this.Nodes
        |> Seq.tryPick (function
            | c when c.Key == x -> Some c
            | _ -> None)

    member this.Childs x =
        this.Nodes
        |> Seq.choose (function
            | c when c.Key == x -> Some c
            | _ -> None)

    member this.FirstKey =
        if _keys.Length > 0 then
            Some(StringResource.stringManager.GetStringForID _keys.[0].normal)
        else
            None

    member this.FirstKeyId = if _keys.Length > 0 then Some _keys.[0] else None

    member this.SecondKey =
        if _keys.Length > 0 then
            Some(StringResource.stringManager.GetStringForID _keys.[1].normal)
        else
            None

    member __.Keys = _keys

    member __.Keys
        with set value = _keys <- value

    member this.ToRaw: Statement list =
        let children = ResizeArray<Statement>(all.Length)

        for child in this.AllArray do
            match child with
            | CommentC c -> children.Add(Comment c)
            | NodeC n -> children.Add(n.ToRaw)
            | LeafValueC lv -> children.Add(lv.ToRaw)
            | LeafC l -> children.Add(l.ToRaw)
            | ValueClauseC vc ->
                let keys = vc.Keys |> Array.map (fun k -> Value(range.Zero, Value.String(k)))
                children.AddRange(keys)
                children.Add(Value(vc.Position, Value.Clause vc.ToRaw))

        children |> List.ofSeq

    static member Create() = ValueClause()

    interface IKeyPos with
        member this.Key = this.FirstKey |> Option.defaultValue "clause"

        member this.KeyId =
            this.FirstKeyId
            |> Option.defaultValue (StringResource.stringManager.InternIdentifierToken(""))

        member this.Position = this.Position

        member this.Trivia
            with get () = this.Trivia
            and set v = this.Trivia <- v

    interface IClause with
        member this.Nodes = this.Nodes
        member this.Leaves = this.Leaves
        member this.LeafValues = this.LeafValues
        member this.ValueClauses = this.ValueClauses
        member this.AllArray = this.AllArray
        member this.Clauses = this.Clauses
        member this.ClauseList = this.Clauses |> List.ofSeq
        member this.TagText x = this.TagText x
        member this.Tag x = this.Tag x


and Node(key: string, pos: range) =
    let bothFind (x: string) =
        function
        | NodeC n when n.Key == x -> true
        | LeafC l when l.Key == x -> true
        | _ -> false

    let bothFindId (x: StringLowerToken) =
        function
        | NodeC n when n.KeyId.lower = x -> true
        | LeafC l when l.KeyId.lower = x -> true
        | _ -> false

    let mutable all: Child array = Array.empty
    let mutable _leaves: Lazy<Leaf array> = lazy Array.empty

    let reset () =
        _leaves <-
            lazy
                (all
                 |> Array.choose (function
                     | LeafC l -> Some l
                     | _ -> None))

    let leaves () = _leaves.Force()

    do reset ()

    new(key: string) = Node(key, range.Zero)

    member val KeyId: StringTokens = StringResource.stringManager.InternIdentifierToken(key) with get, set

    member this.Key
        with get () = StringResource.stringManager.GetStringForID(this.KeyId.normal).Trim quoteCharArray
        and set value = this.KeyId <- StringResource.stringManager.InternIdentifierToken(value)

    member val Position = pos
    member val Scope: Scope = scopeManager.AnyScope with get, set
    member val Trivia: Trivia option = None with get, set

    member val KeyPrefixId: StringTokens option = None with get, set

    member this.KeyPrefix
        with get () =
            this.KeyPrefixId
            |> Option.map (fun pkid -> StringResource.stringManager.GetStringForID(pkid.normal))
        and set value =
            this.KeyPrefixId <-
                value
                |> Option.map (fun value -> StringResource.stringManager.InternIdentifierToken(value))

    member val ValuePrefixId: StringTokens option = None with get, set

    member this.ValuePrefix
        with get () =
            this.ValuePrefixId
            |> Option.map (fun pkid -> StringResource.stringManager.GetStringForID(pkid.normal))
        and set value =
            this.ValuePrefixId <-
                value
                |> Option.map (fun value -> StringResource.stringManager.InternIdentifierToken(value))

    member this.IsComplex = this.KeyPrefixId.IsSome || this.ValuePrefixId.IsSome

    member __.AllChildren = all |> ResizeArray<Child>

    member __.AllChildren
        with set (value: ResizeArray<Child>) =
            all <- (value |> Seq.toArray)
            reset ()

    member __.AllArray = all

    member __.AllArray
        with set value =
            all <- value
            reset ()

    member this.All = all |> List.ofSeq

    member this.All
        with set (value: Child list) =
            all <- (value |> List.toArray)
            reset ()

    member this.Nodes =
        seq {
            for child in all do
                match child with
                | NodeC node -> yield node
                | _ -> ()
        }

    member this.Children = this.Nodes |> List.ofSeq

    member this.Leaves =
        seq {
            for child in all do
                match child with
                | LeafC leaf -> yield leaf
                | _ -> ()
        }

    member this.Values = this.Leaves |> List.ofSeq

    member this.Comments =
        seq {
            for child in all do
                match child with
                | CommentC comment -> yield comment
                | _ -> ()
        }

    member this.LeafValues =
        seq {
            for child in all do
                match child with
                | LeafValueC leafValue -> yield leafValue
                | _ -> ()
        }

    member this.ValueClauses =
        seq {
            for child in all do
                match child with
                | ValueClauseC valueClause -> yield valueClause
                | _ -> ()
        }

    member this.Clauses =
        seq {
            for child in all do
                match child with
                | ValueClauseC vc -> yield vc :> IClause
                | NodeC n -> yield n :> IClause
                | _ -> ()
        }

    member this.Has x = all |> (Array.exists (bothFind x))
    member this.HasById x = all |> (Array.exists (bothFindId x))

    member __.Tag x =
        leaves ()
        |> Array.tryPick (function
            | l when l.Key == x -> Some l.Value
            | _ -> None)

    member __.TagById x =
        leaves ()
        |> Array.tryPick (function
            | l when l.KeyId.lower = x -> Some l.ValueId
            | _ -> None)

    member __.Leafs x =
        leaves ()
        |> Array.choose (function
            | l when l.Key == x -> Some l
            | _ -> None)
        |> Array.toSeq

    member __.LeafsById x =
        leaves () |> Array.filter (fun l -> l.KeyId.lower = x) |> Array.toSeq

    member __.Tags x =
        leaves ()
        |> Array.choose (function
            | l when l.Key == x -> Some l.Value
            | _ -> None)
        |> Array.toSeq

    member this.TagText x =
        this.Tag x
        |> function
            | Some(QString s) -> s.GetString()
            | Some s -> s.ToString()
            | None -> ""

    member this.TagsText x =
        this.Tags x
        |> Seq.map (function
            | QString s -> s.GetString()
            | s -> s.ToString())

    member this.SetTag x v =
        this.All <- this.AllChildren |> List.ofSeq |> List.replaceOrAdd (bothFind x) (fun _ -> v) v

    member this.Child x =
        this.Nodes
        |> Seq.tryPick (function
            | c when c.Key == x -> Some c
            | _ -> None)

    member this.Childs x =
        this.Nodes
        |> Seq.choose (function
            | c when c.Key == x -> Some c
            | _ -> None)

    member this.ToRaw: Statement =
        let children = ResizeArray<Statement>(all.Length)

        for child in this.AllArray do
            match child with
            | CommentC c -> children.Add(Comment c)
            | NodeC n -> children.Add(n.ToRaw)
            | LeafValueC lv -> children.Add(lv.ToRaw)
            | LeafC l -> children.Add(l.ToRaw)
            | ValueClauseC vc ->
                let keys = vc.Keys |> Array.map (fun k -> Value(range.Zero, Value.String(k)))
                children.AddRange(keys)
                children.Add(Value(vc.Position, Value.Clause vc.ToRaw))

        KeyValue(PosKeyValue(this.Position, KeyValueItem(Key this.Key, Clause(List.ofSeq children), Operator.Equals)))

    static member Create key = Node(key)

    interface IKeyPos with
        member this.Key = this.Key
        member this.KeyId = this.KeyId
        member this.Position = this.Position

        member this.Trivia
            with get () = this.Trivia
            and set v = this.Trivia <- v

    interface IClause with
        member this.Nodes = this.Nodes
        member this.Leaves = this.Leaves
        member this.LeafValues = this.LeafValues
        member this.ValueClauses = this.ValueClauses
        member this.AllArray = this.AllArray
        member this.Clauses = this.Clauses
        member this.ClauseList = this.Clauses |> List.ofSeq
        member this.TagText x = this.TagText x
        member this.Tag x = this.Tag x

module ProcessCore =

    let processNode (postinit: Node -> Node) inner (key: string) (pos: range) (sl: Statement list) : Node =
        let node = Node(key, pos) |> postinit //  |> postinit// :?> Node |> postinit
        let children = sl |> List.map inner
        node.All <- children
        node

    type LookupContext =
        { complete: bool
          parents: string list
          scope: string
          previous: string
          entityType: EntityType }

    let processNodeSimple _ = processNode id

    type NodeTypeMap =
        string * range * LookupContext
            -> (LookupContext -> (Statement -> Child) -> string -> range -> Statement list -> Node) *
            string *
            (LookupContext -> LookupContext)

    let updateContext f n key context =
        match n with
        | "" -> f { context with previous = key }
        | _ ->
            f
                { context with
                    parents = n :: context.parents
                    previous = key }

    type BaseProcess() =
        let rec nodeWindowFun context (backtwo: Statement option, backone: Statement option, acc) (next: Statement) =
            //eprintfn "%A %A %A" backtwo backone next
            match backtwo, backone, next with
            | Some(Value(_, Clause _)), _, _
            | Some(Value _), Some(KeyValue(PosKeyValue(_, KeyValueItem(Key _, Clause _, _)))), Value(_, Clause _)
            | _, Some(Value(_, Clause _)), _ -> backone, Some next, (processNodeInner context next) :: acc
            | Some(Value(_, v2)), Some(Value(_, v1)), Value(pos, Clause sl) ->
                None, None, (lookupVC pos context sl [| v2; v1 |]) :: (acc |> List.skip 2)
            | Some(Value(_, v2)), Some(KeyValue(PosKeyValue(_, KeyValueItem(Key(k), v1, _)))), Value(pos, Clause sl) ->
                let node: Node = lookupN k pos context sl
                node.KeyPrefix <- Some(v2.ToRawString())
                node.ValuePrefix <- Some(v1.ToRawString())
                None, None, (NodeC node) :: (acc |> List.skip 2)
            | _, Some(Value(pos, v2)), KeyValue(PosKeyValue(pos2, KeyValueItem(Key(k), Clause sl, _))) when
                pos.StartLine = pos2.StartLine
                ->
                let node = lookupN k pos2 context sl
                node.KeyPrefix <- Some(v2.ToRawString())
                None, None, (NodeC node) :: (acc |> List.skip 1)
            //     None, None,
            | _ -> backone, Some next, (processNodeInner context next) :: acc

        and lookupN =
            (fun (key: string) (pos: range) (context: LookupContext) (sl: Statement list) ->
                let n = Node(key, pos)
                // let children = sl |> List.map (fun e -> (processNodeInner context e))
                let children =
                    sl
                    |> List.fold (nodeWindowFun context) (None, None, [])
                    |> (fun (_, _, ls) -> ls |> List.rev)

                n.All <- children
                n)

        and lookupVC =
            (fun (pos: range) (context: LookupContext) (sl: Statement list) keys ->
                let vc = ValueClause(keys, pos)
                //let children = sl |> List.map (fun e -> (processNodeInner context e))
                let children =
                    sl
                    |> List.fold (nodeWindowFun context) (None, None, [])
                    |> (fun (_, _, ls) -> ls |> List.rev)

                vc.All <- children
                ValueClauseC vc)

        and processNodeInner (c: LookupContext) statement =
            //log "%A" node.Key
            match statement with
            | KeyValue(PosKeyValue(pos, KeyValueItem(Key(k), Clause(sl), _))) -> NodeC(lookupN k pos c sl)
            | KeyValue(PosKeyValue(pos, kv)) -> LeafC(Leaf(kv, pos))
            | Comment(r, c) -> CommentC(r, c)
            | Value(pos, Value.Clause sl) -> lookupVC pos c sl [||]
            | Value(pos, v) -> LeafValueC(LeafValue(v, pos))
        // member __.ProcessNode() = processNode id (processNodeInner { complete = false; parents = []; scope = ""; previous = ""; entityType = EntityType.Other})
        // member __.ProcessNode() = (fun key pos sl -> (processNodeInner { complete = false; parents = []; scope = ""; previous = ""; entityType = EntityType.Other}) (KeyValue(PosKeyValue(pos, KeyValueItem(Key(key) , Clause(sl), Operator.Equals)))))
        member __.ProcessNode() =
            (fun key pos sl ->
                lookupN
                    key
                    pos
                    { complete = false
                      parents = []
                      scope = ""
                      previous = ""
                      entityType = EntityType.Other }
                    sl)

        member __.ProcessNode(entityType: EntityType) =
            (fun key pos sl ->
                lookupN
                    key
                    pos
                    { complete = false
                      parents = []
                      scope = ""
                      previous = ""
                      entityType = entityType }
                    sl)

    let processNodeBasic = BaseProcess().ProcessNode()

    let rec foldNode fNode acc (node: Node) : 'r =
        let recurse = foldNode fNode
        let newAcc = fNode acc node
        node.Children |> Seq.fold recurse newAcc

    let foldNode2 fNode fCombine acc (node: Node) =
        let rec loop nodes cont =
            match nodes with
            | x: Node :: tail ->
                loop x.Children (fun accChildren ->
                    let resNode = fNode x accChildren
                    loop tail (fun accTail -> cont (fCombine resNode accTail)))
            | [] -> cont acc

        loop [ node ] id

    let foldClause2 fNode fCombine acc (node: IClause) =
        let rec loop nodes cont =
            match nodes with
            | x: IClause :: tail ->
                loop (x.Clauses |> List.ofSeq) (fun accChildren ->
                    let resNode = fNode x accChildren
                    loop tail (fun accTail -> cont (fCombine resNode accTail)))
            | [] -> cont acc

        loop [ node ] id

    let foldNode3 fNode (node: Node) =
        let rec loop nodes cont =
            //Rewrite to use a parralel seq for each child
            match nodes with
            | x: Node :: tail ->
                let y =
                    loop x.Children (fun a ->
                        let resNode = fNode x

                        loop tail (fun accTail ->
                            cont (
                                seq {
                                    yield! resNode
                                    yield! accTail
                                }
                            )))

                y
            // cont(fCombine resNode accTail) ))
            | [] ->
                let x = cont Seq.empty
                x

        loop [ node ] id |> List.ofSeq

    let foldNode4 fNode (node: Node) =
        let rec loop (nodes: seq<Node>) =
            seq {
                yield! nodes |> Seq.collect fNode
                let x = nodes |> Seq.collect (fun n -> n.Nodes)
                yield! loop x
            }

        loop (seq { yield node }) |> List.ofSeq


    let foldNode5 fNode (node: Node) =
        let rec loop nodes cont =
            match nodes with
            | x: Node :: tail ->
                loop x.Children (fun a ->
                    let resNode = fNode x

                    loop tail (fun accTail ->
                        cont (
                            seq {
                                yield! a
                                yield! resNode
                                yield! accTail
                            }
                        )))
            | [] -> cont Seq.empty

        loop [ node ] id

    let foldNode6 fNode (node: Node) =
        let rec loop nodes cont =
            match nodes with
            | x: Node :: tail ->
                loop x.Children (fun accChildren ->
                    let resNode = fNode x
                    loop tail (fun accTail -> cont (resNode :: accTail)))
            | [] -> cont []

        loop [ node ] id //|> List.collect

    let foldNode7 fNode (node: Node) =
        let rec loop acc (node: Node) =
            let resNode = fNode node acc
            node.Children |> List.fold loop resNode
        //| [] -> acc
        loop [] node

    let foldNode8 fNode fCombine acc (node: Node) =
        let rec loop acc nodes =
            match nodes with
            | (x: Node) ->
                let resNode = fNode x acc
                x.Children |> List.map (loop resNode) |> fCombine
        //| [] -> acc
        loop acc node

    let rec foldNodeWithState fNode acc (node: Node) =
        let recurse = foldNodeWithState fNode
        let newAcc, res = fNode acc node

        match res with
        | None -> (node.Children |> List.collect (recurse newAcc))
        | Some e -> e :: (node.Children |> List.collect (recurse newAcc))
    //res::(node.Children |> List.collect (recurse newAcc))

    let rec cata fNode (node: Node) : 'r =
        let recurse = cata fNode
        fNode node (node.Children |> Seq.map recurse)

    let rec cataNodeIter fNode (node: Node) =
        let recurse = cataNodeIter fNode
        if fNode node then () else node.Nodes |> Seq.iter recurse

    let rec cataIter fChild (child: Child) =
        let recurse = cataIter fChild

        if fChild child then
            ()
        else
            match child with
            | NodeC node -> node.AllArray |> Array.iter recurse
            | _ -> ()
