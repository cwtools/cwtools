namespace CWTools.ExtensionPoints
open CWTools.Process

[<AbstractClass>]
type StatementVisitor() =
    abstract member Visit: Node -> Unit
    abstract member Visit: Leaf -> Unit
    abstract member Visit: LeafValue -> Unit
    abstract member Visit: string -> Unit
    member this.Visit (x : Child) : Unit =
        match x with
        |NodeC n -> this.Visit(n)
        |LeafC l -> this.Visit(l)
        |LeafValueC lv -> this.Visit(lv)
        |CommentC c -> this.Visit(c)

