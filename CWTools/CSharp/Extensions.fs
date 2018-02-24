namespace CWTools.CSharp
open CWTools.Process
open CWTools.Parser
open System.Collections.Generic
open System.Runtime.CompilerServices
open FParsec.CharParsers

[<Extension>]
type Extensions =
    /// Filter to just Nodes
    [<Extension>]
    static member inline Nodes(obj : IEnumerable<Child>) = obj |> Seq.choose (function |NodeC c -> Some c |_ -> None)
    /// Filter to just Leaves
    [<Extension>]
    static member inline Leaves(obj : IEnumerable<Child>) = obj |> Seq.choose (function |LeafC c -> Some c |_ -> None)
    /// Filter to just LeafValues
    [<Extension>]
    static member inline LeafValues(obj : IEnumerable<Child>) = obj |> Seq.choose (function |LeafValueC c -> Some c |_ -> None)
    /// Filter to just Comments
    [<Extension>]
    static member inline Comments(obj : IEnumerable<Child>) = obj |> Seq.choose (function |CommentC c -> Some c |_ -> None)
    /// Retrieve result or null (if failure)
    [<Extension>]
    static member inline GetResult(obj : ParserResult<_,_>) = obj |> function |Success(s, _, _) -> s |_ -> null
    /// Retrieve error message or null (if success)
    [<Extension>]
    static member inline GetError(obj : ParserResult<_,_>) = obj |> function |Failure(e, _, _) -> e |_ -> null
