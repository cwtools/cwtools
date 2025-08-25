// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.


/// Anything to do with special names of identifiers and other lexical rules
module CWTools.Utilities.Position

open System
open System.IO
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Runtime.Serialization
open System.Threading
open Microsoft.FSharp.Core.Printf

let rec pown32 n =
    if n = 0 then 0 else (pown32 (n - 1) ||| (1 <<< (n - 1)))

let rec pown64 n =
    if n = 0 then 0L else (pown64 (n - 1) ||| (1L <<< (n - 1)))

let mask32 m n = (pown32 n) <<< m
let mask64 m n = (pown64 n) <<< m

type FileIndex = int32

[<Literal>]
let columnBitCount = 11

[<Literal>]
let lineBitCount = 21

let posBitCount = lineBitCount + columnBitCount
let _ = assert (posBitCount <= 32)
let posColumnMask = mask32 0 columnBitCount
let lineColumnMask = mask32 columnBitCount lineBitCount

[<Struct; CustomEquality; NoComparison; IsReadOnly>]
[<System.Diagnostics.DebuggerDisplay("{Line},{Column}")>]
type pos(code: int32) =
    new(l, c) =
        let l = max 0 l
        let c = max 0 c
        let p = (c &&& posColumnMask) ||| ((l <<< columnBitCount) &&& lineColumnMask)
        pos p

    member p.Line = (int32 (uint32 code >>> columnBitCount))
    member p.Column = (code &&& posColumnMask)

    member r.Encoding = code
    static member EncodingSize = posBitCount
    static member Decode(code: int32) : pos = pos code

    override p.Equals(obj) =
        match obj with
        | :? pos as p2 -> code = p2.Encoding
        | _ -> false

    override p.GetHashCode() = hash code
    override p.ToString() = sprintf "(%d,%d)" p.Line p.Column

[<Literal>]
let fileIndexBitCount = 16

[<Literal>]
let startLineBitCount = lineBitCount

[<Literal>]
let startColumnBitCount = columnBitCount

[<Literal>]
let heightBitCount = 20 // If necessary, could probably deduct one or two bits here without ill effect.

[<Literal>]
let endColumnBitCount = columnBitCount

[<Literal>]
let isSyntheticBitCount = 1
#if DEBUG
let _ =
    assert
        (fileIndexBitCount
         + startLineBitCount
         + startColumnBitCount
         + heightBitCount
         + endColumnBitCount
         + isSyntheticBitCount = 64 + 16)
#endif

[<Literal>]
let fileIndexShift = 0

[<Literal>]
let startLineShift = 0

[<Literal>]
let startColumnShift = 21

[<Literal>]
let heightShift = 32

[<Literal>]
let endColumnShift = 52

[<Literal>]
let isSyntheticShift = 63


[<Literal>]
let fileIndexMask =
    0b0000000000000000000000000000000000000000000000000000000000000000L

[<Literal>]
let startLineMask =
    0b0000000000000000000000000000000000000000000111111111111111111111L

[<Literal>]
let startColumnMask =
    0b0000000000000000000000000000000011111111111000000000000000000000L

[<Literal>]
let heightMask = 0b0000000000001111111111111111111100000000000000000000000000000000L

[<Literal>]
let endColumnMask =
    0b0111111111110000000000000000000000000000000000000000000000000000L

[<Literal>]
let isSyntheticMask =
    0b1000000000000000000000000000000000000000000000000000000000000000L

#if DEBUG
// let _ = assert (startLineShift   = fileIndexShift   + fileIndexBitCount)
let _ = assert (startColumnShift = startLineShift + startLineBitCount)
let _ = assert (heightShift = startColumnShift + startColumnBitCount)
let _ = assert (endColumnShift = heightShift + heightBitCount)
let _ = assert (isSyntheticShift = endColumnShift + endColumnBitCount)
// let _ = assert (fileIndexMask =   mask64 0 fileIndexBitCount)
let _ = assert (startLineMask = mask64 startLineShift startLineBitCount)
let _ = assert (startColumnMask = mask64 startColumnShift startColumnBitCount)
let _ = assert (heightMask = mask64 heightShift heightBitCount)
let _ = assert (endColumnMask = mask64 endColumnShift endColumnBitCount)
let _ = assert (isSyntheticMask = mask64 isSyntheticShift isSyntheticBitCount)
#endif

// This is just a standard unique-index table
type FileIndexTable() =
    let indexToFileTable = new ResizeArray<_>(11)
    let fileToIndexTable = new Dictionary<string, int>(11)

    [<NonSerialized>]
    let mutable lock = Lock()

    [<OnDeserialized>]
    member _.OnDeserialized(_context: StreamingContext) =
        // Recreate the lock after deserialization
        lock <- Lock()

    member t.FileToIndex f =
        let mutable res = 0
        let ok = fileToIndexTable.TryGetValue(f, &res)

        if ok then
            res
        else
            lock.Enter()

            try
                let mutable res = 0 in
                let ok = fileToIndexTable.TryGetValue(f, &res) in

                if ok then
                    res
                else
                    let n = indexToFileTable.Count in
                    indexToFileTable.Add(f)
                    fileToIndexTable.[f] <- n
                    n
            finally
                lock.Exit()

    member t.IndexToFile n =
        (if n < 0 then
             failwithf "fileOfFileIndex: negative argument: n = %d\n" n)

        (if n >= indexToFileTable.Count then
             failwithf "fileOfFileIndex: invalid argument: n = %d\n" n)

        indexToFileTable.[n]

let maxFileIndex = pown32 fileIndexBitCount

// ++GLOBAL MUTABLE STATE
// WARNING: Global Mutable State, holding a mapping between integers and filenames
let mutable fileIndexTable = new FileIndexTable()

// If we exceed the maximum number of files we'll start to report incorrect file names
let fileIndexOfFile f =
    fileIndexTable.FileToIndex(f) % maxFileIndex

let fileOfFileIndex n = fileIndexTable.IndexToFile(n)

let mkPos l c = pos (l, c)

[<Struct; CustomEquality; NoComparison; IsReadOnly>]
#if DEBUG
[<System.Diagnostics.DebuggerDisplay("({StartLine},{StartColumn}-{EndLine},{EndColumn}) {FileName} IsSynthetic={IsSynthetic} -> {DebugCode}")>]
#else
[<System.Diagnostics.DebuggerDisplay("({StartLine},{StartColumn}-{EndLine},{EndColumn}) {FileName} IsSynthetic={IsSynthetic}")>]
#endif
type range(code: int64, fidx: int16) =
    static member Zero = range (0L, 0s)

    new(fidx, bl, bc, el, ec) =
        range (
            (int64 bl <<< startLineShift)
            ||| (int64 bc <<< startColumnShift)
            ||| (int64 (el - bl) <<< heightShift)
            ||| (int64 ec <<< endColumnShift),
            int16 fidx
        )

    new(fidx, b: pos, e: pos) = range (fidx, b.Line, b.Column, e.Line, e.Column)

    member r.StartLine = int32 ((code &&& startLineMask) >>> startLineShift)
    member r.StartColumn = int32 ((code &&& startColumnMask) >>> startColumnShift)
    member r.EndLine = int32 ((code &&& heightMask) >>> heightShift) + r.StartLine
    member r.EndColumn = int32 ((code &&& endColumnMask) >>> endColumnShift)
    member r.IsSynthetic = int32 ((code &&& isSyntheticMask) >>> isSyntheticShift) <> 0
    member r.Start = pos (r.StartLine, r.StartColumn)
    member r.End = pos (r.EndLine, r.EndColumn)
    member r.FileIndex = int32 (fidx)
    member m.StartRange = range (m.FileIndex, m.Start, m.Start)
    member m.EndRange = range (m.FileIndex, m.End, m.End)
    member r.FileName = fileOfFileIndex r.FileIndex
#if DEBUG
    member r.DebugCode =
        try
            let endCol = r.EndColumn - 1
            let startCol = r.StartColumn - 1

            File.ReadAllLines(r.FileName)
            |> Seq.skip (r.StartLine - 1)
            |> Seq.take (r.EndLine - r.StartLine + 1)
            |> String.concat "\n"
            |> fun s -> s.Substring(startCol + 1, s.LastIndexOf("\n", StringComparison.Ordinal) + 1 - startCol + endCol)
        with e ->
            e.ToString()
#endif
    member r.MakeSynthetic() = range (code ||| isSyntheticMask, fidx)

    override r.ToString() =
        sprintf
            "%s (%d,%d--%d,%d) IsSynthetic=%b"
            r.FileName
            r.StartLine
            r.StartColumn
            r.EndLine
            r.EndColumn
            r.IsSynthetic

    member r.ToShortString() =
        sprintf "(%d,%d--%d,%d)" r.StartLine r.StartColumn r.EndLine r.EndColumn

    member r.Code = code

    override r.Equals(obj) =
        match obj with
        | :? range as r2 -> code = r2.Code
        | _ -> false

    override r.GetHashCode() = hash code

let memoize (keyFunction: 'a -> 'b) (memFunction: 'a -> 'c) =
    let dict = new System.Collections.Concurrent.ConcurrentDictionary<'b, 'c>()

    fun n ->
        match dict.TryGetValue(keyFunction (n)) with
        | true, v -> v
        | _ ->
            let temp = memFunction (n)
            dict.TryAdd(keyFunction (n), temp) |> ignore
            temp


let mkRangePath (f: string) =
    if Path.IsPathRooted f then
        try
            Path.GetFullPath f
        with _ ->
            f
    else
        f

let mkRangePathMem = memoize id mkRangePath

let mkRange (f: string) b e =
    // remove relative parts from full path
    // let normalizedFilePath = if System.IO.Path.IsPathRooted f then try Path.GetFullPath f with _ -> f else f
    let normalizedFilePath = mkRangePathMem f
    range (fileIndexOfFile normalizedFilePath, b, e)

let mkFileIndexRange fi b e = range (fi, b, e)

(* end representation, start derived ops *)
let orderOn p (pxOrder: IComparer<'U>) =
    { new IComparer<'T> with
        member __.Compare(x, xx) = pxOrder.Compare(p x, p xx) }

let porder (compare1: IComparer<'T1>, compare2: IComparer<'T2>) =
    { new IComparer<'T1 * 'T2> with
        member __.Compare((a1, a2), (aa1, aa2)) =
            let res1 = compare1.Compare(a1, aa1)
            if res1 <> 0 then res1 else compare2.Compare(a2, aa2) }

module Int32 =
    let order = LanguagePrimitives.FastGenericComparer<int>

let posGt (p1: pos) (p2: pos) =
    (p1.Line > p2.Line || (p1.Line = p2.Line && p1.Column > p2.Column))

let posEq (p1: pos) (p2: pos) =
    (p1.Line = p2.Line && p1.Column = p2.Column)

let posGeq p1 p2 = posEq p1 p2 || posGt p1 p2
let posLt p1 p2 = posGt p2 p1

let rangeContainsRange (m1: range) (m2: range) =
    m1.FileIndex = m2.FileIndex && posGeq m2.Start m1.Start && posGeq m1.End m2.End

let rangeContainsPos (m1: range) p = posGeq p m1.Start && posGeq m1.End p

let rangeN filename line =
    mkRange filename (mkPos line 0) (mkPos line 0)

let pos0 = mkPos 1 0
let range0 = rangeN "unknown" 1
let rangeStartup = rangeN "startup" 1
let rangeCmdArgs = rangeN "commandLineArgs" 0

let trimRangeToLine (r: range) =
    let startL, startC = r.StartLine, r.StartColumn
    let endL, _endC = r.EndLine, r.EndColumn

    if endL <= startL then
        r
    else
        let endL, endC =
            startL + 1, 0 (* Trim to the start of the next line (we do not know the end of the current line) *)

        range (r.FileIndex, startL, startC, endL, endC)

(* For Diagnostics *)
let stringOfPos (pos: pos) = sprintf "(%d,%d)" pos.Line pos.Column

let stringOfRange (r: range) =
    sprintf "%s%s-%s" r.FileName (stringOfPos r.Start) (stringOfPos r.End)

#if CHECK_LINE0_TYPES // turn on to check that we correctly transform zero-based line counts to one-based line counts
// Visual Studio uses line counts starting at 0, F# uses them starting at 1
[<Measure>]
type ZeroBasedLineAnnotation

type Line0 = int<ZeroBasedLineAnnotation>
#else
type Line0 = int
#endif
type Pos01 = Line0 * int
type Range01 = Pos01 * Pos01

module Line =
    // Visual Studio uses line counts starting at 0, F# uses them starting at 1
    let fromZ (line: Line0) = int line + 1

    let toZ (line: int) : Line0 =
        LanguagePrimitives.Int32WithMeasure(line - 1)
