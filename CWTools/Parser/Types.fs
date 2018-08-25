namespace CWTools.Parser
open CWTools.Utilities.Position
open FParsec

module rec Types =
    [<Struct>]
    type Position = Position of FParsec.Position with
        override x.ToString() = let (Position p) = x in sprintf "Position (Ln: %i, Pos: %i, File: %s)" p.Line p.Column p.StreamName
        static member Empty = Position (FParsec.Position("none", 0L, 0L, 0L))
        static member File(fileName) = Position (FParsec.Position(fileName, 0L, 0L, 0L))
        static member Conv(pos : FParsec.Position) = Position (pos)
        static member UnConv(pos : Position) = let (Position p) = pos in p
    [<Struct>]
    type Key =
        | Key of string
        override x.ToString() = let (Key v) = x in sprintf "%s" v;

    let valueToString =
        let inner =
                fun x ->
                match x with
                | Clause b -> "{ " + sprintf "%O" b + " }"
                | QString s -> "\"" + s + "\""
                | String s -> s
                | Bool b -> if b then "yes" else "no"
                | Float f -> sprintf "%A" f
                | Int i -> sprintf "%A" i
        //memoize id inner
        inner
    [<Struct>]
    type KeyValueItem =
        | KeyValueItem of Key * Value
        override x.ToString() = let (KeyValueItem (id, v)) = x in sprintf "%O = %O" id v
    and Value =
        | String of string
        | QString of string
        | Float of float
        | Int of int
        | Bool of bool
        | Clause of Statement list
        override x.ToString() = valueToString x

        member x.ToRawString() =
            match x with
            | Clause b -> "{ " + sprintf "%O" b + " }"
            | QString s -> s
            | String s -> s
            | Bool b -> if b then "yes" else "no"
            | Float f -> sprintf "%A" f
            | Int i -> sprintf "%A" i
    and [<CustomEquality; NoComparison; Struct>] PosKeyValue  =
        | PosKeyValue of range * KeyValueItem
        override x.Equals(y) =
            match y with
            | :? PosKeyValue as y ->
                let (PosKeyValue(_, k)) = x
                let (PosKeyValue(_, k2)) = y
                k = k2
            | _ -> false
        override x.GetHashCode() = let (PosKeyValue(_, k)) = x in k.GetHashCode()
        // interface System.IComparable with
        //     member x.CompareTo(y) =
        //         match y with
        //         | :? PosKeyValue as y ->
        //             let (PosKeyValue(_, k)) = x
        //             let (PosKeyValue(_, k2)) = y
        //             compare k k2
        //         | _ -> failwith "wrong type"

    and [<CustomEquality; NoComparison>] Statement =
        | Comment of string
        | KeyValue of PosKeyValue
        | Value of range * Value
        override x.Equals(y) =
            match y with
            | :? Statement as y ->
                match x, y with
                | Comment s1, Comment s2 -> s1 = s2
                | KeyValue kv1, KeyValue kv2 -> kv1 = kv2
                | Value (r1, v1), Value (r2, v2) -> r1 = r2 && v1 = v2
                | _ -> false
            | _ -> false
        override x.GetHashCode() =
            match x with
            |Comment c -> c.GetHashCode()
            |KeyValue kv -> kv.GetHashCode()
            |Value (r, v) -> v.GetHashCode()
        // interface System.IComparable with
        //     member x.CompareTo(y) =
        //         match y with
        //         | :? Statement as y ->
        //             match x, y with
        //             | Comment s1, Comment s2 -> s1.CompareTo(s2)
        //             | KeyValue kv1, KeyValue kv2 -> kv1.CompareTo(kv2)
        //             | Value (r1, v1), Value (r2, v2) -> r1.CompareTo(r2) + v1.CompareTo(v2)//r1 = r2 && v1 = v2
        //             | _ -> false
        //         | _ -> false

                // match y with
                // | :? PosKeyValue as y ->
                //     let (PosKeyValue(_, k)) = x
                //     let (PosKeyValue(_, k2)) = y
                //     compare k k2
                // | _ -> failwith "wrong type"




    [<StructuralEquality; NoComparison>]
    type ParsedFile = |ParsedFile of Statement list

    type ParseFile = string -> ParserResult<ParsedFile, unit>
    type ParseString = string -> string -> ParserResult<ParsedFile, unit>
    type PrettyPrintFile = ParsedFile -> string
    type PrettyPrintStatements = Statement list -> string
    type PrettyPrintFileResult = ParserResult<ParsedFile, unit> -> string

    type ParserAPI =
        {
            parseFile : ParseFile
            parseString : ParseString
        }

    type PrinterAPI =
        {
            prettyPrintFile : PrettyPrintFile
            prettyPrintStatements : PrettyPrintStatements
            prettyPrintFileResult : PrettyPrintFileResult
        }

