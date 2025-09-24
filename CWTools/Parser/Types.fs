namespace CWTools.Parser

open System.Runtime.CompilerServices
open CWTools.Process
open CWTools.Utilities
open CWTools.Utilities.Position
open FParsec
open System.Globalization

[<AutoOpen>]
module Types =
    [<Struct; IsReadOnly>]
    type Position =
        | Position of FParsec.Position

        override x.ToString() =
            let (Position p) = x in sprintf "Position (Ln: %i, Pos: %i, File: %s)" p.Line p.Column p.StreamName

        static member Empty = Position(FParsec.Position("none", 0L, 0L, 0L))

        static member File(fileName) =
            Position(FParsec.Position(fileName, 0L, 0L, 0L))

        static member Conv(pos: FParsec.Position) = Position pos
        static member UnConv(pos: Position) = let (Position p) = pos in p

    type Operator =
        | Equals = 0uy
        | GreaterThan = 1uy
        | LessThan = 2uy
        | GreaterThanOrEqual = 3uy
        | LessThanOrEqual = 4uy
        | NotEqual = 5uy
        | EqualEqual = 6uy
        | QuestionEqual = 7uy

    let operatorToString =
        function
        | Operator.Equals -> "="
        | Operator.GreaterThan -> ">"
        | Operator.LessThan -> "<"
        | Operator.GreaterThanOrEqual -> ">="
        | Operator.LessThanOrEqual -> "<="
        | Operator.NotEqual -> "!="
        | Operator.EqualEqual -> "=="
        | Operator.QuestionEqual -> "?="
        | x -> failwith (sprintf "Unknown enum value %A" x)

    [<Struct; IsReadOnly>]
    type Key =
        | Key of string

        override x.ToString() = let (Key v) = x in sprintf "%s" v

    [<Struct; IsReadOnly>]
    type KeyValueItem =
        | KeyValueItem of Key * Value * Operator

        override x.ToString() =
            let (KeyValueItem(id, v, op)) = x in sprintf "%O %s %O" id (operatorToString op) v

    and Value =
        | String of StringTokens
        | QString of StringTokens
        | Float of decimal
        | Int of int
        | Bool of bool
        | Clause of Statement list
        
        static member CreateString(s: string) =
            String(StringResource.stringManager.InternIdentifierToken(s))

        override x.ToString() =
            match x with
            | Clause b -> "{ " + $"{b}" + " }"
            | QString s -> "\"" + StringResource.stringManager.GetStringForIDs s + "\""
            | String s -> StringResource.stringManager.GetStringForIDs s
            | Bool b -> if b then "yes" else "no"
            | Float f -> f.ToString(CultureInfo.InvariantCulture)
            | Int i -> $"%i{i}"


        member x.ToRawString() =
            match x with
            | Clause b -> "{ " + $"{b}" + " }"
            | QString s -> StringResource.stringManager.GetStringForIDs s
            | String s -> StringResource.stringManager.GetStringForIDs s
            | Bool b -> if b then "yes" else "no"
            | Float f -> f.ToString(CultureInfo.InvariantCulture)
            | Int i -> $"%i{i}"

        member x.ToStringId() =
            match x with
            | String stringTokens -> stringTokens
            | QString stringTokens -> stringTokens
            | _ -> StringResource.stringManager.InternIdentifierToken(x.ToString())

    and
        [<CustomEquality; NoComparison; Struct; IsReadOnly>] PosKeyValue =
        | PosKeyValue of range * KeyValueItem

        override x.Equals(y) =
            match y with
            | :? PosKeyValue as y ->
                let (PosKeyValue(_, k)) = x
                let (PosKeyValue(_, k2)) = y
                k = k2
            | _ -> false

        override x.GetHashCode() =
            let (PosKeyValue(_, k)) = x in k.GetHashCode()

    and [<CustomEquality; NoComparison>] Statement =
        | CommentStatement of Comment
        | KeyValue of PosKeyValue
        | Value of range * Value

        override x.Equals(y) =
            match y with
            | :? Statement as y ->
                match x, y with
                | CommentStatement comment1, CommentStatement comment2 -> comment1 = comment2
                | KeyValue kv1, KeyValue kv2 -> kv1 = kv2
                | Value(r1, v1), Value(r2, v2) -> r1 = r2 && v1 = v2
                | _ -> false
            | _ -> false

        override x.GetHashCode() =
            match x with
            | CommentStatement comment -> comment.GetHashCode()
            | KeyValue kv -> kv.GetHashCode()
            | Value(r, v) -> v.GetHashCode()

    let YesBool = Bool true
    let NoBool = Bool false
    
    [<StructuralEquality; NoComparison>]
    type ParsedFile = ParsedFile of Statement list

    type ParseFile = string -> ParserResult<ParsedFile, unit>
    type ParseString = string -> string -> ParserResult<ParsedFile, unit>
    type PrettyPrintFile = ParsedFile -> string
    type PrettyPrintStatements = Statement list -> string
    type PrettyPrintStatement = Statement -> string
    type PrettyPrintFileResult = ParserResult<ParsedFile, unit> -> string

    type ParserAPI =
        { parseFile: ParseFile
          parseString: ParseString }

    type PrinterAPI =
        { prettyPrintFile: PrettyPrintFile
          prettyPrintStatements: PrettyPrintStatements
          prettyPrintStatement: PrettyPrintStatement
          prettyPrintFileResult: PrettyPrintFileResult }
