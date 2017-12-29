namespace CWTools.Parser

open FParsec


type Key =
    | Key of string
    override x.ToString() = let (Key v) = x in sprintf "%s" v  

type KeyValueItem = 
    | KeyValueItem of Key * Value
    override x.ToString() = let (KeyValueItem (id, v)) = x in sprintf "%O = %O" id v
and Value =
    | String of string
    | QString of string
    | Float of float
    //| Int of int
    | Bool of bool
    | Clause of Statement list
    override x.ToString() =
        match x with
        | Clause b -> "{ " + sprintf "%O" b + " }"
        | QString s -> "\"" + s + "\""
        | String s -> s
        | Bool b -> if b then "yes" else "no"
        | Float f -> sprintf "%A" f
        //| Int i -> sprintf "%A" i

and Statement =
    | Comment of string
    | KeyValue of KeyValueItem
    | Value of Value
[<StructuralEquality; StructuralComparison>]
type EventFile = |EventFile of  Statement list

type ParseFile = string -> ParserResult<EventFile, unit>
type ParseString = string -> string -> ParserResult<EventFile, unit>
type PrettyPrintFile = EventFile -> string
type PrettyPrintStatements = Statement list -> string
type PrettyPrintFileResult = ParserResult<EventFile, unit> -> string

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


module CKParser =

    // Sets of chars
    // =======
    let whitespaceTextChars = " \t\r\n"
    let idchar = letter <|> digit <|> anyOf ['_'; ':'; '@'; '.'; '\"']
    let valuechar = letter <|> digit <|> anyOf ['_'; '.'; '-'; ':'; '\''; '['; ']'; '@']


    // Utility parsers
    // =======
    let ws = (skipMany spaces1 |>> ignore) <?> "whitespace"
    let str s = pstring s .>> ws <?> ("string " + s)
    let strSkip s = skipString s .>> ws <?> ("skip string " + s)
    let ch c = pchar c .>> ws <?> ("char " + string c)
    let chSkip c = skipChar c .>> ws <?> ("skip char " + string c)
    let clause inner = between (chSkip '{') (chSkip '}') inner

    // Base types
    // =======
    let operator = (attempt (strSkip ">=")) <|> (attempt (strSkip "<=")) <|> (attempt (strSkip "==")) <|> chSkip '=' <|> chSkip '<' <|> chSkip '>' <?> "operator"

    let comment = skipChar '#' >>. manyCharsTill anyChar ((newline |>> ignore) <|> eof) .>> ws |>> string <?> "comment"

    let key = (many1Chars idchar) .>> ws |>> Key <?> "id"

    let valueS = (many1Chars valuechar) .>> ws |>> string |>> String <?> "string"

    let quotedCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\\"" |>> string
    let valueQ = between (ch '"') (ch '"') (manyStrings (quotedCharSnippet <|> escapedChar)) |>> QString <?> "quoted string"

    let valueB = ( (skipString "yes") .>> notFollowedBy (valuechar) .>> ws  |>> (fun _ -> Bool(true))) <|>
                    ((skipString "no") .>> notFollowedBy (valuechar) .>> ws  |>> (fun _ -> Bool(false)))

    let valueI = puint64 .>> ws |>> int
                    

    // Complex types
    // =======

    // Recursive types
    let keyvalue, keyvalueimpl = createParserForwardedToRef()
    let value, valueimpl = createParserForwardedToRef()

    let statement = comment |>> Comment <|> keyvalue <?> "statement"
    let valueBlock = clause (many1 ((value |>> Value) <|> (comment |>> Comment))) |>> Clause <?> "value clause"
    
    let valueClause = clause (many statement) |>> Clause <?> "statement clause"

    do valueimpl := valueQ <|> (attempt valueBlock) <|> valueClause <|> (attempt valueB) <|> valueS <?> "value"
    
    do keyvalueimpl := pipe2 (key .>> operator) (value) (fun id value -> KeyValue(KeyValueItem(id, value)))
    let all = ws >>. many statement .>> eof |>> (fun f -> (EventFile f : EventFile))

    let parseEventFile filepath = runParserOnFile all () filepath (System.Text.Encoding.GetEncoding(1252))


    let memoize keyFunction memFunction =
        let dict = new System.Collections.Generic.Dictionary<_,_>()
        fun n ->
            match dict.TryGetValue(keyFunction(n)) with
            | (true, v) -> v
            | _ ->
                let temp = memFunction(n)
                dict.Add(keyFunction(n), temp)
                temp

    let parseEventString fileString fileName =
        let inner = (fun (file, name) -> runParserOnString all () name file)
        let hash = (fun (file, name) -> file.GetHashCode(), name)
        (memoize hash inner) (fileString, fileName)

    let api =
        {
            parseFile = parseEventFile
            parseString = parseEventString
        }

module CKPrinter =
    let tabs n = String.replicate n "\t"

    let printTroop depth t = (tabs depth) + t.ToString()  + "\n"
    let printValuelist depth is =
        let printOne = (fun i -> tabs (depth) + (string i) + "\n")
        List.map printOne is |> List.fold (+) ""
        

    let rec printValue v depth =
        match v with
        | Clause kvl -> "{ \n" + printKeyValueList kvl (depth + 1) + tabs (depth + 1) + " }\n"
        | x -> x.ToString() + "\n"
    and printKeyValue kv depth =
        match kv with
        | Comment c -> (tabs depth) + "#" + c + "\n"
        | KeyValue (KeyValueItem (key, v)) -> (tabs depth) + key.ToString() + " = " + (printValue v depth)
        | Value v -> (tabs depth) + (printValue v depth)
    and printKeyValueList kvl depth =
        kvl |> List.map (fun kv -> printKeyValue kv depth) |> List.fold (+) ""
    let prettyPrint ef =
        let (EventFile sl) = ef
        printKeyValueList sl 0 
    let prettyPrintResult =
        function
        | Success (v,_,_) -> 
            let (EventFile ev) = v
            printKeyValueList ev 0
        | Failure (msg, _, _) -> msg

    let api = 
        {
        prettyPrintFile = prettyPrint
        prettyPrintStatements = (fun f -> printKeyValueList f 0)
        prettyPrintFileResult = prettyPrintResult
        }