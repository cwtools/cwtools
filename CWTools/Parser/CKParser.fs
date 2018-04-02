namespace CWTools.Parser

    
open FParsec
open System.IO

type Position = Position of FParsec.Position with
    override x.ToString() = let (Position p) = x in sprintf "Position (Ln: %i, Pos: %i, File: %s)" p.Line p.Column p.StreamName
    static member Empty = Position (FParsec.Position("none", 0L, 0L, 0L))
    static member File(fileName) = Position (FParsec.Position(fileName, 0L, 0L, 0L))
    static member Conv(pos : FParsec.Position) = Position (pos)
    static member UnConv(pos : Position) = let (Position p) = pos in p

type Key =
    | Key of string
    override x.ToString() = let (Key v) = x in sprintf "%s" v;  

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
    override x.ToString() =
        match x with
        | Clause b -> "{ " + sprintf "%O" b + " }"
        | QString s -> "\"" + s + "\""
        | String s -> s
        | Bool b -> if b then "yes" else "no"
        | Float f -> sprintf "%A" f
        | Int i -> sprintf "%A" i
    member x.ToRawString() =
        match x with
        | Clause b -> "{ " + sprintf "%O" b + " }"
        | QString s -> s
        | String s -> s
        | Bool b -> sprintf "%A" b
        | Float f -> sprintf "%A" f
        | Int i -> sprintf "%A" i
and [<CustomEquality; CustomComparison>] PosKeyValue  = 
    | PosKeyValue of Position * KeyValueItem
    override x.Equals(y) =
        match y with
        | :? PosKeyValue as y ->
            let (PosKeyValue(_, k)) = x
            let (PosKeyValue(_, k2)) = y
            k = k2
        | _ -> false
    override x.GetHashCode() = let (PosKeyValue(_, k)) = x in k.GetHashCode()
    interface System.IComparable with
        member x.CompareTo(y) =
            match y with
            | :? PosKeyValue as y ->
                let (PosKeyValue(_, k)) = x
                let (PosKeyValue(_, k2)) = y
                compare k k2
            | _ -> failwith "wrong type"

and Statement =
    | Comment of string
    | KeyValue of PosKeyValue
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

    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply

    let betweenL (popen: Parser<_,_>) (pclose: Parser<_,_>) (p: Parser<_,_>) label =
        let expectedLabel = expected label
        let notClosedError (pos: FParsec.Position) =
         messageError (sprintf "The %s opened at %s was not closed."
                               label (pos.ToString()))
        fun (stream: CharStream<_>) ->
        // The following code might look a bit complicated, but that's mainly
        // because we manually apply three parsers in sequence and have to merge
        // the errors when they refer to the same parser state.
        let state0 = stream.State
        let reply1 = popen stream
        if reply1.Status = Ok then
          let stateTag1 = stream.StateTag
          let reply2 = p stream
          let error2 = if stateTag1 <> stream.StateTag then reply2.Error
                       else mergeErrors reply1.Error reply2.Error
          if reply2.Status = Ok then
            let stateTag2 = stream.StateTag
            let reply3 = pclose stream
            let error3 = if stateTag2 <> stream.StateTag then reply3.Error
                         else mergeErrors error2 reply3.Error
            if reply3.Status = Ok then
              Reply(Ok, reply2.Result, error3)
            else
              Reply(reply3.Status,
                    mergeErrors error3 (notClosedError (state0.GetPosition(stream))))
          else
            Reply(reply2.Status, reply2.Error)
        else
          let error = if state0.Tag <> stream.StateTag then reply1.Error
                      else expectedLabel
          Reply(reply1.Status, error)

    // Sets of chars
    // =======
    let whitespaceTextChars = " \t\r\n"
    let norseChars =['ö';'ð';'æ';'ó';'ä';'Þ';'Å';'Ö']
    let idchar = choiceL [letter; digit; anyOf ['_'; ':'; '@'; '.'; '\"'; '-'; ''']] "id character"
    let valuechar = choiceL [letter; digit; anyOf (['_'; '.'; '-'; ':'; '\''; '['; ']'; '@';'''; '+'; '`'; '%'; '/'; '!'] @ ['š'; 'Š'; '’'])] "value character"


    // Utility parsers
    // =======
    let ws = (optional spaces1) <?> "whitespace"
    let str s = pstring s .>> ws <?> ("string " + s)
    let strSkip s = skipString s .>> ws <?> ("skip string " + s)
    let ch c = pchar c .>> ws <?> ("char " + string c)
    let chSkip c = skipChar c .>> ws <?> ("skip char " + string c)
    let clause inner = betweenL (chSkip '{' <?> "opening brace") (chSkip '}' <?> "closing brace") inner "clause"
    let quotedCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\\"" |>> string

    // Base types
    // =======
    let operator = choice [chSkip '='; chSkip '>'; chSkip '<'] >>. optional (chSkip '=') <?> "operator"

    let comment = skipChar '#' >>. manyCharsTill anyChar ((newline |>> ignore) <|> eof) .>> ws |>> string <?> "comment"

    let key = (many1Chars idchar) .>> ws |>> Key <?> "id"
    let keyQ =  between (ch '"') (ch '"') (manyStrings (quotedCharSnippet <|> escapedChar)) .>> ws |>> Key <?> "quoted key"

    let valueS = (many1Chars valuechar) .>> ws |>> string |>> String <?> "string"

    let valueQ = between (ch '"') (ch '"') (manyStrings (quotedCharSnippet <|> escapedChar)) |>> QString <?> "quoted string"

    let valueB = ( (skipString "yes") .>> notFollowedBy (valuechar) .>> ws  |>> (fun _ -> Bool(true))) <|>
                    ((skipString "no") .>> notFollowedBy (valuechar) .>> ws  |>> (fun _ -> Bool(false)))
    let valueBYes = skipString "yes" .>> notFollowedBy (valuechar) .>> ws |>> (fun _ -> Bool(true))
    let valueBNo = skipString "no" .>> notFollowedBy (valuechar) .>> ws |>> (fun _ -> Bool(false))

    let valueI = pint64 .>> notFollowedBy (valuechar) .>> ws |>> int |>> Int
    let valueF = pfloat .>> notFollowedBy (valuechar) .>> ws |>> float |>> Float     

    let hsv3 = clause (pipe3 valueF valueF valueF (fun a b c -> Clause [Statement.Value a;Statement.Value b; Statement.Value c]))
    let hsv4 = clause (pipe4 valueF valueF valueF valueF (fun a b c d -> Clause [Statement.Value a;Statement.Value b; Statement.Value c; Statement.Value d]))
    let hsvI = 
        clause (pipe4 valueF valueF valueF (opt valueF) 
            (fun a b c d -> 
            match (a, b, c, d) with 
            | (a, b, c, (Some d)) -> Clause [Statement.Value a;Statement.Value b; Statement.Value c; Statement.Value d]
            | (a, b, c, None) -> Clause [Statement.Value a;Statement.Value b; Statement.Value c;]))
    let hsv = strSkip "hsv" >>. hsvI
    let rgbI = clause (pipe4 valueI valueI valueI (opt valueI) 
            (fun a b c d -> 
            match (a, b, c, d) with 
            | (a, b, c, (Some d)) -> Clause [Statement.Value a;Statement.Value b; Statement.Value c; Statement.Value d]
            | (a, b, c, None) -> Clause [Statement.Value a;Statement.Value b; Statement.Value c;]))


    let rgb3 = clause (pipe3 valueI valueI valueI (fun a b c -> Clause [Statement.Value a;Statement.Value b; Statement.Value c])) 
    let rgb4 = clause (pipe4 valueI valueI valueI valueI (fun a b c d -> Clause [Statement.Value a;Statement.Value b; Statement.Value c; Statement.Value d])) 
    let rgb = strSkip "rgb" >>. rgbI

    // Complex types
    // =======

    // Recursive types
    let keyvalue, keyvalueimpl = createParserForwardedToRef()
    let value, valueimpl = createParserForwardedToRef()

    let statement = comment |>> Comment <|> keyvalue <?> "statement"
    let valueBlock = clause (many1 ((value |>> Value) <|> (comment |>> Comment))) |>> Clause <?> "value clause"
    
    let valueClause = clause (many statement) |>> Clause <?> "statement clause"

    let valueCustom : Parser<Value, unit> =
        let vcP = attempt valueClause
        let vbP = attempt valueBlock
        let iP = attempt valueI
        let fP = attempt valueF
        let byP = attempt valueBYes <|> valueS
        let bnP = attempt valueBNo <|> valueS
        fun (stream: CharStream<_>) ->
            match stream.Peek() with
            | '{' -> 
                let vc = (vcP stream)
                if vc.Status = Ok then vc else
                    let vb = (vbP stream)
                    if vb.Status <> Ok then valueClause stream else vb
                // let vb = (attempt valueBlock stream)
                // if vb.Status = Ok then vb else valueClause stream
            | '"' -> valueQ stream
            | x when isDigit x -> 
                let i = (iP stream)
                if i.Status = Ok then i else
                    let f = (fP stream)
                    if f.Status = Ok then f else
                    valueS stream
            | _ -> 
                match stream.PeekString 3, stream.PeekString 2 with
                | "rgb", _ -> rgb stream
                | "hsv", _ -> hsv stream
                | "yes", _ -> byP stream
                | _, "no" -> bnP stream
                | _ -> valueS stream
                //| _ -> choice [(attempt valueB); valueS] stream
                //choiceL [(attempt valueB); (attempt hsv); (attempt rgb); valueS] "value" stream

    

    do valueimpl := valueCustom
        //choiceL [valueQ; (attempt valueB); (attempt valueI); (attempt valueF); (attempt hsv); (attempt rgb); (attempt valueS); (attempt valueBlock); valueClause;] "value"
    
    do keyvalueimpl := pipe3 (getPosition) ((keyQ <|> key) .>> operator) (value) (fun pos id value -> KeyValue(PosKeyValue(Position pos, KeyValueItem(id, value))))
    let alle = ws >>. many statement .>> eof |>> (fun f -> (EventFile f : EventFile))
    let valuelist = many1 ((comment |>> Comment) <|> (value |>> Value)) .>> eof
    let statementlist = (many statement) .>> eof
    let all = ws >>. ((attempt valuelist) <|> statementlist)
    let parseEventFile filepath = runParserOnFile alle () filepath (System.Text.Encoding.GetEncoding(1252))

    let internal applyParser (parser: Parser<'Result,'UserState>) (stream: CharStream<'UserState>) =
        let reply = parser stream
        if reply.Status = Ok then
            Success(reply.Result, stream.UserState, stream.Position)
        else
            let error = ParserError(stream.Position, stream.UserState, reply.Error)
            Failure(error.ToString(stream), error, stream.UserState)

    let parseFile (filepath : string) =
        let stream = new CharStream<unit>(filepath, System.Text.Encoding.GetEncoding(1252))
        stream.UserState <- ()
        stream.Name <- filepath
        applyParser all stream


    let memoize keyFunction memFunction =
        let dict = new System.Collections.Generic.Dictionary<_,_>()
        fun n ->
            match dict.TryGetValue(keyFunction(n)) with
            | (true, v) -> v
            | _ ->
                let temp = memFunction(n)
                dict.Add(keyFunction(n), temp)
                temp
    let parseString fileString filename = runParserOnString all () filename fileString
        // let inner = (fun (file, name) -> runParserOnString all () name file)
        // let hash = (fun (file, name) -> file.GetHashCode(), name)
        // (memoize hash inner) (fileString, filename)
    let parseEventString fileString fileName =
        let inner = (fun (file, name) -> runParserOnString alle () name file)
        let hash = (fun (file, name) -> file.GetHashCode(), name)
        (memoize hash inner) (fileString, fileName)

    let getSuccess (result) = 
        match result with
        |Success(s, _, _) -> s
        |_ -> EventFile []

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
        | Clause kvl -> "{ \n" + printKeyValueList kvl (depth + 1) + tabs (depth) + " }\n"
        | x -> x.ToString() + "\n"
    and printKeyValue kv depth =
        match kv with
        | Comment c -> (tabs depth) + "#" + c + "\n"
        | KeyValue (PosKeyValue(_, KeyValueItem (key, v))) -> (tabs depth) + key.ToString() + " = " + (printValue v depth)
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