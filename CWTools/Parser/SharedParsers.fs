namespace CWTools.Parser

open System.Text
open CWTools.Process
open CWTools.Utilities
open FParsec
open CWTools.Utilities.Position
open Types
open CWTools.Utilities.Utils

module internal SharedParsers =

    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)

    let (<!>) (p: Parser<_, _>) label : Parser<_, _> =
        fun stream ->
            log (sprintf "%A: Entering %s" stream.Position label)
            let reply = p stream
            log (sprintf "%A: Leaving %s (%A)" stream.Position label reply.Status)
            reply

    let betweenL (popen: Parser<_, _>) (pclose: Parser<_, _>) (p: Parser<_, _>) label =
        let notClosedError (pos: FParsec.Position) =
            messageError (sprintf "The %s opened at %s was not closed." label (pos.ToString()))

        let expectedLabel = expected label

        fun (stream: CharStream<_>) ->
            // The following code might look a bit complicated, but that's mainly
            // because we manually apply three parsers in sequence and have to merge
            // the errors when they refer to the same parser state.
            let state0 = stream.State
            let reply1 = popen stream

            if reply1.Status = Ok then
                let stateTag1 = stream.StateTag
                let reply2 = p stream

                let error2 =
                    if stateTag1 <> stream.StateTag then
                        reply2.Error
                    else
                        mergeErrors reply1.Error reply2.Error

                if reply2.Status = Ok then
                    let stateTag2 = stream.StateTag
                    let reply3 = pclose stream

                    let error3 =
                        if stateTag2 <> stream.StateTag then
                            reply3.Error
                        else
                            mergeErrors error2 reply3.Error

                    if reply3.Status = Ok then
                        Reply(Ok, reply2.Result, error3)
                    else
                        Reply(reply3.Status, mergeErrors error3 (notClosedError (state0.GetPosition(stream))))
                else
                    Reply(reply2.Status, reply2.Error)
            else
                let error =
                    if state0.Tag <> stream.StateTag then
                        reply1.Error
                    else
                        expectedLabel

                Reply(reply1.Status, error)

    // Sets of chars
    // =======
    let idCharArray =
        [| '_'
           ':'
           '@'
           '.'
           '\"'
           '-'
           '''
           '['
           ']'
           '!'
           '<'
           '>'
           '$'
           '^'
           '&'
           '|'
           magicChar |]

    let isAnyOfIdCharArray = isAnyOf idCharArray
    let isIdChar = fun c -> isLetter c || isDigit c || isAnyOfIdCharArray c

    let valueCharArray =
        [| '_'
           '.'
           '-'
           ':'
           ';'
           '\''
           '['
           ']'
           '@'
           '''
           '+'
           '`'
           '%'
           '/'
           '!'
           ','
           '<'
           '>'
           '?'
           '$'
           'š'
           'Š'
           '’'
           '|'
           '^'
           '*'
           '&'
           '('
           ')'
           magicChar |]

    let isAnyValueChar = isAnyOf valueCharArray
    let isValueChar = fun c -> isLetter c || isDigit c || isAnyValueChar c

    // Utility parsers
    // =======
    let ws = spaces <?> "whitespace"
    let str s = pstring s .>> ws <?> ("string " + s)

    let strSkip s =
        skipString s .>> ws <?> ("skip string " + s)

    let ch c = pchar c .>> ws <?> ("char " + string c)

    let chSkip c =
        skipChar c .>> ws <?> ("skip char " + string c)

    let clause inner =
        betweenL (chSkip '{' <?> "opening brace") (skipChar '}' <?> "closing brace") inner "clause"

    let quotedCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = (stringReturn "\\\"" "\"" <|> pstring "\\")
    let metaprogrammingCharSnippet = many1Satisfy (fun c -> c <> ']' && c <> '\\')

    let getRange (start: FParsec.Position) (endp: FParsec.Position) =
        mkRange
            start.StreamName
            (mkPos (int start.Line) (int start.Column - 1))
            (mkPos (int endp.Line) (int endp.Column - 1))

    let parseWithPosition p =
        pipe3 getPosition p getPosition (fun s r e -> getRange s e, r)

    // Base types
    // =======
    let oppLTE = skipString "<=" >>% Operator.LessThanOrEqual
    let oppGTE = skipString ">=" >>% Operator.GreaterThanOrEqual
    let oppNE = skipString "!=" >>% Operator.NotEqual
    let oppEE = skipString "==" >>% Operator.EqualEqual
    let oppQE = skipString "?=" >>% Operator.QuestionEqual
    let oppLT = skipChar '<' >>% Operator.LessThan
    let oppGT = skipChar '>' >>% Operator.GreaterThan
    let oppE = skipChar '=' >>% Operator.Equals

    let operator =
        choiceL [ oppLTE; oppGTE; oppNE; oppEE; oppLT; oppGT; oppE; oppQE ] "operator"
        .>> ws

    let operatorLookahead =
        choice [ chSkip '='; chSkip '>'; chSkip '<'; chSkip '!'; strSkip "?=" ]
        <?> "operator 1"

    let comment =
        parseWithPosition (skipChar '#' >>. restOfLine true .>> ws) <?> "comment"

    let key = (many1SatisfyL isIdChar "id character") .>> ws |>> Key <?> "id"

    let keyQStr =
        between (ch '"') (ch '"') (manyStrings (quotedCharSnippet <|> escapedChar))
        .>> ws
        |>> (fun s -> "\"" + s + "\"")
        |>> Key
        <?> "quoted key"

    let valueStr =
        (many1SatisfyL isValueChar "value character")
        |>> (fun x -> StringResource.stringManager.InternIdentifierToken x)
        |>> String
        <?> "string"

    let valueQStr =
        betweenL (skipChar '"') (skipChar '"') (manyStrings (quotedCharSnippet <|> escapedChar)) "quoted string"
        |>> (fun x -> StringResource.stringManager.InternIdentifierToken x)
        |>> QString
        <?> "quoted string"

    // "yes" and "no" are case-sensitive.
    let valueBYes = skipString "yes" .>> nextCharSatisfiesNot isValueChar >>% YesBool

    let valueBNo = skipString "no" .>> nextCharSatisfiesNot isValueChar >>% NoBool

    let valueInt = pint64 .>> nextCharSatisfiesNot isValueChar |>> int |>> Int
    let valueFloat = pfloat .>> nextCharSatisfiesNot isValueChar |>> decimal |>> Float

    let hsvI =
        clause (
            pipe4
                (parseWithPosition valueFloat .>> ws)
                (parseWithPosition valueFloat .>> ws)
                (parseWithPosition valueFloat .>> ws)
                (opt (parseWithPosition valueFloat .>> ws))
                (fun a b c d ->
                    match (a, b, c, d) with
                    | a, b, c, Some d ->
                        Clause [ Statement.Value a; Statement.Value b; Statement.Value c; Statement.Value d ]
                    | a, b, c, None -> Clause [ Statement.Value a; Statement.Value b; Statement.Value c ])
        )

    let hsv = strSkip "hsv" >>. opt (strSkip "360") >>. hsvI .>> ws
    let hsvC = strSkip "HSV" >>. hsvI .>> ws

    let rgbI =
        clause (
            pipe4
                (parseWithPosition valueInt .>> ws)
                (parseWithPosition valueInt .>> ws)
                (parseWithPosition valueInt .>> ws)
                (opt (parseWithPosition valueInt .>> ws))
                (fun a b c d ->
                    match (a, b, c, d) with
                    | a, b, c, Some d ->
                        Clause [ Statement.Value a; Statement.Value b; Statement.Value c; Statement.Value d ]
                    | a, b, c, None -> Clause [ Statement.Value a; Statement.Value b; Statement.Value c ])
        )

    let rgb = strSkip "rgb" >>. rgbI .>> ws
    let rgbC = strSkip "RGB" >>. rgbI .>> ws

    let metaprograming =
        pipe3 (pstring "@\\[") metaprogrammingCharSnippet (ch ']') (fun a b c -> (a + b + string c))
        |>> (fun x -> StringResource.stringManager.InternIdentifierToken x)
        |>> String
    // Complex types
    // =======

    // Recursive types
    let keyValue, keyvalueimpl = createParserForwardedToRef ()
    let (value: Parser<Value, unit>), valueimpl = createParserForwardedToRef ()

    let leafValue =
        pipe3 getPosition (value .>> ws) getPosition (fun a b c -> (getRange a c, b))

    let statement =
        comment |>> fun (p, c) -> CommentStatement({ Position = p; Comment = c })
        <|> (attempt (leafValue .>> notFollowedBy operatorLookahead |>> Value))
        <|> keyValue
        <?> "statement"

    let valueClause = clause (many statement) |>> Clause

    let valueCustom: Parser<Value, unit> =
        let vcP = valueClause
        let iP = attempt valueInt
        let fP = attempt valueFloat
        let byP = attempt valueBYes <|> valueStr
        let bnP = attempt valueBNo <|> valueStr
        let mpP = metaprograming

        fun (stream: CharStream<_>) ->
            match stream.Peek() with
            | '{' -> vcP stream
            | '"' -> valueQStr stream
            | x when isDigit x || x = '-' ->
                let i = (iP stream)

                if i.Status = Ok then
                    i
                else
                    let f = (fP stream)
                    if f.Status = Ok then f else valueStr stream
            | _ ->
                match stream.PeekString 3, stream.PeekString 2 with
                | "rgb", _ -> rgb stream
                | "RGB", _ -> rgbC stream
                | "hsv", _ -> hsv stream
                | "HSV", _ -> hsvC stream
                | "yes", _ -> byP stream
                | _, "no" -> bnP stream
                | "@\\[", _ -> mpP stream
                | _ -> valueStr stream

    valueimpl.Value <- valueCustom <?> "value"

    keyvalueimpl.Value <-
        pipe5 getPosition (keyQStr <|> key) operator value (getPosition .>> ws) (fun start id op value endp ->
            KeyValue(PosKeyValue(getRange start endp, KeyValueItem(id, value, op))))

    let alle = ws >>. many statement .>> eof |>> ParsedFile

    let valueList =
        many1 (
            (comment |>> fun (p, c) -> CommentStatement({ Position = p; Comment = c }))
            <|> (leafValue |>> Value)
        )
        .>> eof

    let statementList = (many statement) .>> eof
    let all = ws >>. ((attempt valueList) <|> statementList)
