namespace CK2_Events.Application

open FParsec

module CKParser =
    type ID =
        | ID of string
        override x.ToString() = let (ID v) = x in sprintf "%s" v  

    type KeyValueItem = 
        | KeyValueItem of ID * Value
        override x.ToString() = let (KeyValueItem (id, v)) = x in sprintf "%O = %O" id v
    and Value =
        | String of string
        | Float of float
        | Bool of bool
        | Block of Statement list
        override x.ToString() =
            match x with
            | Block b -> "{ " + sprintf "%O" b + " }"
            | x -> sprintf "%A" x
    
    and Statement =
        | Comment of string
        | KeyValue of KeyValueItem
    
    type EventFile = Statement list
    let whitespaceTextChars = " \t\r\n"

    //let spaceAsStr = anyOf whitespaceTextChars |>> fun chr -> string chr
    let ws = (many spaces1 |>> ignore) <?> "whitespace"
    let comment = skipChar '#' >>. manyCharsTill anyChar newline .>> ws |>> string |>> Comment
    let wsc = (many (comment |>> ignore <|> spaces1) |>> ignore) <?> "whitespace_comment"
    let str s = pstring s .>> ws
    let ch c = skipChar c >>. ws
    let id =
        let idchar = asciiLetter <|> digit <|> pchar '_' <|> pchar ':'
        (many1Chars idchar) .>> ws |>> ID <?> "id"
    //let comment = pchar '#' >>. restOfLine true


    //let skipWhitespace = skipMany1 ws

    //let namespaceP = pstring "namespace = " >>. restOfLine false .>> ws
    let valueS = 
        let valuechar = asciiLetter <|> digit <|> pchar '_' <|> pchar '.' <|> pchar '-' <|> pchar '.' <|> pchar ':'
        (many1Chars valuechar) .>> ws |>> string |>> String <?> "valueS"

    let valueQ = 
        between (ch '"') (ch '"') (manyChars (noneOf "\"")) |>> string |>> String <?> "valueQ"
//    restOfLine false .>> ws |>> String <?> "valueS"
    //let valueBlock = ch '{' >>. inner .>> ch '}'
    let keyvalue, keyvalueimpl = createParserForwardedToRef()
    let keyvaluelist = many (comment <|> keyvalue)
    let valueBlock = 
        let list = keyvaluelist |>> Block
        between (ch '{') (ch '}') list <?> "valueBlock"
    let value = valueQ <|> valueBlock <|> valueS <?> "value"
    do keyvalueimpl := 
        pipe2 (id .>> ch '=') (value)
            (fun id value -> KeyValue(KeyValueItem(id, value)))
    
    let all = ws >>. keyvaluelist .>> eof |>> (fun f -> (f : EventFile))

    //let line = (many (attempt comment)) >>. restOfLine false
    //let keyvalueS key = str key >>. ch '=' >>. valueS
    //let keyvalueB key inner = str key >>. ch '=' >>. valueBlock inner
   
    //let namespaceP = keyvalueS "namespace"
    //let event = keyvalueS "id" .>>. keyvalueS "is_triggered_only" .>>. keyvalueS "hide_window"
    //let character_event = keyvalueB "character_event" event
    //let character_event = str "character_event" >>. ch '=' >>. ch '{' >>. event .>> ch '}' .>> ws
    //let eventDec = character_event

    //let events = ws >>. namespaceP .>>. character_event .>> (many anyChar) .>> eof

    //let x = run (many line) "#test\nasd\ntest"
    //let y = runParserOnFile events () "wol_business_events.txt" System.Text.Encoding.UTF8
    let parseEventFile filepath = runParserOnFile all () filepath System.Text.Encoding.UTF8

    let tabs n = String.replicate n "\t"

    let rec printValue v depth =
        match v with
        | Block kvl -> "{\n" + printKeyValueList kvl (depth + 1) + tabs (depth + 1) + "}\n"
        | x -> x.ToString() + "\n"

    and printKeyValue kv depth =
        match kv with
        | Comment c -> (tabs depth) + "#" + c + "\n"
        | KeyValue (KeyValueItem (key, v)) -> (tabs depth) + key.ToString() + " = " + (printValue v depth)
    and printKeyValueList kvl depth =
        kvl |> List.map (fun kv -> printKeyValue kv depth) |> List.fold (+) ""
    let prettyPrint =
        function
        | Success (v,_,_) -> printKeyValueList v 0
        | Failure (msg, _, _) -> msg