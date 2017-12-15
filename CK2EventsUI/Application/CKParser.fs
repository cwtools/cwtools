namespace CK2Events.Application

open FParsec

module CKParser =

    type Key =
        | Key of string
        override x.ToString() = let (Key v) = x in sprintf "%s" v  
    
    type Troop =
        | Troop of Key * int * int
        override x.ToString() = let (Troop (id, a, b)) = x in sprintf "%O = { %O %O }" id a b
    
    type KeyValueItem = 
        | KeyValueItem of Key * Value
        override x.ToString() = let (KeyValueItem (id, v)) = x in sprintf "%O = %O" id v
    and Value =
        | String of string
        | QString of string
        | Float of float
        | Bool of bool
        | Block of Statement list
        override x.ToString() =
            match x with
            | Block b -> "{ " + sprintf "%O" b + " }"
            | QString s -> "\"" + s + "\""
            | String s -> s
            | Bool b -> if b then "yes" else "no"
            | Float f -> sprintf "%A" f
   
    and Statement =
        | Comment of string
        | KeyValue of KeyValueItem
        | Troops of Troop list
    [<StructuralEquality; StructuralComparison>]
    type EventFile = |EventFile of  Statement list

    let whitespaceTextChars = " \t\r\n"
    let idchar = asciiLetter <|> digit <|> anyOf ['_'; ':']
    let valuechar = asciiLetter <|> digit <|> anyOf ['_'; '.'; '-'; ':'; '\''; '['; ']']


    let ws = (many spaces1 |>> ignore) <?> "whitespace"
    let comment = skipChar '#' >>. manyCharsTill anyChar ((newline |>> ignore) <|> eof) .>> ws |>> string |>> Comment
    let wsc = (many (comment |>> ignore <|> spaces1) |>> ignore) <?> "whitespace_comment"
    let str s = pstring s .>> ws
    let ch c = skipChar c >>. ws
    let id =
        (many1Chars idchar) .>> ws |>> Key <?> "id"

    let valueS = 
        (many1Chars valuechar) .>> ws |>> string |>> String <?> "valueS"

    let valueQ = 
        between (ch '"') (ch '"') (manyChars (noneOf "\"")) |>> string |>> QString <?> "valueQ"

    let valueB =
        (notFollowedBy (valuechar) >>.  pstring "yes" .>> ws |>> (fun _ -> Bool(true))) <|> (notFollowedBy (valuechar) >>. pstring "no" .>> ws |>> (fun _ -> Bool(false)))

    let keyvalue, keyvalueimpl = createParserForwardedToRef()
    let troops, troopsimpl = createParserForwardedToRef()
    let keyvaluelist = many (comment <|> (attempt troops) <|> keyvalue)
    let valueBlock = 
        let list = keyvaluelist |>> Block
        between (ch '{') (ch '}') list <?> "valueBlock"
    let value = valueQ <|> valueBlock <|> (attempt valueB) <|> valueS <?> "value"
    let operator = (attempt (str ">=")) <|> (attempt (str "<=")) <|> (attempt (str "==")) <|> str "=" <|> str "<" <|> str ">"
    do keyvalueimpl := 
        pipe2 (id .>> operator) (value)
            (fun id value -> KeyValue(KeyValueItem(id, value)))

    let troop = 

        let troopInner = pipe2 (puint64 .>> ws) (puint64 .>> ws) (fun t1 t2 -> (t1, t2)) 
        let troopValue = many (pipe2 (id .>> ch '=') (between (ch '{') (ch '}') troopInner)
                            (fun id (t1, t2) -> Troop(id, int t1 , int t2)))
        between (ch '{') (ch '}') troopValue

    do troopsimpl :=
        str "troops" .>> ch '=' >>. troop
        |>> Troops
        
    
    
    let all = ws >>. keyvaluelist .>> eof |>> (fun f -> (EventFile f : EventFile))
    let parseEventFile filepath = runParserOnFile all () filepath System.Text.Encoding.UTF8

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

    let tabs n = String.replicate n "\t"

    let printTroop depth t = (tabs depth) + t.ToString()  + "\n"
        //match t with
        //| Troop (id, t1, t2) -> tabs depth + string id + " = " + string t1 + " " + string t2 + "\n"
        

    let rec printValue v depth =
        match v with
        | Block kvl -> "{ \n" + printKeyValueList kvl (depth + 1) + tabs (depth + 1) + " }\n"
        | x -> x.ToString() + "\n"
    

    and printKeyValue kv depth =
        match kv with
        | Comment c -> (tabs depth) + "#" + c + "\n"
        | KeyValue (KeyValueItem (key, v)) -> (tabs depth) + key.ToString() + " = " + (printValue v depth)
        //| Troops tl -> (tabs depth) + tl.ToString() + "\n"
        | Troops tl ->
            (tabs depth) + "troops = {\n" +
            (List.map (printTroop (depth + 1)) tl |> List.fold (+) "") +
            (tabs depth) + "}\n"
        //| Troops tl -> tabs depth + "troops = {\n" + (List.map (fun t -> (tabs (depth + 1)) + t.ToString() + "\n" ) tl |> List.fold (+) "") + tabs depth + "}\n"
    and printKeyValueList kvl depth =
        kvl |> List.map (fun kv -> printKeyValue kv depth) |> List.fold (+) ""
    let prettyPrint =
        function
        | Success (v,_,_) -> 
            let (EventFile ev) = v
            printKeyValueList ev 0
        | Failure (msg, _, _) -> msg