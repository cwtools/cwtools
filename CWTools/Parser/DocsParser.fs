namespace CWTools.Parser


open FParsec
open CWTools.Common



module DocsParser =

    let private idChar = letter <|> digit <|> anyOf [ '_' ]
    let private isvaluechar = SharedParsers.isvaluechar
    let private maxChars = 4000

    let private header =
        skipCharsTillString "DOCUMENTATION ==" true maxChars .>> SharedParsers.ws
        <?> "header"

    let private name =
        (many1Chars idChar) .>> SharedParsers.ws .>> pchar '-' .>>. restOfLine false
        .>> SharedParsers.ws
        <?> "name"

    let private usage =
        charsTillString "Supported scopes:" true maxChars .>> SharedParsers.ws <?> "usage"

    let private usageC =
        charsTillString "Supported Scopes:" true maxChars .>> SharedParsers.ws <?> "usage"

    let private scope =
        many1Satisfy (fun c -> isvaluechar c || c = '?' || c = '(' || c = ')')
        .>> many (anyOf [ ' '; '\t' ])
        <?> "scope"

    let private target = many1Satisfy isvaluechar .>> many (skipChar ' ') <?> "target"
    let private targets = manyTill target newline .>> SharedParsers.ws <?> "targets"

    let private scopesWithoutTarget =
        manyTill scope newline |>> (fun x -> (x, [])) .>> SharedParsers.ws <?> "scopes"

    let private scopes =
        manyTill
            scope
            (newline .>>. skipString "Supported targets:"
             .>> skipManySatisfy (fun c -> c = ' '))
        .>>. targets
        <?> "scopes"

    let private scopesC =
        manyTill
            scope
            (newline .>>. skipString "Supported Targets:"
             .>> skipManySatisfy (fun c -> c = ' '))
        .>>. targets
        <?> "scopes"

    let private doc =
        pipe3
            name
            (attempt usage <|> usageC)
            (attempt scopes <|> attempt scopesC <|> scopesWithoutTarget)
            (fun (n, d) u (s, t) ->
                { name = n
                  desc = d
                  traits = None
                  usage = u
                  scopes = s
                  targets = [] })
        <?> "doc"

    let private footer: Parser<unit, unit> =
        skipString "=================" .>> SharedParsers.ws

    let private docFile = SharedParsers.ws >>. header >>. many doc .>> footer

    let private twoDocs = docFile .>>. docFile

    let toDocEffect<'a when 'a: comparison> effectType parseScopes (x: RawEffect) =
        DocEffect(x, effectType, parseScopes)

    let processDocs parseScopes (t, e) =
        t |> List.map (toDocEffect EffectType.Trigger parseScopes),
        e |> List.map (toDocEffect EffectType.Effect parseScopes)

    let parseDocsFile filepath =
        runParserOnFile twoDocs () filepath (System.Text.Encoding.GetEncoding(1252))

    let parseDocsFilesRes filepath =
        parseDocsFile filepath
        |> (function
        | Success(p, _, _) -> p
        | _ -> [], [])

    let parseDocsStream file =
        runParserOnStream twoDocs () "docFile" file (System.Text.Encoding.GetEncoding(1252))

module JominiParser =
    let pipe6 p1 p2 p3 p4 p5 p6 f =
        pipe4 p1 p2 p3 (tuple3 p4 p5 p6) (fun x1 x2 x3 (x4, x5, x6) -> f x1 x2 x3 x4 x5 x6)

    let private idChar = letter <|> digit <|> anyOf [ '_' ]
    let private isvaluechar = SharedParsers.isvaluechar

    let private header =
        skipCharsTillString "Event Target Documentation:" true 2000 .>> SharedParsers.ws
        <?> "header"

    let private spacer = skipString "--------------------" .>> SharedParsers.ws

    let private name =
        (many1Chars idChar) .>> SharedParsers.ws .>> pchar '-' .>>. restOfLine false
        .>> SharedParsers.ws
        <?> "name"

    let private reqData =
        pstring "Requires Data: yes" .>> SharedParsers.ws <?> "requires data"

    let private wildCard = pstring "Wild Card: yes" .>> SharedParsers.ws <?> "wildcard"

    let private globalLink =
        pstring "Global Link: yes" .>> SharedParsers.ws <?> "globallink"

    let private inscopes =
        pstring "Input Scopes: "
        >>. sepBy (manyChars (noneOf ("," + "\r\n"))) (pstring ",")
        .>> newline
        .>> SharedParsers.ws

    let private outscopes =
        pstring "Output Scopes: "
        >>. sepBy (manyChars (noneOf ("," + "\r\n"))) (pstring ",")
        .>> newline
        .>> SharedParsers.ws

    let private link =
        pipe6 name (opt reqData) (opt wildCard) (opt globalLink) (opt inscopes) (opt outscopes) (fun (n, d) r w g i o ->
            n, d, r, w, i, o, g)

    let private footer =
        pstring "Event Targets Saved from Code:" .>> many1Chars anyChar .>> eof

    let private linkFile =
        SharedParsers.ws >>. header >>. spacer >>. many (attempt (link .>> spacer))
        .>> footer

    let parseLinksFile filepath =
        runParserOnFile linkFile () filepath (System.Text.Encoding.GetEncoding(1252))

    let parseLinksFilesRes filepath =
        parseLinksFile filepath
        |> (function
        | Success(p, _, _) -> p
        | Failure(e, _, _) -> failwith e)

    let private triggerheader =
        skipCharsTillString "Trigger Documentation:" true 2000 .>> SharedParsers.ws
        <?> "header"

    let private effectheader =
        skipCharsTillString "Effect Documentation:" true 2000 .>> SharedParsers.ws
        <?> "header"

    let private supportedscopes =
        pstring "Supported Scopes: "
        >>. sepBy (manyChars (noneOf ("," + "\r\n"))) (pstring ",")
        .>> newline
        .>> SharedParsers.ws
        <?> "scopes"

    let private supportedtargets =
        pstring "Supported Targets: "
        >>. sepBy (manyChars (noneOf ("," + "\r\n"))) (pstring ",")
        .>> newline
        .>> SharedParsers.ws
        <?> "targets"

    let private traits =
        pstring "Traits: " >>. restOfLine false .>> SharedParsers.ws <?> "traits"

    let private tname =
        (many1Chars idChar) .>> SharedParsers.ws .>> pchar '-' .>> SharedParsers.ws
        <?> "name"

    let private endOfDesc =
        newline
        >>. (pstring "Supported Scopes:"
             <|> pstring "Supported Targets:"
             <|> pstring "Traits:")

    let private desc =
        (many1CharsTill anyChar (followedBy endOfDesc)) .>> SharedParsers.ws <?> "desc"

    let private trigger =
        pipe5 tname desc (opt traits) (opt supportedscopes) (opt supportedtargets) (fun n d tr s t ->
            { name = n
              desc = d
              traits = tr
              usage = ""
              scopes = s |> Option.defaultValue []
              targets = t |> Option.defaultValue [] })

    let private triggerFile =
        SharedParsers.ws >>. triggerheader >>. many1 (attempt (spacer >>. trigger))
        .>> eof

    let private effectFile =
        SharedParsers.ws >>. effectheader >>. many1 (attempt (spacer >>. trigger))
        .>> eof

    let parseTriggerFile filepath =
        runParserOnFile triggerFile () filepath (System.Text.Encoding.GetEncoding(1252))

    let parseTriggerFilesRes filepath =
        parseTriggerFile filepath
        |> (function
        | Success(p, _, _) -> p
        | _ -> [])

    let parseTriggerStream file =
        runParserOnStream triggerFile () "triggerFile" file (System.Text.Encoding.GetEncoding(1252))

    let parseTriggerStreamRes file =
        parseTriggerStream file
        |> (function
        | Success(p, _, _) -> Some p
        | _ -> None)

    let parseEffectFile filepath =
        runParserOnFile effectFile () filepath (System.Text.Encoding.GetEncoding(1252))

    let parseEffectFilesRes filepath =
        parseEffectFile filepath
        |> (function
        | Success(p, _, _) -> p
        | _ -> [])

    let parseEffectStream file =
        runParserOnStream effectFile () "effectFile" file (System.Text.Encoding.GetEncoding(1252))

    let parseEffectStreamRes file =
        parseEffectStream file
        |> (function
        | Success(p, _, _) -> Some p
        | _ -> None)

    let toDocEffect<'a when 'a: comparison> effectType parseScopes (x: RawEffect) =
        DocEffect(x, effectType, parseScopes)

    let processEffects parseScopes e =
        e |> List.map (toDocEffect EffectType.Effect parseScopes)

    let processTriggers parseScopes (t: RawEffect list) =
        t
        |> List.map (fun t ->
            if t.traits.IsSome && t.traits.Value.Contains "<, <=, =, !=, >, >=" then
                toDocEffect EffectType.ValueTrigger parseScopes t
            else
                toDocEffect EffectType.Trigger parseScopes t)

module DataTypeParser =
    open SharedParsers

    type JominiLocDataTypes =
        { dataTypes: Map<string, Map<string, string>>
          dataTypeNames: Set<string>
          functions: Map<string, string>
          confidentFunctions: Map<string, string>
          promotes: Map<string, string> }

    let private convToDataTypes
        (promotes: (string * string) list)
        (functions: (string * string) list)
        (dataTypes: (string * (string * string) list) list)
        =
        let customMapOfList (input: (string * string) list) =
            let folder (acc: Map<string, string>) (key, value) =
                match Map.tryFind key acc, value with
                | None, _ -> Map.add key value acc
                | Some "[unregistered]", _ -> Map.add key value acc
                | Some _, "[unregistered]" -> acc
                | _, _ -> Map.add key value acc

            input |> List.fold folder Map.empty

        let promoteMap = customMapOfList promotes
        // TODO: Remove this hack
        let confidentFunctionMap =
            functions
            |> List.choose (fun (k, v) -> if v = "[unregistered]" then None else Some(k, v))
            |> Map.ofList

        let functionMap =
            functions
            |> List.map (fun (k, v) -> k, (if v = "[unregistered]" then "Character" else v))
            |> Map.ofList

        let dataTypeMap =
            Map.ofList (dataTypes |> List.map (fun (k, v) -> k, customMapOfList v))

        let names = dataTypes |> List.map fst |> Set.ofList

        { promotes = promoteMap
          confidentFunctions = confidentFunctionMap
          functions = functionMap
          dataTypes = dataTypeMap
          dataTypeNames = names }
        |> (fun f ->
            eprintfn "%A" f
            f)

    let private idChar = letter <|> digit <|> anyOf [ '_'; '['; ']'; ':' ]

    let private line =
        many1Chars idChar .>> ws .>> skipString "->" .>> ws .>>. many1Chars idChar
        .>> ws

    let private promotes =
        pstring "Global Promotes ="
        >>. ws
        >>. between (skipChar '{' .>> ws) (skipChar '}' .>> ws) (many line)

    let private functions =
        pstring "Global Functions ="
        >>. ws
        >>. between (skipChar '{' .>> ws) (skipChar '}' .>> ws) (many line)

    let private dataType =
        many1Chars idChar .>> ws
        .>>. (between (skipString "= {" .>> ws) (skipChar '}' .>> ws) (many line))

    let private types =
        pstring "Types ="
        >>. ws
        >>. between (skipChar '{' .>> ws) (skipChar '}' .>> ws) (many dataType)

    let private dataTypeDump =
        promotes .>>. functions .>>. types .>> eof
        |>> (fun ((a, b), c) -> convToDataTypes a b c)

    let parseDataTypesFile filepath =
        runParserOnFile dataTypeDump () filepath (System.Text.Encoding.GetEncoding(1252))

    let parseDataTypesFileRes filepath =
        parseDataTypesFile filepath
        |> (function
        | Success(p, _, _) -> p
        | Failure(e, _, _) ->
            CWTools.Utilities.Utils.log (sprintf "datatype parse failed with %A" e)

            { promotes = Map.empty
              confidentFunctions = Map.empty
              functions = Map.empty
              dataTypes = Map.empty
              dataTypeNames = Set.empty })

    let parseDataTypesStream file =
        runParserOnStream dataTypeDump () "" file (System.Text.Encoding.GetEncoding(1252))

    let parseDataTypesStreamRes file =
        parseDataTypesStream file
        |> (function
        | Success(p, _, _) -> p
        | Failure(e, _, _) ->
            CWTools.Utilities.Utils.log (sprintf "datatype parse failed with %A" e)

            { promotes = Map.empty
              confidentFunctions = Map.empty
              functions = Map.empty
              dataTypes = Map.empty
              dataTypeNames = Set.empty })

module StellarisModifierParser =


    let private isvaluechar = SharedParsers.isvaluechar

    let private str s =
        pstring s .>> SharedParsers.ws <?> ("string " + s)

    let private pre = skipCharsTillString "Static Modifier #" true 100
    let private num = pre >>. pint64 .>> SharedParsers.ws |>> int

    let private tag =
        skipString "tag = " >>. many1Satisfy isvaluechar .>> SharedParsers.ws

    let private name = str "name = " >>. restOfLine true //manyCharsTill valuechar newline .>> ws

    let private modifierHeader =
        skipCharsTillString "Printing Modifier Definitions:" true 20000000
        .>> SharedParsers.ws
        <?> "modifier header"

    let private mtag =
        skipCharsTillString "- " true 500
        >>. many1CharsTill (satisfy isvaluechar) (pchar ',')
        .>> SharedParsers.ws

    let private categories =
        skipString "Category: "
        >>. sepBy (manyChars (noneOf ("," + "\r\n"))) (pstring ", ")
        .>> newline
        .>> SharedParsers.ws

    let private modifier =
        pipe2 mtag categories (fun t c -> {| tag = t; categories = c |})

    //    let private footer = many1Chars anyChar

    let private logFile =
        SharedParsers.ws >>. modifierHeader >>. many1 (attempt modifier) .>> eof


    let toDocEffect<'a when 'a: comparison> effectType parseScope (x: RawEffect) = DocEffect(x, effectType, parseScope)

    let parseLogsFile filepath =
        runParserOnFile logFile () filepath (System.Text.Encoding.GetEncoding(1252))

    let parseLogsStream file =
        runParserOnStream logFile () "logFile" file (System.Text.Encoding.GetEncoding(1252))

    let processLogs
        (m:
            {| categories: string list
               tag: string |} list)
        =
        m
        |> List.collect (fun rm ->
            rm.categories
            |> List.map (fun cm ->
                { ActualModifier.tag = rm.tag
                  category = modifierCategoryManager.ParseModifier () cm }))
