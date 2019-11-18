namespace CWTools.Validation
open FParsec
open CWTools.Process.Scopes
open CWTools.Common
open CWTools.Utilities.Utils
open CWTools.Validation
open CWTools.Validation.ValidationCore
open System
open CWTools.Localisation
open CWTools.Utilities.Position
open CWTools.Parser.SharedParsers
open CWTools.Process.Localisation

module LocalisationString =

    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply

    type LocElement =
    | Ref of string
    | Command of string
    | JominiCommand of JominiLocCommand list
    | Chars of string
    let valueChars = many1Satisfy ( isNoneOf ['$'; '['; ']'; '#'] )

    let dollarChars = many1Satisfy ( isNoneOf ['$'; '|'] )
    let dollarColour = pchar '|' .>> dollarChars
    let commandChars = many1Satisfy (isNoneOf [']'; '|'])
    let commandFormat = pchar '|' .>> commandChars
    let ref = between (skipChar '$') (skipChar '$') (dollarChars .>> optional dollarColour |>> Ref)
    let command = between (skipChar '[') (skipChar ']') ((commandChars) .>> optional commandFormat |>> Command )
    let locStringParser = many (valueChars |>> Chars <|> ref <|> command) .>> eof

    let jominiCommand, jominiCommandImpl = createParserForwardedToRef()
    let simpleParam = chSkip '\'' >>. many1Satisfy ( isNoneOf ['.'; '\'']) .>> chSkip '\'' .>> ws |>> JominiLocCommandParam.Param// <!> "Param"
    let simpleCommands = ((sepBy1 (jominiCommand) (chSkip '.')) |>> JominiLocCommandParam.Commands)// <!> "Commands"
    let segment = many1Satisfy ( isNoneOf ['.'; '('; '|'; ']'; ')'; ' '; ',']) .>> ws |>> (fun k -> JominiLocCommand.Command (k, [])) //<!> "segment" //<!> "segment"
    let pParams = sepBy1 (simpleParam <|> simpleCommands) (chSkip ',')// <!> "pParams"//<!> "pParams"
    let pFunction = (many1Satisfy ( isNoneOf ['.'; ']'; '('; ')']) ) .>> (chSkip '(' ) .>>.  (pParams) .>> chSkip ')' |>> (fun (key, cs) -> JominiLocCommand.Command (key, cs))// <!> "function"
    // let pFunction = many1Satisfy ( isNoneOf ['.'; ']']) .>> ch '(' .>> ws .>>. sepBy1 (sepBy1 jominiCommand (ch '.')) (ch ',') .>> ch ')' |>> (fun (key, cs) -> JominiLocCommand.Command (key, cs)) <?> "function"
    jominiCommandImpl := ((attempt pFunction) <|> segment)// <!> "command"
    let jominiCommandWrapper = between (skipChar '[' .>> ws) (skipChar ']' .>> ws) ((sepBy1 jominiCommand (ch '.')) .>> optional commandFormat |>> JominiCommand )
    let jominiLocStringParser = many ((valueChars |>> Chars <|> ref <|> (jominiCommandWrapper)) .>> optional (ch '#' .>> restOfLine false .>> ws) ) .>> eof

    let parseLocString fileString filename = runParserOnString locStringParser () filename fileString
    let parseJominiLocString fileString filename = runParserOnString jominiLocStringParser () filename fileString

    let checkRef (hardcodedLocalisation) (lang : Lang) (keys : LocKeySet) (entry : LocEntry) (r : string) =
        match keys.Contains r with
        | true -> if r == entry.key && not (List.contains r hardcodedLocalisation) then Invalid (Guid.NewGuid(), [invManual (ErrorCodes.RecursiveLocRef) (entry.position) entry.key None ]) else OK
        | false ->
            match r |> seq |> Seq.exists (fun c -> Char.IsLower(c)) && not (List.contains r hardcodedLocalisation) with
            | true -> Invalid (Guid.NewGuid(), [invManual (ErrorCodes.UndefinedLocReference entry.key r (lang :> obj)) (entry.position) entry.key None ])
            | false -> OK

    let validateProcessedLocalisationBase (hardcodedLocalisation) (keys : (Lang * LocKeySet) list) (api : (Lang * Map<string, LocEntry>) list) =
        let validateQuotes _ (e : LocEntry) =
            let desc = e.desc.Trim()
            let lastHash = desc.LastIndexOf "#"
            let lastQuote = desc.LastIndexOf "\""
            let desc =
                match lastHash, lastQuote with
                | -1, _
                | _, -1 -> desc
                | h, q when h > q -> desc.Substring(0, h).Trim()
                | _ -> desc
            if desc.StartsWith "\"" <> desc.EndsWith "\"" then Invalid (Guid.NewGuid() ,[invManual (ErrorCodes.LocMissingQuote e.key) (e.position) e.key None]) else OK
        let validateContextResult (e : LocEntry) cr =
            match cr with
            | LocContextResult.Found _ -> OK
            | LocNotFound s -> Invalid (Guid.NewGuid(), [invManual (ErrorCodes.InvalidLocCommand e.key s) (e.position) e.key None ])
            | LocNotFoundInType (s, dataType, confident) -> Invalid (Guid.NewGuid(), [invManual (ErrorCodes.LocCommandNotInDataType e.key s dataType confident) (e.position) e.key None])
            | LocContextResult.NewScope s -> Invalid (Guid.NewGuid(), [invManual (ErrorCodes.CustomError (sprintf "Localisation command does not end in a command but ends in scope %O" s) Severity.Error ) (e.position) e.key None ])
            | LocContextResult.WrongScope (c, actual, (expected : Scope list)) ->
                Invalid (Guid.NewGuid(), [invManual (ErrorCodes.LocCommandWrongScope c (expected |> List.map (fun f -> f.ToString()) |> String.concat ", ") (actual.ToString())) (e.position) e.key None])
            | _ -> OK
        let validateLocMap (lang, (m : Map<string, LocEntry>)) =
            let keys = keys |> List.filter (fun (l, _) -> l = lang) |> List.map snd |> List.fold (fun a b -> LocKeySet.Union (a, b)) (LocKeySet.Empty(InsensitiveStringComparer()))
            m |> Map.map (fun _ e -> e.refs <&!&> checkRef hardcodedLocalisation lang keys e) |> Map.toList |> List.map snd |> List.fold (<&&>) OK
            <&&>
            (m |> Map.map (fun _ e -> e.scopes <&!&> validateContextResult e) |> Map.toList |> List.map snd |> List.fold (<&&>) OK)
            <&&>
            (m |> Map.map validateQuotes |> Map.toList |> List.map snd |> List.fold (<&&>) OK)


        let validateReplaceMe (lang, (m : Map<string, LocEntry>)) =
            m |> Map.toList |> List.fold (fun s (k, v) -> if v.desc == "\"REPLACE_ME\"" then s <&&> Invalid (Guid.NewGuid(), [invManual (ErrorCodes.ReplaceMeLoc v.key lang) (v.position) v.key None ]) else s ) OK

        api <&!&> validateLocMap <&&> (api <&!&> validateReplaceMe)

    let processLocalisationBase localisationCommandValidator (defaultContext : ScopeContext) (api : (Lang * Map<string, Entry>)) : Lang * Map<string,LocEntry>=
        // let lang = api |> fst
        // let keys = keys |> List.filter (fun (l, _) -> l = lang) |> List.map snd |> List.fold (fun a b -> LocKeySet.Union (a, b)) (LocKeySet.Empty(InsensitiveStringComparer()))
        // let all = api |> snd
        let extractResult =
            function
            |Success (v, _, _) -> v
            |Failure _ -> []
        let parseLoc (e : Entry) = parseLocString e.desc "" |> extractResult |> List.choose (function |Command s -> Some s |_ -> None)
        let parseLocRef (e : Entry) = parseLocString e.desc "" |> extractResult |> List.choose (function |Ref s -> Some s |_ -> None)
        let result = api |> (fun (f, s) -> f, s |> Map.map (fun _ m -> {LocEntry.key = m.key; value = m.value; desc = m.desc; position = m.position; refs = parseLocRef m; scopes = parseLoc m |> List.map (fun s -> localisationCommandValidator defaultContext s) }))
        result

    let processJominiLocalisationBase localisationCommandValidator (defaultContext : ScopeContext) (eventtargets : Collections.Map<string, Scope list>) (setvariables : string list) (api : (Lang * Map<string, Entry>)) : Lang * Map<string,LocEntry>=
        // let lang = api |> fst
        // let keys = keys |> List.filter (fun (l, _) -> l = lang) |> List.map snd |> List.fold (fun a b -> LocKeySet.Union (a, b)) (LocKeySet.Empty(InsensitiveStringComparer()))
        // let all = api |> snd
        // let commands = commands |> List.map fst
        let extractResult =
            function
            |Success (v, _, _) -> v
            |Failure (e, _, _) -> eprintfn "%A" e; []
        let parseLoc (e : Entry) = parseJominiLocString e.desc "" |> extractResult |> List.choose (function |JominiCommand s -> Some s |_ -> None)
        let parseLocRef (e : Entry) = parseJominiLocString e.desc "" |> extractResult |> List.choose (function |Ref s -> Some s |_ -> None)
        let result = api |> (fun (f, s) -> f, s |> Map.map (fun _ m -> {LocEntry.key = m.key; value = m.value; desc = m.desc; position = m.position; refs = parseLocRef m; scopes = parseLoc m |> List.map (fun s -> localisationCommandValidator eventtargets setvariables defaultContext s) }))
        result

    let validateLocalisationCommandsBase localisationCommandValidator (locentry : LocEntry) (startContext : ScopeContext)  =
        let extractResult =
            function
            | Success (v, _, _) -> v
            | Failure _ -> []
        let parseLoc (e : LocEntry) = parseLocString e.desc "" |> extractResult |> List.choose (function | Command s -> Some s | _ -> None)
        let keycommands = parseLoc locentry
        let validateCommand (c : string) =
            match localisationCommandValidator startContext c with
            | LocContextResult.WrongScope (c, actual, (expected : Scope list)) ->
                Invalid (Guid.NewGuid(), [invManual (ErrorCodes.LocCommandWrongScope c (expected |> List.map (fun f -> f.ToString()) |> String.concat ", ") (actual.ToString())) (locentry.position) locentry.key None])
            | _ -> OK
        keycommands <&!&> validateCommand

    let validateJominiLocalisationCommandsBase localisationCommandValidator (eventtargets : Collections.Map<string, Scope list>) (setvariables : string list) (locentry : LocEntry) (startContext : ScopeContext)  =
        let extractResult =
            function
            | Success (v, _, _) -> v
            | Failure _ -> []
        let parseLoc (e : LocEntry) = parseJominiLocString e.desc "" |> extractResult |> List.choose (function | JominiCommand s -> Some s | _ -> None)
        let keycommands = parseLoc locentry
        let validateCommand (c : JominiLocCommand list) =
            match localisationCommandValidator eventtargets setvariables startContext c with
            | LocNotFound s -> Invalid (Guid.NewGuid(), [invManual (ErrorCodes.InvalidLocCommand locentry.key s) (locentry.position) locentry.key None ])
            | LocNotFoundInType (s, dataType, confident) -> Invalid (Guid.NewGuid(), [invManual (ErrorCodes.LocCommandNotInDataType locentry.key s dataType confident) (locentry.position) locentry.key None])
            | LocContextResult.NewScope s -> Invalid (Guid.NewGuid(), [invManual (ErrorCodes.CustomError (sprintf "Localisation command does not end in a command but ends in scope %O" s) Severity.Error ) (locentry.position) locentry.key None ])
            | LocContextResult.WrongScope (c, actual, (expected : Scope list)) ->
                Invalid (Guid.NewGuid(), [invManual (ErrorCodes.LocCommandWrongScope c (expected |> List.map (fun f -> f.ToString()) |> String.concat ", ") (actual.ToString())) (locentry.position) locentry.key None])
            | _ -> OK
        keycommands <&!&> validateCommand

    let private getRange (start: FParsec.Position) (endp : FParsec.Position) = mkRange start.StreamName (mkPos (int start.Line) (int start.Column)) (mkPos (int endp.Line) (int endp.Column))
    open CWTools.Games
    let validateLocalisationSyntax (results : Results) =
        let createInvalid error (pos : Position option) =
            {
                code = "CW001"
                severity = Severity.Error
                range = (getRange pos.Value pos.Value)
                keyLength = 0
                message = error
                data = None
                relatedErrors = None
            }
        results.Values |> List.ofSeq
                       |> List.filter (fun (v, _, _, _) -> not v)
                       <&!&> (fun (_, _, error, pos) -> if pos.IsSome then Invalid (Guid.NewGuid(), [createInvalid error pos]) else OK)
