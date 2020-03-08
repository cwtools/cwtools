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

    let private dollarChars = many1Satisfy ( isNoneOf ['$'; '|'] )
    let private dollarColour = pchar '|' .>> dollarChars
    let private commandChars = many1Satisfy (isNoneOf [']'; '|'])
    let private commandFormat = pchar '|' .>> commandChars
    let private ref = between (skipChar '$') (skipChar '$') (dollarChars .>> optional dollarColour |>> Ref)
    let private command = between (skipChar '[') (skipChar ']') ((commandChars) .>> optional commandFormat |>> Command )
    let private locStringParser = many (valueChars |>> Chars <|> ref <|> command) .>> eof

    let private jominiCommand, jominiCommandImpl = createParserForwardedToRef()
    let private simpleParam = chSkip '\'' >>. many1Satisfy ( isNoneOf ['.'; '\'']) .>> chSkip '\'' .>> ws |>> JominiLocCommandParam.Param// <!> "Param"
    let private simpleCommands = ((sepBy1 (jominiCommand) (chSkip '.')) |>> JominiLocCommandParam.Commands)// <!> "Commands"
    let private segment = many1Satisfy ( isNoneOf ['.'; '('; '|'; ']'; ')'; ' '; ',']) .>> ws |>> (fun k -> JominiLocCommand.Command (k, [])) //<!> "segment" //<!> "segment"
    let private pParams = sepBy1 (simpleParam <|> simpleCommands) (chSkip ',')// <!> "pParams"//<!> "pParams"
    let private pFunction = (many1Satisfy ( isNoneOf ['.'; ']'; '('; ')']) ) .>> (chSkip '(' ) .>>.  (pParams) .>> chSkip ')' |>> (fun (key, cs) -> JominiLocCommand.Command (key, cs))// <!> "function"
    // let pFunction = many1Satisfy ( isNoneOf ['.'; ']']) .>> ch '(' .>> ws .>>. sepBy1 (sepBy1 jominiCommand (ch '.')) (ch ',') .>> ch ')' |>> (fun (key, cs) -> JominiLocCommand.Command (key, cs)) <?> "function"
    jominiCommandImpl := ((attempt pFunction) <|> segment)// <!> "command"
    let private jominiCommandWrapper = between (skipChar '[' .>> ws) (skipChar ']' .>> ws) ((sepBy1 jominiCommand (ch '.')) .>> optional commandFormat |>> JominiCommand )
    let private jominiLocStringParser = many ((valueChars |>> Chars <|> ref <|> (jominiCommandWrapper)) .>> optional (ch '#' .>> restOfLine false .>> ws) ) .>> eof

    let private parseLocString fileString filename = runParserOnString locStringParser () filename fileString
    let private parseJominiLocString fileString filename = runParserOnString jominiLocStringParser () filename fileString

    /// Check that reference loc key `r` is defined and is not recursive, else return an error
    let private checkRef (hardcodedLocalisation) (lang : Lang) (keys : LocKeySet) (entry : LocEntry) (r : string) =
        match keys.Contains r with
        | true -> if r == entry.key && not (List.contains r hardcodedLocalisation) then Invalid (Guid.NewGuid(), [invManual (ErrorCodes.RecursiveLocRef) (entry.position) entry.key None ]) else OK
        | false ->
            match r |> seq |> Seq.exists (fun c -> Char.IsLower(c)) && not (List.contains r hardcodedLocalisation) with
            | true ->
                let firstSpace = r.IndexOf ' '
                let lastSpace = r.LastIndexOf ' '
                if firstSpace <> -1 && lastSpace <> -1 && firstSpace <> lastSpace
                then OK
                else Invalid (Guid.NewGuid(), [invManual (ErrorCodes.UndefinedLocReference entry.key r (lang :> obj)) (entry.position) entry.key None ])
            | false -> OK
    /// Given a set of localisation APIs, validates them.
    /// Checks quotes, localisation command chains, localisation references
    let validateProcessedLocalisationBase (hardcodedLocalisation) (keys : (Lang * LocKeySet) list) (api : (Lang * Map<string, LocEntry>) list) =
        let validateQuotes _ (e : LocEntry) =
            let desc = e.desc.Trim()
            // let lastHash = desc.LastIndexOf "#"
            let lastQuote = desc.LastIndexOf "\""
            let firstHashAfterQuote = if lastQuote = -1 then desc.IndexOf "#" else (desc.Substring(lastQuote).IndexOf "#") + lastQuote
            let desc =
                match firstHashAfterQuote, lastQuote with
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
            m |> Map.toList |> List.fold (fun s (k, v) -> if v.desc == "\"REPLACE_ME\"" || v.desc == "\"TODO_CD\"" then s <&&> Invalid (Guid.NewGuid(), [invManual (ErrorCodes.ReplaceMeLoc v.key lang) (v.position) v.key None ]) else s ) OK

        api <&!&> validateLocMap <&&> (api <&!&> validateReplaceMe)

    let processLocalisationBase localisationCommandValidator (defaultContext : ScopeContext) (api : (Lang * Map<string, Entry>)) : Lang * Map<string,LocEntry> =
        let extractResult =
            function
            |Success (v, _, _) -> v
            |Failure _ -> []
        let processEntry (m : Entry) =
            let locElements = parseLocString m.desc "" |> extractResult
            let commands = locElements |> List.choose (function |Command s -> Some s |_ -> None)
            {
                LocEntry.key = m.key;
                value = m.value;
                desc = m.desc;
                position = m.position;
                refs = locElements |> List.choose (function |Ref s -> Some s |_ -> None)
                commands = commands
                jominiCommands = []
                scopes = commands |> List.map (fun s -> localisationCommandValidator defaultContext s)
            }
        api |> (fun (lang, entries) -> lang, entries |> Map.map (fun _ entry -> processEntry entry))

    let processJominiLocalisationBase localisationCommandValidator (eventtargets : Collections.Map<string, Scope list>) (setvariables : string list) (api : (Lang * Map<string, Entry>)) : Lang * Map<string,LocEntry>=
        let extractResult =
            function
            |Success (v, _, _) -> v
            |Failure (e, _, _) -> eprintfn "%A" e; []
        let processEntry (m : Entry) =
            let locElements = parseJominiLocString m.desc "" |> extractResult
            let commands = locElements |> List.choose (function |JominiCommand s -> Some s |_ -> None)
            {
                LocEntry.key = m.key;
                value = m.value;
                desc = m.desc;
                position = m.position;
                refs = locElements |> List.choose (function |Ref s -> Some s |_ -> None)
                commands = []
                jominiCommands = commands
                scopes = commands |> List.map (fun s -> localisationCommandValidator eventtargets setvariables s)
            }
        api |> (fun (lang, entries) -> lang, entries |> Map.map (fun _ entry -> processEntry entry))

    let validateLocalisationCommandsBase localisationCommandValidator (locentry : LocEntry) (startContext : ScopeContext)  =
        // let extractResult =
        //     function
        //     | Success (v, _, _) -> v
        //     | Failure _ -> []
        // let parseLoc (e : LocEntry) = parseLocString e.desc "" |> extractResult |> List.choose (function | Command s -> Some s | _ -> None)
        // let keycommands = parseLoc locentry
        let keycommands = locentry.commands
        let validateCommand (c : string) =
            match localisationCommandValidator startContext c with
            | LocContextResult.WrongScope (c, actual, (expected : Scope list)) ->
                Invalid (Guid.NewGuid(), [invManual (ErrorCodes.LocCommandWrongScope c (expected |> List.map (fun f -> f.ToString()) |> String.concat ", ") (actual.ToString())) (locentry.position) locentry.key None])
            | _ -> OK
        keycommands <&!&> validateCommand

    let validateJominiLocalisationCommandsBase localisationCommandValidator (eventtargets : Collections.Map<string, Scope list>) (setvariables : string list) (locentry : LocEntry) (startContext : ScopeContext)  =
        // let extractResult =
        //     function
        //     | Success (v, _, _) -> v
        //     | Failure _ -> []
        // let parseLoc (e : LocEntry) = parseJominiLocString e.desc "" |> extractResult |> List.choose (function | JominiCommand s -> Some s | _ -> None)
        // let keycommands = parseLoc locentry
        let keycommands = locentry.jominiCommands
        let validateCommand (c : JominiLocCommand list) =
            match localisationCommandValidator startContext eventtargets setvariables c with
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
