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

module LocalisationString =
    type LocElement =
    | Ref of string
    | Command of string
    | Chars of string
    let valueChars = many1Satisfy ( isNoneOf ['$'; '['; ']'] )

    let dollarChars = many1Satisfy ( isNoneOf ['$'; '|'] )
    let dollarColour = pchar '|' .>> dollarChars
    let commandChars = many1Satisfy (isNoneOf [']'; '|'])
    let commandFormat = pchar '|' .>> commandChars
    let ref = between (skipChar '$') (skipChar '$') (dollarChars .>> optional dollarColour |>> Ref)
    let command = between (skipChar '[') (skipChar ']') ((commandChars) .>> optional commandFormat |>> Command )
    let locStringParser = many (valueChars |>> Chars <|> ref <|> command) .>> eof

    let parseLocString fileString filename = runParserOnString locStringParser () filename fileString

    let checkRef (hardcodedLocalisation) (lang : Lang) (keys : LocKeySet) (entry : LocEntry<_>) (r : string) =
        match keys.Contains r with
        | true -> if r == entry.key && not (List.contains r hardcodedLocalisation) then Invalid[invManual (ErrorCodes.RecursiveLocRef) (entry.position) entry.key None ] else OK
        | false ->
            match r |> seq |> Seq.exists (fun c -> Char.IsLower(c)) && not (List.contains r hardcodedLocalisation) with
            | true -> Invalid [invManual (ErrorCodes.UndefinedLocReference entry.key r (lang :> obj)) (entry.position) entry.key None ]
            | false -> OK

    let validateProcessedLocalisationBase (hardcodedLocalisation) (keys : (Lang * LocKeySet) list) (api : (Lang * Map<string, LocEntry<_>>) list) =
        let validateContextResult (e : LocEntry<_>) cr =
            match cr with
            | LocContextResult.Found _ -> OK
            | LocNotFound s -> Invalid [invManual (ErrorCodes.InvalidLocCommand e.key s) (e.position) e.key None ]
            | LocContextResult.NewScope s -> Invalid [invManual (ErrorCodes.CustomError (sprintf "Localisation command does not end in a command but ends in scope %O" s) Severity.Error ) (e.position) e.key None ]
            | LocContextResult.WrongScope (c, actual, (expected : 'S list)) ->
                Invalid [invManual (ErrorCodes.LocCommandWrongScope c (expected |> List.map (fun f -> f.ToString()) |> String.concat ", ") (actual.ToString())) (e.position) e.key None]
            | _ -> OK
        let validateLocMap (lang, (m : Map<string, LocEntry<_>>)) =
            let keys = keys |> List.filter (fun (l, _) -> l = lang) |> List.map snd |> List.fold (fun a b -> LocKeySet.Union (a, b)) (LocKeySet.Empty(InsensitiveStringComparer()))
            m |> Map.map (fun _ e -> e.refs <&!&> checkRef hardcodedLocalisation lang keys e) |> Map.toList |> List.map snd |> List.fold (<&&>) OK
            <&&>
            (m |> Map.map (fun _ e -> e.scopes <&!&> validateContextResult e) |> Map.toList |> List.map snd |> List.fold (<&&>) OK)

        let validateReplaceMe (lang, (m : Map<string, LocEntry<_>>)) =
            m |> Map.toList |> List.fold (fun s (k, v) -> if v.desc == "\"REPLACE_ME\"" then s <&&> Invalid [invManual (ErrorCodes.ReplaceMeLoc v.key lang) (v.position) v.key None ] else s ) OK

        api <&!&> validateLocMap <&&> (api <&!&> validateReplaceMe)

    let processLocalisationBase<'S when 'S :> IScope<'S>> localisationCommandValidator (defaultContext : ScopeContext<'S>) (commands : (string * 'S list) list) (eventTargets : string list) (scriptedLoc : string list) (setvariables : string list) (api : (Lang * Map<string, Entry>)) : Lang * Map<string,LocEntry<'S>>=
        // let lang = api |> fst
        // let keys = keys |> List.filter (fun (l, _) -> l = lang) |> List.map snd |> List.fold (fun a b -> LocKeySet.Union (a, b)) (LocKeySet.Empty(InsensitiveStringComparer()))
        // let all = api |> snd
        let commands = commands |> List.map fst
        let extractResult =
            function
            |Success (v, _, _) -> v
            |Failure _ -> []
        let parseLoc (e : Entry) = parseLocString e.desc "" |> extractResult |> List.choose (function |Command s -> Some s |_ -> None)
        let parseLocRef (e : Entry) = parseLocString e.desc "" |> extractResult |> List.choose (function |Ref s -> Some s |_ -> None)
        let result = api |> (fun (f, s) -> f, s |> Map.map (fun _ m -> {LocEntry.key = m.key; value = m.value; desc = m.desc; position = m.position; refs = parseLocRef m; scopes = parseLoc m |> List.map (fun s -> localisationCommandValidator (scriptedLoc @ commands) eventTargets setvariables defaultContext s) }))
        result

    let validateLocalisationCommandsBase<'S when 'S :> IScope<'S>> localisationCommandValidator (commands : (string * 'S list) list) (eventTargets : string list) (scriptedLoc : string list) (setvariables : string list) (locentry : LocEntry<'S>) (startContext : ScopeContext<'S>)  =
        let allcommands = commands |> List.map fst
        let extractResult =
            function
            | Success (v, _, _) -> v
            | Failure _ -> []
        let parseLoc (e : LocEntry<'S>) = parseLocString e.desc "" |> extractResult |> List.choose (function | Command s -> Some s | _ -> None)
        let keycommands = parseLoc locentry
        let validateCommand (c : string) =
            match localisationCommandValidator (scriptedLoc @ allcommands) eventTargets setvariables startContext c with
            | LocContextResult.WrongScope (c, actual, (expected : 'S list)) ->
                Invalid [invManual (ErrorCodes.LocCommandWrongScope c (expected |> List.map (fun f -> f.ToString()) |> String.concat ", ") (actual.ToString())) (locentry.position) locentry.key None]
            | _ -> OK
        keycommands <&!&> validateCommand

    let private getRange (start: FParsec.Position) (endp : FParsec.Position) = mkRange start.StreamName (mkPos (int start.Line) (int start.Column)) (mkPos (int endp.Line) (int endp.Column))

    let validateLocalisationSyntax (results : Results) =
        results.Values |> List.ofSeq
                       |> List.filter (fun (v, _, _, _) -> not v)
                       <&!&> (fun (_, _, error, pos) -> if pos.IsSome then Invalid [("CW001", Severity.Error, (getRange pos.Value pos.Value), 0, error, None)] else OK)
