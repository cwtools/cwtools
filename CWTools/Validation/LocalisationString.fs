namespace CWTools.Validation
open FParsec
open CWTools.Process.Scopes
open CWTools.Common
open CWTools.Utilities.Utils
open CWTools.Validation.ValidationCore
open System
open CWTools.Localisation

module LocalisationString =
    type LocElement =
    | Ref of string
    | Command of string
    | Chars of string
    let valueChars = many1Satisfy ( isNoneOf ['$'; '['; ']'] )

    let dollarChars = many1Satisfy ( isNoneOf ['$'; '|'] )
    let dollarColour = pchar '|' .>> dollarChars
    let commandChars = many1Satisfy (isNoneOf [']'])
    let ref = between (skipChar '$') (skipChar '$') (dollarChars .>> optional dollarColour |>> Ref)
    let command = between (skipChar '[') (skipChar ']') (commandChars |>> Command)
    let locStringParser = many (valueChars |>> Chars <|> ref <|> command) .>> eof

    let parseLocString fileString filename = runParserOnString locStringParser () filename fileString

    let checkRef (hardcodedLocalisation) (lang : Lang) (keys : LocKeySet) (entry : LocEntry<_>) (r : string) =
        match keys.Contains r with
        | true -> OK
        | false ->
            match r |> seq |> Seq.exists (fun c -> Char.IsLower(c)) && not (List.contains r hardcodedLocalisation) with
            | true -> Invalid [invManual (ErrorCodes.UndefinedLocReference entry.key r (lang :> obj)) (entry.position) entry.key None ]
            | false -> OK

    let validateProcessedLocalisationBase (hardcodedLocalisation) (keys : (Lang * LocKeySet) list) (api : (Lang * Map<string, LocEntry<_>>) list) =
        let validateContextResult (e : LocEntry<_>) cr =
            match cr with
            | ContextResult.Found _ -> OK
            | LocNotFound s -> Invalid [invManual (ErrorCodes.InvalidLocCommand e.key s) (e.position) e.key None ]
        let validateLocMap (lang, (m : Map<string, LocEntry<_>>)) =
            let keys = keys |> List.filter (fun (l, _) -> l = lang) |> List.map snd |> List.fold (fun a b -> LocKeySet.Union (a, b)) (LocKeySet.Empty(InsensitiveStringComparer()))
            m |> Map.map (fun _ e -> e.refs <&!&> checkRef hardcodedLocalisation lang keys e) |> Map.toList |> List.map snd |> List.fold (<&&>) OK
            <&&>
            (m |> Map.map (fun _ e -> e.scopes <&!&> validateContextResult e) |> Map.toList |> List.map snd |> List.fold (<&&>) OK)

        let validateReplaceMe (lang, (m : Map<string, LocEntry<_>>)) =
            m |> Map.toList |> List.fold (fun s (k, v) -> if v.desc == "\"REPLACE_ME\"" then s <&&> Invalid [invManual (ErrorCodes.ReplaceMeLoc v.key lang) (v.position) v.key None ] else s ) OK

        api <&!&> validateLocMap <&&> (api <&!&> validateReplaceMe)

    let processLocalisationBase localisationCommandContext commands (eventTargets : string list) (scriptedLoc : string list) (setvariables : string list) (keys : (Lang * LocKeySet) list) (api : (Lang * Map<string, Entry>))=
        let lang = api |> fst
        let keys = keys |> List.filter (fun (l, _) -> l = lang) |> List.map snd |> List.fold (fun a b -> LocKeySet.Union (a, b)) (LocKeySet.Empty(InsensitiveStringComparer()))
        let all = api |> snd
        let extractResult =
            function
            |Success (v, _, _) -> v
            |Failure _ -> []
        let parseLoc (e : Entry) = parseLocString e.desc "" |> extractResult |> List.choose (function |Command s -> Some s |_ -> None)
        let parseLocRef (e : Entry) = parseLocString e.desc "" |> extractResult |> List.choose (function |Ref s -> Some s |_ -> None)
        let result = api |> (fun (f, s) -> f, s |> Map.map (fun _ m -> {LocEntry.key = m.key; value = m.value; desc = m.desc; position = m.position; refs = parseLocRef m; scopes = parseLoc m |> List.map (fun s -> localisationCommandContext (scriptedLoc @ commands) eventTargets setvariables m s) }))
        result
