namespace CK2Events.Application

open FSharp.Data
open System.IO
open Microsoft.Extensions.Options
open System.Collections.Generic

module LocalisationDomain =
    type GetDesc = string -> string
    type GetKeys = string list
    type Values = IDictionary<string, string>
    type Results = IDictionary<string, (bool * int * string)>

    type LocalisationAPI =
        {
            results : Results
            values : Values
            getKeys : GetKeys
            getDesc : GetDesc
        }

module CKLocalisation =
    open LocalisationDomain
    type LocalisationEntry = CsvProvider<"#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x", ";",IgnoreErrors=false,Quote='~',HasHeaders=true,Encoding="1252">
    type LocalisationEntryFallback = CsvProvider<"#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x", ";",IgnoreErrors=true,Quote='~',HasHeaders=true,Encoding="1252">


    type CKLocalisationService(localisationDirectory : string, language : CK2Lang) =
        let localisationFolder : string = localisationDirectory
        let language : CK2Lang = language
        let mutable csv : Runtime.CsvFile<LocalisationEntry.Row> = upcast LocalisationEntry.Parse "#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x"
        let mutable csvFallback : Runtime.CsvFile<LocalisationEntryFallback.Row> = upcast LocalisationEntryFallback.Parse "#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x"
        let addFile (x : string) =
            let retry file msg =
                let rows = LocalisationEntryFallback.ParseRows file |> List.ofSeq
                csvFallback <- csvFallback.Append(rows)
                (false, rows.Length, msg)
            let file = 
                File.ReadAllLines(x, System.Text.Encoding.GetEncoding(1252))
                |> Array.filter(fun l -> l.StartsWith("#CODE") || not(l.StartsWith("#")))
                |> String.concat "\n"
            try
                let rows = LocalisationEntry.ParseRows file |> List.ofSeq
                csv <- csv.Append(rows)
                (true, rows.Length, "")
            with
            | Failure msg ->
                try
                    retry file msg
                with
                | Failure msg ->
                    (false, 0, msg)
            | exn ->
                try
                    retry file exn.Message
                with
                | Failure msg ->
                    (false, 0, msg)
        
        let mutable results : IDictionary<string, (bool * int * string)> = upcast new Dictionary<string, (bool * int * string)>()
        let addFiles (x : string list) = List.map (fun f -> (f, addFile f)) x

        let getForLang (x : LocalisationEntry.Row) =
            match language with
            |CK2Lang.English -> x.ENGLISH
            |CK2Lang.French -> x.FRENCH
            |CK2Lang.German -> x.GERMAN
            |CK2Lang.Spanish -> x.SPANISH
            |_ -> x.ENGLISH
                
        let getForLangFallback (x : LocalisationEntryFallback.Row) =
            match language with
            |CK2Lang.English -> x.ENGLISH
            |CK2Lang.French -> x.FRENCH
            |CK2Lang.German -> x.GERMAN
            |CK2Lang.Spanish -> x.SPANISH
            |_ -> x.ENGLISH

        let getKeys = csv.Rows |> Seq.map (fun f -> f.``#CODE``) |> List.ofSeq

        let values = 
            let one = csv.Rows |> Seq.map(fun f -> (f.``#CODE``, getForLang f))
            let two = csvFallback.Rows |> Seq.map(fun f -> (f.``#CODE``, getForLangFallback f))
            Seq.concat [one; two] |> dict

        let getDesc x =
            let one = csv.Rows |> Seq.tryFind (fun f -> f.``#CODE`` = x) 
            let two = csvFallback.Rows |> Seq.tryFind (fun f -> f.``#CODE`` = x)
            match (one, two) with
            | (Some x, _) -> getForLang x
            | (None, Some x) -> getForLangFallback x
            | _ -> x
        
        do
            match Directory.Exists(localisationFolder) with
            | true -> 
                        let files = Directory.EnumerateFiles localisationFolder |> List.ofSeq |> List.sort
                        results <- addFiles files |> dict
            | false -> ()
        new (settings : IOptionsSnapshot<CK2Settings>) = CKLocalisationService(settings.Value.CK2Directory.localisationDirectory, settings.Value.ck2Language)

      
  
        member val Results = results with get, set
        member __.Api = 
            {
                results = results
                values = values
                getDesc = getDesc
                getKeys = getKeys
            }

module YAMLLocalisationParser =
    open FParsec

    type Entry = {
        key : string
        value : string
        desc : string
    }

    type LocFile = {
        key : string
        entries : Entry list
    }
    
    //let key = charsTillString ":" true 1000 .>> spaces <?> "key"
    let key = many1CharsTill anyChar (pchar ':') .>> spaces <?> "key"
    //let descInner = (charsTillString "§")
    let stringThenEscaped = pipe2 (manyCharsTill (noneOf ['"']) (pchar '§')) (manyCharsTill anyChar (pstring "§!")) (+) <?> "escaped"
    //let desc = pipe2 (pchar '"') (many ((attempt stringThenEscaped) <|> manyCharsTill anyChar (pchar '"')) |>> List.reduce (+)) (fun a b -> string a + b)
    //let desc = between (pchar '"') (pchar '"') (charsTillString "\"" false 10000) .>> spaces <?> "desc"
    let desc = pipe3 (pchar '"' |>> string) (many (attempt stringThenEscaped) |>> List.fold (+) "")  (manyCharsTill (noneOf ['§']) (pchar '"')) (fun a b c -> string a + b + c) <?> "string"
    let value = digit .>> spaces <?> "version"
    let entry = pipe3 (key) (value) (desc .>> spaces) (fun k v d -> {key = k; value = string v; desc = d}) <?> "entry"
    let manyEntries = many entry <?> "entry list"
    let file = pipe2 (key) (many entry .>> eof) (fun k es -> {key = k; entries = es}) <?> "file"

    let parseLocFile filepath name = runParserOnFile file () filepath System.Text.Encoding.UTF8

module EU4Localisation =
    open YAMLLocalisationParser
    open LocalisationDomain
    open FParsec

    type EU4LocalisationService (localisationFolder : string, language : CK2Lang) =
        let mutable results : IDictionary<string, (bool * int * string)> = upcast new Dictionary<string, (bool * int * string)>()
        let mutable records : Entry list = []
        let addFile f = 
            match parseLocFile f f with
            | Success(v, _, _) -> records <- v.entries@records; (true, v.entries.Length, "")
            | Failure(msg, _, _) -> (false, 0, msg)
        let addFiles (x : string list) = List.map (fun f -> (f, addFile f)) x

        let values = records |> List.map (fun r -> (r.key, r.desc)) |> dict

        let getDesc x = records |> List.tryPick (fun r -> if r.key = x then Some r.desc else None) |> Option.defaultValue x

        let getKeys = records |> List.map (fun r -> r.key)

        do
            match Directory.Exists(localisationFolder) with
            | true -> 
                        let files = Directory.EnumerateFiles localisationFolder |> List.ofSeq |> List.sort
                        results <- addFiles files |> dict
            | false -> ()

        new (settings : IOptionsSnapshot<CK2Settings>) = EU4LocalisationService(settings.Value.EU4Directory.localisationDirectory, settings.Value.ck2Language)
        member __.Api =
            {       
                results = results
                values = values
                getDesc = getDesc
                getKeys = getKeys
            }