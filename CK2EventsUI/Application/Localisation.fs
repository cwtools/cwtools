namespace CK2Events.Application

open FSharp.Data
open System.IO
open Microsoft.Extensions.Options

module Localisation =
    open System.Collections.Generic
    type LocalisationEntry = CsvProvider<"#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x", ";",IgnoreErrors=false,Quote='~',HasHeaders=true,Encoding="1252">
    type LocalisationEntryFallback = CsvProvider<"#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x", ";",IgnoreErrors=true,Quote='~',HasHeaders=true,Encoding="1252">


    type LocalisationService(localisationDirectory : string, language : CK2Lang) as this =
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

        do
            match Directory.Exists(localisationFolder) with
            | true -> 
                        let files = Directory.EnumerateFiles localisationFolder |> List.ofSeq |> List.sort
                        results <- addFiles files |> dict
            | false -> ()
        new (settings : IOptionsSnapshot<CK2Settings>) = LocalisationService(settings.Value.CK2Directory.localisationDirectory, settings.Value.ck2Language)

        member val Results = results with get, set

        member __.GetKeys = csv.Rows |> Seq.map (fun f -> f.``#CODE``) |> List.ofSeq

        member __.Values = 
            let one = csv.Rows |> Seq.map(fun f -> (f.``#CODE``, getForLang f))
            let two = csvFallback.Rows |> Seq.map(fun f -> (f.``#CODE``, getForLangFallback f))
            Seq.concat [one; two] |> dict



        member __.GetDesc x =
            let one = csv.Rows |> Seq.tryFind (fun f -> f.``#CODE`` = x) 
            let two = csvFallback.Rows |> Seq.tryFind (fun f -> f.``#CODE`` = x)
            match (one, two) with
            | (Some x, _) -> getForLang x
            | (None, Some x) -> getForLangFallback x
            | _ -> x
