namespace CK2Events.Application

open FSharp.Data
open System.IO
open Microsoft.Extensions.Options

module Localisation =
    open System.Collections.Generic
    type LocalisationEntry = CsvProvider<"#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x", ";",IgnoreErrors=false,Quote='~',HasHeaders=true>
    type LocalisationEntryFallback = CsvProvider<"#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x", ";",IgnoreErrors=true,Quote='~',HasHeaders=true>


    type LocalisationService(settings:IOptions<CK2Settings>) as this =
        let settings : CK2Settings = settings.Value
        let mutable csv : Runtime.CsvFile<LocalisationEntry.Row> = upcast LocalisationEntry.Parse "#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x"
        let mutable csvFallback : Runtime.CsvFile<LocalisationEntryFallback.Row> = upcast LocalisationEntryFallback.Parse "#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x"
        let addFile (x : string) =
            let retry file msg =
                let rows = LocalisationEntryFallback.ParseRows file |> List.ofSeq
                csvFallback <- csvFallback.Append(rows)
                (false, rows.Length, msg)
            let file = 
                File.ReadAllLines x 
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

        do
            match Directory.Exists(settings.localisationDirectory) with
            | true -> 
                        let files = Directory.EnumerateFiles settings.localisationDirectory |> List.ofSeq |> List.sort
                        results <- addFiles files |> dict
            | false -> ()
        member val Results = results with get, set

        member __.GetKeys = csv.Rows |> Seq.map (fun f -> f.``#CODE``) |> List.ofSeq

        member __.Values = 
            let one = csv.Rows |> Seq.map(fun f -> (f.``#CODE``, f.ENGLISH))
            let two = csvFallback.Rows |> Seq.map(fun f -> (f.``#CODE``, f.ENGLISH))
            Seq.concat [one; two] |> dict

        member __.GetDesc x =
            let one = csv.Rows |> Seq.tryFind (fun f -> f.``#CODE`` = x) 
            let two = csvFallback.Rows |> Seq.tryFind (fun f -> f.``#CODE`` = x)
            match (one, two) with
            | (Some x, _) -> x.ENGLISH
            | (None, Some x) -> x.ENGLISH
            | _ -> x
