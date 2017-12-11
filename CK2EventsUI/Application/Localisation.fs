namespace CK2Events.Application

open FSharp.Data
open System.IO
open Microsoft.Extensions.Options

module Localisation =
    type LocalisationEntry = CsvProvider<"./localization/v2_30c.csv", ";",IgnoreErrors=false,Quote='~',HasHeaders=true>
    type LocalisationEntryFallback = CsvProvider<"./localization/v2_30c.csv", ";",IgnoreErrors=true,Quote='~',HasHeaders=true>


    type LocalisationService(settings:IOptions<CK2Settings>) as this =
        let settings : CK2Settings = settings.Value
        let mutable csv : Runtime.CsvFile<LocalisationEntry.Row> = upcast LocalisationEntry.Parse "#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x"
        let mutable csvFallback : Runtime.CsvFile<LocalisationEntryFallback.Row> = upcast LocalisationEntryFallback.Parse "#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x"
        member __.Results =
            let files = Directory.EnumerateFiles settings.localisationDirectory |> List.ofSeq |> List.sort
            this.AddFiles files |> dict
        member this.AddFile (x : string) =
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
        
        member this.AddFiles (x : string list) = List.map (fun f -> (f, this.AddFile f)) x

        member __.GetDesc x =
            let one = csv.Rows |> Seq.tryFind (fun f -> f.``#CODE`` = x) 
            let two = csvFallback.Rows |> Seq.tryFind (fun f -> f.``#CODE`` = x)
            match (one, two) with
            | (Some x, _) -> x.ENGLISH
            | (None, Some x) -> x.ENGLISH
            | _ -> x
