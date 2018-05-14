namespace CWTools.Localisation

open FSharp.Data
open System.IO
open System.Collections.Generic
open CWTools.Common
open FSharp.Data.Runtime
open Microsoft.FSharp.Compiler.Range


module CK2Localisation =
    type LocalisationEntry = CsvProvider<"#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x", ";",IgnoreErrors=false,Quote='~',HasHeaders=true,Encoding="1252">
    type LocalisationEntryFallback = CsvProvider<"#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x", ";",IgnoreErrors=true,Quote='~',HasHeaders=true,Encoding="1252">

    type CK2LocalisationService(localisationSettings : LocalisationSettings) =
        let localisationFolder : string = localisationSettings.folder
        // let language : CK2Lang = 
        //     match localisationSettings.language with
        //     | CK2 l -> l
        //     | _ -> failwith "Wrong language for localisation"
        let mutable csv : LocalisationEntry.Row list = [] //upcast LocalisationEntry. "#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x"
        let mutable csvFallback : LocalisationEntryFallback.Row list = [] // upcast LocalisationEntryFallback.Parse "#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x"
        let addFile (x : string) =
            let retry file msg =
                let rows = LocalisationEntryFallback.ParseRows file |> List.ofSeq
                csvFallback <- rows @ csvFallback
                (false, rows.Length, msg)
            let file = 
                File.ReadAllLines(x, System.Text.Encoding.GetEncoding(1252))
                |> Array.filter(fun l -> l.StartsWith("#CODE") || not(l.StartsWith("#")))
                |> String.concat "\n"
            try
                let rows = LocalisationEntry.ParseRows file |> List.ofSeq
                csv <- rows @ csv
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

        let getForLang language (x : LocalisationEntry.Row) =
            match language with
            |CK2Lang.English -> x.ENGLISH
            |CK2Lang.French -> x.FRENCH
            |CK2Lang.German -> x.GERMAN
            |CK2Lang.Spanish -> x.SPANISH
            |_ -> x.ENGLISH
                
        let getForLangFallback language (x : LocalisationEntryFallback.Row) =
            match language with
            |CK2Lang.English -> x.ENGLISH
            |CK2Lang.French -> x.FRENCH
            |CK2Lang.German -> x.GERMAN
            |CK2Lang.Spanish -> x.SPANISH
            |_ -> x.ENGLISH

        let getKeys = csv |> Seq.map (fun f -> f.``#CODE``) |> List.ofSeq
        let valueMap lang = 
            let one = csv |> Seq.map(fun f -> (f.``#CODE``, getForLang lang f))
            let two = csvFallback |> Seq.map(fun f -> (f.``#CODE``, getForLangFallback lang f))
            Seq.concat [one; two] |> Seq.map (fun (k, v) -> k, {key = k; value = None; desc = v; position = range.Zero})
                                  |> Map.ofSeq
        let values lang = 
            let one = csv |> Seq.map(fun f -> (f.``#CODE``, getForLang lang f))
            let two = csvFallback |> Seq.map(fun f -> (f.``#CODE``, getForLangFallback lang f))
            Seq.concat [one; two] |> dict

        let getDesc lang x =
            let one = csv |> Seq.tryFind (fun f -> f.``#CODE`` = x) 
            let two = csvFallback |> Seq.tryFind (fun f -> f.``#CODE`` = x)
            match (one, two) with
            | (Some x, _) -> getForLang lang x
            | (None, Some x) -> getForLangFallback lang x
            | _ -> x
        
        do
            match Directory.Exists(localisationFolder) with
            | true -> 
                        let files = Directory.EnumerateFiles localisationFolder |> List.ofSeq |> List.sort
                        results <- addFiles files |> dict
            | false -> ()
        //new (settings : CK2Settings) = CKLocalisationService(settings.CK2Directory.localisationDirectory, settings.ck2Language)

      
  
        member val Results = results with get, set

        member __.Api lang = {
            new ILocalisationAPI with
                member __.Results = results
                member __.Values = values lang
                member __.GetKeys = getKeys
                member __.GetDesc x = getDesc lang x
                member __.GetLang = CK2 lang
                member __.ValueMap = valueMap lang
            }