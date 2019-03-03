namespace CWTools.Localisation

open FSharp.Data
open System.IO
open System.Collections.Generic
open CWTools.Common
open FSharp.Data.Runtime
open CWTools.Utilities.Position
open CWTools.Utilities.Utils


module CK2Localisation =
    type LocalisationEntry = CsvProvider<"#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x", ";",IgnoreErrors=false,Quote='~',HasHeaders=true,Encoding="1252">
    type LocalisationEntryFallback = CsvProvider<"#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x", ";",IgnoreErrors=true,Quote='~',HasHeaders=true,Encoding="1252">

    type CK2LocalisationService(files : (string * string) list) =
        // let localisationFolder : string = localisationSettings.folder
        // let language : CK2Lang =
        //     match localisationSettings.language with
        //     | CK2 l -> l
        //     | _ -> failwith "Wrong language for localisation"
        let mutable csv : (range * LocalisationEntry.Row) list = [] //upcast LocalisationEntry. "#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x"
        let mutable csvFallback : (range * LocalisationEntryFallback.Row) list = [] // upcast LocalisationEntryFallback.Parse "#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x"

        let makeRangeForRow i fn =
            mkRange fn (mkPos i 0) (mkPos i 0)
        let addFile (filename, filetext : string) =
            let retry file msg =
                let rows = LocalisationEntryFallback.ParseRows file |> List.ofSeq
                let rows = rows |> List.mapi (fun i r -> makeRangeForRow i filename, r)
                csvFallback <- rows @ csvFallback
                (false, rows.Length, msg, None)
            let file =
                filetext.Split([|System.Environment.NewLine|], System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.filter(fun l -> l.StartsWith("#CODE") || not(l.StartsWith("#")))
                |> String.concat "\n"
            try
                let rows = LocalisationEntry.ParseRows file |> List.ofSeq
                let rows = rows |> List.mapi (fun i r -> makeRangeForRow i filename, r)
                csv <- rows @ csv
                (true, rows.Length, "", None)
            with
            | Failure msg ->
                try
                    retry file msg
                with
                | Failure msg ->
                    (false, 0, msg, None)
            | exn ->
                try
                    retry file exn.Message
                with
                | Failure msg ->
                    (false, 0, msg, None)

        let mutable results : Results = upcast new Dictionary<string, (bool * int * string * FParsec.Position option)>()
        let addFiles (x : (string * string) list) = List.map (fun (fn, fs) -> (fn, addFile (fn, fs))) x

        let getForLang language (x : LocalisationEntry.Row) =
            match language with
            |CK2 CK2Lang.English -> x.ENGLISH
            |CK2 CK2Lang.French -> x.FRENCH
            |CK2 CK2Lang.German -> x.GERMAN
            |CK2 CK2Lang.Spanish -> x.SPANISH
            |_ -> x.ENGLISH

        let getForLangFallback language (x : LocalisationEntryFallback.Row) =
            match language with
            |CK2 CK2Lang.English -> x.ENGLISH
            |CK2 CK2Lang.French -> x.FRENCH
            |CK2 CK2Lang.German -> x.GERMAN
            |CK2 CK2Lang.Spanish -> x.SPANISH
            |_ -> x.ENGLISH

        let getKeys() = csvFallback |> Seq.map (snd >> (fun f -> f.``#CODE``)) |> List.ofSeq
        let valueMap lang =
            let one = csv |> Seq.map(fun (p, r)-> (p, r.``#CODE``, getForLang lang r))
            let two = csvFallback |> Seq.map(fun (p, r) -> (p, r.``#CODE``, getForLangFallback lang r))
            let range = mkRange (files |> List.tryHead |> Option.map fst |> Option.defaultValue "") (mkPos 0 0) (mkPos 0 0)
            Seq.concat [one; two] |> Seq.map (fun (p, k, v) -> k, {key = k; value = None; desc = v; position = p })
                                  |> Map.ofSeq
        let values lang =
            let one = csv |> Seq.map( snd >> (fun f -> (f.``#CODE``, getForLang lang f)))
            let two = csvFallback |> Seq.map( snd >> (fun f -> (f.``#CODE``, getForLangFallback lang f)))
            Seq.concat [one; two] |> dict

        let getDesc lang x =
            let one = csv |> Seq.tryFind ( snd >> (fun f -> f.``#CODE`` = x)) |> Option.map snd
            let two = csvFallback |> Seq.tryFind ( snd >> (fun f -> f.``#CODE`` = x)) |> Option.map snd
            match (one, two) with
            | (Some x, _) -> getForLang lang x
            | (None, Some x) -> getForLangFallback lang x
            | _ -> x

        do
            results <- addFiles (files) |> dict
        // do
        //     match Directory.Exists(localisationFolder) with
        //     | true ->
        //                 let files = Directory.EnumerateFiles localisationFolder |> List.ofSeq |> List.sort
        //                 results <- addFiles files |> dict
        //     | false -> ()
        //new (settings : CK2Settings) = CKLocalisationService(settings.CK2Directory.localisationDirectory, settings.ck2Language)
        new(localisationSettings : LocalisationSettings) =
            log (sprintf "Loading CK2 localisation in %s" localisationSettings.folder)
            match Directory.Exists(localisationSettings.folder) with
            | true ->
                        let files = Directory.EnumerateDirectories localisationSettings.folder
                                        |> List.ofSeq
                                        |> List.collect (Directory.EnumerateFiles >> List.ofSeq)
                        let rootFiles = Directory.EnumerateFiles localisationSettings.folder |> List.ofSeq
                        let actualFiles = files @ rootFiles |> List.map (fun f -> f, File.ReadAllText(f, System.Text.Encoding.UTF8))
                        CK2LocalisationService(actualFiles)
            | false ->
                log (sprintf "%s not found" localisationSettings.folder)
                CK2LocalisationService([])



        member val Results = results with get, set

        member __.Api (lang : Lang) = {
            new ILocalisationAPI with
                member __.Results = results
                member __.Values = values lang
                member __.GetKeys = getKeys()
                member __.GetDesc x = getDesc lang x
                member __.GetLang = lang
                member __.ValueMap = valueMap lang
            }

        interface ILocalisationAPICreator with
            member this.Api l = this.Api l
