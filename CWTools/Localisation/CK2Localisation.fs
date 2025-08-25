namespace CWTools.Localisation

open System.IO
open System.Collections.Generic
open CWTools.Common
open CWTools.Utilities.Position
open CWTools.Utilities.Utils
open FParsec


module CK2Localisation =

    type CSVLocRow =
        { Code: string
          English: string
          French: string
          German: string
          Spanish: string }

    let escapeChar =
        skipChar '\\' >>. anyOf "\"\\/bfnrt,"
        |>> function
            | 'b' -> '\b'
            | 'f' -> '\u000C'
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | c -> c


    let nonQuotedCellChar = escapeChar <|> (noneOf ";\r\n")
    let cellChar = escapeChar <|> (noneOf "\"")
    let skipSemicolons = skipMany (skipChar ';')
    // Trys to parse quoted cells, if that fails try to parse non-quoted cells
    let cell =
        (between (skipChar '\"') (skipChar '\"') (manyChars cellChar)
         <|> manyChars nonQuotedCellChar)
        .>> skipSemicolons

    let row =
        pipe5 cell cell cell cell cell (fun c e f g s ->
            { CSVLocRow.Code = c
              English = e
              French = f
              German = g
              Spanish = s })

    let parseRow name text = runParserOnString row () name text
    // Cells are delimited by  a specified string
    // let row delim = sepBy (cell delim) (str delim)

    // type LocalisationEntry = CsvProvider<"#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x", ";",IgnoreErrors=false,Quote='~',HasHeaders=true,Encoding="1252">
    // type LocalisationEntryFallback = CsvProvider<"#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x\nPROV1013;Lori;;Lori;;;;;;;;;;;x", ";",IgnoreErrors=true,Quote='~',HasHeaders=true,Encoding="1252">

    type CK2LocalisationService(files: (string * string) list) =
        // let localisationFolder : string = localisationSettings.folder
        // let language : CK2Lang =
        //     match localisationSettings.language with
        //     | CK2 l -> l
        //     | _ -> failwith "Wrong language for localisation"
        let mutable csv: (range * CSVLocRow) list = [] //upcast LocalisationEntry. "#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x"
        // let mutable csvFallback : (range * LocalisationEntryFallback.Row) list = [] // upcast LocalisationEntryFallback.Parse "#CODE;ENGLISH;FRENCH;GERMAN;;SPANISH;;;;;;;;;x"

        let makeRangeForRow i fn = mkRange fn (mkPos i 0) (mkPos i 0)

        let addFile (filename, filetext: string) =
            // let retry file msg =
            //     let rows = LocalisationEntryFallback.ParseRows file |> List.ofSeq
            //     let rows = rows |> List.mapi (fun i r -> makeRangeForRow i filename, r)
            //     csvFallback <- rows @ csvFallback
            //     (false, rows.Length, msg, None)
            let fileRows =
                filetext.Split([| System.Environment.NewLine |], System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.filter (fun l -> l.StartsWith("#CODE") || not (l.StartsWith("#")))
                |> List.ofArray
            //|> String.concat "\n"
            // try
            let rows =
                fileRows
                |> List.map (parseRow filename)
                |> List.choose (function
                    | Success(r, _, _) -> Some r
                    | _ -> None) //LocalisationEntry.ParseRows file |> List.ofSeq

            let rows = rows |> List.mapi (fun i r -> makeRangeForRow i filename, r)
            csv <- rows @ csv
            (true, rows.Length, "", None)
        // with
        // | Failure msg ->
        //     try
        //         retry file msg
        //     with
        //     | Failure msg2 ->
        //         eprintfn "%A %A" msg msg2
        //         (false, 0, msg2, None)
        // | exn ->
        //     try
        //         retry file exn.Message
        //     with
        //     | Failure msg ->
        //         (false, 0, msg, None)

        let mutable results: Results =
            upcast new Dictionary<string, bool * int * string * FParsec.Position option>()

        let addFiles (x: (string * string) list) =
            List.map (fun (fn, fs) -> (fn, addFile (fn, fs))) x

        let getForLang language (x: CSVLocRow) =
            match language with
            | CK2 CK2Lang.English -> x.English
            | CK2 CK2Lang.French -> x.French
            | CK2 CK2Lang.German -> x.German
            | CK2 CK2Lang.Spanish -> x.Spanish
            | _ -> x.English

        // let getForLangFallback language (x : LocalisationEntryFallback.Row) =
        //     match language with
        //     |CK2 CK2Lang.English -> x.ENGLISH
        //     |CK2 CK2Lang.French -> x.FRENCH
        //     |CK2 CK2Lang.German -> x.GERMAN
        //     |CK2 CK2Lang.Spanish -> x.SPANISH
        //     |_ -> x.ENGLISH

        let getKeys () =
            csv |> Seq.map (snd >> (fun f -> f.Code)) |> Seq.toArray

        let valueMap lang =
            let one = csv |> Seq.map (fun (p, r) -> (p, r.Code, getForLang lang r))

            let range =
                mkRange (files |> List.tryHead |> Option.map fst |> Option.defaultValue "") (mkPos 0 0) (mkPos 0 0)

            one
            |> Seq.map (fun (p, k, v) ->
                k,
                { key = k
                  value = None
                  desc = v
                  position = p
                  errorRange = None })
            |> Map.ofSeq

        let values lang =
            let one = csv |> Seq.map (snd >> (fun f -> (f.Code, getForLang lang f)))
            one |> dict

        let getDesc lang x =
            let one = csv |> Seq.tryFind (snd >> (fun f -> f.Code = x)) |> Option.map snd

            match one with
            | Some x -> getForLang lang x
            | _ -> x

        do results <- addFiles files |> dict
        // do
        //     match Directory.Exists(localisationFolder) with
        //     | true ->
        //                 let files = Directory.EnumerateFiles localisationFolder |> List.ofSeq |> List.sort
        //                 results <- addFiles files |> dict
        //     | false -> ()
        //new (settings : CK2Settings) = CKLocalisationService(settings.CK2Directory.localisationDirectory, settings.ck2Language)
        new(localisationSettings: LocalisationSettings<CK2Lang>) =
            log (sprintf "Loading CK2 localisation in %s" localisationSettings.folder)

            match Directory.Exists(localisationSettings.folder) with
            | true ->
                let files =
                    Directory.EnumerateDirectories localisationSettings.folder
                    |> List.ofSeq
                    |> List.collect (Directory.EnumerateFiles >> List.ofSeq)

                let rootFiles = Directory.EnumerateFiles localisationSettings.folder |> List.ofSeq

                let actualFiles =
                    files @ rootFiles
                    |> List.map (fun f -> f, File.ReadAllText(f, System.Text.Encoding.UTF8))

                CK2LocalisationService(actualFiles)
            | false ->
                log (sprintf "%s not found" localisationSettings.folder)
                CK2LocalisationService([])



        member val Results = results with get, set

        member _.Api(lang: Lang) =
            { new ILocalisationAPI with
                member _.Results = results
                member _.Values = values lang
                member _.GetKeys = getKeys ()
                member _.GetDesc x = getDesc lang x
                member _.GetLang = lang
                member _.ValueMap = valueMap lang }

        interface ILocalisationAPICreator with
            member this.Api l = this.Api l
