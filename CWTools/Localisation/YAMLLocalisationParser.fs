namespace CWTools.Localisation

open CWTools.Common
open CWTools.Utilities.Position
open System.Collections.Generic
open System.IO
open CWTools.Utilities.Utils

module YAMLLocalisationParser =
    open FParsec

    type LocFile = { key: string; entries: Entry list }

    let inline isLocValueChar (c: char) =
        isAsciiLetter c
        || (c >= '\u0020' && c <= '\u007E')
        || (c >= '\u00A0' && c <= '\u024F')
        || (c >= '\u0401' && c <= '\u045F')
        || (c >= '\u0490' && c <= '\u0491')
        || (c >= '\u1E00' && c <= '\u1EFF')
        || (c >= '\u2013' && c <= '\u2044')
        || (c >= '\u2460' && c <= '\u24FF')
        || (c >= '\u4E00' && c <= '\u9FFF')
        || (c >= '\u3000' && c <= '\u30FF')
        || (c >= '\uFE30' && c <= '\uFE4F')
        || (c >= '\uFF00' && c <= '\uFFEF')

    let key = many1Satisfy ((=) ':' >> not) .>> skipChar ':' .>> spaces <?> "key"

    let private desc =
        many1Satisfy isLocValueChar .>>. getPosition .>> restOfLine false <?> "desc"

    let value = digit .>> spaces <?> "version"

    let getRange (start: Position) (endp: Position) =
        mkRange start.StreamName (mkPos (int start.Line) (int start.Column)) (mkPos (int endp.Line) (int endp.Column))

    let entry =
        pipe5 getPosition key (opt value) desc (getPosition .>> spaces) (fun s k v (validDesc, endofValid) e ->
            let errorRange =
                if endofValid <> e then
                    Some(getRange endofValid e)
                else
                    None

            { key = k
              value = v
              desc = validDesc
              position = getRange s e
              errorRange = errorRange })
        <?> "entry"

    let comment = skipChar '#' >>. skipRestOfLine true .>> spaces <?> "comment"

    let file =
        spaces
        >>. skipMany (attempt comment)
        >>. pipe2 key (many ((attempt comment >>% None) <|> (entry |>> Some)) .>> eof) (fun k es ->
            { key = k; entries = List.choose id es })
        <?> "file"

    let parseLocFile filepath =
        runParserOnFile file () filepath System.Text.Encoding.UTF8

    let parseLocText text name = runParserOnString file () name text

    type YAMLLocalisationService<'L>(files: (string * string) list, keyToLanguage, gameLang) =
        let mutable results: Results =
            upcast new Dictionary<string, bool * int * string * Position option>()

        let mutable records: struct (Entry * Lang) array = [||]
        let mutable recordsL: struct (Entry * Lang) list = []

        let addFile f t =
            //log "%s" f
            match parseLocText t f with
            | Success({ key = key; entries = entries }, _, _) ->
                match keyToLanguage key with
                | Some l ->
                    let es = entries |> List.map (fun e -> struct (e, gameLang l))
                    recordsL <- es @ recordsL
                    (true, es.Length, "", None)
                | None -> (true, entries.Length, "", None)
            | Failure(msg, p, _) -> (false, 0, msg, Some p.Position)

        let addFiles (x: (string * string) list) =
            List.map (fun (f, t) -> (f, addFile f t)) x

        let recordsLang (lang: Lang) =
            records
            |> Array.choose (function
                | struct (r, l) when l = lang -> Some r
                | _ -> None)

        let valueMap lang =
            recordsLang lang |> Array.map (fun r -> (r.key, r)) |> Map.ofArray

        let values l =
            recordsLang l |> Array.map (fun r -> (r.key, r.desc)) |> dict

        let getDesc l x =
            recordsLang l
            |> Array.tryPick (fun r -> if r.key = x then Some r.desc else None)
            |> Option.defaultValue x

        let getKeys l = recordsLang l |> Array.map _.key

        do
            results <- addFiles files |> dict
            records <- recordsL |> Array.ofList
            recordsL <- []

        new(localisationSettings: LocalisationSettings<'L>) =
            log (sprintf "Loading %s localisation in %s" localisationSettings.gameName localisationSettings.folder)

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

                YAMLLocalisationService(
                    actualFiles,
                    localisationSettings.keyToLanguage,
                    localisationSettings.gameToLang
                )
            | false ->
                log (sprintf "%s not found" localisationSettings.folder)
                YAMLLocalisationService([], localisationSettings.keyToLanguage, localisationSettings.gameToLang)

        member __.Api lang =
            { new ILocalisationAPI with
                member __.Results = results
                member __.Values = values lang
                member __.GetKeys = getKeys lang
                member __.GetDesc x = getDesc lang x
                member __.GetLang = lang
                member __.ValueMap = valueMap lang }

        interface ILocalisationAPICreator with
            member this.Api l = this.Api l


module EU4 =
    open YAMLLocalisationParser

    let private keyToLanguage =
        function
        | "l_english" -> Some EU4Lang.English
        | "l_french" -> Some EU4Lang.French
        | "l_spanish" -> Some EU4Lang.Spanish
        | "l_german" -> Some EU4Lang.German
        | _ -> None

    let EU4LocalisationService (files: (string * string) list) =
        YAMLLocalisationService(files, keyToLanguage, EU4)

    let EU4LocalisationServiceFromFolder (folder: string) =
        YAMLLocalisationService
            { folder = folder
              gameName = "Europa Universalis IV"
              keyToLanguage = keyToLanguage
              gameToLang = EU4 }

module HOI4 =
    open YAMLLocalisationParser

    let private keyToLanguage =
        function
        | "l_english" -> Some HOI4Lang.English
        | "l_french" -> Some HOI4Lang.French
        | "l_spanish" -> Some HOI4Lang.Spanish
        | "l_german" -> Some HOI4Lang.German
        | "l_russian" -> Some HOI4Lang.Russian
        | "l_polish" -> Some HOI4Lang.Polish
        | "l_braz_por" -> Some HOI4Lang.Braz_Por
        | "l_simp_chinese" -> Some HOI4Lang.Chinese
        | "l_japanese" -> Some HOI4Lang.Japanese
        | _ -> None

    let HOI4LocalisationService (files: (string * string) list) =
        YAMLLocalisationService(files, keyToLanguage, HOI4)

    let HOI4LocalisationServiceFromFolder (folder: string) =
        YAMLLocalisationService
            { folder = folder
              gameName = "Hearts of Iron IV"
              keyToLanguage = keyToLanguage
              gameToLang = HOI4 }

module STL =
    open YAMLLocalisationParser

    let private keyToLanguage =
        function
        | "l_english" -> Some STLLang.English
        | "l_french" -> Some STLLang.French
        | "l_spanish" -> Some STLLang.Spanish
        | "l_german" -> Some STLLang.German
        | "l_russian" -> Some STLLang.Russian
        | "l_polish" -> Some STLLang.Polish
        | "l_braz_por" -> Some STLLang.Braz_Por
        | "l_simp_chinese" -> Some STLLang.Chinese
        | "l_japanese" -> Some STLLang.Japanese
        | "l_korean" -> Some STLLang.Korean
        | _ -> None

    let STLLocalisationService (files: (string * string) list) =
        YAMLLocalisationService(files, keyToLanguage, STL)

    let STLLocalisationServiceFromFolder (folder: string) =
        YAMLLocalisationService
            { folder = folder
              gameName = "Stellaris"
              keyToLanguage = keyToLanguage
              gameToLang = STL }

module IR =
    open YAMLLocalisationParser

    let private keyToLanguage =
        function
        | "l_english" -> Some IRLang.English
        | "l_french" -> Some IRLang.French
        | "l_german" -> Some IRLang.German
        | "l_spanish" -> Some IRLang.Spanish
        | "l_simp_chinese" -> Some IRLang.Chinese
        | "l_russian" -> Some IRLang.Russian
        | _ -> None

    let IRLocalisationService (files: (string * string) list) =
        YAMLLocalisationService(files, keyToLanguage, IR)

    let IRLocalisationServiceFromFolder (folder: string) =
        YAMLLocalisationService
            { folder = folder
              gameName = "Imperator"
              keyToLanguage = keyToLanguage
              gameToLang = IR }

module Custom =
    open YAMLLocalisationParser

    let private keyToLanguage =
        function
        | "l_english" -> Some CustomLang.English
        | "l_french" -> Some CustomLang.French
        | "l_german" -> Some CustomLang.German
        | "l_spanish" -> Some CustomLang.Spanish
        | "l_simp_chinese" -> Some CustomLang.Chinese
        | "l_russian" -> Some CustomLang.Russian
        | "l_polish" -> Some CustomLang.Polish
        | "l_braz_por" -> Some CustomLang.Braz_Por
        | "l_default" -> Some CustomLang.Default
        | _ -> None

    let CustomLocalisationService (files: (string * string) list) =
        YAMLLocalisationService(files, keyToLanguage, Custom)

    let CustomLocalisationServiceFromFolder (folder: string) =
        YAMLLocalisationService
            { folder = folder
              gameName = "Custom"
              keyToLanguage = keyToLanguage
              gameToLang = Custom }

module CK3 =
    open YAMLLocalisationParser

    let private keyToLanguage =
        function
        | "l_english" -> Some CK3Lang.English
        | "l_french" -> Some CK3Lang.French
        | "l_german" -> Some CK3Lang.German
        | "l_spanish" -> Some CK3Lang.Spanish
        | "l_simp_chinese" -> Some CK3Lang.Chinese
        | "l_russian" -> Some CK3Lang.Russian
        | "l_korean" -> Some CK3Lang.Korean
        | _ -> None

    let CK3LocalisationService (files: (string * string) list) =
        YAMLLocalisationService(files, keyToLanguage, CK3)

    let CK3LocalisationServiceFromFolder (folder: string) =
        YAMLLocalisationService
            { folder = folder
              gameName = "CK3"
              keyToLanguage = keyToLanguage
              gameToLang = CK3 }

module VIC3 =
    open YAMLLocalisationParser

    let private keyToLanguage =
        function
        | "l_english" -> Some VIC3Lang.English
        | "l_french" -> Some VIC3Lang.French
        | "l_german" -> Some VIC3Lang.German
        | "l_spanish" -> Some VIC3Lang.Spanish
        | "l_simp_chinese" -> Some VIC3Lang.Chinese
        | "l_russian" -> Some VIC3Lang.Russian
        | "l_korean" -> Some VIC3Lang.Korean
        | "l_japanese" -> Some VIC3Lang.Japanese
        | "l_braz_por" -> Some VIC3Lang.Braz_Por
        | "l_polish" -> Some VIC3Lang.Polish
        | "l_turkish" -> Some VIC3Lang.Turkish
        | _ -> None

    let VIC3LocalisationService (files: (string * string) list) =
        YAMLLocalisationService(files, keyToLanguage, VIC3)

    let VIC3LocalisationServiceFromFolder (folder: string) =
        YAMLLocalisationService
            { folder = folder
              gameName = "VIC3"
              keyToLanguage = keyToLanguage
              gameToLang = VIC3 }

module EU5 =
    open YAMLLocalisationParser

    let private keyToLanguage =
        function
        | "l_english" -> Some EU5Lang.English
        | "l_french" -> Some EU5Lang.French
        | "l_german" -> Some EU5Lang.German
        | "l_spanish" -> Some EU5Lang.Spanish
        | "l_simp_chinese" -> Some EU5Lang.Chinese
        | "l_russian" -> Some EU5Lang.Russian
        | "l_korean" -> Some EU5Lang.Korean
        | "l_japanese" -> Some EU5Lang.Japanese
        | "l_braz_por" -> Some EU5Lang.Braz_Por
        | "l_polish" -> Some EU5Lang.Polish
        | "l_turkish" -> Some EU5Lang.Turkish
        | _ -> None

    let EU5LocalisationService (files: (string * string) list) =
        YAMLLocalisationService(files, keyToLanguage, EU5)

    let EU5LocalisationServiceFromFolder (folder: string) =
        YAMLLocalisationService
            { folder = folder
              gameName = "EU5"
              keyToLanguage = keyToLanguage
              gameToLang = EU5 }
