namespace CWTools.Localisation
open System.Collections.Generic
open System.IO
open CWTools.Common

module EU4Localisation =
    open YAMLLocalisationParser
    open FParsec

    type EU4LocalisationService(files : (string * string) list) =
        // let localisationFolder : string = localisationSettings.folder
        // let language : CK2Lang =
        //     match localisationSettings.language with
        //         | CK2 l -> l
        //         | _ -> failwith "Wrong language for localisation"

        // let languageKey =
        //     match language with
        //     |CK2Lang.English -> "l_english"
        //     |CK2Lang.French -> "l_french"
        //     |CK2Lang.Spanish -> "l_spanish"
        //     |CK2Lang.German -> "l_german"
        //     |_ -> failwith "Unknown language enum value"

        let keyToLanguage =
            function
            |"l_english" -> Some EU4Lang.English
            |"l_french" -> Some EU4Lang.French
            |"l_spanish" -> Some EU4Lang.Spanish
            |"l_german" -> Some EU4Lang.German
            |_ -> None
        let mutable results : IDictionary<string, (bool * int * string)> = upcast new Dictionary<string, (bool * int * string)>()
        let mutable records : struct (Entry * Lang) array = [||]
        let mutable recordsL : struct (Entry * Lang) list = []

        let addFile f t =
            //eprintfn "%s" f
            match parseLocText t f with
            | Success({key = key; entries = entries}, _, _) ->
                match keyToLanguage key with
                |Some l ->
                    let es = entries |> List.map (fun e -> struct (e, EU4 l))
                    recordsL <- es@recordsL; (true, es.Length, "")
                |None ->
                    (true, entries.Length, "")
            | Failure(msg, _, _) ->
                (false, 0, msg)

        // let addFile f =
        //     match parseLocFile f with
        //     | Success({key = key; entries = entries}, _, _) ->
        //         match keyToLanguage key with
        //         |Some l ->
        //             let es = entries |> List.map (fun e -> e, CK2 l)
        //             records <- es@records; (true, es.Length, "")
        //         |None -> (true, entries.Length, "")
        //     | Failure(msg, _, _) -> (false, 0, msg)
        // let addFiles (x : string list) = List.map (fun f -> (f, addFile f)) x
        let addFiles (x : (string * string) list) = List.map (fun (f, t )-> (f, addFile f t)) x

        let recordsLang (lang : Lang) = records |> Array.choose (function |struct (r, l) when l = lang -> Some r |_ -> None) |> List.ofArray

        let valueMap lang = recordsLang lang |> List.map (fun r -> (r.key, r)) |> Map.ofList

        let values l = recordsLang l |> List.map (fun r -> (r.key, r.desc)) |> dict

        let getDesc l x = recordsLang l |> List.tryPick (fun r -> if r.key = x then Some r.desc else None) |> Option.defaultValue x

        let getKeys l = recordsLang l |> List.map (fun r -> r.key)

        do
            results <- addFiles files |> dict
            records <- recordsL |> Array.ofList
            recordsL <- []


        // do
        //     match Directory.Exists(localisationFolder) with
        //     | true ->
        //                 let files = Directory.EnumerateFiles localisationFolder |> List.ofSeq |> List.sort
        //                 results <- addFiles files |> dict
        //     | false -> ()
        new(localisationSettings : LocalisationSettings) =
            eprintfn "Loading EU4 localisation in %s" localisationSettings.folder
            match Directory.Exists(localisationSettings.folder) with
            | true ->
                        let files = Directory.EnumerateDirectories localisationSettings.folder
                                        |> List.ofSeq
                                        |> List.collect (Directory.EnumerateFiles >> List.ofSeq)
                        let rootFiles = Directory.EnumerateFiles localisationSettings.folder |> List.ofSeq
                        let actualFiles = files @ rootFiles |> List.map (fun f -> f, File.ReadAllText(f, System.Text.Encoding.UTF8))
                        EU4LocalisationService(actualFiles)
            | false ->
                eprintfn "%s not found" localisationSettings.folder
                EU4LocalisationService([])
        //new (settings : CK2Settings) = HOI4LocalisationService(settings.HOI4Directory.localisationDirectory, settings.ck2Language)

        //new (settings : CK2Settings) = EU4LocalisationService(settings.EU4Directory.localisationDirectory, settings.ck2Language)
        member __.Api lang = {
            new ILocalisationAPI with
                member __.Results = results
                member __.Values = values lang
                member __.GetKeys = getKeys lang
                member __.GetDesc x = getDesc lang x
                member __.GetLang = lang
                member __.ValueMap = valueMap lang
            }