namespace CWTools.Localisation
open System.Collections.Generic
open System.IO
open CWTools.Common

module STLLocalisation =
    open YAMLLocalisationParser
    open FParsec

    type STLLocalisationService(files : (string * string) list) =
        // let language : STLLang = 
        //     match lang with
        //     | STL l -> l
        //     | _ -> failwith "Wrong language for localisation"
        // let languageKey =
        //     match language with
        //     |STLLang.English -> "l_english"
        //     |STLLang.French -> "l_french"
        //     |STLLang.Spanish -> "l_spanish"
        //     |STLLang.German -> "l_german"
        //     |STLLang.Russian -> "l_russian"
        //     |STLLang.Polish -> "l_polish"
        //     |STLLang.Braz_Por -> "l_braz_por"
        //     |STLLang.Default -> "l_default"
        //     |_ -> failwith "Unknown language enum value"
        let keyToLanguage =
            function
            |"l_english" -> Some STLLang.English
            |"l_french" -> Some STLLang.French
            |"l_spanish" -> Some STLLang.Spanish
            |"l_german" -> Some STLLang.German
            |"l_russian" -> Some STLLang.Russian
            |"l_polish" -> Some STLLang.Polish
            |"l_braz_por" -> Some STLLang.Braz_Por
            |"l_default" -> Some STLLang.Default
            |_ -> None
        let mutable results : IDictionary<string, (bool * int * string)> = upcast new Dictionary<string, (bool * int * string)>()
        let mutable records : (Entry * Lang) list = []
        let addFile f t = 
            //eprintfn "%s" f
            match parseLocText t f with
            | Success({key = key; entries = entries}, _, _) ->
                match keyToLanguage key with
                |Some l -> 
                    let es = entries |> List.map (fun e -> e, STL l)
                    records <- es@records; (true, es.Length, "")
                |None ->
                    (true, entries.Length, "")
            | Failure(msg, _, _) -> 
                (false, 0, msg)
        let addFiles (x : (string * string) list) = List.map (fun (f, t )-> (f, addFile f t)) x

        let recordsLang (lang : Lang) = records |> List.choose (function |(r, l) when l = lang -> Some r |_ -> None)
        let valueMap lang = recordsLang lang |> List.map (fun r -> (r.key, r)) |> Map.ofList
        let values lang = recordsLang lang |> List.map (fun r -> (r.key, r.desc)) |> dict

        let getDesc lang x = recordsLang lang |> List.tryPick (fun r -> if r.key = x then Some r.desc else None) |> Option.defaultValue x

        let getKeys lang = recordsLang lang |> List.map (fun r -> r.key)
        
        do
            results <- addFiles files |> dict

        new(localisationSettings : LocalisationSettings) =
            eprintfn "Loading STL localisation in %s" localisationSettings.folder 
            match Directory.Exists(localisationSettings.folder) with
            | true -> 
                        let files = Directory.EnumerateDirectories localisationSettings.folder 
                                        |> List.ofSeq
                                        |> List.collect (Directory.EnumerateFiles >> List.ofSeq)
                        let rootFiles = Directory.EnumerateFiles localisationSettings.folder |> List.ofSeq
                        let actualFiles = files @ rootFiles |> List.map (fun f -> f, File.ReadAllText(f, System.Text.Encoding.UTF8))
                        STLLocalisationService(actualFiles)
            | false -> 
                eprintfn "%s not found" localisationSettings.folder
                STLLocalisationService([])
        member __.Api lang = {
            new ILocalisationAPI with
                member __.Results = results
                member __.Values = values lang 
                member __.GetKeys = getKeys lang
                member __.GetDesc x = getDesc lang x
                member __.GetLang = lang
                member __.ValueMap = valueMap lang
            }