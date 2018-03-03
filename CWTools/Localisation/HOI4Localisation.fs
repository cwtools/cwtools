namespace CWTools.Localisation
open System.Collections.Generic
open System.IO
open CWTools.Common

module HOI4Localisation =
    open YAMLLocalisationParser
    open FParsec

    type HOI4LocalisationService(localisationSettings : LocalisationSettings) =
        let localisationFolder : string = localisationSettings.folder
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
            |"l_english" -> Some CK2Lang.English
            |"l_french" -> Some CK2Lang.French
            |"l_spanish" -> Some CK2Lang.Spanish
            |"l_german" -> Some CK2Lang.German
            |_ -> None
        let mutable results : IDictionary<string, (bool * int * string)> = upcast new Dictionary<string, (bool * int * string)>()
        let mutable records : (Entry * Lang) list = []
        let addFile f = 
            match parseLocFile f with
            | Success({key = key; entries = entries}, _, _) ->
                match keyToLanguage key with
                |Some l ->
                    let es = entries |> List.map (fun e -> e, CK2 l)
                    records <- es@records; (true, es.Length, "")
                |None -> (true, entries.Length, "")
            | Failure(msg, _, _) -> (false, 0, msg)
        let addFiles (x : string list) = List.map (fun f -> (f, addFile f)) x
        let recordsLang (lang : Lang) = records |> List.choose (function |(r, l) when l = lang -> Some r |_ -> None)

        let values l = recordsLang l |> List.map (fun r -> (r.key, r.desc)) |> dict

        let getDesc l x = recordsLang l |> List.tryPick (fun r -> if r.key = x then Some r.desc else None) |> Option.defaultValue x

        let getKeys l = recordsLang l |> List.map (fun r -> r.key)

        do
            match Directory.Exists(localisationFolder) with
            | true -> 
                        let files = Directory.EnumerateFiles localisationFolder |> List.ofSeq |> List.sort
                        results <- addFiles files |> dict
            | false -> ()

        //new (settings : CK2Settings) = HOI4LocalisationService(settings.HOI4Directory.localisationDirectory, settings.ck2Language)
        member __.Api lang= {
            new ILocalisationAPI with
                member __.Results = results
                member __.Values = values lang
                member __.GetKeys = getKeys lang
                member __.GetDesc x = getDesc lang x
                member __.GetLang = lang
            }