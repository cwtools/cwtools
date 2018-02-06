namespace CWTools.Localisation
open System.Collections.Generic
open System.IO
open CWTools.Common

module STLLocalisation =
    open YAMLLocalisationParser
    open FParsec

    type STLLocalisationService(files : (string * string) list, lang : Lang) =
        let language : STLLang = 
            match lang with
            | STL l -> l
            | _ -> failwith "Wrong language for localisation"
        let languageKey =
            match language with
            |STLLang.English -> "l_english"
            |STLLang.French -> "l_french"
            |STLLang.Spanish -> "l_spanish"
            |STLLang.German -> "l_german"
            |STLLang.Russian -> "l_russian"
            |STLLang.Polish -> "l_polish"
            |STLLang.Braz_Por -> "l_braz_por"
            |STLLang.Default -> "l_default"
            |_ -> failwith "Unknown language enum value"
        let mutable results : IDictionary<string, (bool * int * string)> = upcast new Dictionary<string, (bool * int * string)>()
        let mutable records : Entry list = []
        let addFile f t = 
            //eprintfn "%s" f
            match parseLocText t f with
            | Success({key = key; entries = entries}, _, _) when key = languageKey ->
                //eprintfn "%A %s %i" lang key entries.Length
                records <- entries@records; (true, entries.Length, "")
            | Success({key = key; entries = entries}, _, _) -> 
                //eprintfn "%A %s %i" lang key entries.Length
                (true, entries.Length, "")
            | Failure(msg, _, _) -> 
                //eprintfn "%s %s" f msg
                (false, 0, msg)
        let addFiles (x : (string * string) list) = List.map (fun (f, t )-> (f, addFile f t)) x

        let values() = records |> List.map (fun r -> (r.key, r.desc)) |> dict

        let getDesc x = records |> List.tryPick (fun r -> if r.key = x then Some r.desc else None) |> Option.defaultValue x

        let getKeys() = records |> List.map (fun r -> r.key)
        
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
                        STLLocalisationService(actualFiles, localisationSettings.language)
            | false -> 
                eprintfn "%s not found" localisationSettings.folder
                STLLocalisationService([], localisationSettings.language)


        member __.Api = {
            new ILocalisationAPI with
                member __.Results = results
                member __.Values = values()
                member __.GetKeys = getKeys()
                member __.GetDesc x = getDesc x
                member __.GetLang = STL language
            }