namespace CWTools.Localisation
open System.Collections.Generic
open System.IO
open CWTools.Common

module STLLocalisation =
    open YAMLLocalisationParser
    open FParsec

    type STLLocalisationService(localisationSettings : LocalisationSettings) =
        let localisationFolder : string = localisationSettings.folder
        let language : STLLang = 
            match localisationSettings.language with
            | STL l -> l

        let languageKey =
            match language with
            |STLLang.English -> "l_english"
            |STLLang.French -> "l_french"
            |STLLang.Spanish -> "l_spanish"
            |STLLang.German -> "l_german"
            |STLLang.Russian -> "l_russian"
            |STLLang.Polish -> "l_polish"
            |STLLang.Braz_Por -> "l_braz_por"
            |_ -> failwith "Unknown language enum value"
        let mutable results : IDictionary<string, (bool * int * string)> = upcast new Dictionary<string, (bool * int * string)>()
        let mutable records : Entry list = []
        let addFile f = 
            eprintfn "%s" f
            match parseLocFile f with
            | Success({key = key; entries = entries}, _, _) when key = languageKey ->
                eprintfn "%s %i" key entries.Length
                records <- entries@records; (true, entries.Length, "")
            | Success(v, _, _) -> (true, v.entries.Length, "")
            | Failure(msg, _, _) -> 
                eprintfn "%s %s" f msg
                (false, 0, msg)
        let addFiles (x : string list) = List.map (fun f -> (f, addFile f)) x

        let values() = records |> List.map (fun r -> (r.key, r.desc)) |> dict

        let getDesc x = records |> List.tryPick (fun r -> if r.key = x then Some r.desc else None) |> Option.defaultValue x

        let getKeys() = records |> List.map (fun r -> r.key)

        do
            eprintfn "Loading STL localisation in %s" localisationFolder 
            match Directory.Exists(localisationFolder) with
            | true -> 
                        let files = Directory.EnumerateDirectories localisationFolder 
                                        |> List.ofSeq
                                        |> List.collect (Directory.EnumerateFiles >> List.ofSeq)
                        let rootFiles = Directory.EnumerateFiles localisationFolder |> List.ofSeq
                        results <- addFiles (files @ rootFiles)|> dict
                        eprintfn "%A" (getKeys())
            | false -> eprintfn "%s not found" localisationFolder

        member __.Api = {
            new ILocalisationAPI with
                member __.Results = results
                member __.Values = values()
                member __.GetKeys = getKeys()
                member __.GetDesc x = getDesc x
                member this.GetLang = STL language
            }