namespace CWTools.Localisation
open System.Collections.Generic
open System.IO
open CWTools.Common

module EU4Localisation =
    open YAMLLocalisationParser
    open FParsec

    type EU4LocalisationService(localisationSettings : LocalisationSettings) =
        let localisationFolder : string = localisationSettings.folder
        let language : CK2Lang = 
            match localisationSettings.language with
                | CK2 l -> l
        let languageKey =
            match language with
            |CK2Lang.English -> "l_english"
            |CK2Lang.French -> "l_french"
            |CK2Lang.Spanish -> "l_spanish"
            |CK2Lang.German -> "l_german"
            |_ -> failwith "Unknown language enum value"
        let mutable results : IDictionary<string, (bool * int * string)> = upcast new Dictionary<string, (bool * int * string)>()
        let mutable records : Entry list = []
        let addFile f = 
            match parseLocFile f with
            | Success({key = key; entries = entries}, _, _) when key = languageKey -> records <- entries@records; (true, entries.Length, "")
            | Success(v, _, _) -> (true, v.entries.Length, "")
            | Failure(msg, _, _) -> (false, 0, msg)
        let addFiles (x : string list) = List.map (fun f -> (f, addFile f)) x

        let values = records |> List.map (fun r -> (r.key, r.desc)) |> dict

        let getDesc x = records |> List.tryPick (fun r -> if r.key = x then Some r.desc else None) |> Option.defaultValue x

        let getKeys = records |> List.map (fun r -> r.key)

        do
            match Directory.Exists(localisationFolder) with
            | true -> 
                        let files = Directory.EnumerateFiles localisationFolder |> List.ofSeq |> List.sort
                        results <- addFiles files |> dict
            | false -> ()

        //new (settings : CK2Settings) = EU4LocalisationService(settings.EU4Directory.localisationDirectory, settings.ck2Language)
        member __.Api = {
            new ILocalisationAPI with
                member __.Results = results
                member __.Values = values
                member __.GetKeys = getKeys
                member __.GetDesc x = getDesc x
                member this.GetLang = CK2 language
            }