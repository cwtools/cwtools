namespace CK2Events.Application.Localisation
open CK2Events.Application
open System.Collections.Generic
open Microsoft.Extensions.Options
open System.IO

module EU4Localisation =
    open YAMLLocalisationParser
    open LocalisationDomain
    open FParsec

    type EU4LocalisationService (localisationFolder : string, language : CK2Lang) =
        let language = language
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
            match parseLocFile f f with
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

        new (settings : IOptionsSnapshot<CK2Settings>) = EU4LocalisationService(settings.Value.EU4Directory.localisationDirectory, settings.Value.ck2Language)
        member __.Api =
            {       
                results = results
                values = values
                getDesc = getDesc
                getKeys = getKeys
            }