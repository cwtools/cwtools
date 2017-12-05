namespace CK2Events.Application

open FSharp.Data
open System.IO
open Microsoft.Extensions.Options

module Localisation =
    type LocalisationEntry = CsvProvider<"./localization/v2_30c.csv", ";",IgnoreErrors=true,Quote='~'>

    type LocalisationService(settings:IOptions<CK2Settings>) =
        let settings : CK2Settings = settings.Value

        let files = match settings.localisationDirectory with |Some dir -> Directory.EnumerateFiles(dir) |> List.ofSeq |None -> []
        
        let allDesc : Runtime.CsvFile<LocalisationEntry.Row> option = 
            match files with
            | [x] -> Some (upcast LocalisationEntry.Load(x))
            | x::xs -> 
                let allButFirst = List.map (fun (f : string) -> LocalisationEntry.Load(f).Rows) xs |> List.collect Seq.toList
                let first = LocalisationEntry.Load(x)
                Some (first.Append(allButFirst))
            | [] -> None

        //let keys = allDesc.Rows |> Seq.map (fun x -> x.``#CODE``) |> List.ofSeq
        member __.GetDesc x = 
            match allDesc with
            | Some allDesc -> allDesc.Rows |> Seq.tryFind (fun f -> f.``#CODE`` = x) |> function |Some x -> x.ENGLISH |None -> x
            | None -> x
