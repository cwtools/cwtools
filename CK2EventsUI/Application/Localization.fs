namespace CK2_Events.Application

open FSharp.Data
open System.IO

module Localization =
    type LocalizationEntry = CsvProvider<"./localization/v2_30c.csv", ";",IgnoreErrors=true,Quote='~'>
    let files = Directory.EnumerateFiles("./localization/")
                |> List.ofSeq
    let allDesc = 
        let allButFirst = List.map (fun (f : string) -> LocalizationEntry.Load(f).Rows) files.Tail |> List.collect Seq.toList
        let first = LocalizationEntry.Load(files.Head)
        first.Append(allButFirst)
    let c = allDesc.Rows |> Seq.length
    let keys = allDesc.Rows |> Seq.map (fun x -> x.``#CODE``) |> List.ofSeq
    let GetDesc x = allDesc.Rows |> Seq.tryFind (fun f -> f.``#CODE`` = x) |> function |Some x -> x.ENGLISH |None -> x