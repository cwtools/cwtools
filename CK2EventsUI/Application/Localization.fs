namespace CK2_Events.Application

open FSharp.Data
open Microsoft.AspNetCore.Authorization.Infrastructure
open NodaTime

module Localization =
    type LocalizationEntry = CsvProvider<"./localization/v2_30c.csv", ";",IgnoreErrors=true,Quote='~'>
    let WOLDescs = LocalizationEntry.Load("./localization/v2_30c.csv")
    let c = WOLDescs.Rows |> Seq.length
    let keys = WOLDescs.Rows |> Seq.map (fun x -> x.``#CODE``) |> List.ofSeq
    let GetDesc x = WOLDescs.Rows |> Seq.tryFind (fun f -> f.``#CODE`` = x) |> function |Some x -> x.ENGLISH |None -> x