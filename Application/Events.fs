namespace CK2_Events.Application

open System.IO

module Events =
    let parseAll =
        Directory.EnumerateFiles("events")
        |> Seq.map (CKParser.parseEventFile >> CKParser.prettyPrint)
        |> List.ofSeq