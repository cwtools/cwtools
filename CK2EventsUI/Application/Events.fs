namespace CK2Events.Application

open System.IO
open ParserDomain

module Events =
    let rand = System.Random()

    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    // shuffle an array (in-place)
    let shuffle a =
        Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a

    let parseAll directory (printer : PrinterAPI) =
        Directory.EnumerateFiles(directory)
        |> Array.ofSeq
        |> Array.Parallel.map (fun f -> (f, (CKParser.parseEventFile >> printer.prettyPrintFileResult) f))
        |> List.ofArray

    let parseTen directory (printer : PrinterAPI) =
            let all = Directory.EnumerateFiles(directory)
                    |> Array.ofSeq
            shuffle all
            Array.take 10 all
            |> Array.Parallel.map (fun f -> (f, (CKParser.parseEventFile >> printer.prettyPrintFileResult) f))
            |> List.ofArray
    
    let getFileList directory =
        Directory.EnumerateFiles(directory)
        |> List.ofSeq
        |> List.sort
        |> List.map Path.GetFileNameWithoutExtension
            