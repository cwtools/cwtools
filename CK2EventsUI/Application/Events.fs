namespace CK2_Events.Application

open System.IO
open Microsoft.IdentityModel.Clients.ActiveDirectory

module Events =
    let rand = System.Random()

    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    // shuffle an array (in-place)
    let shuffle a =
        Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a

    let parseAll directory =
        Directory.EnumerateFiles(directory)
        |> Array.ofSeq
        |> Array.Parallel.map (fun f -> (f, (CKParser.parseEventFile >> CKParser.prettyPrint) f))
        |> List.ofArray

    let parseTen directory =
            let all = Directory.EnumerateFiles(directory)
                    |> Array.ofSeq
            shuffle all
            Array.take 10 all
            |> Array.Parallel.map (fun f -> (f, (CKParser.parseEventFile >> CKParser.prettyPrint) f))
            |> List.ofArray
    
    let getFileList directory =
        Directory.EnumerateFiles(directory)
        |> List.ofSeq
            