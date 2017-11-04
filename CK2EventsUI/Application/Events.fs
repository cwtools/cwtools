namespace CK2_Events.Application

open System.IO

module Events =
    let rand = System.Random()

    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    // shuffle an array (in-place)
    let shuffle a =
        Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a

    let parseAll =
        Directory.EnumerateFiles("events")
        |> Seq.map (CKParser.parseEventFile >> CKParser.prettyPrint)
        |> List.ofSeq

    let parseTen =
            let all = Directory.EnumerateFiles("events")
                    |> Array.ofSeq
            shuffle all
            Array.take 10 all
            |> Array.map (fun f -> (f, (CKParser.parseEventFile >> CKParser.prettyPrint) f))
            |> List.ofArray
            