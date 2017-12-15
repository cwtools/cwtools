module CK2EventsTests
open Expecto
open CK2Events.Application
open System.Linq
open FParsec
open CK2Events.Application.Localisation
open CK2Events.Application.CKParser
open System.IO
open Expecto.Expect

[<Tests>]
let parserTests =
    testList "parser tests" [
        testCase "parser one" <| fun () ->
            let parsed = (CKParser.parseEventFile "CK2EventsTests/guilds_events.txt")
            let text = CKParser.prettyPrint parsed
            match parsed with
            | Success(_,_,_) -> ()
            | Failure(_, _, _) -> Expect.isFalse true text

        testCase "parse ten" <| fun () ->
            let parsed = Events.parseTen "CK2EventsTests/events"
            let errors = parsed 
                            |> List.filter (fun (_, p) -> p.Contains "Error")
            let error = List.isEmpty errors
            let message = if not error then sprintf "%A" (errors.First()) else "No error"
            Expect.isTrue error (sprintf "%A" message)

        testCase "parse all" <| fun () ->
            let parsed = Events.parseAll "CK2EventsTests/events"
            let errors = parsed 
                            |> List.filter (fun (_, p) -> p.Contains "Error")
            let error = List.isEmpty errors
            let message = if not error then sprintf "%A" (errors.First()) else "No error"
            Expect.isTrue error (sprintf "%A" message)
           
        testCase "parse output then parse again" <| fun () ->
            let parsed = (CKParser.parseEventFile "CK2EventsTests/test.txt")
            let pretty = CKParser.prettyPrint parsed
            let parsedAgain = (CKParser.parseEventString pretty "test.txt")
            let prettyAgain = CKParser.prettyPrint parsedAgain
            match (parsed, parsedAgain) with
            |( Success(p, _, _), Success(pa, _, _)) -> 
                Expect.equal pretty prettyAgain "Not equal"
                Expect.equal p pa "Not equal"
            | (Failure(msg, _, _), _) -> Expect.isTrue false msg
            |(_, Failure(msg, _, _)) -> Expect.isTrue false msg

        testCase "double parse all string check" <| fun () ->
            let test x = 
                let parsed = (CKParser.parseEventFile x)
                let pretty = CKParser.prettyPrint parsed
                let parsedAgain = (CKParser.parseEventString pretty "2nd")
                let prettyAgain = CKParser.prettyPrint parsedAgain
                match (parsed, parsedAgain) with
                |( Success(p, _, _), Success(pa, _, _)) -> 
                    Expect.equal pretty prettyAgain "Not equal"
                | (Failure(msg, _, _), _) -> Expect.isTrue false (x + msg)
                |(_, Failure(msg, _, _)) -> 
                    Expect.isTrue false (x + msg)
            Directory.EnumerateFiles "CK2EventsTests/events" |> List.ofSeq |> List.iter test

        testCase "double parse all equality check" <| fun () ->
            let test x = 
                let parsed = (CKParser.parseEventFile x)
                let pretty = CKParser.prettyPrint parsed
                let parsedAgain = (CKParser.parseEventString pretty "2nd")
                let prettyAgain = CKParser.prettyPrint parsedAgain
                match (parsed, parsedAgain) with
                |( Success((EventFile p), _, _), Success((EventFile pa), _, _)) -> 
                    List.iter2 (fun sa sb -> Expect.equal sa sb "Not equal") p pa
                    Expect.equal p pa "Not equal"
                | (Failure(msg, _, _), _) -> Expect.isTrue false (x + msg)
                |(_, Failure(msg, _, _)) -> 
                    Expect.isTrue false (x + msg)
            Directory.EnumerateFiles "CK2EventsTests/events" |> List.ofSeq |> List.iter test

        testCase "process one" <| fun () ->
            let parsed = (CKParser.parseEventFile "CK2EventsTests/wol_business_events.txt")

            match parsed with
                |Success(v,_,_) -> 
                    let root = Process.processEventFile v
                    Expect.equal root.Namespace "WoL" "Namespace wrong"
                    let firstEvent = root.Events |> List.last
                    Expect.equal firstEvent.ID "WoL.10100" "ID wrong"
                    Expect.equal (firstEvent.Tag "id").Value (CKParser.Value.String("WoL.10100")) "ID wrong"
                |Failure(msg, _, _) -> 
                    Expect.isTrue false msg

    ]

[<Tests>]
let localisationTests =
    testList "localisation tests" [
        testCase "localisation folder" <| fun () ->
            let settings = Microsoft.Extensions.Options.Options.Create(CK2Settings (gameDirectory="CK2EventsUI/localization"))
            let parsed = LocalisationService settings
            ()
    ]
[<EntryPoint>]
let main argv =
    runTestsInAssembly defaultConfig argv
