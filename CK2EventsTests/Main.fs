module CK2EventsTests
open Expecto
open CK2Events.Application
open System.Linq
open FParsec
open CKParser
open Localisation
open System.IO
open Expecto.Expect
open ParserDomain

let printer = CKPrinter.api
let parser = CKParser.api

let parseEqualityCheck file =
    let parsed = parser.parseFile file
    let pretty = printer.prettyPrintFileResult parsed
    let parsedAgain = parser.parseString pretty "test"
    let prettyAgain = printer.prettyPrintFileResult parsedAgain
    match (parsed, parsedAgain) with
    |( Success(p, _, _), Success(pa, _, _)) -> 
        Expect.equal pretty prettyAgain "Not equal"
        Expect.equal p pa "Not equal"
    | (Failure(msg, _, _), _) -> 
        Expect.isTrue false msg
    |(_, Failure(msg, _, _)) -> 
        Expect.isTrue false msg

[<Tests>]
let parserTests =
    testList "parser tests" [
        testCase "parser one" <| fun () ->
            let parsed = (CKParser.parseEventFile "CK2EventsTests/guilds_events.txt")
            let text = printer.prettyPrintFileResult parsed
            match parsed with
            | Success(_,_,_) -> ()
            | Failure(_, _, _) -> Expect.isFalse true text

        testCase "parse ten" <| fun () ->
            let parsed = Events.parseTen "CK2EventsTests/events" printer
            let errors = parsed 
                            |> List.filter (fun (_, p) -> p.Contains "Error")
            let error = List.isEmpty errors
            let message = if not error then sprintf "%A" (errors.First()) else "No error"
            Expect.isTrue error (sprintf "%A" message)

        testCase "parse all" <| fun () ->
            let parsed = Events.parseAll "CK2EventsTests/events" printer
            let errors = parsed 
                            |> List.filter (fun (_, p) -> p.Contains "Error")
            let error = List.isEmpty errors
            let message = if not error then sprintf "%A" (errors.First()) else "No error"
            Expect.isTrue error (sprintf "%A" message)

        testCase "double parse all test files" <| fun () ->
            Directory.EnumerateFiles "CK2EventsTests/event test files" |> List.ofSeq |> List.iter parseEqualityCheck
        
        testCase "double parse all game files" <| fun () ->
            Directory.EnumerateFiles "CK2EventsTests/events" |> List.ofSeq |> List.iter parseEqualityCheck

        testCase "process one" <| fun () ->
            let parsed = (parser.parseFile "CK2EventsTests/wol_business_events.txt")

            match parsed with
                |Success(v,_,_) -> 
                    let root = Process.processEventFile v
                    Expect.equal root.Namespace "WoL" "Namespace wrong"
                    let firstEvent = root.Events |> List.last
                    Expect.equal firstEvent.ID "WoL.10100" "ID wrong"
                    Expect.equal (firstEvent.Tag "id").Value (String("WoL.10100")) "ID wrong"
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
