module CK2EventsTests
open Expecto
open CK2_Events.Application
open System
open System.Collections.Generic
open System.Linq

[<Tests>]
let parserTests =
    testList "parser tests" [
        testCase "parser one" <| fun () ->
            let parsed = (CKParser.parseEventFile "CK2EventsTests/wol_business_events.txt")
            let text = CKParser.prettyPrint parsed
            Expect.isFalse (text.Contains "Error") text

        testCase "parse ten" <| fun () ->
            let parsed = Events.parseTen "CK2EventsUI/events"
            let errors = parsed 
                            |> List.filter (fun (f, p) -> p.Contains "Error")
            let error = List.isEmpty errors
            let message = if not error then sprintf "%A" (errors.First()) else "No error"
            Expect.isTrue error (sprintf "%A" message)

        testCase "parse all" <| fun () ->
            let parsed = Events.parseAll "CK2EventsUI/events"
            let errors = parsed 
                            |> List.filter (fun (f, p) -> p.Contains "Error")
            let error = List.isEmpty errors
            let message = if not error then sprintf "%A" (errors.First()) else "No error"
            Expect.isTrue error (sprintf "%A" message)
    ]

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
