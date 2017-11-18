module CK2EventsTests
open Expecto
open CK2_Events.Application
open System
open System.Collections.Generic
open System.Linq
open FParsec
open FSharp.Data

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
        
        testCase "foldback" <| fun () ->
            let parsed = (CKParser.parseEventFile "CK2EventsTests/wol_business_events.txt")

            match parsed with
                |Success(v,_,_) -> 
                    let root = Process.processEventFile v
                    let test = List.map Process.getTriggeredEvents root.Events
                    // let test = List.map Process.testNode root.Events |> List.rev
                    // let test2 = List.fold (List.fold (+)) "" test
                    Expect.isTrue false (sprintf "%A" test)
                |Failure(msg, _, _) -> 
                    Expect.isTrue false msg

        testCase "descTest" <| fun () ->
            let desc = Localization.GetDesc "EVTOPTB_WoL_12005"
            Expect.equal desc "Abandon construction... let us save what we can." "Getdesc fail"

        testCase "optionTest" <| fun () ->
            let parsed = (CKParser.parseEventFile "CK2EventsTests/wol_business_events.txt")

            match parsed with
                |Success(v,_,_) -> 
                    let root = Process.processEventFile v
                    let opts = root.Events.[5] |> Process.getOptions
                    Expect.isTrue false (sprintf "%A" opts) 
                |Failure(msg, _, _) ->
                    Expect.isTrue false msg
        //testCase "descTest2" <| fun () ->
        //    Expect.isTrue false (sprintf "%A" (Localization.keys |> List.rev))
    ]

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
