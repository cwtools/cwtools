module CK2EventsTests
open Expecto
open CK2_Events.Application

[<Tests>]
let parserTests =
    testCase "parser one" <| fun () ->
        let parsed = (CKParser.parseEventFile "CK2EventsTests/wol_business_events.txt")
        let text = CKParser.prettyPrint parsed
        Expect.isFalse (text.Contains "Error") text

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
