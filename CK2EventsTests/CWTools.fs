module CWToolsTests
open Expecto
open CK2Events.Application
open System.Linq
open FParsec
open System.IO
open Expecto.Expect
open Newtonsoft.Json
open CK2Events.Controllers.Utils
open System.Text
open CWTools.Common
open CWTools.Parser
open CWTools.Process
open CWTools.Localisation
open CWTools.Localisation.CK2Localisation

[<Tests>]
let processTests =
    testList "process tests" [
        testCase "process artifacts" <| fun () ->
            let slots = [KeyValue(KeyValueItem(Key("weapon"),Int(1)));
                         KeyValue(KeyValueItem(Key("ceremonial_weapon"), Int(1)));
                         KeyValue(KeyValueItem(Key("scepter"), Int(1)));
                         KeyValue(KeyValueItem(Key("crown"), Int(1)));
                         KeyValue(KeyValueItem(Key("wrist"), Int(1)));
                         KeyValue(KeyValueItem(Key("neck"), Int(1)));
                         KeyValue(KeyValueItem(Key("torso"), Int(1)));
                         KeyValue(KeyValueItem(Key("ceremonial_torso"), Int(1)));
                        ]
            let Success(parsed, _, _) as t = (CKParser.parseFile "CK2EventsTests/crusader kings 2/artifacts.txt")
            let Success(parsed2, _, _) as t2 = (CKParser.parseFile "CK2EventsTests/crusader kings 2/artifacts2.txt")
            let processed = Process.processArtifact (parsed @ parsed2)
            Expect.equal processed.Slots slots "Not equal" 
    ]