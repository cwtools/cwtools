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
open CWTools.Process.CK2Process
open CWTools
open System.Diagnostics

let test f =
    let x = CKParser.parseFile f
    match x with
    |Success(_,_,_) -> ()
    |Failure(msg,_,_) -> Expect.isTrue false (f + " " + msg)
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
            let processed = CK2Process.processArtifact (parsed @ parsed2)
            Expect.equal processed.Slots slots "Not equal" 
        
        testList "process all" [
            let folders = Directory.EnumerateDirectories "/home/thomas/.steam/steam/steamapps/common/Crusader Kings II/common" |> List.ofSeq
            let files = folders |> List.map (Directory.EnumerateFiles >> List.ofSeq) |> List.collect id |> List.filter (fun f -> Path.GetExtension(f) = ".txt")
            yield! files |> List.map (fun f -> testCase ("process one " + f.ToString()) <| fun () -> test f)
        ]

        testCase "STLGame test" <| fun () ->
            let game = CWTools.STLGame("/home/thomas/.steam/steam/steamapps/common/Stellaris")
            let results = game.Results
            results |> List.tryFind (function |CWTools.FileResult.Fail(f, e) -> true |_ -> false)
                    |> function |Some (Fail(k, e)) -> Expect.isTrue false (k + " " + e) |None -> ()
        // testCase "process all CK2" <| fun () ->
        //     let commons = 
        //         Directory.EnumerateDirectories "/home/thomas/.steam/steam/steamapps/common/Crusader Kings II/common"
        //         |> List.ofSeq
        //         |> List.map (Directory.EnumerateFiles >> List.ofSeq)
        //         |> List.collect id
        //         |> List.filter (fun f -> Path.GetExtension(f) = ".txt")
        //     commons |> List.map (CKParser.parseFile)
        //             |> List.iter (function |Success(parsed,_,_) -> () |Failure(msg, _, _) -> Expect.isTrue false msg )
    ]