module ParserTests

open Expecto
open FParsec
open CWTools.Common
open CWTools.Process
open CWTools.Parser
open System.IO
open System
open CWTools.Localisation.STL
open CWTools.Localisation.CK2Localisation
open CWTools.Localisation
open CWTools.Utilities.Position

[<Tests>]
let tests =
    testList "parser tests" [
        testList "localisation parser" [
            testCase "localisation folder"
            <| fun () ->
                let folder = "testfiles/localisationtests/localisation/"
                let parsed = STLLocalisationServiceFromFolder(folder).Api(STL STLLang.English)
                ()
            testCase "localisation simple"
            <| fun () ->
                let parsed =
                    STLLocalisationServiceFromFolder("testfiles/localisationtests/localisation").Api(STL STLLang.English)
                let expectedValues =
                    dict
                        [ ("required", "\"test_required\"")
                          ("test_required_desc", "\"test_required\"") ]
                Expect.equal
                    (parsed.Values
                     |> Seq.map (fun (Microsoft.FSharp.Core.Operators.KeyValue(k, v)) -> (k, v))
                     |> List.ofSeq)
                    (expectedValues
                     |> Seq.map (fun (Microsoft.FSharp.Core.Operators.KeyValue(k, v)) -> (k, v))
                     |> List.ofSeq)
                    "Not equal"
                ()
            ptestCase "localisation CK2"
            <| fun () ->
                let files =
                    [ "testfiles/CK2/Localisation/SwordOfIslam.csv",
                      File.ReadAllText "testfiles/CK2/Localisation/SwordOfIslam.csv" ]
                let parsed = CK2LocalisationService(files).Api(CK2 CK2Lang.English)
                match parsed.ValueMap.TryFind "opinion_hajj_saving_on_hajj" with
                | Some value -> Expect.equal "Saved him from bandits" value.desc "Value had wrong value"
                | None -> Expect.isTrue false "Didn't find key"
                ()
            ptestCase "localisation CK2 - 2"
            <| fun () ->
                let files =
                    [ "testfiles/CK2/Localisation/v1_09.csv",
                      File.ReadAllText("testfiles/CK2/Localisation/v1_09.csv", Text.Encoding.GetEncoding(1252)) ]
                let parsed = CK2LocalisationService(files).Api(CK2 CK2Lang.English)
                match parsed.ValueMap.TryFind "tp_wall_3_desc" with
                | Some value ->
                    Expect.equal
                        "These stone walls surround the [Root.Holder.GetHouseOfWorship] and must be overcome by any attacker."
                        value.desc
                        "Value had wrong value"
                | None -> Expect.isTrue false "Didn't find key"
                ()
        ]
        testList "jomini parser" [
            testCase "jomini question-equals operator no space"
            <| fun () ->
                // `?` is an idChar, so `a?=b` reads key as `a?`, strips trailing `?` -> key `a`, op `=`
                let file = "test = { a?=b }"
                match CKParser.parseString file "test" with
                | Success(r, _, _) ->
                    let node = STLProcess.shipProcess.ProcessNode () "root" (range.Zero) r
                    match node.All |> Seq.tryHead with
                    | Some (NodeC testNode) ->
                        match testNode.All |> Seq.tryHead with
                        | Some (LeafC leaf) ->
                            Expect.equal leaf.Key "a" "Key should be `a`"
                            Expect.equal leaf.Operator Operator.Equals "Op should be Equals"
                            Expect.isFalse leaf.HasExistenceGuard "No existence guard on leaf"
                            Expect.equal (leaf.Value.ToString()) "b" "Value should be `b`"
                        | _ -> failwith "Expected LeafC under test node"
                    | _ -> failwith "Expected NodeC(test)"
                | Failure(e, _, _) -> failwithf "Parsing failed: %A" e
            testCase "jomini question-equals operator with space"
            <| fun () ->
                // With space before `?=`, key reads `a`, operator `?=` is recognized
                let file = "test = { a ?= b }"
                match CKParser.parseString file "test" with
                | Success(r, _, _) ->
                    let node = STLProcess.shipProcess.ProcessNode () "root" (range.Zero) r
                    match node.All |> Seq.tryHead with
                    | Some (NodeC testNode) ->
                        match testNode.All |> Seq.tryHead with
                        | Some (LeafC leaf) ->
                            Expect.equal leaf.Key "a" "Key should be `a`"
                            Expect.equal leaf.Operator Operator.QuestionEqual "Op should be QuestionEqual"
                            Expect.equal (leaf.Value.ToString()) "b" "Value should be `b`"
                        | _ -> failwith "Expected LeafC under test node"
                    | _ -> failwith "Expected NodeC(test)"
                | Failure(e, _, _) -> failwithf "Parsing failed: %A" e
            testCase "jomini owner? existence guard"
            <| fun () ->
                // `?` is an idChar, so key parser reads `owner?` and strips trailing `?`
                // The stripped `?` never reaches Node constructor, so HasExistenceGuard is false
                let file = "test = { owner? = { } }"
                match CKParser.parseString file "test" with
                | Success(r, _, _) ->
                    let node = STLProcess.shipProcess.ProcessNode () "root" (range.Zero) r
                    match node.All |> Seq.tryHead with
                    | Some (NodeC testNode) ->
                        match testNode.All |> Seq.tryHead with
                        | Some (NodeC ownerNode) ->
                            Expect.equal ownerNode.Key "owner" "Key `owner` (trailing ? stripped by key parser)"
                            Expect.isFalse ownerNode.HasExistenceGuard "HasExistenceGuard is false (key parser strips ? before Node constructor)"
                        | _ -> failwith "Expected NodeC(owner) under test node"
                    | _ -> failwith "Expected NodeC(test)"
                | Failure(e, _, _) -> failwithf "Parsing failed: %A" e
            testCase "jomini owner? round-trip"
            <| fun () ->
                // Since `?` is stripped by key parser, round-trip doesn't include it
                let file = "test = { owner? = { } }"
                match CKParser.parseString file "test" with
                | Success(r, _, _) ->
                    let node = STLProcess.shipProcess.ProcessNode () "root" (range.Zero) r
                    let printed = CKPrinter.api.prettyPrintStatement node.ToRaw
                    Expect.isFalse (printed.Contains "owner?") "Printed output should NOT contain `owner?` (key parser strips it)"
                | Failure(e, _, _) -> failwithf "Parsing failed: %A" e
        ]
        testList "stellaris docs" [
            testCase "modifiers"
            <| fun () ->
                let file = "testfiles/parsertests/stellarisnewdocs/modifiers.log"
                let res = StellarisModifierParser.parseLogsFile file
                match res with
                | Success(r, _, _) -> Expect.hasLength r 4639 "Expected 4639 items"
                | Failure(e, _, _) -> failwithf "Parsing failed %A" e
            testCase "triggers"
            <| fun () ->
                let file = "testfiles/parsertests/stellarisnewdocs/trigger_docs.log"
                let res = DocsParser.parseDocsFile file
                match res with
                | Success(_) -> ()
                | Failure(e, _, _) -> failwithf "Parsing failed %A" e
            testCase "trigger docs long"
            <| fun () ->
                let file = "testfiles/parsertests/stellarisnewdocs/trigger_docs_long.log"
                let res = DocsParser.parseDocsFile file
                match res with
                | Success(_) -> ()
                | Failure(e, _, _) -> failwithf "Parsing failed %A" e
        ]
    ]
