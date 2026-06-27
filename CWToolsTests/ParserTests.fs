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
    testList
        "localisation parser"
        [
          //testCase "simple localisation test" <| fun () ->
          testCase "localisation folder"
          <| fun () ->
              let folder = "testfiles/localisationtests/localisation/"
              let parsed = STLLocalisationServiceFromFolder(folder).Api(STL STLLang.English)
              // eprintfn "%A" parsed.Results
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
              // eprintfn "%A" parsed.Values
              ()
          testCase "localisation CK2"
          <| fun () ->
              let files =
                  [ "testfiles/CK2/Localisation/SwordOfIslam.csv",
                    File.ReadAllText "testfiles/CK2/Localisation/SwordOfIslam.csv" ]

              let parsed = CK2LocalisationService(files).Api(CK2 CK2Lang.English)

              match parsed.ValueMap.TryFind "opinion_hajj_saving_on_hajj" with
              | Some value -> Expect.equal "Saved him from bandits" value.desc "Value had wrong value"
              | None -> Expect.isTrue false "Didn't find key"

              ()
          testCase "localisation CK2 - 2"
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

              () ]
    |> ignore

    testList
        "jomini parser"
        [ testCase "jomini simple"
          <| fun () ->
              let file = File.ReadAllText "testfiles/parsertests/simple.txt"
              let intermediateParseResult = CKParser.parseString file "test"

              match intermediateParseResult with
              | Success(r, _, _) ->
                  let node = STLProcess.shipProcess.ProcessNode () "root" (range.Zero) r

                  let printed = CKPrinter.api.prettyPrintStatement node.ToRaw

                  Expect.equal printed file "Printing shouldn't have changed string"

              | _ -> failwith "Parsing unsuccessul."
          testCase "jomini question-equals operator no space"
          <| fun () ->
              // `a?=b` should parse as key `a`, op `QuestionEqual`, value `b`
              let file = "test = { a?=b }"
              let intermediateParseResult = CKParser.parseString file "test"
              match intermediateParseResult with
              | Success(r, _, _) ->
                  let node = STLProcess.shipProcess.ProcessNode () "root" (range.Zero) r
                  match node.Statements |> Seq.tryHead with
                  | Some (Types.Statement.KeyValue(_, kvi)) ->
                      Expect.equal kvi.Key.Text "a" "Key should be `a`"
                      Expect.equal kvi.Operator Types.Operator.QuestionEqual "Op should be QuestionEqual"
                      match kvi.Value with
                      | Types.Value.Statement(Types.Statement.Value(Types.StringResource.String "b")) -> ()
                      | _ -> failwith "Value should be `b`"
                  | _ -> failwith "Expected KeyValue"
              | Failure(e, _, _) -> failwithf "Parsing failed: %A" e
          testCase "jomini question-equals operator with space"
          <| fun () ->
              // `a ?= b` should parse as key `a`, op `QuestionEqual`, value `b`
              let file = "test = { a ?= b }"
              let intermediateParseResult = CKParser.parseString file "test"
              match intermediateParseResult with
              | Success(r, _, _) ->
                  let node = STLProcess.shipProcess.ProcessNode () "root" (range.Zero) r
                  match node.Statements |> Seq.tryHead with
                  | Some (Types.Statement.KeyValue(_, kvi)) ->
                      Expect.equal kvi.Key.Text "a" "Key should be `a`"
                      Expect.equal kvi.Operator Types.Operator.QuestionEqual "Op should be QuestionEqual"
                      match kvi.Value with
                      | Types.Value.Statement(Types.Statement.Value(Types.StringResource.String "b")) -> ()
                      | _ -> failwith "Value should be `b`"
                  | _ -> failwith "Expected KeyValue"
              | Failure(e, _, _) -> failwithf "Parsing failed: %A" e
          testCase "jomini owner? existence guard"
          <| fun () ->
              // `owner? = { }` should parse as key `owner?`, op `Equals`, value Clause
              let file = "test = { owner? = { } }"
              let intermediateParseResult = CKParser.parseString file "test"
              match intermediateParseResult with
              | Success(r, _, _) ->
                  let node = STLProcess.shipProcess.ProcessNode () "root" (range.Zero) r
                  match node.Statements |> Seq.tryHead with
                  | Some (Types.Statement.KeyValue(_, kvi)) ->
                      Expect.equal kvi.Key.Text "owner?" "Key should be `owner?` (with guard)"
                      Expect.equal kvi.Operator Types.Operator.Equals "Op should be Equals"
                      match kvi.Value with
                      | Types.Value.Statement(Types.Statement.Clause []) -> ()
                      | _ -> failwith "Value should be empty Clause"
                  | _ -> failwith "Expected KeyValue"
              | Failure(e, _, _) -> failwithf "Parsing failed: %A" e
          testCase "jomini owner? round-trip"
          <| fun () ->
              // Parse `owner? = { }`, process, print back: should round-trip the `?`
              let file = "test = { owner? = { } }"
              let intermediateParseResult = CKParser.parseString file "test"
              match intermediateParseResult with
              | Success(r, _, _) ->
                  let node = STLProcess.shipProcess.ProcessNode () "root" (range.Zero) r
                  let printed = CKPrinter.api.prettyPrintStatement node.ToRaw
                  Expect.isTrue (printed.Contains "owner?") "Printed output should contain `owner?`"
              | Failure(e, _, _) -> failwithf "Parsing failed: %A" e ]
    |> ignore

    testList
        "stellaris docs"
        [ testCase "modifiers"
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
              | Failure(e, _, _) -> failwithf "Parsing failed %A" e ]
