module ProcessTests

open Expecto
open CWTools.Games
open FParsec
open CWTools.Common
open CWTools.Process
open CWTools.Parser
open CWTools.Parser.Types
open CWTools.Process.ProcessCore
open System.IO
open System.Reflection
open CWTools.Parser.DocsParser
open CWTools.Parser.SetupLogParser
open CWTools.Common.STLConstants
open System
open CWTools.Process.STLProcess
open Microsoft.FSharp.Compiler.Range
open CWTools.Parser.ConfigParser
open CWTools.Validation.Rules
open CWTools.Validation.ValidationCore

[<Tests>]
let tests =
    testList "process stl" [
        testCase "option" <| fun () ->
            let input =    "country_event = {\
                            option = {\
                                any_planet = {\
                                    if = {\
                                        limit = {\
                                        }}}}}"
            match CKParser.parseString input "test" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode<Node>() "root" (range.Zero) r)
                let event = node.Children |> List.head
                let option = event.Children |> List.head
                let anyplanet = option.Children |> List.head
                let ifblock = anyplanet.Children |> List.head
                let limit = ifblock.Children |> List.head
                Expect.isTrue (event :? STLProcess.Event) "event not right type"
                Expect.isTrue (option :? STLProcess.Option) "option not right type"
                Expect.isTrue (limit :? TriggerBlock) "node not right type"
        testCase "eventdesctrigger" <| fun () ->
            let input =    "planet_event = {\
                            desc = {\
                                trigger = {\
                                        }}}"
            match CKParser.parseString input "test" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode<Node>() "root" (range.Zero) r)
                let event = node.Children |> List.head
                let desc = event.Children |> List.head
                let trigger = desc.Children |> List.head
                Expect.isTrue (event :? STLProcess.Event) "event not right type"
                Expect.isTrue (trigger :? TriggerBlock) (sprintf "trigger not right type, actual type %A" (trigger.GetType())) 

    ]

[<Tests>]
let testsv =
    testList "config validate" [
        testCase "create_starbase" <| fun () ->
            let input =    "create_starbase = {\n\
                            owner = root \n\
                            size = large \n\
                            module = trafficControl \n\
                            }"
            match CKParser.parseString input "test" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode<Node>() "root" (range.Zero) r)
                let rules = RuleApplicator([ConfigParser.createStarbase])
                let errors = rules.ApplyNodeRule(ClauseField [ConfigParser.createStarbase], node)
                match errors with
                | OK -> ()
                | Invalid es -> Expect.isEmpty es "should be empty"
            |Failure(e, _, _) -> Expect.isTrue false e
        testCase "create_starbase fail" <| fun () ->
            let input =    "create_starbase = {\n\
                            owner = root \n\
                            size = fake \n\
                            module = faker \n\
                            unknown = test
                            }"
            match CKParser.parseString input "test" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode<Node>() "root" (range.Zero) r)
                let rules = RuleApplicator([ConfigParser.createStarbase])
                let errors = rules.ApplyNodeRule(ClauseField [ConfigParser.createStarbase], node)
                match errors with
                | OK -> ()
                | Invalid es -> Expect.equal 3 (es.Length) (sprintf "Following lines are not expected to have an error %A" es )
            |Failure(e, _, _) -> Expect.isTrue false e
        testCase "create_starbase min count" <| fun () ->
            let input =    "create_starbase = {\n\
                            }"
            match CKParser.parseString input "test" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode<Node>() "root" (range.Zero) r)
                let rules = RuleApplicator([ConfigParser.createStarbase])
                let errors = rules.ApplyNodeRule(ClauseField [ConfigParser.createStarbase], node)
                match errors with
                | OK -> ()
                | Invalid es -> Expect.equal 2 (es.Length) (sprintf "Following lines are not expected to have an error %A" es )
            |Failure(e, _, _) -> Expect.isTrue false e
        testCase "create_starbase max count" <| fun () ->
            let input =    "create_starbase = {\n\
                            owner = this \n\
                            owner = this \n\
                            size = large \n\
                            }"
            match CKParser.parseString input "test" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode<Node>() "root" (range.Zero) r)
                let rules = RuleApplicator([ConfigParser.createStarbase])
                let errors = rules.ApplyNodeRule(ClauseField [ConfigParser.createStarbase], node)
                match errors with
                | OK -> ()
                | Invalid es -> Expect.equal (es.Length) 1 (sprintf "Following lines are not expected to have an error %A" es )
            |Failure(e, _, _) -> Expect.isTrue false e
        testCase "create_starbase effect in effect" <| fun () ->
            let input =    "create_starbase = {\n\
                            owner = this \n\
                            size = large \n\
                            effect = {\n\
                            create_starbase = {\
                            owner = this \n size = large\n\
                            }\
                            }\
                            }"
            match CKParser.parseString input "test" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode<Node>() "root" (range.Zero) r)
                let rules = RuleApplicator([ConfigParser.createStarbase])
                let errors = rules.ApplyNodeRule(ClauseField [ConfigParser.createStarbase], node)
                match errors with
                | OK -> ()
                | Invalid es -> Expect.equal (es.Length) 0 (sprintf "Following lines are not expected to have an error %A" es )
            |Failure(e, _, _) -> Expect.isTrue false e
        testCase "test rhs completion" <| fun () ->
            let input =    "create_starbase = {\n\
                            owner = this \n\
                            size = large \n\
                            }"
            match CKParser.parseString input "test" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode<Node>() "root" (range.Zero) r)
                let comp = CompletionService([ConfigParser.createStarbase])
                let pos = mkPos 3 8
                let suggestions = comp.Complete(pos, node) |> Seq.sort
                let expected = ["medium"; "large"] |> Seq.sort
                Expect.sequenceEqual suggestions expected "Completion should match"
            |Failure(e, _, _) -> Expect.isTrue false e
        testCase "test lhs completion" <| fun () ->
            let input =    "create_starbase = {\n\
                            owner = this \n\
                            size \n\
                            }"
            match CKParser.parseString input "test" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode<Node>() "root" (range.Zero) r)
                let comp = CompletionService([ConfigParser.createStarbase])
                let pos = mkPos 3 3
                let suggestions = comp.Complete(pos, node) |> Seq.sort
                let expected = ["size"; "owner"; "building"; "effect"; "module"] |> Seq.sort
                Expect.sequenceEqual suggestions expected "Completion should match"
            |Failure(e, _, _) -> Expect.isTrue false e

       

    ]