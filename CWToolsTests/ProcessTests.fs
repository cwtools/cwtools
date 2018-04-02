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
                let node = (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File("test")) r)
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
                let node = (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File("test")) r)
                let event = node.Children |> List.head
                let desc = event.Children |> List.head
                let trigger = desc.Children |> List.head
                Expect.isTrue (event :? STLProcess.Event) "event not right type"
                Expect.isTrue (trigger :? TriggerBlock) (sprintf "trigger not right type, actual type %A" (trigger.GetType())) 

    ]