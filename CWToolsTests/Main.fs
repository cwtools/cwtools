module CWToolsTests

open Expecto
open System.Text
open CWTools.Parser.DocsParser
open CWTools.Games
open CWTools.Parser
open Expecto
open CWTools.Games
open FParsec
open CWTools.Common
open CWTools.Process
open CWTools.Parser
open CWTools.Process.ProcessCore
open System.IO
open System.Reflection
open CWTools.Parser.DocsParser

let perf() = 
    let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_0.2.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs p)
    let stl = STLGame("./testfiles/performancetest/", FilesScope.All, "", triggers, effects, [], [], [STL STLLang.English], false, true)
    let errors = stl.ValidationErrors |> List.map (fun (c, s, n, l, f, k) -> Position.UnConv n)
    let testVals = stl.AllEntities |> List.map (fun (e) -> e.filepath, Tests.getNodeComments e.entity |> List.map fst)
    ()

let test() =
    match CKParser.parseFile "./testfiles/localisationtests/gamefiles/common/ambient_objects/ambient_objects_test.txt" with
    |Success(a,_,_) -> printfn "%A" a
    |Failure(a,_,_) -> printfn "%A" a
[<EntryPoint>]
let main argv =
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    if Array.tryHead argv = Some "p"
    then perf(); 0
    elif Array.tryHead argv = Some "t"
    then test(); 0
    else Tests.runTestsInAssembly defaultConfig argv
