module CWToolsTests

open Expecto
open System.Text
open CWTools.Parser.DocsParser
open CWTools.Games
open CWTools.Parser.Types
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

let perf(b) = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.2.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs p)
    let stl = STLGame("./testfiles/performancetest/", FilesScope.All, "", triggers, effects, [], [], [], [STL STLLang.English], false, true, false)
    if b then
        let errors = stl.ValidationErrors |> List.map (fun (c, s, n, l, f, k) -> n)
        let testVals = stl.AllEntities
        ()
    else ()
    eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    ()

let perf2(b) = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.2.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs p)
    let stl = STLGame("./testfiles/performancetest2/", FilesScope.All, "", triggers, effects, [], [], [], [STL STLLang.English], false, true, false)
    if b then
        let errors = stl.ValidationErrors |> List.map (fun (c, s, n, l, f, k) -> n)
        let testVals = stl.AllEntities
        ()
    else ()
    eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    ()

let test() =
    match CKParser.parseFile "./testfiles/localisationtests/gamefiles/common/ambient_objects/ambient_objects_test.txt" with
    |Success(a,_,_) -> printfn "%A" a
    |Failure(a,_,_) -> printfn "%A" a
[<EntryPoint>]
let main argv =
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    if Array.tryHead argv = Some "p"
    then perf(true); 0
    elif Array.tryHead argv = Some "u"
    then perf2(true); 0
    elif Array.tryHead argv = Some "o"
    then perf(false); 0
    elif Array.tryHead argv = Some "t"
    then test(); 0
    else Tests.runTestsInAssembly defaultConfig argv
