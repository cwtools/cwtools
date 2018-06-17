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
open CWTools.Games.Files
open CWTools.Games.Stellaris
open CWTools.Games.Stellaris.STLLookup

let emptyStellarisSettings (rootDirectory) = {
    rootDirectory = rootDirectory
    scope = FilesScope.All
    modFilter = None
    validation = {
        validateVanilla = false
        experimental = true
        langs = [STL STLLang.English]
    }
    rules = None
    embedded = {
        triggers = []
        effects = []
        modifiers = []
        embeddedFiles = []
    }
}
let rec getAllFolders dirs =
    if Seq.isEmpty dirs then Seq.empty else
        seq { yield! dirs |> Seq.collect Directory.EnumerateDirectories
              yield! dirs |> Seq.collect Directory.EnumerateDirectories |> getAllFolders }
let getAllFoldersUnion dirs =
    seq {
        yield! dirs
        yield! getAllFolders dirs
    }

let perf(b) =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.2.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs p)
    let configFiles = (if Directory.Exists "./testfiles/performancetest2/.cwtools" then getAllFoldersUnion (["./testfiles/performancetest2/.cwtools"] |> Seq.ofList) else Seq.empty) |> Seq.collect (Directory.EnumerateFiles)
    let configFiles = configFiles |> List.ofSeq |> List.filter (fun f -> Path.GetExtension f = ".cwt")
    let configs = configFiles |> List.map (fun f -> f, File.ReadAllText(f))
    let settings = emptyStellarisSettings "./testfiles/performancetest/"
    let settings = {settings with embedded = {settings.embedded with triggers = triggers; effects = effects};
                                    rules = Some { validateRules = true; ruleFiles = configs}}
    let stl = STLGame(settings) :> IGame<STLComputedData>
    // let stl = STLGame("./testfiles/performancetest/", FilesScope.All, "", triggers, effects, [], [], configs, [STL STLLang.English], false, true, true)
    if b then
        let errors = stl.ValidationErrors() |> List.map (fun (c, s, n, l, f, k) -> n)
        let testVals = stl.AllEntities()
        ()
    else ()
    eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    ()

let perf2(b) =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.2.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs p)
    let configFiles = (if Directory.Exists "./testfiles/performancetest2/.cwtools" then getAllFoldersUnion (["./testfiles/performancetest2/.cwtools"] |> Seq.ofList) else Seq.empty) |> Seq.collect (Directory.EnumerateFiles)
    let configFiles = configFiles |> List.ofSeq |> List.filter (fun f -> Path.GetExtension f = ".cwt")
    let configs = configFiles |> List.map (fun f -> f, File.ReadAllText(f))
    let settings = emptyStellarisSettings "./testfiles/performancetest2/"
    let settings = {settings with embedded = {settings.embedded with triggers = triggers; effects = effects};
                                    rules = Some { validateRules = true; ruleFiles = configs}}
    let stl = STLGame(settings) :> IGame<STLComputedData>

    // let stl = STLGame("./testfiles/performancetest2/", FilesScope.All, "", triggers, effects, [], [], configs, [STL STLLang.English], false, true, true)
    if b then
        let errors = stl.ValidationErrors() |> List.map (fun (c, s, n, l, f, k) -> n)
        let testVals = stl.AllEntities()
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
