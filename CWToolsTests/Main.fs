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
open System.Threading
open System.Globalization
open CWTools.Common.STLConstants
open CWTools.Common.NewScope
open System.Text
open System

let emptyEmbeddedSettings = {
        triggers = []
        effects = []
        modifiers = []
        embeddedFiles = []
        cachedResourceData = []
        localisationCommands = Legacy []
        eventTargetLinks = []
}
let emptyStellarisSettings (rootDirectory) = {
    rootDirectories = [{ name = "test"; path = rootDirectory;}]
    modFilter = None
    validation = {
        validateVanilla = false
        experimental = true
        langs = [STL STLLang.English]
    }
    rules = None
    embedded = FromConfig ([], [])
    scriptFolders = None
    excludeGlobPatterns = None
    maxFileSize = None
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
    let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.2.txt" |> (function |Success(p, _, _) -> (DocsParser.processDocs (scopeManager.ParseScopes)) p)
    let configFiles = (if Directory.Exists "./testfiles/performancetest2/.cwtools" then getAllFoldersUnion (["./testfiles/performancetest2/.cwtools"] |> Seq.ofList) else Seq.empty) |> Seq.collect (Directory.EnumerateFiles)
    let configFiles = configFiles |> List.ofSeq |> List.filter (fun f -> Path.GetExtension f = ".cwt")
    let configs = configFiles |> List.map (fun f -> f, File.ReadAllText(f))
    let settings = emptyStellarisSettings "./testfiles/performancetest/"
    let settings = {settings with embedded = ManualSettings {emptyEmbeddedSettings with triggers = triggers; effects = effects};
                                    rules = Some { validateRules = true; ruleFiles = configs; debugRulesOnly = false; debugMode = false}}
    let stl = STLGame(settings) :> IGame<STLComputedData>
    // let stl = STLGame("./testfiles/performancetest/", FilesScope.All, "", triggers, effects, [], [], configs, [STL STLLang.English], false, true, true)
    if b then
        let errors = stl.ValidationErrors() |> List.map (fun e -> e.range)
        let testVals = stl.AllEntities()
        ()
    else ()
    eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    ()

let perf2(b) =
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    scopeManager.ReInit(defaultScopeInputs)
    let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.2.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs (scopeManager.ParseScopes) p)
    let configFiles = (if Directory.Exists "./testfiles/performancetest2/.cwtools" then getAllFoldersUnion (["./testfiles/performancetest2/.cwtools"] |> Seq.ofList) else Seq.empty) |> Seq.collect (Directory.EnumerateFiles)
    let configFiles = configFiles |> List.ofSeq |> List.filter (fun f -> Path.GetExtension f = ".cwt")
    let configs = configFiles |> List.map (fun f -> f, File.ReadAllText(f))
    let settings = emptyStellarisSettings "./testfiles/performancetest2/"
    let settings = {settings with embedded = FromConfig (configs, []);
                                    rules = Some { validateRules = true; ruleFiles = configs; debugRulesOnly = false; debugMode = false}}
    let stl = STLGame(settings) :> IGame<STLComputedData>
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    // let stl = STLGame("./testfiles/performancetest2/", FilesScope.All, "", triggers, effects, [], [], configs, [STL STLLang.English], false, true, true)
    if b then
        let errors = stl.ValidationErrors() |> List.map (fun e -> e.range)
        let testVals = stl.AllEntities()
        stl.RefreshCaches()
        ()
    else ()
    eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    ()
let getFolderList (filename : string, filetext : string) =
    if Path.GetFileName filename = "folders.cwt"
    then Some (filetext.Split(([|"\r\n"; "\r"; "\n"|]), StringSplitOptions.None) |> List.ofArray |> List.filter (fun s -> s <> ""))
    else None

let perf3(b) =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    scopeManager.ReInit(defaultScopeInputs)
    let configFiles = (if Directory.Exists @"C:\Users\Thomas\git\cwtools-eu4-config\" then getAllFoldersUnion ([@"C:\Users\Thomas\git\cwtools-eu4-config\"] |> Seq.ofList) else Seq.empty) |> Seq.collect (Directory.EnumerateFiles)
    let configFiles = configFiles |> List.ofSeq |> List.filter (fun f -> Path.GetExtension f = ".cwt" || Path.GetExtension f = ".log")
    let configs = configFiles |> List.map (fun f -> f, File.ReadAllText(f))
    let folders = configs |> List.tryPick getFolderList

    let settings = {
        CWTools.Games.EU4.EU4Settings.rootDirectories = [{ WorkspaceDirectory.name = "Europa Universalis IV"; path = @"D:\Games\Steam\steamapps\common\Europa Universalis IV"}]
        modFilter = None
        scriptFolders = folders
        excludeGlobPatterns = None
        validation = {
            validateVanilla = true
            experimental = false
            langs = [EU4 EU4Lang.English]
        }
        rules = Some {
            ruleFiles = configs
            validateRules = true
            debugRulesOnly = false
            debugMode = false
        }
        embedded = FromConfig ([], [])
        maxFileSize = None
    }
    let eu4 = CWTools.Games.EU4.EU4Game(settings) :> IGame<EU4ComputedData>

    // let stl = STLGame("./testfiles/performancetest2/", FilesScope.All, "", triggers, effects, [], [], configs, [STL STLLang.English], false, true, true)
    if b then
        let errors = eu4.ValidationErrors() |> List.map (fun e -> e.range)
        let errors = eu4.LocalisationErrors(true, true) |> List.map (fun e -> e.range)
        let testVals = eu4.AllEntities()
        eu4.RefreshCaches()
        ()
    else ()
    eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    ()


let test() =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    match CKParser.parseFile "./combat_locators.txt" with
    |Success(a,_,_) -> ()
    |Failure(a,_,_) -> printfn "%A" a
    eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
[<EntryPoint>]
let main argv =
    let config =  { defaultConfig  with ``parallel`` = false}
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    CultureInfo.DefaultThreadCurrentCulture <-CultureInfo("ru-RU");
    CultureInfo.DefaultThreadCurrentUICulture <-CultureInfo("ru-RU");
    Thread.CurrentThread.CurrentCulture <- CultureInfo("ru-RU");
    Thread.CurrentThread.CurrentUICulture <- CultureInfo("ru-RU");
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    if Array.tryHead argv = Some "p"
    then perf(true); 0
    elif Array.tryHead argv = Some "u"
    then
        perf2(true);
        perf2(true); 0
    elif Array.tryHead argv = Some "i"
    then
        perf2(true); 0
    elif Array.tryHead argv = Some "o"
    then perf(false); 0
    elif Array.tryHead argv = Some "q"
    then perf3(true); 0
    elif Array.tryHead argv = Some "t"
    then test(); test(); 0
    else Tests.runTestsInAssembly config argv
