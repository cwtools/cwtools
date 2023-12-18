module CWToolsTests

open CWToolsCLI
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
open System.Diagnostics
open Expecto.Logging
open Expecto.Logging.Message

let logger = Log.create "MyTests"
CWTools.Utilities.Utils.logDiag <- logger.logSimple << (event LogLevel.Verbose)
CWTools.Utilities.Utils.logInfo <- logger.logSimple << (event LogLevel.Info)
CWTools.Utilities.Utils.logWarning <- logger.logSimple << (event LogLevel.Warn)
CWTools.Utilities.Utils.logError <- logger.logSimple << (event LogLevel.Error)


let emptyEmbeddedSettings = {
        triggers = []
        effects = []
        modifiers = []
        embeddedFiles = []
        cachedResourceData = []
        localisationCommands = Legacy ([], [], [])
        eventTargetLinks = []
        cachedRuleMetadata = None
        featureSettings = CWTools.Parser.UtilityParser.FeatureSettings.Default
}
let emptyStellarisSettings (rootDirectory) = {
    rootDirectories = [WD { name = "test"; path = rootDirectory;}]
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
    scopeManager.ReInit(defaultScopeInputs, [])
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
    scopeManager.ReInit(defaultScopeInputs, [])
    let configFiles = (if Directory.Exists @"C:\Users\Thomas\git\cwtools-eu4-config\" then getAllFoldersUnion ([@"C:\Users\Thomas\git\cwtools-eu4-config\"] |> Seq.ofList) else Seq.empty) |> Seq.collect (Directory.EnumerateFiles)
    let configFiles = configFiles |> List.ofSeq |> List.filter (fun f -> Path.GetExtension f = ".cwt" || Path.GetExtension f = ".log")
    let configs = configFiles |> List.map (fun f -> f, File.ReadAllText(f))
    let folders = configs |> List.tryPick getFolderList

    let settings = {
        CWTools.Games.EU4.EU4Settings.rootDirectories = [WD { WorkspaceDirectory.name = "Europa Universalis IV"; path = @"D:\Games\Steam\steamapps\common\Europa Universalis IV"}]
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
let perf4(b) =
    Debugger.Launch()
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    scopeManager.ReInit(defaultScopeInputs, [])
    let configFiles = (if Directory.Exists @".\testfiles/custom/rules/" then getAllFoldersUnion ([@".\testfiles/custom/rules"] |> Seq.ofList) else Seq.empty) |> Seq.collect (Directory.EnumerateFiles)
    let configFiles = configFiles |> List.ofSeq |> List.filter (fun f -> Path.GetExtension f = ".cwt" || Path.GetExtension f = ".log")
    let configs = configFiles |> List.map (fun f -> f, File.ReadAllText(f))
    let folders = configs |> List.tryPick getFolderList

    let settings = {
        CWTools.Games.EU4.EU4Settings.rootDirectories = [WD { WorkspaceDirectory.name = "Test"; path = @"./testfiles/custom/files"}]
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


let perf5(b) =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    scopeManager.ReInit(defaultScopeInputs, [])
    let configFiles = (if Directory.Exists @"C:\Users\Thomas\git\cwtools-ck3-config\" then getAllFoldersUnion ([@"C:\Users\Thomas\git\cwtools-ck3-config\"] |> Seq.ofList) else Seq.empty) |> Seq.collect (Directory.EnumerateFiles)
    let configFiles = configFiles |> List.ofSeq |> List.filter (fun f -> Path.GetExtension f = ".cwt" || Path.GetExtension f = ".log")
    let configs = configFiles |> List.map (fun f -> f, File.ReadAllText(f))
    let folders = configs |> List.tryPick getFolderList

    let settings = {
        CWTools.Games.CK3.CK3Settings.rootDirectories = [WD { WorkspaceDirectory.name = "Crusader Kings III"; path = @"D:\Games\Steam\steamapps\common\Crusader Kings III\game"}]
        modFilter = None
        scriptFolders = folders
        excludeGlobPatterns = None
        validation = {
            validateVanilla = true
            experimental = false
            langs = [CK3 CK3Lang.English]
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
    let eu4 = CWTools.Games.CK3.CK3Game(settings) :> IGame<CK3ComputedData>

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
let perfSTL(b) =
      CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
      let timer = new System.Diagnostics.Stopwatch()
      timer.Start()
      scopeManager.ReInit(defaultScopeInputs, [])
           
      let cached, cachedFiles = Serializer.deserialize @"C:\Users\Thomas\Git\cwtools-vscode\.cwtools\stl.cwb"
      let configs = CWToolsCLI.getConfigFiles (None, Some @"C:\Users\Thomas\Git\cwtools-stellaris-config\config")
      let folders = configs |> List.tryPick getFolderList
  
  
      let stlsettings = {
          rootDirectories = [ WorkspaceDirectoryInput.WD { path = @"C:\Users\Thomas\Documents\Paradox Interactive\Hearts of Iron IV\mod\Test\"; name = "test" } ]
          scriptFolders = folders
          excludeGlobPatterns = None
          embedded = FromConfig (cachedFiles, cached)
          validation = {
              validateVanilla = true;
              langs = [ Lang.STL STLLang.English ]
              experimental = false
          }
          rules = Some {
              ruleFiles = configs
              validateRules = true
              debugRulesOnly = false
              debugMode = false
          }
          modFilter = None
          maxFileSize = None
      }
      let game = CWTools.Games.Stellaris.STLGame(stlsettings) :> IGame<STLComputedData>
      eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
      if b then
          let errors = game.ValidationErrors() |> List.map (fun e -> e.range)
          eprintfn "Elapsed Time: %i, %i errors" timer.ElapsedMilliseconds (errors |> List.length)
          let testVals = game.AllEntities()
          eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
          game.RefreshCaches()
          eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
          ()
      else ()
      eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
      ()
       
let perfHOI4(b) =
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    scopeManager.ReInit(defaultScopeInputs, [])
         
    let cached, cachedFiles = Serializer.deserialize @"C:\Users\Thomas\Git\cwtools-vscode\.cwtools\hoi4.cwb"
    let configs = CWToolsCLI.getConfigFiles (None, Some @"C:\Users\Thomas\Git\cwtools-hoi4-config\Config")
    let folders = configs |> List.tryPick getFolderList

    let hoi4modpath = "Main.files.hoi4.modifiers"

    let hoi4settings = {
        rootDirectories = [ WorkspaceDirectoryInput.WD { path = @"D:\Git\Hearts of Iron IV\Millennium_Dawn"; name = "test" } ]
        scriptFolders = folders
        excludeGlobPatterns = None
        embedded = FromConfig (cachedFiles, cached)
        validation = {
            validateVanilla = false;
            langs = [ Lang.HOI4 HOI4Lang.English ]
            experimental = false
        }
        rules = Some {
            ruleFiles = configs
            validateRules = true
            debugRulesOnly = false
            debugMode = false
        }
        modFilter = None
        maxFileSize = None
    }
    let game = CWTools.Games.HOI4.HOI4Game(hoi4settings) :> IGame<HOI4ComputedData>
    eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    if b then
        let errors = game.ValidationErrors() |> List.map (fun e -> e.range)
        eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
        let testVals = game.AllEntities()
        eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
        game.RefreshCaches()
        eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
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
    let config =  { defaultConfig  with runInParallel = false}
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
    elif Array.tryHead argv = Some "y"
    then perf4(true); 0
    elif Array.tryHead argv = Some "z"
    then perf5(true); 0
    elif Array.tryHead argv = Some "t"
    then test(); test(); 0
    elif Array.tryHead argv = Some "j"
    then perfHOI4(true); perfHOI4(true); 0
    elif Array.tryHead argv = Some "k"
    then perfHOI4(true); 0
    elif Array.tryHead argv = Some "s"
    then perfSTL(true); perfSTL(true); 0
    elif Array.tryHead argv = Some "S"
    then perfSTL(true); 0
    else Tests.runTestsInAssemblyWithCLIArgs [Sequenced] argv
