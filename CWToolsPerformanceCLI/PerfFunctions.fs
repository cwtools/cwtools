module CWToolsPerformanceCLI.PerfFunctions

open CWTools.Games.CK3
open CWTools.Games.EU4
open CWTools.Games.HOI4
open CWToolsPerformanceCLI.PerfCommon
open CWTools.Parser.DocsParser
open CWTools.Games
open System.IO
open System.Diagnostics
open CWTools.Games.Files
open CWTools.Common
open CWTools.Parser
open CWTools.Parser.CKParser
open FParsec
open CWTools.Games.Stellaris
open CWTools.Common.STLConstants
open CWToolsCLI

// Performance result record
type PerfResult = {
    ElapsedMilliseconds: int64
    ErrorCount: int
}

// Updated performance runner that returns structured data
let perfRunnerWithResult (buildGame: unit -> IGame<_>) runValidation =
    let timer = Stopwatch()
    timer.Start()
    let game = buildGame()
    let errorCount = 
        if runValidation then
            let errors = game.ValidationErrors() |> List.map (fun e -> e.range)
            let testVals = game.AllEntities()
            game.RefreshCaches()
            errors.Length
        else
            0
    timer.Stop()
    { ElapsedMilliseconds = timer.ElapsedMilliseconds; ErrorCount = errorCount }

// Unified Stellaris settings builder with parameterized paths
let buildStlSettings rootDir configPath useManual useCached cachePath =
    let triggers, effects =
        if useManual then
            parseDocsFile "./CWToolsTests/testfiles/validationtests/trigger_docs_2.0.2.txt"
            |> function
                | Success(p, _, _) -> DocsParser.processDocs scopeManager.ParseScopes p
                | Failure _ -> [], []
        else
            [], []

    let configs, cachedData =
        if useCached then
            let cached, cachedFiles = Serializer.deserialize cachePath
            CWToolsCLI.getConfigFiles (None, Some configPath), Some(cached, cachedFiles)
        else
            enumerateConfigFiles configPath [".cwt"] |> readConfigFiles, None

    let embedded =
        match useCached, useManual with
        | true, _ -> let cached, cachedFiles = cachedData.Value in FromConfig(cachedFiles, cached)
        | false, true -> ManualSettings { emptyEmbeddedSettings with triggers = triggers; effects = effects }
        | _ -> FromConfig(configs, [])

    let folders = configs |> List.tryPick getFolderList

    if useCached then
        { rootDirectories = [ WorkspaceDirectoryInput.WD { path = rootDir; name = "test" } ]
          scriptFolders = folders
          excludeGlobPatterns = None
          embedded = embedded
          validation = { validateVanilla = true; langs = [ Lang.STL STLLang.English ]; experimental = false }
          rules = Some { ruleFiles = configs; validateRules = true; debugRulesOnly = false; debugMode = false }
          modFilter = None
          maxFileSize = None }
    else
        { emptyStellarisSettings rootDir with
            embedded = embedded
            scriptFolders = folders
            rules = Some { validateRules = true; ruleFiles = configs; debugRulesOnly = false; debugMode = false } }

// Legacy Stellaris cached settings with parameterized paths
let buildStlCachedSettings rootDir configPath cachePath =
    buildStlSettings rootDir configPath false true cachePath

// EU4 settings builder with parameterized paths
let buildEu4Settings rootDir configPath useCache cachePath =
    let configs = CWToolsCLI.getConfigFiles (None, Some configPath)
    let folders = configs |> List.tryPick getFolderList
    let embedded = if useCache then let cached, cachedFiles = Serializer.deserialize cachePath in FromConfig(cachedFiles, cached) else FromConfig([], [])
    { rootDirectories = [ WD { name = "Europa Universalis IV"; path = rootDir } ]
      modFilter = None
      scriptFolders = folders
      excludeGlobPatterns = None
      validation = { validateVanilla = not useCache; experimental = false; langs = [ EU4 EU4Lang.English ] }
      rules = Some { ruleFiles = configs; validateRules = true; debugRulesOnly = false; debugMode = false }
      embedded = embedded
      maxFileSize = None }

// CK3 settings builder with parameterized paths
let buildCk3Settings rootDir configPath useCache cachePath =
    let configs = CWToolsCLI.getConfigFiles (None, Some configPath)
    let folders = configs |> List.tryPick getFolderList
    let embedded = if useCache then let cached, cachedFiles = Serializer.deserialize cachePath in FromConfig(cachedFiles, cached) else FromConfig([], [])
    { rootDirectories = [ WD { name = "Crusader Kings III"; path = rootDir } ]
      modFilter = None
      scriptFolders = folders
      excludeGlobPatterns = None
      validation = { validateVanilla = not useCache; experimental = false; langs = [ CK3 CK3Lang.English ] }
      rules = Some { ruleFiles = configs; validateRules = true; debugRulesOnly = false; debugMode = false }
      embedded = embedded
      maxFileSize = None }

// HOI4 settings builder with parameterized paths
let buildHoi4Settings rootDir configPath useCache cachePath =
    let configs = CWToolsCLI.getConfigFiles (None, Some configPath)
    let folders = configs |> List.tryPick getFolderList
    let embedded = if useCache then let cached, cachedFiles = Serializer.deserialize cachePath in FromConfig(cachedFiles, cached) else FromConfig([], [])
    { rootDirectories = [ WD { path = rootDir; name = "test" } ]
      scriptFolders = folders
      excludeGlobPatterns = None
      embedded = embedded
      validation = { validateVanilla = not useCache; langs = [ Lang.HOI4 HOI4Lang.English ]; experimental = false }
      rules = Some { ruleFiles = configs; validateRules = true; debugRulesOnly = false; debugMode = false }
      modFilter = None
      maxFileSize = None }

// Stellaris performance test runners with parameterized paths and structured return
let perfStellarisManualTest rootDir configPath runTests =
    let defaultRootDir = defaultArg rootDir "./CWToolsTests/testfiles/performancetest/"
    let defaultConfigPath = defaultArg configPath "./CWToolsTests/testfiles/performancetest2/.cwtools"
    perfRunnerWithResult (fun () -> STLGame(buildStlSettings defaultRootDir defaultConfigPath true false "") :> IGame<_>) runTests

let perfStellarisVerboseTest rootDir configPath runTests =
    let defaultRootDir = defaultArg rootDir "./CWToolsTests/testfiles/performancetest2/"
    let defaultConfigPath = defaultArg configPath "./CWToolsTests/testfiles/performancetest2/.cwtools"
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    perfRunnerWithResult (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        STLGame(buildStlSettings defaultRootDir defaultConfigPath false false "") :> IGame<_>) runTests

let perfStellarisModCached rootDir configPath cachePath runTests =
    let defaultRootDir = defaultArg rootDir @"C:\Users\Thomas\Documents\Paradox Interactive\Stellaris\mod\test_mod_inline\\"
    let defaultConfigPath = defaultArg configPath @"C:\Users\Thomas\Git\cwtools-stellaris-config\config"
    let defaultCachePath = defaultArg cachePath @"C:\Users\Thomas\Git\cwtools-vscode\.cwtools\stl.cwb"
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    perfRunnerWithResult (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        STLGame(buildStlSettings defaultRootDir defaultConfigPath false true defaultCachePath) :> IGame<_>) runTests

// EU4 performance test runners with parameterized paths and structured return
let perfEU4Vanilla rootDir configPath cachePath runTests =
    let defaultRootDir = defaultArg rootDir @"D:\Games\Steam\steamapps\common\Europa Universalis IV"
    let defaultConfigPath = defaultArg configPath @"C:\Users\Thomas\git\cwtools-eu4-config\\"
    let defaultCachePath = defaultArg cachePath @"D:\Synced\Git\Personal\cwtools\CWToolsCLI\eu4.cwb"
    perfRunnerWithResult (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        EU4Game(buildEu4Settings defaultRootDir defaultConfigPath false defaultCachePath) :> IGame<_>) runTests

let perfEU4Custom rootDir configPath cachePath runTests =
    let defaultRootDir = defaultArg rootDir @"./CWToolsTests/testfiles/custom/files"
    let defaultConfigPath = defaultArg configPath @".\CWToolsTests/testfiles/custom/rules\"
    let defaultCachePath = defaultArg cachePath @"D:\Synced\Git\Personal\cwtools\CWToolsCLI\eu4.cwb"
    perfRunnerWithResult (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        EU4Game(buildEu4Settings defaultRootDir defaultConfigPath false defaultCachePath) :> IGame<_>) runTests

// CK3 performance test runners with parameterized paths and structured return
let perfCK3Vanilla rootDir configPath cachePath runTests =
    let defaultRootDir = defaultArg rootDir @"D:\Games\Steam\steamapps\common\Crusader Kings III\game"
    let defaultConfigPath = defaultArg configPath @"C:\Users\Thomas\git\cwtools-ck3-config\\"
    let defaultCachePath = defaultArg cachePath @"D:\Synced\Git\Personal\cwtools\CWToolsCLI\ck3.cwb"
    perfRunnerWithResult (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        CK3Game(buildCk3Settings defaultRootDir defaultConfigPath false defaultCachePath) :> IGame<_>) runTests

// HOI4 performance test runners with parameterized paths and structured return
let perfHOI4ModCached rootDir configPath cachePath runTests =
    let defaultRootDir = defaultArg rootDir @"D:\Synced\Git\Third Party\Hearts of Iron IV\Millennium_Dawn"
    let defaultConfigPath = defaultArg configPath @"D:\Synced\Git\Personal\cwtools-hoi4-config\Config"
    let defaultCachePath = defaultArg cachePath @"D:\Synced\Git\Personal\cwtools\CWToolsCLI\hoi4.cwb"
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    perfRunnerWithResult (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        HOI4Game(buildHoi4Settings defaultRootDir defaultConfigPath true defaultCachePath) :> IGame<_>) runTests

let perfHOI4Vanilla rootDir configPath cachePath runTests =
    let defaultRootDir = defaultArg rootDir @"D:\Games\Steam\steamapps\common\Hearts of Iron IV"
    let defaultConfigPath = defaultArg configPath @"C:\Users\Thomas\Git\cwtools-hoi4-config\Config"
    let defaultCachePath = defaultArg cachePath @"D:\Synced\Git\Personal\cwtools\CWToolsCLI\hoi4.cwb"
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    perfRunnerWithResult (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        HOI4Game(buildHoi4Settings defaultRootDir defaultConfigPath false defaultCachePath) :> IGame<_>) runTests

// Simple test runner with parameterized paths and structured return
let test filePath =
    let defaultFilePath = defaultArg filePath "./combat_locators.txt"
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let errorCount = 
        match CKParser.parseFile defaultFilePath with
        | Success(a, _, _) -> 0
        | Failure(a, _, _) -> 1
    timer.Stop()
    { ElapsedMilliseconds = timer.ElapsedMilliseconds; ErrorCount = errorCount }
