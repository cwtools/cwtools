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

// Unified Stellaris performance test runner
let perfStellaris rootDir configPath (cachePath: string option) (modPath: string option) runTests =
    let useCache = cachePath.IsSome
    let defaultRootDir = defaultArg rootDir "./CWToolsTests/testfiles/performancetest2/"
    let defaultConfigPath = defaultArg configPath "./CWToolsTests/testfiles/performancetest2/.cwtools"
    let defaultCachePath = defaultArg cachePath @"C:\Users\Thomas\Git\cwtools-vscode\.cwtools\stl.cwb"
    
    // Enable verbose logging by default
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    
    perfRunnerWithResult (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        let settings = buildStlSettings defaultRootDir defaultConfigPath false useCache defaultCachePath
        // Add mod path if provided
        let finalSettings = 
            match modPath with
            | Some mp -> { settings with rootDirectories = settings.rootDirectories @ [WorkspaceDirectoryInput.WD { path = mp; name = "mod" }] }
            | None -> settings
        STLGame(finalSettings) :> IGame<_>) runTests

// Unified EU4 performance test runner
let perfEU4 rootDir configPath (cachePath: string option) (modPath: string option) runTests =
    let useCache = cachePath.IsSome
    let defaultRootDir = defaultArg rootDir @"D:\Games\Steam\steamapps\common\Europa Universalis IV"
    let defaultConfigPath = defaultArg configPath @"C:\Users\Thomas\git\cwtools-eu4-config\\"
    let defaultCachePath = defaultArg cachePath @"D:\Synced\Git\Personal\cwtools\CWToolsCLI\eu4.cwb"
    
    perfRunnerWithResult (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        let settings = buildEu4Settings defaultRootDir defaultConfigPath useCache defaultCachePath
        // Add mod path if provided
        let finalSettings = 
            match modPath with
            | Some mp -> { settings with rootDirectories = settings.rootDirectories @ [WD { path = mp; name = "mod" }] }
            | None -> settings
        EU4Game(finalSettings) :> IGame<_>) runTests

// Unified CK3 performance test runner
let perfCK3 rootDir configPath (cachePath: string option) (modPath: string option) runTests =
    let useCache = cachePath.IsSome
    let defaultRootDir = defaultArg rootDir @"D:\Games\Steam\steamapps\common\Crusader Kings III\game"
    let defaultConfigPath = defaultArg configPath @"C:\Users\Thomas\git\cwtools-ck3-config\\"
    let defaultCachePath = defaultArg cachePath @"D:\Synced\Git\Personal\cwtools\CWToolsCLI\ck3.cwb"
    
    perfRunnerWithResult (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        let settings = buildCk3Settings defaultRootDir defaultConfigPath useCache defaultCachePath
        // Add mod path if provided
        let finalSettings = 
            match modPath with
            | Some mp -> { settings with rootDirectories = settings.rootDirectories @ [WD { path = mp; name = "mod" }] }
            | None -> settings
        CK3Game(finalSettings) :> IGame<_>) runTests

// Unified HOI4 performance test runner
let perfHOI4 rootDir configPath (cachePath: string option) (modPath: string option) runTests =
    let useCache = cachePath.IsSome
    let defaultRootDir = defaultArg rootDir @"D:\Games\Steam\steamapps\common\Hearts of Iron IV"
    let defaultConfigPath = defaultArg configPath @"C:\Users\Thomas\Git\cwtools-hoi4-config\Config"
    let defaultCachePath = defaultArg cachePath @"D:\Synced\Git\Personal\cwtools\CWToolsCLI\hoi4.cwb"
    
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    perfRunnerWithResult (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        let settings = buildHoi4Settings defaultRootDir defaultConfigPath useCache defaultCachePath
        // Add mod path if provided
        let finalSettings = 
            match modPath with
            | Some mp -> { settings with rootDirectories = settings.rootDirectories @ [WD { path = mp; name = "mod" }] }
            | None -> settings
        HOI4Game(finalSettings) :> IGame<_>) runTests

