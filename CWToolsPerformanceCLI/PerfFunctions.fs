module CWToolsPerformanceCLI.PerfFunctions

open CWTools
open CWTools.Games.CK3
open CWTools.Games.EU4
open CWTools.Games.EU5
open CWTools.Games.HOI4
open CWToolsPerformanceCLI.PerfCommon
open CWTools.Parser.DocsParser
open CWTools.Games
open System.IO
open System.Diagnostics
open CWTools.Games.Files
open CWTools.Common
open CWTools.Parser
open FParsec
open CWTools.Games.Stellaris
open CWTools.Common.STLConstants
open CWToolsCLI
open System

// Performance result record
type PerfResult =
    { ElapsedMilliseconds: int64
      ErrorCount: int }

// Centralized path configuration
type PathConfig =
    { SteamRoot: string
      GitRoot: string
      UserHome: string
      CacheRoot: string }

// Create default path configuration
let createDefaultPathConfig () =
    let userHome = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
    let steamRoot = @"D:\Games\Steam\steamapps\common"
    let gitRoot = Path.Combine(userHome, "Git")

    let cacheRoot =
        Path.Combine(Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location), "cache")

    { SteamRoot = steamRoot
      GitRoot = gitRoot
      UserHome = userHome
      CacheRoot = cacheRoot }

// Create path configuration with custom roots
let createPathConfig (steamRoot: string option) (gitRoot: string option) =
    let defaultConfig = createDefaultPathConfig ()

    { SteamRoot = defaultArg steamRoot defaultConfig.SteamRoot
      GitRoot = defaultArg gitRoot defaultConfig.GitRoot
      UserHome = defaultConfig.UserHome
      CacheRoot = defaultConfig.CacheRoot }

// Game-specific default path builders
let getDefaultGamePaths (config: PathConfig) =
    let stellarisRoot = Path.Combine(config.SteamRoot, "Stellaris")

    let stellarisConfig =
        Path.Combine(config.GitRoot, "cwtools-stellaris-config", "config")

    let stellarisCache = Path.Combine(config.CacheRoot, "stl.cwb")

    let eu4Root = Path.Combine(config.SteamRoot, "Europa Universalis IV")
    let eu4Config = Path.Combine(config.GitRoot, "cwtools-eu4-config")
    let eu4Cache = Path.Combine(config.CacheRoot, "eu4.cwb")

    let hoi4Root = Path.Combine(config.SteamRoot, "Hearts of Iron IV")
    let hoi4Config = Path.Combine(config.GitRoot, "cwtools-hoi4-config", "Config")
    let hoi4Cache = Path.Combine(config.CacheRoot, "hoi4.cwb")

    let ck3Root = Path.Combine(config.SteamRoot, "Crusader Kings III", "game")
    let ck3Config = Path.Combine(config.GitRoot, "cwtools-ck3-config")
    let ck3Cache = Path.Combine(config.CacheRoot, "ck3.cwb")

    (stellarisRoot, stellarisConfig, stellarisCache),
    (eu4Root, eu4Config, eu4Cache),
    (hoi4Root, hoi4Config, hoi4Cache),
    (ck3Root, ck3Config, ck3Cache)

// Updated performance runner that returns structured data
let perfRunnerWithResult (buildGame: unit -> IGame<_>) runValidation =
    let timer = Stopwatch()
    timer.Start()
    let game = buildGame ()

    let errorCount =
        if runValidation then
            let errors = game.ValidationErrors() |> List.map (fun e -> e.range)
            let testVals = game.AllEntities()
            game.RefreshCaches()
            game.ForceRecompute()
            game.RefreshCaches()
            game.ValidationErrors() |> ignore
            errors.Length
        else
            0

    timer.Stop()

    { ElapsedMilliseconds = timer.ElapsedMilliseconds
      ErrorCount = errorCount }

// Unified Stellaris settings builder with parameterized paths
let buildStlSettings rootDir configPath useManual useCached cachePath earlyStopMode =
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
            enumerateConfigFiles configPath [ ".cwt" ] |> readConfigFiles, None

    let embedded =
        match useCached, useManual with
        | true, _ -> let cached, cachedFiles = cachedData.Value in FromConfig(cachedFiles, cached)
        | false, true ->
            ManualSettings
                { emptyEmbeddedSettings with
                    triggers = triggers
                    effects = effects }
        | _ -> FromConfig(configs, [])

    let folders = configs |> List.tryPick getFolderList

    if useCached then
        { rootDirectories = [ WorkspaceDirectoryInput.WD { path = rootDir; name = "test" } ]
          scriptFolders = folders
          excludeGlobPatterns = None
          embedded = embedded
          validation =
            { validateVanilla = true
              langs = [ Lang.STL STLLang.English ]
              experimental = false }
          rules =
            Some
                { ruleFiles = configs
                  validateRules = true
                  debugRulesOnly = false
                  debugMode = false }
          modFilter = None
          maxFileSize = None
          debugSettings =
            { DebugSettings.Default with
                EarlyStop = earlyStopMode } }
    else
        { emptyStellarisSettings rootDir with
            embedded = embedded
            scriptFolders = folders
            rules =
                Some
                    { validateRules = true
                      ruleFiles = configs
                      debugRulesOnly = false
                      debugMode = false }
            debugSettings =
                { DebugSettings.Default with
                    EarlyStop = earlyStopMode } }

// Legacy Stellaris cached settings with parameterized paths
let buildStlCachedSettings rootDir configPath cachePath =
    buildStlSettings rootDir configPath false true cachePath

// EU4 settings builder with parameterized paths
let buildEu4Settings rootDir configPath useCache cachePath earlyStopMode =
    let configs = CWToolsCLI.getConfigFiles (None, Some configPath)
    let folders = configs |> List.tryPick getFolderList

    let embedded =
        if useCache then
            let cached, cachedFiles = Serializer.deserialize cachePath in FromConfig(cachedFiles, cached)
        else
            FromConfig([], [])

    { rootDirectories =
        [ WD
              { name = "Europa Universalis IV"
                path = rootDir } ]
      modFilter = None
      scriptFolders = folders
      excludeGlobPatterns = None
      validation =
        { validateVanilla = not useCache
          experimental = false
          langs = [ EU4 EU4Lang.English ] }
      rules =
        Some
            { ruleFiles = configs
              validateRules = true
              debugRulesOnly = false
              debugMode = false }
      embedded = embedded
      maxFileSize = None
      debugSettings =
        { DebugSettings.Default with
            EarlyStop = earlyStopMode } }

// CK3 settings builder with parameterized paths
let buildCk3Settings rootDir configPath useCache cachePath earlyStopMode =
    let configs = CWToolsCLI.getConfigFiles (None, Some configPath)
    let folders = configs |> List.tryPick getFolderList

    let embedded =
        if useCache then
            let cached, cachedFiles = Serializer.deserialize cachePath in FromConfig(cachedFiles, cached)
        else
            FromConfig([], [])

    { rootDirectories =
        [ WD
              { name = "Crusader Kings III"
                path = rootDir } ]
      modFilter = None
      scriptFolders = folders
      excludeGlobPatterns = None
      validation =
        { validateVanilla = not useCache
          experimental = false
          langs = [ CK3 CK3Lang.English ] }
      rules =
        Some
            { ruleFiles = configs
              validateRules = true
              debugRulesOnly = false
              debugMode = false }
      embedded = embedded
      maxFileSize = None
      debugSettings =
        { DebugSettings.Default with
            EarlyStop = earlyStopMode } }

/// HOI4 settings builder with parameterized paths
let buildHoi4Settings rootDir configPath useCache cachePath earlyStopMode =
    let configs = CWToolsCLI.getConfigFiles (None, Some configPath)
    let folders = configs |> List.tryPick getFolderList

    let embedded =
        if useCache then
            let cached, cachedFiles = Serializer.deserialize cachePath in FromConfig(cachedFiles, cached)
        else
            FromConfig([], [])

    { rootDirectories = [ WD { path = rootDir; name = "test" } ]
      scriptFolders = folders
      excludeGlobPatterns = None
      embedded = embedded
      validation =
        { validateVanilla = not useCache
          langs = [ Lang.HOI4 HOI4Lang.English ]
          experimental = false }
      rules =
        Some
            { ruleFiles = configs
              validateRules = true
              debugRulesOnly = false
              debugMode = false }
      modFilter = None
      maxFileSize = None
      debugSettings =
        { DebugSettings.Default with
            EarlyStop = earlyStopMode } }

// Unified Stellaris performance test runner
let perfStellaris
    rootDir
    configPath
    (cachePath: string option)
    (modPath: string option)
    (steamRoot: string option)
    (gitRoot: string option)
    (debugMode: StopPoint)
    runTests
    =
    let pathConfig = createPathConfig steamRoot gitRoot

    let (defaultStellarisRoot, defaultStellarisConfig, defaultStellarisCache), _, _, _ =
        getDefaultGamePaths pathConfig

    let useCache = cachePath.IsSome
    let defaultRootDir = defaultArg rootDir defaultStellarisRoot
    let defaultConfigPath = defaultArg configPath defaultStellarisConfig
    let defaultCachePath = defaultArg cachePath defaultStellarisCache

    // Enable verbose logging by default
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose

    perfRunnerWithResult
        (fun () ->
            scopeManager.ReInit(defaultScopeInputs, [])

            let settings =
                buildStlSettings defaultRootDir defaultConfigPath false useCache defaultCachePath debugMode
            // Add mod path if provided
            let finalSettings =
                match modPath with
                | Some mp ->
                    { settings with
                        rootDirectories =
                            settings.rootDirectories
                            @ [ WorkspaceDirectoryInput.WD { path = mp; name = "mod" } ] }
                | None -> settings

            STLGame(finalSettings) :> IGame<_>)
        runTests

// Unified EU4 performance test runner
let perfEU4
    rootDir
    configPath
    (cachePath: string option)
    (modPath: string option)
    (steamRoot: string option)
    (gitRoot: string option)
    (earlyStopMode: StopPoint)
    runTests
    =
    let pathConfig = createPathConfig steamRoot gitRoot

    let _, (defaultEu4Root, defaultEu4Config, defaultEu4Cache), _, _ =
        getDefaultGamePaths pathConfig

    let useCache = cachePath.IsSome
    let defaultRootDir = defaultArg rootDir defaultEu4Root
    let defaultConfigPath = defaultArg configPath defaultEu4Config
    let defaultCachePath = defaultArg cachePath defaultEu4Cache

    perfRunnerWithResult
        (fun () ->
            scopeManager.ReInit(defaultScopeInputs, [])

            let settings =
                buildEu4Settings defaultRootDir defaultConfigPath useCache defaultCachePath earlyStopMode
            // Add mod path if provided
            let finalSettings =
                match modPath with
                | Some mp ->
                    { settings with
                        rootDirectories = settings.rootDirectories @ [ WD { path = mp; name = "mod" } ] }
                | None -> settings

            EU4Game(finalSettings) :> IGame<_>)
        runTests

// Unified CK3 performance test runner
let perfCK3
    rootDir
    configPath
    (cachePath: string option)
    (modPath: string option)
    (steamRoot: string option)
    (gitRoot: string option)
    (earlyStopMode: StopPoint)
    runTests
    =
    let pathConfig = createPathConfig steamRoot gitRoot

    let _, _, _, (defaultCk3Root, defaultCk3Config, defaultCk3Cache) =
        getDefaultGamePaths pathConfig

    let useCache = cachePath.IsSome
    let defaultRootDir = defaultArg rootDir defaultCk3Root
    let defaultConfigPath = defaultArg configPath defaultCk3Config
    let defaultCachePath = defaultArg cachePath defaultCk3Cache

    perfRunnerWithResult
        (fun () ->
            scopeManager.ReInit(defaultScopeInputs, [])

            let settings =
                buildCk3Settings defaultRootDir defaultConfigPath useCache defaultCachePath earlyStopMode
            // Add mod path if provided
            let finalSettings =
                match modPath with
                | Some mp ->
                    { settings with
                        rootDirectories = settings.rootDirectories @ [ WD { path = mp; name = "mod" } ] }
                | None -> settings

            CK3Game(finalSettings) :> IGame<_>)
        runTests

// Unified HOI4 performance test runner
let perfHOI4
    rootDir
    configPath
    (cachePath: string option)
    (modPath: string option)
    (steamRoot: string option)
    (gitRoot: string option)
    (earlyStopMode: StopPoint)
    runTests
    =
    let pathConfig = createPathConfig steamRoot gitRoot

    let _, _, (defaultHoi4Root, defaultHoi4Config, defaultHoi4Cache), _ =
        getDefaultGamePaths pathConfig

    let useCache = cachePath.IsSome
    let defaultRootDir = defaultArg rootDir defaultHoi4Root
    let defaultConfigPath = defaultArg configPath defaultHoi4Config
    let defaultCachePath = defaultArg cachePath defaultHoi4Cache

    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose

    perfRunnerWithResult
        (fun () ->
            scopeManager.ReInit(defaultScopeInputs, [])

            let settings =
                buildHoi4Settings defaultRootDir defaultConfigPath useCache defaultCachePath earlyStopMode
            // Add mod path if provided
            let finalSettings =
                match modPath with
                | Some mp ->
                    { settings with
                        rootDirectories = settings.rootDirectories @ [ WD { path = mp; name = "mod" } ] }
                | None -> settings

            HOI4Game(finalSettings) :> IGame<_>)
        runTests

// Simple test function for parsing individual files
let test (filePath: string) =
    let timer = Stopwatch()
    timer.Start()

    if File.Exists(filePath) then
        let content = File.ReadAllText(filePath)
        let result = CWTools.Parser.CKParser.parseString content "test"
        timer.Stop()

        match result with
        | Success _ ->
            { ElapsedMilliseconds = timer.ElapsedMilliseconds
              ErrorCount = 0 }
        | Failure(error, _, _) ->
            eprintfn "Parse error: %s" error

            { ElapsedMilliseconds = timer.ElapsedMilliseconds
              ErrorCount = 1 }
    else
        timer.Stop()
        eprintfn "File not found: %s" filePath

        { ElapsedMilliseconds = timer.ElapsedMilliseconds
          ErrorCount = 1 }


// EU5 settings builder with parameterized paths (mirrors EU4 for now)
let buildEu5Settings rootDir configPath useCache cachePath earlyStopMode =
    let configs = CWToolsCLI.getConfigFiles (None, Some configPath)
    let folders = configs |> List.tryPick getFolderList

    let embedded =
        if useCache then
            let cached, cachedFiles = Serializer.deserialize cachePath in FromConfig(cachedFiles, cached)
        else
            FromConfig([], [])

    { rootDirectories =
        [ WD
              { name = "Europa Universalis V"
                path = rootDir } ]
      modFilter = None
      scriptFolders = folders
      excludeGlobPatterns = None
      validation =
        { validateVanilla = not useCache
          experimental = false
          langs = [ EU5 EU5Lang.English ] }
      rules =
        Some
            { ruleFiles = configs
              validateRules = true
              debugRulesOnly = false
              debugMode = false }
      embedded = embedded
      maxFileSize = None
      debugSettings =
        { DebugSettings.Default with
            EarlyStop = earlyStopMode } }

// Unified EU5 performance test runner (uses EU4 defaults for convenience)
let perfEU5
    rootDir
    configPath
    (cachePath: string option)
    (modPath: string option)
    (steamRoot: string option)
    (gitRoot: string option)
    (earlyStopMode: StopPoint)
    runTests
    =
    let pathConfig = createPathConfig steamRoot gitRoot

    let _, (defaultEu4Root, defaultEu4Config, defaultEu4Cache), _, _ =
        getDefaultGamePaths pathConfig

    let useCache = cachePath.IsSome
    let defaultRootDir = defaultArg rootDir defaultEu4Root
    let defaultConfigPath = defaultArg configPath defaultEu4Config
    let defaultCachePath = defaultArg cachePath defaultEu4Cache

    perfRunnerWithResult
        (fun () ->
            scopeManager.ReInit(defaultScopeInputs, [])

            let settings =
                buildEu5Settings defaultRootDir defaultConfigPath useCache defaultCachePath earlyStopMode
            // Add mod path if provided
            let finalSettings =
                match modPath with
                | Some mp ->
                    { settings with
                        rootDirectories = settings.rootDirectories @ [ WD { path = mp; name = "mod" } ] }
                | None -> settings

            EU5Game(finalSettings) :> IGame<_>)
        runTests
