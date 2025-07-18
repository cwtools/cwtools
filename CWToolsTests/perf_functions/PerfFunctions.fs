module PerfFunctions

open PerfCommon
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

// Stellaris performance functions
let buildStlSettings rootDir configPath useManual =
    let triggers, effects =
        parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.2.txt"
        |> function
           | Success(p, _, _) -> (DocsParser.processDocs (scopeManager.ParseScopes)) p
           
    let configs =
        enumerateConfigFiles configPath [".cwt"]
        |> readConfigFiles
    let embedded =
        if useManual then
            ManualSettings { emptyEmbeddedSettings with triggers = triggers; effects = effects }
        else
            FromConfig(configs, [])
    { (emptyStellarisSettings rootDir) with embedded = embedded; rules = Some { validateRules = true; ruleFiles = configs; debugRulesOnly = false; debugMode = false } }

let buildStlCachedSettings rootDir configPath cachePath =
    let cached, cachedFiles = Serializer.deserialize cachePath
    let configs = CWToolsCLI.getConfigFiles (None, Some configPath)
    let folders = configs |> List.tryPick getFolderList
    { rootDirectories = [ WorkspaceDirectoryInput.WD { path = rootDir; name = "test" } ]
      scriptFolders = folders
      excludeGlobPatterns = None
      embedded = FromConfig(cachedFiles, cached)
      validation = { validateVanilla = true; langs = [ Lang.STL STLLang.English ]; experimental = false }
      rules = Some { ruleFiles = configs; validateRules = true; debugRulesOnly = false; debugMode = false }
      modFilter = None
      maxFileSize = None }

let perf runTests =
    perfRunner (fun () -> STLGame(buildStlSettings "./testfiles/performancetest/" "./testfiles/performancetest2/.cwtools" true) :> IGame<_>) runTests

let perf2 runTests =
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    let buildGame () =
        scopeManager.ReInit(defaultScopeInputs, [])
        let settings = buildStlSettings "./testfiles/performancetest2/" "./testfiles/performancetest2/.cwtools" false
        STLGame(settings) :> IGame<_>
    perfRunner buildGame runTests

let perfSTL runTests =
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    let buildGame () =
        scopeManager.ReInit(defaultScopeInputs, [])
        let settings = buildStlCachedSettings @"C:\Users\Thomas\Documents\Paradox Interactive\Stellaris\mod\test_mod_inline\\" @"C:\Users\Thomas\Git\cwtools-stellaris-config\config" @"C:\Users\Thomas\Git\cwtools-vscode\.cwtools\stl.cwb"
        CWTools.Games.Stellaris.STLGame(settings) :> IGame<_>
    perfRunner buildGame runTests

// EU4 performance functions
let buildEu4Settings rootDir configPath =
    let configs = enumerateConfigFiles configPath [".cwt"; ".log"] |> readConfigFiles
    let folders = configs |> List.tryPick getFolderList
    { CWTools.Games.EU4.EU4Settings.rootDirectories = [ WD { WorkspaceDirectory.name = "Europa Universalis IV"; path = rootDir } ]
      modFilter = None
      scriptFolders = folders
      excludeGlobPatterns = None
      validation = { validateVanilla = true; experimental = false; langs = [ EU4 EU4Lang.English ] }
      rules = Some { ruleFiles = configs; validateRules = true; debugRulesOnly = false; debugMode = false }
      embedded = FromConfig([], [])
      maxFileSize = None }

let perf3 runTests =
    let buildGame () =
        scopeManager.ReInit(defaultScopeInputs, [])
        let settings = buildEu4Settings @"D:\Games\Steam\steamapps\common\Europa Universalis IV" @"C:\Users\Thomas\git\cwtools-eu4-config\\"
        CWTools.Games.EU4.EU4Game(settings) :> IGame<_>
    perfRunner buildGame runTests

let perf4 runTests =
    Debugger.Launch()
    let buildGame () =
        scopeManager.ReInit(defaultScopeInputs, [])
        let settings = buildEu4Settings @"./testfiles/custom/files" @".\testfiles/custom/rules/"
        CWTools.Games.EU4.EU4Game(settings) :> IGame<_>
    perfRunner buildGame runTests

// CK3 performance functions
let buildCk3Settings rootDir configPath =
    let configs = enumerateConfigFiles configPath [".cwt"; ".log"] |> readConfigFiles
    let folders = configs |> List.tryPick getFolderList
    { CWTools.Games.CK3.CK3Settings.rootDirectories = [ WD { WorkspaceDirectory.name = "Crusader Kings III"; path = rootDir } ]
      modFilter = None
      scriptFolders = folders
      excludeGlobPatterns = None
      validation = { validateVanilla = true; experimental = false; langs = [ CK3 CK3Lang.English ] }
      rules = Some { ruleFiles = configs; validateRules = true; debugRulesOnly = false; debugMode = false }
      embedded = FromConfig([], [])
      maxFileSize = None }

let perf5 runTests =
    let buildGame () =
        scopeManager.ReInit(defaultScopeInputs, [])
        let settings = buildCk3Settings @"D:\Games\Steam\steamapps\common\Crusader Kings III\game" @"C:\Users\Thomas\git\cwtools-ck3-config\\"
        CWTools.Games.CK3.CK3Game(settings) :> IGame<_>
    perfRunner buildGame runTests

// HOI4 performance functions
let buildHoi4Settings rootDir configPath useCache =
    let configs = CWToolsCLI.getConfigFiles (None, Some configPath)
    let folders = configs |> List.tryPick getFolderList
    let embedded = if useCache then
                      let cached, cachedFiles = Serializer.deserialize @"D:\Synced\Git\Personal\cwtools\CWToolsCLI\hoi4.cwb"
                      FromConfig(cachedFiles, cached)
                   else
                      FromConfig([], [])
    { rootDirectories = [ WorkspaceDirectoryInput.WD { path = rootDir; name = "test" } ]
      scriptFolders = folders
      excludeGlobPatterns = None
      embedded = embedded
      validation = { validateVanilla = not useCache; langs = [ Lang.HOI4 HOI4Lang.English ]; experimental = false }
      rules = Some { ruleFiles = configs; validateRules = true; debugRulesOnly = false; debugMode = false }
      modFilter = None
      maxFileSize = None }

let perfHOI4 runTests =
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    let buildGame () =
        scopeManager.ReInit(defaultScopeInputs, [])
        let settings = buildHoi4Settings @"D:\Synced\Git\Third Party\Hearts of Iron IV\Millennium_Dawn" @"D:\Synced\Git\Personal\cwtools-hoi4-config\Config" true
        CWTools.Games.HOI4.HOI4Game(settings) :> IGame<_>
    perfRunner buildGame runTests

let perfHOI4Vanilla runTests =
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    let buildGame () =
        scopeManager.ReInit(defaultScopeInputs, [])
        let settings = buildHoi4Settings @"D:\Games\Steam\steamapps\common\Hearts of Iron IV" @"C:\Users\Thomas\Git\cwtools-hoi4-config\Config" false
        CWTools.Games.HOI4.HOI4Game(settings) :> IGame<_>
    perfRunner buildGame runTests
