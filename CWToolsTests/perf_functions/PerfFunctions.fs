module PerfFunctions

open CWTools.Games.CK3
open CWTools.Games.EU4
open CWTools.Games.HOI4
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

// Unified Stellaris settings builder
let buildStlSettings rootDir configPath useManual useCached cachePath =
    let triggers, effects =
        if useManual then
            parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.2.txt"
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

// Legacy Stellaris cached settings
let buildStlCachedSettings rootDir configPath cachePath =
    buildStlSettings rootDir configPath false true cachePath

// Stellaris performance runners
let perfStellarisManualTest runTests =
    perfRunner (fun () -> STLGame(buildStlSettings "./testfiles/performancetest/" "./testfiles/performancetest2/.cwtools" true false "") :> IGame<_>) runTests

let perfStellarisVerboseTest runTests =
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    perfRunner (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        STLGame(buildStlSettings "./testfiles/performancetest2/" "./testfiles/performancetest2/.cwtools" false false "") :> IGame<_>) runTests

let perfStellarisModCached runTests =
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    perfRunner (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        STLGame(buildStlSettings @"C:\Users\Thomas\Documents\Paradox Interactive\Stellaris\mod\test_mod_inline\\" @"C:\Users\Thomas\Git\cwtools-stellaris-config\config" false true @"C:\Users\Thomas\Git\cwtools-vscode\.cwtools\stl.cwb") :> IGame<_>) runTests

// EU4 settings and runner
let buildEu4Settings rootDir configPath useCache =
    let configs = CWToolsCLI.getConfigFiles (None, Some configPath)
    let folders = configs |> List.tryPick getFolderList
    let embedded = if useCache then let cached, cachedFiles = Serializer.deserialize @"D:\Synced\Git\Personal\cwtools\CWToolsCLI\eu4.cwb" in FromConfig(cachedFiles, cached) else FromConfig([], [])
    { rootDirectories = [ WD { name = "Europa Universalis IV"; path = rootDir } ]
      modFilter = None
      scriptFolders = folders
      excludeGlobPatterns = None
      validation = { validateVanilla = not useCache; experimental = false; langs = [ EU4 EU4Lang.English ] }
      rules = Some { ruleFiles = configs; validateRules = true; debugRulesOnly = false; debugMode = false }
      embedded = embedded
      maxFileSize = None }

let perfEU4Vanilla runTests =
    perfRunner (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        EU4Game(buildEu4Settings @"D:\Games\Steam\steamapps\common\Europa Universalis IV" @"C:\Users\Thomas\git\cwtools-eu4-config\\" false) :> IGame<_>) runTests

let perfEU4Custom runTests =
    perfRunner (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        EU4Game(buildEu4Settings @"./testfiles/custom/files" @".\testfiles/custom/rules/" false) :> IGame<_>) runTests

// CK3 settings and runner
let buildCk3Settings rootDir configPath useCache =
    let configs = CWToolsCLI.getConfigFiles (None, Some configPath)
    let folders = configs |> List.tryPick getFolderList
    let embedded = if useCache then let cached, cachedFiles = Serializer.deserialize @"D:\Synced\Git\Personal\cwtools\CWToolsCLI\ck3.cwb" in FromConfig(cachedFiles, cached) else FromConfig([], [])
    { rootDirectories = [ WD { name = "Crusader Kings III"; path = rootDir } ]
      modFilter = None
      scriptFolders = folders
      excludeGlobPatterns = None
      validation = { validateVanilla = not useCache; experimental = false; langs = [ CK3 CK3Lang.English ] }
      rules = Some { ruleFiles = configs; validateRules = true; debugRulesOnly = false; debugMode = false }
      embedded = embedded
      maxFileSize = None }

let perfCK3Vanilla runTests =
    perfRunner (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        CK3Game(buildCk3Settings @"D:\Games\Steam\steamapps\common\Crusader Kings III\game" @"C:\Users\Thomas\git\cwtools-ck3-config\\" false) :> IGame<_>) runTests

// HOI4 settings and runners
let buildHoi4Settings rootDir configPath useCache =
    let configs = CWToolsCLI.getConfigFiles (None, Some configPath)
    let folders = configs |> List.tryPick getFolderList
    let embedded = if useCache then let cached, cachedFiles = Serializer.deserialize @"D:\Synced\Git\Personal\cwtools\CWToolsCLI\hoi4.cwb" in FromConfig(cachedFiles, cached) else FromConfig([], [])
    { rootDirectories = [ WD { path = rootDir; name = "test" } ]
      scriptFolders = folders
      excludeGlobPatterns = None
      embedded = embedded
      validation = { validateVanilla = not useCache; langs = [ Lang.HOI4 HOI4Lang.English ]; experimental = false }
      rules = Some { ruleFiles = configs; validateRules = true; debugRulesOnly = false; debugMode = false }
      modFilter = None
      maxFileSize = None }

let perfHOI4ModCached runTests =
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    perfRunner (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        HOI4Game(buildHoi4Settings @"D:\Synced\Git\Third Party\Hearts of Iron IV\Millennium_Dawn" @"D:\Synced\Git\Personal\cwtools-hoi4-config\Config" true) :> IGame<_>) runTests

let perfHOI4Vanilla runTests =
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    perfRunner (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        HOI4Game(buildHoi4Settings @"D:\Games\Steam\steamapps\common\Hearts of Iron IV" @"C:\Users\Thomas\Git\cwtools-hoi4-config\Config" false) :> IGame<_>) runTests