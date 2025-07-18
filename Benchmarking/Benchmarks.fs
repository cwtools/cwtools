module Benchmarks

open System
open System.IO
open System.Diagnostics
open System.Text
open System.Threading
open System.Globalization
open CWTools.Common
open CWTools.Common.STLConstants
open CWTools.Games
open CWTools.Games.Files
open CWTools.Games.Stellaris
open CWTools.Games.EU4
open CWTools.Games.HOI4
open CWTools.Games.CK3
open CWTools.Parser.DocsParser
open CWTools.Parser
open CWToolsCLI
open FParsec

// Default paths - these can be overridden by command line arguments
let defaultGamePaths = 
    Map.ofList [
        ("stellaris", [
            Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), "Documents", "Paradox Interactive", "Stellaris", "mod")
            @"C:\Program Files (x86)\Steam\steamapps\common\Stellaris"
            @"D:\Games\Steam\steamapps\common\Stellaris"
        ])
        ("eu4", [
            @"C:\Program Files (x86)\Steam\steamapps\common\Europa Universalis IV"
            @"D:\Games\Steam\steamapps\common\Europa Universalis IV"
        ])
        ("hoi4", [
            @"C:\Program Files (x86)\Steam\steamapps\common\Hearts of Iron IV"
            @"D:\Games\Steam\steamapps\common\Hearts of Iron IV"
        ])
        ("ck3", [
            @"C:\Program Files (x86)\Steam\steamapps\common\Crusader Kings III\game"
            @"D:\Games\Steam\steamapps\common\Crusader Kings III\game"
        ])
    ]

let defaultConfigPaths = 
    Map.ofList [
        ("stellaris", [
            "./testfiles/performancetest2/.cwtools"
            "./testfiles/performancetest/.cwtools"
        ])
        ("eu4", [
            "./testfiles/custom/rules"
        ])
        ("hoi4", [
            "./testfiles/custom/rules"
        ])
        ("ck3", [
            "./testfiles/custom/rules"
        ])
    ]

let findFirstExistingPath paths =
    paths |> List.tryFind Directory.Exists

let getGamePath game userPath =
    match userPath with
    | Some path when Directory.Exists path -> path
    | _ -> 
        match Map.tryFind game defaultGamePaths with
        | Some paths -> 
            match findFirstExistingPath paths with
            | Some path -> path
            | None -> failwithf "Could not find %s installation. Please specify path with --game-path option" game
        | None -> failwithf "Unknown game: %s" game

let getConfigPath game userPath =
    match userPath with
    | Some path when Directory.Exists path -> path
    | _ -> 
        match Map.tryFind game defaultConfigPaths with
        | Some paths -> 
            match findFirstExistingPath paths with
            | Some path -> path
            | None -> 
                printfn "Warning: No config path found for %s. Using empty config." game
                ""
        | None -> ""

let rec getAllFolders dirs =
    if Seq.isEmpty dirs then
        Seq.empty
    else
        seq {
            yield! dirs |> Seq.collect Directory.EnumerateDirectories
            yield! dirs |> Seq.collect Directory.EnumerateDirectories |> getAllFolders
        }

let getAllFoldersUnion dirs =
    seq {
        yield! dirs
        yield! getAllFolders dirs
    }

let emptyEmbeddedSettings =
    { triggers = []
      effects = []
      modifiers = []
      embeddedFiles = []
      cachedResourceData = []
      localisationCommands = Legacy([], [], [])
      eventTargetLinks = []
      cachedRuleMetadata = None
      featureSettings = CWTools.Parser.UtilityParser.FeatureSettings.Default }

let emptyStellarisSettings (rootDirectory) =
    { rootDirectories = [ WD { name = "test"; path = rootDirectory } ]
      modFilter = None
      validation =
        { validateVanilla = false
          experimental = true
          langs = [ STL STLLang.English ] }
      rules = None
      embedded = FromConfig([], [])
      scriptFolders = None
      excludeGlobPatterns = None
      maxFileSize = None }

let getFolderList (filename: string, filetext: string) =
    if Path.GetFileName filename = "folders.cwt" then
        Some(
            filetext.Split(([| "\r\n"; "\r"; "\n" |]), StringSplitOptions.None)
            |> List.ofArray
            |> List.filter (fun s -> s <> "")
        )
    else
        None

let getConfigFiles configPath =
    if String.IsNullOrEmpty configPath || not (Directory.Exists configPath) then
        []
    else
        getAllFoldersUnion ([ configPath ] |> Seq.ofList)
        |> Seq.collect (Directory.EnumerateFiles)
        |> Seq.filter (fun f -> Path.GetExtension f = ".cwt" || Path.GetExtension f = ".log")
        |> Seq.map (fun f -> f, File.ReadAllText(f))
        |> List.ofSeq

let tryLoadCache cacheFilePathOpt =
    match cacheFilePathOpt with
    | Some cachePath when File.Exists cachePath ->
        try
            CWToolsCLI.Serializer.deserialize cachePath
        with
        | _ -> ([], [])
    | _ -> ([], [])

let runWithTimer description action =
    let timer = new Stopwatch()
    timer.Start()
    printfn "Starting %s..." description
    
    try
        action()
        printfn "%s completed in %i ms" description timer.ElapsedMilliseconds
    with
    | ex ->
        printfn "%s failed after %i ms: %s" description timer.ElapsedMilliseconds ex.Message
        reraise()

let runStellarisTest gamePathOpt configPathOpt cachePathOpt =
    let gamePath = getGamePath "stellaris" gamePathOpt
    let configPath = getConfigPath "stellaris" configPathOpt
    
    runWithTimer "Stellaris performance test" (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        let configs = getConfigFiles configPath
        let folders = configs |> List.tryPick getFolderList
        
        let settings = {
            rootDirectories = [ WD { name = "test"; path = gamePath } ]
            modFilter = None
            validation = {
                validateVanilla = true
                experimental = false
                langs = [ STL STLLang.English ]
            }
            rules = 
                if List.isEmpty configs then None
                else Some {
                    validateRules = true
                    ruleFiles = configs
                    debugRulesOnly = false
                    debugMode = false
                }
            embedded = FromConfig([], [])
            scriptFolders = folders
            excludeGlobPatterns = None
            maxFileSize = None
        }
        
        let game = STLGame(settings) :> IGame<STLComputedData>
        let _errors = game.ValidationErrors() |> List.map (fun e -> e.range)
        let _entities = game.AllEntities()
        game.RefreshCaches()
        printfn "Stellaris test completed with %i errors" (_errors |> List.length)
    )

let runEU4Test gamePathOpt configPathOpt cachePathOpt =
    let gamePath = getGamePath "eu4" gamePathOpt
    let configPath = getConfigPath "eu4" configPathOpt
    
    runWithTimer "EU4 performance test" (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        let configs = getConfigFiles configPath
        let folders = configs |> List.tryPick getFolderList
        
        let settings = {
            CWTools.Games.EU4.EU4Settings.rootDirectories = 
                [ WD { WorkspaceDirectory.name = "EU4"; path = gamePath } ]
            modFilter = None
            scriptFolders = folders
            excludeGlobPatterns = None
            validation = {
                validateVanilla = true
                experimental = false
                langs = [ EU4 EU4Lang.English ]
            }
            rules = 
                if List.isEmpty configs then None
                else Some {
                    ruleFiles = configs
                    validateRules = true
                    debugRulesOnly = false
                    debugMode = false
                }
            embedded = FromConfig([], [])
            maxFileSize = None
        }
        
        let game = CWTools.Games.EU4.EU4Game(settings) :> IGame<EU4ComputedData>
        let _errors = game.ValidationErrors() |> List.map (fun e -> e.range)
        let _locErrors = game.LocalisationErrors(true, true) |> List.map (fun e -> e.range)
        let _entities = game.AllEntities()
        game.RefreshCaches()
        printfn "EU4 test completed with %i validation errors, %i localisation errors" (_errors |> List.length) (_locErrors |> List.length)
    )



let runHOI4Test gamePathOpt configPathOpt cachePathOpt =
    let gamePath = getGamePath "hoi4" gamePathOpt
    let configPath = getConfigPath "hoi4" configPathOpt
    
    runWithTimer "HOI4 performance test" (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        let configs = getConfigFiles configPath
        let folders = configs |> List.tryPick getFolderList
        
        let settings = {
            rootDirectories = 
                [ WorkspaceDirectoryInput.WD { path = gamePath; name = "HOI4" } ]
            scriptFolders = folders
            excludeGlobPatterns = None
            embedded = FromConfig([], [])
            validation = {
                validateVanilla = true
                langs = [ Lang.HOI4 HOI4Lang.English ]
                experimental = false
            }
            rules = 
                if List.isEmpty configs then None
                else Some {
                    ruleFiles = configs
                    validateRules = true
                    debugRulesOnly = false
                    debugMode = false
                }
            modFilter = None
            maxFileSize = None
        }
        
        let game = CWTools.Games.HOI4.HOI4Game(settings) :> IGame<HOI4ComputedData>
        let _errors = game.ValidationErrors() |> List.map (fun e -> e.range)
        let _entities = game.AllEntities()
        game.RefreshCaches()
        printfn "HOI4 test completed with %i errors" (_errors |> List.length)
    )

let runVanillaHOI4Test gamePathOpt configPathOpt cachePathOpt =
    let gamePath = getGamePath "hoi4" gamePathOpt
    let configPath = getConfigPath "hoi4" configPathOpt
    
    runWithTimer "HOI4 vanilla performance test" (fun () ->
        CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
        scopeManager.ReInit(defaultScopeInputs, [])
        
        let configs = getConfigFiles configPath
        let folders = configs |> List.tryPick getFolderList
        
        let settings = {
            rootDirectories = 
                [ WorkspaceDirectoryInput.WD { path = gamePath; name = "HOI4" } ]
            scriptFolders = folders
            excludeGlobPatterns = None
            embedded = FromConfig([], [])
            validation = {
                validateVanilla = true
                langs = [ Lang.HOI4 HOI4Lang.English ]
                experimental = false
            }
            rules = 
                if List.isEmpty configs then None
                else Some {
                    ruleFiles = configs
                    validateRules = true
                    debugRulesOnly = false
                    debugMode = false
                }
            modFilter = None
            maxFileSize = None
        }
        
        let game = CWTools.Games.HOI4.HOI4Game(settings) :> IGame<HOI4ComputedData>
        let _errors = game.ValidationErrors() |> List.map (fun e -> e.range)
        let _entities = game.AllEntities()
        game.RefreshCaches()
        printfn "HOI4 vanilla test completed with %i errors" (_errors |> List.length)
    )

let runCK3Test gamePathOpt configPathOpt cachePathOpt =
    let gamePath = getGamePath "ck3" gamePathOpt
    let configPath = getConfigPath "ck3" configPathOpt
    
    runWithTimer "CK3 performance test" (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        let configs = getConfigFiles configPath
        let folders = configs |> List.tryPick getFolderList
        
        let settings = {
            CWTools.Games.CK3.CK3Settings.rootDirectories = 
                [ WD { WorkspaceDirectory.name = "CK3"; path = gamePath } ]
            modFilter = None
            scriptFolders = folders
            excludeGlobPatterns = None
            validation = {
                validateVanilla = true
                experimental = false
                langs = [ CK3 CK3Lang.English ]
            }
            rules = 
                if List.isEmpty configs then None
                else Some {
                    ruleFiles = configs
                    validateRules = true
                    debugRulesOnly = false
                    debugMode = false
                }
            embedded = FromConfig([], [])
            maxFileSize = None
        }
        
        let game = CWTools.Games.CK3.CK3Game(settings) :> IGame<CK3ComputedData>
        let _errors = game.ValidationErrors() |> List.map (fun e -> e.range)
        let _locErrors = game.LocalisationErrors(true, true) |> List.map (fun e -> e.range)
        let _entities = game.AllEntities()
        game.RefreshCaches()
        printfn "CK3 test completed with %i validation errors, %i localisation errors" (_errors |> List.length) (_locErrors |> List.length)
    )

let runParseTest filePathOpt cachePathOpt =
    let filePath = 
        match filePathOpt with
        | Some path when File.Exists path -> path
        | _ -> 
            let defaultPath = "./combat_locators.txt"
            if File.Exists defaultPath then defaultPath
            else failwith "Test file not found. Please specify with --test option or ensure ./combat_locators.txt exists"
    
    runWithTimer (sprintf "Parse test on %s" filePath) (fun () ->
        match CKParser.parseFile filePath with
        | Success(_, _, _) -> printfn "Parse successful"
        | Failure(error, _, _) -> printfn "Parse failed: %A" error
    )

let runModdedHOI4Test gamePathOpt configPathOpt cacheFilePathOpt =
    let gamePath = getGamePath "hoi4" gamePathOpt
    let configPath = getConfigPath "hoi4" configPathOpt
    
    runWithTimer "Modded HOI4 performance test" (fun () ->
        CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
        scopeManager.ReInit(defaultScopeInputs, [])
        
        let cached, cachedFiles = tryLoadCache cacheFilePathOpt
        
        let configs = getConfigFiles configPath
        let folders = configs |> List.tryPick getFolderList
        
        let hoi4settings = {
            rootDirectories = [
                WorkspaceDirectoryInput.WD { path = gamePath; name = "test" }
            ]
            scriptFolders = folders
            excludeGlobPatterns = None
            embedded = 
                if List.isEmpty cached && List.isEmpty cachedFiles then
                    FromConfig([], [])
                else
                    FromConfig(cachedFiles, cached)
            validation = {
                validateVanilla = false
                langs = [ Lang.HOI4 HOI4Lang.English ]
                experimental = false
            }
            rules = 
                if List.isEmpty configs then None
                else Some {
                    ruleFiles = configs
                    validateRules = true
                    debugRulesOnly = false
                    debugMode = false
                }
            modFilter = None
            maxFileSize = None
        }
        
        let game = CWTools.Games.HOI4.HOI4Game(hoi4settings) :> IGame<HOI4ComputedData>
        let errors = game.ValidationErrors() |> List.map (fun e -> e.range)
        let testVals = game.AllEntities()
        game.RefreshCaches()
        printfn "Modded HOI4 test completed with %i errors" (errors |> List.length)
    )

// Basic Stellaris benchmark (perf from Main.fs)
let runBasicStellarisTest gamePathOpt configPathOpt cachePathOpt =
    let gamePath = 
        match gamePathOpt with
        | Some path when Directory.Exists path -> path
        | _ -> "./testfiles/performancetest/"
    
    let configPath = 
        match configPathOpt with
        | Some path when Directory.Exists path -> path
        | _ -> "./testfiles/performancetest2/.cwtools"
    
    runWithTimer "Basic Stellaris performance test" (fun () ->
        let triggers, effects =
            DocsParser.parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.2.txt"
            |> (function
            | Success(p, _, _) -> (DocsParser.processDocs (scopeManager.ParseScopes)) p)

        let configFiles = getConfigFiles configPath
        
        let settings = {
            rootDirectories = [ WD { name = "test"; path = gamePath } ]
            modFilter = None
            validation = {
                validateVanilla = false
                experimental = true
                langs = [ STL STLLang.English ]
            }
            rules = 
                if List.isEmpty configFiles then None
                else Some {
                    validateRules = true
                    ruleFiles = configFiles
                    debugRulesOnly = false
                    debugMode = false
                }
            embedded = 
                ManualSettings {
                    emptyEmbeddedSettings with
                        triggers = triggers
                        effects = effects
                }
            scriptFolders = None
            excludeGlobPatterns = None
            maxFileSize = None
        }
        
        let stl = STLGame(settings) :> IGame<STLComputedData>
        let _errors = stl.ValidationErrors() |> List.map (fun e -> e.range)
        let _entities = stl.AllEntities()
        printfn "Basic Stellaris test completed with %i errors" (_errors |> List.length)
    )

// Enhanced Stellaris benchmark (perf2 from Main.fs)
let runEnhancedStellarisTest gamePathOpt configPathOpt cachePathOpt =
    let gamePath = 
        match gamePathOpt with
        | Some path when Directory.Exists path -> path
        | _ -> "./testfiles/performancetest2/"
    
    let configPath = 
        match configPathOpt with
        | Some path when Directory.Exists path -> path
        | _ -> "./testfiles/performancetest2/.cwtools"
    
    runWithTimer "Enhanced Stellaris performance test" (fun () ->
        CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
        scopeManager.ReInit(defaultScopeInputs, [])
        
        let configFiles = getConfigFiles configPath
        
        let settings = {
            rootDirectories = [ WD { name = "test"; path = gamePath } ]
            modFilter = None
            validation = {
                validateVanilla = false
                experimental = true
                langs = [ STL STLLang.English ]
            }
            rules = 
                if List.isEmpty configFiles then None
                else Some {
                    validateRules = true
                    ruleFiles = configFiles
                    debugRulesOnly = false
                    debugMode = false
                }
            embedded = FromConfig(configFiles, [])
            scriptFolders = None
            excludeGlobPatterns = None
            maxFileSize = None
        }
        
        let stl = STLGame(settings) :> IGame<STLComputedData>
        let _errors = stl.ValidationErrors() |> List.map (fun e -> e.range)
        let _entities = stl.AllEntities()
        stl.RefreshCaches()
        printfn "Enhanced Stellaris test completed with %i errors" (_errors |> List.length)
    )

// Advanced Stellaris benchmark (perfSTL from Main.fs)
let runAdvancedStellarisTest gamePathOpt configPathOpt cachePathOpt =
    let gamePath = getGamePath "stellaris" gamePathOpt
    let configPath = getConfigPath "stellaris" configPathOpt
    
    runWithTimer "Advanced Stellaris performance test" (fun () ->
        CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
        scopeManager.ReInit(defaultScopeInputs, [])
        
        let cached, cachedFiles = tryLoadCache cachePathOpt
        
        let configs = getConfigFiles configPath
        let folders = configs |> List.tryPick getFolderList
        
        let settings = {
            rootDirectories = [ WD { name = "test"; path = gamePath } ]
            modFilter = None
            validation = {
                validateVanilla = true
                experimental = false
                langs = [ STL STLLang.English ]
            }
            rules = 
                Some {
                    validateRules = true
                    ruleFiles = configs
                    debugRulesOnly = false
                    debugMode = false
                }
            embedded = FromConfig(cachedFiles, cached)
            scriptFolders = folders
            excludeGlobPatterns = None
            maxFileSize = None
        }
        
        let game = STLGame(settings) :> IGame<STLComputedData>
        let _errors = game.ValidationErrors() |> List.map (fun e -> e.range)
        let _entities = game.AllEntities()
        game.RefreshCaches()
        printfn "Advanced Stellaris test completed with %i errors" (_errors |> List.length)
    )
