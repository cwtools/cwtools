module CWToolsPerformanceCLI.Piecemeal



// let buildGamePerf (buildGame: unit -> IGame<_>) =
//     let timer = Stopwatch()
//     timer.Start()
//     let game = buildGame()
//     timer.Stop()
//     { ElapsedMilliseconds = timer.ElapsedMilliseconds; ErrorCount = 0 }
//
// let perfStellarisBuildGame rootDir configPath (cachePath: string option) (modPath: string option) (steamRoot: string option) (gitRoot: string option) =
//     let pathConfig = createPathConfig steamRoot gitRoot
//     let (defaultStellarisRoot, defaultStellarisConfig, defaultStellarisCache), _, _, _ = getDefaultGamePaths pathConfig
//     
//     let useCache = cachePath.IsSome
//     let defaultRootDir = defaultArg rootDir defaultStellarisRoot
//     let defaultConfigPath = defaultArg configPath defaultStellarisConfig
//     let defaultCachePath = defaultArg cachePath defaultStellarisCache
//     
//     // Enable verbose logging by default
//     CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
//     
//     buildGamePerf (fun () ->
//         scopeManager.ReInit(defaultScopeInputs, [])
//         let settings = buildStlSettings defaultRootDir defaultConfigPath false useCache defaultCachePath
//         // Add mod path if provided
//         let finalSettings = 
//             match modPath with
//             | Some mp -> { settings with rootDirectories = settings.rootDirectories @ [WorkspaceDirectoryInput.WD { path = mp; name = "mod" }] }
//             | None -> settings
//         STLGame(finalSettings) :> IGame<_>)
