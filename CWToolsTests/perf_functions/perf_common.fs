module PerfCommon

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

// Helper functions from Main.fs
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

let getFolderList (filename: string, filetext: string) =
    if Path.GetFileName filename = "folders.cwt" then
        Some(
            filetext.Split(([| "\r\n"; "\r"; "\n" |]), System.StringSplitOptions.None)
            |> List.ofArray
            |> List.filter (fun s -> s <> "")
        )
    else
        None

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

let enumerateConfigFiles basePath extensions =
    if Directory.Exists basePath then
        getAllFoldersUnion ([ basePath ] |> Seq.ofList)
        |> Seq.collect Directory.EnumerateFiles
        |> Seq.filter (fun f -> extensions |> List.contains (Path.GetExtension f))
        |> List.ofSeq
    else
        []

let readConfigFiles files =
    files |> List.map (fun f -> (f, File.ReadAllText(f)))

let perfRunner (buildGame : unit -> IGame<_>) runValidation =
    let timer = Stopwatch()
    timer.Start()
    let game = buildGame ()
    if runValidation then
        let errors = game.ValidationErrors() |> List.map (fun e -> e.range)
        let testVals = game.AllEntities()
        game.RefreshCaches()
        eprintfn "Elapsed Time: %i, %i errors" timer.ElapsedMilliseconds (errors |> List.length)
    else
        ()
    eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds

let createPerfRunner scopeInit settingsFactory gameCtor =
    perfRunner (fun () ->
        scopeManager.ReInit(defaultScopeInputs, [])
        gameCtor (settingsFactory()) :> IGame<_>
    )
