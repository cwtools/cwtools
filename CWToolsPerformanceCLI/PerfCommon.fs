module CWToolsPerformanceCLI.PerfCommon

open System.Diagnostics
open System.IO
open CWTools.Common
open CWTools.Games
open CWTools.Games.Files
open CWTools.Games.Stellaris

// Directory helpers
let rec getAllFolders dirs =
    if Seq.isEmpty dirs then
        Seq.empty
    else
        seq {
            yield! dirs |> Seq.collect Directory.EnumerateDirectories
            yield! getAllFolders (dirs |> Seq.collect Directory.EnumerateDirectories)
        }

let getAllFoldersUnion dirs =
    seq {
        yield! dirs
        yield! getAllFolders dirs
    }

let getFolderList (filename: string, filetext: string) =
    if Path.GetFileName filename = "folders.cwt" then
        Some(
            filetext.Split([| "\r\n"; "\r"; "\n" |], System.StringSplitOptions.None)
            |> List.ofArray
            |> List.filter ((<>) "")
        )
    else
        None

// Empty settings
let emptyEmbeddedSettings =
    { EmbeddedSettings.triggers = []
      effects = []
      modifiers = []
      embeddedFiles = []
      cachedResourceData = []
      localisationCommands = Legacy([], [], [])
      eventTargetLinks = []
      cachedRuleMetadata = None
      featureSettings = CWTools.Parser.UtilityParser.FeatureSettings.Default }

let emptyStellarisSettings rootDirectory =
    { StellarisSettings.rootDirectories = [ WD { name = "test"; path = rootDirectory } ]
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

// Config file helpers
let enumerateConfigFiles basePath extensions =
    if Directory.Exists basePath then
        getAllFoldersUnion [ basePath ]
        |> Seq.collect Directory.EnumerateFiles
        |> Seq.filter (fun f -> extensions |> List.contains (Path.GetExtension f))
        |> List.ofSeq
    else
        []

let readConfigFiles files =
    files |> List.map (fun f -> f, File.ReadAllText f)

// Performance runner
let perfRunner (buildGame: unit -> IGame<_>) runValidation =
    let timer = Stopwatch()
    timer.Start()
    let game = buildGame ()

    if runValidation then
        let errors = game.ValidationErrors() |> List.map (fun e -> e.range)
        let testVals = game.AllEntities()
        game.RefreshCaches()
        eprintfn "Elapsed Time: %i, %i errors" timer.ElapsedMilliseconds errors.Length
    else
        ()

    eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
