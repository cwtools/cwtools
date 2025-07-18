module CWToolsTests

open CWToolsCLI
open PerfCommon
open PerfFunctions
open Expecto
open System.Text
open CWTools.Parser.DocsParser
open CWTools.Games
open FParsec
open CWTools.Common
open CWTools.Parser
open System.IO
open CWTools.Games.Files
open CWTools.Games.Stellaris
open System.Threading
open System.Globalization
open CWTools.Common.STLConstants
open System
open System.Diagnostics
open Expecto.Logging
open Expecto.Logging.Message

let logger = Log.create "MyTests"
CWTools.Utilities.Utils.logDiag <- logger.logSimple << (event LogLevel.Verbose)
CWTools.Utilities.Utils.logInfo <- logger.logSimple << (event LogLevel.Info)
CWTools.Utilities.Utils.logWarning <- logger.logSimple << (event LogLevel.Warn)
CWTools.Utilities.Utils.logError <- logger.logSimple << (event LogLevel.Error)


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










let test () =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()

    match CKParser.parseFile "./combat_locators.txt" with
    | Success(a, _, _) -> ()
    | Failure(a, _, _) -> printfn "%A" a

    eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds

[<EntryPoint>]
let main argv =
    let config =
        { defaultConfig with
            runInParallel = false }
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

    CultureInfo.DefaultThreadCurrentCulture <- CultureInfo("ru-RU")
    CultureInfo.DefaultThreadCurrentUICulture <- CultureInfo("ru-RU")
    Thread.CurrentThread.CurrentCulture <- CultureInfo("ru-RU")
    Thread.CurrentThread.CurrentUICulture <- CultureInfo("ru-RU")
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose

    if Array.tryHead argv = Some "p" then
        perfStellarisManualTest (true)
        0
    elif Array.tryHead argv = Some "u" then
        perfStellarisVerboseTest (true)
        perfStellarisVerboseTest (true)
        0
    elif Array.tryHead argv = Some "i" then
        perfStellarisVerboseTest (true)
        0
    elif Array.tryHead argv = Some "o" then
        perfStellarisManualTest (false)
        0
    elif Array.tryHead argv = Some "q" then
        perfEU4Vanilla (true)
        0
    elif Array.tryHead argv = Some "y" then
        perfEU4Custom (true)
        0
    elif Array.tryHead argv = Some "z" then
        perfCK3Vanilla (true)
        0
    elif Array.tryHead argv = Some "t" then
        test ()
        test ()
        0
    elif Array.tryHead argv = Some "j" then
        perfHOI4ModCached (true)
        perfHOI4ModCached (true)
        0
    elif Array.tryHead argv = Some "jj" then
        perfHOI4Vanilla(true)
        perfHOI4Vanilla(true)
        0
    elif Array.tryHead argv = Some "k" then
        perfHOI4ModCached (true)
        0
    elif Array.tryHead argv = Some "s" then
        perfStellarisModCached (true)
        perfStellarisModCached (true)
        0
    elif Array.tryHead argv = Some "S" then
        perfStellarisModCached (true)
        0
    elif Array.tryHead argv = Some "stl-then-cache" then
        perfStellarisModCached (true)
        0
    else
        Tests.runTestsInAssemblyWithCLIArgs [ Sequenced; CLIArguments.Summary ] argv
