module Program

open System.Text
open System.Threading
open System.Globalization
open Argu
open CliArguments
open CWToolsPerformanceCLI.PerfFunctions

// Register encoding and set culture (copied from Main.fs)
let initializeCulture () =
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)
    CultureInfo.DefaultThreadCurrentCulture <- CultureInfo("ru-RU")
    CultureInfo.DefaultThreadCurrentUICulture <- CultureInfo("ru-RU")
    Thread.CurrentThread.CurrentCulture <- CultureInfo("ru-RU")
    Thread.CurrentThread.CurrentUICulture <- CultureInfo("ru-RU")

// Extract path arguments from parsed args
let getGamePath (results: ParseResults<PerformanceArgs>) = results.TryGetResult Game_Path

let getConfigPath (results: ParseResults<PerformanceArgs>) = results.TryGetResult Config_Path

let getCachePath (results: ParseResults<PerformanceArgs>) = results.TryGetResult Cache_Path


let getModPath (results: ParseResults<PerformanceArgs>) = results.TryGetResult Mod_Path

let getSteamRoot (results: ParseResults<PerformanceArgs>) = results.TryGetResult Steam_Root

let getGitRoot (results: ParseResults<PerformanceArgs>) = results.TryGetResult Git_Root

// Run performance test and handle results
let runPerfTest testName testFunc =
    try
        printfn "Running %s..." testName
        let result = testFunc
        printfn "✓ %s completed successfully" testName
        printfn "  Elapsed: %dms, Errors: %d" result.ElapsedMilliseconds result.ErrorCount
        0
    with ex ->
        eprintfn "✗ %s failed: %s" testName ex.Message
        1

// Main program logic
let runCommand (results: ParseResults<PerformanceArgs>) =
    let gamePath = getGamePath results
    let configPath = getConfigPath results
    let cachePath = getCachePath results
    let modPath = getModPath results
    let steamRoot = getSteamRoot results
    let gitRoot = getGitRoot results
    // Default to running validation tests
    let runTests = true

    if results.Contains Stellaris then
        let modInfo =
            match modPath with
            | Some _ -> " + mod"
            | None -> ""

        runPerfTest
            (sprintf "Stellaris Test %s" modInfo)
            (perfStellaris gamePath configPath cachePath modPath steamRoot gitRoot runTests)
    elif results.Contains EU4 then
        let modInfo =
            match modPath with
            | Some _ -> " + mod"
            | None -> ""

        runPerfTest
            (sprintf "EU4 Test %s" modInfo)
            (perfEU4 gamePath configPath cachePath modPath steamRoot gitRoot runTests)
    elif results.Contains HOI4 then
        let cacheInfo = if cachePath.IsSome then " (cached)" else ""

        let modInfo =
            match modPath with
            | Some _ -> " + mod"
            | None -> ""

        runPerfTest
            (sprintf "HOI4 Test%s%s" cacheInfo modInfo)
            (perfHOI4 gamePath configPath cachePath modPath steamRoot gitRoot runTests)
    elif results.Contains CK3 then
        let cacheInfo = if cachePath.IsSome then " (cached)" else ""

        let modInfo =
            match modPath with
            | Some _ -> " + mod"
            | None -> ""

        runPerfTest
            (sprintf "CK3 Test%s%s" cacheInfo modInfo)
            (perfCK3 gamePath configPath cachePath modPath steamRoot gitRoot runTests)
    else
        eprintfn "No valid command specified. Use --help for usage information."
        1

[<EntryPoint>]
let main argv =
    // Initialize culture and encoding
    initializeCulture ()

    try
        // Parse command line arguments
        let parser =
            ArgumentParser.Create<PerformanceArgs>(programName = "CWToolsPerformanceCLI")

        let results = parser.Parse(argv)

        // Run the specified command
        runCommand results
    with
    | :? ArguParseException as ex ->
        eprintfn "Argument parsing error: %s" ex.Message
        1
    | ex ->
        eprintfn "Runtime error: %s" ex.Message
        1
