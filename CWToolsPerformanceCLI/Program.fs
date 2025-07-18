module Program

open System
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
let getGamePath (results: ParseResults<PerformanceArgs>) =
    results.TryGetResult Game_Path

let getConfigPath (results: ParseResults<PerformanceArgs>) =
    results.TryGetResult Config_Path

let getCachePath (results: ParseResults<PerformanceArgs>) =
    results.TryGetResult Cache_Path

let getFilePath (results: ParseResults<PerformanceArgs>) =
    results.TryGetResult File_Path

let getMode (results: ParseResults<PerformanceArgs>) =
    results.TryGetResult Mode

// Run performance test and handle results
let runPerfTest testName testFunc =
    try
        printfn "Running %s..." testName
        let result = testFunc
        printfn "✓ %s completed successfully" testName
        printfn "  Elapsed: %dms, Errors: %d" result.ElapsedMilliseconds result.ErrorCount
        0
    with
    | ex ->
        eprintfn "✗ %s failed: %s" testName ex.Message
        1

// Main program logic
let runCommand (results: ParseResults<PerformanceArgs>) =
    let gamePath = getGamePath results
    let configPath = getConfigPath results
    let cachePath = getCachePath results
    let filePath = getFilePath results
    let mode = getMode results
    
    // Default to running validation tests
    let runTests = true
    
    if results.Contains Stellaris then
        let modeStr = defaultArg mode "verbose"
        runPerfTest (sprintf "Stellaris Test (%s)" modeStr) (perfStellaris gamePath configPath cachePath mode runTests)
    elif results.Contains EU4 then
        let modeStr = defaultArg mode "vanilla"
        runPerfTest (sprintf "EU4 Test (%s)" modeStr) (perfEU4 gamePath configPath cachePath mode runTests)
    elif results.Contains HOI4 then
        let cacheInfo = if cachePath.IsSome then " (cached)" else ""
        runPerfTest (sprintf "HOI4 Test%s" cacheInfo) (perfHOI4 gamePath configPath cachePath mode runTests)
    elif results.Contains CK3 then
        let cacheInfo = if cachePath.IsSome then " (cached)" else ""
        runPerfTest (sprintf "CK3 Test%s" cacheInfo) (perfCK3 gamePath configPath cachePath mode runTests)
    elif results.Contains Parse_Test then
        runPerfTest "Parse Test" (test filePath)
    else
        eprintfn "No valid command specified. Use --help for usage information."
        1

[<EntryPoint>]
let main argv =
    // Initialize culture and encoding
    initializeCulture()
    
    try
        // Parse command line arguments
        let parser = ArgumentParser.Create<PerformanceArgs>(programName = "CWToolsPerformanceCLI")
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
