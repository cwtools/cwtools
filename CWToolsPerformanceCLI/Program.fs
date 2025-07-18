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
    
    // Default to running validation tests
    let runTests = true
    
    if results.Contains Stellaris_Manual then
        runPerfTest "Stellaris Manual Test" (perfStellarisManualTest gamePath configPath runTests)
    elif results.Contains Stellaris_Verbose then
        runPerfTest "Stellaris Verbose Test" (perfStellarisVerboseTest gamePath configPath runTests)
    elif results.Contains Stellaris_ModCached then
        runPerfTest "Stellaris ModCached Test" (perfStellarisModCached gamePath configPath cachePath runTests)
    elif results.Contains EU4_Vanilla then
        runPerfTest "EU4 Vanilla Test" (perfEU4Vanilla gamePath configPath cachePath runTests)
    elif results.Contains EU4_Custom then
        runPerfTest "EU4 Custom Test" (perfEU4Custom gamePath configPath cachePath runTests)
    elif results.Contains HOI4_Vanilla then
        runPerfTest "HOI4 Vanilla Test" (perfHOI4Vanilla gamePath configPath cachePath runTests)
    elif results.Contains HOI4_ModCached then
        runPerfTest "HOI4 ModCached Test" (perfHOI4ModCached gamePath configPath cachePath runTests)
    elif results.Contains CK3_Vanilla then
        runPerfTest "CK3 Vanilla Test" (perfCK3Vanilla gamePath configPath cachePath runTests)
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
