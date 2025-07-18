module BenchmarkingApp

open Argu
open System.Text
open System.Threading
open System.Globalization

// Define the CLI arguments
[<CliPrefix(CliPrefix.DoubleDash)>]
type BenchmarkArgs =
    | [<AltCommandLine("-stl")>] Stellaris
    | [<AltCommandLine("-eu4")>] EU4
    | [<AltCommandLine("-hoi4")>] HOI4
    | [<AltCommandLine("-ck3")>] CK3
    | [<AltCommandLine("-t")>] Test
    | [<AltCommandLine("-basic-stl")>] BasicStellaris
    | [<AltCommandLine("-enhanced-stl")>] EnhancedStellaris
    | [<AltCommandLine("-g")>] Game_Path of string
    | [<AltCommandLine("-c")>] Config_Path of string
    | [<AltCommandLine("-f")>] File_Path of string
    | [<AltCommandLine("-astl")>] AdvancedStellaris
    | [<AltCommandLine("-vhoi4")>] VanillaHOI4
    | [<AltCommandLine("-mhoi4")>] ModdedHOI4
    | [<AltCommandLine("-p")>] Cache_Path of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Stellaris -> "Run Stellaris performance test"
            | EU4 -> "Run EU4 performance test"
            | HOI4 -> "Run HOI4 performance test"
            | CK3 -> "Run CK3 performance test"
            | Test -> "Run parsing test"
            | BasicStellaris -> "Run basic Stellaris performance test (perf from Main.fs)"
            | EnhancedStellaris -> "Run enhanced Stellaris performance test (perf2 from Main.fs)"
            | AdvancedStellaris -> "Run advanced Stellaris performance test"
            | VanillaHOI4 -> "Run vanilla HOI4 performance test"
            | ModdedHOI4 -> "Run modded HOI4 performance test"
            | Cache_Path _ -> "Path to optional .cwb cache file"
            | Game_Path _ -> "Path to game installation directory"
            | Config_Path _ -> "Path to config directory"
            | File_Path _ -> "Path to file for parsing test"


[<EntryPoint>]
let main argv =
    // Initialize encodings and culture
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)
    CultureInfo.DefaultThreadCurrentCulture <- CultureInfo("ru-RU")
    CultureInfo.DefaultThreadCurrentUICulture <- CultureInfo("ru-RU")
    Thread.CurrentThread.CurrentCulture <- CultureInfo("ru-RU")
    Thread.CurrentThread.CurrentUICulture <- CultureInfo("ru-RU")
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    
    let parser = ArgumentParser.Create<BenchmarkArgs>(programName = "cwtools-benchmarks")
    
    try
        let results = parser.Parse(argv)
        
        let gamePath = results.TryGetResult Game_Path
        let configPath = results.TryGetResult Config_Path
        let filePath = results.TryGetResult File_Path
        let cachePath = results.TryGetResult Cache_Path
        
        let benchmarkResults = results.GetAllResults() |> List.filter (function
            | Game_Path _ | Config_Path _ | File_Path _ | Cache_Path _ -> false
            | _ -> true)
        
        match benchmarkResults with
        | [ Stellaris ] -> Benchmarks.runStellarisTest gamePath configPath cachePath
        | [ EU4 ] -> Benchmarks.runEU4Test gamePath configPath cachePath
        | [ HOI4 ] -> Benchmarks.runHOI4Test gamePath configPath cachePath
        | [ CK3 ] -> Benchmarks.runCK3Test gamePath configPath cachePath
        | [ Test ] -> Benchmarks.runParseTest filePath cachePath
        | [ BasicStellaris ] -> Benchmarks.runBasicStellarisTest gamePath configPath cachePath
        | [ EnhancedStellaris ] -> Benchmarks.runEnhancedStellarisTest gamePath configPath cachePath
        | [ AdvancedStellaris ] -> Benchmarks.runAdvancedStellarisTest gamePath configPath cachePath
        | [ VanillaHOI4 ] -> Benchmarks.runVanillaHOI4Test gamePath configPath cachePath
        | [ ModdedHOI4 ] -> Benchmarks.runModdedHOI4Test gamePath configPath cachePath
        | [] -> printfn "%s" (parser.PrintUsage())
        | _ -> printfn "Please specify only one benchmark to run.\n%s" (parser.PrintUsage())
        
        0
    with
    | :? ArguParseException as ex ->
        printfn "%s" ex.Message
        1
    | ex ->
        printfn "Error: %s" ex.Message
        1
