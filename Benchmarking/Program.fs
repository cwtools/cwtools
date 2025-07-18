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
    | [<AltCommandLine("-g")>] Game_Path of string
    | [<AltCommandLine("-c")>] Config_Path of string
    | [<AltCommandLine("-f")>] File_Path of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Stellaris -> "Run Stellaris performance test"
            | EU4 -> "Run EU4 performance test"
            | HOI4 -> "Run HOI4 performance test"
            | CK3 -> "Run CK3 performance test"
            | Test -> "Run parsing test"
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
        
        let benchmarkResults = results.GetAllResults() |> List.filter (function
            | Game_Path _ | Config_Path _ | File_Path _ -> false
            | _ -> true)
        
        match benchmarkResults with
        | [ Stellaris ] -> Benchmarks.runStellarisTest gamePath configPath
        | [ EU4 ] -> Benchmarks.runEU4Test gamePath configPath
        | [ HOI4 ] -> Benchmarks.runHOI4Test gamePath configPath
        | [ CK3 ] -> Benchmarks.runCK3Test gamePath configPath
        | [ Test ] -> Benchmarks.runParseTest filePath
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
