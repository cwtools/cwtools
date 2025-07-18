module CliArguments

open Argu

// Define the CLI arguments
[<CliPrefix(CliPrefix.DoubleDash)>]
type PerformanceArgs =
    | [<AltCommandLine("-stl-manual")>] Stellaris_Manual
    | [<AltCommandLine("-stl-verbose")>] Stellaris_Verbose
    | [<AltCommandLine("-stl-modcached")>] Stellaris_ModCached
    | [<AltCommandLine("-eu4-vanilla")>] EU4_Vanilla
    | [<AltCommandLine("-eu4-custom")>] EU4_Custom
    | [<AltCommandLine("-hoi4-vanilla")>] HOI4_Vanilla
    | [<AltCommandLine("-hoi4-modcached")>] HOI4_ModCached
    | [<AltCommandLine("-ck3-vanilla")>] CK3_Vanilla
    | [<AltCommandLine("-parse-test")>] Parse_Test
    | [<AltCommandLine("-g")>] Game_Path of string
    | [<AltCommandLine("-c")>] Config_Path of string
    | [<AltCommandLine("-p")>] Cache_Path of string
    | [<AltCommandLine("-f")>] File_Path of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Stellaris_Manual -> "Run Stellaris performance test in manual mode"
            | Stellaris_Verbose -> "Run Stellaris performance test in verbose mode"
            | Stellaris_ModCached -> "Run Stellaris performance test with mod caching"
            | EU4_Vanilla -> "Run EU4 vanilla performance test"
            | EU4_Custom -> "Run EU4 custom performance test"
            | HOI4_Vanilla -> "Run HOI4 vanilla performance test"
            | HOI4_ModCached -> "Run HOI4 performance test with mod caching"
            | CK3_Vanilla -> "Run CK3 vanilla performance test"
            | Parse_Test -> "Run parsing test"
            | Game_Path _ -> "Path to game installation directory"
            | Config_Path _ -> "Path to config directory"
            | Cache_Path _ -> "Path to optional .cwb cache file"
            | File_Path _ -> "Path to file for parsing test"
