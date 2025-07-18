module CliArguments

open Argu

// Define the CLI arguments
[<CliPrefix(CliPrefix.DoubleDash)>]
type PerformanceArgs =
    | [<AltCommandLine("-stellaris")>] Stellaris
    | [<AltCommandLine("-eu4")>] EU4
    | [<AltCommandLine("-hoi4")>] HOI4
    | [<AltCommandLine("-ck3")>] CK3
    | [<AltCommandLine("-parse-test")>] Parse_Test
    | [<AltCommandLine("-g")>] Game_Path of string
    | [<AltCommandLine("-c")>] Config_Path of string
    | [<AltCommandLine("-p")>] Cache_Path of string
    | [<AltCommandLine("-mod-path")>] Mod_Path of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Stellaris -> "Run Stellaris performance test"
            | EU4 -> "Run EU4 performance test"
            | HOI4 -> "Run HOI4 performance test"
            | CK3 -> "Run CK3 performance test"
            | Parse_Test -> "Run parsing test"
            | Game_Path _ -> "Path to game installation directory"
            | Config_Path _ -> "Path to config directory"
            | Cache_Path _ -> "Path to optional .cwb cache file"
            | Mod_Path _ -> "Path to optional mod directory"
