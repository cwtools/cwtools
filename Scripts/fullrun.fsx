#r "nuget: CWTools, 0.5.0-alpha"

open System.IO
open System.Text
open CWTools
open CWTools.Common
open CWTools.Games
open CWTools.Games.Stellaris
open CWTools.Parser

CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)

let configDir = @"C:\Users\Thomas\Git\cwtools-stellaris-config\config"
let folders =  [ Files.WorkspaceDirectoryInput.WD {path = @"D:\Games\Steam\steamapps\common\Stellaris"; name = "game"} ]

let rec getAllFolders dirs =
    if Seq.isEmpty dirs then Seq.empty else
        seq { yield! dirs |> Seq.collect Directory.EnumerateDirectories
              yield! dirs |> Seq.collect Directory.EnumerateDirectories |> getAllFolders }
let getAllFoldersUnion dirs =
    seq {
        yield! dirs
        yield! getAllFolders dirs
    }
let getConfigFiles(configDir : string) =
    let configFiles = (if Directory.Exists configDir then getAllFoldersUnion ([configDir] |> Seq.ofList) else Seq.empty) |> Seq.collect (Directory.EnumerateFiles)
    let configFiles = configFiles |> List.ofSeq |> List.filter (fun f -> Path.GetExtension f = ".cwt" || Path.GetExtension f = ".log")
    let configs =
        match true, configFiles.Length > 0 with
        |false, _ -> []
        |_, true ->
            configFiles |> List.map (fun f -> f, File.ReadAllText(f))
            //["./config.cwt", File.ReadAllText("./config.cwt")]
        |_, false -> []
    configs
let settings : StellarisSettings = {
        rootDirectories = folders
        modFilter = None
        validation = {
            validateVanilla = false
            experimental = true
            langs = [ STL STLLang.English ]
        }
        rules = Some { ruleFiles = getConfigFiles(configDir); validateRules = true; debugRulesOnly = true; debugMode = false }
        embedded = FromConfig([], [])
        scriptFolders = None
        excludeGlobPatterns = None
        maxFileSize = None
    }
let game = STLGame(settings)
game.Lookup.eventTargetLinks |> Seq.take 10 |> printf "%A"