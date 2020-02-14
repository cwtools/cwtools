namespace CWToolsCLI
open System.Text
open CWTools.Common
open System.IO
open CWTools.Parser
open CWTools
open FParsec
open System.Diagnostics.Tracing
open System.Reflection
open CWTools.Localisation
open CWTools.Localisation.STL
open CWTools.Games.Files
open CWTools.Common.STLConstants
open CWTools.Games
open CWTools.Validation.Stellaris
open MBrace.FsPickler
open CWTools.Process
open CWTools.Utilities.Position
open System
open FSharpPlus.Control
open Pastel
open System.Drawing
open Reporters.Reporters
open Reporters

module CWToolsCLI =
    open Argu
    open Validator

    type Exiter() =
        interface IExiter with
            member __.Name = "paket exiter"
            member __.Exit (msg,code) =
                if code = ErrorCode.HelpText then
                    printfn "%s" msg ; exit 0
                else eprintfn "%s" msg ; exit 1
// type CustomLang = |English = 0 |French = 1 |German = 2 |Spanish = 3 |Russian = 4 |Polish = 5 |Braz_Por = 6 |Chinese = 7 |Default = 8

    type LanguageArg =
        | English
        | French
        | German
        | Spanish
        | Russian
        | Polish
        | BrazPor
        | SimpChinese
    let parseLanguageArg (game : Game) (lang : LanguageArg) =
        match game, lang with
        | Game.Custom, English -> Some (Custom CustomLang.English)
        | Game.Custom, French -> Some (Custom CustomLang.French)
        | Game.Custom, German -> Some (Custom CustomLang.German)
        | Game.Custom, Spanish -> Some (Custom CustomLang.Spanish)
        | Game.Custom, Russian -> Some (Custom CustomLang.Russian)
        | Game.Custom, Polish -> Some (Custom CustomLang.Polish)
        | Game.Custom, BrazPor -> Some (Custom CustomLang.Braz_Por)
        | Game.Custom, SimpChinese -> Some (Custom CustomLang.Chinese)
        | Game.STL, English -> Some (STL STLLang.English)
        | Game.STL, French -> Some (STL STLLang.French)
        | Game.STL, German -> Some (STL STLLang.German)
        | Game.STL, Spanish -> Some (STL STLLang.Spanish)
        | Game.STL, Russian -> Some (STL STLLang.Russian)
        | Game.STL, Polish -> Some (STL STLLang.Polish)
        | Game.STL, BrazPor -> Some (STL STLLang.Braz_Por)
        | Game.STL, SimpChinese -> Some (STL STLLang.Chinese)
        | Game.HOI4, English -> Some (HOI4 HOI4Lang.English)
        | Game.HOI4, French -> Some (HOI4 HOI4Lang.French)
        | Game.HOI4, German -> Some (HOI4 HOI4Lang.German)
        | Game.HOI4, Spanish -> Some (HOI4 HOI4Lang.Spanish)
        | Game.HOI4, Russian -> Some (HOI4 HOI4Lang.Russian)
        | Game.HOI4, Polish -> Some (HOI4 HOI4Lang.Polish)
        | Game.HOI4, BrazPor -> Some (HOI4 HOI4Lang.Braz_Por)
        | Game.HOI4, _ -> None
        | Game.EU4, English -> Some (EU4 EU4Lang.English)
        | Game.EU4, French -> Some (EU4 EU4Lang.French)
        | Game.EU4, German -> Some (EU4 EU4Lang.German)
        | Game.EU4, Spanish -> Some (EU4 EU4Lang.Spanish)
        | Game.EU4, _ -> None
        | Game.VIC2, English -> Some (VIC2 VIC2Lang.English)
        | Game.VIC2, French -> Some (VIC2 VIC2Lang.French)
        | Game.VIC2, German -> Some (VIC2 VIC2Lang.German)
        | Game.VIC2, Spanish -> Some (VIC2 VIC2Lang.Spanish)
        | Game.VIC2, _ -> None
        | Game.CK2, English -> Some (CK2 CK2Lang.English)
        | Game.CK2, French -> Some (CK2 CK2Lang.French)
        | Game.CK2, German -> Some (CK2 CK2Lang.German)
        | Game.CK2, Spanish -> Some (CK2 CK2Lang.Spanish)
        | Game.CK2, Russian -> Some (CK2 CK2Lang.Russian)
        | Game.CK2, _ -> None
        | Game.IR, English -> Some (IR IRLang.English)
        | Game.IR, French -> Some (IR IRLang.French)
        | Game.IR, German -> Some (IR IRLang.German)
        | Game.IR, Spanish -> Some (IR IRLang.Spanish)
        | Game.IR, SimpChinese -> Some (IR IRLang.Chinese)
        | Game.IR, Russian -> Some (IR IRLang.Russian)
        | Game.IR, _ -> None
        | _ -> None

    type ListTypes =
        | Folders = 1
        | Files = 2
        | Triggers = 3
        | Effects = 4
        | Localisation = 5
        | Technology = 6
        | Types = 7
    type ListSort =
        | Path = 1
    type ListArgs =
        | [<MainCommand; ExactlyOnce; Last>] ListType of ListTypes
        | Sort of ListSort option
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | ListType _ -> "Thing to list"
                | Sort _ -> "Sort by"
    type ValidateType =
        | ParseErrors = 1
        | Errors = 2
        | Warnings = 3
        | Info = 4
        | All = 5
        | Localisation = 6
    type ValidateArgs =
        | [<MainCommand; ExactlyOnce; Last>] ValType of ValidateType
        | ReportType of Reporter
        | OutputFile of filename:string
        | [<EqualsAssignment>] OutputHashes of filename:string option
        | [<EqualsAssignment>] IgnoreHashesFile of filename:string option
        | Languages of LanguageArg list
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | ValType _ -> "Which errors to output"
                | OutputFile _ -> "Output report to a file with this filename"
                | ReportType _ -> "Report type, CLI by default"
                | OutputHashes _ -> "Whether to output the error hash caching file, optionally specifying the file name"
                | IgnoreHashesFile _ -> "A file containing an error hash cache file, which will be ignored, optionall specifying the file name"
                | Languages _ -> "A list of languages to validate (default all)"
    type ParseArgs =
        | [<MainCommand; ExactlyOnce; Last>] File of string
        | ParseOutputFile of filename:string

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                |File _ -> "file to parse"
                |ParseOutputFile _ -> "file to output to"
    type CacheTypes =
        | Full = 1
        | Metadata = 2
    type SerializeArgs =
        | [<MainCommand; ExactlyOnce; Last>] OutputCacheType of CacheTypes
        | OutputCacheFile of filename : string
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | OutputCacheType _ -> "Which cache file to generate"
                | OutputCacheFile _ -> "Filename to save the cache file to"
    type Arguments =
        | [<Inherit>]Directory of path : string
        | [<Inherit>]Game of Game
        | [<Inherit>]Scope of FilesScope
        | [<Inherit>]ModFilter of string
        | [<Inherit>]CacheFile of path : string
        | [<Inherit>]CacheType of cacheType : CacheTypes
        | [<Inherit>]RulesPath of path : string
        | DocsPath of string
        | [<CustomCommandLine("validate")>] Validate of ParseResults<ValidateArgs>
        | [<CustomCommandLine("list")>] List of ParseResults<ListArgs>
        | [<CustomCommandLine("parse")>] Parse of ParseResults<ParseArgs>
        | [<CliPrefix(CliPrefix.None)>] Serialize of ParseResults<SerializeArgs>

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Directory _ -> "specify the main game directory (default: current directory)"
                | Game _ -> "specify the game"
                | Validate _ -> "Validate all mod files (default: mods)"
                | List _ -> "List things"
                | Scope _ -> "which files to include"
                | ModFilter _ -> "filter to mods with this in name"
                | DocsPath _ -> "path to a custom trigger_docs game.log file"
                | Parse _ -> "parse a file"
                | Serialize _ -> "created serialized files for embedding"
                | CacheFile _ -> "path to the cache file"
                | RulesPath _ -> "path to the cwt rules"
                | CacheType _ -> "type of cache file"


    let getEffectsAndTriggers docsPath =
        let docsParsed =
            match docsPath with
            | Some path -> DocsParser.parseDocsFile path
            | None ->
                DocsParser.parseDocsStream (Assembly.GetEntryAssembly().GetManifestResourceStream("CWToolsCLI.game_effects_triggers_1.9.1.txt"))
        match docsParsed with
        |Success(p, _, _) -> p |> (DocsParser.processDocs scopeManager.ParseScopes)
        |Failure(msg,_,_) -> failwith ("docs parsing failed with " + msg)
    let merge (a : Map<'a, 'b>) (b : Map<'a, 'b>) (f : 'a -> 'b * 'b -> 'b) =
        Map.fold (fun s k v ->
            match Map.tryFind k s with
            | Some v' -> Map.add k (f k (v, v')) s
            | None -> Map.add k v s) a b
    let rec getAllFolders dirs =
        if Seq.isEmpty dirs then Seq.empty else
            seq { yield! dirs |> Seq.collect Directory.EnumerateDirectories
                  yield! dirs |> Seq.collect Directory.EnumerateDirectories |> getAllFolders }
    let getAllFoldersUnion dirs =
        seq {
            yield! dirs
            yield! getAllFolders dirs
        }

    let getConfigFiles(gameDir : string option, rulesDir : string option) =
        let configpath = "Main.files.config.cwt"
        let configDir =
            match gameDir, rulesDir with
            | _, Some rulesDir -> rulesDir
            |Some dir, _ -> Path.Combine(dir, ".cwtools")
            |None, _ -> "./.cwtools"
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
    let list game directory scope modFilter docsPath rulesPath (results : ParseResults<ListArgs>) =
        //let triggers, effects = getEffectsAndTriggers docsPath

        let gameObj = ErrorGame(directory, scope, modFilter, getConfigFiles(Some directory, rulesPath), game, FromConfig([],[]), None)
        //let gameObj = STL(directory, scope, modFilter, triggers, effects, getConfigFiles(Some directory, None))
        let sortOrder = results.GetResult <@ Sort @>
        match results.GetResult <@ ListType @> with
        | ListTypes.Folders -> printfn "%A" gameObj.folders
        | ListTypes.Files ->
            match sortOrder with
            | None
            | Some ListSort.Path ->
                let files = gameObj.allFileList |> List.map (fun s -> s.file.Replace(directory, ""))
                File.WriteAllLines("cwtools-files.csv", files)
                //gameObj.allFileList |> List.iter (fun f -> printfn "%O" f)
            | _ -> failwith "Unexpected sort order"
        | ListTypes.Triggers ->
            // let triggers = DocsParser.parseDocs "C:\Users\Jennifer\Documents\Thomas\CK2Events\CK2EventsTests\game_triggers (1).txt"
            // let t = triggers |>  (function |Success(p, _, _) -> p |_ -> [])
            let t = gameObj.scriptedTriggerList
            printfn "%A" t
        | ListTypes.Effects ->
            let t = gameObj.scriptedEffectList
            printfn "%A" t
        | ListTypes.Localisation -> ()
            //printfn "%A" loc.GetKeys
        // | ListTypes.Technology ->
        //     (gameObj.references().Technologies) |> List.map fst |> List.iter (printfn "%A")
        // | ListTypes.Types ->
        //     gameObj.recompute()
        //     let referencedTypes = gameObj.entities() |> List.choose (fun struct(e,l) -> l.Force().Referencedtypes)
        //     // printfn "%A" referencedTypes
        //     let combinedReferences = referencedTypes |> List.fold (fun s m -> merge s m (fun _ (a,b) -> a @ b)) Map.empty
        //                                 |> Map.map (fun _ vs -> vs |> List.map (fun v -> v.name))
        //     let types = gameObj.references().TypeMapInfo |> Map.map (fun _ vs -> vs |> List.map (fun t -> t.id))
        //     let events = types |> Map.tryFind "scripted_trigger" |> Option.defaultValue []
        //     let eventReferences = combinedReferences |> Map.tryFind "scripted_trigger" |> Option.defaultValue []
        //     // eventReferences |> List.iter (printfn "%s")
        //     let unused = List.except eventReferences events
        //     unused |> List.iter (printfn "%s")
        //     let files = events |> List.map (sprintf "%A")
        //     File.WriteAllLines("file1.csv", files)
        //     let files = eventReferences |> List.map (sprintf "%A")
        //     File.WriteAllLines("file2.csv", files)


        | _ -> failwith "Unexpected list type"

    let validate game directory scope modFilter docsPath cachePath rulesPath cacheType (results : ParseResults<ValidateArgs>) =
        let reporter = results.GetResult (ReportType, CLI)
        let outputHashes = results.TryGetResult <@ OutputHashes @>
        let inputHashFile = results.TryGetResult <@ IgnoreHashesFile @>
        let langs = results.TryGetResult <@ Languages @> |> Option.map (List.choose (parseLanguageArg game))
        let langs =
            match langs, game with
            | Some l, _ -> Some l
            | None, Game.STL -> Some [STL STLLang.English]
            | None, Game.EU4 -> Some [EU4 EU4Lang.English]
            | None, Game.HOI4 -> Some [HOI4 HOI4Lang.English]
            | None, Game.CK2 -> Some [CK2 CK2Lang.English]
            | None, Game.VIC2 -> Some [VIC2 VIC2Lang.English]
            | None, Game.IR -> Some [IR IRLang.English]
            | None, Game.Custom -> Some [Custom CustomLang.English]
            | _ -> None
        let embedded =
            match cacheType with
            | CacheTypes.Full ->
                let cached, cachedFiles =
                    match cachePath with
                    |Some path ->
                        Serializer.deserialize path
                    |None -> [], []
                FromConfig(cachedFiles, cached)
            | CacheTypes.Metadata ->
                match cachePath with
                | Some path ->
                    Metadata (Serializer.deserializeMetadata path)
                | None -> FromConfig([], [])
        let hashes =
            match inputHashFile with
            | None -> Set.empty
            | Some (Some filepath) -> File.ReadAllLines filepath |> Set.ofArray
            | Some (None) -> File.ReadAllLines "error-cache.txt" |> Set.ofArray

        //printfn "%A" cachedFiles
        let valType = results.GetResult <@ ValType @>
        let gameObj = ErrorGame(directory, scope, modFilter, getConfigFiles(Some directory, rulesPath),game, embedded, langs)
        let errors =
            match valType with
            | ValidateType.ParseErrors -> (gameObj.parserErrorList) |> List.map ValidationViewModelRow.Parse
            | ValidateType.Errors -> (gameObj.validationErrorList()) |> List.filter (fun e -> e.severity = Severity.Error) |> List.map Error
            | ValidateType.Warnings ->(gameObj.validationErrorList()) |> List.filter (fun e -> e.severity = Severity.Warning) |> List.map Error
            | ValidateType.Info ->(gameObj.validationErrorList()) |> List.filter (fun e -> e.severity = Severity.Information) |> List.map Error
            | ValidateType.Localisation ->
                gameObj.localisationErrorList() |> List.map Error
                //|> List.iter (fun l -> printfn "%O" l)
                //printfn "%A" (gameObj.localisationErrorList)
            | ValidateType.All ->
                ((gameObj.parserErrorList) |> List.map ValidationViewModelRow.Parse)
                @
                ((gameObj.validationErrorList()) |> List.map ValidationViewModelRow.Error)
                @
                (gameObj.localisationErrorList() |> List.map Error)
                // printfn "%A" gameObj.parserErrorList;
                // pprintfn "%A" (gameObj.validationErrorList());
                //printfn "%A" (gameObj.parserErrorList.Length + (gameObj.validationErrorList().Length))
            | _ -> failwith "Unexpected validation type"
        let supressedErrors, newErrors =
            errors |> List.partition (function | Error e -> Set.contains e.hash hashes | ValidationViewModelRow.Parse e -> Set.contains e.hash hashes)

        let outputFile = results.TryGetResult <@ OutputFile @>
        match reporter with
        | CSV ->
            csvReporter outputFile newErrors
        | CLI ->
            cliReporter outputFile newErrors
        | JSON ->
            jsonReporter directory outputFile newErrors supressedErrors

        match outputHashes with
        | Some param ->
            let hashes = errors |> List.map (function |Error r -> r.hash |ValidationViewModelRow.Parse r -> r.hash)
            let hashString = String.concat (Environment.NewLine) hashes
            match param with
            | Some filepath -> File.WriteAllText(filepath, hashString)
            | None -> File.WriteAllText("error-cache.txt", hashString)
        | None -> ()
        match newErrors.Length with
        | 0 -> 0
        | x -> 1


    let parse (results : ParseResults<ParseArgs>) =
        let file = results.GetResult <@ File @>
        let outputFile = results.TryGetResult <@ ParseOutputFile @>
        let success, filetext = newParse file
        match outputFile with
        |Some file ->
            File.WriteAllText(file, filetext); 0
        |None -> printf "%s" (filetext); 1


    let serialize game directory scope modFilter cachePath rulesPath  (results : ParseResults<_>) =
        let cacheType = results.TryGetResult <@ OutputCacheType @> |> Option.defaultValue CacheTypes.Full
        let outputCacheFileName = results.TryGetResult <@ OutputCacheFile @>
        match cacheType with
        | CacheTypes.Metadata ->
            Serializer.serializeMetadata (directory, scope, modFilter, getConfigFiles(Some directory, rulesPath),game, outputCacheFileName)
        | CacheTypes.Full ->
            let filename =
                match game with
                |Game.STL ->
                    Serializer.serializeSTL ([WD {path = directory; name = "undefined"}]) outputCacheFileName
                |Game.HOI4 ->
                    Serializer.serializeHOI4 ({path = directory; name = "undefined"}) outputCacheFileName
                |Game.IR ->
                    Serializer.serializeIR ([WD {path = directory; name = "undefined"}]) outputCacheFileName
                |Game.CK2 ->
                    Serializer.serializeCK2 ([WD {path = directory; name = "undefined"}]) outputCacheFileName
                |Game.EU4 ->
                    Serializer.serializeEU4 ({path = directory; name = "undefined"}) outputCacheFileName
                |Game.VIC2 ->
                    Serializer.serializeVIC2 ([WD {path = directory; name = "undefined"}]) outputCacheFileName
                |Game.Custom -> failwith "This CLI doesn't support serializing for custom games yet"
            eprintfn "Full cache file created at %s, relative to CWD" filename
        // let fileManager = FileManager(directory, Some modFilter, scope, scriptFolders, "stellaris", Encoding.UTF8)
        // let files = fileManager.AllFilesByPath()
        // let resources = ResourceManager(STLCompute.computeSTLData (fun () -> None)).Api
        // let entities = resources.UpdateFiles(files) |> List.map (fun (r, (struct (e, _))) -> r, e)
        // let files = resources.GetResources()
        //             |> List.choose (function |FileResource (_, r) -> Some (r.logicalpath, "")
        //                                         |FileWithContentResource (_, r) -> Some (r.logicalpath, r.filetext)
        //                                         |_ -> None)
        // let data = { resources = entities; fileIndexTable = fileIndexTable; files = files}
        // let pickle = xmlSerializer.Pickle data
        // File.WriteAllBytes("pickled.xml", pickle)


    [<EntryPoint>]
    let main argv =
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
        CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
        let parser = ArgumentParser.Create<Arguments>(programName = "CWToolsCLI.exe", errorHandler = new Exiter(),
                                                    checkStructure = false)
        let results = parser.ParseCommandLine argv
        // let results = parser.ParseCommandLine argv
        let directory = results.TryGetResult <@ Directory @> |> Option.defaultValue System.Environment.CurrentDirectory
        let game = results.GetResult <@ Game @>
        let scope = results.TryGetResult <@ Scope @> |> Option.defaultValue FilesScope.Mods
        let modFilter = results.GetResult(<@ ModFilter @>, defaultValue = "")
        let docsPath = results.TryGetResult <@ DocsPath @>
        let cachePath = results.TryGetResult <@ CacheFile @>
        let rulesPath = results.TryGetResult <@ RulesPath @>
        let cacheType = results.TryGetResult <@ CacheType @> |> Option.defaultValue CacheTypes.Full
        match results.GetSubCommand() with
        | List r -> list game directory scope modFilter docsPath rulesPath r; 0
        | Validate r -> validate game directory scope modFilter docsPath cachePath rulesPath cacheType r
        | Serialize r -> serialize game directory scope modFilter cachePath rulesPath r ;0
        | Directory _
        | Game _ -> failwith "internal error: this code should never be reached"; 1
        | Parse r -> parse r

        //printfn "%A" argv
        // return an integer exit code
