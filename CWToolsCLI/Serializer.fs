module CWToolsCLI.Serializer

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
open CWTools.Utilities
open CWTools.Games.EU4
open CWTools.Validation.EU4
open CWTools.Rules
open CWTools.Validation.HOI4
open CWTools.Games.Stellaris.STLLookup
open CWTools.Games.Compute
open System
open CWTools.Games.HOI4
open CWTools.Games.Stellaris
open CWTools.Games.CK2
open CWTools.Games.CK3
open CWTools.Games.VIC2
open CWTools.Games.Custom
open CWTools.Games.IR
open System.IO.Compression


let mkPickler (resolver : IPicklerResolver) =
    let arrayPickler = resolver.Resolve<Leaf array> ()
    let writer (w : WriteState) (ns : Lazy<Leaf array>) =
        arrayPickler.Write w "value" (ns.Force())
    let reader (r : ReadState) =
        let v = arrayPickler.Read r "value" in Lazy<Leaf array>.CreateFromValue v
    Pickler.FromPrimitives(reader, writer)
let registry = new CustomPicklerRegistry()
do registry.RegisterFactory mkPickler
registry.DeclareSerializable<FParsec.Position>()
let picklerCache = PicklerCache.FromCustomPicklerRegistry registry
let binarySerializer = FsPickler.CreateBinarySerializer(picklerResolver = picklerCache)
let assemblyLocation = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)


// let compressAndWrite (input : byte array) (file : string) =
//     use stream = new MemoryStream(input);
//     let encoder = SevenZip.Compression.LZMA.Encoder()
//     use fileStream = File.Create(file)
//     encoder.WriteCoderProperties(fileStream)
//     for i = 0 to 7 do
//         fileStream.WriteByte((input.Length >>> (8 * i)) |> byte)

//     // fileStream.Write(BitConverter.GetBytes(input.Length), 0, 8)
//     encoder.Code(stream, fileStream, stream.Length, -1L, NullProgress())
//     fileStream.Flush()
//     fileStream.Close()
//     use encoder = ManagedLzma.SevenZip.Writer.ArchiveWriter.Create(fileStream, false)
//     let settings = ManagedLzma.SevenZip.Writer.Lzma2EncoderSettings(ManagedLzma.LZMA2.EncoderSettings())
//     Encode
//     encoder.BeginEncoding()
//     let writer = new ArchiveWriter(stream);
//     write
//     // use encoder = new ManagedLzma.LZMA.AsyncEncoder(ManagedLzma.LZMA.EncoderSettings())
//     // // ManagedLzma.
//     // encoder.EncodeAsync(stream, fileStream)

let compressAndWrite (input : byte array) (file : string) =
    use stream = new MemoryStream(input);
    use fileStream = File.Create(file)
    ICSharpCode.SharpZipLib.BZip2.BZip2.Compress(stream, fileStream, true, 9)

let decompress (path : string) =
    use outStream  = new MemoryStream()
    let bytes = File.ReadAllBytes(path)
    use inStream = new MemoryStream(bytes)
    ICSharpCode.SharpZipLib.BZip2.BZip2.Decompress(inStream, outStream, false)
    outStream.ToArray()

let addDLCs (workspaceDirectory: WorkspaceDirectory) =
    let dir = workspaceDirectory.path
    // eprintfn "ad %A" dir
    // eprintfn "ad2 %A" (Path.Combine [|dir; "dlc"|])
    if Directory.Exists (dir) && Directory.Exists (Path.Combine [|dir; "dlc"|])
    then
        let dlcs = Directory.EnumerateDirectories (Path.Combine [|dir; "dlc"|])
        // eprintfn "ad3 %A" dlcs
        let createZippedDirectory (dlcDir : string) =
            // eprintfn "d1 %A" (Directory.EnumerateFiles dlcDir)
            match Directory.EnumerateFiles dlcDir |> Seq.tryFind (fun f -> (Path.GetExtension f) = ".zip") with
            | Some zip ->
                // eprintfn "d2 %A" zip
                use file = File.OpenRead(zip)
                try
                    use zipFile = new ZipArchive(file, ZipArchiveMode.Read)
                    let files = zipFile.Entries |> Seq.map (fun e -> Path.Combine([|"uri:"; zip; e.FullName.Replace("\\","/")|]), use sr = new StreamReader(e.Open()) in sr.ReadToEnd())
                                |> List.ofSeq
                    Some (ZD { ZippedDirectory.name = Path.GetFileName zip; path = zip.Replace("\\","/"); files = files})
                with
                | _ -> None
            | None -> None
        dlcs |> Seq.choose createZippedDirectory |> List.ofSeq
    else
        []

let serialize gameDirName scriptFolders cacheDirectory = ()
let serializeSTL folder outputFileName =
    let fileManager = FileManager(folder, Some "", STLConstants.scriptFolders, "stellaris", Encoding.UTF8, [], 2)
    let files = fileManager.AllFilesByPath()
    let computefun : unit -> InfoService option = (fun () -> (None))
    let resources = ResourceManager<STLComputedData>(STL.computeSTLData computefun, STL.computeSTLDataUpdate computefun, Encoding.UTF8, Encoding.GetEncoding(1252)).Api
    let entities =
        resources.UpdateFiles(files)
         |> List.choose (fun (r, e) -> e |> function |Some e2 -> Some (r, e2) |_ -> None)
         |> List.map (fun (r, (struct (e, _))) -> r, e)
    let files = resources.GetResources()
                |> List.choose (function |FileResource (_, r) -> Some (r.logicalpath, "")
                                         |FileWithContentResource (_, r) -> Some (r.logicalpath, r.filetext)
                                         |_ -> None)
    let data = { resources = entities; fileIndexTable = fileIndexTable; files = files; stringResourceManager = StringResource.stringManager}
    let pickle = binarySerializer.Pickle data
    let filename = outputFileName |> Option.defaultValue "stl.cwb.bz2"
    compressAndWrite pickle (filename)
    filename
    //File.Create()
    //File.WriteAllBytes(Path.Combine(cacheDirectory, "stl.cwb"), pickle)

let serializeEU4 folder outputFileName =
    let folders = (WD folder ) :: (addDLCs folder)
    let fileManager = FileManager(folders, Some "", EU4Constants.scriptFolders, "stellaris", Encoding.UTF8, [], 2)
    let files = fileManager.AllFilesByPath()
    let computefun : unit -> InfoService option = (fun () -> (None))
    let resources = ResourceManager<EU4ComputedData>(EU4.computeEU4Data computefun, EU4.computeEU4DataUpdate computefun, Encoding.GetEncoding(1252), Encoding.UTF8).Api
    let entities =
        resources.UpdateFiles(files)
        |> List.choose (fun (r, e) -> e |> function |Some e2 -> Some (r, e2) |_ -> None)
        |> List.map (fun (r, (struct (e, _))) -> r, e)
    let files = resources.GetResources()
                |> List.choose (function |FileResource (_, r) -> Some (r.logicalpath, "")
                                         |FileWithContentResource (_, r) -> Some (r.logicalpath, r.filetext)
                                         |_ -> None)
    let data = { resources = entities; fileIndexTable = fileIndexTable; files = files; stringResourceManager = StringResource.stringManager}
    let pickle = binarySerializer.Pickle data
    let filename = outputFileName |> Option.defaultValue "eu4.cwb.bz2"
    compressAndWrite pickle (filename)
    filename

let serializeHOI4 folder outputFileName =
    let folders = (WD folder ) :: (addDLCs folder)
    let fileManager = FileManager(folders, Some "", HOI4Constants.scriptFolders, "hearts of iron iv", Encoding.UTF8, [], 2)
    let files = fileManager.AllFilesByPath()
    let computefun : unit -> InfoService option = (fun () -> (None))
    let resources = ResourceManager<HOI4ComputedData>(computeHOI4Data computefun, computeHOI4DataUpdate computefun, Encoding.UTF8, Encoding.GetEncoding(1252)).Api
    let entities =
        resources.UpdateFiles(files)
        |> List.choose (fun (r, e) -> e |> function |Some e2 -> Some (r, e2) |_ -> None)
        |> List.map (fun (r, (struct (e, _))) -> r, e)
    let files = resources.GetResources()
                |> List.choose (function |FileResource (_, r) -> Some (r.logicalpath, "")
                                         |FileWithContentResource (_, r) -> Some (r.logicalpath, r.filetext)
                                         |_ -> None)
    let data = { resources = entities; fileIndexTable = fileIndexTable; files = files; stringResourceManager = StringResource.stringManager}
    let pickle = binarySerializer.Pickle data
    let filename = outputFileName |> Option.defaultValue "hoi4.cwb.bz2"
    compressAndWrite pickle (filename)
    filename

let serializeCK2 folder outputFileName =
    let fileManager = FileManager(folder, Some "", CK2Constants.scriptFolders, "crusader kings ii", Encoding.UTF8, [], 2)
    let files = fileManager.AllFilesByPath()
    let computefun : unit -> InfoService option = (fun () -> (None))
    let resources = ResourceManager<CK2ComputedData>(computeCK2Data computefun, computeCK2DataUpdate computefun, Encoding.UTF8, Encoding.GetEncoding(1252)).Api
    let entities =
        resources.UpdateFiles(files)
        |> List.choose (fun (r, e) -> e |> function |Some e2 -> Some (r, e2) |_ -> None)
        |> List.map (fun (r, (struct (e, _))) -> r, e)
    let files = resources.GetResources()
                |> List.choose (function |FileResource (_, r) -> Some (r.logicalpath, "")
                                         |FileWithContentResource (_, r) -> Some (r.logicalpath, r.filetext)
                                         |_ -> None)
    let data = { resources = entities; fileIndexTable = fileIndexTable; files = files; stringResourceManager = StringResource.stringManager}
    let pickle = binarySerializer.Pickle data
    let filename = outputFileName |> Option.defaultValue "ck2.cwb.bz2"
    compressAndWrite pickle (filename)
    filename

let serializeIR folder outputFileName =
    let fileManager = FileManager(folder, Some "", IRConstants.scriptFolders, "imperator", Encoding.UTF8, [], 2)
    let files = fileManager.AllFilesByPath()
    let computefun : unit -> InfoService option = (fun () -> (None))
    let resources = ResourceManager<IRComputedData>(Compute.Jomini.computeJominiData computefun, Compute.Jomini.computeJominiDataUpdate computefun, Encoding.UTF8, Encoding.GetEncoding(1252)).Api
    let entities =
        resources.UpdateFiles(files)
        |> List.choose (fun (r, e) -> e |> function |Some e2 -> Some (r, e2) |_ -> None)
        |> List.map (fun (r, (struct (e, _))) -> r, e)
    let files = resources.GetResources()
                |> List.choose (function |FileResource (_, r) -> Some (r.logicalpath, "")
                                         |FileWithContentResource (_, r) -> Some (r.logicalpath, r.filetext)
                                         |_ -> None)
    let data = { resources = entities; fileIndexTable = fileIndexTable; files = files; stringResourceManager = StringResource.stringManager}
    let pickle = binarySerializer.Pickle data
    let filename = outputFileName |> Option.defaultValue "ir.cwb.bz2"
    compressAndWrite pickle (filename)
    filename

let serializeCK3 folder outputFileName =
    let fileManager = FileManager(folder, Some "", CK3Constants.scriptFolders, "crusader kings iii", Encoding.UTF8, [], 2)
    let files = fileManager.AllFilesByPath()
    let computefun : unit -> InfoService option = (fun () -> (None))
    let resources = ResourceManager<CK3ComputedData>(Compute.Jomini.computeJominiData computefun, Compute.Jomini.computeJominiDataUpdate computefun, Encoding.UTF8, Encoding.GetEncoding(1252)).Api
    let entities =
        resources.UpdateFiles(files)
        |> List.choose (fun (r, e) -> e |> function |Some e2 -> Some (r, e2) |_ -> None)
        |> List.map (fun (r, (struct (e, _))) -> r, e)
    let files = resources.GetResources()
                |> List.choose (function |FileResource (_, r) -> Some (r.logicalpath, "")
                                         |FileWithContentResource (_, r) -> Some (r.logicalpath, r.filetext)
                                         |_ -> None)
    let data = { resources = entities; fileIndexTable = fileIndexTable; files = files; stringResourceManager = StringResource.stringManager}
    let pickle = binarySerializer.Pickle data
    let filename = outputFileName |> Option.defaultValue "ck3.cwb.bz2"
    compressAndWrite pickle (filename)
    filename

let serializeVIC2 folder outputFileName =
    let fileManager = FileManager(folder, Some "", VIC2Constants.scriptFolders, "victoria 2", Encoding.UTF8, [], 2)
    let files = fileManager.AllFilesByPath()
    let computefun : unit -> InfoService option = (fun () -> (None))
    let resources = ResourceManager<VIC2ComputedData>(computeVIC2Data computefun, computeVIC2DataUpdate computefun, Encoding.UTF8, Encoding.GetEncoding(1252)).Api
    let entities =
        resources.UpdateFiles(files)
        |> List.choose (fun (r, e) -> e |> function |Some e2 -> Some (r, e2) |_ -> None)
        |> List.map (fun (r, (struct (e, _))) -> r, e)
    let files = resources.GetResources()
                |> List.choose (function |FileResource (_, r) -> Some (r.logicalpath, "")
                                         |FileWithContentResource (_, r) -> Some (r.logicalpath, r.filetext)
                                         |_ -> None)
    let data = { resources = entities; fileIndexTable = fileIndexTable; files = files; stringResourceManager = StringResource.stringManager}
    let pickle = binarySerializer.Pickle data
    let filename = outputFileName |> Option.defaultValue "vic2.cwb.bz2"
    compressAndWrite pickle (filename)
    filename

let deserialize path =
    // registry.DeclareSerializable<System.LazyHelper>()
    // registry.DeclareSerializable<Lazy>()
    match File.Exists path, Path.GetExtension path with
    |true, ".bz2" ->
        let cacheFile = decompress path
        // let cacheFile = File.ReadAllBytes(path)
        // let cacheFile = Assembly.GetEntryAssembly().GetManifestResourceStream("Main.files.pickled.cwb")
        //                 |> (fun f -> use ms = new MemoryStream() in f.CopyTo(ms); ms.ToArray())
        let cached = binarySerializer.UnPickle<CachedResourceData> cacheFile
        fileIndexTable <- cached.fileIndexTable
        StringResource.stringManager <- cached.stringResourceManager
        cached.resources, cached.files
    |true, _ ->
        let cacheFile = File.ReadAllBytes(path)
        // let cacheFile = Assembly.GetEntryAssembly().GetManifestResourceStream("Main.files.pickled.cwb")
        //                 |> (fun f -> use ms = new MemoryStream() in f.CopyTo(ms); ms.ToArray())
        let cached = binarySerializer.UnPickle<CachedResourceData> cacheFile
        fileIndexTable <- cached.fileIndexTable
        StringResource.stringManager <- cached.stringResourceManager
        cached.resources, cached.files
    |false, _ -> failwithf "Cache file %s was specified but does not exist" path

let deserializeMetadata path =
    let metadata =
        match File.Exists path, Path.GetExtension path with
        | true, ".bz2" ->
            let cacheFile = decompress path
            binarySerializer.UnPickle<CachedRuleMetadata> cacheFile
        | true, _ ->
            let cacheFile = File.ReadAllBytes(path)
            binarySerializer.UnPickle<CachedRuleMetadata> cacheFile
        | false, _ -> failwithf "Cache file %s was specified but does not exist" path
    {
        metadata with
            varDefs = metadata.varDefs |> Map.map (fun k v -> v |> List.map (fun (s, _) -> (s, range.Zero)))
            typeDefs = metadata.typeDefs |> Map.map (fun k v -> v |> List.map (fun t -> {t with range = range.Zero; validate = false}))
    }


let loadGame<'T when 'T :> ComputedData> (dir : string, scope : FilesScope, modFilter : string, config, game : Game, embedded : EmbeddedSetupSettings, langSetting) =
    // let langs = [Lang.STL STLLang.English; Lang.STL STLLang.German; Lang.STL STLLang.French; Lang.STL STLLang.Spanish;]
    let langs =
        match langSetting, game with
        | Some ls, _ -> ls
        | _, Game.STL -> LangHelpers.allSTLLangs
        | _, Game.EU4 -> LangHelpers.allEU4Langs
        | _, Game.HOI4 -> LangHelpers.allHOI4Langs
        | _, Game.IR -> LangHelpers.allIRLangs
        | _, Game.VIC2 -> LangHelpers.allVIC2Langs
        | _, Game.CK2 -> LangHelpers.allCK2Langs
        | _, Game.CK3 -> LangHelpers.allCK3Langs
        | _, Game.Custom -> LangHelpers.allCustomLangs
        | _ -> failwith "No languages specified"
    eprintfn "%A" langs
    let folders = WD {path = dir; name = "game"} :: addDLCs {path = dir; name = "game"}
    // let langs = [Lang.HOI4 HOI4Lang.English; Lang.HOI4 HOI4Lang.German; Lang.HOI4 HOI4Lang.French; Lang.HOI4 HOI4Lang.Spanish;]
    let STLoptions : StellarisSettings = {
        rootDirectories = folders
        modFilter = Some modFilter
        validation = {
            validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
            experimental = true
            langs = langs
        }
        rules = Some { ruleFiles = config; validateRules = true; debugRulesOnly = true; debugMode = false }
        embedded = embedded
        scriptFolders = None
        excludeGlobPatterns = None
        maxFileSize = None
    }
    let HOI4options : HOI4Settings = {
        rootDirectories = folders
        modFilter = Some modFilter
        validation = {
            validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
            experimental = true
            langs = langs
        }
        rules = Some { ruleFiles = config; validateRules = true; debugRulesOnly = true; debugMode = false }
        embedded = embedded
        scriptFolders = None
        excludeGlobPatterns = None
        maxFileSize = Some 8

    }
    let EU4options : EU4Settings = {
        rootDirectories = folders
        modFilter = Some modFilter
        validation = {
            validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
            experimental = true
            langs = langs
        }
        rules = Some { ruleFiles = config; validateRules = true; debugRulesOnly = true; debugMode = false }
        embedded = embedded
        scriptFolders = None
        excludeGlobPatterns = None
        maxFileSize = Some 8
    }
    let CK2options : CK2Settings = {
        rootDirectories = folders
        modFilter = Some modFilter
        validation = {
            validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
            experimental = true
            langs = langs
        }
        rules = Some { ruleFiles = config; validateRules = true; debugRulesOnly = true; debugMode = false }
        embedded = embedded
        scriptFolders = None
        excludeGlobPatterns = None
        maxFileSize = Some 8
    }
    let VIC2options : VIC2Settings = {
        rootDirectories = folders
        modFilter = Some modFilter
        validation = {
            validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
            experimental = true
            langs = langs
        }
        rules = Some { ruleFiles = config; validateRules = true; debugRulesOnly = true; debugMode = false }
        embedded = embedded
        scriptFolders = None
        excludeGlobPatterns = None
        maxFileSize = Some 8
    }
    let Customoptions : CustomSettings = {
        rootDirectories = folders
        modFilter = Some modFilter
        validation = {
            validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
            experimental = true
            langs = langs
        }
        rules = Some { ruleFiles = config; validateRules = true; debugRulesOnly = true; debugMode = false }
        embedded = embedded
        scriptFolders = None
        excludeGlobPatterns = None
        maxFileSize = Some 8
    }
    let IRoptions : IRSettings = {
        rootDirectories = folders
        modFilter = Some modFilter
        validation = {
            validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
            experimental = true
            langs = langs
        }
        rules = Some { ruleFiles = config; validateRules = true; debugRulesOnly = true; debugMode = false }
        embedded = embedded
        scriptFolders = None
        excludeGlobPatterns = None
        maxFileSize = Some 8
    }
    let CK3options : CK3Settings = {
        rootDirectories = folders
        modFilter = Some modFilter
        validation = {
            validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
            experimental = true
            langs = langs
        }
        rules = Some { ruleFiles = config; validateRules = true; debugRulesOnly = true; debugMode = false }
        embedded = embedded
        scriptFolders = None
        excludeGlobPatterns = None
        maxFileSize = Some 8
    }
    let game : IGame =
        match game with
        |Game.HOI4 -> HOI4Game(HOI4options) :> IGame
        |Game.STL -> STLGame(STLoptions) :> IGame
        |Game.EU4 -> EU4Game(EU4options) :> IGame
        |Game.CK2 -> CK2Game(CK2options) :> IGame
        |Game.VIC2 -> VIC2Game(VIC2options) :> IGame
        |Game.IR -> IRGame(IRoptions) :> IGame
        |Game.CK3 -> CK3Game(CK3options) :> IGame
        |Game.Custom -> CustomGame(Customoptions, "") :> IGame
    game

let serializeMetadata (dir : string, scope : FilesScope, modFilter : string, config, game : Game, outputFileName) =
    let gameObj = loadGame (dir, scope, modFilter, config, game, FromConfig ([], []), None)
    let data = gameObj.GetEmbeddedMetadata()
    let fileListFiles =
        match File.Exists (Path.Combine(dir, "cwtools-files.csv")) with
        | true ->
            File.ReadAllLines(Path.Combine(dir, "cwtools-files.csv")) |> Set.ofArray
        | false -> Set.empty
    let data = { data with files = Set.union data.files fileListFiles }
    let pickle = binarySerializer.Pickle data
    let filename =
        match outputFileName with
        | Some name -> name
        | None ->
            let gameName = game |> function |Game.CK2 -> "ck2" |Game.HOI4 -> "hoi4" |Game.EU4 -> "eu4" |Game.STL -> "stl" |Game.VIC2 -> "vic2" |Game.IR -> "ir" |Game.CK3 -> "ck3" |Game.Custom -> "custom"
            sprintf "%s.cwv.bz2" gameName
    compressAndWrite pickle (filename)
    eprintfn "Metadata cache file created at %s, relative to CWD" filename


// let deserializeEU4 path =
//     let cacheFile = File.ReadAllBytes(path)
//     let cached = binarySerializer.UnPickle<CachedResourceData> cacheFile
//     fileIndexTable <- cached.fileIndexTable
//     cached.resources


