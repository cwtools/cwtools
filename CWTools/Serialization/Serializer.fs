module CWTools.Serializer

open System.Collections.Concurrent
open System.Collections.Generic
open System.Text
open CWTools.Common
open System.IO
open System.Reflection
open CWTools.Games.Files
open CWTools.Games
open CWTools.Games.VIC3
open MBrace.FsPickler
open CWTools.Process
open CWTools.Utilities.Position
open CWTools.Utilities
open CWTools.Games.EU4
open CWTools.Rules
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
open CWTools.CompressionOptions

let mkPickler (resolver: IPicklerResolver) =
    let arrayPickler = resolver.Resolve<Leaf array>()

    let writer (w: WriteState) (ns: Lazy<Leaf array>) =
        arrayPickler.Write w "value" (ns.Force())

    let reader (r: ReadState) =
        let v = arrayPickler.Read r "value" in Lazy<Leaf array>.CreateFromValue v

    Pickler.FromPrimitives(reader, writer)

let mkConcurrentDictionaryPickler<'a, 'b> (resolver: IPicklerResolver) =
    let dictionaryPickler = resolver.Resolve<KeyValuePair<_, _>[]>()

    let writer (w: WriteState) (dict: ConcurrentDictionary<'a, 'b>) =
        dictionaryPickler.Write w "value" (dict.ToArray())

    let reader (r: ReadState) =
        let v = dictionaryPickler.Read r "value" in new ConcurrentDictionary<_, _>(v)

    Pickler.FromPrimitives(reader, writer)

let registry = new CustomPicklerRegistry()
do registry.RegisterFactory mkPickler
do registry.RegisterFactory mkConcurrentDictionaryPickler<int, string>
do registry.RegisterFactory mkConcurrentDictionaryPickler<int, StringMetadata>
do registry.RegisterFactory mkConcurrentDictionaryPickler<string, StringTokens>
registry.DeclareSerializable<FParsec.Position>()
#nowarn "8989"
let picklerCache = PicklerCache.FromCustomPicklerRegistry registry

let binarySerializer =
    FsPickler.CreateBinarySerializer(picklerResolver = picklerCache)

let assemblyLocation = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)

let compressAndWrite (input: byte array) (file: string) =
    use stream = new MemoryStream(input)
    use fileStream = File.Create(file)
    ICSharpCode.SharpZipLib.BZip2.BZip2.Compress(stream, fileStream, true, 9)

let decompress (path: string) =
    use outStream = new MemoryStream()
    let bytes = File.ReadAllBytes(path)
    use inStream = new MemoryStream(bytes)
    ICSharpCode.SharpZipLib.BZip2.BZip2.Decompress(inStream, outStream, false)
    outStream.ToArray()

let addDLCs (workspaceDirectory: WorkspaceDirectory) =
    let dir = workspaceDirectory.path

    if Directory.Exists(dir) && Directory.Exists(Path.Combine [| dir; "dlc" |]) then
        let dlcs = Directory.EnumerateDirectories(Path.Combine [| dir; "dlc" |])

        let createZippedDirectory (dlcDir: string) =
            match
                Directory.EnumerateFiles dlcDir
                |> Seq.tryFind (fun f -> (Path.GetExtension f) = ".zip")
            with
            | Some zip ->
                use file = File.OpenRead(zip)

                try
                    use zipFile = new ZipArchive(file, ZipArchiveMode.Read)

                    let files =
                        zipFile.Entries
                        |> Seq.map (fun e ->
                            Path.Combine([| "uri:"; zip; e.FullName.Replace("\\", "/") |]),
                            let sr = new StreamReader(e.Open()) in sr.ReadToEnd())
                        |> List.ofSeq

                    Some(
                        ZD
                            { ZippedDirectory.name = Path.GetFileName zip
                              path = zip.Replace("\\", "/")
                              files = files }
                    )
                with _ ->
                    None
            | None ->
                Some(
                    WD
                        { WorkspaceDirectory.name = (Path.GetDirectoryName dlcDir)
                          path = dlcDir }
                )

        dlcs |> Seq.choose createZippedDirectory |> List.ofSeq
    else
        []

let serialize gameDirName scriptFolders cacheDirectory = ()

let serializeSTL folder outputFileName compression =
    let fileManager =
        FileManager(folder, Some "", STLConstants.scriptFolders, "stellaris", Encoding.UTF8, [], 2)

    let files = fileManager.AllFilesByPath()
    let computefun: unit -> InfoService option = (fun () -> (None))

    let resources =
        ResourceManager<STLComputedData>(
            STL.computeSTLData computefun,
            STL.computeSTLDataUpdate computefun,
            Encoding.UTF8,
            Encoding.GetEncoding(1252),
            true
        )
            .Api

    let entities =
        resources.UpdateFiles(files)
        |> List.choose (fun (r, e) ->
            e
            |> function
                | Some e2 -> Some(r, e2)
                | _ -> None)
        |> List.map (fun (r, (struct (e, _))) -> r, e)

    let files =
        resources.GetResources()
        |> List.choose (function
            | FileResource(_, r) -> Some(r.logicalpath, "")
            | FileWithContentResource(_, r) -> Some(r.logicalpath, r.filetext)
            | _ -> None)

    let data =
        { resources = entities
          fileIndexTable = fileIndexTable
          files = files
          stringResourceManager = StringResource.stringManager }

    let pickle = binarySerializer.Pickle data
    let baseFilename = outputFileName |> Option.defaultValue "stl.cwb"
    let filename = baseFilename + (getExtension compression)
    compressAndWriteToFile compression pickle filename
    filename

let serializeEU4 folder outputFileName compression =
    let folders = (WD folder) :: (addDLCs folder)

    let fileManager =
        FileManager(folders, Some "", EU4Constants.scriptFolders, "stellaris", Encoding.UTF8, [], 2)

    let files = fileManager.AllFilesByPath()
    let computefun: unit -> InfoService option = (fun () -> (None))

    let resources =
        ResourceManager<EU4ComputedData>(
            EU4.computeEU4Data computefun,
            EU4.computeEU4DataUpdate computefun,
            Encoding.GetEncoding(1252),
            Encoding.UTF8,
            false
        )
            .Api

    let entities =
        resources.UpdateFiles(files)
        |> List.choose (fun (r, e) ->
            e
            |> function
                | Some e2 -> Some(r, e2)
                | _ -> None)
        |> List.map (fun (r, (struct (e, _))) -> r, e)

    let files =
        resources.GetResources()
        |> List.choose (function
            | FileResource(_, r) -> Some(r.logicalpath, "")
            | FileWithContentResource(_, r) -> Some(r.logicalpath, r.filetext)
            | _ -> None)

    let data =
        { resources = entities
          fileIndexTable = fileIndexTable
          files = files
          stringResourceManager = StringResource.stringManager }

    let pickle = binarySerializer.Pickle data
    let baseFilename = outputFileName |> Option.defaultValue "eu4.cwb"
    let filename = baseFilename + (getExtension compression)
    compressAndWriteToFile compression pickle filename
    filename

let serializeHOI4 folder outputFileName compression =
    let folders = (WD folder) :: (addDLCs folder)

    let fileManager =
        FileManager(folders, Some "", HOI4Constants.scriptFolders, "hearts of iron iv", Encoding.UTF8, [], 2)

    let files = fileManager.AllFilesByPath()
    let computefun: unit -> InfoService option = (fun () -> (None))

    let resources =
        ResourceManager<HOI4ComputedData>(
            computeHOI4Data computefun,
            computeHOI4DataUpdate computefun,
            Encoding.UTF8,
            Encoding.GetEncoding(1252),
            false
        )
            .Api

    let entities =
        resources.UpdateFiles(files)
        |> List.choose (fun (r, e) ->
            e
            |> function
                | Some e2 -> Some(r, e2)
                | _ -> None)
        |> List.map (fun (r, (struct (e, _))) -> r, e)

    let files =
        resources.GetResources()
        |> List.choose (function
            | FileResource(_, r) -> Some(r.logicalpath, "")
            | FileWithContentResource(_, r) -> Some(r.logicalpath, r.filetext)
            | _ -> None)

    let data =
        { resources = entities
          fileIndexTable = fileIndexTable
          files = files
          stringResourceManager = StringResource.stringManager }

    let pickle = binarySerializer.Pickle data
    let baseFilename = outputFileName |> Option.defaultValue "hoi4.cwb"
    let filename = baseFilename + (getExtension compression)
    compressAndWriteToFile compression pickle filename
    filename

let serializeCK2 folder outputFileName compression =
    let fileManager =
        FileManager(folder, Some "", CK2Constants.scriptFolders, "crusader kings ii", Encoding.UTF8, [], 2)

    let files = fileManager.AllFilesByPath()
    let computefun: unit -> InfoService option = (fun () -> (None))

    let resources =
        ResourceManager<CK2ComputedData>(
            computeCK2Data computefun,
            computeCK2DataUpdate computefun,
            Encoding.UTF8,
            Encoding.GetEncoding(1252),
            false
        )
            .Api

    let entities =
        resources.UpdateFiles(files)
        |> List.choose (fun (r, e) ->
            e
            |> function
                | Some e2 -> Some(r, e2)
                | _ -> None)
        |> List.map (fun (r, (struct (e, _))) -> r, e)

    let files =
        resources.GetResources()
        |> List.choose (function
            | FileResource(_, r) -> Some(r.logicalpath, "")
            | FileWithContentResource(_, r) -> Some(r.logicalpath, r.filetext)
            | _ -> None)

    let data =
        { resources = entities
          fileIndexTable = fileIndexTable
          files = files
          stringResourceManager = StringResource.stringManager }

    let pickle = binarySerializer.Pickle data
    let baseFilename = outputFileName |> Option.defaultValue "ck2.cwb"
    let filename = baseFilename + (getExtension compression)
    compressAndWriteToFile compression pickle filename
    filename

let serializeIR folder outputFileName compression =
    let fileManager =
        FileManager(folder, Some "", IRConstants.scriptFolders, "imperator", Encoding.UTF8, [], 2)

    let files = fileManager.AllFilesByPath()
    let computefun: unit -> InfoService option = (fun () -> (None))

    let resources =
        ResourceManager<IRComputedData>(
            Compute.Jomini.computeJominiData computefun,
            Compute.Jomini.computeJominiDataUpdate computefun,
            Encoding.UTF8,
            Encoding.GetEncoding(1252),
            false
        )
            .Api

    let entities =
        resources.UpdateFiles(files)
        |> List.choose (fun (r, e) ->
            e
            |> function
                | Some e2 -> Some(r, e2)
                | _ -> None)
        |> List.map (fun (r, (struct (e, _))) -> r, e)

    let files =
        resources.GetResources()
        |> List.choose (function
            | FileResource(_, r) -> Some(r.logicalpath, "")
            | FileWithContentResource(_, r) -> Some(r.logicalpath, r.filetext)
            | _ -> None)

    let data =
        { resources = entities
          fileIndexTable = fileIndexTable
          files = files
          stringResourceManager = StringResource.stringManager }

    let pickle = binarySerializer.Pickle data
    let baseFilename = outputFileName |> Option.defaultValue "ir.cwb"
    let filename = baseFilename + (getExtension compression)
    compressAndWriteToFile compression pickle filename
    filename

let serializeCK3 folder outputFileName compression =
    let fileManager =
        FileManager(folder, Some "", CK3Constants.scriptFolders, "crusader kings iii", Encoding.UTF8, [], 2)

    let files = fileManager.AllFilesByPath()
    let computefun: unit -> InfoService option = (fun () -> (None))

    let resources =
        ResourceManager<CK3ComputedData>(
            Compute.Jomini.computeJominiData computefun,
            Compute.Jomini.computeJominiDataUpdate computefun,
            Encoding.UTF8,
            Encoding.GetEncoding(1252),
            false
        )
            .Api

    let entities =
        resources.UpdateFiles(files)
        |> List.choose (fun (r, e) ->
            e
            |> function
                | Some e2 -> Some(r, e2)
                | _ -> None)
        |> List.map (fun (r, (struct (e, _))) -> r, e)

    let files =
        resources.GetResources()
        |> List.choose (function
            | FileResource(_, r) -> Some(r.logicalpath, "")
            | FileWithContentResource(_, r) -> Some(r.logicalpath, r.filetext)
            | _ -> None)

    let data =
        { resources = entities
          fileIndexTable = fileIndexTable
          files = files
          stringResourceManager = StringResource.stringManager }

    let pickle = binarySerializer.Pickle data
    let baseFilename = outputFileName |> Option.defaultValue "ck3.cwb"
    let filename = baseFilename + (getExtension compression)
    compressAndWriteToFile compression pickle filename
    filename

let serializeVIC3 folder outputFileName compression =
    let fileManager =
        FileManager(folder, Some "", VIC3Constants.scriptFolders, "victoria 3", Encoding.UTF8, [], 2)

    let files = fileManager.AllFilesByPath()
    let computefun: unit -> InfoService option = (fun () -> (None))

    let resources =
        ResourceManager<VIC3ComputedData>(
            Compute.Jomini.computeJominiData computefun,
            Compute.Jomini.computeJominiDataUpdate computefun,
            Encoding.UTF8,
            Encoding.GetEncoding(1252),
            false
        )
            .Api

    let entities =
        resources.UpdateFiles(files)
        |> List.choose (fun (r, e) ->
            e
            |> function
                | Some e2 -> Some(r, e2)
                | _ -> None)
        |> List.map (fun (r, (struct (e, _))) -> r, e)

    let files =
        resources.GetResources()
        |> List.choose (function
            | FileResource(_, r) -> Some(r.logicalpath, "")
            | FileWithContentResource(_, r) -> Some(r.logicalpath, r.filetext)
            | _ -> None)

    let data =
        { resources = entities
          fileIndexTable = fileIndexTable
          files = files
          stringResourceManager = StringResource.stringManager }

    let pickle = binarySerializer.Pickle data
    let baseFilename = outputFileName |> Option.defaultValue "vic3.cwb"
    let filename = baseFilename + (getExtension compression)
    compressAndWriteToFile compression pickle filename
    filename

let serializeVIC2 folder outputFileName compression =
    let fileManager =
        FileManager(folder, Some "", VIC2Constants.scriptFolders, "victoria 2", Encoding.UTF8, [], 2)

    let files = fileManager.AllFilesByPath()
    let computefun: unit -> InfoService option = (fun () -> (None))

    let resources =
        ResourceManager<VIC2ComputedData>(
            computeVIC2Data computefun,
            computeVIC2DataUpdate computefun,
            Encoding.UTF8,
            Encoding.GetEncoding(1252),
            false
        )
            .Api

    let entities =
        resources.UpdateFiles(files)
        |> List.choose (fun (r, e) ->
            e
            |> function
                | Some e2 -> Some(r, e2)
                | _ -> None)
        |> List.map (fun (r, (struct (e, _))) -> r, e)

    let files =
        resources.GetResources()
        |> List.choose (function
            | FileResource(_, r) -> Some(r.logicalpath, "")
            | FileWithContentResource(_, r) -> Some(r.logicalpath, r.filetext)
            | _ -> None)

    let data =
        { resources = entities
          fileIndexTable = fileIndexTable
          files = files
          stringResourceManager = StringResource.stringManager }

    let pickle = binarySerializer.Pickle data
    let baseFilename = outputFileName |> Option.defaultValue "vic2.cwb"
    let filename = baseFilename + (getExtension compression)
    compressAndWriteToFile compression pickle filename
    filename

let deserialize path =
    match File.Exists path, Path.GetExtension path with
    | true, ".bz2" ->
        let cacheFile = decompress path
        let cached = binarySerializer.UnPickle<CachedResourceData> cacheFile
        fileIndexTable <- cached.fileIndexTable
        StringResource.stringManager <- cached.stringResourceManager
        cached.resources, cached.files
    | true, _ ->
        let cacheFile = File.ReadAllBytes(path)
        let cached = binarySerializer.UnPickle<CachedResourceData> cacheFile
        fileIndexTable <- cached.fileIndexTable
        StringResource.stringManager <- cached.stringResourceManager
        cached.resources, cached.files
    | false, _ -> failwithf "Cache file %s was specified but does not exist" path

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

    { metadata with
        varDefs =
            metadata.varDefs
            |> Map.map (fun k v -> v |> List.map (fun (s, _) -> (s, range.Zero)))
        typeDefs =
            metadata.typeDefs
            |> Map.map (fun k v ->
                v
                |> List.map (fun t ->
                    { t with
                        range = range.Zero
                        validate = false })) }

let loadGame<'T when 'T :> ComputedData>
    (dir: string, scope: FilesScope, modFilter: string, config, game: Game, embedded: EmbeddedSetupSettings, langSetting) =
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

    let folders =
        WD { path = dir; name = "game" } :: addDLCs { path = dir; name = "game" }

    let STLoptions: StellarisSettings =
        { rootDirectories = folders
          modFilter = Some modFilter
          validation =
            { validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
              experimental = true
              langs = langs }
          rules =
            Some
                { ruleFiles = config
                  validateRules = true
                  debugRulesOnly = true
                  debugMode = false }
          embedded = embedded
          scriptFolders = None
          excludeGlobPatterns = None
          maxFileSize = None
          debugSettings = DebugSettings.Default }

    let HOI4options: HOI4Settings =
        { rootDirectories = folders
          modFilter = Some modFilter
          validation =
            { validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
              experimental = true
              langs = langs }
          rules =
            Some
                { ruleFiles = config
                  validateRules = true
                  debugRulesOnly = true
                  debugMode = false }
          embedded = embedded
          scriptFolders = None
          excludeGlobPatterns = None
          maxFileSize = Some 8
          debugSettings = DebugSettings.Default

        }

    let EU4options: EU4Settings =
        { rootDirectories = folders
          modFilter = Some modFilter
          validation =
            { validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
              experimental = true
              langs = langs }
          rules =
            Some
                { ruleFiles = config
                  validateRules = true
                  debugRulesOnly = true
                  debugMode = false }
          embedded = embedded
          scriptFolders = None
          excludeGlobPatterns = None
          maxFileSize = Some 8
          debugSettings = DebugSettings.Default }

    let CK2options: CK2Settings =
        { rootDirectories = folders
          modFilter = Some modFilter
          validation =
            { validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
              experimental = true
              langs = langs }
          rules =
            Some
                { ruleFiles = config
                  validateRules = true
                  debugRulesOnly = true
                  debugMode = false }
          embedded = embedded
          scriptFolders = None
          excludeGlobPatterns = None
          maxFileSize = Some 8
          debugSettings = DebugSettings.Default }

    let VIC2options: VIC2Settings =
        { rootDirectories = folders
          modFilter = Some modFilter
          validation =
            { validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
              experimental = true
              langs = langs }
          rules =
            Some
                { ruleFiles = config
                  validateRules = true
                  debugRulesOnly = true
                  debugMode = false }
          embedded = embedded
          scriptFolders = None
          excludeGlobPatterns = None
          maxFileSize = Some 8
          debugSettings = DebugSettings.Default }

    let Customoptions: CustomSettings =
        { rootDirectories = folders
          modFilter = Some modFilter
          validation =
            { validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
              experimental = true
              langs = langs }
          rules =
            Some
                { ruleFiles = config
                  validateRules = true
                  debugRulesOnly = true
                  debugMode = false }
          embedded = embedded
          scriptFolders = None
          excludeGlobPatterns = None
          maxFileSize = Some 8
          debugSettings = DebugSettings.Default }

    let IRoptions: IRSettings =
        { rootDirectories = folders
          modFilter = Some modFilter
          validation =
            { validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
              experimental = true
              langs = langs }
          rules =
            Some
                { ruleFiles = config
                  validateRules = true
                  debugRulesOnly = true
                  debugMode = false }
          embedded = embedded
          scriptFolders = None
          excludeGlobPatterns = None
          maxFileSize = Some 8
          debugSettings = DebugSettings.Default }

    let CK3options: CK3Settings =
        { rootDirectories = folders
          modFilter = Some modFilter
          validation =
            { validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
              experimental = true
              langs = langs }
          rules =
            Some
                { ruleFiles = config
                  validateRules = true
                  debugRulesOnly = true
                  debugMode = false }
          embedded = embedded
          scriptFolders = None
          excludeGlobPatterns = None
          maxFileSize = Some 8
          debugSettings = DebugSettings.Default }

    let VIC3options: VIC3Settings =
        { rootDirectories = folders
          modFilter = Some modFilter
          validation =
            { validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
              experimental = true
              langs = langs }
          rules =
            Some
                { ruleFiles = config
                  validateRules = true
                  debugRulesOnly = true
                  debugMode = false }
          embedded = embedded
          scriptFolders = None
          excludeGlobPatterns = None
          maxFileSize = Some 8
          debugSettings = DebugSettings.Default }

    let game: IGame =
        match game with
        | Game.HOI4 -> HOI4Game(HOI4options) :> IGame
        | Game.STL -> STLGame(STLoptions) :> IGame
        | Game.EU4 -> EU4Game(EU4options) :> IGame
        | Game.EU5 -> EU4Game(EU4options) :> IGame
        | Game.CK2 -> CK2Game(CK2options) :> IGame
        | Game.VIC2 -> VIC2Game(VIC2options) :> IGame
        | Game.IR -> IRGame(IRoptions) :> IGame
        | Game.CK3 -> CK3Game(CK3options) :> IGame
        | Game.VIC3 -> VIC3Game(VIC3options) :> IGame
        | Game.Custom -> CustomGame(Customoptions, "") :> IGame
        | _ -> failwith "Unknown game"

    game

let serializeMetadata (dir: string, scope: FilesScope, modFilter: string, config, game: Game, outputFileName) =
    let gameObj =
        loadGame (dir, scope, modFilter, config, game, FromConfig([], []), None)

    let data = gameObj.GetEmbeddedMetadata()

    let fileListFiles =
        match File.Exists(Path.Combine(dir, "cwtools-files.csv")) with
        | true -> File.ReadAllLines(Path.Combine(dir, "cwtools-files.csv")) |> Set.ofArray
        | false -> Set.empty

    let data =
        { data with
            files = Set.union data.files fileListFiles }

    let pickle = binarySerializer.Pickle data

    let filename =
        match outputFileName with
        | Some name -> name
        | None ->
            let gameName =
                game
                |> function
                    | Game.CK2 -> "ck2"
                    | Game.HOI4 -> "hoi4"
                    | Game.EU4 -> "eu4"
                    | Game.EU5 -> "eu5"
                    | Game.STL -> "stl"
                    | Game.VIC2 -> "vic2"
                    | Game.IR -> "ir"
                    | Game.CK3 -> "ck3"
                    | Game.Custom -> "custom"
                    | Game.VIC3 -> "vic3"
                    | _ -> failwith "Unknown game"

            sprintf "%s.cwv.bz2" gameName

    compressAndWrite pickle (filename)
    eprintfn "Metadata cache file created at %s, relative to CWD" filename
