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
open CWTools.Localisation.STLLocalisation
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
open CWTools.Validation.Rules
open CWTools.Validation.HOI4
open CWTools.Games.Stellaris.STLLookup


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


let serialize gameDirName scriptFolders cacheDirectory = ()
let serializeSTL folder cacheDirectory =
    let fileManager = FileManager(folder, Some "", FilesScope.Vanilla, STLConstants.scriptFolders, "stellaris", Encoding.UTF8, [])
    let files = fileManager.AllFilesByPath()
    let computefun : unit -> FoldRules<STLConstants.Scope> option = (fun () -> (None))
    let resources = ResourceManager<STLComputedData>(STLCompute.computeSTLData computefun, STLCompute.computeSTLDataUpdate computefun, Encoding.UTF8, Encoding.GetEncoding(1252)).Api
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
    File.WriteAllBytes(Path.Combine(cacheDirectory, "stl.cwb"), pickle)

let serializeEU4 folder cacheDirectory =
    let fileManager = FileManager(folder, Some "", FilesScope.Vanilla, EU4Constants.scriptFolders, "stellaris", Encoding.UTF8, [])
    let files = fileManager.AllFilesByPath()
    let computefun : unit -> FoldRules<EU4Constants.Scope> option = (fun () -> (None))
    let resources = ResourceManager<EU4ComputedData>(EU4Compute.computeEU4Data computefun, EU4Compute.computeEU4DataUpdate computefun, Encoding.GetEncoding(1252), Encoding.UTF8).Api
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
    File.WriteAllBytes(Path.Combine(cacheDirectory, "eu4.cwb"), pickle)
let serializeHOI4 folder cacheDirectory =
    let fileManager = FileManager(folder, Some "", FilesScope.Vanilla, HOI4Constants.scriptFolders, "hearts of iron iv", Encoding.UTF8, [])
    let files = fileManager.AllFilesByPath()
    let computefun : unit -> FoldRules<HOI4Constants.Scope> option = (fun () -> (None))
    let resources = ResourceManager<HOI4ComputedData>(HOI4Compute.computeHOI4Data computefun, HOI4Compute.computeHOI4DataUpdate computefun, Encoding.UTF8, Encoding.GetEncoding(1252)).Api
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
    File.WriteAllBytes(Path.Combine(cacheDirectory, "hoi4.cwb"), pickle)

let deserialize path =
    // registry.DeclareSerializable<System.LazyHelper>()
    // registry.DeclareSerializable<Lazy>()
    match File.Exists path with
    |true ->
        let cacheFile = File.ReadAllBytes(path)
        // let cacheFile = Assembly.GetEntryAssembly().GetManifestResourceStream("Main.files.pickled.cwb")
        //                 |> (fun f -> use ms = new MemoryStream() in f.CopyTo(ms); ms.ToArray())
        let cached = binarySerializer.UnPickle<CachedResourceData> cacheFile
        fileIndexTable <- cached.fileIndexTable
        StringResource.stringManager <- cached.stringResourceManager
        cached.resources, cached.files
    |false -> [], []

// let deserializeEU4 path =
//     let cacheFile = File.ReadAllBytes(path)
//     let cached = binarySerializer.UnPickle<CachedResourceData> cacheFile
//     fileIndexTable <- cached.fileIndexTable
//     cached.resources


