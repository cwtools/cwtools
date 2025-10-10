namespace CWTools.Games

open System
open System.IO
open System.Linq
open CSharpHelpers
open CWTools.Parser
open CWTools.Process
open CWTools.Utilities.Position
open CWTools.Utilities.Utils
open Shared
open FSharp.Collections.ParallelSeq
open FParsec
open DotNet.Globbing

module Files =
    type FilesScope =
        | All = 1uy
        | Mods = 2uy
        | Vanilla = 3uy

    //TODO: normalised rootDirectory.Replace("\\","/").TrimStart('.')
    type WorkspaceDirectory = { path: string; name: string }

    type ZippedDirectory =
        { path: string
          name: string
          files: struct (string * string) array }

    type WorkspaceDirectoryInput =
        | WD of WorkspaceDirectory
        | ZD of ZippedDirectory

    [<Sealed>]
    type FileManager
        (
            inputDirectories: WorkspaceDirectoryInput array,
            embeddedFolder: string option,
            scriptFolders: string array,
            gameDirName: string,
            encoding: System.Text.Encoding,
            ignoreGlobList: string array,
            maxFileSizeMB: int
        ) =
        let rootDirectories, zippedDirectories =
            inputDirectories
            |> Array.choose (function
                | WD wd -> Some wd
                | _ -> None),
            inputDirectories
            |> Array.choose (function
                | ZD zd -> Some zd
                | _ -> None)

        let allDirsBelowRoot (workspaceDir: WorkspaceDirectory) =
            if Directory.Exists workspaceDir.path then
                [| workspaceDir.path |]
                |> Seq.append (Directory.EnumerateDirectories(workspaceDir.path, "*", SearchOption.AllDirectories))
                |> Seq.map (fun folder -> folder, Path.GetFileName folder)
                |> Array.ofSeq
            else
                [||]

        let isVanillaDirectory (workspaceDir: WorkspaceDirectory) =
            let dir =
                allDirsBelowRoot workspaceDir
                |> Array.tryFind (fun (_, folder) ->
                    folder == gameDirName
                    || folder == "game"
                    || folder == gameDirName.Replace(' ', '_'))
                |> Option.map fst
                |> Option.bind (fun f ->
                    if Directory.Exists(Path.Combine(f, "common")) then
                        Some f
                    else
                        None)

            dir.IsSome

        let classifyDirectory (workspaceDir: WorkspaceDirectory) =
            let checkFolderLooksLikeGameFolder =
                (fun (f: string) ->
                    let f = Path.GetFileName f in

                    f = "common"
                    || f = "events"
                    || f = "interface"
                    || f = "gfx"
                    || f = "localisation")

            let getMultipleModDirectory (workspaceDir: WorkspaceDirectory) : ModInfo array =
                let getModFiles modDir =
                    Directory.EnumerateFiles(modDir, "*.mod")
                    |> Seq.map CKParser.parseFile
                    |> Seq.choose (function
                        | Success(p, _, _) -> Some p
                        | _ -> None)
                    |> Seq.map (
                        (ProcessCore.processNodeBasic "mod" range.Zero)
                        >> (fun s -> s.TagText "name", "../" + (s.TagText "path"))
                    )

                let modFolder =
                    allDirsBelowRoot workspaceDir
                    |> Array.tryFind (fun (_, folder) -> folder == "mod" || folder == "mods")

                match modFolder with
                | None -> [||]
                | Some(mf, _) ->
                    let modFoldersFromDot =
                        getModFiles mf
                        |> Seq.map (fun (n, p) ->
                            { ModInfo.name = n
                              path = Path.Combine(workspaceDir.path, p) })

                    let modFoldersFromRoot =
                        Directory.EnumerateDirectories mf
                        |> Seq.filter (fun folder ->
                            Directory.EnumerateDirectories folder
                            |> Seq.exists checkFolderLooksLikeGameFolder)
                        |> Seq.map (fun folder ->
                            { ModInfo.name = Path.GetFileName folder
                              path = folder })

                    modFoldersFromDot.Concat(modFoldersFromRoot).Distinct().ToArray()

            let checkIsGameFolder (workspaceDir: WorkspaceDirectory) =
                let foundAnyFolders =
                    Directory.EnumerateDirectories workspaceDir.path
                    |> Seq.exists checkFolderLooksLikeGameFolder

                foundAnyFolders

            let result: DirectoryType =
                if isVanillaDirectory workspaceDir then
                    DirectoryType.Vanilla
                elif checkIsGameFolder workspaceDir then
                    DirectoryType.Mod
                else
                    let array = getMultipleModDirectory workspaceDir

                    if not (Array.isEmpty array) then
                        DirectoryType.MultipleMod array
                    else
                        DirectoryType.Unknown

            result

        let expandedRootDirectories =
            let expanded =
                rootDirectories
                |> Array.map (fun rd ->
                    let normalisedPath = rd.path.Replace('\\', '/').TrimStart('.')

                    { path = rd.path
                      name = rd.name
                      dirType = classifyDirectory rd
                      normalisedPath = normalisedPath })

            let embeddedDir =
                embeddedFolder
                |> Option.map (fun e ->
                    { path = e
                      name = "embedded"
                      dirType = Unknown
                      normalisedPath = e.Replace('\\', '/').TrimStart('.') })

            match embeddedDir with
            | Some ed -> Array.append [| ed |] expanded
            | None -> expanded

        do
            logInfo "Workspace roots:"

            rootDirectories
            |> Array.iter (fun rd -> logInfo (sprintf "root %s, exists: %b" rd.path (Directory.Exists rd.path)))

            logInfo (sprintf "embedded folder %A" embeddedFolder)

            expandedRootDirectories
            |> Array.iter (fun rd -> log (sprintf "normalised %s" rd.normalisedPath))

        let allFoldersInWorkspaceDir (workspaceDir: ExpandedWorkspaceDirectory) =
            let allFoldersInDirectory (workspaceDir: ExpandedWorkspaceDirectory) =
                match workspaceDir.dirType with
                | Mod ->
                    [| { ModInfo.name = workspaceDir.name
                         path = workspaceDir.path } |]
                | Vanilla ->
                    [| { ModInfo.name = "vanilla"
                         path = workspaceDir.path } |]
                | MultipleMod mods -> mods
                | Unknown -> [||]

            workspaceDir
            |> allFoldersInDirectory
            |> Array.filter (fun d -> Directory.Exists d.path)

        let fileToResourceInput
            (normalisedPath: string)
            normalisedPathLength
            (scope, filepath: string)
            (fileLength: int64)
            (fileTextThunk: unit -> string)
            =
            let rootedPath =
                filepath
                    .AsSpan()
                    .Slice(
                        filepath.IndexOf(normalisedPath, StringComparison.Ordinal)
                        + normalisedPathLength
                        + 1
                    )

            match Path.GetExtension(filepath) with
            | ".txt"
            | ".gui"
            | ".gfx"
            | ".sfx"
            | ".asset"
            | ".map" ->
                if fileLength > ((int64 maxFileSizeMB) * 1000000L) then
                    None
                else
                    Some(
                        EntityResourceInput
                            { scope = scope
                              filepath = filepath
                              logicalpath =
                                FileManagerHelper.ConvertPathToLogicalPath(
                                    rootedPath,
                                    expandedRootDirectories,
                                    scriptFolders
                                )
                              filetext = fileTextThunk ()
                              validate = true }
                    )
            | ".dds"
            | ".tga"
            | ".shader"
            | ".lua"
            | ".png"
            | ".mesh"
            | ".ttf"
            | ".otf"
            | ".wav" ->
                Some(
                    FileResourceInput
                        { scope = scope
                          filepath = filepath
                          logicalpath =
                            FileManagerHelper.ConvertPathToLogicalPath(
                                rootedPath,
                                expandedRootDirectories,
                                scriptFolders
                            ) }
                )
            | ".yml"
            | ".csv" ->
                Some(
                    FileWithContentResourceInput
                        { scope = scope
                          filepath = filepath
                          logicalpath =
                            FileManagerHelper.ConvertPathToLogicalPath(
                                rootedPath,
                                expandedRootDirectories,
                                scriptFolders
                            )
                          filetext = fileTextThunk ()
                          validate = true }
                )
            | _ -> None
        //File.ReadAllText(filepath, encoding
        let allFilesByPath (workspaceDir: ExpandedWorkspaceDirectory) =
            let globs = ignoreGlobList |> Array.map Glob.Parse

            let excludeGlobTest (path: string) = globs |> Array.exists _.IsMatch(path)

            let getAllFiles (modInfo: ModInfo) =
                //log "%A" path
                scriptFolders
                |> Array.map (
                    (fun folder -> modInfo.name, Path.GetFullPath(Path.Combine(modInfo.path, folder)))
                    >> (fun (scope, folder) ->
                        scope,
                        folder,
                        (if Directory.Exists folder then
                             Directory.GetFiles(folder, "*", SearchOption.AllDirectories)
                         else
                             [||]))
                )
                |> Array.collect (fun (scope, _, files) -> files |> Array.map (fun f -> scope, f))
                |> Array.distinct
                |> Array.filter (fun (_, f) -> f |> excludeGlobTest |> not)

            let allFiles =
                duration
                    (fun _ ->
                        PSeq.map getAllFiles (allFoldersInWorkspaceDir workspaceDir)
                        |> PSeq.collect id
                        |> PSeq.choose (fun (scope, fn) ->
                            (fileToResourceInput
                                workspaceDir.normalisedPath
                                workspaceDir.normalisedPath.Length
                                (scope, fn)
                                (fn |> FileInfo).Length
                                (fun _ -> File.ReadAllText(fn, encoding))))
                        |> Array.ofSeq)
                    "Load files"

            allFiles

        let allFilesInZips =
            let zippedDirToResourceInputs (zd: ZippedDirectory) =
                let normalisedPath = zd.path.Replace('\\', '/').TrimStart('.')

                zd.files
                |> Array.choose (fun struct (fn, ft) ->
                    fileToResourceInput normalisedPath normalisedPath.Length (zd.name, fn) 0L (fun _ -> ft))

            zippedDirectories |> Array.collect zippedDirToResourceInputs

        let doesWorkspaceContainVanillaDirectory =
            rootDirectories |> Array.exists isVanillaDirectory

        let getScopeForPath (path: string) =
            let path = path.Replace('\\', '/')

            expandedRootDirectories
            |> Array.tryPick (fun rd ->
                let index = path.IndexOf(rd.normalisedPath, StringComparison.Ordinal)
                if index >= 0 then Some rd.name else None)

        member _.AllFilesByPath() =
            // TODO: ResizeArray
            Array.append (expandedRootDirectories |> Array.collect allFilesByPath) allFilesInZips

        member _.AllFolders() =
            expandedRootDirectories
            |> Array.collect allFoldersInWorkspaceDir
            |> Array.map (fun f -> f.name, f.path)

        member _.ShouldUseEmbedded = not doesWorkspaceContainVanillaDirectory

        member _.ConvertPathToLogicalPath(path: string) =
            FileManagerHelper.ConvertPathToLogicalPath(path, expandedRootDirectories, scriptFolders)

        member _.GetScopeForPath(path: string) = getScopeForPath path
