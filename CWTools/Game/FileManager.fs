namespace CWTools.Games

open System
open System.IO
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
          files: (string * string) list }

    type WorkspaceDirectoryInput =
        | WD of WorkspaceDirectory
        | ZD of ZippedDirectory

    type FileManager
        (
            inputDirectories: WorkspaceDirectoryInput list,
            embeddedFolder: string option,
            scriptFolders: string list,
            gameDirName: string,
            encoding: System.Text.Encoding,
            ignoreGlobList: string list,
            maxFileSizeMB: int
        ) =
        let rootDirectories, zippedDirectories =
            inputDirectories
            |> List.choose (function
                | WD wd -> Some wd
                | _ -> None),
            inputDirectories
            |> List.choose (function
                | ZD zd -> Some zd
                | _ -> None)



        let allDirsBelowRoot (workspaceDir: WorkspaceDirectory) =
            if Directory.Exists workspaceDir.path then
                getAllFoldersUnion [| workspaceDir.path |]
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

            let getMultipleModDirectory (workspaceDir: WorkspaceDirectory) : ModInfo list =
                let getModFiles modDir =
                    Directory.EnumerateFiles(modDir, "*.mod")
                    |> List.ofSeq
                    |> List.map CKParser.parseFile
                    |> List.choose (function
                        | Success(p, _, _) -> Some p
                        | _ -> None)
                    |> List.map (
                        (ProcessCore.processNodeBasic "mod" range.Zero)
                        >> (fun s -> s.TagText "name", "../" + (s.TagText "path"))
                    )

                let modFolder =
                    allDirsBelowRoot workspaceDir
                    |> Array.tryFind (fun (_, folder) -> folder == "mod" || folder == "mods")

                match modFolder with
                | None -> []
                | Some(mf, _) ->
                    let modFoldersFromDot =
                        getModFiles mf
                        |> List.map (fun (n, p) ->
                            { ModInfo.name = n
                              path = Path.Combine(workspaceDir.path, p) })

                    let modFoldersFromRoot =
                        Directory.EnumerateDirectories mf
                        |> List.ofSeq
                        |> List.filter (fun folder ->
                            Directory.EnumerateDirectories folder
                            |> List.ofSeq
                            |> List.exists checkFolderLooksLikeGameFolder)
                        |> List.map (fun folder ->
                            { ModInfo.name = Path.GetFileName folder
                              path = folder })

                    (modFoldersFromDot @ modFoldersFromRoot) |> List.distinct

            let checkIsGameFolder (workspaceDir: WorkspaceDirectory) =
                let foundAnyFolders =
                    Directory.EnumerateDirectories workspaceDir.path
                    |> Seq.exists checkFolderLooksLikeGameFolder

                foundAnyFolders

            match
                isVanillaDirectory workspaceDir, getMultipleModDirectory workspaceDir, checkIsGameFolder workspaceDir
            with
            | true, _, _ -> DirectoryType.Vanilla
            | _, _, true -> DirectoryType.Mod
            | false, [], false -> DirectoryType.Unknown
            | _, ms, _ -> DirectoryType.MultipleMod ms


        let expandedRootDirectories =
            let expanded =
                rootDirectories
                |> List.map (fun rd ->
                    let normalisedPath = rd.path.Replace("\\", "/").TrimStart('.')

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
                      normalisedPath = e.Replace("\\", "/").TrimStart('.') })

            match embeddedDir with
            | Some ed -> ed :: expanded
            | None -> expanded

        do
            logInfo "Workspace roots:"

            rootDirectories
            |> List.iter (fun rd -> logInfo (sprintf "root %s, exists: %b" rd.path (Directory.Exists rd.path)))

            logInfo (sprintf "embedded folder %A" embeddedFolder)

            expandedRootDirectories
            |> List.iter (fun rd -> log (sprintf "normalised %s" rd.normalisedPath))


        let allFoldersInWorkspaceDir (workspaceDir: ExpandedWorkspaceDirectory) =
            let allFoldersInDirectory (workspaceDir: ExpandedWorkspaceDirectory) =
                match workspaceDir.dirType with
                | Mod ->
                    [ { ModInfo.name = workspaceDir.name
                        path = workspaceDir.path } ]
                | Vanilla ->
                    [ { ModInfo.name = "vanilla"
                        path = workspaceDir.path } ]
                | MultipleMod mods -> mods
                | Unknown -> []

            workspaceDir
            |> allFoldersInDirectory
            |> List.filter (fun d -> Directory.Exists d.path)

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
            let excludeGlobTest =
                let globs = ignoreGlobList |> List.map Glob.Parse
                (fun (path: string) -> globs |> List.exists (fun g -> g.IsMatch(path)))

            let getAllFiles (modInfo: ModInfo) =
                //log "%A" path
                scriptFolders
                |> List.map (
                    (fun folder -> modInfo.name, Path.GetFullPath(Path.Combine(modInfo.path, folder)))
                    >> (fun (scope, folder) ->
                        scope,
                        folder,
                        (if Directory.Exists folder then
                             Directory.EnumerateFiles(folder, "*", SearchOption.AllDirectories)
                         else
                             Seq.empty)
                        |> List.ofSeq)
                )
                |> List.collect (fun (scope, _, files) -> files |> List.map (fun f -> scope, f))
                |> List.distinct
                |> List.filter (fun (_, f) -> f |> excludeGlobTest |> not)

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
                        |> List.ofSeq)
                    "Load files"

            allFiles

        let allFilesInZips =
            let zippedDirToResourceInputs (zd: ZippedDirectory) =
                let normalisedPath = zd.path.Replace("\\", "/").TrimStart('.')

                zd.files
                |> List.choose (fun (fn, ft) ->
                    fileToResourceInput normalisedPath normalisedPath.Length (zd.name, fn) 0L (fun _ -> ft))

            zippedDirectories |> List.collect zippedDirToResourceInputs

        let doesWorkspaceContainVanillaDirectory =
            rootDirectories |> List.exists isVanillaDirectory

        let getScopeForPath (path: string) =
            let path = path.Replace("\\", "/")

            expandedRootDirectories
            |> List.tryPick (fun rd ->
                let index = path.IndexOf(rd.normalisedPath, StringComparison.Ordinal)
                if index >= 0 then Some rd.name else None)

        member __.AllFilesByPath() =
            (expandedRootDirectories |> List.collect allFilesByPath) @ allFilesInZips

        member __.AllFolders() =
            expandedRootDirectories
            |> List.collect allFoldersInWorkspaceDir
            |> List.map (fun f -> f.name, f.path)

        member __.ShouldUseEmbedded = not doesWorkspaceContainVanillaDirectory

        member __.ConvertPathToLogicalPath(path: string) =
            FileManagerHelper.ConvertPathToLogicalPath(path, expandedRootDirectories, scriptFolders)

        member __.GetScopeForPath(path: string) = getScopeForPath path
