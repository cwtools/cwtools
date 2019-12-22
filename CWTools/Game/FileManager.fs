namespace CWTools.Games
open System.IO
open CWTools.Parser
open CWTools.Process
open CWTools.Utilities.Position
open CWTools.Utilities.Utils
open FSharp.Collections.ParallelSeq
open FParsec
open DotNet.Globbing

module Files =


    type FilesScope =
        |All
        |Mods
        |Vanilla
    type ModInfo = {
        name : string
        path : string
    }
    type DirectoryType =
        |Vanilla
        |MultipleMod of ModInfo list
        |Mod
        |Unknown
    //TODO: normalised rootDirectory.Replace("\\","/").TrimStart('.')
    type WorkspaceDirectory = {
        path : string
        name : string
    }
    type ZippedDirectory = {
        path : string
        name : string
        files : (string * string) list
    }
    type WorkspaceDirectoryInput =
    | WD of WorkspaceDirectory
    | ZD of ZippedDirectory

    type ExpandedWorkspaceDirectory = {
        path : string
        name : string
        dirType : DirectoryType
        normalisedPath : string
        normalisedPathLength : int
    }

    type FileManager(inputDirectories : WorkspaceDirectoryInput list, embeddedFolder : string option, scriptFolders : string list, gameDirName : string, encoding : System.Text.Encoding, ignoreGlobList : string list, maxFileSizeMB : int) =
        let rootDirectories, zippedDirectories =
            inputDirectories |> List.choose (function | WD wd -> Some wd |_ -> None),
            inputDirectories |> List.choose (function | ZD zd -> Some zd |_ -> None)


        let getAllFoldersUnion dirs =
            let rec getAllFolders dirs =
                if Seq.isEmpty dirs then Seq.empty else
                    seq { yield! dirs |> Seq.collect Directory.EnumerateDirectories
                          yield! dirs |> Seq.collect Directory.EnumerateDirectories |> getAllFolders }
            seq {
                yield! dirs
                yield! getAllFolders dirs
            }

        let allDirsBelowRoot (workspaceDir : WorkspaceDirectory) = if Directory.Exists workspaceDir.path then getAllFoldersUnion [workspaceDir.path] |> List.ofSeq |> List.map(fun folder -> folder, Path.GetFileName folder) else []

        let isVanillaDirectory (workspaceDir : WorkspaceDirectory) =
            let dir =
                allDirsBelowRoot workspaceDir
                |> List.tryFind (fun (_, folder) -> folder.ToLower() = gameDirName || folder.ToLower() = "game" || folder.ToLower() = gameDirName.Replace(" ", "_"))
                |> Option.map (fst)
                |> Option.bind (fun f -> if Directory.Exists (f + (string Path.DirectorySeparatorChar) + "common") then Some f else None)
            dir.IsSome


        let classifyDirectory (workspaceDir : WorkspaceDirectory) =
            let checkFolderLooksLikeGameFolder = (fun (f : string) -> let f = Path.GetFileName f in  f = "common" || f = "events" || f = "interface" || f = "gfx" || f = "localisation")
            let getMultipleModDirectory (workspaceDir : WorkspaceDirectory) : ModInfo list =
                let getModFiles modDir =
                    Directory.EnumerateFiles (modDir)
                            |> List.ofSeq
                            |> List.filter (fun f -> Path.GetExtension(f) = ".mod")
                            |> List.map CKParser.parseFile
                            |> List.choose ( function | Success(p, _, _) -> Some p | _ -> None )
                            |> List.map ((ProcessCore.processNodeBasic "mod" range.Zero) >> (fun s -> s.TagText "name", "../" + (s.TagText "path")))

                let modFolder = allDirsBelowRoot workspaceDir |> List.tryFind(fun (_, folder) -> folder.ToLower() = "mod" || folder.ToLower() = "mods")
                match modFolder with
                | None -> []
                | Some (mf, _) ->
                    let modFoldersFromDot = getModFiles mf |> List.map (fun (n, p) -> { ModInfo.name = n; path = Path.Combine(workspaceDir.path, p) })
                    let modFoldersFromRoot =
                        Directory.EnumerateDirectories mf |> List.ofSeq
                                                          |> List.filter (fun folder -> Directory.EnumerateDirectories folder |> List.ofSeq |> List.exists checkFolderLooksLikeGameFolder)
                                                          |> List.map (fun folder -> { ModInfo.name = Path.GetFileName folder; path = folder})
                    (modFoldersFromDot @ modFoldersFromRoot) |> List.distinct

            let checkIsGameFolder (workspaceDir : WorkspaceDirectory) =
                let foundAnyFolders = Directory.EnumerateDirectories workspaceDir.path |> List.ofSeq |> List.exists checkFolderLooksLikeGameFolder
                foundAnyFolders
            match isVanillaDirectory workspaceDir, getMultipleModDirectory workspaceDir, checkIsGameFolder workspaceDir with
            | true, _, _ -> DirectoryType.Vanilla
            | _, _, true -> DirectoryType.Mod
            | false, [], false -> DirectoryType.Unknown
            | _, ms, _ -> DirectoryType.MultipleMod ms


        let expandedRootDirectories =
            let expanded =
                rootDirectories |> List.map (fun rd ->
                                        let normalisedPath = rd.path.Replace("\\","/").TrimStart('.')
                                        {
                                            path = rd.path
                                            name = rd.name
                                            dirType = classifyDirectory rd
                                            normalisedPath = normalisedPath
                                            normalisedPathLength = normalisedPath.Length
                                        })

            let embeddedDir = embeddedFolder |> Option.map (fun e ->
                {
                    path = e
                    name = "embedded"
                    dirType = Unknown
                    normalisedPath = e.Replace("\\","/").TrimStart('.')
                    normalisedPathLength = e.Replace("\\","/").TrimStart('.').Length
                }
                )
            match embeddedDir with
            | Some ed -> ed::expanded
            | None -> expanded

        do
            logNormal ("Workspace roots:")
            rootDirectories |> List.iter (fun rd -> logNormal (sprintf "root %s, exists: %b" rd.path (Directory.Exists rd.path)))
            logNormal (sprintf "embedded folder %A" embeddedFolder)
            expandedRootDirectories |> List.iter (fun rd -> log (sprintf "normalised %s" rd.normalisedPath))


        let allFoldersInWorkspaceDir (workspaceDir : ExpandedWorkspaceDirectory) =
            let allFoldersInDirectory (workspaceDir : ExpandedWorkspaceDirectory) =
                match workspaceDir.dirType with
                | Mod ->
                    [{ ModInfo.name = workspaceDir.name; path = workspaceDir.path}]
                | Vanilla ->
                    [{ ModInfo.name = "vanilla"; path = workspaceDir.path }]
                | MultipleMod mods ->
                    mods
                | Unknown ->
                    []
            workspaceDir |> allFoldersInDirectory |> List.filter (fun d -> Directory.Exists d.path )

        let convertPathToLogicalPath =
            fun (path : string) ->
                let path = path.Replace("\\","/")
                // log "conv %A" path
                let checkDirectories (pathToCheck : string) =
                    expandedRootDirectories
                        |> List.tryPick (fun rd ->
                                            let index = pathToCheck.IndexOf(rd.normalisedPath)
                                            if index >= 0 then Some (pathToCheck.Substring(index + rd.normalisedPathLength))
                                            else None) |> Option.defaultValue pathToCheck
                let path = checkDirectories path
                // let path = let index = path.IndexOf(normalisedScopeDirectory) in if index >= 0 then path.Substring(index + normalisedScopeDirectoryLength) else path
                // log "conv2 %A" path
                //let path = if path.Contains(normalisedScopeDirectory) then path.Replace(normalisedScopeDirectory+"/", "") else path
                if path.StartsWith "gfx\\" || path.StartsWith "gfx/" then path else
                let pathContains (part : string) =
                    path.Contains ("/"+part+"/" )|| path.Contains( "\\"+part+"\\")
                let pathIndex (part : string) =
                    let i = if path.IndexOf ("/"+part+"/" ) < 0 then path.IndexOf("\\"+part+"\\") else path.IndexOf ("/"+part+"/" )
                    i + 1
                let matches =
                    [ for s in scriptFolders do if pathContains s then let i = pathIndex s in yield i, path.Substring(i) else () ]
                if matches.IsEmpty then path else matches |> List.minBy fst |> snd

        let fileToResourceInput (normalisedPath : string) normalisedPathLength (scope, filepath : string) (fileLength : int64) (fileTextThunk : unit -> string) =
            match Path.GetExtension(filepath) with
            |".txt"
            |".gui"
            |".gfx"
            |".sfx"
            |".asset"
            |".map" ->
                let rootedpath = filepath.Substring(filepath.IndexOf(normalisedPath) + (normalisedPathLength) + 1)
                if fileLength > ((int64 maxFileSizeMB) * 1000000L) then None else
                Some (EntityResourceInput { scope = scope; filepath = filepath; logicalpath = (convertPathToLogicalPath rootedpath); filetext = fileTextThunk(); validate = true})
            |".dds"
            |".tga"
            |".shader"
            |".lua"
            |".png"
            |".mesh"
            |".ttf"
            |".otf"
            |".wav" ->
                let rootedpath = filepath.Substring(filepath.IndexOf(normalisedPath) + (normalisedPathLength) + 1)
                Some (FileResourceInput { scope = scope; filepath = filepath; logicalpath = convertPathToLogicalPath rootedpath })
            |".yml"
            |".csv" ->
                let rootedpath = filepath.Substring(filepath.IndexOf(normalisedPath) + (normalisedPathLength) + 1)
                Some (FileWithContentResourceInput { scope = scope; filepath = filepath; logicalpath = convertPathToLogicalPath rootedpath; filetext = fileTextThunk(); validate = true})
            |_ -> None
//File.ReadAllText(filepath, encoding
        let allFilesByPath (workspaceDir : ExpandedWorkspaceDirectory) =
            let excludeGlobTest =
                let globs = ignoreGlobList |> List.map Glob.Parse
                (fun (path : string) ->
                    globs |> List.exists (fun g -> (g.IsMatch(path)))
                    )

            let getAllFiles (modInfo : ModInfo) =
                //log "%A" path
                scriptFolders
                        |> List.map ((fun folder -> modInfo.name, Path.GetFullPath(Path.Combine(modInfo.path, folder)))
                        >> (fun (scope, folder) -> scope, folder, (if Directory.Exists folder then getAllFoldersUnion [folder] |> Seq.collect Directory.EnumerateFiles else Seq.empty )|> List.ofSeq))
                        |> List.collect (fun (scope, _, files) -> files |> List.map (fun f -> scope, f))
                        |> List.distinct
                        |> List.filter (fun (_, f) -> f |> excludeGlobTest |> not)
            let allFiles = duration (fun _ -> PSeq.map getAllFiles (allFoldersInWorkspaceDir workspaceDir)
                                                |> PSeq.collect id
                                                |> PSeq.choose (fun (scope, fn) -> (fileToResourceInput (workspaceDir.normalisedPath) (workspaceDir.normalisedPathLength) (scope,fn) (fn |> FileInfo).Length (fun _ -> File.ReadAllText(fn, encoding) )))
                                                |> List.ofSeq ) "Load files"
            allFiles
        let allFilesInZips =
            let zippedDirToResourceInputs (zd : ZippedDirectory) =
                let normalisedPath = zd.path.Replace("\\","/").TrimStart('.')
                zd.files |> List.choose (fun (fn, ft) -> fileToResourceInput normalisedPath normalisedPath.Length (zd.name, fn) 0L (fun _ -> ft))
            zippedDirectories |> List.collect zippedDirToResourceInputs

        let doesWorkspaceContainVanillaDirectory = rootDirectories |> List.exists isVanillaDirectory

        let getScopeForPath (path : string) =
            let path = path.Replace("\\","/")
            expandedRootDirectories
                        |> List.tryPick (fun rd ->
                                            let index = path.IndexOf(rd.normalisedPath)
                                            if index >= 0 then Some (rd.name)
                                            else None)

        member __.AllFilesByPath() = (expandedRootDirectories |> List.collect allFilesByPath) @ allFilesInZips
        member __.AllFolders() = expandedRootDirectories |> List.collect allFoldersInWorkspaceDir |> List.map (fun f -> f.name, f.path)
        member __.ShouldUseEmbedded = not doesWorkspaceContainVanillaDirectory
        member __.ConvertPathToLogicalPath(path : string) = convertPathToLogicalPath path
        member __.GetScopeForPath(path : string) = getScopeForPath path