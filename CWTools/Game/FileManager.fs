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

    type ExpandedWorkspaceDirectory = {
        path : string
        name : string
        dirType : DirectoryType
        normalisedPath : string
        normalisedPathLength : int
    }

    type FileManager(rootDirectories : WorkspaceDirectory list, embeddedFolder : string option, scriptFolders : string list, gameDirName : string, encoding : System.Text.Encoding, ignoreGlobList : string list) =
        let excludeGlobTest =
            let globs = ignoreGlobList |> List.map Glob.Parse
            (fun (path : string) ->
                globs |> List.exists (fun g -> (g.IsMatch(path)))
                )

        let rec getAllFolders dirs =
            if Seq.isEmpty dirs then Seq.empty else
                seq { yield! dirs |> Seq.collect Directory.EnumerateDirectories
                      yield! dirs |> Seq.collect Directory.EnumerateDirectories |> getAllFolders }
        let getAllFoldersUnion dirs =
            seq {
                yield! dirs
                yield! getAllFolders dirs
            }

        do logNormal ("Workspace roots:")
        do rootDirectories |> List.iter (fun rd -> logNormal (sprintf "root %s, exists: %b" rd.path (Directory.Exists rd.path)))
        do logNormal (sprintf "embedded folder %A" embeddedFolder)
        // do logNormal (sprintf "Workspace root is %s, exists: %b" rootDirectory (Directory.Exists rootDirectory))
        let allDirsBelowRoot (workspaceDir : WorkspaceDirectory) = if Directory.Exists workspaceDir.path then getAllFoldersUnion [workspaceDir.path] |> List.ofSeq |> List.map(fun folder -> folder, Path.GetFileName folder) else []
        let isVanillaDirectory (workspaceDir : WorkspaceDirectory) =
            let dir =
                allDirsBelowRoot workspaceDir
                |> List.tryFind (fun (_, folder) -> folder.ToLower() = gameDirName || folder.ToLower() = "game" || folder.ToLower() = gameDirName.Replace(" ", "_"))
                |> Option.map (fst)
                |> Option.bind (fun f -> if Directory.Exists (f + (string Path.DirectorySeparatorChar) + "common") then Some f else None)
            dir.IsSome
            // match dir with
            // |Some s -> logNormal (sprintf "Found %s directory at %s" gameDirName s)
            // |None -> logNormal (sprintf "Couldn't find vanilla %s directory, falling back to embedded vanilla files" gameDirName)
        let checkFolderLooksLikeGameFolder = (fun (f : string) -> let f = Path.GetFileName f in  f = "common" || f = "events" || f = "interface" || f = "gfx" || f = "localisation")
        let getMultipleModDirectory (workspaceDir : WorkspaceDirectory) : ModInfo list =
            let getModFiles modDir =
                Directory.EnumerateFiles (modDir)
                        |> List.ofSeq
                        |> List.filter (fun f -> Path.GetExtension(f) = ".mod")
                        |> List.map CKParser.parseFile
                        |> List.choose ( function | Success(p, _, _) -> Some p | _ -> None )
                        |> List.map ((ProcessCore.processNodeBasic "mod" range.Zero) >> (fun s -> s.TagText "name", "../" + (s.TagText "path")))


            // let dotModFiles = (workspaceDir.path, Path.GetFileName rootDirectory)::modDirs |> List.collect (fst >> getModFiles)

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

        //let normalisedScopeDirectory = rootDirectory.Replace("\\","/").TrimStart('.')
        let checkIsGameFolder (workspaceDir : WorkspaceDirectory) =
            let foundAnyFolders = Directory.EnumerateDirectories workspaceDir.path |> List.ofSeq |> List.exists checkFolderLooksLikeGameFolder
            foundAnyFolders

        let classifyDirectory (workspaceDir : WorkspaceDirectory) =
            match isVanillaDirectory workspaceDir, getMultipleModDirectory workspaceDir, checkIsGameFolder workspaceDir with
            | true, _, _ -> DirectoryType.Vanilla
            | _, _, true -> DirectoryType.Mod
            | false, [], false -> DirectoryType.Unknown
            | _, ms, _ -> DirectoryType.MultipleMod ms
            // let modDirs = allDirsBelowRoot workspaceDirectory |> List.filter(fun (_, folder) -> folder.ToLower() = "mod" || folder.ToLower() = "mods")
            //     match modDirs with
            //     | [] -> logNormal (sprintf "%s" "Didn't find any mod directories")
            //     | x ->
            //         log (sprintf "Found %i mod directories:" x.Length)
            //         x |> List.iter (fun d -> log (sprintf "%s" (fst d)))
        let doesWorkspaceContainVanillaDirectory = rootDirectories |> List.exists isVanillaDirectory

        let embeddedDir = embeddedFolder |> Option.map (fun e ->
            {
                path = e
                name = "embedded"
                dirType = Unknown
                normalisedPath = e.Replace("\\","/").TrimStart('.')
                normalisedPathLength = e.Replace("\\","/").TrimStart('.').Length
            }
            )
        let expandedRootDirectories =
            rootDirectories |> List.map (fun rd ->
                                    let normalisedPath = rd.path.Replace("\\","/").TrimStart('.')
                                    {
                                        path = rd.path
                                        name = rd.name
                                        dirType = classifyDirectory rd
                                        normalisedPath = normalisedPath
                                        normalisedPathLength = normalisedPath.Length
                                    })
        let expandedRootDirectories = if embeddedDir.IsSome then embeddedDir.Value::expandedRootDirectories else expandedRootDirectories
        // let rootDirectories = rootDirectories |> List.map (fun rd -> {| rd with normalisedPath = rd.path.Replace("\\","/").TrimStart('.') |})
        // let rootDirectories = rootDirectories |> List.map (fun rd -> {| rd with normalisedScopeDirectoryLength = rd.normalisedPath.Length |})
        do expandedRootDirectories |> List.iter (fun rd -> log (sprintf "normalised %s" rd.normalisedPath))
        // do log (sprintf "normalised %s" normalisedScopeDirectory)
        // let normalisedScopeDirectoryLength = normalisedScopeDirectory.Length
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
        let getScopeForPath (path : string) =
            let path = path.Replace("\\","/")
            expandedRootDirectories
                        |> List.tryPick (fun rd ->
                                            let index = path.IndexOf(rd.normalisedPath)
                                            if index >= 0 then Some (rd.name)
                                            else None)
            // dir
        // let mods =
        //     match dotModFiles with
        //     | [] -> logNormal (sprintf "%s" "Didn't find any mods in subfolders")
        //     | x ->
        //         logNormal (sprintf "Found %i mods:" x.Length)
        //         x |> List.iter (fun (n, p, _) -> (logNormal (sprintf "%s, %s" n p)))
        //     dotModFiles |> List.distinct
        // let modFolders =
        //     let folders = mods |> List.filter (fun (n, _, _) -> modFilter |> function |None -> true |Some mf -> n.Contains(mf))
        //                         |> List.map (fun (n, p, r) -> if Path.IsPathRooted p then n, p else n, (Directory.GetParent(r).FullName + (string Path.DirectorySeparatorChar) + p))
        //     log (sprintf "Mod folders")
        //     folders |> List.iter (fun (n, f) -> log (sprintf "%s, %s" n f))
        //     folders


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
            // match isVanillaDirectory workspaceDir with
            // |None ->
            //     // if modFolders.Length > 0 then modFolders
            //     // else
            //         if modFolders.Length = 0 then logNormal (sprintf "Couldn't find the game directory or any mods") else ()
            //         let checkIsGameFolder = (fun (f : string) -> f.Contains "common" || f.Contains "events" || f.Contains "interface" || f.Contains "gfx" || f.Contains "localisation")
            //         let foundAnyFolders = Directory.EnumerateDirectories workspaceDir.path |> List.ofSeq |> List.exists checkIsGameFolder
            //         match foundAnyFolders with
            //         | true ->
            //             logNormal (sprintf "I think you opened a mod folder directly")
            //             (Path.GetFileName workspaceDir.path, workspaceDir.path)::modFolders
            //         | false ->
            //             logNormal (sprintf "I don't think you opened a mod folder directly")
            //             modFolders
            // |Some s -> ("vanilla", s) :: (modFolders)
        let allFoldersInWorkspace =
            expandedRootDirectories |> List.collect allFoldersInDirectory |> List.filter (fun d -> Directory.Exists d.path )
        let allFilesByPath (workspaceDir : ExpandedWorkspaceDirectory) =
            let getAllFiles (modInfo : ModInfo) =
                //log "%A" path
                scriptFolders
                        |> List.map ((fun folder -> modInfo.name, Path.GetFullPath(Path.Combine(modInfo.path, folder)))
                        >> (fun (scope, folder) -> scope, folder, (if Directory.Exists folder then getAllFoldersUnion [folder] |> Seq.collect Directory.EnumerateFiles else Seq.empty )|> List.ofSeq))
                        |> List.collect (fun (scope, _, files) -> files |> List.map (fun f -> scope, f))
                        |> List.distinct
                        |> List.filter (fun (_, f) -> f |> excludeGlobTest |> not)
            let fileToResourceInput (scope, filepath : string) =
                match Path.GetExtension(filepath) with
                |".txt"
                |".gui"
                |".gfx"
                |".sfx"
                |".asset" ->
                    let rootedpath = filepath.Substring(filepath.IndexOf(workspaceDir.normalisedPath) + (workspaceDir.normalisedPathLength) + 1)
                    if (filepath |> FileInfo).Length > 2000000L then None else
                    Some (EntityResourceInput { scope = scope; filepath = filepath; logicalpath = (convertPathToLogicalPath rootedpath); filetext = File.ReadAllText(filepath, encoding); validate = true})
                |".dds"
                |".tga"
                |".shader"
                |".lua"
                |".png"
                |".mesh"
                |".ttf"
                |".otf" ->
                    let rootedpath = filepath.Substring(filepath.IndexOf(workspaceDir.normalisedPath) + (workspaceDir.normalisedPathLength) + 1)
                    Some (FileResourceInput { scope = scope; filepath = filepath; logicalpath = convertPathToLogicalPath rootedpath })
                |".yml"
                |".csv" ->
                    let rootedpath = filepath.Substring(filepath.IndexOf(workspaceDir.normalisedPath) + (workspaceDir.normalisedPathLength) + 1)
                    Some (FileWithContentResourceInput { scope = scope; filepath = filepath; logicalpath = convertPathToLogicalPath rootedpath; filetext = File.ReadAllText(filepath, encoding); validate = true})
                |_ -> None
            let allFiles = duration (fun _ -> PSeq.map getAllFiles (allFoldersInDirectory workspaceDir) |> PSeq.collect id |> PSeq.choose fileToResourceInput |> List.ofSeq ) "Load files"
            allFiles
        let locFolders (modInfo : ModInfo) =
            Directory.EnumerateDirectories modInfo.path
                    |> List.ofSeq
                    |> List.filter(fun (folder) -> let folder = Path.GetFileName folder in folder.ToLower() = "localisation" || folder.ToLower() = "localisation_synced")
        let allLocFolders = allFoldersInWorkspace |> List.collect (locFolders)
        member __.AllFilesByPath() = expandedRootDirectories |> List.collect allFilesByPath
        member __.AllFolders() = allFoldersInWorkspace |> List.map (fun f -> f.name, f.path)
        member __.LocalisationFiles() = locFolders
        member __.ShouldUseEmbedded = not doesWorkspaceContainVanillaDirectory
        // member __.ScopeDirectory = normalisedScopeDirectory
        member __.ConvertPathToLogicalPath(path : string) = convertPathToLogicalPath path
        member __.GetScopeForPath(path : string) = getScopeForPath path