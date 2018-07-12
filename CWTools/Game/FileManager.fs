namespace CWTools.Games
open System.IO
open CWTools.Parser
open CWTools.Process
open Microsoft.FSharp.Compiler.Range
open CWTools.Utilities.Utils
open FSharp.Collections.ParallelSeq
open FParsec

module Files =


    type FilesScope =
        |All
        |Mods
        |Vanilla

    type FileManager(rootDirectory : string, modFilter : string option, scope : FilesScope, scriptFolders : string list, gameDirName : string) =
        let normalisedScopeDirectory = rootDirectory.Replace("/","\\").TrimStart('.')
        let convertPathToLogicalPath =
            fun (path : string) ->
                let path = path.Replace("/","\\")
                let path = if path.Contains(normalisedScopeDirectory) then path.Replace(normalisedScopeDirectory+"\\", "") else path
                if path.StartsWith "gfx\\" || path.StartsWith "gfx//" then path else
                let pathContains (part : string) =
                    path.Contains ("/"+part+"/" )|| path.Contains( "\\"+part+"\\")
                let pathIndex (part : string) =
                    let i = if path.IndexOf ("/"+part+"/" ) < 0 then path.IndexOf("\\"+part+"\\") else path.IndexOf ("/"+part+"/" )
                    i + 1
                let matches =
                    [
                        if pathContains "common" then let i = pathIndex "common" in yield i, path.Substring(i) else ();
                        if pathContains "interface" then let i = pathIndex "interface" in yield i, path.Substring(i) else ();
                        if pathContains "gfx" then let i = pathIndex "gfx" in yield i, path.Substring(i) else ();
                        if pathContains "events" then let i = pathIndex "events" in yield i, path.Substring(i) else ();
                        if pathContains "localisation" then let i = pathIndex "localisation" in yield i, path.Substring(i) else ();
                        if pathContains "localisation_synced" then let i = pathIndex "localisation_synced" in yield i, path.Substring(i) else ();
                        if pathContains "map" then let i = pathIndex "map" in yield i, path.Substring(i) else ();
                        if pathContains "music" then let i = pathIndex "music" in yield i, path.Substring(i) else ();
                        if pathContains "fonts" then let i = pathIndex "fonts" in yield i, path.Substring(i) else ();
                        if pathContains "sound" then let i = pathIndex "sound" in yield i, path.Substring(i) else ();
                    ]
                if matches.IsEmpty then path else matches |> List.minBy fst |> snd

        let rec getAllFolders dirs =
            if Seq.isEmpty dirs then Seq.empty else
                seq { yield! dirs |> Seq.collect Directory.EnumerateDirectories
                      yield! dirs |> Seq.collect Directory.EnumerateDirectories |> getAllFolders }
        let getAllFoldersUnion dirs =
            seq {
                yield! dirs
                yield! getAllFolders dirs
            }
        do eprintfn "%s %b" rootDirectory (Directory.Exists rootDirectory)
        let allDirsBelowRoot = if Directory.Exists rootDirectory then getAllFoldersUnion [rootDirectory] |> List.ofSeq |> List.map(fun folder -> folder, Path.GetFileName folder) else []
        let stellarisDirectory =
            let dir = allDirsBelowRoot |> List.tryFind (fun (_, folder) -> folder.ToLower() = gameDirName) |> Option.map (fst) |> Option.bind (fun f -> if Directory.Exists (f + (string Path.DirectorySeparatorChar) + "common") then Some f else None)
            match dir with
            |Some s -> eprintfn "Found %s directory at %s" gameDirName s
            |None -> eprintfn "Couldn't find %s directory, falling back to embedded vanilla files" gameDirName
            dir
        let mods =
            let getModFiles modDir =
                Directory.EnumerateFiles (modDir)
                        |> List.ofSeq
                        |> List.filter (fun f -> Path.GetExtension(f) = ".mod")
                        |> List.map CKParser.parseFile
                        |> List.choose ( function | Success(p, _, _) -> Some p | _ -> None )
                        |> List.map ((ProcessCore.processNodeBasic "mod" range.Zero) >> (fun s -> s.TagText "name", s.TagText "path", modDir))


            let modDirs = allDirsBelowRoot |> List.filter(fun (_, folder) -> folder.ToLower() = "mod" || folder.ToLower() = "mods")
            match modDirs with
            | [] -> eprintfn "%s" "Didn't find any mod directories"
            | x ->
                eprintfn "Found %i mod directories:" x.Length
                x |> List.iter (fun d -> eprintfn "%s" (fst d))
            let dotModFiles = (rootDirectory, Path.GetFileName rootDirectory)::modDirs |> List.collect (fst >> getModFiles)
            match dotModFiles with
            | [] -> eprintfn "%s" "Didn't find any mods"
            | x ->
                eprintfn "Found %i mods:" x.Length
                x |> List.iter (fun (n, p, _) -> eprintfn "%s, %s" n p)
            dotModFiles |> List.distinct
        let modFolders =
            let folders = mods |> List.filter (fun (n, _, _) -> modFilter |> function |None -> true |Some mf -> n.Contains(mf))
                                |> List.map (fun (n, p, r) -> if Path.IsPathRooted p then n, p else n, (Directory.GetParent(r).FullName + (string Path.DirectorySeparatorChar) + p))
            eprintfn "Mod folders"
            folders |> List.iter (fun (n, f) -> eprintfn "%s, %s" n f)
            folders


        let allFolders =
            match stellarisDirectory, scope with
            |None, _ ->
                // if modFolders.Length > 0 then modFolders
                // else
                    if modFolders.Length = 0 then eprintfn "Couldn't find the game directory or any mods" else ()
                    let checkIsGameFolder = (fun (f : string) -> f.Contains "common" || f.Contains "events" || f.Contains "interface" || f.Contains "gfx" || f.Contains "localisation")
                    let foundAnyFolders = Directory.EnumerateDirectories rootDirectory |> List.ofSeq |> List.exists checkIsGameFolder
                    match foundAnyFolders with
                    | true ->
                        eprintfn "I think you opened a mod folder directly"
                        (Path.GetFileName rootDirectory, rootDirectory)::modFolders
                    | false ->
                        eprintfn "I don't think you opened a mod folder directly"
                        modFolders
            |Some s, All -> ("vanilla", s) :: (modFolders)
            |_, Mods -> (modFolders)
            |Some s, Vanilla -> ["vanilla", s]
        let locFolders = allDirsBelowRoot |> List.filter(fun (_, folder) -> folder.ToLower() = "localisation" || folder.ToLower() = "localisation_synced")

        let allFilesByPath =
            let getAllFiles (scope, path) =
                eprintfn "%A" path
                scriptFolders
                        |> List.map ((fun folder -> scope, Path.GetFullPath(Path.Combine(path, folder)))
                        >> (fun (scope, folder) -> scope, folder, (if Directory.Exists folder then getAllFoldersUnion [folder] |> Seq.collect Directory.EnumerateFiles else Seq.empty )|> List.ofSeq))
                        |> List.collect (fun (scope, _, files) -> files |> List.map (fun f -> scope, f))
            let fileToResourceInput (scope, filepath) =
                match Path.GetExtension(filepath) with
                |".txt"
                |".gui"
                |".gfx"
                |".asset" ->
                    let rootedpath = filepath.Substring(filepath.IndexOf(normalisedScopeDirectory) + (normalisedScopeDirectory.Length) + 1)
                    Some (EntityResourceInput { scope = scope; filepath = filepath; logicalpath = (convertPathToLogicalPath rootedpath); filetext = File.ReadAllText filepath; validate = true})
                |".dds"
                |".tga"
                |".shader"
                |".lua"
                |".png"
                |".mesh"
                |".ttf"
                |".otf" ->
                    let rootedpath = filepath.Substring(filepath.IndexOf(normalisedScopeDirectory) + (normalisedScopeDirectory.Length) + 1)
                    Some (FileResourceInput { scope = scope; filepath = filepath; logicalpath = convertPathToLogicalPath rootedpath })
                |_ -> None
            let allFiles = duration (fun _ -> PSeq.map getAllFiles allFolders |> PSeq.collect id |> PSeq.choose fileToResourceInput |> List.ofSeq ) "Load files"
            allFiles

        member __.AllFilesByPath() = allFilesByPath
        member __.AllFolders() = allFolders
        member __.LocalisationFiles() = locFolders
        member __.ShouldUseEmbedded = stellarisDirectory.IsNone
        member __.ScopeDirectory = normalisedScopeDirectory
        member __.ConvertPathToLogicalPath(path : string) = convertPathToLogicalPath path