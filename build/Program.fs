
open System
open System.IO
open System.Diagnostics
open Fake.Core
open Fake.DotNet
open Fake.JavaScript
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Tools.Git
open Fake.Api
open System.Text.Json
open System
open FSharp.Collections.ParallelSeq

// --------------------------------------------------------------------------------------
// Configuration
// --------------------------------------------------------------------------------------


// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "cwtools"
let gitHome = "https://github.com/" + gitOwner

// The name of the project on GitHub
let gitName = "cwtools-vscode"


// Read additional information from the release notes document
let releaseNotesData = File.ReadAllLines "CHANGELOG.md" |> ReleaseNotes.parseAll

let release = List.head releaseNotesData

let githubToken = Environment.environVarOrNone "GITHUB_TOKEN"
// open Fake.BuildServer

// BuildServer.install [ GitLab.Installer ]

let run cmd args dir =
    let parms =
        { ExecParams.Empty with
            Program = cmd
            WorkingDir = dir
            CommandLine = args }

    if Process.shellExec parms <> 0 then
        failwithf "Error while running '%s' with args: %s" cmd args

let platformTool tool path =
    match Environment.isUnix with
    | true -> tool
    | _ ->
        match ProcessUtils.tryFindFileOnPath path with
        | None -> failwithf "can't find tool %s on PATH" tool
        | Some v -> v

let npxTool = lazy (platformTool "npx" "npx.cmd")

let vsceTool = lazy (platformTool "@vscode/vsce" "vsce.cmd")


let releaseBin      = "release/bin"
let fsacBin         = "paket-files/github.com/fsharp/FsAutoComplete/bin/release"

let releaseBinNetcore = releaseBin + "_netcore"
let fsacBinNetcore = fsacBin + "_netcore"

let cwtoolsPath = ""
let cwtoolsProjectName = "Main.fsproj"
let cwtoolsLinuxProjectName = "Main.Linux.fsproj"

// --------------------------------------------------------------------------------------
// Build the Generator project and run it
// --------------------------------------------------------------------------------------
let copyBin releaseDir =
    Directory.ensure releaseDir
    Shell.copyDir (releaseDir </> "bin") "out" (fun _ -> true)

let buildPackage dir =
    Process.killAllByName "npx"
    run npxTool.Value "@vscode/vsce package" dir

    !!(sprintf "%s/*.vsix" dir) |> Seq.iter (Shell.moveFile "./temp/")

let setPackageJsonField (name: string) (value: string) releaseDir =
    let fileName = sprintf "./%s/package.json" releaseDir
    let content = File.readAsString fileName
    let jsonObj = System.Text.Json.JsonDocument.Parse content
    let node = System.Text.Json.Nodes.JsonObject.Create jsonObj.RootElement
    node[name] <- value
    let opts = JsonSerializerOptions(WriteIndented = true, AllowTrailingCommas = false)
    File.WriteAllText(fileName, node.ToJsonString(opts))

let setVersion (release: ReleaseNotes.ReleaseNotes) releaseDir =
    let versionString = $"%O{release.NugetVersion}"
    setPackageJsonField "version" versionString releaseDir

let publishToGallery releaseDir =
    let token =
        match Environment.environVarOrDefault "vsce-token" "" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> UserInput.getUserPassword "VSCE Token: "

    Process.killAllByName "npx"
    run npxTool.Value (sprintf "@vscode/vsce publish --pat %s" token) releaseDir

let ensureGitUser user email =
    match Fake.Tools.Git.CommandHelper.runGitCommand "." "config user.name" with
    | true, [ username ], _ when username = user -> ()
    | _, _, _ ->
        Fake.Tools.Git.CommandHelper.directRunGitCommandAndFail "." (sprintf "config user.name %s" user)
        Fake.Tools.Git.CommandHelper.directRunGitCommandAndFail "." (sprintf "config user.email %s" email)

let releaseGithub (release: ReleaseNotes.ReleaseNotes) =
    let user =
        match Environment.environVarOrDefault "github-user" "" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> UserInput.getUserInput "Username: "

    let email =
        match Environment.environVarOrDefault "user-email" "" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> UserInput.getUserInput "Email: "

    let remote =
        CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function
            | None -> gitHome + "/" + gitName
            | Some(s: string) -> s.Split().[0]

    Staging.stageAll ""
    ensureGitUser user email
    Commit.exec "." (sprintf "Bump version to %s" release.NugetVersion)
    Branches.pushBranch "" remote "main"
    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion

    let files = !!("./temp" </> "*.vsix")

    let token =
        match githubToken with
        | Some s -> s
        | _ ->
            failwith
                "please set the github_token environment variable to a github personal access token with repo access."

    // release on github
    let cl =
        GitHub.createClientWithToken token
        |> GitHub.draftNewRelease
            gitOwner
            gitName
            release.NugetVersion
            (release.SemVer.PreRelease <> None)
            release.Notes

    (cl, files)
    ||> Seq.fold (fun acc e -> acc |> GitHub.uploadFile e)
    |> GitHub.publishDraft //releaseDraft
    |> Async.RunSynchronously
let initTargets () =

    Target.create "Clean" (fun _ ->
        Shell.cleanDir "./temp"
        Shell.cleanDir "./out"
        Shell.cleanDir "./release/bin"
        Shell.copyFiles "release" [ "README.md"; "LICENSE.md" ]
        Shell.copyFile "release/CHANGELOG.md" "CHANGELOG.md")

    Target.create "NpmInstall" <| fun _ ->
        Npm.install id

    Target.create "PackageNpmInstall" <| fun _ -> Npm.install (fun p -> { p with WorkingDirectory = "release" })

    Target.create "DotNetRestore" <| fun _ ->
        DotNet.restore (fun p -> { p with Common = { p.Common with WorkingDirectory = "src/Main" }} ) cwtoolsProjectName
        DotNet.restore (fun p -> { p with Common = { p.Common with WorkingDirectory = "src/Main" }} ) cwtoolsLinuxProjectName

    Target.create "CopyDocs" (fun _ ->
        Shell.copyFiles "release" [ "README.md"; "LICENSE.md" ]
        Shell.copyFile "release/CHANGELOG.md" "CHANGELOG.md")

    let publishParams (outputDir : string) (framework : string) (release : bool) =
        (fun (p : DotNet.PublishOptions) ->
            { p with
                Common =
                    {
                        p.Common with
                            WorkingDirectory = "src/Main"
                            CustomParams = Some ("--self-contained true" + (if release then " /p:PublishReadyToRun=true" else " /p:LinkDuringPublish=false"))
                    }
                OutputPath = Some ("../.." </> outputDir </> framework)
                Runtime = Some framework
                Configuration = DotNet.BuildConfiguration.Release
                MSBuildParams = { MSBuild.CliArguments.Create() with DisableInternalBinLog = true }
            })

    let buildParams (release : bool) (local : bool) =
        let args = ((if release then "" else " /p:LinkDuringPublish=false"))
        let args = if local then args + " /p:LocalPaket=True" else args
        (fun (b : DotNet.BuildOptions) ->
            { b with
                Common =
                    {
                        b.Common with
                            WorkingDirectory = "src/Main"
                            CustomParams = Some args
                    }
                OutputPath = Some ("../../out/server/local")
                Configuration = if release  then DotNet.BuildConfiguration.Release else DotNet.BuildConfiguration.Debug
                MSBuildParams = { MSBuild.CliArguments.Create() with DisableInternalBinLog = true }
            })

    Target.create "BuildServer" <| fun _ ->
        DotNet.build (buildParams true false) cwtoolsProjectName

    Target.create "BuildServerLocal" <| fun _ ->
        DotNet.build (buildParams true true) cwtoolsProjectName

    // Target.create "BuildServer" <| fun _ ->
    //     match Environment.isWindows with
    //     |true -> DotNet.publish (publishParams "win-x64" false) cwtoolsProjectName
    //     |false -> DotNet.publish (publishParams "linux-x64" false) cwtoolsLinuxProjectName
    //     // DotNetCli.Publish (fun p -> {p with WorkingDir = "src/Main"; AdditionalArgs = ["--self-contained"; "true"; "/p:LinkDuringPublish=false"]; Output = "../../out/server/win-x64"; Runtime = "win-x64"; Configuration = "Release"})
    //     // DotNet.publish (publishParams "linux-x64" false) cwtoolsProjectName //(fun p -> {p with Common = { p.Common with WorkingDirectory = "src/Main"; CustomParams = Some "--self-contained true /p:LinkDuringPublish=false";}; OutputPath = Some "../../out/server/linux-x64"; Runtime =  Some "linux-x64"; Configuration = DotNet.BuildConfiguration.Release }) cwtoolsProjectName

    Target.create "PublishServer" <| fun _ ->
        DotNet.publish (publishParams "out/server"  "win-x64" true) cwtoolsProjectName
        DotNet.publish (publishParams "out/server" "linux-x64" true) cwtoolsLinuxProjectName
        DotNet.publish (publishParams "out/server" "osx-x64" true) cwtoolsProjectName

    Target.create "BuildClient" (fun _ ->
        match ProcessUtils.tryFindFileOnPath "npx" with
        |Some tsc ->
            CreateProcess.fromRawCommand tsc ["tsc"; "-p"; "./tsconfig.extension.json"]
            |> Proc.run
            |> (fun r -> if r.ExitCode <> 0 then failwith "tsc fail")
        // |Some tsc -> Process.directExec (fun (p : ProcStartInfo) -> p.WithFileName(tsc).WithArguments("tsc -p ./tsconfig.extension.json"))
        |_ -> failwith "didn't find tsc"
        match ProcessUtils.tryFindFileOnPath "npx" with
        |Some tsc ->
            CreateProcess.fromRawCommand tsc ["rollup"; "-c"; "-o"; "./out/client/webview/graph.js"]
            |> Proc.run
            |> (fun r -> if r.ExitCode <> 0 then failwith "rollup fail")
        |_ -> failwith "didn't find rollup"
        // Process.directExec (fun (p : ProcStartInfo) -> p.WithFileName("tsc").WithLoadUserProfile(true).WithUseShellExecute(false)) |> ignore
    )

    Target.create "CopyHtml" (fun _ ->
        !!("client/webview/*.css")
            |> Shell.copyFiles "out/client/webview"
    )

    // Target.create "PaketRestore" (fun _ ->
    //     Shell.replaceInFiles ["../cwtools",Path.getFullName("../cwtools")] ["paket.lock"]
    //     // match Environment.isWindows with
    //     // |true -> Paket.restore (fun _ -> Paket.PaketRestoreDefaults())
    //     // |_ -> Shell.Exec( "mono", @"./.paket/paket.exe restore") |> ignore
    //     Paket.restore (fun _ -> Paket.PaketRestoreDefaults())
    //     // Paket.PaketRestoreDefaults |> ignore
    //     Shell.replaceInFiles [Path.getFullName("../cwtools"),"../cwtools"] ["paket.lock"]
    //     )

    Target.create "BuildPackage" (fun _ -> buildPackage "release")

    Target.create "SetVersion" (fun _ -> setVersion release "release")

    Target.create "PublishToGallery" (fun _ -> publishToGallery "release")

    Target.create "ReleaseGitHub" (fun _ -> releaseGithub release)


    Target.description "Assemble the extension"
    Target.create "PrePackage" (fun _ -> copyBin "release")


    // --------------------------------------------------------------------------------------
    // Run generator by default. Invoke 'build <Target>' to override
    // --------------------------------------------------------------------------------------
    Target.description "Build the requirements to run the extension locally, using remote cwtools"
    Target.create "QuickBuild" ignore
    Target.description "Build the requirements to run the extension locally, using local cwtools"
    Target.create "QuickBuildLocal" ignore
    Target.description "Package into the vsix, but don't publish it"
    Target.create "DryRelease" ignore
    Target.description "Package into the vsix, and publish it"
    Target.create "Release" ignore


open Fake.Core.TargetOperators
let buildTargetTree () =
    let (==>!) x y = x ==> y |> ignore
    "NpmInstall" ==>! "BuildClient"
    "DotNetRestore" ==>! "BuildServer"
    "DotNetRestore" ==>! "BuildServerLocal"

    "Clean"
    ==> "BuildClient"
    ==> "CopyDocs"
    ==> "CopyHtml"
    ==> "PrePackage"
    ==>! "BuildPackage"

    "PublishServer" ?=> "PrePackage" |> ignore
    "BuildServer" ?=> "PrePackage" |> ignore
    "BuildServerLocal" ?=> "PrePackage" |> ignore

    // "Format" ==>
    "DotNetRestore"
    ==> "PublishServer"
    ==> "SetVersion"
    ==> "PackageNpmInstall"
    ==> "BuildPackage"
    ==> "ReleaseGitHub"
    ==> "PublishToGallery"
    ==>! "Release"

    "BuildPackage"
    ==>! "DryRelease"

    "BuildServer"
    ==>! "QuickBuild"

    "PrePackage"
    ==>! "QuickBuild"

    "PrePackage"
    ==>! "QuickBuildLocal"

    "BuildServerLocal"
    ==>! "QuickBuildLocal"

[<EntryPoint>]
let main argv =
    argv
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext

    initTargets()
    buildTargetTree()

    Target.runOrDefaultWithArguments "Build"
    0
