open System.IO
open Fake.Api
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open System
open Fake.IO.FileSystemOperators
open Fake.Tools.Git

// --------------------------------------------------------------------------------------
// Configuration
// --------------------------------------------------------------------------------------


// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "cwtools"
let gitHome = "https://github.com/" + gitOwner

// The name of the project on GitHub
let gitName = "cwtools"


// Read additional information from the release notes document
let releaseNotesData = ReleaseNotes.load "RELEASE_NOTES.md"

let githubToken = lazy (Environment.environVarOrFail "GITHUB_TOKEN")

let nugetToken = lazy (Environment.environVarOrFail "NUGET_TOKEN")

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



// let releaseBin      = "release/bin"
// let fsacBin         = "paket-files/github.com/fsharp/FsAutoComplete/bin/release"

// let releaseBinNetcore = releaseBin + "_netcore"
// let fsacBinNetcore = fsacBin + "_netcore"

let cwtoolsPath = ""
let cwtoolsProjectName = "CWTools/CWTools.fsproj"
let cwtoolsCLIProjectName = "CWToolsCLI/CWToolsCLI.fsproj"

let libraryProjects = !! "CWTools/CWTools.fsproj"

let toolProjects =
    !! "CWToolsCLI/CWToolsCLI.fsproj"
    ++ "CWToolsDocs/CWToolsDocs.fsproj"
    ++ "CWToolsScripts/CWToolsScripts.fsproj"

let testProjects =
    !! "CWTools.Tests/CWTools.Tests.fsproj"
    ++ "CWToolsCSTests/CWToolsCSTests.fsproj"

let pkgPath = Path.GetFullPath "./pkg"


// --------------------------------------------------------------------------------------
// Build the Generator project and run it
// --------------------------------------------------------------------------------------

let testAll () =
    let res =
        DotNet.exec (DotNet.Options.withWorkingDirectory "./CWToolsTests") "run" ""

    match res.ExitCode with
    | 0 -> ()
    | _ -> failwithf "Tests failed with exit code %d" res.ExitCode

    DotNet.test id "CWToolsCSTests/CWToolsCSTests.csproj"

// testProjects |> Seq.iter (fun p -> DotNet.test (DotNet.Options.withWorkingDirectory p) p)

let buildAll () =
    libraryProjects |> Seq.iter (DotNet.build id)
    toolProjects |> Seq.iter (DotNet.build id)
    testProjects |> Seq.iter (DotNet.build id)

let args =
    { MSBuild.CliArguments.Create() with
        Properties = [ "VersionPrefix", releaseNotesData.NugetVersion ] }

let packAllLibs () =
    libraryProjects
    |> Seq.iter (fun path ->
        DotNet.pack
            (fun p ->
                { p with
                    OutputPath = Some pkgPath
                    MSBuildParams = args })
            path)

let packAllTools () =

    toolProjects
    |> Seq.iter (fun path ->
        DotNet.pack
            (fun p ->
                { p with
                    OutputPath = Some pkgPath
                    MSBuildParams = args })
            path)

let pushAll () =
    !!(pkgPath </> "*.nupkg")
    |> Seq.iter (
        DotNet.nugetPush (fun p ->
            { p with
                PushParams =
                    { p.PushParams with
                        ApiKey = Some nugetToken.Value } })
    )

// let setVersion (release: ReleaseNotes.ReleaseNotes) releaseDir =
//     let versionString = $"%O{release.NugetVersion}"
//     setPackageJsonField "version" versionString releaseDir
//
//
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
    Branches.pushBranch "" remote "master"
    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion

    let files = !!("./pkg" </> "*.nupkg")

    let token = githubToken.Value

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
    Target.create "Clean"
    <| fun _ -> !! "./**/bin/" ++ "./**/obj/" -- "./build/**" ++ pkgPath |> Shell.cleanDirs

    Target.create "Build" (fun _ -> buildAll ())
    Target.create "Test" (fun _ -> testAll ())
    Target.create "PackLibs" (fun _ -> packAllLibs ())
    Target.create "PackTools" (fun _ -> packAllTools ())
    Target.create "Push" (fun _ -> pushAll ())
    Target.create "ReleaseGitHub" (fun _ -> releaseGithub releaseNotesData)

let buildTargetTree () =
    "Clean"
    ==> "Build"
    ==> "Test"
    ==> "PackLibs"
    ==> "PackTools"
    ==> "Push"
    ==> "ReleaseGitHub"
    |> ignore

[<EntryPoint>]
let main argv =
    Microsoft.Build.Logging.StructuredLogger.Strings.Initialize()

    argv
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext

    initTargets ()
    buildTargetTree ()

    Target.runOrDefaultWithArguments "Build"
    0
