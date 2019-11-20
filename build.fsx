open Fake.DotNet
// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "paket:
    source https://api.nuget.org/v3/index.json
    nuget Fake.Core
    nuget Fake.Core.Target
    nuget Fake.Core.Environment
    nuget Fake.Core.UserInput
    nuget Fake.DotNet.Cli
    nuget Fake.IO.FileSystem
    nuget Fake.Core.ReleaseNotes //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake
open Fake.IO
open Fake.DotNet
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open System
open System.Text

// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------
//let configuration = environVarOrDefault "CONFIGURATION" "Release"
let configuration = "Debug"
let buildDir  = "./build/"
let appReferences = !! "CWTools/CWTools.fsproj"

let testReferences = !! "CWToolsTests/CWToolsTests.fsproj"
let mutable dotnetExePath = "dotnet"

let project = "cwtools"
let authors = "Thomas Boby"
let owners = "Thomas Boby"

let description = "A library for parsing, editing, and validating Paradox Interactive script files."

let release =
    File.read "RELEASE_NOTES.md"
    |> Fake.Core.ReleaseNotes.parse



// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------

// let run' timeout cmd args dir =
//     if execProcess (fun info ->
//         info.FileName <- cmd
//         if not (String.IsNullOrWhiteSpace dir) then
//             info.WorkingDirectory <- dir
//         info.Arguments <- args
//     ) timeout |> not then
//         failwithf "Error while running '%s' with args: %s" cmd args

// let run = run' System.TimeSpan.MaxValue

// let runDotnet workingDir args =
//     let result =
//         ExecProcess (fun info ->
//             info.FileName <- dotnetExePath
//             info.WorkingDirectory <- workingDir
//             info.Arguments <- args) TimeSpan.MaxValue
//     if result <> 0 then failwithf "dotnet %s failed" args

open Fake.DotNet.NuGet

let packParameters name =
  [ //"--no-build"
    //"--no-restore"
    sprintf "/p:Title=\"%s\"" project
    "/p:PackageVersion=" + release.NugetVersion
    sprintf "/p:Authors=\"%s\"" authors
    sprintf "/p:Owners=\"%s\"" owners
    "/p:PackageRequireLicenseAcceptance=false"
    sprintf "/p:Description=\"%s\"" (description.Replace(",",""))
    sprintf "/p:PackageReleaseNotes=\"%O\"" ((Core.String.toLines release.Notes).Replace(",",""))
    // sprintf "/p:Copyright=\"%s\"" copyright
    // sprintf "/p:PackageTags=\"%s\"" tags
    // sprintf "/p:PackageProjectUrl=\"%s\"" projectUrl
    // sprintf "/p:PackageIconUrl=\"%s\"" iconUrl
    // sprintf "/p:PackageLicenseUrl=\"%s\"" licenceUrl
  ]
  |> String.concat " "

let buildParams (release : bool) =
    (fun (b : DotNet.BuildOptions) ->
        { b with
            Common =
                {
                    b.Common with
                        WorkingDirectory = "src/Main"
                        CustomParams = Some ((if release then "" else " /p:LinkDuringPublish=false"))
                }
            OutputPath = Some ("../../out/server/local")
            Configuration = if release  then DotNet.BuildConfiguration.Release else DotNet.BuildConfiguration.Debug
        })

// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------

Core.Target.create "Clean" (fun _ ->
    Shell.cleanDirs [buildDir; "bin"]
    appReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        DotNet.exec ( (fun (b : DotNet.Options) -> { b with WorkingDirectory = dir })) "clean" "" |> ignore
    )
    DotNet.exec id "clean" "" |> ignore

    !! "**/obj/**/*.nuspec"
    |> File.deleteAll

)


Core.Target.create "Restore" (fun _ ->
    appReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        DotNet.restore id dir
    )
)

Core.Target.create "Build" (fun _ ->
    appReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        DotNet.build id dir
    )
)

Core.Target.create "Test" (fun _ ->
    testReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        DotNet.exec ( (fun (b : DotNet.Options) -> { b with WorkingDirectory = dir })) "run" "" |> ignore
    )
)

Core.Target.create "Pack" (fun _ ->
  !! "CWTools/CWTools.fsproj"
  |> Seq.iter (fun proj ->
    let path = proj.Substring(0, proj.Length - ".fsproj".Length)
    printfn "%A" path
    let name = System.IO.Path.GetFileName path
    DotNet.exec id "pack" (
      sprintf
        "\"%s\" -o ../bin %s"
        //"pack \"%s\" -c Debug  -o ../bin %s"
        proj  (packParameters name)) |> ignore
  )
//   DotNetCli.RunCommand id (
//       sprintf
//         "pack './CWTools/CWTools.fsproj' -o ../bin %s"
//         (packParameters "CWTools")
//   )
)


// let packParameters name =
//   [ //"--no-build"
//     //"--no-restore"
//     sprintf "/p:Title=\"%s\"" project
//     "/p:PackageVersion=" + release.NugetVersion
//     sprintf "/p:Authors=\"%s\"" authors
//     sprintf "/p:Owners=\"%s\"" owners
//     "/p:PackageRequireLicenseAcceptance=false"
//     sprintf "/p:Description=\"%s\"" (description.Replace(",",""))
//     sprintf "/p:PackageReleaseNotes=\"%O\"" ((Core.String.toLines release.Notes).Replace(",",""))
//     // sprintf "/p:Copyright=\"%s\"" copyright
//     // sprintf "/p:PackageTags=\"%s\"" tags
//     // sprintf "/p:PackageProjectUrl=\"%s\"" projectUrl
//     // sprintf "/p:PackageIconUrl=\"%s\"" iconUrl
//     // sprintf "/p:PackageLicenseUrl=\"%s\"" licenceUrl
//   ]
//   |> String.concat " "


Core.Target.create "PublishTool" (fun _ ->

    !! "**/output/**/*.nupkg"
    |> File.deleteAll

    // DotNet.pack (fun po -> { po with Configuration = Fake.DotNet.DotNet.BuildConfiguration.Release } ) "CWToolsCLI/CWToolsCLI.fsproj"
    DotNet.exec id "pack" (sprintf " ./CWToolsCLI/CWToolsCLI.fsproj -c Release -o ./CWToolsCLI/output") |> ignore
    let token =
        match Fake.Core.Environment.environVarOrDefault "NUGET_ACCESS_KEY" System.String.Empty with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> Fake.Core.UserInput.getUserPassword "NUGET token: "
    DotNet.exec (fun o -> { o with WorkingDirectory = "./CWToolsCLI/output"}) "nuget" (sprintf " push *.nupkg -k %s -s https://api.nuget.org/v3/index.json" token) |> ignore
  )
    //   <projectUrl>https://github.com/tboby/cwtools</projectUrl>
    // <licenseUrl>https://github.com/tboby/cwtools/blob/master/LICENSE</licenseUrl>
    // <iconUrl>https://raw.githubusercontent.com/tboby/cwtools-vscode/master/docs/cwtools_logo.png</iconUrl>

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------

"Clean"
  ==> "Restore"
  ==> "Pack"

"Clean"
  ==> "Restore"
  ==> "Build"

"Clean"
  ==> "Restore"
  ==> "Build"
  ==> "PublishTool"

Fake.Core.Target.runOrDefaultWithArguments "Build"
