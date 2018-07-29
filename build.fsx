// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#I "packages/tools/FAKE/tools"
#r "FakeLib.dll"

open Fake
open System

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
    ReadFile "RELEASE_NOTES.md"
    |> ReleaseNotesHelper.parseReleaseNotes



// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------

let run' timeout cmd args dir =
    if execProcess (fun info ->
        info.FileName <- cmd
        if not (String.IsNullOrWhiteSpace dir) then
            info.WorkingDirectory <- dir
        info.Arguments <- args
    ) timeout |> not then
        failwithf "Error while running '%s' with args: %s" cmd args

let run = run' System.TimeSpan.MaxValue

let runDotnet workingDir args =
    let result =
        ExecProcess (fun info ->
            info.FileName <- dotnetExePath
            info.WorkingDirectory <- workingDir
            info.Arguments <- args) TimeSpan.MaxValue
    if result <> 0 then failwithf "dotnet %s failed" args

let packParameters name =
  [ //"--no-build"
    //"--no-restore"
    sprintf "/p:Title=\"%s\"" project
    "/p:PackageVersion=" + release.NugetVersion
    sprintf "/p:Authors=\"%s\"" authors
    sprintf "/p:Owners=\"%s\"" owners
    "/p:PackageRequireLicenseAcceptance=false"
    sprintf "/p:Description=\"%s\"" (description.Replace(",",""))
    sprintf "/p:PackageReleaseNotes=\"%O\"" ((toLines release.Notes).Replace(",",""))
    // sprintf "/p:Copyright=\"%s\"" copyright
    // sprintf "/p:PackageTags=\"%s\"" tags
    // sprintf "/p:PackageProjectUrl=\"%s\"" projectUrl
    // sprintf "/p:PackageIconUrl=\"%s\"" iconUrl
    // sprintf "/p:PackageLicenseUrl=\"%s\"" licenceUrl
  ]
  |> String.concat " "

// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------

Target "Clean" (fun _ ->
    CleanDirs [buildDir; "bin"]
    appReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        runDotnet dir "clean"
    )
    DotNetCli.RunCommand id "clean"
    !! "**/obj/**/*.nuspec"
    |> DeleteFiles

)


Target "Restore" (fun _ ->
    appReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        runDotnet dir "restore"
    )
)

Target "Build" (fun _ ->
    appReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        runDotnet dir "build"
    )
)

Target "Test" (fun _ ->
    testReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        runDotnet dir "run"
    )
)

Target "Pack" (fun _ ->
  !! "CWTools/CWTools.fsproj"
  |> Seq.iter (fun proj ->
    let path = proj.Substring(0, proj.Length - ".fsproj".Length)
    printfn "%A" path
    let name = System.IO.Path.GetFileName path
    DotNetCli.RunCommand id (
      sprintf
        "pack \"%s\" -o ../bin %s"
        //"pack \"%s\" -c Debug  -o ../bin %s"
        proj  (packParameters name))
  )
//   DotNetCli.RunCommand id (
//       sprintf
//         "pack './CWTools/CWTools.fsproj' -o ../bin %s"
//         (packParameters "CWTools")
//   )
)
// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------

"Clean"
  ==> "Restore"
  ==> "Pack"

"Clean"
  ==> "Restore"
  ==> "Build"

RunTargetOrDefault "Build"
