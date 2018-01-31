module Tests

open Expecto
open CWTools.Games
open FParsec
open CWTools.Common
open CWTools.Process
open CWTools.Parser
open CWTools.Process.ProcessCore
open System.IO


let getAllTestLocs node =
    let fNode = (fun (x:Node) (req, notreq) ->
        let required = x.Values |> List.filter ( fun l -> l.Value.ToRawString() = "test_required") |> List.map (fun l -> (let (Position p) = l.Position in p))
        let notrequired = x.Values |> List.filter ( fun l -> l.Value.ToRawString() = "test_optional") |> List.map (fun l -> (let (Position p) = l.Position in p))
        required @ req, notrequired @ notreq)
    let fCombine = (fun (r,n) (r2, n2) -> (r@r2, n@n2))
    node |> (foldNode2 fNode fCombine ([],[]))

let parseEntities validfiles =
    validfiles
    |> List.map ((fun (file, _, parsed) -> (file, parsed.statements, parsed.parseTime))
    >> (fun (f, parsed, _) ->  f, (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) parsed)))

[<Tests>]
let tests =
  
  testList "localisation" [
    testCase "test" <| fun _ ->
        let stl = STLGame("./testfiles/localisationtests/gamefiles", FilesScope.All, "", [], [], [], [STL STLLang.English], true)
        let errors = stl.LocalisationErrors |> List.map (fun (s, n, l, f) -> Position.UnConv n)
        let testLocKeys = stl.ValidFiles |> parseEntities |> List.map (fun (f, s) -> getAllTestLocs s)
        let inner ((req : FParsec.Position list), (noreq : FParsec.Position list) )=
            let extra = noreq |> List.filter (fun r -> errors |> List.contains r)
            Expect.containsAll errors req "Missing required"
            Expect.isEmpty (extra) "Incorrect required"
        testLocKeys |> List.iter inner
      //stl
    testCase "withLoc" <| fun _ ->
        let locfiles = "localisation/l_english.yml", File.ReadAllText("./testfiles/localisationtests/localisation/l_english.yml")
        let stl = STLGame("./testfiles/localisationtests/gamefiles", FilesScope.All, "", [], [], [locfiles], [STL STLLang.English], true)
        let errors = stl.LocalisationErrors |> List.map (fun (s, n, l, f) -> Position.UnConv n)
        let testLocKeys = stl.ValidFiles |> parseEntities |> List.map (fun (f, s) -> getAllTestLocs s)
        let inner ((req : FParsec.Position list), (noreq : FParsec.Position list) )=
            let missing = req |> List.filter (fun r -> not (errors |> List.contains r))
            let extra = noreq |> List.filter (fun r -> errors |> List.contains r)
            Expect.isEmpty missing "Missing required despite having key"
            Expect.isEmpty (extra) "Incorrect required"
        testLocKeys |> List.iter inner
  ]