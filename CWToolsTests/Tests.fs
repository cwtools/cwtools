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

let getNodeComments (node : Node) =
    let findComment t s (a : Both) =
            match (s, a) with
            | ((b, c), _) when b -> (b, c)
            | ((_, c), CommentI nc) -> (false, nc::c)
            | ((_, c), NodeI n) when n.Key = t -> (true, c)
            | ((_, c), LeafI v) when v.Key = t -> (true, c)
            | ((_, _), _) -> (false, [])
    let fNode = (fun (node:Node) (children) ->
            let one = node.Values |> List.map (fun e -> let (Position p) = e.Position in p, node.All |> List.rev |> List.fold (findComment e.Key) (false, []) |> snd)
            let two = node.Children |> List.map (fun e -> let (Position p) = e.Position in p, node.All |> List.rev |> List.fold (findComment e.Key) (false, []) |> snd)
            let new2 = one @ two |> List.filter (fun (p, c) -> not (List.isEmpty c))
            new2 @ children
                )
    let fCombine = (@)
    node |> (foldNode2 fNode fCombine [])

let getLocTestInfo node = 
    let req, noreq = getAllTestLocs node
    let comments = getNodeComments node |> List.filter(fun (p, c) -> not (List.isEmpty c)) |> List.map fst
    req, noreq, comments
let parseEntities validfiles =
    validfiles
    |> List.map ((fun (file, _, parsed) -> (file, parsed.statements, parsed.parseTime))
    >> (fun (f, parsed, _) ->  f, (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) parsed)))

[<Tests>]
let tests =
    testList "localisation" [
        testList "no loc" [
                let stl = STLGame("./testfiles/localisationtests/gamefiles", FilesScope.All, "", [], [], [], [STL STLLang.English], true)
                let errors = stl.LocalisationErrors |> List.map (fun (s, n, l, f) -> Position.UnConv n)
                let entities = stl.ValidFiles |> parseEntities
                let testLocKeys = entities |> List.map (fun (f, s) -> f, getLocTestInfo s)
                let nodeComments = entities |> List.collect (fun (f, s) -> getNodeComments s) |> List.map fst
                
                eprintfn "%A" testLocKeys
                // eprintfn "%A" entities
                eprintfn "%A" errors
                // eprintfn "%A" stl.LocalisationErrors
                let inner (file, ((req : FParsec.Position list), (noreq : FParsec.Position list), (nodekeys : FParsec.Position list)) )=
                    let extra = noreq |> List.filter (fun r -> errors |> List.contains r)
                    let expected = req @ nodekeys
                    let fileErrors = errors |> List.filter (fun f -> f.StreamName = file )
                    let missing = Set.difference (Set.ofList expected) (Set.ofList fileErrors)
                    let extras = Set.difference (Set.ofList fileErrors) (Set.ofList expected)
                    Expect.isEmpty (missing) (sprintf "Following lines are expected to have an error %A, all errors: %A, all expected: %A" missing fileErrors expected)
                    Expect.isEmpty (extras) (sprintf "Following lines are not expected to have an error %A, all errors: %A, all expected: %A" extras fileErrors expected)
                    Expect.isEmpty (extra) (sprintf "Incorrect required %s" file)
                yield! testLocKeys |> List.map (fun (f, t) -> testCase (f.ToString()) <| fun () -> inner (f, t))
            ];
            testList "with loc" [
                
                let locfiles = "localisation/l_english.yml", File.ReadAllText("./testfiles/localisationtests/localisation/l_english.yml")
                let stl = STLGame("./testfiles/localisationtests/gamefiles", FilesScope.All, "", [], [], [locfiles], [STL STLLang.English], true)
                let errors = stl.LocalisationErrors |> List.map (fun (s, n, l, f) -> Position.UnConv n)
                let testLocKeys = stl.ValidFiles |> parseEntities |> List.map (fun (f, s) -> f, getLocTestInfo s)
                let inner (file, ((req : FParsec.Position list), (noreq : FParsec.Position list), (nodekeys : FParsec.Position list) ))=
                    let missing = req |> List.filter (fun r -> not (errors |> List.contains r))
                    let extra = noreq |> List.filter (fun r -> errors |> List.contains r)
                    Expect.isEmpty missing (sprintf "Missing required despite having key %s" file)
                    Expect.isEmpty (extra) (sprintf "Incorrect required %s" file)
                yield! testLocKeys |> List.map (fun (f, t) -> testCase (f.ToString()) <| fun () -> inner (f, t))
            ]
    ]
    