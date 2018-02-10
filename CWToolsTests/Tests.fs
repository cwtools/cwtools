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
    let findComments t s (a : Both) =
            match (s, a) with
            | ((b, c), _) when b -> (b, c)
            | ((_, c), CommentI nc) -> (false, nc::c)
            | ((_, c), NodeI n) when n.Key = t -> (true, c)
            | ((_, c), LeafI v) when v.Key = t -> (true, c)
            | ((_, _), _) -> (false, [])
    let fNode = (fun (node:Node) (children) ->
            let one = node.Values |> List.map (fun e -> let (Position p) = e.Position in p, node.All |> List.rev |> List.fold (findComments e.Key) (false, []) |> snd)
            //eprintfn "%s %A" node.Key (node.All |> List.rev)
            //eprintfn "%A" one
            let two = node.Children |> List.map (fun e -> let (Position p) = e.Position in p, node.All |> List.rev |> List.fold (findComments e.Key) (false, []) |> snd)
            let new2 = one @ two |> List.filter (fun (p, c) -> not (List.isEmpty c))
            new2 @ children
                )
    let fCombine = (@)
    node |> (foldNode2 fNode fCombine [])

let rec remove_first lst item =
    match lst with
    | h::t when item = h -> t
    | h::t -> h::remove_first t item
    | _ -> []
let remove_all x y =
    y |> List.fold remove_first x



let getLocTestInfo node = 
    let req, noreq = getAllTestLocs node
    let comments = getNodeComments node |> List.filter(fun (p, c) -> not (List.isEmpty c)) |> List.collect (fun (f, c) -> c |> List.map (fun cc -> f, cc)) |> List.map fst
    req, noreq, comments
let parseEntities validfiles =
    validfiles
    |> List.map ((fun (file, _, parsed) -> (file, parsed.statements, parsed.parseTime))
    >> (fun (f, parsed, _) ->  f, (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) parsed)))

[<Tests>]
let tests =
    testList "localisation" [
        testList "no loc" [
                let stl = STLGame("./testfiles/localisationtests/gamefiles", FilesScope.All, "", [], [], [], [STL STLLang.English], false)
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
                    let missing = remove_all expected fileErrors
                    let extras = remove_all fileErrors expected
                    Expect.isEmpty (missing) (sprintf "Following lines are expected to have an error %A" missing)
                    Expect.isEmpty (extras) (sprintf "Following lines are not expected to have an error %A" extras )
                    Expect.isEmpty (extra) (sprintf "Incorrect required %s" file)
                yield! testLocKeys |> List.map (fun (f, t) -> testCase (f.ToString()) <| fun () -> inner (f, t))
            ];
            testList "with loc" [
                
                let locfiles = "localisation/l_english.yml", File.ReadAllText("./testfiles/localisationtests/localisation/l_english.yml")
                let stl = STLGame("./testfiles/localisationtests/gamefiles", FilesScope.All, "", [], [], [locfiles], [STL STLLang.English], false)
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
    
[<Tests>]
let tests2 = 
    testList "validation" [
        let stl = STLGame("./testfiles/validationtests/interfacetests", FilesScope.All, "", [], [], [], [STL STLLang.English], false)
        let errors = stl.ValidationErrors |> List.map (fun (s, n, l, f) -> Position.UnConv n)
        let testVals = stl.ValidFiles |> parseEntities |> List.map (fun (f, s) -> f, getNodeComments s |> List.map fst)
        //let nodeComments = entities |> List.collect (fun (f, s) -> getNodeComments s) |> List.map fst
        let inner (file, ((nodekeys : FParsec.Position list)) )=
            let expected = nodekeys
            let fileErrors = errors |> List.filter (fun f -> f.StreamName = file )
            let missing = remove_all expected fileErrors
            let extras = remove_all fileErrors expected
            Expect.isEmpty (missing) (sprintf "Following lines are expected to have an error %A" missing)
            Expect.isEmpty (extras) (sprintf "Following lines are not expected to have an error %A" extras )
        yield! testVals |> List.map (fun (f, t) -> testCase (f.ToString()) <| fun () -> inner (f, t))

    ]