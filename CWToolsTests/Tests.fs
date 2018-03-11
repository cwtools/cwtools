module Tests

open Expecto
open CWTools.Games
open FParsec
open CWTools.Common
open CWTools.Process
open CWTools.Parser
open CWTools.Process.ProcessCore
open System.IO
open System.Reflection
open CWTools.Parser.DocsParser
open CWTools.Parser.SetupLogParser
open CWTools.Common.STLConstants
open System


let getAllTestLocs node =
    let fNode = (fun (x:Node) (req, notreq) ->
        let required = x.Values |> List.filter ( fun l -> l.Value.ToRawString() = "test_required") |> List.map (fun l -> (let (Position p) = l.Position in p))
        let notrequired = x.Values |> List.filter ( fun l -> l.Value.ToRawString() = "test_optional") |> List.map (fun l -> (let (Position p) = l.Position in p))
        required @ req, notrequired @ notreq)
    let fCombine = (fun (r,n) (r2, n2) -> (r@r2, n@n2))
    node |> (foldNode2 fNode fCombine ([],[]))

let getNodeComments (node : Node) =
    let findComments t s (a : Child) =
            match (s, a) with
            | ((b, c), _) when b -> (b, c)
            | ((_, c), CommentC nc) when nc.StartsWith("#") -> (false, c)
            | ((_, c), CommentC nc) -> (false, nc::c)
            | ((_, c), NodeC n) when n.Position = t -> (true, c)
            | ((_, c), LeafC v) when v.Position = t -> (true, c)
            | ((_, _), _) -> (false, [])
    let fNode = (fun (node:Node) (children) ->
            let one = node.Values |> List.map (fun e -> let (Position p) = e.Position in p, node.All |> List.rev |> List.fold (findComments e.Position) (false, []) |> snd)
            //eprintfn "%s %A" node.Key (node.All |> List.rev)
            //eprintfn "%A" one
            let two = node.Children |> List.map (fun e -> let (Position p) = e.Position in p, node.All |> List.rev |> List.fold (findComments e.Position) (false, []) |> snd)
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
                let stl = STLGame("./testfiles/localisationtests/gamefiles", FilesScope.All, "", [], [], [], [], [STL STLLang.English], true)
                let parseErrors = stl.ParserErrors
                let errors = stl.LocalisationErrors |> List.map (fun (c, s, n, l, f, k) -> Position.UnConv n)
                let entities = stl.AllEntities
                let testLocKeys = entities |> List.map (fun e -> e.filepath, getLocTestInfo e.entity)
                let nodeComments = entities |> List.collect (fun e -> getNodeComments e.entity) |> List.map fst
                yield testCase ("parse") <| fun () -> Expect.isEmpty parseErrors (parseErrors |> List.tryHead |> Option.map (sprintf "%A") |> Option.defaultValue "")
                yield testCase ("parse2") <| fun () -> Expect.isEmpty stl.ParserErrors (stl.ParserErrors |> List.tryHead |> Option.map (sprintf "%A") |> Option.defaultValue "")
                //eprintfn "%A" testLocKeys
                // eprintfn "%A" entities
                //eprintfn "%A" errors
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
                let stl = STLGame("./testfiles/localisationtests/gamefiles", FilesScope.All, "", [], [], [], [locfiles], [STL STLLang.English], false)
                let parseErrors = stl.ParserErrors
                yield testCase ("parse") <| fun () -> Expect.isEmpty parseErrors (parseErrors |> List.tryHead |> Option.map (sprintf "%A") |> Option.defaultValue "")

                let errors = stl.LocalisationErrors |> List.map (fun (c, s, n, l, f, k) -> Position.UnConv n)
                let testLocKeys = stl.AllEntities |> List.map (fun e -> e.filepath, getLocTestInfo e.entity)
                let inner (file, ((req : FParsec.Position list), (noreq : FParsec.Position list), (nodekeys : FParsec.Position list) ))=
                    let missing = req |> List.filter (fun r -> not (errors |> List.contains r))
                    let extra = noreq |> List.filter (fun r -> errors |> List.contains r)
                    Expect.isEmpty missing (sprintf "Missing required despite having key %s" file)
                    Expect.isEmpty (extra) (sprintf "Incorrect required %s" file)
                yield! testLocKeys |> List.map (fun (f, t) -> testCase (f.ToString()) <| fun () -> inner (f, t))
            ]
    ]
    
let testFolder folder testsname =
    testList testsname [
        let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_0.2.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs p)
        let modifiers = SetupLogParser.parseLogsFile "./testfiles/validationtests/setup.log" |> (function |Success(p, _, _) -> SetupLogParser.processLogs p)
        let stl = STLGame(folder, FilesScope.All, "", triggers, effects, modifiers, [], [STL STLLang.English], false)
        let errors = stl.ValidationErrors |> List.map (fun (c, s, n, l, f, k) -> c, Position.UnConv n) //>> (fun p -> FParsec.Position(p.StreamName, p.Index, p.Line, 1L)))
        let testVals = stl.AllEntities |> List.map (fun (e) -> e.filepath, getNodeComments e.entity |> List.map fst)
        printfn "%A" (errors |> List.map (fun (c, f) -> f.StreamName))

        // eprintfn "%A" (stl.AllFiles())
        //let nodeComments = entities |> List.collect (fun (f, s) -> getNodeComments s) |> List.map fst
        let inner (file, ((nodekeys : FParsec.Position list)) )=
            let expected = nodekeys  //|> List.map (fun p -> FParsec.Position(p.StreamName, p.Index, p.Line, 1L))
            let fileErrors = errors |> List.filter (fun (c, f) -> f.StreamName = file )
            let fileErrorPositions = fileErrors |> List.map snd
            let missing = remove_all expected fileErrorPositions
            let extras = remove_all fileErrorPositions expected
            Expect.isEmpty (extras) (sprintf "Following lines are not expected to have an error %A, all %A" extras expected )
            Expect.isEmpty (missing) (sprintf "Following lines are expected to have an error %A" missing)
        yield! testVals |> List.map (fun (f, t) -> testCase (f.ToString()) <| fun () -> inner (f, t))
    ]
    
[<Tests>]
let folderTests =
    testList "validation" [
        testFolder "./testfiles/validationtests/interfacetests" "interface"
        testFolder "./testfiles/validationtests/scopetests" "scopes"
        testFolder "./testfiles/validationtests/variabletests" "variables"
        testFolder "./testfiles/validationtests/modifiertests" "modifiers"
    ]

[<Tests>]
let specialtests =
    testList "log" [
        testCase "modifiers" <| fun () ->
            let modfile = SetupLogParser.parseLogsFile "./testfiles/scriptedorstatictest/setup.log"
            (modfile |> (function |Failure(e, _,_) -> eprintfn "%s" e |_ -> ()))
            let modifiers = (modfile |> (function |Success(p, _, _) -> SetupLogParser.processLogs p))
            let stl = STLGame("./testfiles/scriptedorstatictest/", FilesScope.All, "", [], [], modifiers, [], [STL STLLang.English], false)
            let exp = [{tag = "test"; categories = [ModifierCategory.Pop]; core = false}]
            Expect.equal stl.StaticModifiers exp ""
    ]
// [<Tests>]
// let tests2 = 
//     testList "validation" [
//         let stl = STLGame("./testfiles/validationtests/interfacetests", FilesScope.All, "", [], [], [], [], [STL STLLang.English], false)
//         let errors = stl.ValidationErrors |> List.map (fun (c, s, n, l, f) -> Position.UnConv n)
//         let testVals = stl.AllEntities |> List.map (fun (e) -> e.filepath, getNodeComments e.entity |> List.map fst)
//         // eprintfn "%A" (stl.AllFiles())
//         //let nodeComments = entities |> List.collect (fun (f, s) -> getNodeComments s) |> List.map fst
//         let inner (file, ((nodekeys : FParsec.Position list)) )=
//             let expected = nodekeys
//             let fileErrors = errors |> List.filter (fun f -> f.StreamName = file )
//             let missing = remove_all expected fileErrors
//             let extras = remove_all fileErrors expected
//             Expect.isEmpty (extras) (sprintf "Following lines are not expected to have an error %A" extras )
//             Expect.isEmpty (missing) (sprintf "Following lines are expected to have an error %A" missing)
//         yield! testVals |> List.map (fun (f, t) -> testCase (f.ToString()) <| fun () -> inner (f, t))

//     ]

// [<Tests>]
// let tests3 = 
//     testList "validation" [
//         let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_0.2.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs p)
//         let stl = STLGame("./testfiles/validationtests/scopetests", FilesScope.All, "", triggers, effects, [], [], [STL STLLang.English], false)
//         let errors = stl.ValidationErrors |> List.map (fun (c, s, n, l, f) -> Position.UnConv n)
//         let testVals = stl.AllEntities |> List.map (fun (e) -> e.filepath, getNodeComments e.entity |> List.map fst)
//         //let nodeComments = entities |> List.collect (fun (f, s) -> getNodeComments s) |> List.map fst
//         let inner (file, ((nodekeys : FParsec.Position list)) )=
//             let expected = nodekeys
//             let fileErrors = errors |> List.filter (fun f -> f.StreamName = file )
//             let missing = remove_all expected fileErrors
//             let extras = remove_all fileErrors expected
//             Expect.isEmpty (extras) (sprintf "Following lines are not expected to have an error %A" extras )
//             Expect.isEmpty (missing) (sprintf "Following lines are expected to have an error %A" missing)
//         yield! testVals |> List.map (fun (f, t) -> testCase (f.ToString()) <| fun () -> inner (f, t))
//     ]

let rec replaceFirst predicate value = function
        | [] -> []
        | h :: t when predicate h -> value :: t
        | h :: t -> h :: replaceFirst predicate value t

let fixEmbeddedFileName (s : string) =
    let count = (Seq.filter ((=) '.') >> Seq.length) s
    let mutable out = "//" + s
    [1 .. count - 1] |> List.iter (fun _ -> out <- (replaceFirst ((=) '.') '/' (out |> List.ofSeq)) |> Array.ofList |> FSharp.Core.string )
    out

[<Tests>]
let embeddedTests =
    testList "embedded" [
        let filelist = Assembly.GetEntryAssembly().GetManifestResourceStream("CWToolsTests.testfiles.embeddedtest.embedded.vanilla_files_test.csv") 
                                |> (fun f -> (new StreamReader(f)).ReadToEnd().Split(Environment.NewLine))
                                |> Array.toList |> List.map (fun f -> f, "")
        // eprintfn "%A" filelist               
        let embeddedFileNames = Assembly.GetEntryAssembly().GetManifestResourceNames() |> Array.filter (fun f -> f.Contains("common") || f.Contains("localisation") || f.Contains("interface"))
        let embeddedFiles = embeddedFileNames |> List.ofArray |> List.map (fun f -> fixEmbeddedFileName f, (new StreamReader(Assembly.GetEntryAssembly().GetManifestResourceStream(f))).ReadToEnd())
        let stlE = STLGame("./testfiles/embeddedtest/test", FilesScope.All, "", [], [], [], embeddedFiles @ filelist, [STL STLLang.English], false)
        let stlNE = STLGame("./testfiles/embeddedtest/test", FilesScope.All, "", [], [], [], [], [STL STLLang.English], false)
        let eerrors = stlE.ValidationErrors |> List.map (fun (c, s, n, l, f, k) -> Position.UnConv n)
        let neerrors = stlNE.ValidationErrors |> List.map (fun (c, s, n, l, f, k) -> Position.UnConv n)
        let etestVals = stlE.AllEntities |> List.map (fun (e) -> e.filepath, getNodeComments e.entity |> List.map fst)
        let netestVals = stlNE.AllEntities |> List.map (fun (e) -> e.filepath, getNodeComments e.entity |> List.map fst)
        let einner (file, ((nodekeys : FParsec.Position list)) )=
            let fileErrors = eerrors |> List.filter (fun f -> f.StreamName = file )
            Expect.isEmpty (fileErrors) (sprintf "Following lines are not expected to have an error %A" fileErrors )
        yield! etestVals |> List.map (fun (f, t) -> testCase ("embed" + f.ToString()) <| fun () -> einner (f, t))
        let neinner (file, ((nodekeys : FParsec.Position list)) )=
            let expected = nodekeys
            let fileErrors = neerrors |> List.filter (fun f -> f.StreamName = file )
            let missing = remove_all expected fileErrors
            let extras = remove_all fileErrors expected
            Expect.isEmpty (extras) (sprintf "Following lines are not expected to have an error %A" extras )
            Expect.isEmpty (missing) (sprintf "Following lines are expected to have an error %A" missing)
        yield! netestVals |> List.map (fun (f, t) -> testCase ("no embed" + f.ToString()) <| fun () -> neinner (f, t))

    ]

// [<Tests>]
// let logTests =
//     testList "logs" [
//         testCase "logFile" <| fun () ->
//             let logs = parseLogsFile "./testfiles/parsertests/setup.log"
//             match logs with
//             |Success((s, m), _, _) -> 
//                 s |> List.iter (printfn "%A")
//                 m |> List.iter (printfn "%A")
//                 m |> List.map (fun x -> x.categories) |> List.distinct |> List.sort |> printfn "%A"
//             |Failure(e ,_, _) -> Expect.isFalse true e
//     ]