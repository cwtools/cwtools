module Tests

open Expecto
open CWTools.Games
open FParsec
open CWTools.Common
open CWTools.Process
open CWTools.Parser
open CWTools.Parser.Types
open CWTools.Process.ProcessCore
open System.IO
open System.Reflection
open CWTools.Parser.DocsParser
open CWTools.Parser.SetupLogParser
open CWTools.Common.STLConstants
open System
open CWTools.Utilities.Position
open CWTools.Games.Files
open CWTools.Games.Stellaris
open CWTools.Games.Stellaris.STLLookup
open System.Threading
open System.Globalization
open CWTools.Validation.Stellaris
open MBrace.FsPickler

let emptyStellarisSettings (rootDirectory) = {
    rootDirectory = rootDirectory
    scope = FilesScope.All
    modFilter = None
    validation = {
        validateVanilla = false
        experimental = true
        langs = [STL STLLang.English]
    }
    rules = None
    embedded = {
        triggers = []
        effects = []
        modifiers = []
        embeddedFiles = []
        cachedResourceData = []
    }
}

let getAllTestLocs node =
    let fNode = (fun (x:Node) (req, notreq) ->
        let required = x.Values |> List.filter ( fun l -> l.Value.ToRawString() = "test_required") |> List.map (fun l -> l.Position)
        let notrequired = x.Values |> List.filter ( fun l -> l.Value.ToRawString() = "test_optional") |> List.map (fun l -> l.Position)
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
            // | ((_, c), LeafValueC lv) when lv.Position = t -> (true, c)
            | ((_, _), _) -> (false, [])
    let fNode = (fun (node:Node) (children) ->
            let one = node.Values |> List.map (fun e -> e.Position, node.All |> List.fold (findComments e.Position) (false, []) |> snd)
            //eprintfn "%s %A" node.Key (node.All |> List.rev)
            //eprintfn "%A" one
            let two = node.Children |> List.map (fun e -> e.Position, node.All |> List.fold (findComments e.Position) (false, []) |> snd)
            let three = node.LeafValues |> Seq.toList |> List.map (fun e -> e.Position, node.All |> List.fold (findComments e.Position) (false, []) |> snd)
            let new2 = one @ two @ three |> List.filter (fun (p, c) -> not (List.isEmpty c))
            new2 @ children
                )
    let fCombine = (@)
    node |> (foldNode2 fNode fCombine [])

let rec remove_first f lst item =
    match lst with
    | h::t when f item = f h -> t
    | h::t -> h::remove_first f t item
    | _ -> []
let remove_all_by x y f =
    y |> List.fold (remove_first f) x
let remove_all x y =
    remove_all_by x y (id)
    //y |> List.fold remove_first x



let getLocTestInfo node =
    let req, noreq = getAllTestLocs node
    let comments = getNodeComments node |> List.filter(fun (p, c) -> not (List.isEmpty c)) |> List.collect (fun (f, c) -> c |> List.map (fun cc -> f, cc)) |> List.map fst
    req, noreq, comments

[<Tests>]
let tests =
    testList "localisation" [
        testList "no loc" [
                let stl = STLGame(emptyStellarisSettings "./testfiles/localisationtests/gamefiles") :> IGame<STLComputedData>
                let parseErrors = stl.ParserErrors()
                let errors = stl.LocalisationErrors(true) |> List.map (fun (c, s, n, l, f, k) -> n)
                let entities = stl.AllEntities()
                let testLocKeys = entities |> List.map (fun struct (e, _) -> e.filepath, getLocTestInfo e.entity)
                let nodeComments = entities |> List.collect (fun struct (e, _) -> getNodeComments e.entity) |> List.map fst
                yield testCase ("parse") <| fun () -> Expect.isEmpty parseErrors (parseErrors |> List.tryHead |> Option.map (sprintf "%A") |> Option.defaultValue "")
                yield testCase ("parse2") <| fun () -> Expect.isEmpty (stl.ParserErrors()) (stl.ParserErrors() |> List.tryHead |> Option.map (sprintf "%A") |> Option.defaultValue "")
                //eprintfn "%A" testLocKeys
                // eprintfn "%A" entities
                //eprintfn "%A" errors
                // eprintfn "%A" stl.LocalisationErrors
                let inner (file, ((req : range list), (noreq : range list), (nodekeys : range list)) )=
                    let extra = noreq |> List.filter (fun r -> errors |> List.contains r)
                    let expected = req @ nodekeys
                    let fileErrors = errors |> List.filter (fun f -> f.FileName = file )
                    let missing = remove_all expected fileErrors
                    let extras = remove_all fileErrors expected
                    Expect.isEmpty (missing) (sprintf "Following lines are expected to have an error %A" missing)
                    Expect.isEmpty (extras) (sprintf "Following lines are not expected to have an error %A" extras )
                    Expect.isEmpty (extra) (sprintf "Incorrect required %s" file)
                yield! testLocKeys |> List.map (fun (f, t) -> testCase (f.ToString()) <| fun () -> inner (f, t))
            ];
            testList "with loc" [

                let locfiles = "localisation/l_english.yml", File.ReadAllText("./testfiles/localisationtests/localisation/l_english.yml")
                let settings = emptyStellarisSettings "./testfiles/localisationtests/gamefiles"
                let settings = { settings with embedded = { settings.embedded with embeddedFiles = [locfiles] };
                                            validation = {settings.validation with langs = [STL STLLang.English; STL STLLang.German] }}
                let stl = STLGame(settings) :> IGame<STLComputedData>
                let parseErrors = stl.ParserErrors()
                yield testCase ("parse") <| fun () -> Expect.isEmpty parseErrors (parseErrors |> List.tryHead |> Option.map (sprintf "%A") |> Option.defaultValue "")

                let errors = stl.LocalisationErrors(true) |> List.map (fun (c, s, n, l, f, k) -> n)
                let testLocKeys = stl.AllEntities() |> List.map (fun struct (e, _) -> e.filepath, getLocTestInfo e.entity)
                let inner (file, ((req : range list), (noreq : range list), (nodekeys : range list) ))=
                    let missing = req |> List.filter (fun r -> not (errors |> List.contains r))
                    let extra = noreq |> List.filter (fun r -> errors |> List.contains r)
                    Expect.isEmpty missing (sprintf "Missing required despite having key %s" file)
                    Expect.isEmpty (extra) (sprintf "Incorrect required %s" file)
                yield! testLocKeys |> List.map (fun (f, t) -> testCase (f.ToString()) <| fun () -> inner (f, t))
                eprintfn "%A" (stl.LocalisationErrors(true))
                let locErrorCodes = [ "CW225"; "CW226"; "CW254"; "CW255"; "CW256"; "CW257"; "CW258"]
                let globalLocError = stl.LocalisationErrors(true) |> List.filter (fun (c, s, n, l, f, k) -> List.contains c locErrorCodes)
                yield testCase "globalLoc" <| fun () ->
                    Expect.hasCountOf globalLocError 7u (fun f -> true) (sprintf "wrong number of errors %A" globalLocError)
            ]
    ]

let testFolder folder testsname config (culture : string) =
    testList (testsname + culture) [
        Thread.CurrentThread.CurrentCulture <- CultureInfo(culture);
        Thread.CurrentThread.CurrentUICulture <- CultureInfo(culture);
        let configtext = "./testfiles/configtests/test.cwt", File.ReadAllText "./testfiles/configtests/test.cwt"
        let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.1.0.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs p)
        let modifiers = SetupLogParser.parseLogsFile "./testfiles/validationtests/setup.log" |> (function |Success(p, _, _) -> SetupLogParser.processLogs p)
        // let stl = STLGame(folder, FilesScope.All, "", triggers, effects, modifiers, [], [configtext], [STL STLLang.English], false, true, config)
        let settings = emptyStellarisSettings folder
        let settings = { settings with embedded = { settings.embedded with triggers = triggers; effects = effects; modifiers = modifiers; };
                                            rules = if config then Some { ruleFiles = [configtext]; validateRules = config} else None}
        let stl = STLGame(settings) :> IGame<STLComputedData>
        let errors = stl.ValidationErrors() |> List.map (fun (c, s, n, l, f, k) -> f, n) //>> (fun p -> FParsec.Position(p.StreamName, p.Index, p.Line, 1L)))
        let testVals = stl.AllEntities() |> List.map (fun struct (e, _) -> e.filepath, getNodeComments e.entity |> List.collect (fun (r, cs) -> cs |> List.map (fun _ -> r)))
        // printfn "%A" (errors |> List.map (fun (c, f) -> f.StreamName))
        //printfn "%A" (testVals)
        //eprintfn "%A" testVals
        // eprintfn "%A" (stl.AllFiles())
        //let nodeComments = entities |> List.collect (fun (f, s) -> getNodeComments s) |> List.map fst
        let inner (file, ((nodekeys : range list)) )=
            let expected = nodekeys |> List.map (fun nk -> "", nk)
             //|> List.map (fun p -> FParsec.Position(p.StreamName, p.Index, p.Line, 1L))
            let fileErrors = errors |> List.filter (fun (c, f) -> f.FileName = file )
            let fileErrorPositions = fileErrors //|> List.map snd
            let missing = remove_all_by expected fileErrorPositions snd
            let extras = remove_all_by fileErrorPositions expected snd
            //eprintfn "%A" nodekeys
            Expect.isEmpty (extras) (sprintf "Following lines are not expected to have an error %A, expected %A, actual %A" extras expected fileErrors)
            Expect.isEmpty (missing) (sprintf "Following lines are expected to have an error %A" missing)
        yield! testVals |> List.map (fun (f, t) -> testCase (f.ToString()) <| fun () -> inner (f, t))
    ]

[<Tests>]
let folderTests =
    testList "validation" [
        testFolder "./testfiles/validationtests/interfacetests" "interface" false "en-GB"
        testFolder "./testfiles/validationtests/gfxtests" "gfx" false "en-GB"
        testFolder "./testfiles/validationtests/scopetests" "scopes" false "en-GB"
        testFolder "./testfiles/validationtests/variabletests" "variables" false "en-GB"
        testFolder "./testfiles/validationtests/modifiertests" "modifiers" false "en-GB"
        testFolder "./testfiles/validationtests/eventtests" "events" false "en-GB"
        testFolder "./testfiles/validationtests/weighttests" "weights" false "en-GB"
        testFolder "./testfiles/multiplemodtests" "multiple" false "en-GB"
        testFolder "./testfiles/configtests/validationtests" "configrules" true "en-GB"
        testFolder "./testfiles/configtests/validationtests" "configrules" true "ru-RU"
    ]

[<Tests>]
let specialtests =
    testList "log" [
        testCase "modifiers" <| fun () ->
            let modfile = SetupLogParser.parseLogsFile "./testfiles/scriptedorstatictest/setup.log"
            (modfile |> (function |Failure(e, _,_) -> eprintfn "%s" e |_ -> ()))
            let modifiers = (modfile |> (function |Success(p, _, _) -> SetupLogParser.processLogs p))
            let settings = emptyStellarisSettings "./testfiles/scriptedorstatictest"
            let stl = STLGame({settings with embedded = {settings.embedded with modifiers = modifiers}}) :> IGame<STLComputedData>
            // let stl = STLGame("./testfiles/scriptedorstatictest/", FilesScope.All, "", [], [], modifiers, [], [], [STL STLLang.English], false, true, false)
            let exp = [{tag = "test"; categories = [ModifierCategory.Pop]; core = false}]
            Expect.equal (stl.StaticModifiers()) exp ""
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
        let embeddedFileNames = Assembly.GetEntryAssembly().GetManifestResourceNames() |> Array.filter (fun f -> f.Contains("embeddedtest") && (f.Contains("common") || f.Contains("localisation") || f.Contains("interface")))

        //Test serialization
        let fileManager = FileManager("./testfiles/embeddedtest/test", Some "", FilesScope.Vanilla, scriptFolders, "stellaris")
        let files = fileManager.AllFilesByPath()
        let resources = ResourceManager(STLCompute.computeSTLData (fun () -> None)).Api
        let entities = resources.UpdateFiles(files) |> List.map (fun (r, (struct (e, _))) -> r, e)
        let mkPickler (resolver : IPicklerResolver) =
            let arrayPickler = resolver.Resolve<Leaf array> ()
            let writer (w : WriteState) (ns : Lazy<Leaf array>) =
                arrayPickler.Write w "value" (ns.Force())
            let reader (r : ReadState) =
                let v = arrayPickler.Read r "value" in Lazy<Leaf array>.CreateFromValue v
            Pickler.FromPrimitives(reader, writer)
        let registry = new CustomPicklerRegistry()
        do registry.RegisterFactory mkPickler
        registry.DeclareSerializable<FParsec.Position>()
        let cache = PicklerCache.FromCustomPicklerRegistry registry
        let binarySerializer = FsPickler.CreateBinarySerializer(picklerResolver = cache)
        let data = { resources = entities; fileIndexTable = fileIndexTable}
        let pickle = binarySerializer.Pickle data

        let unpickled = binarySerializer.UnPickle pickle
        fileIndexTable <- unpickled.fileIndexTable
        let cached = unpickled.resources


        let embeddedFiles = embeddedFileNames |> List.ofArray |> List.map (fun f -> fixEmbeddedFileName f, (new StreamReader(Assembly.GetEntryAssembly().GetManifestResourceStream(f))).ReadToEnd())
        let settings = emptyStellarisSettings "./testfiles/embeddedtest/test"
        let settingsE = { settings with embedded = { settings.embedded with embeddedFiles = filelist; cachedResourceData = cached };}

        let stlE = STLGame(settingsE) :> IGame<STLComputedData>
        let stlNE = STLGame(settings) :> IGame<STLComputedData>
        let eerrors = stlE.ValidationErrors() |> List.map (fun (c, s, n, l, f, k) -> n)
        eprintfn "%A" (stlE.ValidationErrors())
        let neerrors = stlNE.ValidationErrors() |> List.map (fun (c, s, n, l, f, k) -> f, n)
        let etestVals = stlE.AllEntities() |> List.map (fun struct (e, _) -> e.filepath, getNodeComments e.entity |> List.map fst)
        let netestVals = stlNE.AllEntities() |> List.map (fun struct (e, _) -> e.filepath, getNodeComments e.entity |> List.map fst)
        let einner (file, ((nodekeys : range list)) )=
            let fileErrors = eerrors |> List.filter (fun f -> f.FileName = file )
            Expect.isEmpty (fileErrors) (sprintf "Following lines are not expected to have an error %A" fileErrors )
        yield! etestVals |> List.map (fun (f, t) -> testCase ("embed" + f.ToString()) <| fun () -> einner (f, t))
        let neinner (file, ((nodekeys : range list)) )=
            // let expected = nodekeys
            // let fileErrors = neerrors |> List.filter (fun f -> f.FileName = file )
            // let missing = remove_all expected fileErrors
            // let extras = remove_all fileErrors expected
            // Expect.isEmpty (extras) (sprintf "Following lines are not expected to have an error %A" extras )
            // Expect.isEmpty (missing) (sprintf "Following lines are expected to have an error %A" missing)
            let expected = nodekeys |> List.map (fun nk -> "", nk)
            //|> List.map (fun p -> FParsec.Position(p.StreamName, p.Index, p.Line, 1L))
            let fileErrors = neerrors |> List.filter (fun (c, f) -> f.FileName = file )
            let fileErrorPositions = fileErrors //|> List.map snd
            let missing = remove_all_by expected fileErrorPositions snd
            let extras = remove_all_by fileErrorPositions expected snd
            //eprintfn "%A" nodekeys
            Expect.isEmpty (extras) (sprintf "Following lines are not expected to have an error %A, expected %A, actual %A" extras expected fileErrors)
            Expect.isEmpty (missing) (sprintf "Following lines are expected to have an error %A" missing)

        yield! netestVals |> List.map (fun (f, t) -> testCase ("no embed" + f.ToString()) <| fun () -> neinner (f, t))

    ]

[<Tests>]
let overwriteTests =
    testList "overwrite" [
        // eprintfn "%A" filelist
        let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.2.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs p)
        let modifiers = SetupLogParser.parseLogsFile "./testfiles/validationtests/setup.log" |> (function |Success(p, _, _) -> SetupLogParser.processLogs p)
        let embeddedFileNames = Assembly.GetEntryAssembly().GetManifestResourceNames() |> Array.filter (fun f -> f.Contains("overwritetest") && (f.Contains("common") || f.Contains("localisation") || f.Contains("interface")))
        let embeddedFiles = embeddedFileNames |> List.ofArray |> List.map (fun f -> fixEmbeddedFileName f, (new StreamReader(Assembly.GetEntryAssembly().GetManifestResourceStream(f))).ReadToEnd())
        let settings = emptyStellarisSettings "./testfiles/overwritetest/test"
        let settings = { settings with embedded = { settings.embedded with triggers = triggers; effects = effects; modifiers = modifiers; embeddedFiles = embeddedFiles };}
        let stl = STLGame(settings) :> IGame<STLComputedData>
        let errors = stl.ValidationErrors() |> List.map (fun (c, s, n, l, f, k) -> f, n) //>> (fun p -> FParsec.Position(p.StreamName, p.Index, p.Line, 1L)))
        let testVals = stl.AllEntities() |> List.map (fun struct (e, _) -> e.filepath, getNodeComments e.entity |> List.map fst)
        let inner (file, ((nodekeys : range list)) )=
            let expected = nodekeys  //|> List.map (fun p -> FParsec.Position(p.StreamName, p.Index, p.Line, 1L))
            let fileErrors = errors |> List.filter (fun (c, f) -> f.FileName = file )
            let fileErrorPositions = fileErrors |> List.map snd
            let missing = remove_all expected fileErrorPositions
            let extras = remove_all fileErrorPositions expected
            //eprintfn "%A" fileErrors
            Expect.isEmpty (extras) (sprintf "Following lines are not expected to have an error %A, all %A" extras expected )
            Expect.isEmpty (missing) (sprintf "Following lines are expected to have an error %A" missing)
        yield! testVals |> List.map (fun (f, t) -> testCase (f.ToString()) <| fun () -> inner (f, t))
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