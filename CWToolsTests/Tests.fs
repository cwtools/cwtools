module Tests

open Expecto
open Expecto.Logging
open Expecto.Logging.Message
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
open CWTools.Utilities
open CWTools.Games.Files
open CWTools.Games.Stellaris
open CWTools.Games.Stellaris.STLLookup
open System.Threading
open System.Globalization
open CWTools.Validation.Stellaris
open MBrace.FsPickler
open System.Text
open CWTools.Parser.CKPrinter
open CWTools.Common.NewScope
open CWTools.Utilities.Utils


let emptyEmbeddedSettings = {
        triggers = []
        effects = []
        modifiers = []
        embeddedFiles = []
        cachedResourceData = []
        localisationCommands = Legacy ([], [])
        eventTargetLinks = []
        cachedRuleMetadata = None
}
let emptyStellarisSettings (rootDirectory) = {
    rootDirectories = [WD { name = "test"; path = rootDirectory;}]
    modFilter = None
    validation = {
        validateVanilla = false
        experimental = true
        langs = [STL STLLang.English]
    }
    rules = None
    embedded = FromConfig ([], [])
    scriptFolders = None
    excludeGlobPatterns = None
    maxFileSize = None
}
let emptyImperatorSettings (rootDirectory) = {
    rootDirectories = [WD { name = "test"; path = rootDirectory;}]
    modFilter = None
    validation = {
        validateVanilla = false
        experimental = true
        langs = [IR IRLang.English]
    }
    rules = None
    embedded = FromConfig ([], [])
    scriptFolders = None
    excludeGlobPatterns = None
    maxFileSize = None
}

let getAllTestLocs node =
    let fNode = (fun (x:Node) (req, notreq) ->
        let required = x.Values |> List.filter ( fun l -> l.Value.ToRawString() = "test_required") |> List.map (fun l -> l.Position)
        let notrequired = x.Values |> List.filter ( fun l -> l.Value.ToRawString() = "test_optional") |> List.map (fun l -> l.Position)
        required @ req, notrequired @ notreq)
    let fCombine = (fun (r,n) (r2, n2) -> (r@r2, n@n2))
    node |> (foldNode2 fNode fCombine ([],[]))

let getNodeComments (clause : IClause) =
    let findComments t s (a : Child) =
            match (s, a) with
            | ((b, c), _) when b -> (b, c)
            | ((_, c), CommentC (_, nc)) when nc.StartsWith("#") -> (false, c)
            | ((_, c), CommentC (_, nc)) when nc.StartsWith("@") -> (false, c)
            | ((_, c), CommentC (_, nc)) -> (false, nc::c)
            | ((_, c), NodeC n) when n.Position = t -> (true, c)
            | ((_, c), LeafC v) when v.Position = t -> (true, c)
            | ((_, c), LeafValueC v) when v.Position = t -> (true, c)
            | ((_, c), ValueClauseC vc) when vc.Position = t -> (true, c)
            | _ -> (false, [])
            // | ((_, c), LeafValueC lv) when lv.Position = t -> (true, c)
            // | ((_, _), _) -> (false, [])
    let fNode = (fun (clause : IClause) (children) ->
        let one = clause.Leaves |> Seq.map (fun e -> e.Position, clause.AllArray |> Array.fold (findComments e.Position) (false, []) |> snd) |> List.ofSeq
        //log "%s %A" node.Key (node.All |> List.rev)
        //log "%A" one
        let two = clause.Nodes |> Seq.map (fun e -> e.Position, clause.AllArray |> Array.fold (findComments e.Position) (false, []) |> snd |> (fun l -> (l))) |> List.ofSeq
        let three = clause.LeafValues |> Seq.toList |> List.map (fun e -> e.Position, clause.AllArray |> Array.fold (findComments e.Position) (false, []) |> snd)
        let four = clause.ValueClauses |> Seq.toList |> List.map (fun e -> e.Position, clause.AllArray |> Array.fold (findComments e.Position) (false, []) |> snd)
        let new2 = one @ two @ three @ four |> List.filter (fun (p, c) -> not (List.isEmpty c))
        new2 @ children
            )
    let fCombine = (@)
    clause |> (foldClause2 fNode fCombine [])

// [<Tests>]
// let testsConfig =
//     testList "testFindComments" [
//         ftestCase "basic" <| fun () ->
//             let testString = """
// #error
// test = test
// #error
// test2 = test
// test3 = test
// test
// """
//             let parsed = CWTools.Parser.CKParser.parseString testString "test"
//             match parsed with
//             |Success(res,_,_) ->
//                 let node = (STLProcess.shipProcess.ProcessNode() "root" (range.Zero) res)
//                 let comments = getNodeComments (node)
//                 eprintfn "%A" comments
//             |_ -> ()
//     ]

let getCompletionTests (clause : IClause) =
    let findComments t s (a : Child) =
            match (s, a) with
            | ((b, c), _) when b -> (b, c)
            | ((_, c), CommentC (_, nc)) when nc.StartsWith("@") -> (false, nc::c)
            | ((_, c), CommentC _) -> (false, c)
            | ((_, c), NodeC n) when n.Position = t -> (true, c)
            | ((_, c), LeafC v) when v.Position = t -> (true, c)
            | ((_, c), LeafValueC v) when v.Position = t -> (true, c)
            | ((_, c), ValueClauseC vc) when vc.Position = t -> (true, c)
            | _ -> (false, [])
            // | ((_, c), LeafValueC lv) when lv.Position = t -> (true, c)
            // | ((_, _), _) -> (false, [])
    let fNode = (fun (clause : IClause) (children) ->
        let one = clause.Leaves |> Seq.map (fun e -> e.Position, clause.AllArray |> Array.fold (findComments e.Position) (false, []) |> snd) |> List.ofSeq
        //log "%s %A" node.Key (node.All |> List.rev)
        //log "%A" one
        let two = clause.Nodes |> Seq.map (fun e -> e.Position, clause.AllArray |> Array.fold (findComments e.Position) (false, []) |> snd |> (fun l -> (l))) |> List.ofSeq
        let three = clause.LeafValues |> Seq.toList |> List.map (fun e -> e.Position, clause.AllArray |> Array.fold (findComments e.Position) (false, []) |> snd)
        let four = clause.ValueClauses |> Seq.toList |> List.map (fun e -> e.Position, clause.AllArray |> Array.fold (findComments e.Position) (false, []) |> snd)
        let new2 = one @ two @ three @ four |> List.filter (fun (p, c) -> not (List.isEmpty c))
        new2 @ children
            )
    let fCombine = (@)
    let res = clause |> (foldClause2 fNode fCombine []) |> List.collect (fun (r, sl) -> sl |> List.map (fun s -> r, s))
    let convertResToCompletionTest ((pos : range), (comment : string)) =
        let [| option; column; text; |] = comment.Split(' ', 3)
        let negate = option = "@!"
        let lowscore = option = "@?"
        let pos = mkPos pos.Start.Line (pos.Start.Column + (int column) - 1)
        pos, text, negate, lowscore
    res |> List.map convertResToCompletionTest

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

let locErrorCodes = [ "CW225"; "CW226"; "CW254"; "CW255"; "CW256"; "CW257"; "CW258"; "CW259"; "CW260"]

[<Tests>]
let tests =
    testList "localisation" [
        testList "no loc" [
                let configtext = ["./testfiles/localisationtests/test.cwt", File.ReadAllText "./testfiles/localisationtests/test.cwt"]
                let configtext = ("./testfiles/localisationtests/localisation.cwt", File.ReadAllText "./testfiles/localisationtests/localisation.cwt")::configtext
                let settings = emptyStellarisSettings "./testfiles/localisationtests/gamefiles"
                let settings = { settings with rules = Some { ruleFiles = configtext; validateRules = false; debugRulesOnly = false; debugMode = false} }
                // UtilityParser.initializeScopes None (Some defaultScopeInputs)
                let stl = STLGame(settings) :> IGame<STLComputedData>
                let parseErrors = stl.ParserErrors()
                let errors = stl.LocalisationErrors(true, true) |> List.map (fun e -> e.range)
                let entities = stl.AllEntities()
                let testLocKeys = entities |> List.map (fun struct (e, _) -> e.filepath, getLocTestInfo e.entity)
                let nodeComments = entities |> List.collect (fun struct (e, _) -> getNodeComments e.entity) |> List.map fst
                logInfo (sprintf "%A" (entities |> List.head |> (fun struct (e, _)  -> api.prettyPrintStatements (e.entity.ToRaw))))
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
                let configtext = ["./testfiles/localisationtests/test.cwt", File.ReadAllText "./testfiles/localisationtests/test.cwt"]
                let configtext = ("./testfiles/localisationtests/localisation.cwt", File.ReadAllText "./testfiles/localisationtests/localisation.cwt")::configtext
                let locfiles = "localisation/l_english.yml", File.ReadAllText("./testfiles/localisationtests/localisation/l_english.yml")
                // let locCommands = STLParser.loadLocCommands "./testfiles/localisationtests/test.cwt" (File.ReadAllText "./testfiles/localisationtests/test.cwt")
                // UtilityParser.initializeScopes None (Some defaultScopeInputs)

                let settings = emptyStellarisSettings "./testfiles/localisationtests/gamefiles"
                let settings = { settings with embedded = FromConfig ([locfiles], []);
                                            validation = {settings.validation with langs = [STL STLLang.English; STL STLLang.German] };
                                            rules = Some { ruleFiles = configtext; validateRules = false; debugRulesOnly = false; debugMode = false} }
                let stl = STLGame(settings) :> IGame<STLComputedData>
                let parseErrors = stl.ParserErrors()
                yield testCase ("parse") <| fun () -> Expect.isEmpty parseErrors (parseErrors |> List.tryHead |> Option.map (sprintf "%A") |> Option.defaultValue "")

                let errors = stl.LocalisationErrors(true, true) |> List.map (fun e -> e.range)
                let testLocKeys = stl.AllEntities() |> List.map (fun struct (e, _) -> e.filepath, getLocTestInfo e.entity)
                let inner (file, ((req : range list), (noreq : range list), (nodekeys : range list) ))=
                    let missing = req |> List.filter (fun r -> not (errors |> List.contains r))
                    let extra = noreq |> List.filter (fun r -> errors |> List.contains r)
                    Expect.isEmpty missing (sprintf "Missing required despite having key %s" file)
                    Expect.isEmpty (extra) (sprintf "Incorrect required %s" file)
                yield! testLocKeys |> List.map (fun (f, t) -> testCase (f.ToString()) <| fun () -> inner (f, t))
                // eprintfn "%A" (stl.LocalisationErrors(true))
                let globalLocError = stl.LocalisationErrors(true, true) |> List.filter (fun e -> List.contains (e.code) locErrorCodes)
                yield testCase "globalLoc" <| fun () ->
                    Expect.hasCountOf globalLocError 10u (fun f -> true) (sprintf "wrong number of errors %A" globalLocError)
            ]
    ]

let rec getAllFolders dirs =
    if Seq.isEmpty dirs then Seq.empty else
        seq { yield! dirs |> Seq.collect Directory.EnumerateDirectories
              yield! dirs |> Seq.collect Directory.EnumerateDirectories |> getAllFolders }

let getAllFoldersUnion dirs =
    seq {
        yield! dirs
        yield! getAllFolders dirs
    }

let configFilesFromDir folder =
    let configFiles =
        if Directory.Exists folder
        then
            getAllFoldersUnion ([folder] |> Seq.ofList)
                                |> Seq.collect (Directory.EnumerateFiles)
        else
            if File.Exists folder
            then [folder] |> Seq.ofList
            else Seq.empty
    configFiles |> List.ofSeq |> List.filter (fun f -> Path.GetExtension f = ".cwt")
                |> List.map (fun f -> f, File.ReadAllText f)

let testFolder folder testsname config configValidate configfile configOnly configLoc stl (culture : string) =
    testList (testsname + culture) [
        Thread.CurrentThread.CurrentCulture <- CultureInfo(culture);
        Thread.CurrentThread.CurrentUICulture <- CultureInfo(culture);
        let configtext = if config then configFilesFromDir configfile else []
        // configtext |> Seq.iter (fun (fn, _) -> eprintfn "%s" fn)
        let completionTest (game : IGame) filename filetext (pos : pos, text : string, negate : bool, lowscore : bool) =
            let getLabel =
                function
                | Simple(label, score)
                | Detailed(label, _, score )
                | Snippet(label, _, _, score) -> label, score
            let compRes = game.Complete pos filename filetext |> List.map getLabel
            let labels = compRes |> List.map fst
            let lowscorelables = compRes |> List.choose (fun (label, score) -> score |> Option.bind (fun s -> if s <= 10 then Some label else None))
            match negate, lowscore with
            | true, _ ->
                Expect.hasCountOf (labels) 0u ((=) text) (sprintf "Completion shouldn't contain value %s at %A in %s" text pos filename)
            | false, true ->
                logInfo (sprintf "ct %A" compRes)
                Expect.contains lowscorelables text (sprintf "Incorrect completion values (missing low score) at %A in %s" pos filename)
            | false, false ->
                Expect.contains labels text (sprintf "Incorrect completion values at %A in %s, %A" pos filename labels)
                Expect.isNonEmpty labels (sprintf "No completion results, expected %s" text)

        let completionTestPerFile (game : IGame) (filename : string, tests) =
            let filetext = File.ReadAllText filename
            tests |> List.iter (completionTest game filename filetext)
        // let stl = STLGame(folder, FilesScope.All, "", triggers, effects, modifiers, [], [configtext], [STL STLLang.English], false, true, config)
        let (game : IGame), errors, testVals, completionVals, parseErrors =
            if stl = 1
            then
                let configtext = ("./testfiles/validationtests/trigger_docs.log", File.ReadAllText "./testfiles/validationtests/trigger_docs.log")::configtext
                // configtext |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
                //             |> (fun f -> UtilityParser.initializeScopes f (Some defaultScopeInputs) )

                // let eventTargetLinks =
                //             configtext |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "links.cwt")
                //                     |> Option.map (fun (fn, ft) -> UtilityParser.loadEventTargetLinks (scopeManager.AnyScope) (scopeManager.ParseScope()) (scopeManager.AllScopes) fn ft)
                //                     |> Option.defaultValue (Scopes.STL.scopedEffects() |> List.map SimpleLink)
                // let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.1.0.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs (scopeManager.ParseScopes) p)
                // let modifiers = SetupLogParser.parseLogsFile "./testfiles/validationtests/setup.log" |> (function |Success(p, _, _) -> SetupLogParser.processLogs p)
                let settings = emptyStellarisSettings folder
                let settings = { settings with rules = if config then Some { ruleFiles = configtext; validateRules = configValidate; debugRulesOnly = configOnly; debugMode = false} else None}
                let stl = STLGame(settings) :> IGame<STLComputedData>
                let errors = stl.ValidationErrors() @ (if configLoc then stl.LocalisationErrors(false, false) else []) |> List.map (fun e -> e.message, e.range) //>> (fun p -> FParsec.Position(p.StreamName, p.Index, p.Line, 1L)))
                let testVals = stl.AllEntities() |> List.map (fun struct (e, _) -> e.filepath, getNodeComments e.entity |> List.collect (fun (r, cs) -> cs |> List.map (fun _ -> r)))
                let completionTests =
                                stl.AllEntities()
                                |> List.map (fun struct (e, _) ->
                                    e.filepath,
                                    getCompletionTests e.entity
                                    )
                (stl :> IGame), errors, testVals, completionTests, (stl.ParserErrors())
            else if stl = 0 then
                let configtext = ("./testfiles/configtests/rulestests/IR/triggers.log", File.ReadAllText "./testfiles/configtests/rulestests/IR/triggers.log")::configtext
                let configtext = ("./testfiles/configtests/rulestests/IR/effects.log", File.ReadAllText "./testfiles/configtests/rulestests/IR/effects.log")::configtext
                // let triggers = JominiParser.parseTriggerFilesRes "./testfiles/configtests/rulestests/IR/triggers.log" |> CWTools.Parser.JominiParser.processTriggers IRConstants.parseScopes
                // let effects = JominiParser.parseEffectFilesRes "./testfiles/configtests/rulestests/IR/effects.log" |> CWTools.Parser.JominiParser.processEffects IRConstants.parseScopes
                // eprintfn "testtest %A" triggers
                // configtext |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
                //             |> (fun f -> UtilityParser.initializeScopes f None )

                // let eventTargetLinks =
                //             configtext |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "links.cwt")
                //                     |> Option.map (fun (fn, ft) -> UtilityParser.loadEventTargetLinks IRConstants.Scope.Any IRConstants.parseScope IRConstants.allScopes fn ft)
                //                     |> Option.defaultValue (Scopes.IR.scopedEffects |> List.map SimpleLink)
                let settings = emptyImperatorSettings folder
                let settings = { settings with rules = if config then Some { ruleFiles = configtext; validateRules = configValidate; debugRulesOnly = configOnly; debugMode = false} else None}
                let ir = CWTools.Games.IR.IRGame(settings) :> IGame<IRComputedData>
                let errors = ir.ValidationErrors() @ (if configLoc then ir.LocalisationErrors(false, false) else []) |> List.map (fun e -> e.message, e.range) //>> (fun p -> FParsec.Position(p.StreamName, p.Index, p.Line, 1L)))
                let testVals = ir.AllEntities()
                                |> List.map (fun struct (e, _) ->
                                    e.filepath,
                                    getNodeComments e.entity |> List.collect (fun (r, cs) -> cs |> List.map (fun _ -> r))
                                    )
                let completionTests =
                                ir.AllEntities()
                                |> List.map (fun struct (e, _) ->
                                    e.filepath,
                                    getCompletionTests e.entity
                                    )
                (ir :> IGame), errors, testVals, completionTests, (ir.ParserErrors())
            else
                // let configtext = ("./testfiles/configtests/rulestests/IR/triggers.log", File.ReadAllText "./testfiles/configtests/rulestests/IR/triggers.log")::configtext
                // let configtext = ("./testfiles/configtests/rulestests/IR/effects.log", File.ReadAllText "./testfiles/configtests/rulestests/IR/effects.log")::configtext
                // let triggers = JominiParser.parseTriggerFilesRes "./testfiles/configtests/rulestests/IR/triggers.log" |> CWTools.Parser.JominiParser.processTriggers IRConstants.parseScopes
                // let effects = JominiParser.parseEffectFilesRes "./testfiles/configtests/rulestests/IR/effects.log" |> CWTools.Parser.JominiParser.processEffects IRConstants.parseScopes
                // eprintfn "testtest %A" triggers
                // configtext |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
                //             |> (fun f -> UtilityParser.initializeScopes f None )

                // let eventTargetLinks =
                //             configtext |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "links.cwt")
                //                     |> Option.map (fun (fn, ft) -> UtilityParser.loadEventTargetLinks IRConstants.Scope.Any IRConstants.parseScope IRConstants.allScopes fn ft)
                //                     |> Option.defaultValue (Scopes.IR.scopedEffects |> List.map SimpleLink)
                let settings = emptyImperatorSettings folder
                let settings = { settings with rules = if config then Some { ruleFiles = configtext; validateRules = configValidate; debugRulesOnly = configOnly; debugMode = false} else None}
                let hoi4 = CWTools.Games.HOI4.HOI4Game(settings) :> IGame<HOI4ComputedData>
                let errors = hoi4.ValidationErrors() @ (if configLoc then hoi4.LocalisationErrors(false, false) else []) |> List.map (fun e -> e.message, e.range) //>> (fun p -> FParsec.Position(p.StreamName, p.Index, p.Line, 1L)))
                let testVals = hoi4.AllEntities()
                                |> List.map (fun struct (e, _) ->
                                    e.filepath,
                                    getNodeComments e.entity |> List.collect (fun (r, cs) -> cs |> List.map (fun _ -> r))
                                    )
                let completionTests =
                                hoi4.AllEntities()
                                |> List.map (fun struct (e, _) ->
                                    e.filepath,
                                    getCompletionTests e.entity
                                    )
                (hoi4 :> IGame), errors, testVals, completionTests, (hoi4.ParserErrors())

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
        // eprintfn "ss %s %s" folder testsname
        yield testCase (sprintf "parse %s" folder) <| fun () -> Expect.isEmpty parseErrors (parseErrors |> List.tryHead |> Option.map (sprintf "%A") |> Option.defaultValue "")
        yield! testVals |> List.map (fun (f, t) -> testCase (f.ToString()) <| fun () -> inner (f, t))
        yield! completionVals |> List.map (fun (f, t) -> testCase ("Completion " + f.ToString()) <| fun() -> completionTestPerFile game (f, t))
    ]

let testSubdirectories stl dir =
    let dirs = Directory.EnumerateDirectories dir
    dirs |> Seq.map (fun d -> testFolder d "detailedconfigrules" true true (d) true true stl "en-GB")
[<Tests>]
let folderTests =
    testList "validation" [
        testFolder "./testfiles/validationtests/interfacetests" "interface" false false "" false false 1 "en-GB"
        testFolder "./testfiles/validationtests/gfxtests" "gfx" false false "" false false 1 "en-GB"
        // testFolder "./testfiles/validationtests/scopetests" "scopes" false "" false false "en-GB"
        // testFolder "./testfiles/validationtests/variabletests" "variables" true false "./testfiles/stellarisconfig" false false "en-GB"
        // testFolder "./testfiles/validationtests/modifiertests" "modifiers" false "" false false "en-GB"
        testFolder "./testfiles/validationtests/eventtests" "events" true false "./testfiles/stellarisconfig" false false 1 "en-GB"
        // testFolder "./testfiles/validationtests/weighttests" "weights" false "" false false "en-GB"
        testFolder "./testfiles/multiplemodtests" "multiple" true true "./testfiles/multiplemodtests/test.cwt" false false 1 "en-GB"
        testFolder "./testfiles/configtests/validationtests" "configrules" true true "./testfiles/configtests/test.cwt" false false 1 "en-GB"
        testFolder "./testfiles/configtests/validationtests" "configrules" true true "./testfiles/configtests/test.cwt" false false 1 "ru-RU"
        // yield! testSubdirectories "./testfiles/configtests/rulestests"
        // testFolder "./testfiles/configtests/rulestests" "detailedconfigrules" true "./testfiles/configtests/rulestests/rules.cwt" true "en-GB"
    ]
//[<Tests>]
//let stlAllSubfolderTests = testList "validation all stl" (testSubdirectories true "./testfiles/configtests/rulestests/All" |> List.ofSeq)
//[<Tests>]
//let irAllSubfolderTests = testList "validation all ir" (testSubdirectories false "./testfiles/configtests/rulestests/All" |> List.ofSeq)
[<Tests>]
let stlSubfolderTests = testList "validation stl" (testSubdirectories 1 "./testfiles/configtests/rulestests/STL" |> List.ofSeq)
[<Tests>]
let irSubfolderTests = testList "validation ir" (testSubdirectories 0 "./testfiles/configtests/rulestests/IR" |> List.ofSeq)
[<Tests>]
let hoi4SubfolderTests = testList "validation hoi4" (testSubdirectories 2 "./testfiles/configtests/rulestests/HOI4" |> List.ofSeq)

[<Tests>]
let specialtests =
    testList "log" [
        testCase "modifiers" <| fun () ->
            let configtext = [("./testfiles/scriptedorstatictest/setup.log", File.ReadAllText "./testfiles/scriptedorstatictest/setup.log")]
            let modfile = SetupLogParser.parseLogsFile "./testfiles/scriptedorstatictest/setup.log"
                        // (modfile |> (function |Failure(e, _,_) -> eprintfn "%s" e |_ -> ()))
            let modifiers = (modfile |> (function |Success(p, _, _) -> SetupLogParser.processLogs p))
            let settings = emptyStellarisSettings "./testfiles/scriptedorstatictest"
            // UtilityParser.initializeScopes None (Some defaultScopeInputs)
            let stl = STLGame({settings with rules = Some {  ruleFiles = configtext; validateRules = false; debugRulesOnly = false; debugMode = false   };
                                                embedded = ManualSettings { emptyEmbeddedSettings with modifiers = modifiers}}) :> IGame<STLComputedData>
            // let stl = STLGame("./testfiles/scriptedorstatictest/", FilesScope.All, "", [], [], modifiers, [], [], [STL STLLang.English], false, true, false)
            let exp = [{tag = "test"; categories = [modifierCategoryManager.ParseModifier() "pop"] }]
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
        let fileManager = FileManager([WD { name = "test"; path = "./testfiles/embeddedtest/test"}], Some "", scriptFolders, "stellaris", Encoding.UTF8, [], 2000000)
        let files = fileManager.AllFilesByPath()
        let resources : IResourceAPI<STLComputedData> = ResourceManager<STLComputedData>(Compute.STL.computeSTLData (fun () -> None), Compute.STL.computeSTLDataUpdate (fun () -> None), Encoding.UTF8, Encoding.GetEncoding(1252)).Api
        let entities = resources.UpdateFiles(files) |> List.choose (fun (r, e) -> e |> function |Some e2 -> Some (r, e2) |_ -> None) |> List.map (fun (r, (struct (e, _))) -> r, e)
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
        let data = { resources = entities; fileIndexTable = fileIndexTable; files = []; stringResourceManager = StringResource.stringManager}
        let pickle = binarySerializer.Pickle data

        let unpickled = binarySerializer.UnPickle pickle
        fileIndexTable <- unpickled.fileIndexTable
        let cached = unpickled.resources


        let embeddedFiles = embeddedFileNames |> List.ofArray |> List.map (fun f -> fixEmbeddedFileName f, (new StreamReader(Assembly.GetEntryAssembly().GetManifestResourceStream(f))).ReadToEnd())
        let settings = emptyStellarisSettings "./testfiles/embeddedtest/test"
        let settingsE = { settings with embedded = ManualSettings {emptyEmbeddedSettings with embeddedFiles = filelist; cachedResourceData = cached };}
        // UtilityParser.initializeScopes None (Some defaultScopeInputs)

        let stlE = STLGame(settingsE) :> IGame<STLComputedData>
        let stlNE = STLGame(settings) :> IGame<STLComputedData>
        let eerrors = stlE.ValidationErrors() |> List.map (fun e -> e.range)
        // eprintfn "%A" (stlE.ValidationErrors())
        let neerrors = stlNE.ValidationErrors() |> List.map (fun e -> e.message, e.range)
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
        let configtext = ["./testfiles/overwritetest/test.cwt", File.ReadAllText "./testfiles/overwritetest/test.cwt"]
        let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.2.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs (scopeManager.ParseScopes) p)
        let modifiers = SetupLogParser.parseLogsFile "./testfiles/validationtests/setup.log" |> (function |Success(p, _, _) -> SetupLogParser.processLogs p)
        let embeddedFileNames = Assembly.GetEntryAssembly().GetManifestResourceNames() |> Array.filter (fun f -> f.Contains("overwritetest") && (f.Contains("common") || f.Contains("localisation") || f.Contains("interface")))
        let embeddedFiles = embeddedFileNames |> List.ofArray |> List.map (fun f -> fixEmbeddedFileName f, (new StreamReader(Assembly.GetEntryAssembly().GetManifestResourceStream(f))).ReadToEnd())
        let settings = emptyStellarisSettings "./testfiles/overwritetest/test"
        let settings = { settings with embedded = ManualSettings {emptyEmbeddedSettings with triggers = triggers; effects = effects; modifiers = modifiers; embeddedFiles = embeddedFiles };
                                            rules = Some { ruleFiles = configtext; validateRules = true; debugRulesOnly = false; debugMode = false}}
        // UtilityParser.initializeScopes None (Some defaultScopeInputs)
        let stl = STLGame(settings) :> IGame<STLComputedData>
        let errors = stl.ValidationErrors() |> List.map (fun e -> e.message, e.range) //>> (fun p -> FParsec.Position(p.StreamName, p.Index, p.Line, 1L)))
        let testVals = stl.AllEntities() |> List.map (fun struct (e, _) -> e.filepath, getNodeComments e.entity |> List.map fst)
        let inner (file, ((nodekeys : range list)) )=
            let expected = nodekeys  //|> List.map (fun p -> FParsec.Position(p.StreamName, p.Index, p.Line, 1L))
            let fileErrors = errors |> List.filter (fun (c, f) -> f.FileName = file )
            let fileErrorPositions = fileErrors |> List.map snd
            let missing = remove_all expected fileErrorPositions
            let extras = remove_all fileErrorPositions expected
            //eprintfn "%A" fileErrors
            Expect.isEmpty (extras) (sprintf "Following lines are not expected to have an error %A, all %A, actual %A" extras expected fileErrors )
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
