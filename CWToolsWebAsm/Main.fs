module CWToolsTests

open Expecto
open System.Text
open CWTools.Parser.DocsParser
open CWTools.Games
open CWTools.Parser.Types
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
open CWTools.Games.Files
open CWTools.Games.Stellaris
open CWTools.Games.Stellaris.STLLookup
open System.Threading
open System.Globalization
open CWTools.Common.STLConstants
open System
open CWTools.Utilities.Position
open CWTools.Validation.Rules
open CWTools.Parser.ConfigParser
open CWTools.Utilities
open CWTools.Utilities.Utils
open CWTools.Process.STLScopes

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
        localisationCommands = []
        eventTargetLinks = []
    }
    scriptFolders = None
    excludeGlobPatterns = None
}
let rec getAllFolders dirs =
    if Seq.isEmpty dirs then Seq.empty else
        seq { yield! dirs |> Seq.collect Directory.EnumerateDirectories
              yield! dirs |> Seq.collect Directory.EnumerateDirectories |> getAllFolders }
let getAllFoldersUnion dirs =
    seq {
        yield! dirs
        yield! getAllFolders dirs
    }

let perf(b) =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.2.txt" |> (function |Success(p, _, _) -> (DocsParser.processDocs parseScopes) p)
    let configFiles = (if Directory.Exists "./testfiles/performancetest2/.cwtools" then getAllFoldersUnion (["./testfiles/performancetest2/.cwtools"] |> Seq.ofList) else Seq.empty) |> Seq.collect (Directory.EnumerateFiles)
    let configFiles = configFiles |> List.ofSeq |> List.filter (fun f -> Path.GetExtension f = ".cwt")
    let configs = configFiles |> List.map (fun f -> f, File.ReadAllText(f))
    let settings = emptyStellarisSettings "./testfiles/performancetest/"
    let settings = {settings with embedded = {settings.embedded with triggers = triggers; effects = effects};
                                    rules = Some { validateRules = true; ruleFiles = configs; debugRulesOnly = false; debugMode = false}}
    let stl = STLGame(settings) :> IGame<STLComputedData, Scope, Modifier>
    // let stl = STLGame("./testfiles/performancetest/", FilesScope.All, "", triggers, effects, [], [], configs, [STL STLLang.English], false, true, true)
    if b then
        let errors = stl.ValidationErrors() |> List.map (fun (c, s, n, l, f, k) -> n)
        let testVals = stl.AllEntities()
        ()
    else ()
    eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    ()

let perf2(b) =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.2.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs parseScopes p)
    let configFiles = (if Directory.Exists "./testfiles/performancetest2/.cwtools" then getAllFoldersUnion (["./testfiles/performancetest2/.cwtools"] |> Seq.ofList) else Seq.empty) |> Seq.collect (Directory.EnumerateFiles)
    let configFiles = configFiles |> List.ofSeq |> List.filter (fun f -> Path.GetExtension f = ".cwt")
    let configs = configFiles |> List.map (fun f -> f, File.ReadAllText(f))
    let settings = emptyStellarisSettings "./testfiles/performancetest2/"
    let settings = {settings with embedded = {settings.embedded with triggers = triggers; effects = effects};
                                    rules = Some { validateRules = true; ruleFiles = configs; debugRulesOnly = false; debugMode = false}}
    let stl = STLGame(settings) :> IGame<STLComputedData, Scope, Modifier>

    // let stl = STLGame("./testfiles/performancetest2/", FilesScope.All, "", triggers, effects, [], [], configs, [STL STLLang.English], false, true, true)
    if b then
        let errors = stl.ValidationErrors() |> List.map (fun (c, s, n, l, f, k) -> n)
        let testVals = stl.AllEntities()
        stl.RefreshCaches()
        ()
    else ()
    eprintfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    ()

let testString = """
namespace = test



#One event

country_event = {

        id = test.1

    desc = "test description"

}

#Another event

country_event = {

    id = test.2

desc = "test 2 description"

}"""
let effectMap = Microsoft.FSharp.Collections.Tagged.Map<string,Effect,InsensitiveStringComparer>.Create(InsensitiveStringComparer(), [])

let leftScope = RootRule.AliasRule("effect", (NodeRule((ScopeField Scope.Any), [LeafRule ((AliasField "effect"), (AliasField "Effect")), optionalMany]), optionalMany))
let eopEffect = RootRule.AliasRule("effect", (NodeRule((ValueField (ValueType.Specific (StringResource.stringManager.InternIdentifierToken "every_owned_planet"))), [LeafRule ((AliasField "effect"), (AliasField "Effect")), optionalMany]), {optionalMany with pushScope = Some Scope.Planet} ))
let logEffect = RootRule.AliasRule("effect", (LeafRule((NewField.ValueField (ConfigParser.ValueType.Specific (StringResource.stringManager.InternIdentifierToken "log"))), (ValueField (ValueType.Bool))), {optionalMany with pushScope = Some Scope.Planet} ))

let test() =

    let input = "create_starbase = {\n\
                 effect = {\n\
                 every_owned_planet = {\n\
                 prev = { \n\
                 }\n\
                 }\n\
                 }\n\
                 }"
    match CKParser.parseString input "test.txt" with
    |Success(r, _, _) ->
        let node = (STLProcess.shipProcess.ProcessNode() "root" (range.Zero) r)
        let entity = { filepath = "events/test.txt"; logicalpath = "events/test.txt"; entity = node; validate = true; entityType = EntityType.Events; overwrite = Overwrite.No}
        let rules = RuleApplicator<Scope>([TypeRule ("create_starbase", ConfigParser.createStarbase); eopEffect; leftScope], [ConfigParser.createStarbaseTypeDef], Map.empty, Map.empty, Map.empty, [], Set.empty, effectMap, effectMap, effectMap, Scope.Any, changeScope, defaultContext, STL STLLang.Default)
        let foldRules = FoldRules<Scope>([TypeRule ("create_starbase", ConfigParser.createStarbase); eopEffect; leftScope], [ConfigParser.createStarbaseTypeDef], Map.empty, Map.empty, Map.empty, [], Set.empty, effectMap, effectMap, effectMap, rules, changeScope, defaultContext, Scope.Any,  STL STLLang.Default)
        // let comp = CompletionService([TypeRule ("create_starbase", ConfigParser.createStarbase)], [ConfigParser.createStarbaseTypeDef], Map.empty, Map.empty, [], Set.empty, [], [])
        let pos = mkPos 4 9
        let suggestions = foldRules.GetInfo (pos, entity)
        match suggestions with
        |None -> Expect.isTrue false "info failed"
        |Some (context, _) ->
            let scopes = context.Scopes
            let expected = [Scope.Country; Scope.Planet; Scope.Country;]
            Expect.sequenceEqual scopes expected "Scopes should match"
        Console.WriteLine("Tests passed")

[<EntryPoint>]
let main argv =
    printfn "Hello from F#!"
            //Support UTF-8

    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    let parsed = CWTools.Parser.CKParser.parseEventString testString "test"
    let eventFile = CWTools.Parser.CKParser.getSuccess parsed
    let processed = CK2Process.processEventFile(eventFile)
    let output = processed.ToRaw
    Console.WriteLine(CKPrinter.printKeyValueList output 0)
    test()
    0


    // let config = defaultConfig // with ``parallel`` = false}
    // Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    // CultureInfo.DefaultThreadCurrentCulture <-CultureInfo("ru-RU");
    // CultureInfo.DefaultThreadCurrentUICulture <-CultureInfo("ru-RU");
    // Thread.CurrentThread.CurrentCulture <- CultureInfo("ru-RU");
    // Thread.CurrentThread.CurrentUICulture <- CultureInfo("ru-RU");
    // CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose
    // if Array.tryHead argv = Some "p"
    // then perf(true); 0
    // elif Array.tryHead argv = Some "u"
    // then
    //     perf2(true);
    //     perf2(true); 0
    // elif Array.tryHead argv = Some "i"
    // then
    //     perf2(true); 0
    // elif Array.tryHead argv = Some "o"
    // then perf(false); 0
    // elif Array.tryHead argv = Some "t"
    // then test(); 0
    // else Tests.runTestsInAssembly config argv
