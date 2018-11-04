namespace CWTools.Games.HOI4
open CWTools.Localisation
open CWTools.Validation.ValidationCore
open CWTools.Games.Files
open CWTools.Games
open CWTools.Common
open FSharp.Collections.ParallelSeq
open CWTools.Localisation.HOI4Localisation
open CWTools.Utilities.Utils
open CWTools.Utilities.Position
open System.IO
open System.Text

type EmbeddedSettings = {
    embeddedFiles : (string * string) list
}

type ValidationSettings = {
    langs : Lang list
    validateVanilla : bool
}
type HOI4Settings = {
    rootDirectory : string
    embedded : EmbeddedSettings
    validation : ValidationSettings
}

type HOI4ComputedData = {
    temp : string list
}

type HOI4Game(settings : HOI4Settings) =

    let scriptFolders = [
        "common/abilities";
        "common/aces";
        "common/ai_areas";
        "common/ai_focuses";
        "common/ai_peace";
        "common/ai_strategy";
        "common/ai_strategy_plans";
        "common/ai_templates";
        "common/autonomous_states";
        "common/bookmarks";
        "common/buildings";
        "common/continuous_focus";
        "common/countries";
        "common/country_leader";
        "common/country_tags";
        "common/decisions";
        "common/difficulty_settings";
        "common/idea_tags";
        "common/ideas";
        "common/ideologies";
        "common/modifiers";
        "common/names";
        "common/national_focus";
        "common/on_actions";
        "common/opinion_modifiers";
        "common/resources";
        "common/scripted_effects";
        "common/scripted_localisation";
        "common/scripted_triggers";
        "common/state_category";
        "common/technologies";
        "common/technology_sharing";
        "common/technology_tags";
        "common/terrain";
        "common/timed_activities";
        "common/unit_leader";
        "common/unit_tags";
        "common/units";
        "common/wargoals";
        "events";
        "gfx";
        "interface";
    ]

    let fileManager = FileManager(settings.rootDirectory, None, FilesScope.All, scriptFolders, "hearts of iron iv", Encoding.UTF8)

    let computeHOI4Data (e : Entity) = { temp = [] }
    let resourceManager = ResourceManager(computeHOI4Data)
    let resources = resourceManager.Api
    let validatableFiles() = resources.ValidatableFiles
    let mutable localisationAPIs : (bool * ILocalisationAPI) list = []
    let allLocalisation() = localisationAPIs |> List.map snd
    let validatableLocalisation() = localisationAPIs |> List.choose (fun (validate, api) -> if validate then Some api else None)
    let mutable localisationErrors : CWError list option = None

    let getEmbeddedFiles() = settings.embedded.embeddedFiles |> List.map (fun (fn, f) -> "embedded", "embeddedfiles/" + fn, f)

    let updateLocalisation() =
        localisationAPIs <-
            let locs = fileManager.LocalisationFiles() |> PSeq.ofList |> PSeq.map (fun (folder, _) -> HOI4LocalisationService({ folder = folder})) |> PSeq.toList
            let allLocs = locs |> List.collect (fun l -> (settings.validation.langs)|> List.map (fun lang -> true, l.Api(lang)))
            match fileManager.ShouldUseEmbedded with
            |false -> allLocs
            |true ->
                allLocs @ (getEmbeddedFiles()
                |> List.filter (fun (_, fn, _ )-> fn.Contains("localisation"))
                |> List.map (fun (_, fn, f) -> (fn, f))
                |> (fun files -> HOI4LocalisationService(files))
                |> (fun l -> (settings.validation.langs) |> List.map (fun lang -> false, l.Api(lang))))
        let taggedKeys = allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.GetKeys) |> List.fold (fun (s : LocKeySet) v -> s.Add v) (LocKeySet.Empty(InsensitiveStringComparer())) )
        let validatableEntries = validatableLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList) |> Map.ofList)
        ()
        //lookup.proccessedLoc <- validatableEntries |> List.map (fun f -> processLocalisation lookup.scriptedEffects lookup.scriptedLoc lookup.definedScriptVariables (EntitySet (resources.AllEntities())) f taggedKeys)
        //TODO: Add processed loc bacck

    let parseErrors() =
        resources.GetResources()
            |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None)
            |> List.choose (fun r -> r.result |> function |(Fail (result)) when r.validate -> Some (r.filepath, result.error, result.position)  |_ -> None)

    let updateFile filepath (filetext : string option) =
        eprintfn "%s" filepath
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let res =
            match filepath with
            |x when x.EndsWith (".yml") ->
                updateLocalisation()
                // let les = (localisationCheck (resources.ValidatableEntities())) @ globalLocalisation()
                // localisationErrors <- Some les
                // globalLocalisation()
                []
            | _ ->
                let filepath = Path.GetFullPath(filepath)
                let file = filetext |> Option.defaultWith (fun () -> File.ReadAllText filepath)
                let rootedpath = filepath.Substring(filepath.IndexOf(fileManager.ScopeDirectory) + (fileManager.ScopeDirectory.Length))
                let logicalpath = fileManager.ConvertPathToLogicalPath rootedpath
                //eprintfn "%s %s" logicalpath filepath
                let newEntities = resources.UpdateFile (EntityResourceInput {scope = ""; filepath = filepath; logicalpath = logicalpath; filetext = file; validate = true})
                // match filepath with
                // |x when x.Contains("scripted_triggers") -> updateScriptedTriggers()
                // |x when x.Contains("scripted_effects") -> updateScriptedEffects()
                // |x when x.Contains("static_modifiers") -> updateStaticodifiers()
                // |_ -> ()
                // updateDefinedVariables()
                // validateAll true newEntities @ localisationCheck newEntities
                []
        eprintfn "Update Time: %i" timer.ElapsedMilliseconds
        res


    do
        eprintfn "Parsing %i files" (fileManager.AllFilesByPath().Length)
        let files = fileManager.AllFilesByPath()
        let filteredfiles = if settings.validation.validateVanilla then files else files |> List.choose (function |FileResourceInput f -> Some (FileResourceInput f) |EntityResourceInput f -> if f.scope = "vanilla" then Some (EntityResourceInput {f with validate = false}) else Some (EntityResourceInput f))
        resources.UpdateFiles(filteredfiles) |> ignore
        let embedded = settings.embedded.embeddedFiles |> List.map (fun (f, ft) -> if ft = "" then FileResourceInput { scope = "embedded"; filepath = f; logicalpath = (fileManager.ConvertPathToLogicalPath f) } else EntityResourceInput {scope = "embedded"; filepath = f; logicalpath = (fileManager.ConvertPathToLogicalPath f); filetext = ft; validate = false})
        if fileManager.ShouldUseEmbedded then resources.UpdateFiles(embedded) |> ignore else ()

        // updateScriptedTriggers()
        // updateScriptedEffects()
        // updateStaticodifiers()
        // updateScriptedLoc()
        // updateDefinedVariables()
        // updateModifiers()
        // updateTechnologies()
        updateLocalisation()
        // updateTypeDef()
    interface IGame<HOI4ComputedData, string> with
    //member __.Results = parseResults
        member __.ParserErrors() = parseErrors()
        member __.ValidationErrors() = [] //(validateAll false (resources.ValidatableEntities()))
        member __.LocalisationErrors(force : bool) = []
            // let generate =
            //     let les = (localisationCheck (resources.ValidatableEntities())) @ globalLocalisation()
            //     localisationErrors <- Some les
            //     les
            // match localisationErrors with
            // |Some les -> if force then generate else les
            // |None -> generate

        //member __.ValidationWarnings = warningsAll
        member __.Folders() = fileManager.AllFolders()
        member __.AllFiles() =
            resources.GetResources()
            // |> List.map
            //     (function
            //         |EntityResource (f, r) ->  r.result |> function |(Fail (result)) -> (r.filepath, false, result.parseTime) |Pass(result) -> (r.filepath, true, result.parseTime)
            //         |FileResource (f, r) ->  (r.filepath, false, 0L))
            //|> List.map (fun r -> r.result |> function |(Fail (result)) -> (r.filepath, false, result.parseTime) |Pass(result) -> (r.filepath, true, result.parseTime))
        member __.ScriptedTriggers() = []//lookup.scriptedTriggers
        member __.ScriptedEffects() = [] //lookup.scriptedEffects
        member __.StaticModifiers() = [] //lookup.staticModifiers
        member __.UpdateFile shallow file text = updateFile file text
        member __.AllEntities() = resources.AllEntities()
        member __.References() = References<HOI4ComputedData, string>(resources, Lookup(), (localisationAPIs |> List.map snd))
        member __.Complete pos file text = [] //completion pos file text
        member __.ScopesAtPos pos file text = None //scopesAtPos pos file text
        member __.GoToType pos file text = Some range0
        member __.FindAllRefs pos file text = Some [range0]
        member __.ReplaceConfigRules rules = ()
        member __.RefreshCaches() = ()
        member __.ForceRecompute() = ()

            //member __.ScriptedTriggers = parseResults |> List.choose (function |Pass(f, p, t) when f.Contains("scripted_triggers") -> Some p |_ -> None) |> List.map (fun t -> )