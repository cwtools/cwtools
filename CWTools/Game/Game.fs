namespace CWTools.Games
open CWTools.Common
// open CWTools.Process.STLScopes
open Files
open System.Text
open CWTools.Utilities.Utils
open System.IO
open CWTools.Rules



type ValidationSettings = {
    langs : Lang list
    validateVanilla : bool
    experimental : bool
}

type GameSettings<'L> = {
    rootDirectories : WorkspaceDirectoryInput list
    embedded : EmbeddedSettings
    validation : ValidationSettings
    rules : RulesSettings option
    scriptFolders : string list option
    excludeGlobPatterns : string list option
    modFilter : string option
    initialLookup : 'L
    maxFileSize : int option
}

type EmbeddedSetupSettings =
    | FromConfig of embeddedFiles : (string * string) list * cachedResourceData : (Resource * Entity) list
    | Metadata of cachedRuleMetadata : CachedRuleMetadata
    | ManualSettings of EmbeddedSettings

type GameSetupSettings<'L> = {
    rootDirectories : WorkspaceDirectoryInput list
    embedded : EmbeddedSetupSettings
    validation : ValidationSettings
    rules : RulesSettings option
    scriptFolders : string list option
    excludeGlobPatterns : string list option
    modFilter : string option
    maxFileSize : int option
}


type GameObject<'T, 'L when 'T :> ComputedData
                    and 'L :> Lookup>
    (settings : GameSettings<'L>, game, scriptFolders, computeFunction, computeUpdateFunction, localisationService,
     processLocalisation, validateLocalisationCommand, defaultContext, noneContext,
     encoding : Encoding, fallbackencoding : Encoding,
     validationSettings,
     globalLocalisation : GameObject<'T, 'L> -> CWError list,
     afterUpdateFile : GameObject<'T, 'L> -> string -> unit,
     localisationExtension : string,
     ruleManagerSettings : RuleManagerSettings<'T, 'L>) as this =
    let scriptFolders = settings.scriptFolders |> Option.defaultValue scriptFolders
    let excludeGlobPatterns = settings.excludeGlobPatterns |> Option.defaultValue []
    let embeddedDir =
        settings.embedded.cachedResourceData
            |> List.tryHead
            |> Option.map (fun (r,e) -> e.filepath.Replace("\\","/").TrimStart('.').Replace(e.logicalpath, ""))
    let fileManager = FileManager(settings.rootDirectories, embeddedDir, scriptFolders, game, encoding, excludeGlobPatterns, settings.maxFileSize |> Option.defaultValue 2)
    // let computeEU4Data (e : Entity) = EU4ComputedData()
    // let mutable infoService : InfoService<_> option = None
    // let mutable completionService : CompletionService<_> option = None
    let mutable ruleValidationService : RuleValidationService option = None
    let mutable infoService : InfoService option = None
    let resourceManager = ResourceManager<'T>(computeFunction (fun () -> this.InfoService), computeUpdateFunction (fun () -> this.InfoService), encoding, fallbackencoding)
    let validatableFiles() = this.Resources.ValidatableFiles
    let lookup = settings.initialLookup
    let localisationManager = LocalisationManager<'T>(resourceManager.Api, localisationService, settings.validation.langs, lookup, processLocalisation, localisationExtension)
    let debugMode = settings.rules |> Option.map (fun r -> r.debugMode) |> Option.defaultValue false
    let addEmbeddedLoc langs =
        match settings.embedded.cachedRuleMetadata with
        | None -> id
        | Some md ->
            fun (newList : (Lang * Set<string>) list) ->
                let newMap = newList |> Map.ofList
                let oldList = md.loc |> List.filter (fun (l, _) -> List.contains l langs)
                let embeddedMap = oldList |> Map.ofList
                let res =
                    Map.fold (fun s k v ->
                    match Map.tryFind k s with
                    | Some v' -> Map.add k (Set.union v  v') s
                    | None -> Map.add k v s) newMap embeddedMap
                res |> Map.toList

    let validationServices() =
        {
            resources = resourceManager.Api
            lookup = lookup
            ruleValidationService = ruleValidationService
            infoService = infoService
            localisationKeys = (fun _ -> addEmbeddedLoc (settings.validation.langs) (localisationManager.LocalisationKeys()))
            fileManager = fileManager
        }
    let mutable validationManager : ValidationManager<'T> = ValidationManager(validationSettings, validationServices(), validateLocalisationCommand, defaultContext, (if debugMode then noneContext else defaultContext), new System.Collections.Concurrent.ConcurrentDictionary<_,CWError list>())

    let rulesManager = RulesManager<'T, 'L>(resourceManager.Api, lookup, ruleManagerSettings, localisationManager, settings.embedded, settings.validation.langs, debugMode)
    // let mutable localisationAPIs : (bool * ILocalisationAPI) list = []
    // let mutable localisationErrors : CWError list option = None
    // let mutable localisationKeys = []
    // let mutable taggedLocalisationKeys = []
    let getEmbeddedFiles() = settings.embedded.embeddedFiles |> List.map (fun (fn, f) -> "embedded", "embeddedfiles/" + fn, f)
    let mutable errorCache = Map.empty
    let updateFile (shallow : bool) filepath (filetext : string option) =
        log (sprintf "%s" filepath)
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let res =
            match filepath with
            | x when x.EndsWith (localisationExtension) ->
                let file = filetext |> Option.defaultWith (fun () -> File.ReadAllText filepath)
                let resourceInput = LanguageFeatures.makeFileWithContentResourceInput fileManager filepath file
                let resource, _ = this.Resources.UpdateFile(resourceInput)
                match resource with
                | FileWithContentResource (_, r) -> this.LocalisationManager.UpdateLocalisationFile r
                | _ -> logNormal (sprintf "Localisation file failed to parse %s" filepath)
                // let les = (validationManager.ValidateLocalisation (this.Resources.ValidatableEntities()))
                let ges = globalLocalisation(this)
                // this.LocalisationManager.localisationErrors <- Some les
                this.LocalisationManager.globalLocalisationErrors <- Some ges
                []
                // les @ ges
            | _ ->
                let file = filetext |> Option.defaultWith (fun () -> File.ReadAllText(filepath, encoding))
                let resource = LanguageFeatures.makeEntityResourceInput fileManager filepath file
                let newEntities = [this.Resources.UpdateFile (resource)] |> List.choose snd
                afterUpdateFile this filepath
                match shallow with
                | true ->
                    let (shallowres, _) = (validationManager.Validate(shallow, newEntities))
                    let shallowres = shallowres @ (validationManager.ValidateLocalisation newEntities)
                    let deep = errorCache |> Map.tryFind filepath |> Option.defaultValue []
                    shallowres @ deep
                | false ->
                    let (shallowres, deepres) = (validationManager.Validate(shallow, newEntities))
                    let shallowres = shallowres @ (validationManager.ValidateLocalisation newEntities)
                    errorCache <- errorCache.Add(filepath, deepres)
                    shallowres @ deepres
                //validateAll shallow newEntities @ localisationCheck newEntities
        log (sprintf "Update Time: %i" timer.ElapsedMilliseconds)
        res
    let initialLoad() =
            log (sprintf "Parsing %i files" (fileManager.AllFilesByPath().Length))
            let timer = new System.Diagnostics.Stopwatch()
            timer.Start()
            // let efiles = allFilesByPath |> List.filter (fun (_, f, _) -> not(f.EndsWith(".dds")))
            //             |> List.map (fun (s, f, ft) -> EntityResourceInput {scope = s; filepath = f; filetext = ft; validate = true})
            // let otherfiles = allFilesByPath |> List.filter (fun (_, f, _) -> f.EndsWith(".dds"))
            //                     |> List.map (fun (s, f, _) -> FileResourceInput {scope = s; filepath = f;})
            let files = fileManager.AllFilesByPath()
            let filteredfiles =
                if settings.validation.validateVanilla
                then files
                else files |> List.choose (function
                    |FileResourceInput f -> Some (FileResourceInput f)
                    |FileWithContentResourceInput f -> Some (FileWithContentResourceInput f)
                    |EntityResourceInput f -> (if f.scope = "vanilla" then Some (EntityResourceInput {f with validate = false}) else Some (EntityResourceInput f))
                    |_ -> None
                    )
            resourceManager.Api.UpdateFiles(filteredfiles) |> ignore
            let embeddedFiles =
                settings.embedded.embeddedFiles
                |> List.map (fun (f, ft) -> f.Replace("\\","/"), ft)
                |> List.choose (fun (f, ft) ->
                    if ft = ""
                    then Some (FileResourceInput { scope = "embedded"; filepath = f; logicalpath = (fileManager.ConvertPathToLogicalPath f) })
                    else Some (FileWithContentResourceInput { scope = "embedded"; filepath = f; logicalpath = (fileManager.ConvertPathToLogicalPath f); filetext = ft; validate = false}))
            let disableValidate (r, e) : Resource * Entity =
                match r with
                |EntityResource (s, er) -> EntityResource (s, { er with validate = false; scope = "embedded" })
                |x -> x
                , {e with validate = false}
            let cached = settings.embedded.cachedResourceData |> List.map (fun (r, e) -> CachedResourceInput (disableValidate (r, e)))
            let embedded = embeddedFiles @ cached
            if fileManager.ShouldUseEmbedded then resourceManager.Api.UpdateFiles(embedded) |> ignore else ()
    let updateRulesCache() =
        let rules, info, completion = rulesManager.RefreshConfig()
        this.RuleValidationService <- Some rules
        this.InfoService <- Some info
        this.completionService <- Some completion
        this.RefreshValidationManager()

    let initialConfigRules() =
        localisationManager.UpdateAllLocalisation()
        if settings.rules.IsSome then rulesManager.LoadBaseConfig(settings.rules.Value) else ()
        updateRulesCache()
        this.Resources.ForceRecompute()
        localisationManager.UpdateAllLocalisation()

    do
        lookup.rootFolders <- settings.rootDirectories
        initialLoad()

    member __.RuleValidationService with get() = ruleValidationService
    member __.RuleValidationService with set(value) = ruleValidationService <- value
    // member val ruleValidationService : RuleValidationService<'S> option = None with get, set
    member __.InfoService with get() = infoService
    member __.InfoService with set(value) = infoService <- value
    // member val infoService : InfoService<'S> option = None with get, set
    member val completionService : CompletionService option = None with get, set

    member __.Resources : IResourceAPI<'T> = resourceManager.Api;
    member __.ResourceManager = resourceManager
    member __.Lookup : 'L = lookup
    // member __.AllLocalisation() = localisationManager.allLocalisation()
    // member __.ValidatableLocalisation() = localisationManager.validatableLocalisation()
    member __.FileManager = (fun () -> fileManager)()
    member __.LocalisationManager : LocalisationManager<'T> = localisationManager
    member __.ValidationManager = validationManager
    member __.Settings = settings
    member __.UpdateFile shallow file text = updateFile shallow file text
    member __.RefreshValidationManager() =
        validationManager <- ValidationManager(validationSettings, validationServices(), validateLocalisationCommand, defaultContext,(if debugMode then noneContext else defaultContext), validationManager.ErrorCache())

    member this.InfoAtPos pos file text = LanguageFeatures.symbolInformationAtPos this.FileManager this.ResourceManager this.InfoService this.Lookup pos file text
    member __.ReplaceConfigRules rules =
        rulesManager.LoadBaseConfig rules
    member __.RefreshCaches() =
        updateRulesCache()
    member __.InitialConfigRules() = initialConfigRules()
    static member CreateGame settings afterInit =
        let game = GameObject(settings)
        afterInit game
        game.InitialConfigRules()
        game