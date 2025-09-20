namespace CWTools.Games

open CWTools.Common
// open CWTools.Process.STLScopes
open Files
open System.Text
open CWTools.Utilities.Utils
open System.IO
open CWTools.Rules



type ValidationSettings =
    { langs: Lang array
      validateVanilla: bool
      experimental: bool }

type GameSettings<'L> =
    { rootDirectories: WorkspaceDirectoryInput list
      embedded: EmbeddedSettings
      validation: ValidationSettings
      rules: RulesSettings option
      scriptFolders: string array option
      excludeGlobPatterns: string array option
      modFilter: string option
      initialLookup: 'L
      maxFileSize: int option
      enableInlineScripts: bool }

type EmbeddedSetupSettings =
    | FromConfig of embeddedFiles: (string * string) list * cachedResourceData: (Resource * Entity) list
    | Metadata of cachedRuleMetadata: CachedRuleMetadata
    | ManualSettings of EmbeddedSettings

type StopPoint =
    | GameCtor
    | GameInitLoad
    | GameAfterInit
    | GameInitialConfigRules
    | Full

type DebugSettings =
    { EarlyStop: StopPoint }

    static member Default = { EarlyStop = Full }

type GameSetupSettings<'L> =
    { rootDirectories: WorkspaceDirectoryInput list
      embedded: EmbeddedSetupSettings
      validation: ValidationSettings
      rules: RulesSettings option
      scriptFolders: string array option
      excludeGlobPatterns: string array option
      modFilter: string option
      maxFileSize: int option
      debugSettings: DebugSettings }

[<Sealed>]
type GameObject<'T, 'L when 'T :> ComputedData and 'L :> Lookup>
    (
        settings: GameSettings<'L>,
        game,
        scriptFolders,
        computeFunction,
        computeUpdateFunction,
        localisationService,
        locFunctions,
        defaultContext,
        noneContext,
        encoding: Encoding,
        fallbackencoding: Encoding,
        validationSettings,
        globalLocalisation: GameObject<'T, 'L> -> CWError list,
        afterUpdateFile: GameObject<'T, 'L> -> string -> unit,
        localisationExtension: string,
        ruleManagerSettings: RuleManagerSettings<'T, 'L>,
        debugSettings: DebugSettings
    ) as this =
    let scriptFolders = settings.scriptFolders |> Option.defaultValue scriptFolders
    let excludeGlobPatterns = settings.excludeGlobPatterns |> Option.defaultValue [||]

    let embeddedDir =
        settings.embedded.cachedResourceData
        |> List.tryHead
        |> Option.map (fun (r, e) -> e.filepath.Replace("\\", "/").TrimStart('.').Replace(e.logicalpath, ""))

    let fileManager =
        FileManager(
            settings.rootDirectories,
            embeddedDir,
            scriptFolders,
            game,
            encoding,
            excludeGlobPatterns,
            settings.maxFileSize |> Option.defaultValue 2
        )
    // let computeEU4Data (e : Entity) = EU4ComputedData()
    // let mutable infoService : InfoService<_> option = None
    // let mutable completionService : CompletionService<_> option = None
    let mutable ruleValidationService: RuleValidationService option = None
    let mutable infoService: InfoService option = None

    let resourceManager =
        ResourceManager<'T>(
            computeFunction (fun () -> this.InfoService),
            computeUpdateFunction (fun () -> this.InfoService),
            encoding,
            fallbackencoding,
            settings.enableInlineScripts
        )

    let validatableFiles () = this.Resources.ValidatableFiles
    let lookup = settings.initialLookup

    let localisationManager =
        LocalisationManager<'T>(
            resourceManager.Api,
            localisationService,
            settings.validation.langs,
            lookup,
            locFunctions >> fst,
            localisationExtension
        )

    let debugMode =
        settings.rules |> Option.map (fun r -> r.debugMode) |> Option.defaultValue false

    let addEmbeddedLoc langs =
        match settings.embedded.cachedRuleMetadata with
        | None -> id
        | Some md ->
            fun (newList: (Lang * Set<string>) list) ->
                let newMap = newList |> Map.ofList
                let oldList = md.loc |> List.filter (fun (l, _) -> Array.contains l langs)
                let embeddedMap = oldList |> Map.ofList

                let res =
                    Map.fold
                        (fun s k v ->
                            match Map.tryFind k s with
                            | Some v' -> Map.add k (Set.union v v') s
                            | None -> Map.add k v s)
                        newMap
                        embeddedMap

                res |> Map.toList

    let validationServices () =
        { resources = resourceManager.Api
          lookup = lookup
          ruleValidationService = ruleValidationService
          infoService = infoService
          localisationKeys =
            (fun _ -> addEmbeddedLoc settings.validation.langs (localisationManager.LocalisationKeys()))
          fileManager = fileManager }

    let mutable validationManager: ValidationManager<'T> =
        ValidationManager(
            validationSettings,
            validationServices (),
            locFunctions >> snd,
            defaultContext,
            (if debugMode then noneContext else defaultContext),
            ErrorCache()
        )

    let rulesManager =
        RulesManager<'T, 'L>(
            resourceManager.Api,
            lookup,
            ruleManagerSettings,
            localisationManager,
            settings.embedded,
            settings.validation.langs,
            debugMode
        )
    // let mutable localisationAPIs : (bool * ILocalisationAPI) list = []
    // let mutable localisationErrors : CWError list option = None
    // let mutable localisationKeys = []
    // let mutable taggedLocalisationKeys = []
    let getEmbeddedFiles () =
        settings.embedded.embeddedFiles
        |> List.map (fun (fn, f) -> "embedded", "embeddedfiles/" + fn, f)

    let mutable errorCache = Map.empty

    let updateFile (shallow: bool) filepath (fileText: string option) =
        log $"updateFile %s{filepath}"
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()

        let res =
            match filepath with
            | x when x.EndsWith localisationExtension ->
                let file = fileText |> Option.defaultWith (fun () -> File.ReadAllText filepath)

                let resourceInput =
                    LanguageFeatures.makeFileWithContentResourceInput fileManager filepath file

                let resource, _ = this.Resources.UpdateFile(resourceInput)

                match resource with
                | FileWithContentResource(_, r) -> this.LocalisationManager.UpdateLocalisationFile r
                | _ -> logWarning (sprintf "Localisation file failed to parse %s" filepath)

                let ges = globalLocalisation (this)
                this.LocalisationManager.globalLocalisationErrors <- Some ges
                []
            | _ ->
                let file =
                    fileText |> Option.defaultWith (fun () -> File.ReadAllText(filepath, encoding))

                let resource = LanguageFeatures.makeEntityResourceInput fileManager filepath file
                let newEntities = [ this.Resources.UpdateFile resource ] |> List.choose snd
                afterUpdateFile this filepath

                match shallow with
                | true ->
                    let shallowres, _ = validationManager.Validate(shallow, newEntities)
                    let shallowres = shallowres @ (validationManager.ValidateLocalisation newEntities)
                    let deep = errorCache |> Map.tryFind filepath |> Option.defaultValue []
                    shallowres @ deep
                | false ->
                    let shallowres, deepres = validationManager.Validate(shallow, newEntities)
                    let shallowres = shallowres @ (validationManager.ValidateLocalisation newEntities)
                    errorCache <- errorCache.Add(filepath, deepres)
                    shallowres @ deepres

        log $"Update file Time: %i{timer.ElapsedMilliseconds}"
        res

    let initialLoad () =
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()
        let files = fileManager.AllFilesByPath()
        log $"Parsing %i{files.Length} files"

        let filteredfiles =
            if settings.validation.validateVanilla then
                files
            else
                files
                |> List.choose (function
                    | FileResourceInput f -> Some(FileResourceInput f)
                    | FileWithContentResourceInput f -> Some(FileWithContentResourceInput f)
                    | EntityResourceInput f ->
                        (if f.scope = "vanilla" then
                             Some(EntityResourceInput { f with validate = false })
                         else
                             Some(EntityResourceInput f))
                    | _ -> None)

        resourceManager.Api.UpdateFiles(filteredfiles) |> ignore
        log $"Parsed files in %A{timer.ElapsedMilliseconds}"
        timer.Restart()

        let embeddedFiles =
            settings.embedded.embeddedFiles
            |> List.map (fun (filePath: string, fileText) ->
                let newFilePath = filePath.Replace('\\', '/')
                if fileText = "" then
                    FileResourceInput
                        { scope = "embedded"
                          filepath = newFilePath
                          logicalpath = (fileManager.ConvertPathToLogicalPath newFilePath) }
                else
                    FileWithContentResourceInput
                        { scope = "embedded"
                          filepath = newFilePath
                          logicalpath = (fileManager.ConvertPathToLogicalPath newFilePath)
                          filetext = fileText
                          validate = false })

        let disableValidate (r, e) : Resource * Entity =
            match r with
            | EntityResource(s, er) ->
                EntityResource(
                    s,
                    { er with
                        validate = false
                        scope = "embedded" }
                )
            | x -> x
            , { e with validate = false }

        let cached =
            settings.embedded.cachedResourceData
            |> List.map (fun (r, e) -> CachedResourceInput(disableValidate (r, e)))

        let embedded = embeddedFiles @ cached

        if fileManager.ShouldUseEmbedded then
            resourceManager.Api.UpdateFiles(embedded) |> ignore
        else
            ()

        log (sprintf "Parsed embedded in %A" timer.ElapsedMilliseconds)

    let updateRulesCache () =
        let rules, info, completion = rulesManager.RefreshConfig()
        this.RuleValidationService <- Some rules
        this.InfoService <- Some info
        this.completionService <- Some completion
        this.RefreshValidationManager()

    let initialConfigRules () =
        log (sprintf "Initial config rules update")
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        localisationManager.UpdateAllLocalisation()
        log (sprintf "Loc updated in %A" timer.ElapsedMilliseconds)
        timer.Restart()

        if settings.rules.IsSome then
            rulesManager.LoadBaseConfig(settings.rules.Value)
        else
            ()

        log (sprintf "Rules loaded in %A" timer.ElapsedMilliseconds)
        timer.Restart()
        updateRulesCache ()
        log (sprintf "Rules cache updated in %A" timer.ElapsedMilliseconds)
        timer.Restart()
        this.Resources.ForceRecompute()
        log (sprintf "Resource recomputer in %A" timer.ElapsedMilliseconds)
        timer.Restart()
        localisationManager.UpdateAllLocalisation()
        log (sprintf "Loc updated again in %A" timer.ElapsedMilliseconds)

    do
        lookup.rootFolders <- settings.rootDirectories

        if debugSettings.EarlyStop >= GameInitLoad then
            initialLoad ()

    member _.RuleValidationService = ruleValidationService

    member _.RuleValidationService
        with set value = ruleValidationService <- value
    // member val ruleValidationService : RuleValidationService<'S> option = None with get, set
    member _.InfoService = infoService

    member _.InfoService
        with set value = infoService <- value
    // member val infoService : InfoService<'S> option = None with get, set
    member val completionService: CompletionService option = None with get, set

    member _.Resources: IResourceAPI<'T> = resourceManager.Api
    member _.ResourceManager = resourceManager
    member _.Lookup: 'L = lookup
    // member __.AllLocalisation() = localisationManager.allLocalisation()
    // member __.ValidatableLocalisation() = localisationManager.validatableLocalisation()
    member _.FileManager = (fun () -> fileManager) ()
    member _.LocalisationManager: LocalisationManager<'T> = localisationManager
    member _.ValidationManager = validationManager
    member _.Settings = settings
    member _.UpdateFile shallow file text = updateFile shallow file text

    member _.RefreshValidationManager() =
        validationManager <-
            ValidationManager(
                validationSettings,
                validationServices (),
                locFunctions >> snd,
                defaultContext,
                (if debugMode then noneContext else defaultContext),
                validationManager.ErrorCache()
            )

    member this.InfoAtPos pos file text =
        LanguageFeatures.symbolInformationAtPos
            this.FileManager
            this.ResourceManager
            this.InfoService
            this.Lookup
            pos
            file
            text

    member _.ReplaceConfigRules rules = rulesManager.LoadBaseConfig rules
    member _.RefreshCaches() = updateRulesCache ()
    member _.InitialConfigRules() = initialConfigRules ()
    member private _.DebugSettings = debugSettings

    static member CreateGame settings afterInit =
        let game = GameObject(settings)

        if game.DebugSettings.EarlyStop >= GameAfterInit then
            afterInit game

        if game.DebugSettings.EarlyStop >= GameInitialConfigRules then
            game.InitialConfigRules()

        game
