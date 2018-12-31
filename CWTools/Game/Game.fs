namespace CWTools.Games
open CWTools.Common
// open CWTools.Process.STLScopes
open CWTools.Validation.Rules
open CWTools.Validation.ValidationCore
open CWTools.Games.Stellaris.STLLookup
open FParsec
open CWTools.Utilities.Position
open CWTools.Common
open CWTools.Process.Scopes
open Files
open System.Text
open CWTools.Localisation
open System.Resources
open CWTools.Utilities.Utils
open System.IO

type IGame =
    abstract ParserErrors : unit -> (string * string * Position) list
    abstract ValidationErrors : unit -> CWError list
    abstract LocalisationErrors : bool -> CWError list
    abstract Folders : unit -> (string * string) list
    abstract AllFiles : unit -> Resource list
    abstract UpdateFile : bool -> string -> string option -> CWError list
    abstract Complete : pos -> string -> string -> CompletionResponse list
    abstract GoToType : pos -> string -> string -> range option
    abstract FindAllRefs : pos -> string -> string -> range list option
    abstract ReplaceConfigRules : (string * string) list -> unit
    abstract RefreshCaches : unit -> unit
    abstract ForceRecompute : unit -> unit

type IGame<'S when 'S : comparison> =
    inherit IGame
    abstract ScriptedTriggers : unit -> Effect<'S> list
    abstract ScriptedEffects : unit -> Effect<'S> list
    abstract StaticModifiers : unit -> CWTools.Common.STLConstants.Modifier list
    abstract ScopesAtPos : pos -> string -> string -> OutputScopeContext<'S> option

type IGame<'T, 'S, 'M when 'S : comparison and 'S :> IScope<'S> and 'T :> ComputedData and 'M :> IModifier> =
    inherit IGame<'S>
    abstract AllEntities : unit -> struct (Entity * Lazy<'T>) list
    abstract References : unit -> References<'T, 'S, 'M>

type EmbeddedSettings<'S,'M when 'S : comparison> = {
    triggers : DocEffect<'S> list
    effects : DocEffect<'S> list
    embeddedFiles : (string * string) list
    modifiers : 'M list
    cachedResourceData : (Resource * Entity) list
}

type ValidationSettings = {
    langs : Lang list
    validateVanilla : bool
    experimental : bool
}
type RulesSettings = {
    ruleFiles : (string * string) list
    validateRules : bool
    debugRulesOnly : bool
}
type GameSettings<'M, 'S when 'S : comparison> = {
    rootDirectory : string
    embedded : EmbeddedSettings<'S, 'M>
    validation : ValidationSettings
    rules : RulesSettings option
    scriptFolders : string list option
    modFilter : string option
    scope : FilesScope
}

type GameObject<'S, 'M, 'T when 'S : comparison and 'S :> IScope<'S> and 'T :> ComputedData and 'M :> IModifier>
    (settings : GameSettings<'M, 'S>, game, scriptFolders, computeFunction, localisationService, processLocalisation,
     encoding : Encoding, validationSettings,
     globalLocalisation : GameObject<'S, 'M, 'T> -> CWError list,
     afterUpdateFile : GameObject<'S, 'M, 'T> -> string -> unit) as this =
    let scriptFolders = settings.scriptFolders |> Option.defaultValue scriptFolders

    let fileManager = FileManager(settings.rootDirectory, settings.modFilter, settings.scope, scriptFolders, game, encoding)

    // let computeEU4Data (e : Entity) = EU4ComputedData()
    // let mutable infoService : FoldRules<_> option = None
    // let mutable completionService : CompletionService<_> option = None
    let mutable ruleApplicator : RuleApplicator<'S> option = None
    let mutable infoService : FoldRules<'S> option = None
    let resourceManager = ResourceManager<'T>(computeFunction (fun () -> this.InfoService))
    let validatableFiles() = this.Resources.ValidatableFiles
    let lookup = Lookup<'S, 'M>()
    let localisationManager = LocalisationManager<'S, 'T, 'M>(resourceManager.Api, localisationService, settings.validation.langs, lookup, processLocalisation)
    let validationServices() =
        {
            resources = resourceManager.Api
            lookup = lookup
            ruleApplicator = ruleApplicator
            foldRules = infoService
            localisationKeys = (fun () -> this.LocalisationManager.localisationKeys)
        }
    let mutable validationManager : ValidationManager<'T, 'S, 'M> = ValidationManager(validationSettings, validationServices())

    // let mutable localisationAPIs : (bool * ILocalisationAPI) list = []
    // let mutable localisationErrors : CWError list option = None
    // let mutable localisationKeys = []
    // let mutable taggedLocalisationKeys = []
    let getEmbeddedFiles() = settings.embedded.embeddedFiles |> List.map (fun (fn, f) -> "embedded", "embeddedfiles/" + fn, f)
    let mutable errorCache = Map.empty
    let updateFile (shallow : bool) filepath (filetext : string option) =
        eprintfn "%s" filepath
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let res =
            match filepath with
            |x when x.EndsWith (".yml") ->
                let file = filetext |> Option.defaultWith (fun () -> File.ReadAllText filepath)
                let resource = LanguageFeatures.makeFileWithContentResourceInput fileManager filepath file
                this.Resources.UpdateFile(resource) |> ignore
                this.LocalisationManager.UpdateAllLocalisation()
                let les = (validationManager.ValidateLocalisation (this.Resources.ValidatableEntities())) @ globalLocalisation(this)
                this.LocalisationManager.localisationErrors <- Some les
                globalLocalisation(this)
            | _ ->
                let file = filetext |> Option.defaultWith (fun () -> File.ReadAllText(filepath, encoding))
                let resource = LanguageFeatures.makeEntityResourceInput fileManager filepath file
                let newEntities = this.Resources.UpdateFile (resource) |> List.map snd
                afterUpdateFile this filepath
                match shallow with
                |true ->
                    let (shallowres, _) = (validationManager.Validate(shallow, newEntities))
                    let shallowres = shallowres @ (validationManager.ValidateLocalisation newEntities)
                    let deep = errorCache |> Map.tryFind filepath |> Option.defaultValue []
                    shallowres @ deep
                |false ->
                    let (shallowres, deepres) = (validationManager.Validate(shallow, newEntities))
                    let shallowres = shallowres @ (validationManager.ValidateLocalisation newEntities)
                    errorCache <- errorCache.Add(filepath, deepres)
                    shallowres @ deepres
                //validateAll shallow newEntities @ localisationCheck newEntities
        eprintfn "Update Time: %i" timer.ElapsedMilliseconds
        res


    member __.RuleApplicator with get() = ruleApplicator
    member __.RuleApplicator with set(value) = ruleApplicator <- value
    // member val ruleApplicator : RuleApplicator<'S> option = None with get, set
    member __.InfoService with get() = infoService
    member __.InfoService with set(value) = infoService <- value
    // member val infoService : FoldRules<'S> option = None with get, set
    member val completionService : CompletionService<'S> option = None with get, set

    member __.Resources : IResourceAPI<'T> = resourceManager.Api
    member __.ResourceManager = resourceManager
    member __.Lookup : Lookup<'S, 'M> = lookup
    // member __.AllLocalisation() = localisationManager.allLocalisation()
    // member __.ValidatableLocalisation() = localisationManager.validatableLocalisation()
    member __.FileManager = fileManager
    member __.LocalisationManager : LocalisationManager<'S, 'T, 'M> = localisationManager
    member __.ValidationManager = validationManager
    member __.Settings = settings
    member __.UpdateFile shallow file text = updateFile shallow file text
    member this.RefreshValidationManager() =
        validationManager <- ValidationManager(validationSettings, validationServices())
