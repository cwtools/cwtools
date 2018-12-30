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
     encoding : Encoding) as this =
    let scriptFolders = settings.scriptFolders |> Option.defaultValue scriptFolders

    let fileManager = FileManager(settings.rootDirectory, settings.modFilter, settings.scope, scriptFolders, game, encoding)

    // let computeEU4Data (e : Entity) = EU4ComputedData()
    // let mutable infoService : FoldRules<_> option = None
    // let mutable completionService : CompletionService<_> option = None
    let resourceManager = ResourceManager<'T>(computeFunction (fun () -> this.infoService))
    let validatableFiles() = this.Resources.ValidatableFiles
    let lookup = Lookup<'S, 'M>()
    let localisationManager = LocalisationManager<'S, 'T, 'M>(resourceManager.Api, localisationService, settings.validation.langs, lookup, processLocalisation)
    // let mutable localisationAPIs : (bool * ILocalisationAPI) list = []
    // let mutable localisationErrors : CWError list option = None
    // let mutable localisationKeys = []
    // let mutable taggedLocalisationKeys = []
    let getEmbeddedFiles() = settings.embedded.embeddedFiles |> List.map (fun (fn, f) -> "embedded", "embeddedfiles/" + fn, f)
    member val ruleApplicator : RuleApplicator<'S> option = None with get, set
    member val infoService : FoldRules<'S> option = None with get, set
    member val completionService : CompletionService<'S> option = None with get, set

    member __.Resources : IResourceAPI<'T> = resourceManager.Api
    member __.ResourceManager = resourceManager
    member __.Lookup : Lookup<'S, 'M> = lookup
    // member __.AllLocalisation() = localisationManager.allLocalisation()
    // member __.ValidatableLocalisation() = localisationManager.validatableLocalisation()
    member __.FileManager = fileManager
    member __.LocalisationManager = localisationManager
