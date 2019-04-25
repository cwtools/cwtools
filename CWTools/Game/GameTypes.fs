namespace CWTools.Games
open CWTools.Utilities.Position
open CWTools.Common
open CWTools.Process.Scopes

type SymbolLocalisationInfo = {
    key : string
    value : string
}
type SymbolInformation =
    {
        typename : string
        name : string
        localisation : SymbolLocalisationInfo list
        ruleDescription : string option
        ruleRequiredScopes : string list
    }

type CWError = (string * Severity * range * int * string * option<string>)

type CompletionResponse =
    |Simple of label : string * score : int option
    |Detailed of label : string * desc : string option * score : int option
    |Snippet of label : string * snippet : string * desc : string option * score : int option
    static member CreateSimple(label : string) = Simple(label, None)
    static member CreateSnippet(label, snippet, desc) = Snippet(label, snippet, desc, None)

type IGame =
    abstract ParserErrors : unit -> (string * string * FParsec.Position) list
    abstract ValidationErrors : unit -> CWError list
    abstract LocalisationErrors : bool * bool -> CWError list
    abstract Folders : unit -> (string * string) list
    abstract AllFiles : unit -> Resource list
    abstract AllLoadedLocalisation : unit -> string list
    abstract UpdateFile : bool -> string -> string option -> CWError list
    abstract Complete : pos -> string -> string -> CompletionResponse list
    abstract GoToType : pos -> string -> string -> range option
    abstract FindAllRefs : pos -> string -> string -> range list option
    abstract ReplaceConfigRules : (string * string) list -> unit
    abstract RefreshCaches : unit -> unit
    abstract RefreshLocalisationCaches : unit -> unit
    abstract ForceRecompute : unit -> unit
    abstract Types : unit ->  Map<string,(string * range) list>
    abstract InfoAtPos : pos -> string -> string -> SymbolInformation option

type IGame<'S when 'S : comparison and 'S :> IScope<'S>> =
    inherit IGame
    abstract ScriptedTriggers : unit -> Effect<'S> list
    abstract ScriptedEffects : unit -> Effect<'S> list
    abstract StaticModifiers : unit -> CWTools.Common.STLConstants.Modifier list
    abstract ScopesAtPos : pos -> string -> string -> ScopeContext<'S> option

type IGame<'T, 'S, 'M when 'S : comparison and 'S :> IScope<'S> and 'T :> ComputedData and 'M :> IModifier> =
    inherit IGame<'S>
    abstract AllEntities : unit -> struct (Entity * Lazy<'T>) list
    abstract References : unit -> References<'T, 'S, 'M>
