namespace CWTools.Games
// open CWTools.Common.STLConstants
// open CWTools.Process.STLScopes
open CWTools.Validation.Rules
open CWTools.Validation.ValidationCore
open CWTools.Games.Stellaris.STLLookup
open FParsec
open CWTools.Utilities.Position
open CWTools.Common
open CWTools.Process.Scopes

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

type IGame<'T, 'S when 'S : comparison> =
    inherit IGame<'S>
    abstract AllEntities : unit -> struct (Entity * Lazy<'T>) list
    abstract References : unit -> References<'T, 'S>
