namespace CWTools.Games
open CWTools.Common.STLConstants
open CWTools.Process.STLScopes
open CWTools.Validation.Rules
open CWTools.Validation.ValidationCore
open CWTools.Games.Stellaris.STLLookup
open FParsec
open CWTools.Utilities.Position

type IGame =
    abstract ParserErrors : unit -> (string * string * Position) list
    abstract ValidationErrors : unit -> CWError list
    abstract LocalisationErrors : bool -> CWError list
    abstract Folders : unit -> (string * string) list
    abstract AllFiles : unit -> Resource list
    abstract ScriptedTriggers : unit -> Effect list
    abstract ScriptedEffects : unit -> Effect list
    abstract StaticModifiers : unit -> Modifier list
    abstract UpdateFile : string -> string option -> CWError list
    abstract Complete : pos -> string -> string -> CompletionResponse list
    abstract ScopesAtPos : pos -> string -> string -> ScopeContext option
    abstract GoToType : pos -> string -> string -> range option
    abstract FindAllRefs : pos -> string -> string -> range list option

type IGame<'T> =
    inherit IGame
    abstract AllEntities : unit -> struct (Entity * Lazy<'T>) list
    abstract References : unit -> References<'T>
