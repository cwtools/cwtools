namespace CWTools.Games

open CWTools.Utilities.Position
open CWTools.Common
open CWTools.Process.Scopes

type SymbolLocalisationInfo = { key: string; value: string }

type SymbolInformation =
    { typename: string
      name: string
      localisation: SymbolLocalisationInfo list
      ruleDescription: string option
      ruleRequiredScopes: string list }

type GraphDataItem =
    {
        id: string
        displayName: string option
        documentation: string option
        /// name * isOutgoing
        references: (string * bool * string option) list
        location: range option
        details: Map<string, string list> option
        /// Whether this item is in the files given (as opposed to only referenced to)
        isPrimary: bool
        entityType: string
        entityTypeDisplayName: string option
        abbreviation: string option
    }

type GraphDataRequest = string list -> string -> int -> GraphDataItem list



type CWRelatedError = { location: range; message: string }

type CWError =
    { code: string
      severity: Severity
      range: range
      keyLength: int
      message: string
      data: string option
      relatedErrors: CWRelatedError list option }

type CachedRuleMetadata =
    { typeDefs: Map<string, array<TypeDefInfo>>
      enumDefs: Map<string, string * array<string>>
      varDefs: Map<string, array<string * range>>
      loc: (Lang * Set<string>) list
      files: Set<string>
      scriptedLoc: string list }

type CompletionCategory =
    | Link = 1uy
    | Global = 2uy
    | Variable = 3uy
    | Value = 4uy
    | Other = 5uy

type CompletionResponse =
    | Simple of label: string * score: int option * CompletionCategory
    | Detailed of label: string * desc: string option * score: int option * CompletionCategory
    | Snippet of label: string * snippet: string * desc: string option * score: int option * CompletionCategory

    static member CreateSimple(label: string) = Simple(label, None, CompletionCategory.Other)

    static member CreateSnippet(label, snippet, desc) =
        Snippet(label, snippet, desc, None, CompletionCategory.Other)

type IGame =
    abstract ParserErrors: unit -> (string * string * FParsec.Position) list
    abstract ValidationErrors: unit -> CWError list
    abstract LocalisationErrors: bool * bool -> CWError list
    abstract Folders: unit -> (string * string) list
    abstract AllFiles: unit -> Resource list
    abstract AllLoadedLocalisation: unit -> string list
    abstract UpdateFile: bool -> string -> string option -> CWError list
    abstract Complete: pos -> string -> string -> CompletionResponse list
    abstract GoToType: pos -> string -> string -> range option
    abstract FindAllRefs: pos -> string -> string -> range list option
    abstract ReplaceConfigRules: (string * string) list -> unit
    abstract RefreshCaches: unit -> unit
    abstract RefreshLocalisationCaches: unit -> unit
    abstract ForceRecompute: unit -> unit
    abstract Types: unit -> Map<string, TypeDefInfo array>
    abstract TypeDefs: unit -> CWTools.Rules.TypeDefinition list
    abstract InfoAtPos: pos -> string -> string -> SymbolInformation option
    abstract GetPossibleCodeEdits: string -> string -> range list
    abstract GetCodeEdits: string -> string -> (range seq * pos * string) list option
    abstract GetEventGraphData: GraphDataRequest
    abstract ScriptedTriggers: unit -> Effect list
    abstract ScriptedEffects: unit -> Effect list
    abstract StaticModifiers: unit -> StaticModifier list
    abstract ScopesAtPos: pos -> string -> string -> ScopeContext option
    abstract GetEmbeddedMetadata: unit -> CachedRuleMetadata

type IGame<'T when 'T :> ComputedData> =
    inherit IGame
    abstract AllEntities: unit -> struct (Entity * Lazy<'T>) list
    abstract References: unit -> References<'T>
