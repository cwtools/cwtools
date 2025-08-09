namespace CWTools.Rules

open System.Runtime.CompilerServices
open CWTools.Common
open CWTools.Utilities
open CWTools.Process
open CWTools.Utilities.StringResource
open Shared

type ReplaceScopes =
    { root: Scope option
      this: Scope option
      froms: Scope list option
      prevs: Scope list option }

type Options =
    { min: int
      max: int
      strictMin: bool
      leafvalue: bool
      description: string option
      pushScope: Scope option
      replaceScopes: ReplaceScopes option
      severity: Severity option
      requiredScopes: Scope list
      comparison: bool
      referenceDetails: (bool * string) option
      keyRequiredQuotes: bool
      valueRequiredQuotes: bool
      typeHint: (string * bool) option
      errorIfOnlyMatch: string option }

    static member DefaultOptions =
        { min = 0
          max = 1000
          strictMin = true
          leafvalue = false
          description = None
          pushScope = None
          replaceScopes = None
          severity = None
          requiredScopes = []
          comparison = false
          referenceDetails = None
          keyRequiredQuotes = false
          valueRequiredQuotes = false
          typeHint = None
          errorIfOnlyMatch = None }

type ValueType =
    | Enum of enumc: string
    | Float of minmax: (decimal * decimal)
    | Bool
    | Int of minmaxi: struct (int * int)
    | Percent
    | Date
    | DateTime
    | CK2DNA
    | CK2DNAProperty
    | IRFamilyName
    | STLNameFormat of variable: string

    override x.ToString() =
        match x with
        // | Scalar -> "Scalar"
        | Enum enumc -> $"Enum %s{enumc}"
        // | Specific valuec -> sprintf "Specific %s" (StringResource.stringManager.GetStringForIDs valuec)
        | Float(min, max) -> $"Float with min %f{min} and max %f{max}"
        | Bool -> "Bool"
        | Int(min, max) -> $"Int with min %i{min} and max %i{max}"
        | Percent -> "Percent"
        | Date -> "Date"
        | DateTime -> "DateTime"
        | CK2DNA -> "CK2DNA"
        | CK2DNAProperty -> "CK2DNAProperty"
        | IRFamilyName -> "IRFamilyName"
        | STLNameFormat x -> $"STLNameFormat %s{x}"

[<Struct; IsReadOnly>]
type SpecificValue = SpecificValue of valuec: StringTokens

[<Struct; IsReadOnly>]
type ScalarValue = | ScalarValue

type TypeType =
    | Simple of name: string
    | Complex of prefix: string * name: string * suffix: string

[<Struct; IsReadOnly>]
type Marker =
    | ColourField
    | IRCountryTag

type TypeLocalisation =
    { name: string
      prefix: string
      suffix: string
      required: bool
      optional: bool
      explicitField: string option
      replaceScopes: ReplaceScopes option
      primary: bool }

type TypeModifier =
    { prefix: string
      suffix: string
      category: ModifierCategory
      documentation: string option
      explicit: bool }

type SkipRootKey =
    | SpecificKey of string
    | AnyKey
    | MultipleKeys of string list * bool

type SubTypeDefinition =
    { name: string
      displayName: string option
      abbreviation: string option
      rules: NewRule array
      typeKeyField: string option
      startsWith: string option
      pushScope: Scope option
      localisation: TypeLocalisation list
      onlyIfNot: string list
      modifiers: TypeModifier list }

and TypeDefinition =
    { name: string
      nameField: string option
      pathOptions: PathOptions
      conditions: Node option
      subtypes: SubTypeDefinition list
      typeKeyFilter: (string list * bool) option
      skipRootKey: SkipRootKey list
      startsWith: string option
      type_per_file: bool
      keyPrefix: string option
      warningOnly: bool
      unique: bool
      shouldBeReferenced: bool
      localisation: TypeLocalisation list
      graphRelatedTypes: string list
      modifiers: TypeModifier list }

and NewField =
    | ValueField of ValueType
    | SpecificField of SpecificValue
    | ScalarField of ScalarValue
    | TypeField of TypeType
    /// This is only used internally to match type definitions
    | TypeMarkerField of dummyKey: StringLowerToken * typedef: TypeDefinition
    | ScopeField of Scope list
    | LocalisationField of synced: bool * isInline: bool
    | FilepathField of prefix: string option * extension: string option
    | IconField of string
    /// The keys of an alias rule
    | AliasValueKeysField of string
    | AliasField of string
    | SingleAliasField of string
    | SingleAliasClauseField of string * string
    | SubtypeField of string * bool * NewRule list
    | VariableSetField of string
    | VariableGetField of string
    | VariableField of isInt: bool * is32Bit: bool * minmax: (decimal * decimal)
    | ValueScopeMarkerField of isInt: bool * minmax: (decimal * decimal)
    | ValueScopeField of isInt: bool * minmax: (decimal * decimal)
    | MarkerField of Marker
    | JominiGuiField
    | IgnoreMarkerField
    | IgnoreField of field: NewField

    override x.ToString() =
        match x with
        | ValueField vt -> $"Field of {vt}"
        | ScalarField sv -> "Field of any value"
        | SpecificField(SpecificValue sv) -> $"Field of %s{stringManager.GetStringForID sv.normal}"
        | VariableGetField sv -> $"Field of \"%s{sv}\" value"
        | VariableField(isint, is32bit, (min, max)) -> $"Field of defined variable or number between {min} and {max}"
        | _ -> $"Field of %A{x}"

and RuleType =
    | NodeRule of left: NewField * rules: NewRule array
    | LeafRule of left: NewField * right: NewField
    | LeafValueRule of right: NewField
    | ValueClauseRule of rules: NewRule array
    | SubtypeRule of string * bool * NewRule array

    override x.ToString() =
        match x with
        | NodeRule(l, r) -> $"NodeRule with Left ({l}) and inner ({r})"
        | LeafRule(l, r) -> $"LeafRule with Left ({l}) and right ({r})"
        | LeafValueRule r -> $"LeafValueRule ({r})"
        | ValueClauseRule rs -> $"ValueClauseRule with inner ({rs})"
        | SubtypeRule(n, p, r) -> $"SubtypeRule %s{n} with inner ({r})"

and NewRule = RuleType * Options

type RootRule =
    | AliasRule of string * NewRule
    | SingleAliasRule of string * NewRule
    | TypeRule of string * NewRule

    override x.ToString() =
        match x with
        | AliasRule(n, r) -> $"Alias definition %s{n} ({r})"
        | SingleAliasRule(n, r) -> $"Single alias definition %s{n} ({r})"
        | TypeRule(n, r) -> $"Type rule %s{n} ({r})"
// type EffectRule = Rule // Add scopes

type EnumDefinition =
    { key: string
      description: string
      values: string list
      valuesWithRange: (string * CWTools.Utilities.Position.range option) list }

type ComplexEnumDef =
    { name: string
      description: string
      pathOptions: PathOptions
      nameTree: Node
      start_from_root: bool }
