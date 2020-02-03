namespace CWTools.Rules
open CWTools.Common
open CWTools.Utilities
open CWTools.Process
open CWTools.Utilities.StringResource

type ReplaceScopes = {
    root : Scope option
    this : Scope option
    froms : Scope list option
    prevs : Scope list option
}
type Options = {
    min : int
    max : int
    strictMin : bool
    leafvalue : bool
    description : string option
    pushScope : Scope option
    replaceScopes : ReplaceScopes option
    severity : Severity option
    requiredScopes : Scope list
    comparison : bool
    referenceDetails : (bool * string) option
    keyRequiredQuotes : bool
    valueRequiredQuotes : bool
}

type PathOptions = {
    paths : string list
    pathStrict : bool
    pathFile : string option
    pathExtension : string option
}

[<Struct>]
type ValueType =
| Enum of enumc : string
| Float of minmax: (decimal*decimal)
| Bool
| Int of minmaxi: (int*int)
| Percent
| Date
| DateTime
| CK2DNA
| CK2DNAProperty
| IRFamilyName
| STLNameFormat of variable : string
    override x.ToString() =
        match x with
        // | Scalar -> "Scalar"
        | Enum enumc -> sprintf "Enum %s" enumc
        // | Specific valuec -> sprintf "Specific %s" (StringResource.stringManager.GetStringForIDs valuec)
        | Float (min, max) -> sprintf "Float with min %f and max %f" min max
        | Bool -> "Bool"
        | Int (min, max) -> sprintf "Int with min %i and max %i" min max
        | Percent -> "Percent"
        | Date -> "Date"
        | DateTime -> "DateTime"
        | CK2DNA -> "CK2DNA"
        | CK2DNAProperty -> "CK2DNAProperty"
        | IRFamilyName -> "IRFamilyName"
        | STLNameFormat x -> sprintf "STLNameFormat %s" x

[<Struct>]
type SpecificValue = |SpecificValue of valuec : StringTokens
[<Struct>]
type ScalarValue = |ScalarValue

type TypeType =
| Simple of name: string
| Complex of prefix : string * name : string * suffix : string

type Marker =
| ColourField
| IRCountryTag

type TypeLocalisation = {
    name : string
    prefix : string
    suffix: string
    required : bool
    optional : bool
    explicitField : string option
    replaceScopes : ReplaceScopes option
    primary : bool
}

type SkipRootKey = |SpecificKey of string |AnyKey |MultipleKeys of string list * bool
type SubTypeDefinition = {
    name : string
    displayName : string option
    abbreviation : string option
    rules : NewRule list
    typeKeyField : string option
    startsWith : string option
    pushScope : Scope option
    localisation : TypeLocalisation list
    onlyIfNot : string list
}
and TypeDefinition = {
    name : string
    nameField : string option
    pathOptions : PathOptions
    conditions : Node option
    subtypes : SubTypeDefinition list
    typeKeyFilter : (string list * bool) option
    skipRootKey : SkipRootKey list
    startsWith : string option
    type_per_file : bool
    warningOnly : bool
    unique : bool
    localisation : TypeLocalisation list
    graphRelatedTypes : string list
}

and NewField =
| ValueField of ValueType
| SpecificField of SpecificValue
| ScalarField of ScalarValue
| TypeField of TypeType
/// This is only used internally to match type definitions
| TypeMarkerField of dummyKey : StringLowerToken * typedef : TypeDefinition
| ScopeField of Scope
| LocalisationField of synced : bool * isInline : bool
| FilepathField of prefix : string option * extension : string option
| IconField of string
/// The keys of an alias rule
| AliasValueKeysField of string
| AliasField of string
| SingleAliasField of string
| SingleAliasClauseField of string * string
| SubtypeField of string * bool * NewRule list
| VariableSetField of string
| VariableGetField of string
| VariableField of isInt : bool * minmax : (decimal * decimal)
| ValueScopeMarkerField of isInt : bool * minmax : (decimal * decimal)
| ValueScopeField of isInt : bool * minmax : (decimal * decimal)
| MarkerField of Marker
| JominiGuiField
| IgnoreMarkerField
| IgnoreField of field : NewField
    override x.ToString() =
        match x with
        | ValueField vt -> sprintf "Field of %O" vt
        | ScalarField sv -> "Field of any value"
        | SpecificField (SpecificValue sv) -> sprintf "Field of %s" (stringManager.GetStringForID sv.normal)
        | _ -> sprintf "Field of %A" x
and RuleType =
|NodeRule of left : NewField * rules : NewRule list
|LeafRule of left : NewField * right : NewField
|LeafValueRule of right : NewField
|ValueClauseRule of rules : NewRule list
|SubtypeRule of string * bool * NewRule list
    override x.ToString() =
        match x with
        | NodeRule (l, r) -> sprintf "NodeRule with Left (%O) and inner (%O)" l r
        | LeafRule (l, r) -> sprintf "LeafRule with Left (%O) and right (%O)" l r
        | LeafValueRule (r) -> sprintf "LeafValueRule (%O)" r
        | ValueClauseRule (rs) -> sprintf "ValueClauseRule with inner (%O)" rs
        | SubtypeRule (n, p, r) -> sprintf "SubtypeRule %s with inner (%O)" n r

and NewRule = RuleType * Options

type RootRule =
| AliasRule of string * NewRule
| SingleAliasRule of string * NewRule
| TypeRule of string * NewRule
    override x.ToString() =
        match x with
        | AliasRule (n, r) -> sprintf "Alias definition %s (%O)" n r
        | SingleAliasRule (n, r) -> sprintf "Single alias definition %s (%O)" n r
        | TypeRule (n, r) -> sprintf "Type rule %s (%O)" n r
// type EffectRule = Rule // Add scopes

type EnumDefinition = {
        key : string
        description : string
        values : string list
    }
type ComplexEnumDef = {
    name : string
    description : string
    pathOptions : PathOptions
    nameTree : Node
    start_from_root : bool
}
