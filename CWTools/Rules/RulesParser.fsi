namespace CWTools.Rules
  open CWTools
  type ReplaceScopes<'a> =
    {root: 'a option;
     this: 'a option;
     froms: 'a list option;
     prevs: 'a list option;}
  type Options<'a> =
    {min: int;
     max: int;
     leafvalue: bool;
     description: string option;
     pushScope: 'a option;
     replaceScopes: ReplaceScopes<'a> option;
     severity: Common.Severity option;
     requiredScopes: 'a list;
     comparison: bool;}
  [<StructAttribute ()>]
  type ValueType =
    | Scalar
    | Enum of enumc: string
    | Specific of valuec: Utilities.StringTokens
    | Float of minmax: (float * float)
    | Bool
    | Int of minmaxi: (int * int)
    | Percent
    | Date
    | CK2DNA
    | CK2DNAProperty
    | IRFamilyName
    with
      override ToString : unit -> string
    end
  type TypeType =
    | Simple of string
    | Complex of prefix: string * name: string * suffix: string
  type Marker =
    | ColourField
    | IRCountryTag
  type TypeLocalisation<'a> =
    {name: string;
     prefix: string;
     suffix: string;
     required: bool;
     optional: bool;
     explicitField: string option;
     replaceScopes: ReplaceScopes<'a> option;}
  type SkipRootKey =
    | SpecificKey of string
    | AnyKey
    | MultipleKeys of string list * bool
  type SubTypeDefinition<'a> =
    {name: string;
     rules: NewRule<'a> list;
     typeKeyField: string option;
     startsWith: string option;
     pushScope: 'a option;
     localisation: TypeLocalisation<'a> list;}
  and TypeDefinition<'a> =
    {name: string;
     nameField: string option;
     path: string list;
     path_strict: bool;
     path_file: string option;
     conditions: Process.Node option;
     subtypes: SubTypeDefinition<'a> list;
     typeKeyFilter: (string list * bool) option;
     skipRootKey: SkipRootKey list;
     startsWith: string option;
     type_per_file: bool;
     warningOnly: bool;
     unique: bool;
     localisation: TypeLocalisation<'a> list;}
  and NewField<'a> =
    | ValueField of ValueType
    | TypeField of TypeType
    | TypeMarkerField of
      dummyKey: Utilities.StringLowerToken * typedef: TypeDefinition<'a>
    | ScopeField of 'a
    | LocalisationField of synced: bool
    | FilepathField of prefix : string option * extension : string option
    | IconField of string
    | AliasField of string
    | SingleAliasField of string
    | SubtypeField of string * bool * NewRule<'a> list
    | VariableSetField of string
    | VariableGetField of string
    | VariableField of isInt: bool * minmax: (float * float)
    | ValueScopeMarkerField of isInt: bool * minmax: (float * float)
    | ValueScopeField of isInt: bool * minmax: (float * float)
    | MarkerField of Marker
    with
      override ToString : unit -> string
    end
  and RuleType<'a> =
    | NodeRule of left: NewField<'a> * rules: NewRule<'a> list
    | LeafRule of left: NewField<'a> * right: NewField<'a>
    | LeafValueRule of right: NewField<'a>
    | ValueClauseRule of rules: NewRule<'a> list
    | SubtypeRule of string * bool * NewRule<'a> list
    with
      override ToString : unit -> string
    end
  and NewRule<'a> = RuleType<'a> * Options<'a>
  type RootRule<'a> =
    | AliasRule of string * NewRule<'a>
    | SingleAliasRule of string * NewRule<'a>
    | TypeRule of string * NewRule<'a>
    with
      override ToString : unit -> string
    end
  type EnumDefinition =
    {key: string;
     description: string;
     values: string list;}
  type ComplexEnumDef =
    {name: string;
     description: string;
     path: string;
     nameTree: Process.Node;
     start_from_root: bool;}
  module RulesParser = begin
    val specificField : x:string -> NewField<'a>
    val defaultOptions : Options<'a>
    val requiredSingle : Options<Common.STLConstants.Scope>
    val requiredMany<'a> : Options<obj>
    val optionalSingle : Options<Common.STLConstants.Scope>
    val optionalMany : Options<Common.STLConstants.Scope>
    val defaultFloat : NewField<'a>
    val defaultInt : NewField<'a>
    val getSettingFromString : full:string -> key:string -> string option
    val parseConfig :
      parseScope:(string -> 'a) ->
        allScopes:'a list ->
          anyScope:'a ->
            filename:string ->
              fileString:string ->
                RootRule<'a> list * TypeDefinition<'a> list *
                EnumDefinition list * ComplexEnumDef list *
                (string * string list) list when 'a : equality
    val parseConfigs :
      parseScope:(string -> 'a) ->
        allScopes:'a list ->
          anyScope:'a ->
            files:(string * string) list ->
              RootRule<'a> list * TypeDefinition<'a> list * EnumDefinition list *
              ComplexEnumDef list * (string * string list) list
        when 'a : equality
  end
