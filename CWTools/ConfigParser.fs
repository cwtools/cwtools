

namespace CWTools.Utilities
  module Position = begin
    val pown32 : n:int -> int
    val pown64 : n:int -> int64
    val mask32 : m:int32 -> n:int -> int
    val mask64 : m:int32 -> n:int -> int64
    type FileIndex = int32
    [<LiteralAttribute ()>]
    val columnBitCount : int
    [<LiteralAttribute ()>]
    val lineBitCount : int
    val posBitCount : int
    val posColumnMask : int
    val lineColumnMask : int
    val inline lsr : x:int -> y:int -> int32
    [<StructAttribute (); CustomEqualityAttribute (); NoComparisonAttribute ();
      System.Diagnostics.DebuggerDisplay ("{Line},{Column}")>]
    type pos =
      struct
        new : code:int32 -> pos
        new : l:int * c:int -> pos
        override Equals : obj:obj -> bool
        override GetHashCode : unit -> int
        override ToString : unit -> string
        member Column : int32
        member Encoding : int32
        member Line : int32
        static member Decode : code:int32 -> pos
        static member EncodingSize : int
      end
    [<LiteralAttribute ()>]
    val fileIndexBitCount : int
    [<LiteralAttribute ()>]
    val startLineBitCount : int
    [<LiteralAttribute ()>]
    val startColumnBitCount : int
    [<LiteralAttribute ()>]
    val heightBitCount : int
    [<LiteralAttribute ()>]
    val endColumnBitCount : int
    [<LiteralAttribute ()>]
    val isSyntheticBitCount : int
    [<LiteralAttribute ()>]
    val fileIndexShift : int
    [<LiteralAttribute ()>]
    val startLineShift : int
    [<LiteralAttribute ()>]
    val startColumnShift : int
    [<LiteralAttribute ()>]
    val heightShift : int
    [<LiteralAttribute ()>]
    val endColumnShift : int
    [<LiteralAttribute ()>]
    val isSyntheticShift : int
    [<LiteralAttribute ()>]
    val fileIndexMask : int64
    [<LiteralAttribute ()>]
    val startLineMask : int64
    [<LiteralAttribute ()>]
    val startColumnMask : int64
    [<LiteralAttribute ()>]
    val heightMask : int64
    [<LiteralAttribute ()>]
    val endColumnMask : int64
    [<LiteralAttribute ()>]
    val isSyntheticMask : int64
    type FileIndexTable =
      class
        new : unit -> FileIndexTable
        member FileToIndex : f:string -> int
        member IndexToFile : n:int -> string
      end
    val maxFileIndex : int
    val mutable fileIndexTable : FileIndexTable
    val fileIndexOfFile : f:string -> int
    val fileOfFileIndex : n:int -> string
    val mkPos : l:int -> c:int -> pos
    [<StructAttribute (); CustomEqualityAttribute (); NoComparisonAttribute ();
      System.Diagnostics.DebuggerDisplay
      ("({StartLine},{StartColumn}-{EndLine},{EndColumn}) {FileName} IsSynthetic={IsSynthetic} -> {DebugCode}")>]
    type range =
      struct
        new : code:int64 -> range
        new : fidx:int32 * b:pos * e:pos -> range
        new : fidx:int32 * bl:int32 * bc:int32 * el:int32 * ec:int32 -> range
        override Equals : obj:obj -> bool
        override GetHashCode : unit -> int
        member MakeSynthetic : unit -> range
        member ToShortString : unit -> string
        override ToString : unit -> string
        member Code : int64
        member DebugCode : string
        member End : pos
        member EndColumn : int32
        member EndLine : int32
        member EndRange : range
        member FileIndex : int32
        member FileName : string
        member IsSynthetic : bool
        member Start : pos
        member StartColumn : int32
        member StartLine : int32
        member StartRange : range
        static member Zero : range
      end
    val mkRange : f:string -> b:pos -> e:pos -> range
    val mkFileIndexRange : fi:int32 -> b:pos -> e:pos -> range
    val orderOn :
      p:('T -> 'U) ->
        pxOrder:System.Collections.Generic.IComparer<'U> ->
          System.Collections.Generic.IComparer<'T>
    val porder :
      compare1:System.Collections.Generic.IComparer<'T1> *
      compare2:System.Collections.Generic.IComparer<'T2> ->
        System.Collections.Generic.IComparer<'T1 * 'T2>
    module Int32 = begin
      val order : System.Collections.Generic.IComparer<int>
    end
    module String = begin
      val order : System.Collections.Generic.IComparer<string>
    end
    val posOrder : System.Collections.Generic.IComparer<pos>
    val rangeOrder : System.Collections.Generic.IComparer<range>
    val outputPos : os:System.IO.TextWriter -> m:pos -> unit
    val outputRange : os:System.IO.TextWriter -> m:range -> unit
    val boutputPos : os:System.Text.StringBuilder -> m:pos -> unit
    val boutputRange : os:System.Text.StringBuilder -> m:range -> unit
    val posGt : p1:pos -> p2:pos -> bool
    val posEq : p1:pos -> p2:pos -> bool
    val posGeq : p1:pos -> p2:pos -> bool
    val posLt : p1:pos -> p2:pos -> bool
    val unionRanges : m1:range -> m2:range -> range
    val rangeContainsRange : m1:range -> m2:range -> bool
    val rangeContainsPos : m1:range -> p:pos -> bool
    val rangeBeforePos : m1:range -> p:pos -> bool
    val rangeN : filename:string -> line:int -> range
    val pos0 : pos
    val range0 : range
    val rangeStartup : range
    val rangeCmdArgs : range
    val trimRangeToLine : r:range -> range
    val stringOfPos : pos:pos -> string
    val stringOfRange : r:range -> string
    type Line0 = int
    type Pos01 = Line0 * int
    type Range01 = Pos01 * Pos01
    module Line = begin
      val fromZ : line:Line0 -> int
      val toZ : line:int -> Line0
    end
    module Pos = begin
      val fromZ : line:Line0 -> idx:int -> pos
      val toZ : p:pos -> Line0 * int32
    end
    module Range = begin
      val toZ : m:range -> (Line0 * int32) * (Line0 * int32)
      val toFileZ : m:range -> string * ((Line0 * int32) * (Line0 * int32))
    end
  end

namespace CWTools.Utilities
  module Utils = begin
    val inline ( == ) : x:string -> y:string -> bool
    type InsensitiveStringComparer =
      class
        interface System.Collections.Generic.IComparer<string>
        new : unit -> InsensitiveStringComparer
      end
    type LocKeySet = Tagged.Set<string,InsensitiveStringComparer>
    val memoize :
      keyFunction:('a -> 'b) -> memFunction:('a -> 'c) -> ('a -> 'c)
        when 'b : equality
    val duration : f:(unit -> 'a) -> s:'b -> 'a
    type LogLevel =
      | Silent
      | Normal
      | Verbose
    val mutable loglevel : LogLevel
    val logInner : level:LogLevel -> message:string -> unit
    val logVerbose : message:string -> unit
    val logNormal : message:string -> unit
    val log : (string -> unit)
    val mkZeroFile : file:string -> Position.range
  end
  module TryParser = begin
    val tryParseWith : tryParseFunc:('a -> bool * 'b) -> ('a -> 'b option)
    val parseDate : (string -> System.DateTime option)
    val parseInt : (string -> int option)
    val parseIntWithDecimal : (string -> int option)
    val parseSingle : (string -> float32 option)
    val parseDouble : (string -> float option)
    val ( |Date|_| ) : (string -> System.DateTime option)
    val ( |Int|_| ) : (string -> int option)
    val ( |Single|_| ) : (string -> float32 option)
    val ( |Double|_| ) : (string -> float option)
  end
  type StringToken = int
  type StringLowerToken = int
  type StringTokens =
    struct
      new : lower:StringLowerToken * normal:StringToken -> StringTokens
      val lower: StringLowerToken
      val normal: StringToken
    end
  [<SealedAttribute ()>]
  type StringResourceManager =
    class
      new : unit -> StringResourceManager
      member GetLowerStringForIDs : id:StringTokens -> string
      member GetStringForID : id:StringToken -> string
      member GetStringForIDs : id:StringTokens -> string
      member InternIdentifierToken : s:string -> StringTokens
    end
  module StringResource = begin
    val mutable stringManager : StringResourceManager
  end

namespace CWTools.Common
  type Game =
    | CK2 = 0
    | HOI4 = 1
    | EU4 = 2
    | STL = 3
  type CK2Lang =
    | English = 0
    | French = 1
    | German = 2
    | Spanish = 3
    | Russian = 4
    | Default = 5
  type STLLang =
    | English = 0
    | French = 1
    | German = 2
    | Spanish = 3
    | Russian = 4
    | Polish = 5
    | Braz_Por = 6
    | Default = 7
    | Chinese = 8
  type HOI4Lang =
    | English = 0
    | French = 1
    | German = 2
    | Spanish = 3
    | Russian = 4
    | Polish = 5
    | Braz_Por = 6
    | Default = 7
  type EU4Lang =
    | English = 0
    | French = 1
    | German = 2
    | Spanish = 3
    | Default = 4
  type IRLang =
    | English = 0
    | French = 1
    | German = 2
    | Spanish = 3
    | Chinese = 4
    | Russian = 5
  type Lang =
    | CK2 of CK2Lang
    | STL of STLLang
    | HOI4 of HOI4Lang
    | EU4 of EU4Lang
    | IR of IRLang
    with
      override ToString : unit -> string
    end
  type RawEffect =
    {name: string;
     desc: string;
     usage: string;
     scopes: string list;
     targets: string list;
     traits: string option;}
  type Severity =
    | Error = 1
    | Warning = 2
    | Information = 3
    | Hint = 4
  type EffectType =
    | Effect
    | Trigger
    | Link
    | ValueTrigger
  type Effect<'T when 'T : comparison> =
    class
      interface System.IComparable
      internal new : name:string * scopes:'T list * effectType:EffectType ->
                       Effect<'T>
      override Equals : y:obj -> bool
      override ToString : unit -> string
      member Name : string
      member Scopes : 'T list
      member ScopesSet : Set<'T>
      member Type : EffectType
    end
  type ScriptedEffect<'T when 'T : comparison> =
    class
      inherit Effect<'T>
      interface System.IComparable
      new : name:string * scopes:'T list * effectType:EffectType *
            comments:string * globals:string list * settargets:string list *
            usedtargets:string list -> ScriptedEffect<'T>
      override Equals : y:obj -> bool
      member Comments : string
      member GlobalEventTargets : string list
      member SavedEventTargets : string list
      member UsedEventTargets : string list
    end
  type DocEffect<'T when 'T : comparison> =
    class
      inherit Effect<'T>
      interface System.IComparable
      new : rawEffect:RawEffect * effectType:EffectType *
            parseScopes:(string -> 'T list) -> DocEffect<'T>
      new : name:string * scopes:'T list * target:'T option *
            effectType:EffectType * desc:string * usage:string -> DocEffect<'T>
      override Equals : y:obj -> bool
      member Desc : string
      member Target : 'T option
      member Usage : string
    end
  type ScopedEffect<'T when 'T : comparison> =
    class
      inherit DocEffect<'T>
      new : de:DocEffect<'T> * inner:'T option -> ScopedEffect<'T>
      new : de:DocEffect<'T> * inner:'T option * isScopeChange:bool *
            ignoreChildren:string list * scopeonlynoteffect:bool * isValue:bool ->
              ScopedEffect<'T>
      new : name:string * scopes:'T list * inner:'T option *
            effectType:EffectType * desc:string * usage:string *
            scopeonlynoteffect:bool -> ScopedEffect<'T>
      new : name:string * scopes:'T list * inner:'T * effectType:EffectType *
            desc:string * usage:string * scopeonlynoteffect:bool ->
              ScopedEffect<'T>
      new : name:string * scopes:'T list * inner:'T option *
            effectType:EffectType * desc:string * usage:string *
            scopeonlynoteffect:bool * isValue:bool -> ScopedEffect<'T>
      new : name:string * scopes:'T list * inner:'T option *
            effectType:EffectType * desc:string * usage:string *
            isScopeChange:bool * ignoreChildren:string list *
            scopeonlynoteffect:bool * isValue:bool * isWildCard:bool ->
              ScopedEffect<'T>
      member IgnoreChildren : string list
      member IsScopeChange : bool
      member IsValueScope : bool
      member IsWildCard : bool
      member ScopeOnlyNotEffect : bool
    end
  type IScope<'T> =
    interface
      abstract member MatchesScope : 'T -> bool
      abstract member AnyScope : 'T
    end
  type IModifier =
    interface
      abstract member Tag : string
    end
  type TitleType =
    | Empire
    | Kingdom
    | Duchy_Hired
    | Duchy_Normal
    | County
    | Barony
  type DataLinkType =
    | Scope
    | Value
    | Both
  type EventTargetDataLink<'S> =
    {name: string;
     inputScopes: 'S list;
     outputScope: 'S;
     description: string;
     dataPrefix: string option;
     sourceRuleType: string;
     dataLinkType: DataLinkType;}
  type EventTargetLink<'S when 'S : comparison> =
    | SimpleLink of ScopedEffect<'S>
    | DataLink of EventTargetDataLink<'S>

namespace CWTools.Common
  module STLConstants = begin
    type Scope =
      | Country
      | Leader
      | GalacticObject
      | Planet
      | Ship
      | Fleet
      | Pop
      | AmbientObject
      | Army
      | Tile
      | Species
      | PopFaction
      | Sector
      | Alliance
      | War
      | Megastructure
      | Any
      | Design
      | Starbase
      | Star
      | Deposit
      | InvalidScope
      with
        interface IScope<Scope>
        override ToString : unit -> string
        static member AnyScope : Scope
      end
    val allScopes : Scope list
    val allScopesSet : Set<Scope>
    val parseScope : x:string -> Scope
    val parseScopes : _arg1:string -> Scope list
    type Effect = Effect<Scope>
    type DocEffect = DocEffect<Scope>
    type ScriptedEffect = ScriptedEffect<Scope>
    type ScopedEffect = ScopedEffect<Scope>
    type ModifierCategory =
      | Pop
      | Science
      | Country
      | Army
      | Leader
      | Planet
      | PopFaction
      | ShipSize
      | Ship
      | Tile
      | Megastructure
      | PlanetClass
      | Starbase
      | Resource
      | Any
    type RawStaticModifier =
      {num: int;
       tag: string;
       name: string;}
    type RawModifier =
      {tag: string;
       category: int;}
    type Modifier =
      {tag: string;
       categories: ModifierCategory list;
       core: bool;}
      with
        interface IModifier
      end
    val createModifier : raw:RawModifier -> Modifier
    val categoryScopeList : (ModifierCategory * Scope list) list
    val modifierCategoryToScopesMap : Map<ModifierCategory,Scope list>
    type EntityType =
      | Agenda = 1
      | AmbientObjects = 2
      | Anomalies = 3
      | Armies = 4
      | ArmyAttachments = 5
      | AscensionPerks = 6
      | Attitudes = 7
      | BombardmentStances = 8
      | BuildablePops = 9
      | BuildingTags = 10
      | Buildings = 11
      | ButtonEffects = 12
      | Bypass = 13
      | CasusBelli = 14
      | Colors = 15
      | ComponentFlags = 16
      | ComponentSets = 17
      | ComponentTags = 18
      | ComponentTemplates = 19
      | CountryCustomization = 20
      | CountryTypes = 21
      | Deposits = 22
      | DiploPhrases = 23
      | DiplomaticActions = 24
      | Edicts = 25
      | Ethics = 26
      | EventChains = 27
      | FallenEmpires = 28
      | GameRules = 29
      | GlobalShipDesigns = 30
      | Governments = 31
      | Authorities = 90
      | Civics = 32
      | GraphicalCulture = 33
      | Mandates = 34
      | MapModes = 35
      | Megastructures = 36
      | NameLists = 37
      | NotificationModifiers = 38
      | ObservationStationMissions = 39
      | OnActions = 40
      | OpinionModifiers = 41
      | Personalities = 42
      | PlanetClasses = 43
      | PlanetModifiers = 44
      | Policies = 45
      | PopFactionTypes = 46
      | PrecursorCivilizations = 47
      | ScriptedEffects = 48
      | ScriptedLoc = 49
      | ScriptedTriggers = 50
      | ScriptedVariables = 51
      | SectionTemplates = 52
      | SectorTypes = 53
      | ShipBehaviors = 54
      | ShipSizes = 55
      | SolarSystemInitializers = 56
      | SpecialProjects = 57
      | SpeciesArchetypes = 58
      | SpeciesClasses = 59
      | SpeciesNames = 60
      | SpeciesRights = 61
      | StarClasses = 62
      | StarbaseBuilding = 63
      | StarbaseLevels = 64
      | StarbaseModules = 65
      | StarbaseTypes = 66
      | SpaceportModules = 67
      | StartScreenMessages = 68
      | StaticModifiers = 69
      | StrategicResources = 70
      | Subjects = 71
      | SystemTypes = 72
      | Technology = 73
      | Terraform = 74
      | TileBlockers = 75
      | TraditionCategories = 76
      | Traditions = 77
      | Traits = 78
      | TriggeredModifiers = 79
      | WarDemandCounters = 80
      | WarDemandTypes = 81
      | WarGoals = 82
      | Events = 83
      | MapGalaxy = 84
      | MapSetupScenarios = 85
      | PrescriptedCountries = 86
      | Interface = 87
      | GfxGfx = 88
      | Other = 89
      | GfxAsset = 90
      | Decisions = 91
    val scriptFolders : string list
  end

namespace CWTools.Common
  module EU4Constants = begin
    type Scope =
      | Country
      | Province
      | TradeNode
      | Unit
      | Monarch
      | Heir
      | Consort
      | RebelFaction
      | Religion
      | Culture
      | Advisor
      | Any
      | InvalidScope
      with
        interface IScope<Scope>
        override ToString : unit -> string
        static member AnyScope : Scope
      end
    val allScopes : Scope list
    val allScopesSet : Set<Scope>
    val parseScope : x:string -> Scope
    val parseScopes : _arg1:string -> Scope list
    type Effect = Effect<Scope>
    type DocEffect = DocEffect<Scope>
    type ScriptedEffect = ScriptedEffect<Scope>
    type ScopedEffect = ScopedEffect<Scope>
    type ModifierCategory =
      | Country
      | Province
      | Any
    type Modifier =
      {tag: string;
       categories: ModifierCategory list;
       core: bool;}
      with
        interface IModifier
      end
    val scriptFolders : string list
  end

namespace CWTools.Common
  module HOI4Constants = begin
    type Scope =
      | State
      | Country
      | UnitLeader
      | Air
      | Any
      | InvalidScope
      with
        interface IScope<Scope>
        override ToString : unit -> string
        static member AnyScope : Scope
      end
    val allScopes : Scope list
    val allScopesSet : Set<Scope>
    val parseScope : x:string -> Scope
    val parseScopes : _arg1:string -> Scope list
    type Effect = Effect<Scope>
    type DocEffect = DocEffect<Scope>
    type ScriptedEffect = ScriptedEffect<Scope>
    type ScopedEffect = ScopedEffect<Scope>
    type ModifierCategory =
      | State
      | Country
      | Unit
      | UnitLeader
      | Air
      | Any
    type Modifier =
      {tag: string;
       categories: ModifierCategory list;
       core: bool;}
      with
        interface IModifier
      end
    val categoryScopeList : (ModifierCategory * Scope list) list
    val modifierCategoryToScopesMap : Map<ModifierCategory,Scope list>
    val scriptFolders : string list
  end

namespace CWTools.Common
  module CK2Constants = begin
    type Scope =
      | Character
      | Title
      | Province
      | Offmap
      | War
      | Siege
      | Unit
      | Religion
      | Culture
      | Society
      | Artifact
      | Bloodline
      | Any
      | InvalidScope
      with
        interface IScope<Scope>
        override ToString : unit -> string
        static member AnyScope : Scope
      end
    val allScopes : Scope list
    val allScopesSet : Set<Scope>
    val parseScope : x:string -> Scope
    val parseScopes : _arg1:string -> Scope list
    type Effect = Effect<Scope>
    type DocEffect = DocEffect<Scope>
    type ScriptedEffect = ScriptedEffect<Scope>
    type ScopedEffect = ScopedEffect<Scope>
    type ModifierCategory =
      | Character
      | Province
      | Unit
      | Any
    type Modifier =
      {tag: string;
       categories: ModifierCategory list;
       core: bool;}
      with
        interface IModifier
      end
    val categoryScopeList : (ModifierCategory * Scope list) list
    val modifierCategoryToScopesMap : Map<ModifierCategory,Scope list>
    val scriptFolders : string list
  end

namespace CWTools.Common
  module IRConstants = begin
    type Scope =
      | NoneScope
      | Value
      | Bool
      | Flag
      | Color
      | Country
      | Character
      | Province
      | Combat
      | Unit
      | Pop
      | Family
      | Party
      | Religion
      | Culture
      | Job
      | CultureGroup
      | Area
      | State
      | Subunit
      | Governorship
      | Region
      | Any
      | InvalidScope
      with
        interface IScope<Scope>
        override ToString : unit -> string
        static member AnyScope : Scope
      end
    val allScopes : Scope list
    val allScopesSet : Set<Scope>
    val parseScope : x:string -> Scope
    val parseScopes : _arg1:string -> Scope list
    type Effect = Effect<Scope>
    type DocEffect = DocEffect<Scope>
    type ScriptedEffect = ScriptedEffect<Scope>
    type ScopedEffect = ScopedEffect<Scope>
    type ModifierCategory =
      | Character
      | Province
      | Country
      | Unit
      | Any
    type Modifier =
      {tag: string;
       categories: ModifierCategory list;
       core: bool;}
      with
        interface IModifier
      end
    val categoryScopeList : (ModifierCategory * Scope list) list
    val modifierCategoryToScopesMap : Map<ModifierCategory,Scope list>
    val scriptFolders : string list
  end

namespace CWTools.Parser
  module Types = begin
    [<StructAttribute ()>]
    type Position =
      | Position of FParsec.Position
      with
        override ToString : unit -> string
        static member Conv : pos:FParsec.Position -> Position
        static member File : fileName:string -> Position
        static member UnConv : pos:Position -> FParsec.Position
        static member Empty : Position
      end
    type Operator =
      | Equals = 0uy
      | GreaterThan = 1uy
      | LessThan = 2uy
      | GreaterThanOrEqual = 3uy
      | LessThanOrEqual = 4uy
      | NotEqual = 5uy
      | EqualEqual = 6uy
    val operatorToString : _arg1:Operator -> string
    [<StructAttribute ()>]
    type Key =
      | Key of string
      with
        override ToString : unit -> string
      end
    [<StructAttribute ()>]
    type KeyValueItem =
      | KeyValueItem of Key * Value * Operator
      with
        override ToString : unit -> string
      end
    and Value =
      | String of string
      | QString of string
      | Float of float
      | Int of int
      | Bool of bool
      | Clause of Statement list
      with
        member ToRawString : unit -> string
        override ToString : unit -> string
      end
    [<CustomEqualityAttribute (); NoComparisonAttribute (); StructAttribute ()>]
    and PosKeyValue =
      | PosKeyValue of Utilities.Position.range * KeyValueItem
      with
        override Equals : y:obj -> bool
        override GetHashCode : unit -> int
      end
    [<CustomEqualityAttribute (); NoComparisonAttribute ()>]
    and Statement =
      | Comment of string
      | KeyValue of PosKeyValue
      | Value of Utilities.Position.range * Value
      with
        override Equals : y:obj -> bool
        override GetHashCode : unit -> int
      end
    [<StructuralEqualityAttribute (); NoComparisonAttribute ()>]
    type ParsedFile = | ParsedFile of Statement list
    type ParseFile = string -> FParsec.CharParsers.ParserResult<ParsedFile,unit>
    type ParseString =
      string -> string -> FParsec.CharParsers.ParserResult<ParsedFile,unit>
    type PrettyPrintFile = ParsedFile -> string
    type PrettyPrintStatements = Statement list -> string
    type PrettyPrintFileResult =
      FParsec.CharParsers.ParserResult<ParsedFile,unit> -> string
    type ParserAPI =
      {parseFile: ParseFile;
       parseString: ParseString;}
    type PrinterAPI =
      {prettyPrintFile: PrettyPrintFile;
       prettyPrintStatements: PrettyPrintStatements;
       prettyPrintFileResult: PrettyPrintFileResult;}
  end

namespace CWTools.Parser
  module internal SharedParsers = begin
    val ( <!> ) :
      p:FParsec.Primitives.Parser<'a,'b> ->
        label:string -> stream:FParsec.CharStream<'b> -> FParsec.Reply<'a>
    val betweenL :
      popen:FParsec.Primitives.Parser<'a,'b> ->
        pclose:FParsec.Primitives.Parser<'c,'b> ->
          p:FParsec.Primitives.Parser<'d,'b> ->
            label:string -> (FParsec.CharStream<'b> -> FParsec.Reply<'d>)
    val whitespaceTextChars : string
    val norseChars : char []
    val idCharArray : char []
    val isAnyofidCharArray : (char -> bool)
    val isidchar : c:char -> bool
    val valueCharArray : char []
    val isAnyValueChar : (char -> bool)
    val isvaluechar : c:char -> bool
    val ws : FParsec.Primitives.Parser<unit,unit>
    val str : s:string -> FParsec.Primitives.Parser<string,unit>
    val strSkip : s:string -> FParsec.Primitives.Parser<unit,unit>
    val ch : c:char -> FParsec.Primitives.Parser<char,unit>
    val chSkip : c:char -> FParsec.Primitives.Parser<unit,unit>
    val clause :
      inner:FParsec.Primitives.Parser<'a,unit> ->
        (FParsec.CharStream<unit> -> FParsec.Reply<'a>)
    val quotedCharSnippet : FParsec.Primitives.Parser<string,unit>
    val escapedChar : FParsec.Primitives.Parser<string,unit>
    val getRange :
      start:FParsec.Position ->
        endp:FParsec.Position -> Utilities.Position.range
    val parseWithPosition :
      p:FParsec.Primitives.Parser<'a,'b> ->
        FParsec.Primitives.Parser<(Utilities.Position.range * 'a),'b>
    val oppLTE : FParsec.Primitives.Parser<Types.Operator,unit>
    val oppGTE : FParsec.Primitives.Parser<Types.Operator,unit>
    val oppNE : FParsec.Primitives.Parser<Types.Operator,unit>
    val oppEE : FParsec.Primitives.Parser<Types.Operator,unit>
    val oppLT : FParsec.Primitives.Parser<Types.Operator,unit>
    val oppGT : FParsec.Primitives.Parser<Types.Operator,unit>
    val oppE : FParsec.Primitives.Parser<Types.Operator,unit>
    val operator : FParsec.Primitives.Parser<Types.Operator,unit>
    val operatorLookahead : FParsec.Primitives.Parser<unit,unit>
    val comment : FParsec.Primitives.Parser<string,unit>
    val key : FParsec.Primitives.Parser<Types.Key,unit>
    val keyQ : FParsec.Primitives.Parser<Types.Key,unit>
    val valueS : FParsec.Primitives.Parser<Types.Value,unit>
    val valueQ : FParsec.Primitives.Parser<Types.Value,unit>
    val valueBYes : FParsec.Primitives.Parser<Types.Value,unit>
    val valueBNo : FParsec.Primitives.Parser<Types.Value,unit>
    val valueI : FParsec.Primitives.Parser<Types.Value,unit>
    val valueF : FParsec.Primitives.Parser<Types.Value,unit>
    val hsv3 : (FParsec.CharStream<unit> -> FParsec.Reply<Types.Value>)
    val hsv4 : (FParsec.CharStream<unit> -> FParsec.Reply<Types.Value>)
    val hsvI : (FParsec.CharStream<unit> -> FParsec.Reply<Types.Value>)
    val hsv : FParsec.Primitives.Parser<Types.Value,unit>
    val rgbI : (FParsec.CharStream<unit> -> FParsec.Reply<Types.Value>)
    val rgb3 : (FParsec.CharStream<unit> -> FParsec.Reply<Types.Value>)
    val rgb4 : (FParsec.CharStream<unit> -> FParsec.Reply<Types.Value>)
    val rgb : FParsec.Primitives.Parser<Types.Value,unit>
    val keyvalueimpl : FParsec.Primitives.Parser<Types.Statement,unit> ref
    val keyvalue : FParsec.Primitives.Parser<Types.Statement,unit>
    val valueimpl : FParsec.Primitives.Parser<Types.Value,unit> ref
    val value : FParsec.Primitives.Parser<Types.Value,unit>
    val leafValue :
      FParsec.Primitives.Parser<(Utilities.Position.range * Types.Value),unit>
    val statement : FParsec.Primitives.Parser<Types.Statement,unit>
    val valueBlock : FParsec.Primitives.Parser<Types.Value,unit>
    val valueClause : FParsec.Primitives.Parser<Types.Value,unit>
    val valueCustom : FParsec.Primitives.Parser<Types.Value,unit>
    val alle : FParsec.Primitives.Parser<Types.ParsedFile,unit>
    val valuelist : FParsec.Primitives.Parser<Types.Statement list,unit>
    val statementlist : FParsec.Primitives.Parser<Types.Statement list,unit>
    val all : FParsec.Primitives.Parser<Types.Statement list,unit>
  end

namespace CWTools.Parser
  module CKParser = begin
    val parseEventFile :
      filepath:string -> FParsec.CharParsers.ParserResult<Types.ParsedFile,unit>
    val private applyParser :
      parser:FParsec.Primitives.Parser<'Result,'UserState> ->
        stream:FParsec.CharStream<'UserState> ->
          FParsec.CharParsers.ParserResult<'Result,'UserState>
    val parseFile :
      filepath:string ->
        FParsec.CharParsers.ParserResult<Types.Statement list,unit>
    val private memoize :
      keyFunction:('a -> 'b) -> memFunction:('a -> 'c) -> ('a -> 'c)
        when 'b : equality
    val parseString :
      fileString:string ->
        filename:string ->
          FParsec.CharParsers.ParserResult<Types.Statement list,unit>
    val parseEventString :
      fileString:string ->
        fileName:string ->
          FParsec.CharParsers.ParserResult<Types.ParsedFile,unit>
    val getSuccess :
      result:FParsec.CharParsers.ParserResult<Types.ParsedFile,'a> ->
        Types.ParsedFile
  end

namespace CWTools.Parser
  module CKPrinter = begin
    val private tabs : n:int -> string
    val private printTroop : depth:int -> t:'a -> string
    val private printValuelist : depth:int -> is:obj list -> string
    val printValue : v:Types.Value -> depth:int -> string
    val printKeyValue : kv:Types.Statement -> depth:int -> string
    val printKeyValueList : kvl:Types.Statement list -> depth:int -> string
    val prettyPrint : ef:Types.ParsedFile -> string
    val prettyPrintResult :
      _arg1:FParsec.CharParsers.ParserResult<Types.ParsedFile,'a> -> string
    val api : Types.PrinterAPI
  end

namespace CWTools.Parser
  module DocsParser = begin
    val private idChar : FParsec.Primitives.Parser<char,unit>
    val private isvaluechar : (char -> bool)
    val private header : FParsec.Primitives.Parser<unit,unit>
    val private name : FParsec.Primitives.Parser<(string * string),unit>
    val private usage : FParsec.Primitives.Parser<string,unit>
    val private usageC : FParsec.Primitives.Parser<string,unit>
    val private scope : FParsec.Primitives.Parser<string,unit>
    val private scopes : FParsec.Primitives.Parser<string list,unit>
    val private scopesC : FParsec.Primitives.Parser<string list,unit>
    val private target : FParsec.Primitives.Parser<string,unit>
    val private targets : FParsec.Primitives.Parser<string list,unit>
    val private doc : FParsec.Primitives.Parser<Common.RawEffect,unit>
    val private footer : FParsec.Primitives.Parser<unit,unit>
    val private docFile : FParsec.Primitives.Parser<Common.RawEffect list,unit>
    val private twoDocs :
      FParsec.Primitives.Parser<(Common.RawEffect list * Common.RawEffect list),
                                unit>
    val toDocEffect :
      effectType:Common.EffectType ->
        parseScopes:(string -> 'a list) ->
          x:Common.RawEffect -> Common.DocEffect<'a> when 'a : comparison
    val processDocs :
      parseScopes:(string -> 'a list) ->
        t:Common.RawEffect list * e:Common.RawEffect list ->
          Common.DocEffect<'a> list * Common.DocEffect<'a> list
        when 'a : comparison
    val parseDocsFile :
      filepath:string ->
        FParsec.CharParsers.ParserResult<(Common.RawEffect list *
                                          Common.RawEffect list),unit>
    val parseDocsFilesRes :
      filepath:string -> Common.RawEffect list * Common.RawEffect list
    val parseDocsStream :
      file:System.IO.Stream ->
        FParsec.CharParsers.ParserResult<(Common.RawEffect list *
                                          Common.RawEffect list),unit>
  end
  module JominiParser = begin
    val private idChar : FParsec.Primitives.Parser<char,unit>
    val private isvaluechar : (char -> bool)
    val private header : FParsec.Primitives.Parser<unit,unit>
    val private spacer : FParsec.Primitives.Parser<unit,unit>
    val private name : FParsec.Primitives.Parser<(string * string),unit>
    val private reqData : FParsec.Primitives.Parser<string,unit>
    val private wildCard : FParsec.Primitives.Parser<string,unit>
    val private inscopes : FParsec.Primitives.Parser<string list,unit>
    val private outscopes : FParsec.Primitives.Parser<string list,unit>
    val private link :
      FParsec.Primitives.Parser<(string * string * string option * string option *
                                 string list option * string list option),unit>
    val private footer : FParsec.Primitives.Parser<string,unit>
    val private linkFile :
      FParsec.Primitives.Parser<(string * string * string option * string option *
                                 string list option * string list option) list,
                                unit>
    val parseLinksFile :
      filepath:string ->
        FParsec.CharParsers.ParserResult<(string * string * string option *
                                          string option * string list option *
                                          string list option) list,unit>
    val parseLinksFilesRes :
      filepath:string ->
        (string * string * string option * string option * string list option *
         string list option) list
    val private triggerheader : FParsec.Primitives.Parser<unit,unit>
    val private effectheader : FParsec.Primitives.Parser<unit,unit>
    val private supportedscopes : FParsec.Primitives.Parser<string list,unit>
    val private supportedtargets : FParsec.Primitives.Parser<string list,unit>
    val private traits : FParsec.Primitives.Parser<string,unit>
    val private tname : FParsec.Primitives.Parser<string,unit>
    val private endOfDesc : FParsec.Primitives.Parser<string,unit>
    val private desc : FParsec.Primitives.Parser<string,unit>
    val private trigger : FParsec.Primitives.Parser<Common.RawEffect,unit>
    val private triggerFile :
      FParsec.Primitives.Parser<Common.RawEffect list,unit>
    val private effectFile :
      FParsec.Primitives.Parser<Common.RawEffect list,unit>
    val parseTriggerFile :
      filepath:string ->
        FParsec.CharParsers.ParserResult<Common.RawEffect list,unit>
    val parseTriggerFilesRes : filepath:string -> Common.RawEffect list
    val parseTriggerStream :
      file:System.IO.Stream ->
        FParsec.CharParsers.ParserResult<Common.RawEffect list,unit>
    val parseEffectFile :
      filepath:string ->
        FParsec.CharParsers.ParserResult<Common.RawEffect list,unit>
    val parseEffectFilesRes : filepath:string -> Common.RawEffect list
    val parseEffectStream :
      file:System.IO.Stream ->
        FParsec.CharParsers.ParserResult<Common.RawEffect list,unit>
    val toDocEffect :
      effectType:Common.EffectType ->
        parseScopes:(string -> 'a list) ->
          x:Common.RawEffect -> Common.DocEffect<'a> when 'a : comparison
    val processEffects :
      parseScopes:(string -> 'a list) ->
        e:Common.RawEffect list -> Common.DocEffect<'a> list
        when 'a : comparison
    val processTriggers :
      parseScopes:(string -> 'a list) ->
        t:Common.RawEffect list -> Common.DocEffect<'a> list
        when 'a : comparison
  end

namespace CWTools.Parser
  module SetupLogParser = begin
    val private isvaluechar : (char -> bool)
    val private str : s:string -> FParsec.Primitives.Parser<string,unit>
    val private header : FParsec.Primitives.Parser<unit,unit>
    val private pre : FParsec.Primitives.Parser<unit,unit>
    val private num : FParsec.Primitives.Parser<int,unit>
    val private tag : FParsec.Primitives.Parser<string,unit>
    val private name : FParsec.Primitives.Parser<string,unit>
    val private staticModifier :
      FParsec.Primitives.Parser<Common.STLConstants.RawStaticModifier,unit>
    val private modifierHeader : FParsec.Primitives.Parser<unit,unit>
    val private mtag : FParsec.Primitives.Parser<string,unit>
    val private cat : FParsec.Primitives.Parser<int,unit>
    val private modifier :
      FParsec.Primitives.Parser<Common.STLConstants.RawModifier,unit>
    val private footer : FParsec.Primitives.Parser<string,unit>
    val private logFile :
      FParsec.Primitives.Parser<(Common.STLConstants.RawStaticModifier list *
                                 Common.STLConstants.RawModifier list),unit>
    val toDocEffect :
      effectType:Common.EffectType ->
        parseScope:(string -> 'a list) ->
          x:Common.RawEffect -> Common.DocEffect<'a> when 'a : comparison
    val parseLogsFile :
      filepath:string ->
        FParsec.CharParsers.ParserResult<(Common.STLConstants.RawStaticModifier list *
                                          Common.STLConstants.RawModifier list),
                                         unit>
    val parseLogsStream :
      file:System.IO.Stream ->
        FParsec.CharParsers.ParserResult<(Common.STLConstants.RawStaticModifier list *
                                          Common.STLConstants.RawModifier list),
                                         unit>
    val processLogs :
      s:Common.STLConstants.RawStaticModifier list *
      m:Common.STLConstants.RawModifier list ->
        Common.STLConstants.Modifier list
  end

namespace CWTools.Localisation
  [<StructAttribute ()>]
  type Entry =
    {key: string;
     value: char option;
     desc: string;
     position: Utilities.Position.range;}
  type GetDesc = string -> string
  type GetKeys = string list
  type Values = System.Collections.Generic.IDictionary<string,string>
  type Results =
    System.Collections.Generic.IDictionary<string,
                                           (bool * int * string *
                                            FParsec.Position option)>
  type ILocalisationAPI =
    interface
      abstract member GetDesc : string -> string
      abstract member GetKeys : string list
      abstract member GetLang : Common.Lang
      abstract member Results : Results
      abstract member ValueMap : Map<string,Entry>
      abstract member
        Values : System.Collections.Generic.IDictionary<string,string>
    end
  type ILocalisationAPICreator =
    interface
      abstract member Api : Common.Lang -> ILocalisationAPI
    end
  type LocalisationSettings =
    {folder: string;}

namespace CWTools.Localisation
  module YAMLLocalisationParser = begin
    type LocFile =
      {key: string;
       entries: Entry list;}
    val inline isLocValueChar : c:char -> bool
    val key : FParsec.Primitives.Parser<string,unit>
    val desc : FParsec.Primitives.Parser<string,unit>
    val value : FParsec.Primitives.Parser<char,unit>
    val getRange :
      start:FParsec.Position ->
        endp:FParsec.Position -> Utilities.Position.range
    val entry : FParsec.Primitives.Parser<Entry,unit>
    val comment : FParsec.Primitives.Parser<string,unit>
    val file : FParsec.Primitives.Parser<LocFile,unit>
    val parseLocFile :
      filepath:string -> FParsec.CharParsers.ParserResult<LocFile,unit>
    val parseLocText :
      text:string ->
        name:string -> FParsec.CharParsers.ParserResult<LocFile,unit>
  end

namespace CWTools.Localisation
  module CK2Localisation = begin
    type LocalisationEntry = FSharp.Data.CsvProvider<...>
    type LocalisationEntryFallback = FSharp.Data.CsvProvider<...>
    type CK2LocalisationService =
      class
        interface ILocalisationAPICreator
        new : files:(string * string) list -> CK2LocalisationService
        new : localisationSettings:LocalisationSettings ->
                CK2LocalisationService
        member Api : lang:Common.Lang -> ILocalisationAPI
        member Results : Results
        member Results : Results with set
      end
  end

namespace CWTools.Localisation
  module EU4Localisation = begin
    type EU4LocalisationService =
      class
        interface ILocalisationAPICreator
        new : files:(string * string) list -> EU4LocalisationService
        new : localisationSettings:LocalisationSettings ->
                EU4LocalisationService
        member Api : lang:Common.Lang -> ILocalisationAPI
      end
  end

namespace CWTools.Localisation
  module HOI4Localisation = begin
    type HOI4LocalisationService =
      class
        interface ILocalisationAPICreator
        new : files:(string * string) list -> HOI4LocalisationService
        new : localisationSettings:LocalisationSettings ->
                HOI4LocalisationService
        member Api : lang:Common.Lang -> ILocalisationAPI
      end
  end

namespace CWTools.Localisation
  module STLLocalisation = begin
    type STLLocalisationService =
      class
        interface ILocalisationAPICreator
        new : files:(string * string) list -> STLLocalisationService
        new : localisationSettings:LocalisationSettings ->
                STLLocalisationService
        member Api : lang:Common.Lang -> ILocalisationAPI
      end
  end

namespace CWTools.Localisation
  module IRLocalisation = begin
    type IRLocalisationService =
      class
        interface ILocalisationAPICreator
        new : files:(string * string) list -> IRLocalisationService
        new : localisationSettings:LocalisationSettings -> IRLocalisationService
        member Api : lang:Common.Lang -> ILocalisationAPI
      end
  end

namespace CWTools.Process
  module List = begin
    val replace :
      f:('a -> bool) -> sub:('a -> 'a) -> xs:'a list -> 'a list option
    val replaceOrAdd :
      f:('a -> bool) -> sub:('a -> 'a) -> add:'a -> xs:'a list -> 'a list
  end
  type IKeyPos =
    interface
      abstract member Key : string
      abstract member Position : Utilities.Position.range
    end
  type IClause =
    interface
      inherit IKeyPos
      abstract member AllArray : Child array
      abstract member Clauses : seq<IClause>
      abstract member LeafValues : seq<LeafValue>
      abstract member Leaves : seq<Leaf>
      abstract member Nodes : seq<Node>
      abstract member ValueClauses : seq<ValueClause>
    end
  [<StructAttribute ()>]
  and Leaf =
    struct
      interface IKeyPos
      new : keyvalueitem:Parser.Types.KeyValueItem *
            ?pos:Utilities.Position.range -> Leaf
      new : key:string * value:Parser.Types.Value * pos:Utilities.Position.range *
            op:Parser.Types.Operator -> Leaf
      val mutable KeyId: Utilities.StringTokens
      val mutable private _valueId: Utilities.StringTokens
      val mutable private _value: Parser.Types.Value
      val mutable Position: Utilities.Position.range
      val mutable Operator: Parser.Types.Operator
      member Key : string
      member ToRaw : Parser.Types.KeyValueItem
      member Value : Parser.Types.Value
      member ValueId : Utilities.StringTokens
      member ValueText : string
      member Key : string with set
      member Value : Parser.Types.Value with set
      static member
        Create : key:Parser.Types.KeyValueItem ->
                   value:Utilities.Position.range -> Child
    end
  and LeafValue =
    class
      interface IKeyPos
      new : value:Parser.Types.Value * ?pos:Utilities.Position.range ->
              LeafValue
      member Key : string
      member Position : Utilities.Position.range
      member ToRaw : Parser.Types.Statement
      member Value : Parser.Types.Value
      member ValueId : Utilities.StringTokens
      member ValueText : string
      member private _value : Parser.Types.Value
      member Value : Parser.Types.Value with set
      member ValueId : Utilities.StringTokens with set
      member private _value : Parser.Types.Value with set
      static member Create : value:Parser.Types.Value -> LeafValue
    end
  [<StructAttribute ()>]
  and Child =
    | NodeC of node: Node
    | LeafC of leaf: Leaf
    | CommentC of comment: string
    | LeafValueC of leafvalue: LeafValue
    | ValueClauseC of valueclause: ValueClause
  and ValueClause =
    class
      interface IClause
      interface IKeyPos
      new : unit -> ValueClause
      new : pos:Utilities.Position.range -> ValueClause
      member Child : x:string -> Node option
      member Childs : x:string -> seq<Node>
      member Has : x:string -> bool
      member Leafs : x:string -> seq<Leaf>
      member SetTag : x:string -> v:Child -> unit
      member Tag : x:string -> Parser.Types.Value option
      member TagText : x:string -> string
      member Tags : x:string -> seq<Parser.Types.Value>
      member TagsText : x:string -> seq<string>
      member All : Child list
      member AllArray : Child array
      member AllChildren : ResizeArray<Child>
      member Children : Node list
      member Clauses : seq<IClause>
      member Comments : seq<string>
      member LeafValues : seq<LeafValue>
      member Leaves : seq<Leaf>
      member Nodes : seq<Node>
      member Position : Utilities.Position.range
      member Scope : Common.STLConstants.Scope
      member ToRaw : Parser.Types.Statement list
      member ValueClauses : seq<ValueClause>
      member Values : Leaf list
      member All : Child list with set
      member AllArray : Child array with set
      member AllChildren : ResizeArray<Child> with set
      member Scope : Common.STLConstants.Scope with set
      static member Create : (Utilities.Position.range -> ValueClause)
    end
  and Node =
    class
      interface IClause
      interface IKeyPos
      new : key:string -> Node
      new : key:string * pos:Utilities.Position.range -> Node
      member Child : x:string -> Node option
      member Childs : x:string -> seq<Node>
      member Has : x:string -> bool
      member Leafs : x:string -> seq<Leaf>
      member SetTag : x:string -> v:Child -> unit
      member Tag : x:string -> Parser.Types.Value option
      member TagText : x:string -> string
      member Tags : x:string -> seq<Parser.Types.Value>
      member TagsText : x:string -> seq<string>
      member All : Child list
      member AllArray : Child array
      member AllChildren : ResizeArray<Child>
      member Children : Node list
      member Clauses : seq<IClause>
      member Comments : seq<string>
      member Key : string
      member KeyId : Utilities.StringTokens
      member LeafValues : seq<LeafValue>
      member Leaves : seq<Leaf>
      member Nodes : seq<Node>
      member Position : Utilities.Position.range
      member Scope : Common.STLConstants.Scope
      member ToRaw : Parser.Types.Statement list
      member ValueClauses : seq<ValueClause>
      member Values : Leaf list
      member All : Child list with set
      member AllArray : Child array with set
      member AllChildren : ResizeArray<Child> with set
      member Key : string with set
      member KeyId : Utilities.StringTokens with set
      member Scope : Common.STLConstants.Scope with set
      static member Create : key:string -> Node
    end
  module ProcessCore = begin
    val processNode :
      postinit:(Node -> Node) ->
        inner:(Parser.Types.Statement -> Child) ->
          key:string ->
            pos:Utilities.Position.range ->
              sl:Parser.Types.Statement list -> Node
    type LookupContext =
      {complete: bool;
       parents: string list;
       scope: string;
       previous: string;
       entityType: Common.STLConstants.EntityType;}
    val processNodeSimple :
      'a ->
        ((Parser.Types.Statement -> Child) -> string ->
           Utilities.Position.range -> Parser.Types.Statement list -> Node)
    type NodeTypeMap =
      string * Utilities.Position.range * LookupContext ->
        (LookupContext -> (Parser.Types.Statement -> Child) -> string ->
           Utilities.Position.range -> Parser.Types.Statement list -> Node) *
        string * (LookupContext -> LookupContext)
    val fst3 : x:'a * 'b * 'c -> 'a
    val snd3 : 'a * x:'b * 'c -> 'b
    val tri3 : 'a * 'b * x:'c -> 'c
    val updateContext :
      f:(LookupContext -> 'a) ->
        n:string -> key:string -> context:LookupContext -> 'a
    type BaseProcess =
      class
        new : maps:NodeTypeMap -> BaseProcess
        member
          ProcessNode : unit ->
                          (string -> Utilities.Position.range ->
                             Parser.Types.Statement list -> Node)
        member
          ProcessNode : entityType:Common.STLConstants.EntityType ->
                          (string -> Utilities.Position.range ->
                             Parser.Types.Statement list -> Node)
      end
    val baseMap :
      'a ->
        ('b -> (Parser.Types.Statement -> Child) -> string ->
           Utilities.Position.range -> Parser.Types.Statement list -> Node) *
        string * ('c -> 'c)
    val processNodeBasic :
      (string -> Utilities.Position.range -> Parser.Types.Statement list -> Node)
    val foldNode : fNode:('r -> Node -> 'r) -> acc:'r -> node:Node -> 'r
    val foldNode2 :
      fNode:(Node -> 'a -> 'b) ->
        fCombine:('b -> 'a -> 'a) -> acc:'a -> node:Node -> 'a
    val foldClause2 :
      fNode:(IClause -> 'a -> 'b) ->
        fCombine:('b -> 'a -> 'a) -> acc:'a -> node:IClause -> 'a
    val foldNode3 : fNode:(Node -> #seq<'b>) -> node:Node -> 'b list
    val foldNode4 : fNode:(Node -> #seq<'b>) -> node:Node -> 'b list
    val foldNode5 : fNode:(Node -> #seq<'b>) -> node:Node -> seq<'b>
    val foldNode6 : fNode:(Node -> 'a) -> node:Node -> 'a list
    val foldNode7 : fNode:(Node -> 'a list -> 'a list) -> node:Node -> 'a list
    val foldNode8 :
      fNode:(Node -> 'a -> 'a) ->
        fCombine:('b list -> 'b) -> acc:'a -> node:Node -> 'b
    val foldNodeWithState :
      fNode:('a -> Node -> 'a * 'b option) -> acc:'a -> node:Node -> 'b list
    val cata : fNode:(Node -> seq<'r> -> 'r) -> node:Node -> 'r
  end

namespace CWTools.Process
  module CK2Process = begin
    type Option =
      class
        inherit Node
        new : key:string * pos:Utilities.Position.range -> Option
        member CustomTooltip : string
        member Name : string
      end
    type Event =
      class
        inherit Node
        new : key:string * pos:Utilities.Position.range -> Event
        member Desc : string
        member Hidden : bool
        member ID : string
      end
    type EventRoot =
      class
        inherit Node
        new : key:obj * pos:obj -> EventRoot
        member Events : Event list
        member Namespace : string
      end
    type ArtifactFile =
      class
        inherit Node
        new : key:obj * pos:obj -> ArtifactFile
        member Slots : Parser.Types.Statement list
        member Weapons : Node list
      end
    val maps :
      string * 'a * 'b ->
        ('c -> (Parser.Types.Statement -> Child) -> string ->
           Utilities.Position.range -> Parser.Types.Statement list -> Node) *
        string * ('d -> 'd)
    val ck2Process : ProcessCore.BaseProcess
    val processCK2Node :
      (string -> Utilities.Position.range -> Parser.Types.Statement list -> Node)
    val processEventFile : ev:Parser.Types.ParsedFile -> EventRoot
    val processArtifact : (Parser.Types.Statement list -> ArtifactFile)
    val getTriggeredEvents : event:Event -> string * string list
    val getTriggeredEventsAll : root:EventRoot -> (string * string list) list
    val getIDs : node:Node -> string list
    val getAllLocalisationKeys :
      localisation:Localisation.ILocalisationAPI ->
        node:Node -> (string * string) list
    val addLocalisedDesc :
      localisation:Localisation.ILocalisationAPI -> node:Node -> unit
    val addLocalisedDescAll :
      root:EventRoot -> localisation:Localisation.ILocalisationAPI -> EventRoot
    val getOption :
      localisation:Localisation.ILocalisationAPI ->
        option:Option -> (string * string) * string list
    val getOptions :
      localisation:Localisation.ILocalisationAPI ->
        event:Event -> ((string * string) * string list) list
    val getEventsOptions :
      localisation:Localisation.ILocalisationAPI ->
        root:EventRoot -> (string * ((string * string) * string list) list) list
    val getEventComments : root:EventRoot -> (string * string) list
    val getImmediate : event:Event -> string list list
    val getAllImmediates : root:EventRoot -> (string * string list list) list
    type eventView =
      {ID: string;
       Desc: string;
       Hidden: bool;
       Key: string;}
    val getEventViews : root:EventRoot -> eventView list
  end

namespace CWTools.Process
  module Scopes = begin
    type EffectMap<'S when 'S : comparison> =
      Tagged.Map<string,Common.Effect<'S>,
                 Utilities.Utils.InsensitiveStringComparer>
    type StringSet =
      Tagged.Set<string,Utilities.Utils.InsensitiveStringComparer>
    type UsageScopeContext<'S> = 'S list
    type OutputScopeContext<'T> =
      {Root: 'T;
       From: 'T list;
       Scopes: 'T list;}
      with
        member GetFrom : i:int -> 'T option
        member CurrentScope : 'T option
        member PopScope : 'T list
      end
    type ScopeContext<'T when 'T :> Common.IScope<'T>> =
      {Root: 'T;
       From: 'T list;
       Scopes: 'T list;}
      with
        member GetFrom : i:int -> 'T
        member CurrentScope : 'T
        member PopScope : 'T list
      end
    type LocContextResult<'S when 'S :> Common.IScope<'S>> =
      | Start of startContext: ScopeContext<'S>
      | NewScope of newScope: ScopeContext<'S>
      | WrongScope of command: string * scope: 'S * expected: 'S list
      | Found of endContext: ScopeContext<'S>
      | LocNotFound of key: string
    [<StructAttribute ()>]
    type LocEntry<'S when 'S :> Common.IScope<'S>> =
      {key: string;
       value: char option;
       desc: string;
       position: Utilities.Position.range;
       scopes: LocContextResult<'S> list;
       refs: string list;}
    type ScopeResult<'T when 'T :> Common.IScope<'T>> =
      | NewScope of newScope: ScopeContext<'T> * ignoreKeys: string list
      | WrongScope of command: string * scope: 'T * expected: 'T list
      | NotFound
      | VarFound
      | VarNotFound of var: string
      | ValueFound
    val simpleVarPrefixFun : prefix:string -> (string -> string * bool)
    val complexVarPrefixFun :
      prefix1:string -> prefix2:string -> (string -> string * bool)
    val private applyTargetScope : scope:'a option -> context:'a list -> 'a list
    val createJominiChangeScope :
      oneToOneScopes:(string *
                      (ScopeContext<'T> * bool -> ScopeContext<'T> * bool)) list ->
        varPrefixFun:(string -> string * bool) ->
          varLHS:bool ->
            skipEffect:bool ->
              eventTargetLinks:EffectMap<'T> ->
                valueTriggers:EffectMap<'T> ->
                  wildcardLinks:Common.ScopedEffect<'T> list ->
                    vars:StringSet ->
                      key:string -> source:ScopeContext<'T> -> ScopeResult<'T>
        when 'T :> Common.IScope<'T> and 'T : comparison
    val createChangeScope :
      oneToOneScopes:(string *
                      (ScopeContext<'T> * bool -> ScopeContext<'T> * bool)) list ->
        varPrefixFun:(string -> string * bool) ->
          varLHS:bool ->
            skipEffect:bool ->
              eventTargetLinks:EffectMap<'T> ->
                _arg5:EffectMap<'T> ->
                  _arg4:Common.ScopedEffect<'T> list ->
                    vars:StringSet ->
                      key:string -> source:ScopeContext<'T> -> ScopeResult<'T>
        when 'T :> Common.IScope<'T> and 'T : comparison
    val createLocalisationCommandValidator :
      locPrimaryScopes:(string *
                        (ScopeContext<'T> * bool -> ScopeContext<'T> * bool)) list ->
        scopedLocEffectsMap:EffectMap<'T> ->
          commands:string list ->
            eventtargets:string list ->
              setvariables:string list ->
                source:ScopeContext<'T> ->
                  command:string -> LocContextResult<'T>
        when 'T :> Common.IScope<'T> and 'T : comparison
    type ChangeScope<'S when 'S :> Common.IScope<'S> and 'S : comparison> =
      bool -> bool -> EffectMap<'S> -> EffectMap<'S> ->
        Common.ScopedEffect<'S> list -> StringSet -> string ->
        ScopeContext<'S> -> ScopeResult<'S>
  end

namespace CWTools.Process
  module STLScopes = begin
    type LocEntry = Scopes.LocEntry<Common.STLConstants.Scope>
    val defaultDesc : string
    val scopedEffects : Common.ScopedEffect<Common.STLConstants.Scope> list
    val effectInnerScopes : (string * Common.STLConstants.Scope) list
    val effectInnerScopeFunctions :
      (string * Common.STLConstants.Scope option * string list) list
    val addInnerScope :
      des:Common.STLConstants.DocEffect list ->
        Common.STLConstants.DocEffect list
    val defaultContext : Scopes.ScopeContext<Common.STLConstants.Scope>
    val noneContext : Scopes.ScopeContext<Common.STLConstants.Scope>
    val oneToOneScopes :
      (string *
       (Scopes.ScopeContext<Common.STLConstants.Scope> * bool ->
          Scopes.ScopeContext<Common.STLConstants.Scope> * bool)) list
    val oneToOneScopesNames : string list
    type EffectMap =
      Tagged.Map<string,Common.STLConstants.Effect,
                 Utilities.Utils.InsensitiveStringComparer>
    val changeScope :
      (bool -> bool -> Scopes.EffectMap<Common.STLConstants.Scope> ->
         Scopes.EffectMap<Common.STLConstants.Scope> ->
         Common.ScopedEffect<Common.STLConstants.Scope> list ->
         Scopes.StringSet -> string ->
         Scopes.ScopeContext<Common.STLConstants.Scope> ->
         Scopes.ScopeResult<Common.STLConstants.Scope>)
    val sourceScope :
      effects:Common.STLConstants.Effect list ->
        key:string -> Common.STLConstants.Scope list
    val scopedLocEffects : Common.ScopedEffect<Common.STLConstants.Scope> list
    val scopedLocEffectsMap :
      Tagged.Map<string,Common.STLConstants.Effect,
                 Utilities.Utils.InsensitiveStringComparer>
    val locPrimaryScopes :
      (string *
       (Scopes.ScopeContext<Common.STLConstants.Scope> * bool ->
          Scopes.ScopeContext<Common.STLConstants.Scope> * bool)) list
    val localisationCommandValidator :
      (string list -> string list -> string list ->
         Scopes.ScopeContext<Common.STLConstants.Scope> -> string ->
         Scopes.LocContextResult<Common.STLConstants.Scope>)
  end

namespace CWTools.Process
  module STLProcess = begin
    val toTriggerBlockKeys : string list
    val _targetKeys : string list
    val targetKeys : string list
    val toEffectBlockKeys : string list
    val ignoreKeys : string list
    val isTargetKey : _arg1:string -> bool
    val scriptedTriggerScope :
      strict:bool ->
        effects:Common.STLConstants.Effect list ->
          triggers:Common.STLConstants.Effect list ->
            root:string -> node:Node -> Set<Common.STLConstants.Scope>
    val findAllUsedEventTargets : event:Node -> Set<string>
    val findAllSavedEventTargets : event:Node -> Set<string>
    val findAllExistsEventTargets : event:Node -> Set<string>
    val findAllSavedGlobalEventTargets : event:Node -> Set<string>
    val getScriptedTriggerScope :
      firstRun:bool ->
        effectType:Common.EffectType ->
          effects:Common.STLConstants.Effect list ->
            triggers:Common.STLConstants.Effect list ->
              Node * string list ->
                Common.ScriptedEffect<Common.STLConstants.Scope>
    val copy : source:obj -> target:obj -> unit
    val optionTriggers : string list
    val optionEffects : string list
    val optionExcludes : string list
    val optionExcludeSet : Set<string>
    val copyChild : a:Child -> Child option
    type Ship =
      class
        inherit Node
        new : key:string * pos:Utilities.Position.range -> Ship
        member Name : string
        member ShipSize : string
      end
    type ShipSection =
      class
        inherit Node
        new : key:string * pos:Utilities.Position.range -> ShipSection
        member Slot : string
        member Template : string
      end
    type ShipComponent =
      class
        inherit Node
        new : key:string * pos:Utilities.Position.range -> ShipComponent
        member Slot : string
        member Template : string
      end
    type Button_Effect =
      class
        inherit Node
        new : key:string * pos:Utilities.Position.range -> Button_Effect
      end
    type Event =
      class
        inherit Node
        new : key:string * pos:Utilities.Position.range -> Event
        member Desc : string
        member Hidden : bool
        member ID : string
      end
    type EffectBlock =
      class
        inherit Node
        new : key:string * pos:Utilities.Position.range -> EffectBlock
      end
    type TriggerBlock =
      class
        inherit Node
        new : key:string * pos:Utilities.Position.range -> TriggerBlock
        member InEffectBlock : bool
        member InEffectBlock : bool with set
      end
    val filterOptionToEffects : o:Node -> EffectBlock
    type Option =
      class
        inherit Node
        new : key:string * pos:Utilities.Position.range -> Option
        member AsEffectBlock : EffectBlock
      end
    type ModifierBlock =
      class
        inherit Node
        new : key:string * pos:Utilities.Position.range -> ModifierBlock
      end
    type WeightBlock =
      class
        inherit Node
        new : key:string * pos:Utilities.Position.range -> WeightBlock
      end
    type WeightModifierBlock =
      class
        inherit Node
        new : key:string * pos:Utilities.Position.range -> WeightModifierBlock
      end
    val shipProcess : ProcessCore.BaseProcess
    val simpleProcess : ProcessCore.BaseProcess
    val staticModifierCategory :
      modifiers:(string * Common.STLConstants.ModifierCategory list) list ->
        node:Node -> Common.STLConstants.ModifierCategory list
    val getStaticModifierCategory :
      modifiers:Common.STLConstants.Modifier list ->
        node:Node -> Common.STLConstants.Modifier
  end

namespace CWTools.Process
  module EU4Scopes = begin
    val defaultContext : Scopes.ScopeContext<Common.EU4Constants.Scope>
    val noneContext : Scopes.ScopeContext<Common.EU4Constants.Scope>
    val defaultDesc : string
    val scopedEffects : Common.ScopedEffect<Common.EU4Constants.Scope> list
    val oneToOneScopes :
      (string *
       (Scopes.ScopeContext<Common.EU4Constants.Scope> * bool ->
          Scopes.ScopeContext<Common.EU4Constants.Scope> * bool)) list
    val oneToOneScopesNames : string list
    type EffectMap =
      Tagged.Map<string,Common.EU4Constants.Effect,
                 Utilities.Utils.InsensitiveStringComparer>
    val changeScope :
      (bool -> bool -> Scopes.EffectMap<Common.EU4Constants.Scope> ->
         Scopes.EffectMap<Common.EU4Constants.Scope> ->
         Common.ScopedEffect<Common.EU4Constants.Scope> list ->
         Scopes.StringSet -> string ->
         Scopes.ScopeContext<Common.EU4Constants.Scope> ->
         Scopes.ScopeResult<Common.EU4Constants.Scope>)
    val scopedLocEffects : Common.ScopedEffect<Common.EU4Constants.Scope> list
    val scopedLocEffectsMap :
      Tagged.Map<string,Common.EU4Constants.Effect,
                 Utilities.Utils.InsensitiveStringComparer>
    val locPrimaryScopes :
      (string *
       (Scopes.ScopeContext<Common.EU4Constants.Scope> * bool ->
          Scopes.ScopeContext<Common.EU4Constants.Scope> * bool)) list
    val localisationCommandValidator :
      (string list -> string list -> string list ->
         Scopes.ScopeContext<Common.EU4Constants.Scope> -> string ->
         Scopes.LocContextResult<Common.EU4Constants.Scope>)
  end

namespace CWTools.Process
  module HOI4Scopes = begin
    val defaultContext : Scopes.ScopeContext<Common.HOI4Constants.Scope>
    val noneContext : Scopes.ScopeContext<Common.HOI4Constants.Scope>
    val defaultDesc : string
    val scopedEffects : Common.ScopedEffect<Common.HOI4Constants.Scope> list
    val oneToOneScopes :
      (string *
       (Scopes.ScopeContext<Common.HOI4Constants.Scope> * bool ->
          Scopes.ScopeContext<Common.HOI4Constants.Scope> * bool)) list
    val oneToOneScopesNames : string list
    type EffectMap =
      Tagged.Map<string,Common.HOI4Constants.Effect,
                 Utilities.Utils.InsensitiveStringComparer>
    val changeScope :
      (bool -> bool -> Scopes.EffectMap<Common.HOI4Constants.Scope> ->
         Scopes.EffectMap<Common.HOI4Constants.Scope> ->
         Common.ScopedEffect<Common.HOI4Constants.Scope> list ->
         Scopes.StringSet -> string ->
         Scopes.ScopeContext<Common.HOI4Constants.Scope> ->
         Scopes.ScopeResult<Common.HOI4Constants.Scope>)
    val scopedLocEffects : Common.ScopedEffect<Common.HOI4Constants.Scope> list
    val scopedLocEffectsMap :
      Tagged.Map<string,Common.HOI4Constants.Effect,
                 Utilities.Utils.InsensitiveStringComparer>
    val locPrimaryScopes :
      (string *
       (Scopes.ScopeContext<Common.HOI4Constants.Scope> * bool ->
          Scopes.ScopeContext<Common.HOI4Constants.Scope> * bool)) list
    val localisationCommandValidator :
      (string list -> string list -> string list ->
         Scopes.ScopeContext<Common.HOI4Constants.Scope> -> string ->
         Scopes.LocContextResult<Common.HOI4Constants.Scope>)
  end

namespace CWTools.Process
  module CK2Scopes = begin
    val defaultContext : Scopes.ScopeContext<Common.CK2Constants.Scope>
    val noneContext : Scopes.ScopeContext<Common.CK2Constants.Scope>
    val defaultDesc : string
    val scopedEffects : Common.ScopedEffect<Common.CK2Constants.Scope> list
    val oneToOneScopes :
      (string *
       (Scopes.ScopeContext<Common.CK2Constants.Scope> * bool ->
          Scopes.ScopeContext<Common.CK2Constants.Scope> * bool)) list
    val oneToOneScopesNames : string list
    type EffectMap =
      Tagged.Map<string,Common.CK2Constants.Effect,
                 Utilities.Utils.InsensitiveStringComparer>
    val changeScope :
      (bool -> bool -> Scopes.EffectMap<Common.CK2Constants.Scope> ->
         Scopes.EffectMap<Common.CK2Constants.Scope> ->
         Common.ScopedEffect<Common.CK2Constants.Scope> list ->
         Scopes.StringSet -> string ->
         Scopes.ScopeContext<Common.CK2Constants.Scope> ->
         Scopes.ScopeResult<Common.CK2Constants.Scope>)
    val scopedLocEffects : Common.ScopedEffect<Common.CK2Constants.Scope> list
    val scopedLocEffectsMap :
      Tagged.Map<string,Common.CK2Constants.Effect,
                 Utilities.Utils.InsensitiveStringComparer>
    val locPrimaryScopes :
      (string *
       (Scopes.ScopeContext<Common.CK2Constants.Scope> * bool ->
          Scopes.ScopeContext<Common.CK2Constants.Scope> * bool)) list
    val localisationCommandValidator :
      (string list -> string list -> string list ->
         Scopes.ScopeContext<Common.CK2Constants.Scope> -> string ->
         Scopes.LocContextResult<Common.CK2Constants.Scope>)
  end

namespace CWTools.Process
  module IRScopes = begin
    val defaultContext : Scopes.ScopeContext<Common.IRConstants.Scope>
    val noneContext : Scopes.ScopeContext<Common.IRConstants.Scope>
    val defaultDesc : string
    val scopedEffects : 'a list
    val oneToOneScopes :
      (string *
       (Scopes.ScopeContext<Common.IRConstants.Scope> * bool ->
          Scopes.ScopeContext<Common.IRConstants.Scope> * bool)) list
    val oneToOneScopesNames : string list
    type EffectMap =
      Tagged.Map<string,Common.IRConstants.Effect,
                 Utilities.Utils.InsensitiveStringComparer>
    val changeScope :
      (bool -> bool -> Scopes.EffectMap<Common.IRConstants.Scope> ->
         Scopes.EffectMap<Common.IRConstants.Scope> ->
         Common.ScopedEffect<Common.IRConstants.Scope> list ->
         Scopes.StringSet -> string ->
         Scopes.ScopeContext<Common.IRConstants.Scope> ->
         Scopes.ScopeResult<Common.IRConstants.Scope>)
    val scopedLocEffects : Common.ScopedEffect<Common.IRConstants.Scope> list
    val scopedLocEffectsMap :
      Tagged.Map<string,Common.IRConstants.Effect,
                 Utilities.Utils.InsensitiveStringComparer>
    val locPrimaryScopes :
      (string *
       (Scopes.ScopeContext<Common.IRConstants.Scope> * bool ->
          Scopes.ScopeContext<Common.IRConstants.Scope> * bool)) list
    val localisationCommandValidator :
      (string list -> string list -> string list ->
         Scopes.ScopeContext<Common.IRConstants.Scope> -> string ->
         Scopes.LocContextResult<Common.IRConstants.Scope>)
  end


namespace CWTools.Parser
  module EU4Parser = begin
    val private parseModifier :
      _arg1:string -> Common.EU4Constants.ModifierCategory
    val loadModifiers :
      filename:string -> fileString:string -> Common.EU4Constants.Modifier list
    val getLocCommands :
      node:Process.Node -> (string * Common.EU4Constants.Scope list) list
    val loadLocCommands :
      filename:string ->
        fileString:string -> (string * Common.EU4Constants.Scope list) list
  end

namespace CWTools.Parser
  module HOI4Parser = begin
    val private parseModifier :
      _arg1:string -> Common.HOI4Constants.ModifierCategory
    val loadModifiers :
      filename:string -> fileString:string -> Common.HOI4Constants.Modifier list
    val private getLocCommands :
      node:Process.Node -> (string * Common.HOI4Constants.Scope list) list
    val loadLocCommands :
      filename:string ->
        fileString:string -> (string * Common.HOI4Constants.Scope list) list
  end

namespace CWTools.Parser
  module STLParser = begin
    val getLocCommands :
      node:Process.Node -> (string * Common.STLConstants.Scope list) list
    val loadLocCommands :
      filename:string ->
        fileString:string -> (string * Common.STLConstants.Scope list) list
  end

namespace CWTools.Parser
  module CK2Parser = begin
    val private parseModifier :
      _arg1:string -> Common.CK2Constants.ModifierCategory
    val loadModifiers :
      filename:string -> fileString:string -> Common.CK2Constants.Modifier list
    val getLocCommands :
      node:Process.Node -> (string * Common.CK2Constants.Scope list) list
    val loadLocCommands :
      filename:string ->
        fileString:string -> (string * Common.CK2Constants.Scope list) list
  end

namespace CWTools.Parser
  module IRParser = begin
    val private parseModifier :
      _arg1:string -> Common.IRConstants.ModifierCategory
    val loadModifiers :
      filename:string -> fileString:string -> Common.IRConstants.Modifier list
    val getLocCommands :
      node:Process.Node -> (string * Common.IRConstants.Scope list) list
    val loadLocCommands :
      filename:string ->
        fileString:string -> (string * Common.IRConstants.Scope list) list
  end

namespace CWTools.Parser
  module UtilityParser = begin
    val parseLink :
      anyScope:'a ->
        parseScope:(string -> 'a) ->
          allScopes:'a list -> node:Process.Node -> Common.EventTargetLink<'a>
        when 'a : comparison
    val loadEventTargetLinks :
      anyScope:'a ->
        parseScope:(string -> 'a) ->
          allScopes:'a list ->
            filename:string ->
              fileString:string -> Common.EventTargetLink<'a> list
        when 'a : comparison
  end

namespace CWTools.Games
  type ComputedData =
    class
      new : referencedtypes:Map<string,(string * Utilities.Position.range) list> option *
            definedvariable:Map<string,(string * Utilities.Position.range) list> option *
            withRulesData:bool * effectBlocks:Process.Node list option *
            triggersBlocks:Process.Node list option -> ComputedData
      member Cache : Map<string,obj list>
      member
        Definedvariables : Map<string,(string * Utilities.Position.range) list> option
      member EffectBlocks : Process.Node list option
      member
        Referencedtypes : Map<string,(string * Utilities.Position.range) list> option
      member TriggerBlocks : Process.Node list option
      member WithRulesData : bool
      member Cache : Map<string,obj list> with set
      member
        Definedvariables : Map<string,(string * Utilities.Position.range) list> option
                             with set
      member EffectBlocks : Process.Node list option with set
      member
        Referencedtypes : Map<string,(string * Utilities.Position.range) list> option
                            with set
      member TriggerBlocks : Process.Node list option with set
      member WithRulesData : bool with set
    end
  type EU4ComputedData =
    class
      inherit ComputedData
      new : referencedtypes:Map<string,(string * Utilities.Position.range) list> option *
            definedvariable:Map<string,(string * Utilities.Position.range) list> option *
            scriptedeffectparams:string list option * withRulesData:bool *
            effectBlocks:Process.Node list option *
            triggersBlocks:Process.Node list option -> EU4ComputedData
      member ScriptedEffectParams : string list option
    end
  type HOI4ComputedData =
    class
      inherit ComputedData
      new : referencedtypes:Map<string,(string * Utilities.Position.range) list> option *
            definedvariable:Map<string,(string * Utilities.Position.range) list> option *
            withRulesData:bool * effectBlocks:Process.Node list option *
            triggersBlocks:Process.Node list option -> HOI4ComputedData
    end
  type CK2ComputedData =
    class
      inherit ComputedData
      new : referencedtypes:Map<string,(string * Utilities.Position.range) list> option *
            definedvariable:Map<string,(string * Utilities.Position.range) list> option *
            withRulesData:bool * effectBlocks:Process.Node list option *
            triggersBlocks:Process.Node list option -> CK2ComputedData
    end
  type IRComputedData =
    class
      inherit ComputedData
      new : referencedtypes:Map<string,(string * Utilities.Position.range) list> option *
            definedvariable:Map<string,(string * Utilities.Position.range) list> option *
            withRulesData:bool * effectBlocks:Process.Node list option *
            triggersBlocks:Process.Node list option -> IRComputedData
    end
  type PassFileResult =
    {parseTime: int64;}
  type FailFileResult =
    {error: string;
     position: FParsec.Position;
     parseTime: int64;}
  type FileResult =
    | Pass of result: PassFileResult
    | Fail of result: FailFileResult
  type Overwrite =
    | No
    | Overwrote
    | Overwritten
  type EntityResource =
    {scope: string;
     filepath: string;
     logicalpath: string;
     result: FileResult;
     overwrite: Overwrite;
     validate: bool;}
  type FileResource =
    {scope: string;
     filepath: string;
     logicalpath: string;}
  type FileWithContentResource =
    {scope: string;
     filetext: string;
     filepath: string;
     extension: string;
     logicalpath: string;
     overwrite: Overwrite;
     validate: bool;}
  type Resource =
    | EntityResource of string * EntityResource
    | FileResource of string * FileResource
    | FileWithContentResource of string * FileWithContentResource
  type EntityResourceInput =
    {scope: string;
     filepath: string;
     logicalpath: string;
     filetext: string;
     validate: bool;}
  type FileResourceInput =
    {scope: string;
     filepath: string;
     logicalpath: string;}
  type FileWithContentResourceInput =
    {scope: string;
     filetext: string;
     filepath: string;
     logicalpath: string;
     validate: bool;}
  type Entity =
    {filepath: string;
     logicalpath: string;
     entity: Process.Node;
     validate: bool;
     entityType: Common.STLConstants.EntityType;
     overwrite: Overwrite;}
    with
      override ToString : unit -> string
    end
  type CachedResourceData =
    {resources: (Resource * Entity) list;
     files: (string * string) list;
     fileIndexTable: Utilities.Position.FileIndexTable;
     stringResourceManager: Utilities.StringResourceManager;}
  type ResourceInput =
    | EntityResourceInput of EntityResourceInput
    | FileResourceInput of FileResourceInput
    | CachedResourceInput of Resource * Entity
    | FileWithContentResourceInput of FileWithContentResourceInput
  type UpdateFile<'T> =
    ResourceInput -> Resource * struct (Entity * Lazy<'T>) option
  type UpdateFiles<'T> =
    ResourceInput list -> (Resource * struct (Entity * Lazy<'T>) option) list
  type GetResources = unit -> Resource list
  type ValidatableFiles = unit -> EntityResource list
  type AllEntities<'T> = unit -> struct (Entity * Lazy<'T>) list
  type ValidatableEntities<'T> = unit -> struct (Entity * Lazy<'T>) list
  type GetFileNames = unit -> string list
  type IResourceAPI<'T when 'T :> ComputedData> =
    interface
      abstract member ForceRecompute : unit -> unit
      abstract member ForceRulesDataGenerate : unit -> unit
      abstract member AllEntities : AllEntities<'T>
      abstract member GetFileNames : GetFileNames
      abstract member GetResources : GetResources
      abstract member UpdateFile : UpdateFile<'T>
      abstract member UpdateFiles : UpdateFiles<'T>
      abstract member ValidatableEntities : ValidatableEntities<'T>
      abstract member ValidatableFiles : ValidatableFiles
    end
  type ResourceManager<'T when 'T :> ComputedData> =
    class
      new : computedDataFunction:(Entity -> 'T) *
            computedDataUpdateFunction:(Entity -> 'T -> unit) *
            encoding:System.Text.Encoding *
            fallbackencoding:System.Text.Encoding -> ResourceManager<'T>
      member
        ManualProcess : filename:string ->
                          filetext:string -> Process.Node option
      member Api : IResourceAPI<'T>
      member ManualProcessResource : (ResourceInput -> Entity option)
    end

namespace CWTools.Games
  module Files = begin
    type FilesScope =
      | All
      | Mods
      | Vanilla
    type FileManager =
      class
        new : rootDirectory:string * modFilter:string option * scope:FilesScope *
              scriptFolders:string list * gameDirName:string *
              encoding:System.Text.Encoding * ignoreGlobList:string list ->
                FileManager
        member AllFilesByPath : unit -> ResourceInput list
        member AllFolders : unit -> (string * string) list
        member ConvertPathToLogicalPath : path:string -> string
        member LocalisationFiles : unit -> (string * string) list
        member ScopeDirectory : string
        member ShouldUseEmbedded : bool
      end
  end



namespace CWTools.Games
  type Lookup<'S,'M
                when 'S : comparison and 'S :> Common.IScope<'S> and
                     'M :> Common.IModifier> =
    class
      new : unit -> Lookup<'S,'M>
      member CK2LandedTitles : Map<(Common.TitleType * bool),string list>
      member CK2provinces : string list
      member EU4ScriptedEffectKeys : string list
      member EU4TrueLegacyGovernments : string list
      member HOI4provinces : string list
      member IRcharacters : string list
      member IRprovinces : string list
      member allCoreLinks : Common.Effect<'S> list
      member configRules : Rules.RootRule<'S> list
      member coreModifiers : 'M list
      member definedScriptVariables : string list
      member effects : Common.Effect<'S> list
      member
        effectsMap : Tagged.Map<string,Common.Effect<'S>,
                                Utilities.Utils.InsensitiveStringComparer>
      member enumDefs : Map<string,(string * string list)>
      member eventTargetLinks : Common.Effect<'S> list
      member
        eventTargetLinksMap : Tagged.Map<string,Common.Effect<'S>,
                                         Utilities.Utils.InsensitiveStringComparer>
      member globalScriptedVariables : string list
      member onlyScriptedEffects : Common.Effect<'S> list
      member onlyScriptedTriggers : Common.Effect<'S> list
      member
        proccessedLoc : (Common.Lang * Map<string,Process.Scopes.LocEntry<'S>>) list
      member rootFolder : string
      member scriptedLoc : string list
      member staticModifiers : 'M list
      member technologies : (string * string list) list
      member triggers : Common.Effect<'S> list
      member
        triggersMap : Tagged.Map<string,Common.Effect<'S>,
                                 Utilities.Utils.InsensitiveStringComparer>
      member typeDefInfo : Map<string,(string * Utilities.Position.range) list>
      member
        typeDefInfoForValidation : Map<string,
                                       (string * Utilities.Position.range) list>
      member
        typeDefInfoRaw : Map<string,
                             (bool * string * Utilities.Position.range) list>
      member typeDefs : Rules.TypeDefinition<'S> list
      member
        valueTriggerMap : Tagged.Map<string,Common.Effect<'S>,
                                     Utilities.Utils.InsensitiveStringComparer>
      member valueTriggers : Common.Effect<'S> list
      member varDefInfo : Map<string,(string * Utilities.Position.range) list>
      member
        CK2LandedTitles : Map<(Common.TitleType * bool),string list> with set
      member CK2provinces : string list with set
      member EU4ScriptedEffectKeys : string list with set
      member EU4TrueLegacyGovernments : string list with set
      member HOI4provinces : string list with set
      member IRcharacters : string list with set
      member IRprovinces : string list with set
      member allCoreLinks : Common.Effect<'S> list with set
      member configRules : Rules.RootRule<'S> list with set
      member coreModifiers : 'M list with set
      member definedScriptVariables : string list with set
      member enumDefs : Map<string,(string * string list)> with set
      member globalScriptedVariables : string list with set
      member onlyScriptedEffects : Common.Effect<'S> list with set
      member onlyScriptedTriggers : Common.Effect<'S> list with set
      member
        proccessedLoc : (Common.Lang * Map<string,Process.Scopes.LocEntry<'S>>) list
                          with set
      member rootFolder : string with set
      member scriptedLoc : string list with set
      member staticModifiers : 'M list with set
      member technologies : (string * string list) list with set
      member
        typeDefInfoForValidation : Map<string,
                                       (string * Utilities.Position.range) list>
                                     with set
      member
        typeDefInfoRaw : Map<string,
                             (bool * string * Utilities.Position.range) list>
                           with set
      member typeDefs : Rules.TypeDefinition<'S> list with set
      member
        varDefInfo : Map<string,(string * Utilities.Position.range) list>
                       with set
    end

namespace CWTools.Games
  type References<'T,'S,'M
                    when 'T :> ComputedData and 'S : comparison and
                         'S :> Common.IScope<'S> and 'M :> Common.IModifier> =
    class
      new : resourceManager:IResourceAPI<'T> * lookup:Lookup<'S,'M> *
            localisation:Localisation.ILocalisationAPI list ->
              References<'T,'S,'M>
      member ConfigRules : Rules.RootRule<'S> list
      member EffectNames : string list
      member EventIDs : string list
      member Localisation : (string * Localisation.Entry) list
      member ModifierNames : string list
      member ScopeNames : string list
      member ScriptVariableNames : string list
      member Technologies : (string * string list) list
      member TriggerNames : string list
      member TypeMapInfo : Map<string,(string * Utilities.Position.range) list>
    end

namespace CWTools.Games
  type SymbolLocalisationInfo =
    {key: string;
     value: string;}
  type SymbolInformation =
    {typename: string;
     name: string;
     localisation: SymbolLocalisationInfo list;
     ruleDescription: string option;
     ruleRequiredScopes: string list;}
  type CWError =
    string * Common.Severity * Utilities.Position.range * int * string *
    string option
  type CompletionResponse =
    | Simple of label: string * score: int option
    | Detailed of label: string * desc: string option * score: int option
    | Snippet of
      label: string * snippet: string * desc: string option * score: int option
    with
      static member CreateSimple : label:string -> CompletionResponse
      static member
        CreateSnippet : label:string * snippet:string * desc:string option ->
                          CompletionResponse
    end
  type IGame =
    interface
      abstract member AllFiles : unit -> Resource list
      abstract member AllLoadedLocalisation : unit -> string list
      abstract member
        Complete : Utilities.Position.pos ->
                     string -> string -> CompletionResponse list
      abstract member
        FindAllRefs : Utilities.Position.pos ->
                        string -> string -> Utilities.Position.range list option
      abstract member Folders : unit -> (string * string) list
      abstract member ForceRecompute : unit -> unit
      abstract member
        GoToType : Utilities.Position.pos ->
                     string -> string -> Utilities.Position.range option
      abstract member
        InfoAtPos : Utilities.Position.pos ->
                      string -> string -> SymbolInformation option
      abstract member LocalisationErrors : bool * bool -> CWError list
      abstract member
        ParserErrors : unit -> (string * string * FParsec.Position) list
      abstract member RefreshCaches : unit -> unit
      abstract member RefreshLocalisationCaches : unit -> unit
      abstract member ReplaceConfigRules : (string * string) list -> unit
      abstract member
        Types : unit -> Map<string,(string * Utilities.Position.range) list>
      abstract member
        UpdateFile : bool -> string -> string option -> CWError list
      abstract member ValidationErrors : unit -> CWError list
    end
  type IGame<'S when 'S : comparison> =
    interface
      inherit IGame
      abstract member
        ScopesAtPos : Utilities.Position.pos ->
                        string ->
                          string -> Process.Scopes.OutputScopeContext<'S> option
      abstract member ScriptedEffects : unit -> Common.Effect<'S> list
      abstract member ScriptedTriggers : unit -> Common.Effect<'S> list
      abstract member
        StaticModifiers : unit -> Common.STLConstants.Modifier list
    end
  type IGame<'T,'S,'M
               when 'T :> ComputedData and 'S : comparison and
                    'S :> Common.IScope<'S> and 'M :> Common.IModifier> =
    interface
      inherit IGame<'S>
      abstract member AllEntities : unit -> struct (Entity * Lazy<'T>) list
      abstract member References : unit -> References<'T,'S,'M>
    end

namespace CWTools.Games.Stellaris
  module STLLookup = begin
    type FlagType =
      | Country
      | Planet
      | Fleet
      | Ship
      | Pop
      | Global
      | Star
      | Relation
      | Leader
      | AmbientObject
      | Species
      | Megastructure
      | PopFaction
    type STLComputedData =
      class
        inherit ComputedData
        new : eventids:string list * setvariables:string list *
              setflags:(FlagType * string) list * savedeventtargets:string list *
              referencedtypes:Map<string,
                                  (string * Utilities.Position.range) list> option *
              hastechs:string list *
              definedvariable:Map<string,
                                  (string * Utilities.Position.range) list> option *
              withRulesData:bool * effectBlocks:Process.Node list option *
              triggersBlocks:Process.Node list option -> STLComputedData
        member Eventids : string list
        member Hastechs : string list
        member Savedeventtargets : string list
        member Setflags : (FlagType * string) list
        member Setvariables : string list
      end
    val getChildrenWithComments :
      root:Process.Node -> (Process.Node * string list) list
    val updateScriptedTriggers :
      resources:IResourceAPI<STLComputedData> ->
        vanillaTriggers:Common.STLConstants.Effect list ->
          Common.STLConstants.Effect list * Common.STLConstants.Effect list
    val manualEffectScopeOverrides : Map<string,Common.STLConstants.Scope list>
    val updateScriptedEffects :
      resources:IResourceAPI<STLComputedData> ->
        vanillaEffects:Common.STLConstants.Effect list ->
          scriptedTriggers:Common.STLConstants.Effect list ->
            Common.STLConstants.Effect list * Common.STLConstants.Effect list
  end

namespace CWTools.Validation
  module ValidationCore = begin
    type ErrorCode =
      {ID: string;
       Severity: Common.Severity;
       Message: string;}
    type ErrorCodes =
      class
        static member AmbiguousIfElse : ErrorCode
        static member ButtonEffectMissing : (string -> ErrorCode)
        static member
          ConfigRulesErrorInTarget : (string -> string -> string -> ErrorCode)
        static member ConfigRulesExpectedVariableValue : ErrorCode
        static member ConfigRulesInvalidScopeCommand : (string -> ErrorCode)
        static member ConfigRulesInvalidTarget : (string -> string -> ErrorCode)
        static member
          ConfigRulesRuleWrongScope : (string -> string -> string -> ErrorCode)
        static member
          ConfigRulesTargetWrongScope : (string -> string -> string -> ErrorCode)
        static member
          ConfigRulesUnexpectedProperty : (string -> Common.Severity ->
                                             ErrorCode)
        static member
          ConfigRulesUnexpectedValue : (string -> Common.Severity -> ErrorCode)
        static member ConfigRulesUnsetVariable : (string -> ErrorCode)
        static member
          ConfigRulesWrongNumber : (string -> Common.Severity -> ErrorCode)
        static member CustomError : (string -> Common.Severity -> ErrorCode)
        static member DeprecatedElse : ErrorCode
        static member DeprecatedSetName : ErrorCode
        static member EventEveryTick : ErrorCode
        static member IfElseOrder : ErrorCode
        static member
          IncorrectEffectScope : (string -> string -> string -> ErrorCode)
        static member
          IncorrectModifierScope : (string -> string -> string -> ErrorCode)
        static member IncorrectNotUsage : ErrorCode
        static member IncorrectScopeAsLeaf : (string -> string -> ErrorCode)
        static member
          IncorrectScopeScope : (string -> string -> string -> ErrorCode)
        static member
          IncorrectStaticModifierScope : (string -> string -> string ->
                                            ErrorCode)
        static member
          IncorrectTriggerScope : (string -> string -> string -> ErrorCode)
        static member InvalidLocCommand : (string -> string -> ErrorCode)
        static member
          LocCommandWrongScope : (string -> string -> string -> ErrorCode)
        static member
          LocFileLangMismatch : (Common.STLLang -> Common.STLLang -> ErrorCode)
        static member LocFileLangWrongPlace : ErrorCode
        static member MaybeUnsavedEventTarget : (string -> string -> ErrorCode)
        static member
          MismatchedComponentAndSlot : (string -> string -> string -> string ->
                                          ErrorCode)
        static member MissingFile : (string -> ErrorCode)
        static member MissingLocFileLang : ErrorCode
        static member MissingLocFileLangHeader : ErrorCode
        static member MissingLocalisation : (string -> Common.Lang -> ErrorCode)
        static member MissingSectionSlot : (string -> string -> ErrorCode)
        static member MixedBlock : ErrorCode
        static member PlanetKillerMissing : (string -> ErrorCode)
        static member RecursiveLocRef : ErrorCode
        static member RedundantBoolean : ErrorCode
        static member ReplaceMeLoc : (string -> Common.Lang -> ErrorCode)
        static member ResearchLeaderArea : ErrorCode
        static member ResearchLeaderTech : (string -> string -> ErrorCode)
        static member RulesError : (string -> Common.Severity -> ErrorCode)
        static member SpriteMissing : (string -> ErrorCode)
        static member TechCatMissing : ErrorCode
        static member UndefinedEffect : (string -> ErrorCode)
        static member UndefinedEntity : (string -> ErrorCode)
        static member UndefinedEvent : (string -> ErrorCode)
        static member
          UndefinedFlag : (string -> Games.Stellaris.STLLookup.FlagType ->
                             ErrorCode)
        static member
          UndefinedLocReference : (string -> string -> obj -> ErrorCode)
        static member UndefinedModifier : (string -> ErrorCode)
        static member UndefinedPDXMesh : (string -> ErrorCode)
        static member UndefinedScriptVariable : (string -> ErrorCode)
        static member UndefinedSectionEntity : (string -> string -> ErrorCode)
        static member
          UndefinedSectionEntityFallback : (string -> string -> string ->
                                              ErrorCode)
        static member UndefinedStaticModifier : (string -> ErrorCode)
        static member UndefinedTrigger : (string -> ErrorCode)
        static member UndefinedVariable : (string -> ErrorCode)
        static member UnknownComponentTemplate : (string -> ErrorCode)
        static member UnknownSectionTemplate : (string -> ErrorCode)
        static member UnnecessaryBoolean : (string -> ErrorCode)
        static member UnsavedEventTarget : (string -> string -> ErrorCode)
        static member UnusedTech : (string -> ErrorCode)
        static member WrongEncoding : ErrorCode
        static member ZeroModifier : (string -> ErrorCode)
      end
    type ValidationResult =
      | OK
      | Invalid of Games.CWError list
    val inline invData :
      code:ErrorCode ->
        l:Process.IKeyPos ->
          data:string option ->
            string * Common.Severity * Utilities.Position.range * int * string *
            string option
    val inline inv :
      code:ErrorCode ->
        l:Process.IKeyPos ->
          string * Common.Severity * Utilities.Position.range * int * string *
          string option
    val invLeafValue :
      code:ErrorCode ->
        lv:Process.LeafValue ->
          data:string option ->
            string * Common.Severity * Utilities.Position.range * int * string *
            string option
    val invManual :
      code:ErrorCode ->
        pos:Utilities.Position.range ->
          key:string ->
            data:string option ->
              string * Common.Severity * Utilities.Position.range * int * string *
              string option
    val inline invCustom :
      l:Process.IKeyPos ->
        string * Common.Severity * Utilities.Position.range * int * string *
        string option
    type Validator<'T when 'T :> Process.Node> = 'T -> ValidationResult
    val ( <&> ) :
      f1:('a -> ValidationResult) ->
        f2:('a -> ValidationResult) -> x:'a -> ValidationResult
    val ( <&&> ) :
      f1:ValidationResult -> f2:ValidationResult -> ValidationResult
    val ( <&&&> ) :
      string * Common.Severity * Utilities.Position.range * int * string *
      string option -> f:ValidationResult -> ValidationResult
    val ( <&?&> ) :
      f1:ValidationResult -> f2:ValidationResult -> ValidationResult
    val mergeValidationErrors :
      errorcode:string ->
        (('a * 'b * 'c * 'd * string * 'e) list ->
           'a * 'b * 'c * 'd * string * 'e)
    val ( <&!!&> ) :
      es:seq<'a> -> f:('a -> ValidationResult) -> ValidationResult
    val ( <&!&> ) : es:seq<'a> -> f:('a -> ValidationResult) -> ValidationResult
    val applyToAll : es:seq<'a> -> f:('b -> 'a -> 'b) -> e:'b -> 'b
    val ( <&??&> ) :
      es:seq<'a> -> f:('a -> ValidationResult) -> ValidationResult
    val inline checkNonNull : argName:string -> arg:'a -> unit
    val takeWhileOne : predicate:('a -> bool) -> source:seq<'a> -> seq<'a>
    val merger :
      original:ValidationResult -> news:ValidationResult -> Games.CWError option
    val lazyErrorMerge :
      rs:seq<'a> ->
        f:('a -> ValidationResult -> ValidationResult) ->
          defValue:(unit -> ValidationResult) ->
            errors:ValidationResult -> merge:bool -> ValidationResult
    type EntitySet<'T when 'T :> Games.ComputedData> =
      class
        new : entities:struct (Games.Entity * Lazy<'T>) list -> EntitySet<'T>
        member
          AddOrGetCached : id:string ->
                             generator:(Games.Entity -> obj list) -> obj list
        member
          AllOfType : entityType:Common.STLConstants.EntityType ->
                        (Process.Node * Lazy<'T>) list
        member
          AllOfTypeChildren : entityType:Common.STLConstants.EntityType ->
                                Process.Node list
        member GlobMatch : pattern:string -> Process.Node list
        member GlobMatchChildren : pattern:string -> Process.Node list
        member All : Process.Node list
        member AllEffects : Process.Node list
        member AllModifiers : Process.STLProcess.WeightModifierBlock list
        member AllTriggers : Process.Node list
        member AllWithData : (Process.Node * Lazy<'T>) list
        member Raw : struct (Games.Entity * Lazy<'T>) list
      end
    type STLEntitySet = EntitySet<Games.Stellaris.STLLookup.STLComputedData>
    type EU4EntitySet = EntitySet<Games.EU4ComputedData>
    type StructureValidator<'T when 'T :> Games.ComputedData> =
      EntitySet<'T> -> EntitySet<'T> -> ValidationResult
    type STLStructureValidator =
      StructureValidator<Games.Stellaris.STLLookup.STLComputedData>
    type EU4StructureValidator = StructureValidator<Games.EU4ComputedData>
    type FileValidator<'T when 'T :> Games.ComputedData> =
      Games.IResourceAPI<'T> -> EntitySet<'T> -> ValidationResult
    type STLFileValidator =
      FileValidator<Games.Stellaris.STLLookup.STLComputedData>
    type LookupValidator<'T,'S,'M
                           when 'T :> Games.ComputedData and 'S : comparison and
                                'S :> Common.IScope<'S> and
                                'M :> Common.IModifier> =
      Games.Lookup<'S,'M> -> StructureValidator<'T>
    type LocalisationValidator<'T when 'T :> Games.ComputedData> =
      EntitySet<'T> -> (Common.Lang * Set<string>) list -> EntitySet<'T> ->
        ValidationResult
  end

namespace CWTools.Games
  type LocalisationManager<'S,'T,'M
                             when 'S : comparison and 'S :> Common.IScope<'S> and
                                  'T :> ComputedData and 'M :> Common.IModifier> =
    class
      new : resources:IResourceAPI<'T> *
            localisationService:((string * string) list ->
                                   Localisation.ILocalisationAPICreator) *
            langs:Common.Lang list * lookup:Lookup<'S,'M> *
            processLocalisation:(Lookup<'S,'M> ->
                                   Common.Lang * Map<string,Localisation.Entry> ->
                                   Common.Lang *
                                   Map<string,Process.Scopes.LocEntry<'S>>) *
            localisationExtension:string -> LocalisationManager<'S,'T,'M>
      member
        LocalisationAPIs : unit -> (bool * Localisation.ILocalisationAPI) list
      member LocalisationFileNames : unit -> string list
      member LocalisationKeys : unit -> (Common.Lang * Set<string>) list
      member UpdateAllLocalisation : unit -> unit
      member UpdateLocalisationFile : locFile:FileWithContentResource -> unit
      member UpdateProcessedLocalisation : unit -> unit
      member globalLocalisationErrors : CWError list option
      member localisationErrors : CWError list option
      member localisationKeys : (Common.Lang * Set<string>) list
      member rulesLocalisationErrors : CWError list option
      member
        taggedLocalisationKeys : (Common.Lang * Utilities.Utils.LocKeySet) list
      member globalLocalisationErrors : CWError list option with set
      member localisationErrors : CWError list option with set
      member localisationKeys : (Common.Lang * Set<string>) list with set
      member rulesLocalisationErrors : CWError list option with set
      member
        taggedLocalisationKeys : (Common.Lang * Utilities.Utils.LocKeySet) list
                                   with set
    end

namespace CWTools.Validation
  module LocalisationString = begin
    type LocElement =
      | Ref of string
      | Command of string
      | Chars of string
    val valueChars : FParsec.Primitives.Parser<string,unit>
    val dollarChars : FParsec.Primitives.Parser<string,unit>
    val dollarColour : FParsec.Primitives.Parser<char,unit>
    val commandChars : FParsec.Primitives.Parser<string,unit>
    val ref : FParsec.Primitives.Parser<LocElement,unit>
    val command : FParsec.Primitives.Parser<LocElement,unit>
    val locStringParser : FParsec.Primitives.Parser<LocElement list,unit>
    val parseLocString :
      fileString:string ->
        filename:string ->
          FParsec.CharParsers.ParserResult<LocElement list,unit>
    val checkRef :
      hardcodedLocalisation:string list ->
        lang:Common.Lang ->
          keys:Utilities.Utils.LocKeySet ->
            entry:Process.Scopes.LocEntry<'a> ->
              r:string -> ValidationCore.ValidationResult
        when 'a :> Common.IScope<'a>
    val validateProcessedLocalisationBase :
      hardcodedLocalisation:string list ->
        keys:(Common.Lang * Utilities.Utils.LocKeySet) list ->
          api:(Common.Lang * Map<string,Process.Scopes.LocEntry<'a>>) list ->
            ValidationCore.ValidationResult when 'a :> Common.IScope<'a>
    val processLocalisationBase :
      localisationCommandValidator:(string list -> string list -> string list ->
                                      Process.Scopes.ScopeContext<'S> ->
                                      string ->
                                      Process.Scopes.LocContextResult<'S>) ->
        defaultContext:Process.Scopes.ScopeContext<'S> ->
          commands:(string * 'S list) list ->
            eventTargets:string list ->
              scriptedLoc:string list ->
                setvariables:string list ->
                  Common.Lang * Map<string,Localisation.Entry> ->
                    Common.Lang * Map<string,Process.Scopes.LocEntry<'S>>
        when 'S :> Common.IScope<'S>
    val validateLocalisationCommandsBase :
      localisationCommandValidator:(string list -> string list -> string list ->
                                      Process.Scopes.ScopeContext<'S> ->
                                      string ->
                                      Process.Scopes.LocContextResult<'S>) ->
        commands:(string * 'S list) list ->
          eventTargets:string list ->
            scriptedLoc:string list ->
              setvariables:string list ->
                locentry:Process.Scopes.LocEntry<'S> ->
                  startContext:Process.Scopes.ScopeContext<'S> ->
                    ValidationCore.ValidationResult when 'S :> Common.IScope<'S>
    val private getRange :
      start:FParsec.Position ->
        endp:FParsec.Position -> Utilities.Position.range
    val validateLocalisationSyntax :
      results:Localisation.Results -> ValidationCore.ValidationResult
  end

namespace CWTools.Validation.Common
  module CommonValidation = begin
    val validateMixedBlocks :
      ValidationCore.EntitySet<#Games.ComputedData> ->
        es:ValidationCore.EntitySet<#Games.ComputedData> ->
          ValidationCore.ValidationResult
    val validateEU4NaiveNot :
      ValidationCore.EntitySet<#Games.ComputedData> ->
        es:ValidationCore.EntitySet<#Games.ComputedData> ->
          ValidationCore.ValidationResult
    val validateNOTMultiple :
      ValidationCore.EntitySet<#Games.ComputedData> ->
        es:ValidationCore.EntitySet<#Games.ComputedData> ->
          ValidationCore.ValidationResult
  end

namespace CWTools.Validation.Stellaris
  module STLValidation = begin
    type S = Common.Severity
    val shipName :
      ship:Process.STLProcess.Ship -> ValidationCore.ValidationResult
    val shipSize :
      ship:Process.STLProcess.Ship -> ValidationCore.ValidationResult
    val validateShips :
      ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val getDefinedVariables : node:Process.Node -> string list
    val checkUsedVariables :
      node:Process.Node ->
        variables:string list -> ValidationCore.ValidationResult
    val validateVariables :
      os:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val inline checkCategoryInScope :
      modifier:string ->
        scope:Common.STLConstants.Scope ->
          node:#Process.IKeyPos ->
            cat:Common.STLConstants.ModifierCategory ->
              ValidationCore.ValidationResult
    val inline valStaticModifier :
      modifiers:Common.STLConstants.Modifier list ->
        scopes:Process.Scopes.ScopeContext<Common.STLConstants.Scope> ->
          modifier:string ->
            node:Process.IKeyPos -> ValidationCore.ValidationResult
    val valNotUsage : node:Process.Node -> ValidationCore.ValidationResult
    val validateEventValsInternal :
      event:Process.Node -> ValidationCore.ValidationResult
    val validateEvents :
      ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val valResearchLeader :
      area:string ->
        cat:string option ->
          node:Process.Node -> ValidationCore.ValidationResult
    val valTechnology :
      ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val valButtonEffects :
      os:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val valSprites :
      os:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val valSpriteFiles :
      rm:Games.IResourceAPI<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val findAllSetVariables : node:Process.Node -> string list
    val validateUsedVariables :
      variables:string list ->
        node:Process.Node -> ValidationCore.ValidationResult
    val getDefinedScriptVariables :
      es:ValidationCore.STLEntitySet -> string list
    val getEntitySetVariables : e:Games.Entity -> string list
    val valVariables :
      os:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val hasFlagMap :
      flags:Map<Games.Stellaris.STLLookup.FlagType,string list> ->
        leaf:Process.Leaf -> ValidationCore.ValidationResult
    val validateUsedFlags :
      flags:Map<Games.Stellaris.STLLookup.FlagType,string list> ->
        node:Process.Node -> ValidationCore.ValidationResult
    val valTest :
      os:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val inline checkModifierInScope :
      modifier:string ->
        scope:Common.STLConstants.Scope ->
          node:#Process.IKeyPos ->
            cat:Common.STLConstants.ModifierCategory ->
              ValidationCore.ValidationResult
    val valModifier :
      modifiers:Common.STLConstants.Modifier list ->
        scope:Common.STLConstants.Scope ->
          leaf:Process.Leaf -> ValidationCore.ValidationResult
    val valModifiers :
      modifiers:Common.STLConstants.Modifier list ->
        node:Process.STLProcess.ModifierBlock -> ValidationCore.ValidationResult
    val valAllModifiers :
      lu:Games.Lookup<'b,Common.STLConstants.Modifier> ->
        _arg1:ValidationCore.EntitySet<#Games.ComputedData> ->
          es:ValidationCore.EntitySet<#Games.ComputedData> ->
            ValidationCore.ValidationResult
        when 'b : comparison and 'b :> Common.IScope<'b>
    val addGeneratedModifiers :
      modifiers:Common.STLConstants.Modifier list ->
        es:ValidationCore.STLEntitySet -> Common.STLConstants.Modifier list
    val findAllSavedEventTargets : event:Process.Node -> string list
    val findAllSavedEventTargetsInEntity : e:Games.Entity -> string list
    val getTechnologies :
      es:ValidationCore.STLEntitySet -> (string * string list) list
    val validateTechnologies :
      os:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val validateShipDesigns :
      os:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val validateSolarSystemInitializers :
      os:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val validatePlanetKillers :
      os:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val validateAnomaly210 :
      ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val validateIfElse210 :
      ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val validateIfElse :
      ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    type BoolState =
      | AND
      | OR
    val validateRedundantAND :
      ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val validateDeprecatedSetName :
      ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
  end

namespace CWTools.Validation.Stellaris
  module ScopeValidation = begin
    val valTriggerLeafUsage :
      modifiers:Common.STLConstants.Modifier list ->
        scopes:Process.Scopes.ScopeContext<Common.STLConstants.Scope> ->
          leaf:Process.Leaf -> ValidationCore.ValidationResult
    val valTriggerNodeUsage :
      modifiers:Common.STLConstants.Modifier list ->
        scopes:Process.Scopes.ScopeContext<Common.STLConstants.Scope> ->
          node:Process.Node -> ValidationCore.ValidationResult
    val valEffectLeafUsage :
      modifiers:Common.STLConstants.Modifier list ->
        scopes:Process.Scopes.ScopeContext<Common.STLConstants.Scope> ->
          leaf:Process.Leaf -> ValidationCore.ValidationResult
    val valEffectNodeUsage :
      modifiers:Common.STLConstants.Modifier list ->
        scopes:Process.Scopes.ScopeContext<Common.STLConstants.Scope> ->
          node:Process.Node -> ValidationCore.ValidationResult
    val copy : source:obj -> target:obj -> unit
  end

namespace CWTools.Validation.Stellaris
  module STLLocalisationString = begin
    val hardcodedLocalisation : string list
    val commands : string list
    val locCommands : (string * Common.STLConstants.Scope list) list
    val validateProcessedLocalisation :
      ((Common.Lang * Utilities.Utils.LocKeySet) list ->
         (Common.Lang *
          Map<string,Process.Scopes.LocEntry<Common.STLConstants.Scope>>) list ->
         ValidationCore.ValidationResult)
    val processLocalisation :
      ((string * Common.STLConstants.Scope list) list -> string list ->
         string list -> string list ->
         Common.Lang * Map<string,Localisation.Entry> ->
         Common.Lang *
         Map<string,Process.Scopes.LocEntry<Common.STLConstants.Scope>>)
    val validateLocalisationCommand :
      ((string * Common.STLConstants.Scope list) list -> string list ->
         string list -> string list ->
         Process.Scopes.LocEntry<Common.STLConstants.Scope> ->
         Process.Scopes.ScopeContext<Common.STLConstants.Scope> ->
         ValidationCore.ValidationResult)
    val checkFileEncoding : file:string -> ValidationCore.ValidationResult
    val checkLocFileName : file:string -> ValidationCore.ValidationResult
    val validateLocalisationFiles :
      locFiles:string list -> ValidationCore.ValidationResult
  end

namespace CWTools.Validation.Stellaris
  module STLLocalisationValidation = begin
    type S = Common.Severity
    type LocalisationValidator =
      ValidationCore.LocalisationValidator<Games.Stellaris.STLLookup.STLComputedData>
    val inline checkLocKey :
      leaf:#Process.IKeyPos ->
        keys:Set<string> ->
          lang:Common.Lang -> key:string -> ValidationCore.ValidationResult
    val inline checkLocKeyN :
      leaf:#Process.IKeyPos ->
        keys:Set<string> ->
          lang:Common.Lang ->
            errors:ValidationCore.ValidationResult ->
              key:string -> ValidationCore.ValidationResult
    val inline checkLocKeyNE :
      keys:Set<string> -> lang:Common.Lang -> key:string -> bool
    val inline checkLocName :
      leaf:#Process.IKeyPos ->
        keys:Set<string> ->
          lang:Common.Lang -> key:string -> ValidationCore.ValidationResult
    val inline checkLocNameN :
      leaf:#Process.IKeyPos ->
        keys:Set<string> ->
          lang:Common.Lang ->
            key:string ->
              errors:ValidationCore.ValidationResult ->
                ValidationCore.ValidationResult
    val inline checkLocNameNE :
      keys:Set<string> -> lang:Common.Lang -> key:string -> bool
    val inline checkLocKeysLeafOrNode :
      keys:(Common.Lang * Set<string>) list ->
        key:string ->
          leafornode:#Process.IKeyPos -> ValidationCore.ValidationResult
    val inline checkLocKeysLeafOrNodeN :
      keys:(Common.Lang * Set<string>) list ->
        key:string ->
          leafornode:#Process.IKeyPos ->
            errors:ValidationCore.ValidationResult ->
              ValidationCore.ValidationResult
    val inline checkLocKeysLeafOrNodeNE :
      keys:(Common.Lang * Set<string>) list -> key:string -> bool
    val checkLocKeys :
      keys:(Common.Lang * Set<string>) list ->
        leaf:Process.Leaf -> ValidationCore.ValidationResult
    val getLocKeys :
      keys:(Common.Lang * Set<string>) list ->
        tags:string list -> node:Process.Node -> ValidationCore.ValidationResult
    val valEventNameLocs :
      keys:(Common.Lang * Set<string>) list ->
        node:Process.Node -> ValidationCore.ValidationResult
    val checkLocLeafValue :
      keys:Set<string> ->
        lang:Common.Lang ->
          key:string -> lv:Process.LeafValue -> ValidationCore.ValidationResult
    val checkLocNode :
      keys:Set<string> ->
        lang:Common.Lang ->
          key:string -> node:Process.Node -> ValidationCore.ValidationResult
    val checkLocLeafValueS :
      keys:(Common.Lang * Set<string>) list ->
        key:string -> lv:Process.LeafValue -> ValidationCore.ValidationResult
    val checkLocNodeS :
      keys:(Common.Lang * Set<string>) list ->
        key:string -> node:Process.Node -> ValidationCore.ValidationResult
    val checkKeyAndDesc :
      keys:(Common.Lang * Set<string>) list ->
        node:Process.Node -> ValidationCore.ValidationResult
    val checkLocLeafValueKeyAdv :
      keys:(Common.Lang * Set<string>) list ->
        prefix:string ->
          suffix:string ->
            lv:Process.LeafValue -> ValidationCore.ValidationResult
    val checkLocNodeKeyAdv :
      keys:(Common.Lang * Set<string>) list ->
        prefix:string ->
          suffix:string -> node:Process.Node -> ValidationCore.ValidationResult
    val checkLocLeafValueKeyAdvs :
      keys:(Common.Lang * Set<string>) list ->
        prefix:string ->
          suffixes:string list ->
            lv:Process.LeafValue -> ValidationCore.ValidationResult
    val checkLocNodeKeyAdvs :
      keys:(Common.Lang * Set<string>) list ->
        prefix:string ->
          suffixes:string list ->
            node:Process.Node -> ValidationCore.ValidationResult
    val checkLocNodeTagAdv :
      keys:(Common.Lang * Set<string>) list ->
        prefix:string ->
          suffix:string ->
            tag:string -> node:Process.Node -> ValidationCore.ValidationResult
    val checkLocNodeTagAdvs :
      keys:(Common.Lang * Set<string>) list ->
        prefix:string ->
          suffixes:string list ->
            tag:string -> node:Process.Node -> ValidationCore.ValidationResult
    val checkLocNodeTag :
      keys:(Common.Lang * Set<string>) list ->
        tag:string -> node:Process.Node -> ValidationCore.ValidationResult
    val valEventLocs :
      ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
            ValidationCore.ValidationResult
    val valEffectLocs :
      ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
            ValidationCore.ValidationResult
    val valTriggerLocs :
      ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
            ValidationCore.ValidationResult
    val valTechLocs :
      ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
            ValidationCore.ValidationResult
    val valCompSetLocs :
      ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
            ValidationCore.ValidationResult
    val valCompTempLocs :
      ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
            ValidationCore.ValidationResult
    val valTraditionLocs :
      node:Process.Node ->
        keys:(Common.Lang * Set<string>) list ->
          starts:string list ->
            finals:string list ->
              traditions:string list -> ValidationCore.ValidationResult
    val processTradCat :
      keys:(Common.Lang * Set<string>) list ->
        cat:Process.Node -> string * string * string list
    val valTraditionLocCats :
      STLEntitySet:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          nes:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
            ValidationCore.ValidationResult
    val valPolicies :
      ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
            ValidationCore.ValidationResult
  end

namespace CWTools.Validation.Stellaris
  module STLEventValidation = begin
    type 'a Node = 'a * 'a list
    type 'a Edge = 'a * 'a
    type 'a Graph = 'a list * 'a Edge list
    type 'a AdjacencyGraph = 'a Node list
    type Vertex<'a> =
      {string: 'a;}
    val connectedComponents : 'a list * 'a Edge list -> 'a list list
    val findAllReferencedEvents :
      projects:(Process.Node * string) list -> event:Process.Node -> string list
    val findAllUsedEventTargets : event:Process.Node -> Set<string>
    val findAllSavedEventTargets : event:Process.Node -> Set<string>
    val findAllExistsEventTargets : event:Process.Node -> Set<string>
    val findAllSavedGlobalEventTargets : event:Process.Node -> Set<string>
    val addScriptedEffectTargets :
      effects:Common.STLConstants.ScriptedEffect list ->
        (string * Process.Node) * Set<string> * Set<string> * string list *
        Set<string> ->
          (string * Process.Node) * Set<string> * Set<string> * string list *
          Set<string>
    val addSystemInitializer :
      sinits:Process.Node list ->
        (string * Process.Node) * Set<string> * Set<string> * string list *
        Set<string> ->
          (string * Process.Node) * Set<string> * Set<string> * string list *
          Set<string>
    val checkEventChain :
      effects:Common.STLConstants.ScriptedEffect list ->
        sinits:Process.Node list ->
          projects:(Process.Node * string) list ->
            globals:Set<string> ->
              events:Process.Node list -> ValidationCore.ValidationResult
    val getEventChains :
      lu:Games.Lookup<Common.STLConstants.Scope,#Common.IModifier> ->
        os:ValidationCore.EntitySet<#Games.ComputedData> ->
          es:ValidationCore.EntitySet<#Games.ComputedData> ->
            ValidationCore.ValidationResult
    val valEventCalls :
      os:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
  end

namespace CWTools.Validation.Stellaris
  module Graphics = begin
    val valMeshFiles :
      rm:Games.IResourceAPI<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val valAssetFiles :
      rm:Games.IResourceAPI<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val inline validateEntityCulture :
      entities:Set<string> ->
        cultures:(string * string list) list ->
          entity:string ->
            leaf:Process.IKeyPos ->
              culture:string -> ValidationCore.ValidationResult option
    val inline validateEntityCultures :
      entities:Set<string> ->
        allcultures:(string * string list) list ->
          entity:string ->
            leaf:Process.IKeyPos ->
              cultures:string list -> ValidationCore.ValidationResult
    val inline validateEntity :
      entities:Set<string> ->
        entity:string -> node:Process.IKeyPos -> ValidationCore.ValidationResult
    val getGraphicalCultures :
      es:ValidationCore.STLEntitySet -> (string * string) list
    val valSectionGraphics :
      os:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val valComponentGraphics :
      os:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val valMegastructureGraphics :
      os:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val valPlanetClassGraphics :
      os:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val validateAmbientGraphics :
      os:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
    val valIconLeaf :
      sprites:Set<string> ->
        leaf:Process.Leaf -> ValidationCore.ValidationResult
    val valIcon :
      sprites:Set<string> ->
        key:string -> node:Process.Node -> ValidationCore.ValidationResult
    type gfxFolders = | Buildings
    val doesFileExist :
      files:Set<string> -> folder:gfxFolders -> file:string -> bool
    val valIconWithInterface :
      sprites:Set<string> ->
        files:Set<string> ->
          folder:gfxFolders ->
            key:string -> node:Process.Node -> ValidationCore.ValidationResult
    val valComponentIcons :
      res:Games.IResourceAPI<Games.Stellaris.STLLookup.STLComputedData> ->
        es:ValidationCore.EntitySet<Games.Stellaris.STLLookup.STLComputedData> ->
          ValidationCore.ValidationResult
  end

namespace CWTools.Validation
  type RuleContext<'T when 'T :> Common.IScope<'T>> =
    {subtypes: string list;
     scopes: Process.Scopes.ScopeContext<'T>;
     warningOnly: bool;}
  module Rules = begin
    val fst3 : a:'a * 'b * 'c -> 'a
    val snd3 : 'a * b:'b * 'c -> 'b
    val thd3 : 'a * 'b * c:'c -> 'c
    val checkPathDir :
      t:Rules.TypeDefinition<'a> -> pathDir:string -> file:string -> bool
    val getValidValues : _arg1:Rules.ValueType -> string list option
    val checkFileExists :
      files:Set<string> -> leaf:Process.Leaf -> ValidationCore.ValidationResult
    val checkIconExists :
      files:Set<string> ->
        folder:string -> leaf:Process.Leaf -> ValidationCore.ValidationResult
    val firstCharEqualsAmp : s:string -> bool
    val quoteArray : char []
    val ampArray : char []
    val trimQuote : s:string -> string
    val checkValidValue :
      enumsMap:Map<string,
                   (string *
                    Tagged.Set<string,
                               #System.Collections.Generic.IComparer<string>>)> ->
        severity:Common.Severity ->
          vt:Rules.ValueType ->
            id:Utilities.StringToken ->
              key:string ->
                leafornode:Process.IKeyPos ->
                  errors:ValidationCore.ValidationResult ->
                    ValidationCore.ValidationResult
    val checkValidValueNE :
      enumsMap:Map<string,
                   (string *
                    Tagged.Set<string,
                               #System.Collections.Generic.IComparer<string>>)> ->
        severity:Common.Severity ->
          vt:Rules.ValueType -> id:Utilities.StringToken -> key:string -> bool
    val checkLocalisationField :
      keys:(Common.Lang * Set<string>) list ->
        defaultLang:Common.Lang ->
          synced:bool ->
            key:string ->
              leafornode:Process.IKeyPos ->
                errors:ValidationCore.ValidationResult ->
                  ValidationCore.ValidationResult
    val checkLocalisationFieldNE :
      keys:(Common.Lang * Set<string>) list ->
        defaultLang:Common.Lang -> synced:bool -> key:string -> bool
    val memoize :
      keyFunction:('a -> 'b) -> memFunction:('a -> 'c) -> ('a -> 'c)
        when 'b : equality
    val checkTypeField :
      typesMap:Map<string,Process.Scopes.StringSet> ->
        severity:Common.Severity ->
          typetype:Rules.TypeType ->
            key:string ->
              leafornode:Process.IKeyPos ->
                errors:ValidationCore.ValidationResult ->
                  ValidationCore.ValidationResult
    val checkTypeFieldNE :
      typesMap:Map<string,Process.Scopes.StringSet> ->
        severity:'a -> typetype:Rules.TypeType -> key:string -> bool
    val checkVariableGetField :
      varMap:Map<string,Process.Scopes.StringSet> ->
        severity:Common.Severity ->
          varName:string ->
            key:string ->
              leafornode:Process.IKeyPos ->
                errors:ValidationCore.ValidationResult ->
                  ValidationCore.ValidationResult
    val checkVariableGetFieldNE :
      varMap:Map<string,Process.Scopes.StringSet> ->
        severity:'a -> varName:string -> key:string -> bool
    val checkFilepathField :
      files:Set<string> ->
        key:string ->
          leafornode:Process.IKeyPos ->
            errors:ValidationCore.ValidationResult ->
              ValidationCore.ValidationResult
    val checkFilepathFieldNE : files:Set<string> -> key:string -> bool
    val checkIconField :
      files:Set<string> ->
        folder:string ->
          key:string ->
            leafornode:Process.IKeyPos ->
              errors:ValidationCore.ValidationResult ->
                ValidationCore.ValidationResult
    val checkIconFieldNE :
      files:Set<string> -> folder:string -> key:string -> bool
    val checkScopeField :
      linkMap:Tagged.Map<'a,'b,'c> ->
        valueTriggerMap:Tagged.Map<'d,'e,'f> ->
          wildcardLinks:Common.ScopedEffect<'g> list ->
            varSet:'h ->
              changeScope:(bool -> bool -> Tagged.Map<'a,'b,'c> ->
                             Tagged.Map<'d,'e,'f> ->
                             Common.ScopedEffect<'g> list -> 'h -> string ->
                             Process.Scopes.ScopeContext<'i> ->
                             Process.Scopes.ScopeResult<'j>) ->
                anyScope:'j ->
                  ctx:RuleContext<'i> ->
                    s:'j ->
                      key:string ->
                        leafornode:Process.IKeyPos ->
                          errors:ValidationCore.ValidationResult ->
                            ValidationCore.ValidationResult
        when 'c :> System.Collections.Generic.IComparer<'a> and
             'f :> System.Collections.Generic.IComparer<'d> and 'g : comparison and
             'i :> Common.IScope<'i> and 'j :> Common.IScope<'j> and
             'j : equality
    val checkScopeFieldNE :
      linkMap:Tagged.Map<'a,'b,'c> ->
        valueTriggerMap:Tagged.Map<'d,'e,'f> ->
          wildcardLinks:Common.ScopedEffect<'g> list ->
            varSet:'h ->
              changeScope:(bool -> bool -> Tagged.Map<'a,'b,'c> ->
                             Tagged.Map<'d,'e,'f> ->
                             Common.ScopedEffect<'g> list -> 'h -> 'i ->
                             Process.Scopes.ScopeContext<'j> ->
                             Process.Scopes.ScopeResult<'k>) ->
                anyScope:'k -> ctx:RuleContext<'j> -> s:'k -> key:'i -> bool
        when 'c :> System.Collections.Generic.IComparer<'a> and
             'f :> System.Collections.Generic.IComparer<'d> and 'g : comparison and
             'j :> Common.IScope<'j> and 'k :> Common.IScope<'k> and
             'k : equality
    val checkVariableField :
      linkMap:Tagged.Map<'a,'b,'c> ->
        valueTriggerMap:Tagged.Map<'d,'e,'f> ->
          wildcardLinks:Common.ScopedEffect<'g> list ->
            varSet:'h ->
              changeScope:(bool -> bool -> Tagged.Map<'a,'b,'c> ->
                             Tagged.Map<'d,'e,'f> ->
                             Common.ScopedEffect<'g> list -> 'h -> string ->
                             Process.Scopes.ScopeContext<'i> ->
                             Process.Scopes.ScopeResult<'j>) ->
                anyScope:'k ->
                  ctx:RuleContext<'i> ->
                    isInt:bool ->
                      min:float ->
                        max:float ->
                          key:string ->
                            leafornode:Process.IKeyPos ->
                              errors:ValidationCore.ValidationResult ->
                                ValidationCore.ValidationResult
        when 'c :> System.Collections.Generic.IComparer<'a> and
             'f :> System.Collections.Generic.IComparer<'d> and 'g : comparison and
             'i :> Common.IScope<'i> and 'j :> Common.IScope<'j>
    val checkVariableFieldNE :
      linkMap:Tagged.Map<'a,'b,'c> ->
        valueTriggerMap:Tagged.Map<'d,'e,'f> ->
          wildcardLinks:Common.ScopedEffect<'g> list ->
            varSet:'h ->
              changeScope:(bool -> bool -> Tagged.Map<'a,'b,'c> ->
                             Tagged.Map<'d,'e,'f> ->
                             Common.ScopedEffect<'g> list -> 'h -> string ->
                             Process.Scopes.ScopeContext<'i> ->
                             Process.Scopes.ScopeResult<'j>) ->
                anyScope:'k ->
                  ctx:RuleContext<'i> ->
                    isInt:bool -> min:float -> max:float -> key:string -> bool
        when 'c :> System.Collections.Generic.IComparer<'a> and
             'f :> System.Collections.Generic.IComparer<'d> and 'g : comparison and
             'i :> Common.IScope<'i> and 'j :> Common.IScope<'j>
    val checkValueScopeField :
      enumsMap:Map<string,
                   (string *
                    Tagged.Set<string,
                               #System.Collections.Generic.IComparer<string>>)> ->
        linkMap:Tagged.Map<'b,'c,'d> ->
          valueTriggerMap:Tagged.Map<'e,'f,'g> ->
            wildcardLinks:Common.ScopedEffect<'h> list ->
              varSet:'i ->
                changeScope:(bool -> bool -> Tagged.Map<'b,'c,'d> ->
                               Tagged.Map<'e,'f,'g> ->
                               Common.ScopedEffect<'h> list -> 'i -> string ->
                               Process.Scopes.ScopeContext<'j> ->
                               Process.Scopes.ScopeResult<'k>) ->
                  anyScope:'l ->
                    ctx:RuleContext<'j> ->
                      isInt:bool ->
                        min:float ->
                          max:float ->
                            key:string ->
                              leafornode:Process.IKeyPos ->
                                errors:ValidationCore.ValidationResult ->
                                  ValidationCore.ValidationResult
        when 'd :> System.Collections.Generic.IComparer<'b> and
             'g :> System.Collections.Generic.IComparer<'e> and 'h : comparison and
             'j :> Common.IScope<'j> and 'k :> Common.IScope<'k>
    val checkValueScopeFieldNE :
      enumsMap:Map<string,
                   (string *
                    Tagged.Set<string,
                               #System.Collections.Generic.IComparer<string>>)> ->
        linkMap:Tagged.Map<'b,'c,'d> ->
          valueTriggerMap:Tagged.Map<'e,'f,'g> ->
            wildcardLinks:Common.ScopedEffect<'h> list ->
              varSet:'i ->
                changeScope:(bool -> bool -> Tagged.Map<'b,'c,'d> ->
                               Tagged.Map<'e,'f,'g> ->
                               Common.ScopedEffect<'h> list -> 'i -> string ->
                               Process.Scopes.ScopeContext<'j> ->
                               Process.Scopes.ScopeResult<'k>) ->
                  anyScope:'l ->
                    ctx:RuleContext<'j> ->
                      isInt:bool -> min:float -> max:float -> key:string -> bool
        when 'd :> System.Collections.Generic.IComparer<'b> and
             'g :> System.Collections.Generic.IComparer<'e> and 'h : comparison and
             'j :> Common.IScope<'j> and 'k :> Common.IScope<'k>
    type checkFieldParams<'S when 'S :> Common.IScope<'S> and 'S : comparison> =
      {varMap: Map<string,Process.Scopes.StringSet>;
       enumsMap: Map<string,(string * Process.Scopes.StringSet)>;
       typesMap: Map<string,Process.Scopes.StringSet>;
       linkMap:
         Tagged.Map<string,Common.Effect<'S>,
                    Utilities.Utils.InsensitiveStringComparer>;
       wildcardLinks: Common.ScopedEffect<'S> list;
       valueTriggerMap:
         Tagged.Map<string,Common.Effect<'S>,
                    Utilities.Utils.InsensitiveStringComparer>;
       varSet: Process.Scopes.StringSet;
       localisation: (Common.Lang * Set<string>) list;
       files: Set<string>;
       changeScope: Process.Scopes.ChangeScope<'S>;
       anyScope: 'S;
       defaultLang: Common.Lang;
       severity: Common.Severity;
       ctx: RuleContext<'S>;}
    val checkField :
      p:checkFieldParams<'a> ->
        field:Rules.NewField<'a> ->
          id:Utilities.StringToken ->
            key:string ->
              leafornode:Process.IKeyPos ->
                errors:ValidationCore.ValidationResult ->
                  ValidationCore.ValidationResult
        when 'a :> Common.IScope<'a> and 'a : comparison
    val checkFieldNE :
      p:checkFieldParams<'a> ->
        field:Rules.NewField<'a> ->
          id:Utilities.StringToken -> key:string -> bool
        when 'a :> Common.IScope<'a> and 'a : comparison
    val checkLeftField :
      p:checkFieldParams<'a> ->
        field:Rules.NewField<'a> ->
          id:Utilities.StringToken -> key:string -> bool
        when 'a :> Common.IScope<'a> and 'a : comparison
    val checkFieldByKey :
      p:checkFieldParams<'a> ->
        field:Rules.NewField<'a> ->
          id:Utilities.StringToken -> key:string -> bool
        when 'a :> Common.IScope<'a> and 'a : comparison
    val inline validateTypeLocalisation :
      typedefs:Rules.TypeDefinition<'a> list ->
        invertedTypeMap:Map<string,string list> ->
          localisation:(Common.Lang * Set<string>) list ->
            typeKey:string ->
              key:string ->
                leafornode:Process.IKeyPos -> ValidationCore.ValidationResult
    val typekeyfilter : td:Rules.TypeDefinition<'a> -> n:string -> bool
    type RuleApplicator<'T when 'T :> Common.IScope<'T> and 'T : comparison> =
      class
        new : rootRules:Rules.RootRule<'T> list *
              typedefs:Rules.TypeDefinition<'T> list *
              types:Map<string,Process.Scopes.StringSet> *
              enums:Map<string,(string * Process.Scopes.StringSet)> *
              varMap:Map<string,Process.Scopes.StringSet> *
              localisation:(Common.Lang * Set<string>) list * files:Set<string> *
              links:Tagged.Map<string,Common.Effect<'T>,
                               Utilities.Utils.InsensitiveStringComparer> *
              valueTriggers:Tagged.Map<string,Common.Effect<'T>,
                                       Utilities.Utils.InsensitiveStringComparer> *
              anyScope:'T * changeScope:Process.Scopes.ChangeScope<'T> *
              defaultContext:Process.Scopes.ScopeContext<'T> *
              defaultLang:Common.Lang -> RuleApplicator<'T>
        member
          ApplyNodeRule : rule:Rules.NewRule<'T> list * node:Process.Node ->
                            ValidationCore.ValidationResult
        member
          RuleValidate : unit ->
                           ('a ->
                              ValidationCore.EntitySet<#Games.ComputedData> ->
                              ValidationCore.ValidationResult)
        member
          TestSubType : subtypes:Rules.SubTypeDefinition<'T> list *
                        node:Process.Node -> 'T option * string list
        member
          RuleValidateEntity : (Games.Entity -> ValidationCore.ValidationResult)
      end
    type FoldRules<'T when 'T :> Common.IScope<'T> and 'T : comparison> =
      class
        new : rootRules:Rules.RootRule<'T> list *
              typedefs:Rules.TypeDefinition<'T> list *
              types:Map<string,Process.Scopes.StringSet> *
              enums:Map<string,(string * Process.Scopes.StringSet)> *
              varMap:Map<string,Process.Scopes.StringSet> *
              localisation:(Common.Lang * Set<string>) list * files:Set<string> *
              links:Tagged.Map<string,Common.Effect<'T>,
                               Utilities.Utils.InsensitiveStringComparer> *
              valueTriggers:Tagged.Map<string,Common.Effect<'T>,
                                       Utilities.Utils.InsensitiveStringComparer> *
              ruleApplicator:RuleApplicator<'T> *
              changeScope:Process.Scopes.ChangeScope<'T> *
              defaultContext:Process.Scopes.ScopeContext<'T> * anyScope:'T *
              defaultLang:Common.Lang -> FoldRules<'T>
        member
          BatchFolds : entity:Games.Entity ->
                         Map<string,(string * Utilities.Position.range) list> *
                         Map<string,(string * Utilities.Position.range) list> *
                         (Process.Node list * bool) * (Process.Node list * bool)
        member
          GetDefinedVariables : entity:Games.Entity ->
                                  Map<string,
                                      (string * Utilities.Position.range) list>
        member
          GetEffectBlocks : entity:Games.Entity ->
                              (Process.Node list * bool) *
                              (Process.Node list * bool)
        member
          GetInfo : pos:Utilities.Position.pos * entity:Games.Entity ->
                      (Process.Scopes.ScopeContext<'T> *
                       (Rules.Options<'T> option * (string * string) option *
                        Process.Child option)) option
        member
          GetReferencedTypes : entity:Games.Entity ->
                                 Map<string,
                                     (string * Utilities.Position.range) list>
        member
          GetTypeLocalisationErrors : entity:Games.Entity ->
                                        ValidationCore.ValidationResult
      end
    type CompletionService<'T when 'T :> Common.IScope<'T> and 'T : comparison> =
      class
        new : rootRules:Rules.RootRule<'T> list *
              typedefs:Rules.TypeDefinition<'T> list *
              types:Map<string,Process.Scopes.StringSet> *
              enums:Map<string,(string * Process.Scopes.StringSet)> *
              varMap:Map<string,Process.Scopes.StringSet> *
              localisation:(Common.Lang * Set<string>) list * files:Set<string> *
              links:Tagged.Map<string,Common.Effect<'T>,
                               Utilities.Utils.InsensitiveStringComparer> *
              valueTriggers:Tagged.Map<string,Common.Effect<'T>,
                                       Utilities.Utils.InsensitiveStringComparer> *
              globalScriptVariables:string list *
              changeScope:Process.Scopes.ChangeScope<'T> *
              defaultContext:Process.Scopes.ScopeContext<'T> * anyScope:'T *
              oneToOneScopes:obj * defaultLang:Common.Lang ->
                CompletionService<'T>
        member
          Complete : pos:Utilities.Position.pos * entity:Games.Entity *
                     scopeContext:Process.Scopes.ScopeContext<'T> option ->
                       Games.CompletionResponse list
      end
    val getTypesFromDefinitions :
      ruleapplicator:RuleApplicator<'a> ->
        types:Rules.TypeDefinition<'a> list ->
          es:Games.Entity list ->
            Map<string,(bool * string * Utilities.Position.range) list>
        when 'a :> Common.IScope<'a> and 'a : comparison
    val getEnumsFromComplexEnums :
      complexenums:Rules.ComplexEnumDef list ->
        es:Games.Entity list -> Rules.EnumDefinition list
    val getDefinedVariables :
      foldRules:FoldRules<'a> ->
        es:Games.Entity list ->
          Map<string,(string * Utilities.Position.range) list>
        when 'a :> Common.IScope<'a> and 'a : comparison
  end

namespace CWTools.Validation.Stellaris
  module STLRules = begin
    type NewField = Rules.NewField<Common.STLConstants.Scope>
    type NewRule = Rules.NewRule<Common.STLConstants.Scope>
    type Options = Rules.Options<Common.STLConstants.Scope>
    type TypeDefinition = Rules.TypeDefinition<Common.STLConstants.Scope>
    type RootRule = Rules.RootRule<Common.STLConstants.Scope>
    type SubTypeDefinition = Rules.SubTypeDefinition<Common.STLConstants.Scope>
    val scopes : string list
    val inline checkLocalisationField :
      keys:(Common.Lang * Set<string>) list ->
        synced:bool ->
          key:string ->
            leafornode:#Process.IKeyPos -> ValidationCore.ValidationResult
  end

namespace CWTools.Games
  type RulesSettings =
    {ruleFiles: (string * string) list;
     validateRules: bool;
     debugRulesOnly: bool;
     debugMode: bool;}
  type EmbeddedSettings<'S,'M when 'S : comparison> =
    {triggers: Common.DocEffect<'S> list;
     effects: Common.DocEffect<'S> list;
     embeddedFiles: (string * string) list;
     modifiers: 'M list;
     cachedResourceData: (Resource * Entity) list;
     localisationCommands: (string * 'S list) list;
     eventTargetLinks: Common.EventTargetLink<'S> list;}
  type RuleManagerSettings<'S,'M,'T
                             when 'S :> Common.IScope<'S> and 'S : comparison and
                                  'M :> Common.IModifier and 'T :> ComputedData> =
    {rulesSettings: RulesSettings option;
     parseScope: string -> 'S;
     allScopes: 'S list;
     anyScope: 'S;
     changeScope: Process.Scopes.ChangeScope<'S>;
     defaultContext: Process.Scopes.ScopeContext<'S>;
     defaultLang: Common.Lang;
     oneToOneScopesNames: string list;
     loadConfigRulesHook:
       Rules.RootRule<'S> list -> Lookup<'S,'M> -> EmbeddedSettings<'S,'M> ->
         Rules.RootRule<'S> list;
     refreshConfigBeforeFirstTypesHook:
       Lookup<'S,'M> -> IResourceAPI<'T> -> EmbeddedSettings<'S,'M> -> unit;
     refreshConfigAfterFirstTypesHook:
       Lookup<'S,'M> -> IResourceAPI<'T> -> EmbeddedSettings<'S,'M> -> unit;
     refreshConfigAfterVarDefHook:
       Lookup<'S,'M> -> IResourceAPI<'T> -> EmbeddedSettings<'S,'M> -> unit;}
  type RulesManager<'T,'S,'M
                      when 'T :> ComputedData and 'S :> Common.IScope<'S> and
                           'S : comparison and 'M :> Common.IModifier> =
    class
      new : resources:IResourceAPI<'T> * lookup:Lookup<'S,'M> *
            settings:RuleManagerSettings<'S,'M,'T> *
            localisation:LocalisationManager<'S,'T,'M> *
            embeddedSettings:EmbeddedSettings<'S,'M> -> RulesManager<'T,'S,'M>
      member LoadBaseConfig : rulesSettings:RulesSettings -> unit
      member
        RefreshConfig : unit ->
                          Validation.Rules.RuleApplicator<'S> *
                          Validation.Rules.FoldRules<'S> *
                          Validation.Rules.CompletionService<'S>
    end

namespace CWTools.Games
  module LanguageFeatures = begin
    val makeEntityResourceInput :
      fileManager:Files.FileManager ->
        filepath:string -> filetext:string -> ResourceInput
    val makeFileWithContentResourceInput :
      fileManager:Files.FileManager ->
        filepath:string -> filetext:string -> ResourceInput
    val completion :
      fileManager:Files.FileManager ->
        completionService:Validation.Rules.CompletionService<'a> option ->
          infoService:Validation.Rules.FoldRules<'a> option ->
            resourceManager:ResourceManager<#ComputedData> ->
              pos:Utilities.Position.pos ->
                filepath:string -> filetext:string -> CompletionResponse list
        when 'a :> Common.IScope<'a> and 'a : comparison
    val getInfoAtPos :
      fileManager:Files.FileManager ->
        resourceManager:ResourceManager<#ComputedData> ->
          infoService:Validation.Rules.FoldRules<'b> option ->
            lookup:Lookup<'c,#Common.IModifier> ->
              pos:Utilities.Position.pos ->
                filepath:string ->
                  filetext:string -> Utilities.Position.range option
        when 'b :> Common.IScope<'b> and 'b : comparison and 'c : comparison and
             'c :> Common.IScope<'c>
    val findAllRefsFromPos :
      fileManager:Files.FileManager ->
        resourceManager:ResourceManager<#ComputedData> ->
          infoService:Validation.Rules.FoldRules<'b> option ->
            pos:Utilities.Position.pos ->
              filepath:string ->
                filetext:string -> Utilities.Position.range list option
        when 'b :> Common.IScope<'b> and 'b : comparison
    val scopesAtPos :
      fileManager:Files.FileManager ->
        resourceManager:ResourceManager<#ComputedData> ->
          infoService:Validation.Rules.FoldRules<'a0> option ->
            anyScope:'a0 ->
              pos:Utilities.Position.pos ->
                filepath:string ->
                  filetext:string -> Process.Scopes.ScopeContext<'a0> option
        when 'a0 :> Common.IScope<'a0> and 'a0 : comparison
    val symbolInformationAtPos :
      fileManager:Files.FileManager ->
        resourceManager:ResourceManager<#ComputedData> ->
          infoService:Validation.Rules.FoldRules<'b> option ->
            lookup:Lookup<'c,#Common.IModifier> ->
              pos:Utilities.Position.pos ->
                filepath:string -> filetext:string -> SymbolInformation option
        when 'b :> Common.IScope<'b> and 'b : comparison and 'c : comparison and
             'c :> Common.IScope<'c>
  end

namespace CWTools.Validation.EU4
  module EU4Rules = begin
    type NewField = Rules.NewField<Common.EU4Constants.Scope>
    type NewRule = Rules.NewRule<Common.EU4Constants.Scope>
    type Options = Rules.Options<Common.EU4Constants.Scope>
    type TypeDefinition = Rules.TypeDefinition<Common.EU4Constants.Scope>
    type RootRule = Rules.RootRule<Common.EU4Constants.Scope>
    type SubTypeDefinition = Rules.SubTypeDefinition<Common.EU4Constants.Scope>
    val scopes : string list
    val inline checkLocalisationField :
      keys:(Common.Lang * Set<string>) list ->
        synced:bool ->
          key:string ->
            leafornode:#Process.IKeyPos -> ValidationCore.ValidationResult
  end

namespace CWTools.Validation.EU4
  module EU4LocalisationString = begin
    val hardcodedLocalisation : string list
    val commands : string list
    val locCommands : (string * Common.EU4Constants.Scope list) list
    val validateProcessedLocalisation :
      ((Common.Lang * Utilities.Utils.LocKeySet) list ->
         (Common.Lang *
          Map<string,Process.Scopes.LocEntry<Common.EU4Constants.Scope>>) list ->
         ValidationCore.ValidationResult)
    val processLocalisation :
      ((string * Common.EU4Constants.Scope list) list -> string list ->
         string list -> string list ->
         Common.Lang * Map<string,Localisation.Entry> ->
         Common.Lang *
         Map<string,Process.Scopes.LocEntry<Common.EU4Constants.Scope>>)
    val validateLocalisationCommand :
      ((string * Common.EU4Constants.Scope list) list -> string list ->
         string list -> string list ->
         Process.Scopes.LocEntry<Common.EU4Constants.Scope> ->
         Process.Scopes.ScopeContext<Common.EU4Constants.Scope> ->
         ValidationCore.ValidationResult)
    val checkFileEncoding : file:string -> ValidationCore.ValidationResult
    val checkLocFileName : file:string -> ValidationCore.ValidationResult
    val validateLocalisationFiles :
      locFiles:string list -> ValidationCore.ValidationResult
  end

namespace CWTools.Validation.EU4
  module EU4LocalisationValidation = begin
    type LocalisationValidator =
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        (Common.Lang * Set<string>) list ->
        ValidationCore.EntitySet<Games.EU4ComputedData> ->
        ValidationCore.ValidationResult
    val valOpinionModifierLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
    val valStaticModifierLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
    val valTimedModifierLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
    val valEventModifierLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
    val valTriggeredModifierLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
    val valProvinceTriggeredModifierLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
    val valUnitTypeLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
    val valAdvisorTypeLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
    val valTradeGoodLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
    val valTradeCompanyLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
    val valTradeCompanyInvestmentLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
    val valTradeNodeLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
    val valTradingPolicyLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
    val valTradeCenterLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
    val valCasusBelliLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
    val valWarGoalLocs :
      ValidationCore.EntitySet<Games.EU4ComputedData> ->
        keys:(Common.Lang * Set<string>) list ->
          es:ValidationCore.EntitySet<Games.EU4ComputedData> ->
            ValidationCore.ValidationResult
  end

namespace CWTools.Validation.EU4
  module EU4Validation = begin
    type S = Common.Severity
    val addGeneratedModifiers :
      modifiers:Common.EU4Constants.Modifier list ->
        es:ValidationCore.EU4EntitySet -> Common.EU4Constants.Modifier list
    val getScriptedEffectParams : node:Process.Node -> string list
    val getScriptedEffectParamsEntity : e:Games.Entity -> string list
  end

namespace CWTools.Validation.EU4
  module EU4Compute = begin
    val computeEU4Data :
      foldRules:(unit -> Rules.FoldRules<'a> option) ->
        e:Games.Entity -> Games.EU4ComputedData
        when 'a :> Common.IScope<'a> and 'a : comparison
    val computeEU4DataUpdate :
      foldRules:(unit -> Rules.FoldRules<'a> option) ->
        e:Games.Entity -> data:Games.EU4ComputedData -> unit
        when 'a :> Common.IScope<'a> and 'a : comparison
  end

namespace CWTools.Validation.HOI4
  module HOI4Compute = begin
    val computeHOI4Data :
      foldRules:(unit -> Rules.FoldRules<'a> option) ->
        e:Games.Entity -> Games.HOI4ComputedData
        when 'a :> Common.IScope<'a> and 'a : comparison
    val computeHOI4DataUpdate :
      foldRules:(unit -> Rules.FoldRules<'a> option) ->
        e:Games.Entity -> data:Games.HOI4ComputedData -> unit
        when 'a :> Common.IScope<'a> and 'a : comparison
  end

namespace CWTools.Validation.Stellaris
  module STLCompute = begin
    val getAllTechPrereqs : e:Games.Entity -> string list
    val setFlagMap :
      x:Process.Leaf -> (Games.Stellaris.STLLookup.FlagType * string) option
    val setTimedFlagMap :
      x:Process.Node -> (Games.Stellaris.STLLookup.FlagType * string) option
    val findAllSetFlags :
      e:Games.Entity -> (Games.Stellaris.STLLookup.FlagType * string) list
    val computeSTLData :
      foldRules:(unit -> Rules.FoldRules<Common.STLConstants.Scope> option) ->
        e:Games.Entity -> Games.Stellaris.STLLookup.STLComputedData
    val computeSTLDataUpdate :
      foldRules:(unit -> Rules.FoldRules<Common.STLConstants.Scope> option) ->
        e:Games.Entity -> data:Games.Stellaris.STLLookup.STLComputedData -> unit
  end

namespace CWTools.Validation.CK2
  module CK2LocalisationString = begin
    val hardcodedLocalisation : string list
    val commands : string list
    val locCommands : (string * Common.CK2Constants.Scope list) list
    val validateProcessedLocalisation :
      ((Common.Lang * Utilities.Utils.LocKeySet) list ->
         (Common.Lang *
          Map<string,Process.Scopes.LocEntry<Common.CK2Constants.Scope>>) list ->
         ValidationCore.ValidationResult)
    val processLocalisation :
      ((string * Common.CK2Constants.Scope list) list -> string list ->
         string list -> string list ->
         Common.Lang * Map<string,Localisation.Entry> ->
         Common.Lang *
         Map<string,Process.Scopes.LocEntry<Common.CK2Constants.Scope>>)
    val validateLocalisationCommand :
      ((string * Common.CK2Constants.Scope list) list -> string list ->
         string list -> string list ->
         Process.Scopes.LocEntry<Common.CK2Constants.Scope> ->
         Process.Scopes.ScopeContext<Common.CK2Constants.Scope> ->
         ValidationCore.ValidationResult)
  end

namespace CWTools.Validation.CK2
  module CK2Compute = begin
    val computeCK2Data :
      foldRules:(unit -> Rules.FoldRules<'a> option) ->
        e:Games.Entity -> Games.CK2ComputedData
        when 'a :> Common.IScope<'a> and 'a : comparison
    val computeCK2DataUpdate :
      foldRules:(unit -> Rules.FoldRules<'a> option) ->
        e:Games.Entity -> data:Games.CK2ComputedData -> unit
        when 'a :> Common.IScope<'a> and 'a : comparison
  end

namespace CWTools.Validation.IR
  module IRLocalisationString = begin
    val hardcodedLocalisation : string list
    val commands : string list
    val locCommands : (string * Common.IRConstants.Scope list) list
    val validateProcessedLocalisation :
      ((Common.Lang * Utilities.Utils.LocKeySet) list ->
         (Common.Lang *
          Map<string,Process.Scopes.LocEntry<Common.IRConstants.Scope>>) list ->
         ValidationCore.ValidationResult)
    val processLocalisation :
      ((string * Common.IRConstants.Scope list) list -> string list ->
         string list -> string list ->
         Common.Lang * Map<string,Localisation.Entry> ->
         Common.Lang *
         Map<string,Process.Scopes.LocEntry<Common.IRConstants.Scope>>)
    val validateLocalisationCommand :
      ((string * Common.IRConstants.Scope list) list -> string list ->
         string list -> string list ->
         Process.Scopes.LocEntry<Common.IRConstants.Scope> ->
         Process.Scopes.ScopeContext<Common.IRConstants.Scope> ->
         ValidationCore.ValidationResult)
  end

namespace CWTools.Validation.IR
  module IRCompute = begin
    val computeIRData :
      foldRules:(unit -> Rules.FoldRules<'a> option) ->
        e:Games.Entity -> Games.IRComputedData
        when 'a :> Common.IScope<'a> and 'a : comparison
    val computeIRDataUpdate :
      foldRules:(unit -> Rules.FoldRules<'a> option) ->
        e:Games.Entity -> data:Games.IRComputedData -> unit
        when 'a :> Common.IScope<'a> and 'a : comparison
  end

namespace CWTools.Validation.HOI4
  module HOI4LocalisationString = begin
    val hardcodedLocalisation : string list
    val commands : string list
    val locCommands : (string * Common.HOI4Constants.Scope list) list
    val validateProcessedLocalisation :
      ((Common.Lang * Utilities.Utils.LocKeySet) list ->
         (Common.Lang *
          Map<string,Process.Scopes.LocEntry<Common.HOI4Constants.Scope>>) list ->
         ValidationCore.ValidationResult)
    val processLocalisation :
      ((string * Common.HOI4Constants.Scope list) list -> string list ->
         string list -> string list ->
         Common.Lang * Map<string,Localisation.Entry> ->
         Common.Lang *
         Map<string,Process.Scopes.LocEntry<Common.HOI4Constants.Scope>>)
    val validateLocalisationCommand :
      ((string * Common.HOI4Constants.Scope list) list -> string list ->
         string list -> string list ->
         Process.Scopes.LocEntry<Common.HOI4Constants.Scope> ->
         Process.Scopes.ScopeContext<Common.HOI4Constants.Scope> ->
         ValidationCore.ValidationResult)
  end

namespace CWTools.Validation.HOI4
  module HOI4Validation = begin
    val findAllSetVariables : node:Process.Node -> string list
  end

namespace CWTools.Games
  type ValidationManagerSettings<'T,'S,'M
                                   when 'T :> ComputedData and
                                        'S :> Common.IScope<'S> and
                                        'S : comparison and
                                        'M :> Common.IModifier> =
    {validators:
       (Validation.ValidationCore.StructureValidator<'T> * string) list;
     experimentalValidators:
       (Validation.ValidationCore.StructureValidator<'T> * string) list;
     heavyExperimentalValidators:
       (Validation.ValidationCore.LookupValidator<'T,'S,'M> * string) list;
     experimental: bool;
     fileValidators: (Validation.ValidationCore.FileValidator<'T> * string) list;
     lookupValidators:
       (Validation.ValidationCore.LookupValidator<'T,'S,'M> * string) list;
     useRules: bool;
     debugRulesOnly: bool;
     localisationValidators:
       Validation.ValidationCore.LocalisationValidator<'T> list;}
  type ValidationManagerServices<'T,'S,'M
                                   when 'T :> ComputedData and
                                        'S :> Common.IScope<'S> and
                                        'S : comparison and
                                        'M :> Common.IModifier> =
    {resources: IResourceAPI<'T>;
     lookup: Lookup<'S,'M>;
     ruleApplicator: Validation.Rules.RuleApplicator<'S> option;
     foldRules: Validation.Rules.FoldRules<'S> option;
     localisationKeys: unit -> (Common.Lang * Set<string>) list;}
  type ValidationManager<'T,'S,'M
                           when 'T :> ComputedData and 'S :> Common.IScope<'S> and
                                'S : comparison and 'M :> Common.IModifier> =
    class
      new : settings:ValidationManagerSettings<'T,'S,'M> *
            services:ValidationManagerServices<'T,'S,'M> *
            validateLocalisationCommand:(Lookup<'S,'M> ->
                                           Process.Scopes.LocEntry<'S> ->
                                           Process.Scopes.ScopeContext<'S> ->
                                           Validation.ValidationCore.ValidationResult) *
            defaultContext:Process.Scopes.ScopeContext<'S> *
            noneContext:Process.Scopes.ScopeContext<'S> ->
              ValidationManager<'T,'S,'M>
      member CachedRuleErrors : entities:Entity list -> CWError list
      member
        Validate : shallow:bool * entities:struct (Entity * Lazy<'T>) list ->
                     CWError list * CWError list
      member
        ValidateGlobalLocalisation : unit ->
                                       Validation.ValidationCore.ValidationResult
      member
        ValidateLocalisation : entities:struct (Entity * Lazy<'T>) list ->
                                 CWError list
    end

namespace CWTools.Games
  module Helpers = begin
    val updateEventTargetLinks :
      embeddedSettings:EmbeddedSettings<'a,'b> -> Common.Effect<'a> list
        when 'a : comparison
    val private convertSourceRuleType :
      lookup:Lookup<'S,#Common.IModifier> ->
        link:Common.EventTargetDataLink<'a> -> string list
        when 'S : comparison and 'S :> Common.IScope<'S>
    val private getWildCard :
      link:Common.EventTargetDataLink<'a> -> Common.ScopedEffect<'a> option
        when 'a : comparison
    val addDataEventTargetLinks :
      lookup:Lookup<'S,#Common.IModifier> ->
        embeddedSettings:EmbeddedSettings<'a,'b> ->
          addWildCardLinks:bool -> Common.Effect<'a> list
        when 'S : comparison and 'S :> Common.IScope<'S> and 'a : comparison
  end

namespace CWTools.Games
  type ValidationSettings =
    {langs: Common.Lang list;
     validateVanilla: bool;
     experimental: bool;}
  type GameSettings<'M,'S when 'S : comparison> =
    {rootDirectory: string;
     embedded: EmbeddedSettings<'S,'M>;
     validation: ValidationSettings;
     rules: RulesSettings option;
     scriptFolders: string list option;
     excludeGlobPatterns: string list option;
     modFilter: string option;
     scope: Files.FilesScope;}
  type GameObject<'S,'M,'T
                    when 'S : comparison and 'S :> Common.IScope<'S> and
                         'M :> Common.IModifier and 'T :> ComputedData> =
    class
      new : settings:GameSettings<'M,'S> * game:string *
            scriptFolders:string list *
            computeFunction:((unit -> Validation.Rules.FoldRules<'S> option) ->
                               Entity -> 'T) *
            computeUpdateFunction:((unit ->
                                      Validation.Rules.FoldRules<'S> option) ->
                                     Entity -> 'T -> unit) *
            localisationService:((string * string) list ->
                                   Localisation.ILocalisationAPICreator) *
            processLocalisation:(Lookup<'S,'M> ->
                                   Common.Lang * Map<string,Localisation.Entry> ->
                                   Common.Lang *
                                   Map<string,Process.Scopes.LocEntry<'S>>) *
            validateLocalisationCommand:(Lookup<'S,'M> ->
                                           Process.Scopes.LocEntry<'S> ->
                                           Process.Scopes.ScopeContext<'S> ->
                                           Validation.ValidationCore.ValidationResult) *
            defaultContext:Process.Scopes.ScopeContext<'S> *
            noneContext:Process.Scopes.ScopeContext<'S> *
            encoding:System.Text.Encoding *
            fallbackencoding:System.Text.Encoding *
            validationSettings:ValidationManagerSettings<'T,'S,'M> *
            globalLocalisation:(GameObject<'S,'M,'T> -> CWError list) *
            afterUpdateFile:(GameObject<'S,'M,'T> -> string -> unit) *
            localisationExtension:string *
            ruleManagerSettings:RuleManagerSettings<'S,'M,'T> ->
              GameObject<'S,'M,'T>
      member
        InfoAtPos : pos:Utilities.Position.pos ->
                      file:string -> text:string -> SymbolInformation option
      member InitialConfigRules : unit -> unit
      member RefreshCaches : unit -> unit
      member RefreshValidationManager : unit -> unit
      member ReplaceConfigRules : rules:RulesSettings -> unit
      member
        UpdateFile : shallow:bool ->
                       file:string -> text:string option -> CWError list
      member FileManager : Files.FileManager
      member InfoService : Validation.Rules.FoldRules<'S> option
      member LocalisationManager : LocalisationManager<'S,'T,'M>
      member Lookup : Lookup<'S,'M>
      member ResourceManager : ResourceManager<'T>
      member Resources : IResourceAPI<'T>
      member RuleApplicator : Validation.Rules.RuleApplicator<'S> option
      member Settings : GameSettings<'M,'S>
      member ValidationManager : ValidationManager<'T,'S,'M>
      member completionService : Validation.Rules.CompletionService<'S> option
      member InfoService : Validation.Rules.FoldRules<'S> option with set
      member
        RuleApplicator : Validation.Rules.RuleApplicator<'S> option with set
      member
        completionService : Validation.Rules.CompletionService<'S> option
                              with set
      static member
        CreateGame : settings:(GameSettings<'M,'S> * string * string list *
                               ((unit -> Validation.Rules.FoldRules<'S> option) ->
                                  Entity -> 'T) *
                               ((unit -> Validation.Rules.FoldRules<'S> option) ->
                                  Entity -> 'T -> unit) *
                               ((string * string) list ->
                                  Localisation.ILocalisationAPICreator) *
                               (Lookup<'S,'M> ->
                                  Common.Lang * Map<string,Localisation.Entry> ->
                                  Common.Lang *
                                  Map<string,Process.Scopes.LocEntry<'S>>) *
                               (Lookup<'S,'M> -> Process.Scopes.LocEntry<'S> ->
                                  Process.Scopes.ScopeContext<'S> ->
                                  Validation.ValidationCore.ValidationResult) *
                               Process.Scopes.ScopeContext<'S> *
                               Process.Scopes.ScopeContext<'S> *
                               System.Text.Encoding * System.Text.Encoding *
                               ValidationManagerSettings<'T,'S,'M> *
                               (GameObject<'S,'M,'T> -> CWError list) *
                               (GameObject<'S,'M,'T> -> string -> unit) * string *
                               RuleManagerSettings<'S,'M,'T>) ->
                       afterInit:(GameObject<'S,'M,'T> -> unit) ->
                         GameObject<'S,'M,'T>
    end

namespace CWTools.Games.Stellaris
  module STLGameFunctions = begin
    type GameObject =
      GameObject<Common.STLConstants.Scope,Common.STLConstants.Modifier,
                 STLLookup.STLComputedData>
    val processLocalisationFunction :
      localisationCommands:(string * Common.STLConstants.Scope list) list ->
        lookup:Lookup<Common.STLConstants.Scope,Common.STLConstants.Modifier> ->
          (Common.Lang * Map<string,Localisation.Entry> ->
             Common.Lang *
             Map<string,Process.Scopes.LocEntry<Common.STLConstants.Scope>>)
    val validateLocalisationCommandFunction :
      localisationCommands:(string * Common.STLConstants.Scope list) list ->
        lookup:Lookup<Common.STLConstants.Scope,Common.STLConstants.Modifier> ->
          (Process.Scopes.LocEntry<Common.STLConstants.Scope> ->
             Process.Scopes.ScopeContext<Common.STLConstants.Scope> ->
             Validation.ValidationCore.ValidationResult)
    val updateScriptedTriggers :
      game:GameObject -> Common.STLConstants.Effect list
    val updateScriptedEffects :
      game:GameObject -> Common.STLConstants.Effect list
    val updateStaticodifiers : game:GameObject -> unit
    val updateScriptedLoc : game:GameObject -> unit
    val updateDefinedVariables : game:GameObject -> unit
    val afterUpdateFile :
      game:GameObject<Common.STLConstants.Scope,Common.STLConstants.Modifier,
                      STLLookup.STLComputedData> -> filepath:string -> unit
    val globalLocalisation : game:GameObject -> CWError list
    val updateModifiers : game:GameObject -> unit
    val updateTechnologies : game:GameObject -> unit
    val addModifiersWithScopes :
      lookup:Lookup<'a,Common.STLConstants.Modifier> ->
        Rules.RootRule<Common.STLConstants.Scope> list
        when 'a : comparison and 'a :> Common.IScope<'a>
    val addTriggerDocsScopes :
      lookup:Lookup<Common.STLConstants.Scope,#Common.IModifier> ->
        rules:Rules.RootRule<Common.STLConstants.Scope> list ->
          Rules.RootRule<Common.STLConstants.Scope> list
    val loadConfigRulesHook :
      rules:Rules.RootRule<Common.STLConstants.Scope> list ->
        lookup:Lookup<Common.STLConstants.Scope,Common.STLConstants.Modifier> ->
          embedded:EmbeddedSettings<Common.STLConstants.Scope,'a> ->
            Rules.RootRule<Common.STLConstants.Scope> list
    val refreshConfigBeforeFirstTypesHook :
      lookup:Lookup<'a,#Common.IModifier> ->
        'c -> embeddedSettings:EmbeddedSettings<'d,'e> -> unit
        when 'a : comparison and 'a :> Common.IScope<'a> and 'd : comparison
    val refreshConfigAfterFirstTypesHook :
      lookup:Lookup<'a,#Common.IModifier> ->
        resources:IResourceAPI<#ComputedData> ->
          embeddedSettings:EmbeddedSettings<'a,'d> -> unit
        when 'a : comparison and 'a :> Common.IScope<'a>
    val refreshConfigAfterVarDefHook :
      lookup:Lookup<'a,#Common.IModifier> ->
        resources:IResourceAPI<#ComputedData> ->
          embeddedSettings:EmbeddedSettings<'a,'d> -> unit
        when 'a : comparison and 'a :> Common.IScope<'a>
    val afterInit : game:GameObject -> unit
  end
  type StellarisSettings =
    GameSettings<Common.STLConstants.Modifier,Common.STLConstants.Scope>
  type STLGame =
    class
      interface
        IGame<STLLookup.STLComputedData,Common.STLConstants.Scope,
              Common.STLConstants.Modifier>
      new : settings:StellarisSettings -> STLGame
    end

namespace CWTools.Games.HOI4
  module HOI4GameFunctions = begin
    type GameObject =
      GameObject<Common.HOI4Constants.Scope,Common.HOI4Constants.Modifier,
                 HOI4ComputedData>
    val processLocalisationFunction :
      localisationCommands:(string * Common.HOI4Constants.Scope list) list ->
        lookup:Lookup<Common.HOI4Constants.Scope,Common.HOI4Constants.Modifier> ->
          (Common.Lang * Map<string,Localisation.Entry> ->
             Common.Lang *
             Map<string,Process.Scopes.LocEntry<Common.HOI4Constants.Scope>>)
    val validateLocalisationCommandFunction :
      localisationCommands:(string * Common.HOI4Constants.Scope list) list ->
        lookup:Lookup<Common.HOI4Constants.Scope,Common.HOI4Constants.Modifier> ->
          (Process.Scopes.LocEntry<Common.HOI4Constants.Scope> ->
             Process.Scopes.ScopeContext<Common.HOI4Constants.Scope> ->
             Validation.ValidationCore.ValidationResult)
    val globalLocalisation : game:GameObject -> CWError list
    val updateModifiers : game:GameObject -> unit
    val updateProvinces : game:GameObject -> unit
    val updateScriptedEffects :
      rules:Rules.RootRule<Common.HOI4Constants.Scope> list ->
        states:string list ->
          countries:string list -> Common.HOI4Constants.Effect list
    val updateScriptedTriggers :
      rules:Rules.RootRule<Common.HOI4Constants.Scope> list ->
        states:string list ->
          countries:string list -> Common.HOI4Constants.Effect list
    val addModifiersWithScopes :
      lookup:Lookup<'a,Common.HOI4Constants.Modifier> ->
        Rules.RootRule<Common.HOI4Constants.Scope> list
        when 'a : comparison and 'a :> Common.IScope<'a>
    val loadConfigRulesHook :
      rules:Rules.RootRule<Common.HOI4Constants.Scope> list ->
        lookup:Lookup<'a,Common.HOI4Constants.Modifier> ->
          embedded:EmbeddedSettings<'a,'b> ->
            Rules.RootRule<Common.HOI4Constants.Scope> list
        when 'a : comparison and 'a :> Common.IScope<'a>
    val refreshConfigBeforeFirstTypesHook :
      lookup:Lookup<'a,#Common.IModifier> -> 'c -> 'd -> unit
        when 'a : comparison and 'a :> Common.IScope<'a>
    val refreshConfigAfterFirstTypesHook :
      lookup:Lookup<Common.HOI4Constants.Scope,#Common.IModifier> ->
        'b ->
          embeddedSettings:EmbeddedSettings<Common.HOI4Constants.Scope,'c> ->
            unit
    val refreshConfigAfterVarDefHook :
      lookup:Lookup<'a,#Common.IModifier> ->
        resources:IResourceAPI<#ComputedData> ->
          embeddedSettings:EmbeddedSettings<'a,'d> -> unit
        when 'a : comparison and 'a :> Common.IScope<'a>
    val afterInit : game:GameObject -> unit
  end
  type HOI4Settings =
    GameSettings<Common.HOI4Constants.Modifier,Common.HOI4Constants.Scope>
  type HOI4Game =
    class
      interface
        IGame<HOI4ComputedData,Common.HOI4Constants.Scope,
              Common.HOI4Constants.Modifier>
      new : settings:HOI4Settings -> HOI4Game
    end

namespace CWTools.Games.EU4
  module EU4GameFunctions = begin
    type GameObject =
      GameObject<Common.EU4Constants.Scope,Common.EU4Constants.Modifier,
                 EU4ComputedData>
    val processLocalisationFunction :
      localisationCommands:(string * Common.EU4Constants.Scope list) list ->
        lookup:Lookup<Common.EU4Constants.Scope,Common.EU4Constants.Modifier> ->
          (Common.Lang * Map<string,Localisation.Entry> ->
             Common.Lang *
             Map<string,Process.Scopes.LocEntry<Common.EU4Constants.Scope>>)
    val validateLocalisationCommandFunction :
      localisationCommands:(string * Common.EU4Constants.Scope list) list ->
        lookup:Lookup<Common.EU4Constants.Scope,Common.EU4Constants.Modifier> ->
          (Process.Scopes.LocEntry<Common.EU4Constants.Scope> ->
             Process.Scopes.ScopeContext<Common.EU4Constants.Scope> ->
             Validation.ValidationCore.ValidationResult)
    val globalLocalisation : game:GameObject -> CWError list
    val updateScriptedLoc : game:GameObject -> unit
    val updateModifiers : game:GameObject -> unit
    val updateLegacyGovernments : game:GameObject -> unit
    val addModifiersWithScopes :
      lookup:Lookup<'a,Common.EU4Constants.Modifier> ->
        Rules.RootRule<Common.EU4Constants.Scope> list
        when 'a : comparison and 'a :> Common.IScope<'a>
    val updateScriptedEffects :
      rules:Rules.RootRule<Common.EU4Constants.Scope> list ->
        Common.EU4Constants.Effect list
    val updateScriptedTriggers :
      rules:Rules.RootRule<Common.EU4Constants.Scope> list ->
        Common.EU4Constants.Effect list
    val addModifiersAsTypes :
      lookup:Lookup<'a,Common.EU4Constants.Modifier> ->
        typesMap:Map<string,(bool * string * Utilities.Position.range) list> ->
          Map<string,(bool * string * Utilities.Position.range) list>
        when 'a : comparison and 'a :> Common.IScope<'a>
    val loadConfigRulesHook :
      rules:Rules.RootRule<Common.EU4Constants.Scope> list ->
        lookup:Lookup<Common.EU4Constants.Scope,Common.EU4Constants.Modifier> ->
          embedded:EmbeddedSettings<Common.EU4Constants.Scope,'a> ->
            Rules.RootRule<Common.EU4Constants.Scope> list
    val refreshConfigBeforeFirstTypesHook :
      lookup:Lookup<'a,Common.EU4Constants.Modifier> ->
        resources:IResourceAPI<EU4ComputedData> -> 'b -> unit
        when 'a : comparison and 'a :> Common.IScope<'a>
    val refreshConfigAfterFirstTypesHook :
      lookup:Lookup<'a,Common.EU4Constants.Modifier> ->
        'b -> embeddedSettings:EmbeddedSettings<'a,'c> -> unit
        when 'a : comparison and 'a :> Common.IScope<'a>
    val refreshConfigAfterVarDefHook :
      lookup:Lookup<'a,#Common.IModifier> ->
        resources:IResourceAPI<#ComputedData> ->
          embeddedSettings:EmbeddedSettings<'a,'d> -> unit
        when 'a : comparison and 'a :> Common.IScope<'a>
    val afterInit : game:GameObject -> unit
  end
  type EU4Settings =
    GameSettings<Common.EU4Constants.Modifier,Common.EU4Constants.Scope>
  type EU4Game =
    class
      interface
        IGame<EU4ComputedData,Common.EU4Constants.Scope,
              Common.EU4Constants.Modifier>
      new : settings:EU4Settings -> EU4Game
    end

namespace CWTools.Games.CK2
  module CK2GameFunctions = begin
    type GameObject =
      GameObject<Common.CK2Constants.Scope,Common.CK2Constants.Modifier,
                 CK2ComputedData>
    val processLocalisationFunction :
      localisationCommands:(string * Common.CK2Constants.Scope list) list ->
        lookup:Lookup<Common.CK2Constants.Scope,Common.CK2Constants.Modifier> ->
          (Common.Lang * Map<string,Localisation.Entry> ->
             Common.Lang *
             Map<string,Process.Scopes.LocEntry<Common.CK2Constants.Scope>>)
    val validateLocalisationCommandFunction :
      localisationCommands:(string * Common.CK2Constants.Scope list) list ->
        lookup:Lookup<Common.CK2Constants.Scope,Common.CK2Constants.Modifier> ->
          (Process.Scopes.LocEntry<Common.CK2Constants.Scope> ->
             Process.Scopes.ScopeContext<Common.CK2Constants.Scope> ->
             Validation.ValidationCore.ValidationResult)
    val globalLocalisation : game:GameObject -> CWError list
    val updateScriptedLoc : game:GameObject -> unit
    val updateModifiers : game:GameObject -> unit
    val addModifiersWithScopes :
      lookup:Lookup<'a,Common.CK2Constants.Modifier> ->
        Rules.RootRule<Common.CK2Constants.Scope> list
        when 'a : comparison and 'a :> Common.IScope<'a>
    val updateLandedTitles : game:GameObject -> unit
    val createLandedTitleTypes :
      lookup:Lookup<'a,#Common.IModifier> ->
        map:Map<string,(bool * string * Utilities.Position.range) list> ->
          Map<string,(bool * string * Utilities.Position.range) list>
        when 'a : comparison and 'a :> Common.IScope<'a>
    val updateProvinces : game:GameObject -> unit
    val updateScriptedEffects :
      lookup:Lookup<'a,#Common.IModifier> ->
        rules:Rules.RootRule<Common.CK2Constants.Scope> list ->
          embeddedSettings:EmbeddedSettings<'c,'d> ->
            Common.CK2Constants.Effect list
        when 'a : comparison and 'a :> Common.IScope<'a> and 'c : comparison
    val updateScriptedTriggers :
      lookup:Lookup<'a,#Common.IModifier> ->
        rules:Rules.RootRule<Common.CK2Constants.Scope> list ->
          embeddedSettings:EmbeddedSettings<'c,'d> ->
            Common.CK2Constants.Effect list
        when 'a : comparison and 'a :> Common.IScope<'a> and 'c : comparison
    val addModifiersAsTypes :
      lookup:Lookup<'a,Common.CK2Constants.Modifier> ->
        typesMap:Map<string,(bool * string * Utilities.Position.range) list> ->
          Map<string,(bool * string * Utilities.Position.range) list>
        when 'a : comparison and 'a :> Common.IScope<'a>
    val loadConfigRulesHook :
      rules:Rules.RootRule<Common.CK2Constants.Scope> list ->
        lookup:Lookup<Common.CK2Constants.Scope,Common.CK2Constants.Modifier> ->
          embedded:EmbeddedSettings<Common.CK2Constants.Scope,'a> ->
            Rules.RootRule<Common.CK2Constants.Scope> list
    val refreshConfigBeforeFirstTypesHook :
      lookup:Lookup<'a,#Common.IModifier> -> 'c -> 'd -> unit
        when 'a : comparison and 'a :> Common.IScope<'a>
    val refreshConfigAfterFirstTypesHook :
      lookup:Lookup<'a,Common.CK2Constants.Modifier> ->
        'b -> embeddedSettings:EmbeddedSettings<'a,'c> -> unit
        when 'a : comparison and 'a :> Common.IScope<'a>
    val refreshConfigAfterVarDefHook :
      lookup:Lookup<'a,#Common.IModifier> ->
        resources:IResourceAPI<#ComputedData> ->
          embeddedSettings:EmbeddedSettings<'a,'d> -> unit
        when 'a : comparison and 'a :> Common.IScope<'a>
    val afterInit : game:GameObject -> unit
  end
  type CK2Settings =
    GameSettings<Common.CK2Constants.Modifier,Common.CK2Constants.Scope>
  type CK2Game =
    class
      interface
        IGame<CK2ComputedData,Common.CK2Constants.Scope,
              Common.CK2Constants.Modifier>
      new : settings:CK2Settings -> CK2Game
    end

namespace CWTools.Games.IR
  module IRGameFunctions = begin
    type GameObject =
      GameObject<Common.IRConstants.Scope,Common.IRConstants.Modifier,
                 IRComputedData>
    val processLocalisationFunction :
      localisationCommands:(string * Common.IRConstants.Scope list) list ->
        lookup:Lookup<Common.IRConstants.Scope,Common.IRConstants.Modifier> ->
          (Common.Lang * Map<string,Localisation.Entry> ->
             Common.Lang *
             Map<string,Process.Scopes.LocEntry<Common.IRConstants.Scope>>)
    val validateLocalisationCommandFunction :
      localisationCommands:(string * Common.IRConstants.Scope list) list ->
        lookup:Lookup<Common.IRConstants.Scope,Common.IRConstants.Modifier> ->
          (Process.Scopes.LocEntry<Common.IRConstants.Scope> ->
             Process.Scopes.ScopeContext<Common.IRConstants.Scope> ->
             Validation.ValidationCore.ValidationResult)
    val globalLocalisation : game:GameObject -> CWError list
    val updateScriptedLoc : game:GameObject -> unit
    val updateModifiers : game:GameObject -> unit
    val addModifiersWithScopes :
      lookup:Lookup<'a,Common.IRConstants.Modifier> ->
        Rules.RootRule<Common.IRConstants.Scope> list
        when 'a : comparison and 'a :> Common.IScope<'a>
    val updateScriptedTriggers :
      lookup:Lookup<'a,#Common.IModifier> ->
        rules:Rules.RootRule<Common.IRConstants.Scope> list ->
          embeddedSettings:EmbeddedSettings<Common.IRConstants.Scope,'c> ->
            Common.IRConstants.Effect list
        when 'a : comparison and 'a :> Common.IScope<'a>
    val updateScriptedEffects :
      lookup:Lookup<'a,#Common.IModifier> ->
        rules:Rules.RootRule<Common.IRConstants.Scope> list ->
          embeddedSettings:EmbeddedSettings<Common.IRConstants.Scope,'c> ->
            Common.IRConstants.Effect list
        when 'a : comparison and 'a :> Common.IScope<'a>
    val addModifiersAsTypes :
      lookup:Lookup<'a,Common.IRConstants.Modifier> ->
        typesMap:Map<string,(bool * string * Utilities.Position.range) list> ->
          Map<string,(bool * string * Utilities.Position.range) list>
        when 'a : comparison and 'a :> Common.IScope<'a>
    val updateProvinces : game:GameObject -> unit
    val updateCharacters : game:GameObject -> unit
    val addScriptFormulaLinks :
      lookup:Lookup<'S,#Common.IModifier> ->
        Common.Effect<Common.IRConstants.Scope> list
        when 'S : comparison and 'S :> Common.IScope<'S>
    val addTriggerDocsScopes :
      lookup:Lookup<Common.IRConstants.Scope,#Common.IModifier> ->
        rules:Rules.RootRule<Common.IRConstants.Scope> list ->
          Rules.RootRule<Common.IRConstants.Scope> list
    val loadConfigRulesHook :
      rules:Rules.RootRule<Common.IRConstants.Scope> list ->
        lookup:Lookup<Common.IRConstants.Scope,Common.IRConstants.Modifier> ->
          embedded:EmbeddedSettings<Common.IRConstants.Scope,'a> ->
            Rules.RootRule<Common.IRConstants.Scope> list
    val refreshConfigBeforeFirstTypesHook :
      lookup:Lookup<'a,#Common.IModifier> -> 'c -> 'd -> unit
        when 'a : comparison and 'a :> Common.IScope<'a>
    val refreshConfigAfterFirstTypesHook :
      lookup:Lookup<Common.IRConstants.Scope,Common.IRConstants.Modifier> ->
        'a -> embedded:EmbeddedSettings<Common.IRConstants.Scope,'b> -> unit
    val refreshConfigAfterVarDefHook :
      lookup:Lookup<Common.IRConstants.Scope,#Common.IModifier> ->
        resources:IResourceAPI<#ComputedData> ->
          embedded:EmbeddedSettings<Common.IRConstants.Scope,'c> -> unit
    val afterInit : game:GameObject -> unit
  end
  type IRSettings =
    GameSettings<Common.IRConstants.Modifier,Common.IRConstants.Scope>
  type IRGame =
    class
      interface
        IGame<IRComputedData,Common.IRConstants.Scope,
              Common.IRConstants.Modifier>
      new : settings:IRSettings -> IRGame
    end

namespace CWTools.CSharp
  [<System.Runtime.CompilerServices.Extension ()>]
  type Extensions =
    class
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        Comments : obj:System.Collections.Generic.IEnumerable<Process.Child> ->
                     seq<string>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        GetError : obj:FParsec.CharParsers.ParserResult<'a,'b> -> string
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        GetResult : obj:FParsec.CharParsers.ParserResult<'c,'d> -> 'c
                      when 'c : null
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        LeafValues : obj:System.Collections.Generic.IEnumerable<Process.Child> ->
                       seq<Process.LeafValue>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        Leaves : obj:System.Collections.Generic.IEnumerable<Process.Child> ->
                   seq<Process.Leaf>
      [<System.Runtime.CompilerServices.Extension ()>]
      static member
        Nodes : obj:System.Collections.Generic.IEnumerable<Process.Child> ->
                  seq<Process.Node>
    end

namespace CWTools.ExtensionPoints
  [<AbstractClassAttribute ()>]
  type StatementVisitor =
    class
      new : unit -> StatementVisitor
      abstract member Visit : Process.Node -> Unit
      abstract member Visit : Process.Leaf -> Unit
      abstract member Visit : Process.LeafValue -> Unit
      abstract member Visit : Process.ValueClause -> Unit
      abstract member Visit : string -> Unit
      member Visit : x:Process.Child -> Unit
    end



