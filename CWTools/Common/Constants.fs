namespace CWTools.Common

open System.Collections.Generic
open System
open CWTools.Utilities

type Game =
    | CK2 = 0
    | HOI4 = 1
    | EU4 = 2
    | STL = 3
    | VIC2 = 4
    | IR = 5
    | CK3 = 6
    | VIC3 = 7
    | EU5 = 8
    | Custom = 99

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
    | Japanese = 9
    | Korean = 10

type HOI4Lang =
    | English = 0
    | French = 1
    | German = 2
    | Spanish = 3
    | Russian = 4
    | Polish = 5
    | Braz_Por = 6
    | Default = 7 //Default doesnt' exist!
    | Chinese = 8
    | Japanese = 9

type EU4Lang =
    | English = 0
    | French = 1
    | German = 2
    | Spanish = 3
    | Default = 4

type EU5Lang =
    | English = 0
    | French = 1
    | German = 2
    | Spanish = 3
    | Chinese = 4
    | Russian = 5
    | Korean = 6
    | Braz_Por = 7
    | Japanese = 8
    | Polish = 9
    | Turkish = 10

type IRLang =
    | English = 0
    | French = 1
    | German = 2
    | Spanish = 3
    | Chinese = 4
    | Russian = 5

type VIC2Lang =
    | English = 0
    | French = 1
    | German = 2
    | Spanish = 3

type CK3Lang =
    | English = 0
    | French = 1
    | German = 2
    | Spanish = 3
    | Chinese = 4
    | Russian = 5
    | Korean = 6

type VIC3Lang =
    | English = 0
    | French = 1
    | German = 2
    | Spanish = 3
    | Chinese = 4
    | Russian = 5
    | Korean = 6
    | Braz_Por = 7
    | Japanese = 8
    | Polish = 9
    | Turkish = 10

type CustomLang =
    | English = 0
    | French = 1
    | German = 2
    | Spanish = 3
    | Russian = 4
    | Polish = 5
    | Braz_Por = 6
    | Chinese = 7
    | Default = 8

type Lang =
    | CK2 of CK2Lang
    | STL of STLLang
    | HOI4 of HOI4Lang
    | EU4 of EU4Lang
    | EU5 of EU5Lang
    | IR of IRLang
    | VIC2 of VIC2Lang
    | CK3 of CK3Lang
    | VIC3 of VIC3Lang
    | Custom of CustomLang

    override x.ToString() =
        x
        |> function
            | CK2 c -> c.ToString()
            | STL s -> s.ToString()
            | HOI4 s -> s.ToString()
            | EU4 s -> s.ToString()
            | EU5 s -> s.ToString()
            | IR s -> s.ToString()
            | VIC2 s -> s.ToString()
            | CK3 s -> s.ToString()
            | VIC3 s -> s.ToString()
            | Custom s -> s.ToString()

module LangHelpers =
    let allCK2Langs =
        [ CK2 CK2Lang.English
          CK2 CK2Lang.French
          CK2 CK2Lang.German
          CK2 CK2Lang.Spanish
          CK2 CK2Lang.Russian ]

    let allSTLLangs =
        [ STL STLLang.English
          STL STLLang.French
          STL STLLang.German
          STL STLLang.Spanish
          STL STLLang.Russian
          STL STLLang.Polish
          STL STLLang.Braz_Por
          STL STLLang.Chinese
          STL STLLang.Japanese
          STL STLLang.Korean ]

    let allHOI4Langs =
        [ HOI4 HOI4Lang.English
          HOI4 HOI4Lang.French
          HOI4 HOI4Lang.German
          HOI4 HOI4Lang.Spanish
          HOI4 HOI4Lang.Russian
          HOI4 HOI4Lang.Polish
          HOI4 HOI4Lang.Braz_Por
          HOI4 HOI4Lang.Chinese
          HOI4 HOI4Lang.Japanese ]

    let allEU4Langs =
        [ EU4 EU4Lang.English
          EU4 EU4Lang.French
          EU4 EU4Lang.German
          EU4 EU4Lang.Spanish ]

    let allEU5Langs =
        [ EU5 EU5Lang.English
          EU5 EU5Lang.Chinese
          EU5 EU5Lang.French
          EU5 EU5Lang.German
          EU5 EU5Lang.Japanese
          EU5 EU5Lang.Korean
          EU5 EU5Lang.Polish
          EU5 EU5Lang.Russian
          EU5 EU5Lang.Spanish
          EU5 EU5Lang.Turkish
          EU5 EU5Lang.Braz_Por ]


    let allIRLangs =
        [ IR IRLang.English
          IR IRLang.French
          IR IRLang.German
          IR IRLang.Spanish
          IR IRLang.Russian
          IR IRLang.Chinese ]

    let allVIC2Langs =
        [ VIC2 VIC2Lang.English
          VIC2 VIC2Lang.French
          VIC2 VIC2Lang.German
          VIC2 VIC2Lang.Spanish ]

    let allCK3Langs =
        [ CK3 CK3Lang.English
          CK3 CK3Lang.French
          CK3 CK3Lang.German
          CK3 CK3Lang.Spanish
          CK3 CK3Lang.Chinese
          CK3 CK3Lang.Russian
          CK3 CK3Lang.Korean ]

    let allVIC3Langs =
        [ VIC3 VIC3Lang.English
          VIC3 VIC3Lang.Chinese
          VIC3 VIC3Lang.French
          VIC3 VIC3Lang.German
          VIC3 VIC3Lang.Japanese
          VIC3 VIC3Lang.Korean
          VIC3 VIC3Lang.Polish
          VIC3 VIC3Lang.Russian
          VIC3 VIC3Lang.Spanish
          VIC3 VIC3Lang.Turkish
          VIC3 VIC3Lang.Braz_Por ]

    let allCustomLangs =
        [ Custom CustomLang.English
          Custom CustomLang.French
          Custom CustomLang.German
          Custom CustomLang.Spanish
          Custom CustomLang.Russian
          Custom CustomLang.Polish
          Custom CustomLang.Braz_Por
          Custom CustomLang.Chinese ]

type RawEffect =
    { name: string
      desc: string
      usage: string
      scopes: string list
      targets: string list
      traits: string option }

type Severity =
    | Error = 1
    | Warning = 2
    | Information = 3
    | Hint = 4

[<AutoOpen>]
module rec NewScope =
    open CWTools.Utilities.Utils

    type ScopeInput =
        { name: string
          aliases: string list
          isSubscopeOf: string list
          dataTypeName: string option }

    type ScopeWrapper = byte

    type ScopeManager() =
        let mutable initialized = false
        let mutable dict = Dictionary<string, Scope>()
        let mutable reverseDict = Dictionary<Scope, ScopeInput>()
        let mutable groupDict = Map<string, Scope list>(Seq.empty)
        let mutable complexEquality = false
        let mutable matchesSet = Set<Scope * Scope>(Seq.empty)
        let mutable dataTypeMap = Map<Scope, string>(Seq.empty)
        let anyScope = Scope(0uy)

        let anyScopeInput =
            { ScopeInput.name = "Any"
              aliases = [ "any"; "all"; "no_scope"; "none" ]
              isSubscopeOf = []
              dataTypeName = None }

        let invalidScope = Scope(1uy)

        let invalidScopeInput =
            { ScopeInput.name = "Invalid"
              aliases = [ "invalid_scope" ]
              isSubscopeOf = []
              dataTypeName = None }

        let parseScope () =
            if not initialized then
                CWTools.Utilities.Utils.logError "Error: parseScope was used without initializing scopes"
            else
                ()

            (fun (x: string) ->
                let found, value = dict.TryGetValue(x.ToLower())

                if found then
                    value
                else
                    log (sprintf "Unexpected scope %O" x)
                    anyScope)

        let init (scopes: ScopeInput list, scopeGroups: (string * string list) list) =
            initialized <- true
            // log (sprintfn "Init scopes %A" scopes)
            dict <- Dictionary<string, Scope>()
            reverseDict <- Dictionary<Scope, ScopeInput>()
            dict.Add("any", anyScope)
            dict.Add("all", anyScope)
            dict.Add("no_scope", anyScope)
            dict.Add("none", anyScope)
            dict.Add("invalid_scope", invalidScope)
            reverseDict.Add(anyScope, anyScopeInput)
            reverseDict.Add(invalidScope, invalidScopeInput)
            let mutable nextByte = 2uy

            let addScope (newScope: ScopeInput) =
                let newID = nextByte
                nextByte <- nextByte + 1uy
                newScope.aliases |> List.iter (fun s -> dict.Add(s, Scope(newID)))
                reverseDict.Add(Scope(newID), newScope)

                match newScope.dataTypeName with
                | Some dtn -> dataTypeMap <- dataTypeMap |> Map.add (Scope(newID)) dtn
                | None -> ()

            scopes |> List.iter addScope

            let addScopeSubset (newScope: ScopeInput) =
                newScope.isSubscopeOf
                |> List.iter (fun ss ->
                    matchesSet <-
                        (Set.add (parseScope () (newScope.aliases |> List.head), parseScope () ss) matchesSet))

            scopes |> List.iter addScopeSubset

            if Set.isEmpty matchesSet then
                ()
            else
                complexEquality <- true

            groupDict <-
                scopeGroups
                |> List.map (fun (name, scopes) -> name, (scopes |> List.map (fun s -> parseScope () s)))
                |> Map.ofList

        member this.GetName(scope: Scope) =
            let found, value = reverseDict.TryGetValue scope

            if found then
                value.name
            else
                log (sprintf "Unexpected scope %O" scope.tag)
                ""

        member this.AllScopes = reverseDict.Keys |> List.ofSeq
        member this.AnyScope = anyScope
        member this.InvalidScope = invalidScope
        member this.ParseScope = parseScope
        member this.ScopeGroups = groupDict

        member this.ParseScopes =
            function
            | "all" -> this.AllScopes
            | x -> [ this.ParseScope () x ]

        member this.ReInit(scopes: ScopeInput list, scopeGroups: (string * string list) list) =
            init (scopes, scopeGroups)

        member this.MatchesScope (source: Scope) (target: Scope) =
            if not complexEquality then
                match source, target with
                | x, _
                | _, x when x = anyScope -> true
                | x, y -> x = y
            else
                match Set.contains (source, target) matchesSet, source, target with
                | true, _, _ -> true
                | _, x, _
                | _, _, x when x = anyScope -> true
                | _, x, y -> x = y

        member this.DataTypeForScope(scope: Scope) =
            Map.tryFind scope dataTypeMap |> Option.defaultValue (scope.ToString())

        member this.Initialized = initialized

    let scopeManager = ScopeManager()

    type ModifierCategoryInput =
        { name: string
          internalID: int option
          scopes: Scope list }

    [<Sealed>]
    type ModifierCategoryManager() =
        let mutable initialized = false
        let mutable dict = Dictionary<string, ModifierCategory>()

        let mutable reverseDict = Dictionary<ModifierCategory, ModifierCategoryInput>()

        let mutable matchesSet = Set<ModifierCategory * Scope>(Seq.empty)
        let mutable idMap = Map<int, ModifierCategory>([])
        let anyModifier = ModifierCategory(0uy)

        let anyModifierInput =
            { ModifierCategoryInput.name = "Any"
              internalID = None
              scopes = [ scopeManager.AnyScope ] }

        let invalidModifier = ModifierCategory(1uy)

        let invalidModifierInput =
            { ModifierCategoryInput.name = "Invalid"
              internalID = None
              scopes = [] }

        let parseModifierCategory () =
            if not initialized then
                CWTools.Utilities.Utils.logError
                    "Error: parseModifierCategory was used without initializing modifier categories"
            else
                ()

            (fun (x: string) ->
                let found, value = dict.TryGetValue(x.ToLower())

                if found then
                    value
                else
                    log (sprintf "Unexpected modifier category %O" x)
                    anyModifier)

        let init (modifiers: ModifierCategoryInput list) =
            initialized <- true
            // log (sprintfn "Init scopes %A" scopes)
            dict <- Dictionary<string, ModifierCategory>()
            reverseDict <- Dictionary<ModifierCategory, ModifierCategoryInput>()
            dict.Add("any", anyModifier)
            dict.Add("invalid_modifier", invalidModifier)
            reverseDict.Add(anyModifier, anyModifierInput)
            reverseDict.Add(invalidModifier, invalidModifierInput)
            let mutable nextByte = 2uy

            let addModifier (newModifier: ModifierCategoryInput) =
                let newID = nextByte
                nextByte <- nextByte + 1uy
                let modifier = ModifierCategory(newID)
                dict.Add(newModifier.name.ToLower(), modifier)
                reverseDict.Add(modifier, newModifier)

                newModifier.scopes
                |> List.iter (fun s -> matchesSet <- matchesSet |> Set.add (modifier, s))

                match newModifier.internalID with
                | Some id -> idMap <- idMap |> (Map.add id modifier)
                | None -> ()

            modifiers |> List.iter addModifier

        member this.GetName(modifier: ModifierCategory) =
            let found, value = reverseDict.TryGetValue modifier

            if found then
                value.name
            else
                log (sprintf "Unexpected modifier category %O" modifier.tag)
                ""

        member this.AllModifiers = reverseDict.Keys |> List.ofSeq
        member this.AnyModifier = anyModifier
        member this.InvalidModifiere = invalidModifier
        member this.ParseModifier = parseModifierCategory
        member this.ReInit(modifiers: ModifierCategoryInput list) = init modifiers

        member this.SupportsScope (source: ModifierCategory) (target: Scope) =
            match Set.contains (source, target) matchesSet, source, target with
            | true, _, _ -> true
            | _, x, _ when x = anyModifier -> true
            | _, _, x when x = scopeManager.AnyScope -> true
            | _ -> false

        member this.SupportedScopes(modifier: ModifierCategory) =
            let found, value = reverseDict.TryGetValue modifier

            if found then
                value.scopes
            else
                log (sprintf "Unexpected modifier category %O" modifier.tag)
                []

        member this.GetCategoryFromID(id: int) =
            match idMap |> Map.tryFind id with
            | Some category -> category
            | None -> anyModifier

        member this.Initialized = initialized

    let modifierCategoryManager = ModifierCategoryManager()

    [<Sealed>]
    type ModifierCategory(tag: byte) =
        member _.tag = tag
        override x.ToString() = modifierCategoryManager.GetName(x)

        override x.Equals(target: obj) =
            match target with
            | :? ModifierCategory as t -> tag = t.tag
            | _ -> false

        override x.GetHashCode() = tag.GetHashCode()

        interface IComparable with
            member this.CompareTo target =
                match target with
                | :? ModifierCategory as t -> tag.CompareTo t.tag
                | _ -> 0

        member this.SupportsScope x =
            modifierCategoryManager.SupportsScope this x

        member this.Name = modifierCategoryManager.GetName this

    type Modifier = ModifierCategory

    [<Sealed>]
    type Scope(tag: byte) =
        member _.tag = tag
        override x.ToString() = scopeManager.GetName(x)

        override x.Equals(target: obj) =
            match target with
            | :? Scope as t -> tag = t.tag
            | _ -> false

        override x.GetHashCode() = tag.GetHashCode()

        interface IComparable with
            member this.CompareTo target =
                match target with
                | :? Scope as t -> tag.CompareTo t.tag
                | _ -> 0

        member this.AnyScope = scopeManager.AnyScope

        member this.IsOfScope target =
            match this, target with
            | _, x
            | x, _ when x = scopeManager.AnyScope -> true
            | this, target -> scopeManager.MatchesScope this target


    type TypeDefInfo =
        { id: string
          validate: bool
          range: Position.range
          explicitLocalisation: (string * string * bool) list
          subtypes: string list }

type ModifierSource =
    | Rules
    | CodeGen
    | TypeDef of name: string * typeDef: string

type ActualModifier =
    { tag: string
      // source: ModifierSource
      category: ModifierCategory }

type StaticModifier =
    { tag: string
      categories: ModifierCategory list }

type EffectType =
    | Effect = 0uy
    | Trigger = 1uy
    | Link = 2uy
    | ValueTrigger = 3uy

type ReferenceHint =
    | TypeRef of typeName: string * typeValue: string
    | LocRef of locKey: string
    | EnumRef of enumName: string * enumValue: string
    | FileRef of filename: string

type Effect internal (name, scopes, effectType, refHint) =
    member val Name: StringTokens = name
    member val Scopes: Scope list = scopes
    member val Type: EffectType = effectType
    member val RefHint: ReferenceHint option = refHint


    override x.Equals(y) =
        match y with
        | :? Effect as y -> x.Name = y.Name && x.Scopes = y.Scopes && x.Type = y.Type
        | _ -> false

    override x.GetHashCode() =
        hash (name, scopes, effectType, refHint)

    interface IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Effect as y ->
                let r1 = x.Name.normal.CompareTo(y.Name.normal)

                if r1 = 0 then
                    0
                else
                    List.compareWith compare x.Scopes y.Scopes
            | _ -> invalidArg "yobj" ("cannot compare values of different types" + yobj.GetType().ToString())

    override x.ToString() =
        sprintf "%s: %A" (StringResource.stringManager.GetStringForIDs x.Name) x.Scopes

    new(name: StringTokens, scopes, effectType) = Effect(name, scopes, effectType, None)

    new(name: string, scopes, effectType) =
        Effect(StringResource.stringManager.InternIdentifierToken name, scopes, effectType, None)

    new(name: string, scopes, effectType, refHint) =
        Effect(StringResource.stringManager.InternIdentifierToken name, scopes, effectType, refHint)

[<Sealed>]
type ScriptedEffect(name: StringTokens, scopes, effectType, comments, globals, settargets, usedtargets) =
    inherit Effect(name, scopes, effectType)
    member val Comments: string = comments
    member val GlobalEventTargets: string list = globals
    member val SavedEventTargets: string list = settargets
    member val UsedEventTargets: string list = usedtargets

    override x.Equals(y) =
        match y with
        | :? ScriptedEffect as y -> x.Name = y.Name && x.Scopes = y.Scopes && x.Type = y.Type
        | _ -> false


    override x.GetHashCode() =
        hash (x.Name, x.Scopes, x.Type, x.Comments, x.GlobalEventTargets, x.SavedEventTargets, x.UsedEventTargets)

    interface IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Effect as y -> x.Name.normal.CompareTo(y.Name.normal)
            | _ -> invalidArg "yobj" "cannot compare values of different types"

type DocEffect(name: StringTokens, scopes, target, effectType, desc, usage, refHint) =
    inherit Effect(name, scopes, effectType, refHint)
    member val Desc: string = desc
    member val Usage: string = usage
    member val Target: Scope option = target

    override x.Equals(y) =
        match y with
        | :? DocEffect as y ->
            x.Name = y.Name
            && x.Scopes = y.Scopes
            && x.Type = y.Type
            && x.Desc = y.Desc
            && x.Usage = y.Usage
        | _ -> false

    override x.GetHashCode() =
        hash (x.Name, x.Scopes, x.Type, x.Desc, x.Usage, x.Target)


    interface IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Effect as y -> x.Name.normal.CompareTo(y.Name.normal)
            | _ -> invalidArg "yobj" "cannot compare values of different types"

    new(rawEffect: RawEffect, effectType: EffectType, parseScopes) =
        let scopes =
            rawEffect.scopes |> List.map (fun x -> x.Trim()) |> List.collect parseScopes

        let target = rawEffect.targets |> List.collect parseScopes |> List.tryHead

        DocEffect(
            StringResource.stringManager.InternIdentifierToken rawEffect.name,
            scopes,
            target,
            effectType,
            rawEffect.desc,
            rawEffect.usage
        )

    new(name, scopes, target, effectType, desc, usage) = DocEffect(name, scopes, target, effectType, desc, usage, None)

    new(name: string, scopes, target, effectType, desc, usage) =
        DocEffect(
            StringResource.stringManager.InternIdentifierToken name,
            scopes,
            target,
            effectType,
            desc,
            usage,
            None
        )

    new(name: string, scopes, target, effectType, desc, usage, refHint) =
        DocEffect(
            StringResource.stringManager.InternIdentifierToken name,
            scopes,
            target,
            effectType,
            desc,
            usage,
            refHint
        )

type ScopedEffect
    (
        name: StringTokens,
        scopes,
        inner,
        effectType,
        desc,
        usage,
        isScopeChange,
        ignoreChildren,
        scopeonlynoteffect,
        isValue,
        isWildCard,
        refHint
    ) =
    inherit DocEffect(name, scopes, inner, effectType, desc, usage, refHint)
    member val IsScopeChange: bool = isScopeChange
    member val IgnoreChildren: string list = ignoreChildren
    member val ScopeOnlyNotEffect: bool = scopeonlynoteffect
    /// If this scoped effect is a value scope
    member val IsValueScope: bool = isValue
    /// If this scoped effect is a prefix that should accept anything afterwards
    member val IsWildCard: bool = isWildCard

    new(de: DocEffect, inner, isScopeChange, ignoreChildren, scopeonlynoteffect, isValue) =
        ScopedEffect(
            de.Name,
            de.Scopes,
            inner,
            de.Type,
            de.Desc,
            de.Usage,
            isScopeChange,
            ignoreChildren,
            scopeonlynoteffect,
            isValue,
            false,
            None
        )

    new(de: DocEffect, inner) =
        ScopedEffect(de.Name, de.Scopes, inner, de.Type, de.Desc, de.Usage, true, [], false, false, false, None)

    new(name, scopes, inner, effectType, desc, usage, scopeonlynoteffect, isValue, refHint) =
        ScopedEffect(
            name,
            scopes,
            inner,
            effectType,
            desc,
            usage,
            true,
            [],
            scopeonlynoteffect,
            isValue,
            false,
            refHint
        )

    new(name: string, scopes, inner, effectType, desc, usage, scopeonlynoteffect) =
        ScopedEffect(
            StringResource.stringManager.InternIdentifierToken name,
            scopes,
            Some inner,
            effectType,
            desc,
            usage,
            true,
            [],
            scopeonlynoteffect,
            false,
            false,
            None
        )
    // ScopedEffect((StringResource.stringManager.InternIdentifierToken name), scopes, inner, effectType, desc, usage, true, [], scopeonlynoteffect, false, false, None)
    new(name: StringTokens, scopes, inner, effectType, desc, usage, scopeonlynoteffect) =
        ScopedEffect(name, scopes, inner, effectType, desc, usage, true, [], scopeonlynoteffect, false, false, None)

    new(name, scopes, inner, effectType, desc, usage, scopeonlynoteffect) =
        ScopedEffect(
            name,
            scopes,
            Some inner,
            effectType,
            desc,
            usage,
            true,
            [],
            scopeonlynoteffect,
            false,
            false,
            None
        )

type TitleType =
    | Empire = 0uy
    | Kingdom = 1uy
    | Duchy_Hired = 2uy
    | Duchy_Normal = 3uy
    | County = 4uy
    | Barony = 5uy

type DataLinkType =
    | Scope = 0uy
    | Value = 1uy
    | Both = 2uy

type EventTargetDataLink =
    { name: string
      inputScopes: Scope list
      outputScope: Scope
      description: string
      dataPrefix: string option
      sourceRuleType: string
      dataLinkType: DataLinkType }
 
type EventTargetLink =
    | SimpleLink of ScopedEffect
    | DataLink of EventTargetDataLink
