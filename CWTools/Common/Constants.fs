namespace CWTools.Common
open System.Collections.Generic
open System

type Game = |CK2 = 0 |HOI4 = 1 |EU4 = 2 |STL = 3 |VIC2 = 4 |Custom = 99
type CK2Lang = |English = 0 |French = 1 |German = 2 |Spanish = 3 |Russian = 4 |Default = 5
type STLLang = |English = 0 |French = 1 |German = 2 |Spanish = 3 |Russian = 4 |Polish = 5 |Braz_Por = 6  |Default = 7 |Chinese = 8
type HOI4Lang = |English = 0 |French = 1 |German = 2 |Spanish = 3 |Russian = 4 |Polish = 5 |Braz_Por = 6 |Default = 7 //Default doesnt' exist!
type EU4Lang = |English = 0 |French = 1 |German = 2 |Spanish = 3 |Default = 4
type IRLang = |English = 0 |French = 1 |German = 2 |Spanish = 3 |Chinese = 4 |Russian = 5
type VIC2Lang = |English = 0 |French = 1 |German = 2 |Spanish = 3
type CustomLang = |English = 0 |French = 1 |German = 2 |Spanish = 3 |Russian = 4 |Polish = 5 |Braz_Por = 6 |Chinese = 7 |Default = 8
type Lang =
    |CK2 of CK2Lang
    |STL of STLLang
    |HOI4 of HOI4Lang
    |EU4 of EU4Lang
    |IR of IRLang
    |VIC2 of VIC2Lang
    |Custom of CustomLang
    override x.ToString() = x |> function |CK2 c -> c.ToString() | STL s -> s.ToString() |HOI4 s -> s.ToString() |EU4 s -> s.ToString() |IR s -> s.ToString() |VIC2 s -> s.ToString() |Custom s -> s.ToString()

type RawEffect =
    {
        name : string
        desc : string
        usage : string
        scopes : string list
        targets : string list
        traits : string option
    }

type Severity =
| Error = 1
| Warning = 2
| Information = 3
| Hint = 4

type IScope<'T> =
    abstract member AnyScope : 'T
    /// The first value is or can be coerced to the second
    abstract member IsOfScope : 'T -> bool

type IModifier =
    abstract member Tag : string


[<AutoOpen>]
module rec NewScope =
    open CWTools.Utilities.Utils
    type ScopeInput = {
        name : string
        aliases : string list
        isSubscopeOf : string list
    }
    type ScopeWrapper = byte
        // override x.ToString() =
    type ScopeManager() =
        let mutable initialized = false
        let mutable dict = Dictionary<string, Scope>()
        let mutable reverseDict = Dictionary<Scope, ScopeInput>()
        let mutable complexEquality = false
        let mutable matchesSet = Set<Scope * Scope>(Seq.empty)
        let anyScope = Scope(0uy)
        let anyScopeInput = { ScopeInput.name = "Any"; aliases = ["any"; "all"; "no_scope"; "none"]; isSubscopeOf = [] }
        let invalidScope = Scope(1uy)
        let invalidScopeInput = { ScopeInput.name = "Invalid"; aliases = ["invalid_scope"]; isSubscopeOf = []}
        let parseScope() =
            if not initialized then eprintfn "Error: parseScope was used without initializing scopes" else ()
            (fun (x : string) ->
            let found, value = dict.TryGetValue (x.ToLower())
            if found
            then value
            else log (sprintf "Unexpected scope %O" x ); anyScope
            )
        let init(scopes : ScopeInput list) =
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
            let addScope (newScope : ScopeInput) =
                let newID = nextByte
                nextByte <- nextByte + 1uy
                newScope.aliases |> List.iter (fun s -> dict.Add(s, Scope(newID)))
                reverseDict.Add(Scope(newID), newScope)
            scopes |> List.iter addScope
            let addScopeSubset (newScope : ScopeInput) =
                newScope.isSubscopeOf |> List.iter (fun ss -> matchesSet <- (Set.add (parseScope() (newScope.aliases |> List.head), parseScope() ss) matchesSet))
            scopes |> List.iter addScopeSubset
            if Set.isEmpty matchesSet then () else complexEquality <- true
        member this.GetName(scope : Scope) =
            let found, value = reverseDict.TryGetValue scope
            if found
            then value.name
            else log (sprintf "Unexpected scope %O" scope.tag); ""
        member this.AllScopes = reverseDict.Keys |> List.ofSeq
        member this.AnyScope = anyScope
        member this.InvalidScope = invalidScope
        member this.ParseScope = parseScope
        member this.ParseScopes = function | "all" -> this.AllScopes | x -> [this.ParseScope() x]
        member this.ReInit(scopes : ScopeInput list) = init(scopes)
        member this.MatchesScope (source : Scope) (target : Scope) =
            if not complexEquality
            then
                match source, target with
                | x, _
                | _, x when x = anyScope-> true
                | x, y -> x = y
            else
                match Set.contains (source, target) matchesSet ,source, target with
                | true, _, _ -> true
                | _, x, _
                | _, _, x when x = anyScope -> true
                | _, x, y -> x = y
        member this.Initialized = initialized
    let scopeManager = ScopeManager()

    type ModifierCategoryInput = {
        name : string
        internalID : int option
        scopes : Scope list
    }

    type ModifierCategoryManager() =
        let mutable initialized = false
        let mutable dict = Dictionary<string, ModifierCategory>()
        let mutable reverseDict = Dictionary<ModifierCategory, ModifierCategoryInput>()
        let mutable matchesSet = Set<ModifierCategory * Scope>(Seq.empty)
        let mutable idMap = Map<int, ModifierCategory>([])
        let anyModifier = ModifierCategory(0uy)
        let anyModifierInput = { ModifierCategoryInput.name = "Any"; internalID = None; scopes = [scopeManager.AnyScope] }
        let invalidModifier = ModifierCategory(1uy)
        let invalidModifierInput = { ModifierCategoryInput.name = "Invalid"; internalID = None; scopes = []}
        let parseModifierCategory() =
            if not initialized then eprintfn "Error: parseModifierCategory was used without initializing modifier categories" else ()
            (fun (x : string) ->
            let found, value = dict.TryGetValue (x.ToLower())
            if found
            then value
            else log (sprintf "Unexpected modifier category %O" x ); anyModifier
            )
        let init(modifiers : ModifierCategoryInput list) =
            initialized <- true
            // log (sprintfn "Init scopes %A" scopes)
            dict <- Dictionary<string, ModifierCategory>()
            reverseDict <- Dictionary<ModifierCategory, ModifierCategoryInput>()
            dict.Add("any", anyModifier)
            dict.Add("invalid_modifier", invalidModifier)
            reverseDict.Add(anyModifier, anyModifierInput)
            reverseDict.Add(invalidModifier, invalidModifierInput)
            let mutable nextByte = 2uy
            let addModifier (newModifier : ModifierCategoryInput) =
                let newID = nextByte
                nextByte <- nextByte + 1uy
                let modifier = ModifierCategory(newID)
                dict.Add(newModifier.name.ToLower(), modifier)
                reverseDict.Add(modifier, newModifier)
                newModifier.scopes |> List.iter (fun s -> matchesSet <- matchesSet |> Set.add (modifier, s) )
                match newModifier.internalID with
                | Some id ->
                    idMap <- idMap |> (Map.add id modifier)
                | None -> ()
            modifiers |> List.iter addModifier

        member this.GetName(modifier : ModifierCategory) =
            let found, value = reverseDict.TryGetValue modifier
            if found
            then value.name
            else log (sprintf "Unexpected modifier category %O" modifier.tag); ""
        member this.AllModifiers = reverseDict.Keys |> List.ofSeq
        member this.AnyModifier= anyModifier
        member this.InvalidModifiere = invalidModifier
        member this.ParseModifier = parseModifierCategory
        // member this.ParseScopes = function | "all" -> this.AllScopes | x -> [this.ParseScope() x]
        member this.ReInit(modifiers : ModifierCategoryInput list) = init(modifiers)
        member this.SupportsScope (source : ModifierCategory) (target : Scope) =
            match Set.contains (source, target) matchesSet ,source, target with
            | true, _, _ -> true
            | _, x, _ when x = anyModifier -> true
            | _, _, x when x = scopeManager.AnyScope -> true
            | _ -> false
        member this.SupportedScopes (modifier : ModifierCategory) =
            let found, value = reverseDict.TryGetValue modifier
            if found
            then value.scopes
            else log (sprintf "Unexpected modifier category %O" modifier.tag); []
        member this.GetCategoryFromID (id: int) =
            match idMap |> Map.tryFind id with
            | Some category -> category
            | None -> anyModifier
        member this.Initialized = initialized
    let modifierCategoryManager = ModifierCategoryManager()
    type ModifierCategory(tag : byte) =
        member val tag = tag
        override x.ToString() = modifierCategoryManager.GetName(x)
        override x.Equals (target : obj) =
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
        member this.Name =
            modifierCategoryManager.GetName this
        interface IModifier with
            member this.Tag = modifierCategoryManager.GetName this
    type Modifier = ModifierCategory
    type Scope(tag : byte) =
        member val tag = tag
        override x.ToString() = scopeManager.GetName(x)
        override x.Equals (target : obj) =
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
            // |TradeNode, Province -> true
            | _, x
            | x, _ when x = scopeManager.AnyScope -> true
            |this, target -> scopeManager.MatchesScope this target

        interface IScope<Scope> with
            member this.AnyScope = scopeManager.AnyScope
            member this.IsOfScope target =
                match this, target with
                // |TradeNode, Province -> true
                | _, x
                | x, _ when x = scopeManager.AnyScope -> true
                |this, target -> scopeManager.MatchesScope this target

    type TypeDefInfo = {
        id : string
        validate : bool
        range : CWTools.Utilities.Position.range
        explicitLocalisation : (string * string * bool) list
        subtypes : string list
    }

type ActualModifier = {
        tag : string
        // categories : ModifierCategory list
        category : ModifierCategory
    }

type StaticModifier = {
    tag : string
    categories : ModifierCategory list
}
type EffectType = |Effect |Trigger |Link |ValueTrigger
type Effect internal (name, scopes, effectType) =
    member val Name : string = name
    member val Scopes : Scope list = scopes
    member this.ScopesSet = this.Scopes |> Set.ofList
    member val Type : EffectType = effectType
    override x.Equals(y) =
        match y with
        | :? Effect as y -> x.Name = y.Name && x.Scopes = y.Scopes && x.Type = y.Type
        |_ -> false
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Effect as y ->
                let r1 = x.Name.CompareTo(y.Name)
                if r1 = 0 then 0 else List.compareWith compare x.Scopes y.Scopes
            | _ -> invalidArg "yobj" ("cannot compare values of different types" + yobj.GetType().ToString())
    override x.ToString() = sprintf "%s: %A" x.Name x.Scopes
type ScriptedEffect(name, scopes, effectType, comments, globals, settargets, usedtargets) =
    inherit Effect(name, scopes, effectType)
    member val Comments : string = comments
    member val GlobalEventTargets : string list = globals
    member val SavedEventTargets : string list = settargets
    member val UsedEventTargets : string list = usedtargets
    override x.Equals(y) =
        match y with
        | :? ScriptedEffect as y -> x.Name = y.Name && x.Scopes = y.Scopes && x.Type = y.Type
        |_ -> false
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Effect as y -> x.Name.CompareTo(y.Name)
            | _ -> invalidArg "yobj" "cannot compare values of different types"

type DocEffect(name, scopes, target, effectType, desc, usage) =
    inherit Effect(name, scopes, effectType)
    member val Desc : string = desc
    member val Usage : string = usage
    member val Target : Scope option = target
    override x.Equals(y) =
        match y with
        | :? DocEffect as y -> x.Name = y.Name && x.Scopes = y.Scopes && x.Type = y.Type && x.Desc = y.Desc && x.Usage = y.Usage
        |_ -> false
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Effect as y -> x.Name.CompareTo(y.Name)
            | _ -> invalidArg "yobj" "cannot compare values of different types"
    new(rawEffect : RawEffect, effectType : EffectType, parseScopes) =
        let scopes = rawEffect.scopes |> List.collect parseScopes
        let target = rawEffect.targets |> List.collect parseScopes |> List.tryHead
        DocEffect(rawEffect.name, scopes, target, effectType, rawEffect.desc, rawEffect.usage)

type ScopedEffect(name, scopes, inner, effectType, desc, usage, isScopeChange, ignoreChildren, scopeonlynoteffect, isValue, isWildCard) =
    inherit DocEffect(name, scopes, inner, effectType, desc, usage)
    member val IsScopeChange : bool = isScopeChange
    member val IgnoreChildren : string list = ignoreChildren
    member val ScopeOnlyNotEffect : bool = scopeonlynoteffect
    /// If this scoped effect is a value scope
    member val IsValueScope : bool = isValue
    /// If this scoped effect is a prefix that should accept anything afterwards
    member val IsWildCard : bool = isWildCard
    new(de : DocEffect, inner, isScopeChange, ignoreChildren, scopeonlynoteffect, isValue) =
        ScopedEffect(de.Name, de.Scopes, inner, de.Type, de.Desc, de.Usage, isScopeChange, ignoreChildren, scopeonlynoteffect, isValue, false)
    new(de : DocEffect, inner) =
        ScopedEffect(de.Name, de.Scopes, inner, de.Type, de.Desc, de.Usage, true, [], false, false, false)
    new(name, scopes, inner, effectType, desc, usage, scopeonlynoteffect, isValue) =
        ScopedEffect(name, scopes, inner, effectType, desc, usage, true, [], scopeonlynoteffect, isValue, false)
    new(name, scopes, inner, effectType, desc, usage, scopeonlynoteffect) =
        ScopedEffect(name, scopes, inner, effectType, desc, usage, true, [], scopeonlynoteffect, false, false)
    new(name, scopes, inner, effectType, desc, usage, scopeonlynoteffect) =
        ScopedEffect(name, scopes, Some inner, effectType, desc, usage, true, [], scopeonlynoteffect, false, false)


// type CustomModifier =
//     {
//         tag : string
//         categories : string list
//         /// Is this a core modifier or a static modifier?
//         isCore : bool
//     }
//     interface IModifier with
//         member this.Tag = this.tag


type TitleType = |Empire |Kingdom |Duchy_Hired |Duchy_Normal |County |Barony

type DataLinkType = |Scope |Value |Both




type EventTargetDataLink = {
    name : string
    inputScopes : Scope list
    outputScope : Scope
    description : string
    dataPrefix : string option
    sourceRuleType : string
    dataLinkType : DataLinkType
}

type EventTargetLink =
| SimpleLink of ScopedEffect
| DataLink of EventTargetDataLink