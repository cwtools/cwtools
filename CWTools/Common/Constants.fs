namespace CWTools.Common

type Game = |CK2 = 0 |HOI4 = 1 |EU4 = 2 |STL = 3 |VIC2 = 4
type CK2Lang = |English = 0 |French = 1 |German = 2 |Spanish = 3 |Russian = 4 |Default = 5
type STLLang = |English = 0 |French = 1 |German = 2 |Spanish = 3 |Russian = 4 |Polish = 5 |Braz_Por = 6  |Default = 7 |Chinese = 8
type HOI4Lang = |English = 0 |French = 1 |German = 2 |Spanish = 3 |Russian = 4 |Polish = 5 |Braz_Por = 6 |Default = 7 //Default doesnt' exist!
type EU4Lang = |English = 0 |French = 1 |German = 2 |Spanish = 3 |Default = 4
type IRLang = |English = 0 |French = 1 |German = 2 |Spanish = 3 |Chinese = 4 |Russian = 5
type VIC2Lang = |English = 0 |French = 1 |German = 2 |Spanish = 3
type Lang =
    |CK2 of CK2Lang
    |STL of STLLang
    |HOI4 of HOI4Lang
    |EU4 of EU4Lang
    |IR of IRLang
    |VIC2 of VIC2Lang
    override x.ToString() = x |> function |CK2 c -> c.ToString() | STL s -> s.ToString() |HOI4 s -> s.ToString() |EU4 s -> s.ToString() |IR s -> s.ToString() |VIC2 s -> s.ToString()

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




type EffectType = |Effect |Trigger |Link |ValueTrigger
type Effect<'T when 'T : comparison> internal (name, scopes, effectType) =
    member val Name : string = name
    member val Scopes : 'T list = scopes
    member this.ScopesSet = this.Scopes |> Set.ofList
    member val Type : EffectType = effectType
    override x.Equals(y) =
        match y with
        | :? Effect<'T> as y -> x.Name = y.Name && x.Scopes = y.Scopes && x.Type = y.Type
        |_ -> false
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Effect<'T> as y ->
                let r1 = x.Name.CompareTo(y.Name)
                if r1 = 0 then 0 else List.compareWith compare x.Scopes y.Scopes
            | _ -> invalidArg "yobj" ("cannot compare values of different types" + yobj.GetType().ToString())
    override x.ToString() = sprintf "%s: %A" x.Name x.Scopes
type ScriptedEffect<'T when 'T : comparison>(name, scopes, effectType, comments, globals, settargets, usedtargets) =
    inherit Effect<'T>(name, scopes, effectType)
    member val Comments : string = comments
    member val GlobalEventTargets : string list = globals
    member val SavedEventTargets : string list = settargets
    member val UsedEventTargets : string list = usedtargets
    override x.Equals(y) =
        match y with
        | :? ScriptedEffect<'T> as y -> x.Name = y.Name && x.Scopes = y.Scopes && x.Type = y.Type
        |_ -> false
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Effect<'T> as y -> x.Name.CompareTo(y.Name)
            | _ -> invalidArg "yobj" "cannot compare values of different types"

type DocEffect<'T when 'T : comparison>(name, scopes, target, effectType, desc, usage) =
    inherit Effect<'T>(name, scopes, effectType)
    member val Desc : string = desc
    member val Usage : string = usage
    member val Target : 'T option = target
    override x.Equals(y) =
        match y with
        | :? DocEffect<'T> as y -> x.Name = y.Name && x.Scopes = y.Scopes && x.Type = y.Type && x.Desc = y.Desc && x.Usage = y.Usage
        |_ -> false
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Effect<'T> as y -> x.Name.CompareTo(y.Name)
            | _ -> invalidArg "yobj" "cannot compare values of different types"
    new(rawEffect : RawEffect, effectType : EffectType, parseScopes) =
        let scopes = rawEffect.scopes |> List.collect parseScopes
        let target = rawEffect.targets |> List.collect parseScopes |> List.tryHead
        DocEffect<'T>(rawEffect.name, scopes, target, effectType, rawEffect.desc, rawEffect.usage)

type ScopedEffect<'T when 'T : comparison>(name, scopes, inner, effectType, desc, usage, isScopeChange, ignoreChildren, scopeonlynoteffect, isValue, isWildCard) =
    inherit DocEffect<'T>(name, scopes, inner, effectType, desc, usage)
    member val IsScopeChange : bool = isScopeChange
    member val IgnoreChildren : string list = ignoreChildren
    member val ScopeOnlyNotEffect : bool = scopeonlynoteffect
    /// If this scoped effect is a value scope
    member val IsValueScope : bool = isValue
    /// If this scoped effect is a prefix that should accept anything afterwards
    member val IsWildCard : bool = isWildCard
    new(de : DocEffect<'T>, inner : 'T option, isScopeChange, ignoreChildren, scopeonlynoteffect, isValue) =
        ScopedEffect<'T>(de.Name, de.Scopes, inner, de.Type, de.Desc, de.Usage, isScopeChange, ignoreChildren, scopeonlynoteffect, isValue, false)
    new(de : DocEffect<'T>, inner : 'T option) =
        ScopedEffect<'T>(de.Name, de.Scopes, inner, de.Type, de.Desc, de.Usage, true, [], false, false, false)
    new(name, scopes, inner, effectType, desc, usage, scopeonlynoteffect, isValue) =
        ScopedEffect<'T>(name, scopes, inner, effectType, desc, usage, true, [], scopeonlynoteffect, isValue, false)
    new(name, scopes, inner, effectType, desc, usage, scopeonlynoteffect) =
        ScopedEffect<'T>(name, scopes, inner, effectType, desc, usage, true, [], scopeonlynoteffect, false, false)
    new(name, scopes, inner, effectType, desc, usage, scopeonlynoteffect) =
        ScopedEffect<'T>(name, scopes, Some inner, effectType, desc, usage, true, [], scopeonlynoteffect, false, false)

type IScope<'T> =
    abstract member AnyScope : 'T
    /// The first value is or can be coerced to the second
    abstract member MatchesScope : 'T -> bool

type IModifier =
    abstract member Tag : string

type TitleType = |Empire |Kingdom |Duchy_Hired |Duchy_Normal |County |Barony

type DataLinkType = |Scope |Value |Both
type EventTargetDataLink<'S> = {
    name : string
    inputScopes : 'S list
    outputScope : 'S
    description : string
    dataPrefix : string option
    sourceRuleType : string
    dataLinkType : DataLinkType
}

type EventTargetLink<'S when 'S : comparison> =
| SimpleLink of ScopedEffect<'S>
| DataLink of EventTargetDataLink<'S>
