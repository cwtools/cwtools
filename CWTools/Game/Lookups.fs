namespace CWTools.Games

open CWTools.Common
open CWTools.Rules
open CWTools.Process.Scopes
open CWTools.Utilities.Position
open Files
open CWTools.Process.Localisation



type Lookup() =

    let mutable _allCoreLinks: Effect list = []

    let _triggers () =
        _allCoreLinks
        |> List.filter (fun l -> l.Type = EffectType.Trigger || l.Type = EffectType.ValueTrigger)

    let mutable _triggersMap: Lazy<EffectMap> = lazy (EffectMap())

    let resetTriggers () =
        _triggersMap <- lazy (_triggers () |> EffectMap.FromList)

    let _effects () =
        _allCoreLinks |> List.filter (fun l -> l.Type = EffectType.Effect)

    let mutable _effectsMap: Lazy<EffectMap> = lazy EffectMap()

    let resetEffects () =
        _effectsMap <- lazy (_effects () |> (fun l -> EffectMap.FromList(l)))

    let _eventTargetLinks () =
        _allCoreLinks |> List.filter (fun l -> l.Type = EffectType.Link)

    let mutable _eventTargetLinksMap: Lazy<EffectMap> = lazy EffectMap()

    let resetEventTargetLinks () =
        _eventTargetLinksMap <- lazy (_eventTargetLinks () |> EffectMap.FromList)

    let _valueTriggers () =
        _allCoreLinks |> List.filter (fun l -> l.Type = EffectType.ValueTrigger)

    let mutable _valueTriggersMap: Lazy<EffectMap> = lazy EffectMap()

    let resetValueTriggers () =
        _valueTriggersMap <- lazy (_valueTriggers () |> EffectMap.FromList)

    member __.allCoreLinks
        with get () = _allCoreLinks
        and set value =
            _allCoreLinks <- value
            resetTriggers ()
            resetEffects ()
            resetEventTargetLinks ()
            resetValueTriggers ()

    member __.triggers = _triggers ()
    member this.triggersMap = _triggersMap.Force()
    member __.effects = _effects ()
    member this.effectsMap = _effectsMap.Force()
    member __.eventTargetLinks = _eventTargetLinks ()
    member this.eventTargetLinksMap = _eventTargetLinksMap.Force()
    member __.valueTriggers = _valueTriggers ()
    member this.valueTriggerMap = _valueTriggersMap.Force()
    member val onlyScriptedEffects: Effect list = [] with get, set
    member val onlyScriptedTriggers: Effect list = [] with get, set

    member val rootFolders: WorkspaceDirectoryInput list = [] with get, set
    member val staticModifiers: StaticModifier list = [] with get, set
    member val coreModifiers: ActualModifier list = [] with get, set
    member val definedScriptVariables: string list = [] with get, set
    member val embeddedScriptedLoc: string list = [] with get, set
    member val _realScriptedLoc: string list = [] with get, set
    member this.scriptedLoc = this.embeddedScriptedLoc @ this._realScriptedLoc

    member this.scriptedLoc
        with set value = this._realScriptedLoc <- value

    member val proccessedLoc: (Lang * Collections.Map<string, LocEntry>) list = [] with get, set
    member val technologies: (string * string list) list = [] with get, set
    member val configRules: RootRule array = [||] with get, set
    member val typeDefs: TypeDefinition list = [] with get, set
    /// Map<enum key, (description * values list)
    member val enumDefs: Map<string, string * (string * range option) array> = Map.empty with get, set
    member val typeDefInfo: Collections.Map<string, TypeDefInfo array> = Map.empty with get, set
    member val typeDefInfoForValidation: Collections.Map<string, (string * range) list> = Map.empty with get, set
    member val varDefInfo: Collections.Map<string, (string * range) array> = Map.empty with get, set
    member val savedEventTargets: ResizeArray<string * range * Scope> = new ResizeArray<_>() with get, set

type JominiLookup() =
    inherit Lookup()
    member val ScriptedEffectKeys: string list = [] with get, set

type CK2Lookup() =
    inherit Lookup()
    member val CK2LandedTitles: Collections.Map<TitleType * bool, string list> = Map.empty with get, set // Title * landless
    member val CK2provinces: string array = [||] with get, set

type EU4Lookup() =
    inherit Lookup()
    member val EU4ScriptedEffectKeys: string array = [||] with get, set
    member val EU4TrueLegacyGovernments: string array = [||] with get, set

type HOI4Lookup() =
    inherit Lookup()
    member val HOI4provinces: string array = [||] with get, set

type STLLookup() =
    inherit Lookup()

type IRLookup() =
    inherit JominiLookup()
    member val IRprovinces: string array = [||] with get, set
    member val IRcharacters: string array = [||] with get, set

type VIC2Lookup() =
    inherit Lookup()
    member val VIC2provinces: string array = [||] with get, set
