namespace CWTools.Games

open CWTools.Common
open CWTools.Rules
open CWTools.Process.Scopes
open CWTools.Utilities.Position
open Files
open CWTools.Process.Localisation



type Lookup() =

    let mutable _allCoreLinks: Effect list = []

    let getTriggers () =
        _allCoreLinks
        |> List.filter (fun l -> l.Type = EffectType.Trigger || l.Type = EffectType.ValueTrigger)

    let mutable _triggersMap: Lazy<EffectMap> = lazy (EffectMap())

    let resetTriggers () =
        _triggersMap <- lazy (getTriggers () |> EffectMap.FromList)

    let getEffects () =
        _allCoreLinks |> List.filter (fun l -> l.Type = EffectType.Effect)

    let mutable _effectsMap: Lazy<EffectMap> = lazy EffectMap()

    let resetEffects () =
        _effectsMap <- lazy (getEffects () |> (fun l -> EffectMap.FromList(l)))

    let getEventTargetLinks () =
        _allCoreLinks |> List.filter (fun l -> l.Type = EffectType.Link)

    let mutable _eventTargetLinksMap: Lazy<EffectMap> = lazy EffectMap()

    let resetEventTargetLinks () =
        _eventTargetLinksMap <- lazy (getEventTargetLinks () |> EffectMap.FromList)

    let getValueTriggers () =
        _allCoreLinks |> List.filter (fun l -> l.Type = EffectType.ValueTrigger)

    let mutable _valueTriggersMap: Lazy<EffectMap> = lazy EffectMap()

    let resetValueTriggers () =
        _valueTriggersMap <- lazy (getValueTriggers () |> EffectMap.FromList)

    member _.allCoreLinks
        with get () = _allCoreLinks
        and set value =
            _allCoreLinks <- value
            resetTriggers ()
            resetEffects ()
            resetEventTargetLinks ()
            resetValueTriggers ()

    member _.triggers = getTriggers ()
    member this.triggersMap = _triggersMap.Force()
    member _.effects = getEffects ()
    member this.effectsMap = _effectsMap.Force()
    member _.eventTargetLinks = getEventTargetLinks ()
    member this.eventTargetLinksMap = _eventTargetLinksMap.Force()
    member _.valueTriggers = getValueTriggers ()
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
    member val typeDefInfo: Map<string, TypeDefInfo array> = Map.empty with get, set
    member val typeDefInfoForValidation: Map<string, struct (string * range) array> = Map.empty with get, set
    member val varDefInfo: Map<string, (string * range) array> = Map.empty with get, set
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
