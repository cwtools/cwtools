namespace CWTools.Games
open CWTools.Process
open FSharp.Collections.ParallelSeq
open FParsec
open System.IO
open CWTools.Parser
open CWTools.Process.STLProcess
open CWTools.Common.STLConstants
open CWTools.Common
// open CWTools.Process.STLScopes
open CWTools.Parser.ConfigParser
open CWTools.Process.Scopes
open CWTools.Utilities.Position
open CWTools.Utilities.Utils
open Microsoft.FSharp.Collections.Tagged



type Lookup<'S, 'M when 'S : comparison and 'S :> IScope<'S> and 'M :> IModifier>() =
    let mutable _triggers : Effect<'S> list = []

    let mutable _triggersMap : Lazy<Map<string,Effect<'S>,InsensitiveStringComparer>> = lazy ( Map<string,Effect<'S>,InsensitiveStringComparer>.Empty (InsensitiveStringComparer()) )
    let resetTriggers() =
        _triggersMap <- lazy (_triggers|> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap<'S>.FromList(InsensitiveStringComparer(), l)))

    let mutable _effects : Effect<'S> list = []

    let mutable _effectsMap : Lazy<Map<string,Effect<'S>,InsensitiveStringComparer>> = lazy ( Map<string,Effect<'S>,InsensitiveStringComparer>.Empty (InsensitiveStringComparer()) )
    let resetEffects() =
        _effectsMap <- lazy (_effects|> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap<'S>.FromList(InsensitiveStringComparer(), l)))
    let mutable _eventTargetLinks : Effect<'S> list = []

    let mutable _eventTargetLinksMap : Lazy<Map<string,Effect<'S>,InsensitiveStringComparer>> = lazy ( Map<string,Effect<'S>,InsensitiveStringComparer>.Empty (InsensitiveStringComparer()) )
    let resetEventTargetLinks() =
        _eventTargetLinksMap <- lazy (_eventTargetLinks|> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap<'S>.FromList(InsensitiveStringComparer(), l)))
    member __.triggers
        with get () = _triggers
        and set (value) = resetTriggers(); _triggers <- value
    member this.triggersMap with get() = _triggersMap.Force()
    member __.effects
        with get () = _effects
        and set (value) = resetEffects(); _effects <- value
    member this.effectsMap with get() = _effectsMap.Force()
    member __.eventTargetLinks
        with get () = _eventTargetLinks
        and set (value) =  _eventTargetLinks <- value; resetEventTargetLinks();
    member this.eventTargetLinksMap with get() = _eventTargetLinksMap.Force()
    member val onlyScriptedEffects : Effect<'S> list = [] with get, set
    member val onlyScriptedTriggers : Effect<'S> list = [] with get, set

    member val rootFolder : string = "" with get, set
    member val staticModifiers : 'M list = [] with get, set
    member val coreModifiers : 'M list = [] with get, set
    member val HOI4provinces : string list = [] with get, set
    member val EU4ScriptedEffectKeys : string list = [] with get, set
    member val EU4TrueLegacyGovernments : string list = [] with get, set
    member val CK2LandedTitles : Collections.Map<TitleType * bool, string list> = Map.empty with get, set // Title * landless
    member val CK2provinces : string list = [] with get, set
    member val IRprovinces : string list = [] with get, set
    member val definedScriptVariables : string list = [] with get, set
    member val scriptedLoc : string list = [] with get, set
    member val proccessedLoc : (Lang * Collections.Map<string, LocEntry<'S>>) list = [] with get, set
    member val technologies : (string * (string list)) list =  [] with get, set
    member val configRules : RootRule<'S> list = [] with get, set
    member val typeDefs : TypeDefinition<'S> list = [] with get, set
    /// Map<enum key, (description * values list)
    member val enumDefs : Collections.Map<string, string * string list> = Map.empty with get, set
    member val typeDefInfoRaw : Collections.Map<string, (bool * string * range) list> = Map.empty with get, set
    member this.typeDefInfo
        with get () : Collections.Map<string, (string * range) list> = this.typeDefInfoRaw |> Collections.Map.map (fun _ v -> v |> List.map (fun (_, t, r) -> (t, r)))
    member val typeDefInfoForValidation : Collections.Map<string, (string * range) list> = Map.empty with get, set
    member val varDefInfo : Collections.Map<string, (string * range) list> = Map.empty with get, set
    member val globalScriptedVariables : string list = [] with get, set
