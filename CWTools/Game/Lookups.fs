namespace CWTools.Games
open CWTools.Process
open FSharp.Collections.ParallelSeq
open FParsec
open System.IO
open CWTools.Parser
open CWTools.Process.STLProcess
open CWTools.Common.STLConstants
open CWTools.Common
open CWTools.Process.STLScopes
open CWTools.Parser.ConfigParser



type Lookup() =
    member val scriptedTriggers : Effect list = [] with get, set
    
    member val scriptedEffects : Effect list = [] with get, set
    member val staticModifiers : Modifier list = [] with get, set
    member val coreModifiers : Modifier list = [] with get, set
    member val definedScriptVariables : string list = [] with get, set
    member val scriptedLoc : string list = [] with get, set
    member val proccessedLoc : (Lang * Map<string, LocEntry>) list = [] with get, set
    member val technologies : (string * (string list)) list =  [] with get, set
    member val configRules : Rule list = [] with get, set
    member val typeDefInfo : Map<string, string list> = Map.empty with get, set
