namespace CWTools.Games
open CWTools.Process
open FSharp.Collections.ParallelSeq
open FParsec
open System.IO
open CWTools.Parser
open CWTools.Process.STLProcess
open CWTools.Common.STLConstants

type Lookup() =
    member val scriptedTriggers : Effect list = [] with get, set
    
    member val scriptedEffects : Effect list = [] with get, set
    member val staticModifiers : Modifier list = [] with get, set
    member val definedScriptVariables : string list = [] with get, set
