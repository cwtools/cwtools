namespace CWTools.Validation.EU4
open CWTools.Validation.ValidationCore
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser.Types
open CWTools.Process.EU4Scopes
open CWTools.Common
open CWTools.Common.EU4Constants
open DotNet.Globbing
open CWTools.Games
open Newtonsoft.Json.Linq
open CWTools.Utilities.Utils
open System
open Microsoft.FSharp.Collections.Tagged
open System.Collections
open System.Threading.Tasks
open FSharp.Collections.ParallelSeq
open System.Globalization
open CWTools.Process.Scopes
open FSharpx.Collections


module EU4Validation =
    type S = Severity
    let addGeneratedModifiers (modifiers : Modifier list) (es : EU4EntitySet) =
        let factions = es.GlobMatchChildren("**/common/factions/*.txt") |> List.map (fun f -> f.Key)
        let factionsModifierCreate =
            (fun k ->
            [
                {tag = k+"_influence"; categories = [ModifierCategory.Country]; core = true }
            ])
        let factionsModifiers = factions |> List.collect factionsModifierCreate
        factionsModifiers @ modifiers
