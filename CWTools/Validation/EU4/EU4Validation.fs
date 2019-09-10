namespace CWTools.Validation.EU4
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Common
open CWTools.Common.EU4Constants
open CWTools.Games
open System
open FSharpx.Collections


module EU4Validation =
    type S = Severity
    let addGeneratedModifiers (modifiers : ActualModifier list) (es : EU4EntitySet) =
        let factions = es.GlobMatchChildren("**/common/factions/*.txt") |> List.map (fun f -> f.Key)
        let factionsModifierCreate =
            (fun k ->
            [
                {tag = k+"_influence"; category = modifierCategoryManager.ParseModifier() "Country" }
            ])
        let factionsModifiers = factions |> List.collect factionsModifierCreate
        factionsModifiers @ modifiers
