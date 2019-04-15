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
    let addGeneratedModifiers (modifiers : Modifier list) (es : EU4EntitySet) =
        let factions = es.GlobMatchChildren("**/common/factions/*.txt") |> List.map (fun f -> f.Key)
        let factionsModifierCreate =
            (fun k ->
            [
                {tag = k+"_influence"; categories = [ModifierCategory.Country]; core = true }
            ])
        let factionsModifiers = factions |> List.collect factionsModifierCreate
        factionsModifiers @ modifiers

    let getScriptedEffectParams (node : Node) =
        let getDollarText (s : string) (acc) =
            let split = s.Split([|'$'|],3)
            if split.Length = 3 then split.[1]::acc else acc
        let fNode = (fun (x:Node) acc ->
                        let nodeRes = getDollarText x.Key acc
                        x.Values |> List.fold (fun a n -> getDollarText n.Key (getDollarText (n.Value.ToRawString()) a)) nodeRes
                        )
        node |> (foldNode7 fNode) |> List.ofSeq

    let getScriptedEffectParamsEntity (e : Entity) =
        if (e.logicalpath.StartsWith("common/scripted_effects", StringComparison.OrdinalIgnoreCase)
                    || e.logicalpath.StartsWith("common/scripted_triggers", StringComparison.OrdinalIgnoreCase))
                then getScriptedEffectParams (e.entity) else []