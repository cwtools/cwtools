namespace CWTools.Games

open CWTools.Process.Scopes.STL
open CWTools.Localisation
open CWTools.Common




type References<'T when 'T :> ComputedData>
    (resourceManager: IResourceAPI<'T>, lookup: Lookup, localisation: ILocalisationAPI seq) =

    let modifiers () =
        lookup.staticModifiers |> Array.map _.tag

    let triggers () =
        lookup.triggers |> List.map (fun t -> t.Name)

    let effects () =
        lookup.effects |> List.map (fun e -> e.Name)

    let localisation () =
        localisation
        |> Seq.collect (fun l -> l.GetEntries |> Seq.map (fun x -> (x.key, x)))
        |> Seq.toList

    member _.ModifierNames = modifiers ()
    member _.TriggerNames = triggers ()
    member _.EffectNames = effects ()
    member _.ScopeNames = oneToOneScopes |> List.map (fun (n, _) -> n)
    member _.Technologies = lookup.technologies
    member _.Localisation = localisation ()
    member _.TypeMapInfo = lookup.typeDefInfo
    member _.ConfigRules = lookup.configRules
    member _.SavedScopes = lookup.savedEventTargets
