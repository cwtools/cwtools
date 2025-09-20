namespace CWTools.Games

open CWTools.Process.Scopes.STL
open CWTools.Localisation
open CWTools.Common




type References<'T when 'T :> ComputedData>
    (resourceManager: IResourceAPI<'T>, lookup: Lookup, localisation: ILocalisationAPI list) =
    let entities () =
        resourceManager.AllEntities() |> List.map (fun struct (e, _) -> e.entity)

    let modifiers () =
        lookup.staticModifiers |> List.map (fun m -> m.tag)

    let triggers () =
        lookup.triggers |> List.map (fun t -> t.Name)

    let effects () =
        lookup.effects |> List.map (fun e -> e.Name)

    let localisation () =
        localisation |> List.collect (fun l -> l.ValueMap |> Map.toList)

    member _.ModifierNames = modifiers ()
    member _.TriggerNames = triggers ()
    member _.EffectNames = effects ()
    member _.ScopeNames = oneToOneScopes |> List.map (fun (n, _) -> n)
    member _.Technologies = lookup.technologies
    member _.Localisation = localisation ()
    member _.TypeMapInfo = lookup.typeDefInfo
    member _.ConfigRules = lookup.configRules
    member _.SavedScopes = lookup.savedEventTargets
