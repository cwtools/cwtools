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

    member __.ModifierNames = modifiers ()
    member __.TriggerNames = triggers ()
    member __.EffectNames = effects ()
    member __.ScopeNames = oneToOneScopes |> List.map (fun (n, _) -> n)
    member __.Technologies = lookup.technologies
    member __.Localisation = localisation ()
    member __.TypeMapInfo = lookup.typeDefInfo
    member __.ConfigRules = lookup.configRules
    member __.SavedScopes = lookup.savedEventTargets
