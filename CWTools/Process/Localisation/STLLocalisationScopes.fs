namespace CWTools.Process.Localisation
open CWTools.Process.Scopes
open CWTools.Common
open CWTools.Utilities.Utils
open CWTools.Process.Scopes.Scopes
open CWTools.Process.Localisation.ChangeLocScope

module STL =

    let scopedLocEffects() : ScopedEffect list = [
        // ScopedEffect("capital", scopeManager.AllScopes, scopeManager.ParseScope() "Planet", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("capital_scope", scopeManager.AllScopes, scopeManager.ParseScope() "Planet", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("system", scopeManager.AllScopes, scopeManager.ParseScope() "GalacticObject", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("solar_system", scopeManager.AllScopes, scopeManager.ParseScope() "GalacticObject", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("star", [scopeManager.ParseScope() "GalacticObject"], scopeManager.ParseScope() "Star", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("owner", [scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Pop"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "PopFaction"; scopeManager.ParseScope() "Sector"; scopeManager.ParseScope() "Leader"; scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Starbase"; scopeManager.ParseScope() "GalacticObject"], scopeManager.ParseScope() "Country", EffectType.Link, "", "", true);
        // ScopedEffect("planet", [scopeManager.ParseScope() "Pop"; scopeManager.ParseScope() "Planet"], scopeManager.ParseScope() "Planet", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("leader", [scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "PopFaction"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "Sector"], scopeManager.ParseScope() "Leader", EffectType.Link, "", "", true);
        // ScopedEffect("species", [scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Leader"; scopeManager.ParseScope() "Pop"], scopeManager.ParseScope() "Species", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("fleet", [scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Starbase"], scopeManager.ParseScope() "Fleet", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("ship", [scopeManager.ParseScope() "Leader"], scopeManager.ParseScope() "Ship", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("ruler", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Leader", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("sector", [scopeManager.ParseScope() "Planet"; scopeManager.ParseScope() "GalacticObject"], scopeManager.ParseScope() "Sector", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("starbase", [scopeManager.ParseScope() "GalacticObject"; scopeManager.ParseScope() "Planet"], scopeManager.ParseScope() "Starbase", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("home_planet", [scopeManager.ParseScope() "Country"; scopeManager.ParseScope() "Species"; scopeManager.ParseScope() "Planet"], scopeManager.ParseScope() "Planet", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("overlord", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("space_owner", [scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "GalacticObject"; scopeManager.ParseScope() "Planet"], scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
        // ScopedEffect("federation", [scopeManager.ParseScope() "Country"], scopeManager.ParseScope() "Federation", EffectType.Link, defaultDesc, "", true);
        ]
    let scopedLocEffectsMap() = EffectMap.FromList(InsensitiveStringComparer(), scopedLocEffects() |> List.map (fun se -> se.Name, se :> Effect))


    let locPrimaryScopes() =
        let from = fun (s, change) -> {s with Scopes = scopeManager.AnyScope::s.Scopes}, true
        let prev = fun (s, change) -> {s with Scopes = s.PopScope}, true
        [
        "THIS", id;
        "ROOT", fun (s, change) -> {s with Scopes = s.Root::s.Scopes}, true;
        "FROM", from; //TODO Make it actually use FROM
        "FROMFROM", from >> from;
        "FROMFROMFROM", from >> from >> from;
        "FROMFROMFROMFROM", from >> from >> from >> from;
        "PREV", prev;
        "PREVPREV", prev >> prev;
        "PREVPREVPREV", prev >> prev >> prev;
        "PREVPREVPREVPREV", prev >> prev >> prev >> prev
        "Recipient", id;
        "Actor", id;
        "Third_party", id;
        ]
    let locStaticSettings commands variableCommands (localisationLinks : (string * Scope list * Scope) list, scopeLinks : EventTargetLink list) =
        let scopedLocEffects =
                localisationLinks |> List.map (fun (key, inputs, outputs) ->
                    ScopedEffect(key, inputs, outputs, EffectType.Link, defaultDesc, "", true))
        let eventTargetLinks =
            scopeLinks |> List.choose (function |EventTargetLink.SimpleLink se -> Some se |_ -> None)
        let scopedLocEffectsMap =
            if localisationLinks |> List.isEmpty
            then scopedLocEffectsMap()
            else
                let locLinks = (scopedLocEffects @ eventTargetLinks) |> List.map (fun se -> se.Name, se :> Effect)
                EffectMap.FromList(InsensitiveStringComparer(), locLinks)
        {
            questionMarkVariable = true
            usesVariableCommands = false
            parameterVariables = true
            locPrimaryScopes = locPrimaryScopes()
            scopedLocEffectsMap = scopedLocEffectsMap
            commands = commands
            variableCommands = variableCommands
        }
    // let localisationCommandValidator commands variableCommands = createLegacyLocalisationCommandValidator (locStaticSettings commands variableCommands)
