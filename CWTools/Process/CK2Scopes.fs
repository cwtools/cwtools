namespace CWTools.Process

open System
open CWTools.Localisation
open CWTools.Utilities.Position
open CWTools.Common.CK2Constants
open CWTools.Common
open CWTools.Process.Scopes
module CK2Scopes =
    open CWTools.Utilities.Utils
    open Microsoft.FSharp.Collections.Tagged

    let defaultContext =
        { Root = Scope.Any; From = []; Scopes = [] }
    let noneContext =
        { Root = Scope.InvalidScope; From = []; Scopes = [Scope.InvalidScope] }
    let defaultDesc = "Scope (/context) switch"


    let scopedEffects =
        [
            ScopedEffect("owner", [Scope.Province], Scope.Character, EffectType.Both, defaultDesc, "", true);
        ]


    // type ScopeResult =
    //     | NewScope of newScope : ScopeContext<Scope> * ignoreKeys : string list
    //     | WrongScope of command : string * scope : Scope * expected : Scope list
    //     | NotFound

    let oneToOneScopes =
        let from i = fun ((s), change) -> {s with Scopes = (s.GetFrom i)::s.Scopes}, true
        let prev = fun ((s), change) -> {s with Scopes = s.PopScope}, true
        [
        "THIS", id;
        "ROOT", fun ((s), change) -> {s with Scopes = s.Root::s.Scopes}, true;
        "FROM", from 1;
        // "FROMFROM", from 2;
        // "FROMFROMFROM", from 3;
        // "FROMFROMFROMFROM", from 4;
        "PREV", prev;
        // "PREVPREV", prev >> prev;
        // "PREVPREVPREV", prev >> prev >> prev;
        // "PREVPREVPREVPREV", prev >> prev >> prev >> prev
        "AND", id;
        "OR", id;
        "NOR", id;
        "NOT", id;
        "hidden_effect", id;
        "hidden_trigger", id;
    ]
    let oneToOneScopesNames = List.map fst oneToOneScopes
    type EffectMap = Map<string, Effect, InsensitiveStringComparer>
    let changeScope = createChangeScope<Scope> oneToOneScopes (complexVarPrefixFun "variable:from:" "variable:")


    let scopedLocEffects = [
        // ScopedEffect("Capital", [Scope.Country], Scope.Province, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("ColonialParent", [Scope.Country], Scope.Country, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Culture", [Scope.Country; Scope.Province; Scope.RebelFaction], Scope.Culture, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Dynasty", [Scope.Consort; Scope.Monarch; Scope.Heir], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Heir", [Scope.Country], Scope.Heir, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Location", [Scope.RebelFaction], Scope.Any, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Monarch", [Scope.Country], Scope.Monarch, EffectType.Both, defaultDesc, "", true);
        ScopedEffect("Owner", [Scope.Province], Scope.Character, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Religion", [Scope.Country; Scope.Province; Scope.RebelFaction], Scope.Religion, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("SecondaryReligion", [Scope.Country], Scope.Religion, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("TradeCompany", [Scope.Country; Scope.Province], Scope.Country, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Dip_Advisor", [Scope.Country], Scope.Advisor, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Adm_Advisor", [Scope.Country], Scope.Advisor, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Mil_Advisor", [Scope.Country], Scope.Advisor, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Country", [Scope.Country; Scope.RebelFaction], Scope.Country, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Province", [Scope.Any], Scope.Province, EffectType.Both, defaultDesc, "", true);
        // ScopedEffect("Overlord", [Scope.Country], Scope.Country, EffectType.Both, defaultDesc, "", true);
    ]
    let scopedLocEffectsMap = EffectMap.FromList(InsensitiveStringComparer(), scopedLocEffects |> List.map (fun se -> se.Name, se :> Effect))


    let locPrimaryScopes =
        let from = fun (s, change) -> {s with Scopes = Scope.Any::s.Scopes}, true
        [
        "This", id;
        "Root", fun (s, change) -> {s with Scopes = s.Root::s.Scopes}, true;
        "From", from; //TODO Make it actually use FROM
        "FromFrom", from >> from;
        "FromFromFrom", from >> from >> from;
        "FromFromFromFrom", from >> from >> from >> from;
        ]

    let localisationCommandValidator = createLocalisationCommandValidator locPrimaryScopes scopedLocEffectsMap
