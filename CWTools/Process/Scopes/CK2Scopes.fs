namespace CWTools.Process.Scopes

open CWTools.Common.CK2Constants
open CWTools.Common
open CWTools.Process.Scopes
module CK2 =
    open CWTools.Utilities.Utils
    open Microsoft.FSharp.Collections.Tagged

    let defaultContext =
        { Root = Scope.Any; From = []; Scopes = [] }
    let noneContext =
        { Root = Scope.InvalidScope; From = []; Scopes = [Scope.InvalidScope] }
    let defaultDesc = "Scope (/context) switch"


    let scopedEffects =
        [
            // To title
            ScopedEffect("primary_title", [Scope.Character], Scope.Title, EffectType.Link, defaultDesc, "", true)
            // To character
            ScopedEffect("mother", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true)
            ScopedEffect("mother_even_if_dead", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true)
            ScopedEffect("father", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true)
            ScopedEffect("father_even_if_dead", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true)
            ScopedEffect("killer", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true)
            ScopedEffect("liege", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true)
            ScopedEffect("liege_before_war", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true)
            ScopedEffect("top_liege", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true)
            // To province
            ScopedEffect("capital_scope", [Scope.Character; Scope.Title], Scope.Province, EffectType.Link, defaultDesc, "", true)
            ScopedEffect("owner", [Scope.Province], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ]


    // type ScopeResult =
    //     | NewScope of newScope : ScopeContext<Scope> * ignoreKeys : string list
    //     | WrongScope of command : string * scope : Scope * expected : Scope list
    //     | NotFound

    let oneToOneScopes =
        let from i = fun ((s), change) -> {s with Scopes = (s.GetFrom i)::s.Scopes}, true
        let prev = fun ((s), change) -> {s with Scopes = s.PopScope}, true
        let root = fun ((s), change) -> {s with Scopes = s.Root::s.Scopes}, true
        [
        "THIS", id;
        "ROOT", root;
        "ROOT_FROM", root >> from 1;
        "ROOT_FROMFROM", root >> from 2;
        "ROOT_FROMFROMFROM", root >> from 3;
        "ROOT_FROMFROMFROMFROM", root >> from 4;
        "FROM", from 1;
        "FROMFROM", from 2;
        "FROMFROMFROM", from 3;
        "FROMFROMFROMFROM", from 4;
        "PREV", prev;
        "PREVPREV", prev >> prev;
        "PREVPREVPREV", prev >> prev >> prev;
        "PREVPREVPREVPREV", prev >> prev >> prev >> prev
        "AND", id;
        "OR", id;
        "NOR", id;
        "NOT", id;
        "hidden_effect", id;
        "hidden_trigger", id;
    ]
    let oneToOneScopesNames = List.map fst oneToOneScopes
    type EffectMap = Map<string, Effect, InsensitiveStringComparer>
    let changeScope = Scopes.createChangeScope<Scope> oneToOneScopes (Scopes.complexVarPrefixFun "variable:from:" "variable:")


    let scopedLocEffects = [
        ScopedEffect("Owner", [Scope.Province], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Betrothed", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Culture", [Scope.Character], Scope.Culture, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Employer", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Father", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("FatherOfUnborn", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("GetLiege", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Governor", [Scope.Offmap], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("GovernorTitle", [Scope.Offmap], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("GrandMaster", [Scope.Society], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Guardian", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Heir", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Holder", [Scope.Title], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("job_chancellor", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("job_marshal", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("job_spiritual", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("job_spymaster", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("job_treasurer", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Killer", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Liege", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Location", [Scope.Title], Scope.Province, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Lover", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Mother", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("NextGrandMaster", [Scope.Society], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Offmap", [Scope.Province], Scope.Offmap, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("OldestChild", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("OriginalOwner", [Scope.Artifact; Scope.Wonder], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Owner", [Scope.Artifact; Scope.Wonder], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("ParentReligion", [Scope.Character], Scope.Religion, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("PlotTarget", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("PlotTargetTitle", [Scope.Character], Scope.Title, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("PrevRuler", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Province", [Scope.Wonder], Scope.Province, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("PrimaryTitle", [Scope.Character], Scope.Title, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("RealFather", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Realm", [Scope.Character], Scope.Title, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Regent", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Reincarnation", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Religion", [Scope.Province; Scope.Character], Scope.Religion, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Ruler", [Scope.Offmap], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("SecretReligion", [Scope.Character], Scope.Religion, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Society", [Scope.Character], Scope.Society, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Spouse", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("SupportedClaimant", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("SupportedClaimantTitle", [Scope.Character], Scope.Title, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("ThePope", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("ThirdPartyCharacter", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("ThirdPartyTitle", [Scope.Character], Scope.Title, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Title", [Scope.Character], Scope.Title, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("TopLiege", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("TrueFather", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("TrueReligion", [Scope.Character], Scope.Religion, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Twin", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Wonder", [Scope.Province], Scope.Wonder, EffectType.Link, defaultDesc, "", true);

        // TODO: Check these as
        ScopedEffect("Actor", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("ActorExtra", [Scope.Province], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("AdultExtra", [Scope.Province], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("ChildExtra", [Scope.Province], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Clan", [Scope.Province], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("County", [Scope.Province], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Capital", [Scope.Character], Scope.Province, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Duchy", [Scope.Province], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Government", [Scope.Character], Scope.AnyScope, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Host", [Scope.Title], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Kingdom", [Scope.Character], Scope.Title, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Player", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Recipient", [Scope.Character], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("RecipientExtra", [Scope.Province], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("RelHead", [Scope.Religion], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("SeaZone", [Scope.Province], Scope.Character, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Siege", [Scope.Province], Scope.Siege, EffectType.Link, defaultDesc, "", true);
    ]
    let scopedLocEffectsMap = EffectMap.FromList(InsensitiveStringComparer(), scopedLocEffects |> List.map (fun se -> se.Name, se :> Effect))


    let locPrimaryScopes =
        let from = fun (s : ScopeContext<_>, change) -> {s with Scopes = Scope.Any::s.Scopes}, true
        [
        "This", id;
        "Root", fun (s : ScopeContext<_>, change) -> {s with Scopes = s.Root::s.Scopes}, true;
        "From", from; //TODO Make it actually use FROM
        "FromFrom", from >> from;
        "FromFromFrom", from >> from >> from;
        "FromFromFromFrom", from >> from >> from >> from;
        ]

    let localisationCommandValidator = Scopes.createLocalisationCommandValidator locPrimaryScopes scopedLocEffectsMap
