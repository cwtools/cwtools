namespace CWTools.Process.Scopes

open CWTools.Common.CK2Constants
open CWTools.Common
open CWTools.Process.Scopes
open CWTools.Common.NewScope
module CK2 =
    open CWTools.Utilities.Utils
    open Microsoft.FSharp.Collections.Tagged

    let defaultDesc = "Scope (/context) switch"


    let scopedEffects() =
        [
            // To title
            ScopedEffect("primary_title", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Title", EffectType.Link, defaultDesc, "", true)
            // To character
            ScopedEffect("mother", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true)
            ScopedEffect("mother_even_if_dead", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true)
            ScopedEffect("father", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true)
            ScopedEffect("father_even_if_dead", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true)
            ScopedEffect("killer", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true)
            ScopedEffect("liege", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true)
            ScopedEffect("liege_before_war", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true)
            ScopedEffect("top_liege", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true)
            // To province
            ScopedEffect("capital_scope", [scopeManager.ParseScope() "Character"; scopeManager.ParseScope() "Title"], scopeManager.ParseScope() "Province", EffectType.Link, defaultDesc, "", true)
            ScopedEffect("owner", [scopeManager.ParseScope() "Province"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
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
    ]
    let oneToOneScopesNames = List.map fst oneToOneScopes
    type EffectMap = Map<string, Effect, InsensitiveStringComparer>
    let changeScope = Scopes.createChangeScope oneToOneScopes (Scopes.complexVarPrefixFun "variable:from:" "variable:")


    let scopedLocEffects() = [
        ScopedEffect("Owner", [scopeManager.ParseScope() "Province"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Betrothed", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Culture", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Culture", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Employer", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Father", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("FatherOfUnborn", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("GetLiege", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Governor", [scopeManager.ParseScope() "Offmap"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("GovernorTitle", [scopeManager.ParseScope() "Offmap"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("GrandMaster", [scopeManager.ParseScope() "Society"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Guardian", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Heir", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Holder", [scopeManager.ParseScope() "Title"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("job_chancellor", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("job_marshal", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("job_spiritual", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("job_spymaster", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("job_treasurer", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Killer", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Liege", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Location", [scopeManager.ParseScope() "Title"], scopeManager.ParseScope() "Province", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Lover", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Mother", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("NextGrandMaster", [scopeManager.ParseScope() "Society"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Offmap", [scopeManager.ParseScope() "Province"], scopeManager.ParseScope() "Offmap", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("OldestChild", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("OriginalOwner", [scopeManager.ParseScope() "Artifact"; scopeManager.ParseScope() "Wonder"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Owner", [scopeManager.ParseScope() "Artifact"; scopeManager.ParseScope() "Wonder"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("ParentReligion", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Religion", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("PlotTarget", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("PlotTargetTitle", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Title", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("PrevRuler", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Province", [scopeManager.ParseScope() "Wonder"], scopeManager.ParseScope() "Province", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("PrimaryTitle", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Title", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("RealFather", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Realm", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Title", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Regent", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Reincarnation", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Religion", [scopeManager.ParseScope() "Province"; scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Religion", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Ruler", [scopeManager.ParseScope() "Offmap"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("SecretReligion", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Religion", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Society", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Society", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Spouse", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("SupportedClaimant", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("SupportedClaimantTitle", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Title", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("ThePope", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("ThirdPartyCharacter", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("ThirdPartyTitle", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Title", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Title", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Title", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("TopLiege", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("TrueFather", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("TrueReligion", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Religion", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Twin", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Wonder", [scopeManager.ParseScope() "Province"], scopeManager.ParseScope() "Wonder", EffectType.Link, defaultDesc, "", true);

        // TODO: Check these as
        ScopedEffect("Actor", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("ActorExtra", [scopeManager.ParseScope() "Province"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("AdultExtra", [scopeManager.ParseScope() "Province"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("ChildExtra", [scopeManager.ParseScope() "Province"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Clan", [scopeManager.ParseScope() "Province"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("County", [scopeManager.ParseScope() "Province"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Capital", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Province", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Duchy", [scopeManager.ParseScope() "Province"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Government", [scopeManager.ParseScope() "Character"], scopeManager.AnyScope, EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Host", [scopeManager.ParseScope() "Title"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Kingdom", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Title", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Player", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Recipient", [scopeManager.ParseScope() "Character"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("RecipientExtra", [scopeManager.ParseScope() "Province"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("RelHead", [scopeManager.ParseScope() "Religion"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("SeaZone", [scopeManager.ParseScope() "Province"], scopeManager.ParseScope() "Character", EffectType.Link, defaultDesc, "", true);
        ScopedEffect("Siege", [scopeManager.ParseScope() "Province"], scopeManager.ParseScope() "Siege", EffectType.Link, defaultDesc, "", true);
    ]
    let scopedLocEffectsMap() = EffectMap.FromList(InsensitiveStringComparer(), scopedLocEffects() |> List.map (fun se -> se.Name, se :> Effect))


    let locPrimaryScopes() =
        let from = fun (s : ScopeContext, change) -> {s with Scopes = scopeManager.AnyScope::s.Scopes}, true
        [
        "This", id;
        "Root", fun (s : ScopeContext, change) -> {s with Scopes = s.Root::s.Scopes}, true;
        "From", from; //TODO Make it actually use FROM
        "FromFrom", from >> from;
        "FromFromFrom", from >> from >> from;
        "FromFromFromFrom", from >> from >> from >> from;
        ]

    let localisationCommandValidator() = Scopes.createLocalisationCommandValidator (locPrimaryScopes()) (scopedLocEffectsMap())
