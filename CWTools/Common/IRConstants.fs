namespace CWTools.Common

open CWTools.Utilities.Utils
open CWTools.Common.NewScope
module IRConstants =
    // type Scope =
    //     | NoneScope
    //     | Value
    //     | Bool
    //     | Flag
    //     | Color
    //     | Country
    //     | Character
    //     | Province
    //     | Combat
    //     | Unit
    //     | Pop
    //     | Family
    //     | Party
    //     | Religion
    //     | Culture
    //     | Job
    //     | CultureGroup
    //     | Area
    //     | State
    //     | Subunit
    //     | Governorship
    //     | Region
    //     //Misc
    //     | Any
    //     | InvalidScope
    //     override x.ToString() =
    //         match x with
    //         | Any -> "Any/Unknown"
    //         | CultureGroup -> "Culture group"
    //         | InvalidScope -> "InvalidScope"
    //         | NoneScope -> "None"
    //         | _ -> sprintf "%A" x

    //     static member AnyScope = Scope.Any

    //     interface IScope<Scope> with
    //         member this.AnyScope = Scope.Any
    //         member this.MatchesScope target =
    //             match this, target with
    //             | Scope.Any, _
    //             | _, Scope.Any -> true
    //             | _, _ -> this = target

    // let allScopes = [
    //     Scope.Country;
    //     Scope.Character;
    //     Scope.Province;
    //     Scope.Combat;
    //     Scope.Unit;
    //     Scope.Pop;
    //     Scope.Family;
    //     Scope.Party;
    //     Scope.Religion;
    //     Scope.Culture;
    //     Scope.Job;
    //     Scope.CultureGroup;
    //     Scope.Area;
    //     Scope.State;
    //     Scope.Subunit;
    //     Scope.Governorship;
    //     Scope.Region;
    //         ]
    // let allScopesSet = allScopes |> Set.ofList
    // let parseScope =
    //     (fun (x : string) ->
    //     x.ToLower()
    //     |>
    //         function
    //             | "none" -> Scope.Any
    //             | "value" -> Scope.Value
    //             | "bool" -> Scope.Bool
    //             | "flag" -> Scope.Flag
    //             | "color" -> Scope.Color
    //             | "country" -> Scope.Country
    //             | "character" -> Scope.Character
    //             | "province" -> Scope.Province
    //             | "combat" -> Scope.Combat
    //             | "unit" -> Scope.Unit
    //             | "pop" -> Scope.Pop
    //             | "family" -> Scope.Family
    //             | "party" -> Scope.Party
    //             | "religion" -> Scope.Religion
    //             | "culture" -> Scope.Culture
    //             | "job" -> Scope.Job
    //             | "culture group" -> Scope.CultureGroup
    //             | "area" -> Scope.Area
    //             | "state" -> Scope.State
    //             | "subunit" -> Scope.Subunit
    //             | "governorship" -> Scope.Governorship
    //             | "region" -> Scope.Region
    //             | "any" -> Scope.Any
    //             | "all" -> Scope.Any
    //             | "no_scope" -> Scope.Any
    //             | x -> log (sprintf "Unexpected scope %O" x); Scope.Any) //failwith ("unexpected scope" + x.ToString()))

    // let parseScopes =
    //     function
    //     | "all" -> allScopes
    //     | x -> [parseScope x]
    let defaultScopes = [
        "Value", ["value"]
        "Bool", ["bool"]
        "Flag", ["flag"]
        "Color", ["color"]
        "Country", ["country"]
        "Character", ["character"]
        "Province", ["province"]
        "Combat", ["combat"]
        "Unit", ["unit"]
        "Pop", ["pop"]
        "Family", ["family"]
        "Party", ["party"]
        "Religion", ["religion"]
        "Culture", ["culture"]
        "Job", ["job"]
        "CultureGroup", ["culture group"]
        "Area", ["area"]
        "State", ["state"]
        "Subunit", ["subunit"]
        "Governorship", ["governorship"]
        "Region", ["region"]
    ]

    let defaultScopeInputs =
        defaultScopes |> List.map (fun (n, s) -> { NewScope.ScopeInput.name = n; NewScope.ScopeInput.aliases = s; NewScope.ScopeInput.isSubscopeOf = []})

    type Effect = Effect<Scope>

    type DocEffect = DocEffect<Scope>
    type ScriptedEffect = ScriptedEffect<Scope>
    type ScopedEffect = ScopedEffect<Scope>
    type ModifierCategory =
        | Character
        | Province
        | Country
        | Unit
        | Any

    type Modifier =
        {
            tag : string
            categories : ModifierCategory list
            /// Is this a core modifier or a static modifier?
            core : bool
        }
        interface IModifier with
            member this.Tag = this.tag

    let categoryScopeList() = [
        ModifierCategory.Character, [scopeManager.ParseScope() "Character"; scopeManager.ParseScope() "Country"];
        ModifierCategory.Unit, [scopeManager.ParseScope() "Unit"; scopeManager.ParseScope() "Country"];
        ModifierCategory.Province, [scopeManager.ParseScope() "Province"; scopeManager.ParseScope() "Country"];
        ModifierCategory.Country, [scopeManager.ParseScope() "Country"];
        ModifierCategory.Any, [];
    ]
    let modifierCategoryToScopesMap() = categoryScopeList() |> Map.ofList

    let scriptFolders = [
        "common";
        "events";
        "localization";
        "map_data";
    ]
