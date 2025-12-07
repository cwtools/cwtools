namespace CWTools.Common

open CWTools.Common.NewScope

module IRConstants =
    let defaultScopes =
        [| "Value", [ "value" ]
           "Bool", [ "bool" ]
           "Flag", [ "flag" ]
           "Color", [ "color" ]
           "Country", [ "country" ]
           "Character", [ "character" ]
           "Province", [ "province" ]
           "Combat", [ "combat" ]
           "Unit", [ "unit" ]
           "Pop", [ "pop" ]
           "Family", [ "family" ]
           "Party", [ "party" ]
           "Religion", [ "religion" ]
           "Culture", [ "culture" ]
           "Job", [ "job" ]
           "CultureGroup", [ "culture group" ]
           "Area", [ "area" ]
           "State", [ "state" ]
           "Subunit", [ "subunit" ]
           "Governorship", [ "governorship" ]
           "Region", [ "region" ] |]

    let defaultScopeInputs () =
        defaultScopes
        |> Array.map (fun (n, s) ->
            { ScopeInput.name = n
              ScopeInput.aliases = s
              ScopeInput.isSubscopeOf = []
              dataTypeName = None })

    let defaultModifiers =
        [| "Province", None, [ "Province"; "Country" ]
           "Country", None, [ "country" ]
           "Unit", None, [ "Unit"; "Country" ]
           "Character", None, [ "Character"; "Country" ] |]

    let defaultModifiersInputs () =
        defaultModifiers
        |> Array.map (fun (n, intID, ss) ->
            { ModifierCategoryInput.name = n
              ModifierCategoryInput.internalID = intID
              ModifierCategoryInput.scopes = ss |> List.map (scopeManager.ParseScope()) })

    let scriptFolders = [| "common"; "events"; "localization"; "map_data" |]
