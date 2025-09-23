namespace CWTools.Common

open CWTools.Common.NewScope

module HOI4Constants =
    let defaultScopes =
        [| "Country", [ "country" ]
           "State", [ "state" ]
           "Unit Leader", [ "unit leader"; "unit_leader" ]
           "Air", [ "air" ] |]

    let defaultScopeInputs () =
        defaultScopes
        |> Array.map (fun (n, s) ->
            { ScopeInput.name = n
              ScopeInput.aliases = s
              ScopeInput.isSubscopeOf = []
              dataTypeName = None })

    let defaultModifiers =
        [| "State", None, [ "any" ]
           "Country", None, [ "country" ]
           "Unit", None, [ "UnitLeader"; "Country" ]
           "UnitLeader", None, [ "UnitLeader"; "Country" ]
           "Air", None, [ "air"; "country" ] |]

    let defaultModifiersInputs () =
        defaultModifiers
        |> Array.map (fun (n, intID, ss) ->
            { ModifierCategoryInput.name = n
              ModifierCategoryInput.internalID = intID
              ModifierCategoryInput.scopes = ss |> List.map (scopeManager.ParseScope()) })

    let scriptFolders =
        [| "common"; "country_metadata"; "events"; "gfx"; "interface"; "localisation"; "history"; "map"; "music"; "portraits"; "sound"|]
