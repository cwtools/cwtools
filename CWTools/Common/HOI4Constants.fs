namespace CWTools.Common

open CWTools.Common.NewScope
module HOI4Constants =
    let defaultScopes = [
        "Country", ["country"]
        "State", ["state"]
        "Unit Leader", ["unit leader"; "unit_leader"]
        "Air", ["air"]
    ]
    let defaultScopeInputs =
        defaultScopes |> List.map (fun (n, s) -> { NewScope.ScopeInput.name = n; NewScope.ScopeInput.aliases = s; NewScope.ScopeInput.isSubscopeOf = []; dataTypeName = None})

    let defaultModifiers = [
        "State", None, ["any"]
        "Country", None, ["country"]
        "Unit", None, ["UnitLeader"; "Country"]
        "UnitLeader", None, ["UnitLeader"; "Country"]
        "Air", None, ["air"; "country"]
    ]

    let defaultModifiersInputs() =
        defaultModifiers |> List.map (fun (n, intID, ss) ->
            {
                NewScope.ModifierCategoryInput.name = n;
                NewScope.ModifierCategoryInput.internalID = intID;
                NewScope.ModifierCategoryInput.scopes = ss |> List.map (scopeManager.ParseScope())
                })

    let scriptFolders = [
        "common";
        "events";
        "gfx";
        "interface";
        "localisation";
        "history";
        "map"
    ]
