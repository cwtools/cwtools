module CWTools.Validation.LocalisationValidation

open CWTools.Common
open CWTools.Validation.ValidationCore
open CWTools.Utilities

let inline checkLocKeyN (leaf: ^a) (keys: Set<string>) (lang: Lang) errors key =
    match key = "" || key.Contains(' ') || (key.StartsWith('[') && key.EndsWith(']')), Set.contains key keys with
    | true, _ -> errors
    | _, true -> errors
    | _, false -> invData (ErrorCodes.MissingLocalisation key lang) leaf (Some key) <&&&> errors

let inline checkLocKeyNE (keys: Set<string>) key =
    match key = "" || key.Contains(' ') || (key.StartsWith('[') && key.EndsWith(']')), Set.contains key keys with
    | true, _ -> true
    | _, true -> true
    | _, false -> false

let inline checkLocKeyInlineN (leaf: ^a) (keys: Set<string>) (lang: Lang) errors (ids: StringTokens) key =
    match ids.quoted, Set.contains key keys with
    | true, true -> invData (ErrorCodes.LocalisationKeyInInline key) leaf (Some key) <&&&> errors
    | true, false -> errors
    | false, true -> errors
    | false, false -> invData (ErrorCodes.MissingLocalisation key lang) leaf (Some key) <&&&> errors

let inline checkLocNameN (leaf: ^a) (keys: Set<string>) (lang: Lang) (ids: StringTokens) (key: string) errors =
    match
        (key.Contains '.' || key.Contains('_')) && (key.Contains(' ') |> not),
        key = "" || key.Contains(' ') || (key.StartsWith('[') && key.EndsWith(']')),
        Set.contains key keys
    with
    | false, _, _ -> errors
    | _, true, _ -> errors
    | _, _, true -> errors
    | _, _, false -> invData (ErrorCodes.MissingLocalisation key lang) leaf (Some key) <&&&> errors

let inline checkLocKeysLeafOrNodeN (keys: (Lang * Set<string>) array) (key: string) (leafornode: ^a) errors =
    keys
    |> Array.fold (fun state (l, keys) -> checkLocKeyN leafornode keys l state key) errors

let inline checkLocKeysLeafOrNodeNE (keys: (Lang * Set<string>) array) (key: string) =
    keys
    |> Array.fold (fun state (l, keys) -> state && checkLocKeyNE keys key) true

let inline checkLocKeysInlineLeafOrNodeN (keys: (Lang * Set<string>) array) ids (key: string) (leafornode: ^a) errors =
    keys
    |> Array.fold (fun state (l, keys) -> checkLocKeyInlineN leafornode keys l state ids key) errors
