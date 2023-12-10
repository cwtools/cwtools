module CWTools.Validation.LocalisationValidation

open CWTools.Common
open CWTools.Validation.ValidationCore
open CWTools.Utilities

type S = Severity

let inline checkLocKeyN (leaf: ^a) (keys: Set<string>) (lang: Lang) errors (ids: StringTokens) key =
    match key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")), Set.contains key keys with
    | true, _ -> errors
    | _, true -> errors
    | _, false -> invData (ErrorCodes.MissingLocalisation key lang) leaf (Some key) <&&&> errors

let inline checkLocKeyNE (keys: Set<string>) (lang: Lang) (ids: StringTokens) key =
    match key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")), Set.contains key keys with
    | true, _ -> true
    | _, true -> true
    | _, false -> false

let inline checkLocKeyInlineN (leaf: ^a) (keys: Set<string>) (lang: Lang) errors (ids: StringTokens) key =
    match ids.quoted, Set.contains key keys with
    | true, true -> invData (ErrorCodes.LocalisationKeyInInline key) leaf (Some key) <&&&> errors
    | true, false -> errors
    | false, true -> errors
    | false, false -> invData (ErrorCodes.MissingLocalisation key lang) leaf (Some key) <&&&> errors

let inline checkLocKeyInlineNE (keys: Set<string>) (lang: Lang) (ids: StringTokens) key =
    match ids.quoted, Set.contains key keys with
    | true, true -> false
    | true, false -> true
    | false, true -> true
    | false, false -> false

let inline checkLocNameN (leaf: ^a) (keys: Set<string>) (lang: Lang) (ids: StringTokens) (key: string) errors =
    match
        (key.Contains "." || key.Contains("_")) && (key.Contains(" ") |> not),
        key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")),
        Set.contains key keys
    with
    | false, _, _ -> errors
    | _, true, _ -> errors
    | _, _, true -> errors
    | _, _, false -> invData (ErrorCodes.MissingLocalisation key lang) leaf (Some key) <&&&> errors

let inline checkLocNameNE (keys: Set<string>) (lang: Lang) (ids: StringTokens) (key: string) =
    match
        (key.Contains "." || key.Contains("_")) && (key.Contains(" ") |> not),
        key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")),
        Set.contains key keys
    with
    | false, _, _ -> true
    | _, true, _ -> true
    | _, _, true -> true
    | _, _, false -> false

let inline checkLocKeysLeafOrNodeN (keys: (Lang * Set<string>) list) ids (key: string) (leafornode: ^a) errors =
    keys
    |> List.fold (fun state (l, keys) -> checkLocKeyN leafornode keys l state ids key) errors

let inline checkLocKeysLeafOrNodeNE (keys: (Lang * Set<string>) list) ids (key: string) =
    keys
    |> List.fold (fun state (l, keys) -> state && checkLocKeyNE keys l ids key) true

let inline checkLocKeysInlineLeafOrNodeN (keys: (Lang * Set<string>) list) ids (key: string) (leafornode: ^a) errors =
    keys
    |> List.fold (fun state (l, keys) -> checkLocKeyInlineN leafornode keys l state ids key) errors

let inline checkLocKeysInlineLeafOrNodeNE (keys: (Lang * Set<string>) list) ids (key: string) =
    keys
    |> List.fold (fun state (l, keys) -> state && checkLocKeyInlineNE keys l ids key) true
