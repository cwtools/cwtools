module CWTools.Validation.LocalisationValidation
open CWTools.Common
open CWTools.Validation.ValidationCore

    type S = Severity
    let inline checkLocKeyN (leaf : ^a) (keys : Set<string>) (lang : Lang) errors key  =
        match key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")), Set.contains key keys with
        | true, _ -> errors
        | _, true -> errors
        | _, false -> invData (ErrorCodes.MissingLocalisation key (lang)) leaf (Some key) <&&&> errors
    let inline checkLocKeyNE (keys : Set<string>) (lang : Lang) key =
        match key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")), Set.contains key keys with
        | true, _ -> true
        | _, true -> true
        | _, false -> false

    let inline checkLocNameN (leaf : ^a) (keys : Set<string>) (lang : Lang) (key : string) (errors) =
        match (key.Contains (".") || key.Contains("_")) && (key.Contains(" ") |> not), key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")), Set.contains key keys with
        | false, _, _ -> errors
        | _, true, _ -> errors
        | _, _, true -> errors
        | _, _, false -> invData (ErrorCodes.MissingLocalisation key (lang)) leaf (Some key) <&&&> errors
    let inline checkLocNameNE (keys : Set<string>) (lang : Lang) (key : string)  =
        match (key.Contains (".") || key.Contains("_")) && (key.Contains(" ") |> not), key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")), Set.contains key keys with
        | false, _, _ -> true
        | _, true, _ -> true
        | _, _, true -> true
        | _, _, false -> false

    let inline checkLocKeysLeafOrNodeN (keys : (Lang * Set<string>) list) (key : string) (leafornode : ^a) (errors) =
        keys |> List.fold (fun state (l, keys)  -> checkLocKeyN leafornode keys l state key) errors
    let inline checkLocKeysLeafOrNodeNE (keys : (Lang * Set<string>) list) (key : string) =
        keys |> List.fold (fun state (l, keys)  -> state && checkLocKeyNE keys l key) true
