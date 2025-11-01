namespace CWTools.Validation.Stellaris

open CWTools.Games
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser.Types
open CWTools.Common
open System

module STLLocalisationValidation =
    type S = Severity
    type LocalisationValidator = LocalisationValidator<STLComputedData>

    let inline checkLocKey (leaf: ^a) (keys: Set<string>) (lang: Lang) key =
        if lang = STL STLLang.Default then
            OK
        else
            match
                key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")), Set.contains key keys
            with
            | true, _ -> OK
            | _, true -> OK
            | _, false -> Invalid(Guid.NewGuid(), [ invData (ErrorCodes.MissingLocalisation key lang) leaf (Some key) ])

    let inline checkLocName (leaf: ^a) (keys: Set<string>) (lang: Lang) (key: string) =
        match
            (key.Contains "." || key.Contains("_")) && (key.Contains(" ") |> not),
            key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")),
            Set.contains key keys
        with
        | false, _, _ -> OK
        | _, true, _ -> OK
        | _, _, true -> OK
        | _, _, false -> Invalid(Guid.NewGuid(), [ invData (ErrorCodes.MissingLocalisation key lang) leaf (Some key) ])

    let inline checkLocKeysLeafOrNode (keys: (Lang * Set<string>) array) (key: string) (leafornode: ^a) =
        keys
        |> Array.fold (fun state (l, keys) -> state <&&> checkLocKey leafornode keys l key) OK

    let checkLocKeys (keys: (Lang * Set<string>) array) (leaf: Leaf) =
        let key =
            leaf.Value
            |> (function
            | QString s -> s.GetString()
            | s -> s.ToString())

        keys
        |> Array.fold (fun state (l, keys) -> state <&&> checkLocKey leaf keys l key) OK

    let getLocKeys (keys: (Lang * Set<string>) array) (tags: string list) (node: Node) =
        let fNode =
            (fun (x: Node) children ->
                let results =
                    x.Leaves
                    |> Seq.filter (fun l -> tags |> List.contains l.Key)
                    |> Seq.fold (fun s t -> s <&&> (checkLocKeys keys t)) OK

                results <&&> children)

        let fCombine = (<&&>)
        node |> (foldNode2 fNode fCombine OK)

    let checkLocLeafValue (keys: Set<string>) (lang: Lang) key (lv: LeafValue) =
        if lang = STL STLLang.Default then
            OK
        else
            match
                key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")), Set.contains key keys
            with
            | true, _ -> OK
            | _, true -> OK
            | _, false ->
                Invalid(Guid.NewGuid(), [ invLeafValue (ErrorCodes.MissingLocalisation key lang) lv (Some key) ])

    let checkLocNode (keys: Set<string>) (lang: Lang) key (node: Node) =
        if lang = STL STLLang.Default then
            OK
        else
            match
                key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")), Set.contains key keys
            with
            | true, _ -> OK
            | _, true -> OK
            | _, false -> Invalid(Guid.NewGuid(), [ invData (ErrorCodes.MissingLocalisation key lang) node (Some key) ])

    let checkLocLeafValueS (keys: (Lang * Set<string>) list) key (lv: LeafValue) =
        keys
        |> List.fold (fun state (l, keys) -> state <&&> checkLocLeafValue keys l key lv) OK

    let checkLocNodeS (keys: (Lang * Set<string>) list) key (node: Node) =
        keys
        |> List.fold (fun state (l, keys) -> state <&&> checkLocNode keys l key node) OK

    let checkKeyAndDesc (keys: (Lang * Set<string>) array) (node: Node) =
        let key = node.Key
        let desc = key + "_desc"

        let keyres =
            keys
            |> Array.fold (fun state (l, keys) -> state <&&> checkLocNode keys l key node) OK

        let descres =
            keys
            |> Array.fold (fun state (l, keys) -> state <&&> checkLocNode keys l desc node) OK

        keyres <&&> descres

    let checkLocLeafValueKeyAdv keys prefix suffix (lv: LeafValue) =
        let key = prefix + lv.Value.ToString() + suffix

        (keys
         |> List.fold (fun state (l, keys) -> state <&&> checkLocLeafValue keys l key lv) OK)

    let checkLocNodeKeyAdv keys prefix suffix (node: Node) =
        let key = prefix + node.Key + suffix

        (keys
         |> List.fold (fun state (l, keys) -> state <&&> checkLocNode keys l key node) OK)

    let checkLocLeafValueKeyAdvs keys prefix suffixes lv =
        suffixes
        |> List.fold (fun s c -> s <&&> (checkLocLeafValueKeyAdv keys prefix c lv)) OK

    let checkLocNodeKeyAdvs keys prefix suffixes node =
        suffixes
        |> List.fold (fun s c -> s <&&> (checkLocNodeKeyAdv keys prefix c node)) OK

    let checkLocNodeTagAdv keys prefix suffix tag (node: Node) =
        let names = node.Leafs tag

        let inner =
            (fun (leaf: Leaf) ->
                (keys
                 |> Array.fold
                     (fun state (l, keys) ->
                         state <&&> checkLocKey leaf keys l (prefix + leaf.Value.ToRawString() + suffix))
                     OK))

        names <&!&> inner
    // let name  = node.TagText tag
    // match name with
    // | "" -> OK
    // | x -> (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l (prefix + x + suffix)) OK)

    let checkLocNodeTagAdvs keys prefix suffixes tag (node: Node) =
        suffixes
        |> List.fold (fun s c -> s <&&> (checkLocNodeTagAdv keys prefix c tag node)) OK

    let checkLocNodeTag keys tag (node: Node) =
        checkLocNodeTagAdvs keys "" [ "" ] tag node


    let valTechLocs: LocalisationValidator =
        fun _ keys es ->
            let entities = es.GlobMatchChildren("**/common/technology/*.txt")

            entities
            |> List.map (fun (node: Node) ->
                let keyres = checkKeyAndDesc keys node

                let innerKeys =
                    node.Childs "prereqfor_desc"
                    |> List.ofSeq
                    |> List.fold (fun s c -> s <&&> (getLocKeys keys [ "desc"; "title" ] c)) OK

                let flags =
                    node.Child "feature_flags"
                    |> Option.map (fun c ->
                        c.AllArray
                        |> Array.choose (function
                            | LeafValueC lv -> Some lv.Value
                            | _ -> None))
                    |> Option.defaultValue [||]

                let flags2 = flags |> Array.map (fun f -> "feature_" + f.ToString())

                let flagres =
                    flags2
                    |> Array.fold
                        (fun s c ->
                            s
                            <&&> (keys
                                  |> Array.fold (fun state (l, keys) -> state <&&> checkLocNode keys l c node) OK))
                        OK

                let flagdesc = flags2 |> Array.map (fun f -> f + "_desc")

                let flagdescres =
                    flagdesc
                    |> Array.fold
                        (fun s c ->
                            s
                            <&&> (keys
                                  |> Array.fold (fun state (l, keys) -> state <&&> checkLocNode keys l c node) OK))
                        OK

                let gateway = node.TagText "gateway"

                let gatewayres =
                    match gateway with
                    | "" -> OK
                    | x ->
                        keys
                        |> Array.fold (fun state (l, keys) -> state <&&> checkLocNode keys l ("gateway_" + x) node) OK

                keyres <&&> innerKeys <&&> gatewayres <&&> flagres <&&> flagdescres)
            |> List.fold (<&&>) OK


    let valPolicies: LocalisationValidator =
        fun _ keys es ->
            let policies = es.GlobMatchChildren("**/common/policies/*.txt")
            let options = policies |> Seq.collect (fun p -> p.Childs "option")

            let oinner =
                fun (node: Node) ->
                    let vals =
                        node.Child "policy_flags"
                        |> Option.map (fun c ->
                            c.AllArray
                            |> Array.choose (function
                                | LeafValueC lv -> Some lv.Value
                                | _ -> None))
                        |> Option.defaultValue [||]

                    let vals2 = vals |> Array.map (fun v -> v.ToString() + "_name")

                    vals2
                    |> Array.fold
                        (fun s c ->
                            s
                            <&&> (keys
                                  |> Array.fold (fun state (l, keys) -> state <&&> checkLocNode keys l c node) OK))
                        OK
                    <&&> (checkLocNodeTagAdvs keys "" [ ""; "_desc" ] "name" node)

            (options <&!&> oinner)
