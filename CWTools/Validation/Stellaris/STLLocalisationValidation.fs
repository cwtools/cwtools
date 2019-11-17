namespace CWTools.Validation.Stellaris
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser.Types
open CWTools.Common
open CWTools.Games.Stellaris.STLLookup
open System

module STLLocalisationValidation =
    type S = Severity
    type LocalisationValidator = LocalisationValidator<STLComputedData>
    let inline checkLocKey (leaf : ^a) (keys : Set<string>) (lang : Lang) key =
        if lang = STL STLLang.Default then OK else
        match key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")), Set.contains key keys with
        | true, _ -> OK
        | _, true -> OK
        | _, false -> Invalid (Guid.NewGuid(), [invData (ErrorCodes.MissingLocalisation key (lang)) leaf (Some key)])

    let inline checkLocName (leaf : ^a) (keys : Set<string>) (lang : Lang) (key : string)  =
        match (key.Contains (".") || key.Contains("_")) && (key.Contains(" ") |> not), key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")), Set.contains key keys with
        | false, _, _ -> OK
        | _, true, _ -> OK
        | _, _, true -> OK
        | _, _, false -> Invalid (Guid.NewGuid(), [invData (ErrorCodes.MissingLocalisation key (lang)) leaf (Some key)])

    let inline checkLocKeysLeafOrNode (keys : (Lang * Set<string>) list) (key : string) (leafornode : ^a) =
        keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocKey leafornode keys l key) OK

    let checkLocKeys (keys : (Lang * Set<string>) list) (leaf : Leaf) =
        let key = leaf.Value |> (function |QString s -> s |s -> s.ToString())
        keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocKey leaf keys l key) OK

    let getLocKeys (keys : (Lang * Set<string>) list) (tags : string list) (node : Node) =
        let fNode = (fun (x:Node) children ->
                        let results =  x.Values |> List.filter (fun l -> tags |> List.contains l.Key) |> List.fold (fun s t -> s <&&> (checkLocKeys keys t) ) OK
                        results <&&> children)
        let fCombine = (<&&>)
        node |> (foldNode2 fNode fCombine OK)

    let valEventNameLocs (keys : (Lang * Set<string>) list) (node : Node) =
        let names = keys |> List.choose (fun (l, ks) -> if l = STL STLLang.Default then Some ks else None) |> List.tryHead |> Option.defaultValue Set.empty
        let fNode = (fun (x : Node) children ->
                        match x.Key.ToLower(), x.Leafs "name" |> List.ofSeq with
                        | "create_point_of_interest", _
                        | "enable_special_project", _
                        | "create_species", _
                        | "option", _ -> children
                        | _, [] -> children
                        | _, [leaf] ->
                            let name = leaf.Value |> function |(QString s) -> s |s -> s.ToString()
                            if name = "random" then OK else
                                children <&&> (checkLocName leaf names (STL STLLang.Default) (name) )
                        | _, names -> children
                         )
        let fCombine = (<&&>)
        node |> (foldNode2 fNode fCombine OK)

    let checkLocLeafValue (keys : Set<string>) (lang : Lang) key (lv : LeafValue) =
        if lang = STL STLLang.Default then OK else
            match key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")), Set.contains key keys with
            | true, _ -> OK
            | _, true -> OK
            | _, false -> Invalid (Guid.NewGuid(), [invLeafValue (ErrorCodes.MissingLocalisation key (lang)) lv (Some key)])

    let checkLocNode (keys : Set<string>) (lang : Lang) key (node : Node) =
        if lang = STL STLLang.Default then OK else
            match key = "" || key.Contains(" ") || (key.StartsWith("[") && key.EndsWith("]")), Set.contains key keys with
            | true, _ -> OK
            | _, true -> OK
            | _, false -> Invalid (Guid.NewGuid(), [invData (ErrorCodes.MissingLocalisation key (lang)) node (Some key)])

    let checkLocLeafValueS (keys : (Lang * Set<string>) list) key (lv : LeafValue) =
         keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocLeafValue keys l key lv) OK

    let checkLocNodeS (keys : (Lang * Set<string>) list) key (node : Node) =
         keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l key node) OK

    let checkKeyAndDesc (keys : (Lang * Set<string>) list) (node : Node) =
        let key = node.Key
        let desc = key + "_desc"
        let keyres = keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l key node) OK
        let descres = keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l desc node) OK
        keyres <&&> descres

    let checkLocLeafValueKeyAdv keys prefix suffix (lv : LeafValue) =
        let key = prefix + lv.Value.ToString() + suffix
        (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocLeafValue keys l key lv) OK)

    let checkLocNodeKeyAdv keys prefix suffix (node : Node) =
        let key = prefix + node.Key + suffix
        (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l key node) OK)

    let checkLocLeafValueKeyAdvs keys prefix suffixes lv = suffixes |> List.fold (fun s c -> s <&&> (checkLocLeafValueKeyAdv keys prefix c lv)) OK

    let checkLocNodeKeyAdvs keys prefix suffixes node = suffixes |> List.fold (fun s c -> s <&&> (checkLocNodeKeyAdv keys prefix c node)) OK

    let checkLocNodeTagAdv keys prefix suffix tag (node : Node) =
        let names = node.Leafs tag
        let inner = (fun (leaf : Leaf) -> (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocKey leaf keys l (prefix + (leaf.Value.ToRawString()) + suffix) ) OK))
        names <&!&> inner
        // let name  = node.TagText tag
        // match name with
        // | "" -> OK
        // | x -> (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l (prefix + x + suffix)) OK)

    let checkLocNodeTagAdvs keys prefix suffixes tag (node : Node) =
        suffixes |> List.fold (fun s c -> s <&&> (checkLocNodeTagAdv keys prefix c tag node)) OK

    let checkLocNodeTag keys tag (node : Node) = checkLocNodeTagAdvs keys "" [""] tag node




    let valEventLocs : LocalisationValidator =
        fun _ keys es ->
            let es = es.GlobMatchChildren("**/events/*.txt")
            let inner =
                fun (event : Node) ->
                    let titles = event.Leafs "title" |> List.ofSeq |> List.map (checkLocKeys keys) |> List.fold (<&&>) OK
                    let desc = event.Leafs "desc" |> List.ofSeq |> List.map (checkLocKeys keys) |> List.fold (<&&>) OK
                    let options = event.Childs "option" |> List.ofSeq
                                                        |> List.collect (fun o -> o.Leafs "name" |> List.ofSeq |> List.map (checkLocKeys keys))
                                                        |> List.fold (<&&>) OK
                    let usedKeys = event.Children |> List.fold (fun s c -> s <&&> (getLocKeys keys ["desc"; "text"; "custom_tooltip"; "fail_text"; "response_text"] c)) OK
                    let nameres = valEventNameLocs keys event
                    titles <&&> desc <&&> options <&&> usedKeys <&&> nameres
            es <&!&> inner

    let valEffectLocs : LocalisationValidator =
        fun _ keys es ->
            let fNode = (fun (x : Node) children ->
                getLocKeys keys ["custom_tooltip"] x <&&> children)
            let fCombine = (<&&>)
            es.AllEffects <&!&> (foldNode2 fNode fCombine OK)

    let valTriggerLocs : LocalisationValidator =
        fun _ keys es ->
            let fNode = (fun (x : Node) children ->
                getLocKeys keys ["custom_tooltip"] x <&&> children)
            let fCombine = (<&&>)
            es.AllTriggers <&!&> (foldNode2 fNode fCombine OK)

    let valTechLocs : LocalisationValidator =
        fun _ keys es ->
            let entities = es.GlobMatchChildren("**/common/technology/*.txt")
            entities |> List.map
                (fun (node : Node) ->
                let keyres = checkKeyAndDesc keys node
                let innerKeys = node.Childs "prereqfor_desc" |> List.ofSeq |> List.fold (fun s c -> s <&&> (getLocKeys keys ["desc"; "title"] c)) OK
                let flags = node.Child "feature_flags" |> Option.map (fun c -> c.All |> List.choose (function |LeafValueC lv -> Some lv.Value |_ -> None )) |> Option.defaultValue []
                let flags2 = flags |> List.map (fun f -> "feature_" + f.ToString())
                let flagres = flags2 |> List.fold (fun s c -> s <&&> (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l c node) OK)) OK
                let flagdesc = flags2 |> List.map (fun f -> f + "_desc")
                let flagdescres = flagdesc |> List.fold (fun s c -> s <&&> (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l c node) OK)) OK
                let gateway = node.TagText "gateway"
                let gatewayres =
                    match gateway with
                    | "" -> OK
                    | x -> keys |> List.fold (fun state (l, keys) -> state <&&> checkLocNode keys l ("gateway_" + x) node) OK
                keyres <&&> innerKeys <&&> gatewayres <&&> flagres <&&> flagdescres
                )
                |> List.fold (<&&>) OK


    let valCompSetLocs : LocalisationValidator =
        fun _ keys es ->
            let entities = es.GlobMatchChildren("**/common/component_sets/*.txt")
            entities |> List.map
                (fun (node : Node) ->
                    let key = node.Key
                    let required = node.Tag "required_component_set" |> (function |Some (Bool b) when b = true -> true |_ -> false)
                    match key, required with
                    | "component_set", false ->
                        checkLocNodeTagAdvs keys "" [""; "_DESC"] "key" node
                    | _ -> OK)
                |> List.fold (<&&>) OK



    let valCompTempLocs : LocalisationValidator =
        fun _ keys es ->
            let entities = es.GlobMatchChildren("**/common/component_templates/*.txt")
            let inner =
                fun (node : Node) ->
                    let keyres =
                        match node.TagText "hidden", node.TagText "type" with
                        | "yes", _ -> OK
                        | _, "planet_killer" ->
                            checkLocNodeTagAdvs keys "" [""; "_ACTION"; "_DESC"] "key" node
                            <&&>
                            (checkLocNodeTagAdvs keys "FLEETORDER_DESTROY_PLANET_WITH_" [""] "key" node)
                            <&&>
                            (checkLocNodeTagAdvs keys "MESSAGE_DESC_FOR_" [""] "key" node)
                        | _ -> node.Leafs "key" |> List.ofSeq |> List.fold (fun s l -> s <&&> (checkLocKeys keys l)) OK
                    let auras = Seq.append (node.Childs "friendly_aura") (node.Childs "hostile_aura")
                    let aurares = auras |> List.ofSeq |> List.fold (fun s c -> s <&&> (getLocKeys keys ["name"] c)) OK
                    keyres <&&> aurares
            entities |> List.map inner |> List.fold (<&&>) OK


    // let valBuildingLocs : LocalisationValidator =
    //     fun _ keys es ->
    //         let entities = es.GlobMatchChildren("**/common/buildings/*.txt")
    //         let inner =
    //             fun (node : Node) ->
    //                 let keyres = checkKeyAndDesc keys node
    //                 //let failtext = node.Children <&!&> ((checkLocNodeTag keys "text") <&> (checkLocNodeTag keys "fail_text"))
    //                 //let failtext = node.Children |> List.map ((checkLocNodeTag keys "text") <&> (checkLocNodeTag keys "fail_text")) |> List.fold (<&&>) OK
    //                 let failtext = node.Children <&!&> (getLocKeys keys ["text"; "fail_text"])
    //                 keyres <&&> failtext
    //         entities |> List.map inner |> List.fold (<&&>) OK

    // let valScriptedTriggers : LocalisationValidator =
    //     fun _ keys es ->
    //         let entities = es.GlobMatchChildren("**/common/scripted_triggers/*.txt")
    //         let inner =
    //             fun (node : Node) ->
    //                 node.Children <&!&> (getLocKeys keys ["text"; "fail_text"])
    //         entities |> List.map inner |> List.fold (<&&>) OK

    let valTraditionLocs (node : Node) (keys : (Lang * Set<string>) list) (starts : string list) (finals : string list) (traditions : string list)=
        let key = node.Key
        let finishres =
            match finals |> List.contains key with
            | true ->
                let effect = key + "_effect"
                keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l effect node) OK
            | false -> OK
        let adoptres =
            match starts |> List.contains key with
            | true ->
                (let effect = key + "_effect"
                keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l effect node) OK)
                <&&>
                (let desc = key + "_desc"
                keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l desc node) OK)
            | false -> OK
        let traditionsres =
            match traditions |> List.contains key with
            | true ->
                let desc = key + "_desc"
                let a = keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l desc node) OK
                let delayed = key + "_delayed"
                let b = keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l delayed node) OK
                a <&&> b
            | false -> OK
        let keyres = keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l key node) OK
        keyres <&&> finishres <&&> adoptres <&&> traditionsres

    let processTradCat  (keys : (Lang * Set<string>) list) (cat : Node) =
        let key = cat.Key
        let start = cat.TagText "adoption_bonus"
        let finish = cat.TagText "finish_bonus"
        let vals = cat.Child "traditions" |> Option.map (fun c -> c.All |> List.choose (function |LeafValueC lv -> Some lv.Value |_ -> None )) |> Option.defaultValue []
        let traditions = vals |> List.map (function |QString s -> s |x -> x.ToString())
        let keyres = keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l key cat) OK
        (start, finish, traditions)

    let valTraditionLocCats : LocalisationValidator =
        fun es keys nes ->
            let cats = es.GlobMatch("**/tradition_categories/*.txt") |> List.collect (fun e -> e.Children)
            let newcats = nes.GlobMatch("**/tradition_categories/*.txt") |> List.collect (fun e -> e.Children)
            let starts, finishes, trads = cats |> List.map (processTradCat keys) |> List.fold (fun ( ss, fs, ts) (s, f, t) -> s::ss,  f::fs, ts @ t) ([], [], [])
            let traditions = nes.GlobMatch("**/traditions/*.txt")  |> List.collect (fun e -> e.Children)
            let inner = fun tradition -> valTraditionLocs tradition keys starts finishes trads
            let innerCat = fun (cat : Node) -> keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l cat.Key cat) OK
            newcats |> List.map innerCat |> List.fold (<&&>) OK
            <&&>
            (traditions |> List.map inner |> List.fold (<&&>) OK)

    let valPolicies : LocalisationValidator =
        fun _ keys es ->
            let policies = es.GlobMatchChildren("**/common/policies/*.txt")
            let options = policies |> Seq.collect (fun p -> p.Childs "option")
            let inner = checkLocNodeKeyAdvs keys "policy_" [""; "_desc"]
            let oinner =
                fun (node : Node) ->
                    let vals = node.Child "policy_flags" |> Option.map (fun c -> c.All |> List.choose (function |LeafValueC lv -> Some lv.Value |_ -> None )) |> Option.defaultValue []
                    let vals2 = vals |> List.map (fun v -> v.ToString() + "_name")
                    vals2 |> List.fold (fun s c -> s <&&> (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l c node) OK)) OK
                    <&&>
                    (checkLocNodeTagAdvs keys "" [""; "_desc"] "name" node)
            policies <&!&> inner
            <&&>
            (options <&!&> oinner)
