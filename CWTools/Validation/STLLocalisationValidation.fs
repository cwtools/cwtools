namespace CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser
open CWTools.Process.STLScopes
open CWTools.Common
open CWTools.Common.STLConstants
open DotNet.Globbing
open CWTools.Validation.STLValidation

module STLLocalisationValidation =
    type S = Severity
    type LocalisationValidator = EntitySet -> (Lang * Set<string>) list -> EntitySet -> ValidationResult

    let checkLocKey (leaf : Leaf) (keys : Set<string>) (lang : Lang) key =
        match key = "" || key.Contains(" "), Set.contains key keys with
        | true, _ -> OK
        | _, true -> OK
        | _, false -> Invalid [inv S.Warning leaf (sprintf "Localisation key %s is not defined for %O" key lang)]

    let checkLocKeys (keys : (Lang * Set<string>) list) (leaf : Leaf) =
        let key = leaf.Value |> (function |QString s -> s |s -> s.ToString())
        keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocKey leaf keys l key) OK

    let getLocKeys (keys : (Lang * Set<string>) list) (tags : string list) (node : Node) =
        let fNode = (fun (x:Node) children ->
                        let results =  x.Values |> List.filter (fun l -> tags |> List.contains l.Key) |> List.fold (fun s t -> s <&&> (checkLocKeys keys t) ) OK
                        results <&&> children)
        let fCombine = (<&&>)
        node |> (foldNode2 fNode fCombine OK)

    
    let valEventLocs (event : Event) (keys : (Lang * Set<string>) list) =
        let titles = event.Leafs "title" |> List.map (checkLocKeys keys) |> List.fold (<&&>) OK
        let options = event.Childs "option" |> List.collect (fun o -> o.Leafs "name" |> List.map (checkLocKeys keys))
                                            |> List.fold (<&&>) OK                
        let usedKeys = event.Children |> List.fold (fun s c -> s <&&> (getLocKeys keys ["desc"; "text"; "custom_tooltip"; "fail_text"; "response_text"] c)) OK
        titles <&&> options <&&> usedKeys

    let checkLocNode (node : Node) (keys : Set<string>) (lang : Lang) key =
        match key = "" || key.Contains(" "), Set.contains key keys with
        | true, _ -> OK
        | _, true -> OK
        | _, false -> Invalid [inv S.Warning node (sprintf "Localisation key %s is not defined for %O" key lang)]
        
    let checkKeyAndDesc (node : Node) (keys : (Lang * Set<string>) list) =
        let key = node.Key
        let desc = key + "_desc"
        let keyres = keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l key) OK
        let descres = keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l desc) OK
        keyres <&&> descres
        
    let valTechLocs : LocalisationValidator =
        fun _ keys es ->
            let entities = es.GlobMatchChildren("**/common/technology/*.txt")
            entities |> List.map
                (fun (node : Node) ->
                let keyres = checkKeyAndDesc node keys
                let innerKeys = node.Childs "prereqfor_desc" |> List.fold (fun s c -> s <&&> (getLocKeys keys ["desc"; "title"] c)) OK
                keyres <&&> innerKeys
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
                        let ckey = node.TagText "key"
                        let ckeydesc = ckey + "_DESC"
                        let ckeyres =  keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l ckey) OK
                        let ckeydescres =  keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l ckeydesc) OK
                        ckeyres <&&> ckeydescres
                    | _ -> OK)
                |> List.fold (<&&>) OK
            


    let valCompTempLocs : LocalisationValidator = 
        fun _ keys es ->
            let entities = es.GlobMatchChildren("**/common/component_templates/*.txt")
            let inner = 
                fun (node : Node) ->
                    let keyres = node.Leafs "key" |> List.fold (fun s l -> s <&&> (checkLocKeys keys l)) OK
                    let auras = node.Childs "friendly_aura" @ node.Childs "hostile_aura"
                    let aurares = auras |> List.fold (fun s c -> s <&&> (getLocKeys keys ["name"] c)) OK
                    keyres <&&> aurares
            entities |> List.map inner |> List.fold (<&&>) OK


    let valBuildingLocs : LocalisationValidator =
        fun _ keys es ->
            let entities = es.GlobMatchChildren("**/common/buildings/*.txt")
            let inner =
                fun node ->
                    let keyres = checkKeyAndDesc node keys
                    let failtext = node.Children |> List.fold (fun s c -> s <&&> (getLocKeys keys ["fail_text"] c)) OK
                    keyres <&&> failtext
            entities |> List.map inner |> List.fold (<&&>) OK

    let valTraditionLocs (node : Node) (keys : (Lang * Set<string>) list) (starts : string list) (finals : string list) (traditions : string list)= 
        let key = node.Key
        let finishres = 
            match finals |> List.contains key with
            | true -> 
                let effect = key + "_effect"
                keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l effect) OK
            | false -> OK
        let adoptres =
            match starts |> List.contains key with
            | true ->
                (let effect = key + "_effect"
                keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l effect) OK)
                <&&>
                (let desc = key + "_desc"
                keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l desc) OK)
            | false -> OK
        let traditionsres = 
            match traditions |> List.contains key with
            | true ->
                let desc = key + "_desc"
                let a = keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l desc) OK
                let delayed = key + "_delayed"
                let b = keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l delayed) OK
                a <&&> b
            | false -> OK
        let keyres = keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l key) OK
        keyres <&&> finishres <&&> adoptres <&&> traditionsres

    let processTradCat  (keys : (Lang * Set<string>) list) (cat : Node) =
        let key = cat.Key
        let start = cat.TagText "adoption_bonus"
        let finish = cat.TagText "finish_bonus"
        let vals = cat.Child "traditions" |> Option.map (fun c -> c.All |> List.choose (function |LeafValueI lv -> Some lv.Value |_ -> None )) |> Option.defaultValue []
        let traditions = vals |> List.map (function |QString s -> s |x -> x.ToString())
        let keyres = keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode cat keys l key) OK
        (keyres, start, finish, traditions)

    let valTraditionLocCats : LocalisationValidator = 
        fun entitySet keys nes -> 
            let cats = entitySet.GlobMatch("**/tradition_categories/*.txt") |> List.collect (fun e -> e.Children)
            let catres, starts, finishes, trads = cats |> List.map (processTradCat keys) |> List.fold (fun (rs, ss, fs, ts) (r, s, f, t) -> rs <&&> r, s::ss,  f::fs, ts @ t) (OK, [], [], [])
            let traditions = nes.GlobMatch("**/traditions/*.txt")  |> List.collect (fun e -> e.Children)
            let inner = fun tradition -> valTraditionLocs tradition keys starts finishes trads
            traditions |> List.map inner |> List.fold (<&&>) catres

    // let valTraditionLocCats (cats : Node list) (traditions : Node list) (keys : (Lang * Set<string>) list) =
    //     //eprintfn "%A" cats
    //     let catres, starts, finishes, trads = cats |> List.map (processTradCat keys) |> List.fold (fun (rs, ss, fs, ts) (r, s, f, t) -> rs <&&> r, s::ss,  f::fs, ts @ t) (OK, [], [], [])
    //     //eprintfn "%A %A %A" starts finishes trads
    //     let tradres = traditions |> List.fold (fun state trad -> state <&&> (valTraditionLocs trad keys starts finishes trads)) OK
    //     catres <&&> tradres

    let valArmiesLoc : LocalisationValidator =
        fun _ keys es ->
            let armies = es.GlobMatchChildren("**/common/armies/*.txt")
            let inner =
                fun (node : Node) ->
                    let army = node.Key
                    let armyplural = army + "_plural"
                    let armydesc = army + "_desc"
                    (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l army) OK)
                    <&&>
                    (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l armyplural) OK)
                    <&&>
                    (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l armydesc) OK)
            armies |> List.map inner |> List.fold (<&&>) OK

    let valArmyAttachmentLocs : LocalisationValidator =
        fun _ keys es ->
            let armies = es.GlobMatchChildren("**/common/army_attachments/*.txt")
            let inner =
                fun (node : Node) ->
                    let army = "army_attachment_"+node.Key
                    let armyplural = army + "_plural"
                    let armydesc = army + "_desc"
                    (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l army) OK)
                    <&&>
                    (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l armyplural) OK)
                    <&&>
                    (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode node keys l armydesc) OK)
            armies |> List.map inner |> List.fold (<&&>) OK

    let valDiploPhrases : LocalisationValidator =
        fun _ keys es ->
            let diplos = es.GlobMatchChildren("**/common/diplo_phrases/*.txt")
            let rec inner =
                fun (node : Node) ->
                    match node.Key with
                    | "greetings"
                    | "select"
                    | "propose"
                    | "accept"
                    | "consider"
                    | "refuse"
                    | "propose_vote" ->
                         node.Children |> List.map (fun c -> keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode c keys l c.Key) OK)  |> List.fold (<&&>) OK
                    | _ -> node.Children |> List.map inner |> List.fold (<&&>) OK
                    
            diplos |> List.collect (fun n -> n.Children) |> List.map inner |> List.fold (<&&>) OK


