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
open System.Xml.Linq
open System.Threading

module STLLocalisationValidation =
    type S = Severity
    type LocalisationValidator = EntitySet -> (Lang * Set<string>) list -> EntitySet -> ValidationResult

    let checkLocKey (leaf : Leaf) (keys : Set<string>) (lang : Lang) key =
        if lang = STL STLLang.Default then OK else
        match key = "" || key.Contains(" "), Set.contains key keys with
        | true, _ -> OK
        | _, true -> OK
        | _, false -> Invalid [inv (ErrorCodes.MissingLocalisation key (lang :> obj)) leaf]

    let checkLocName (leaf : Leaf) (keys : Set<string>) (lang : Lang) key  =
        match key = "" || key.Contains(" "), Set.contains key keys with
        | true, _ -> OK
        | _, true -> OK
        | _, false -> Invalid [inv (ErrorCodes.MissingLocalisation key (lang :> obj)) leaf]

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
                        match x.Key.ToLower(), x.Leafs "name" with
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
    

    let checkLocNode (keys : Set<string>) (lang : Lang) key (node : Node) =
        if lang = STL STLLang.Default then OK else
            match key = "" || key.Contains(" "), Set.contains key keys with
            | true, _ -> OK
            | _, true -> OK
            | _, false -> Invalid [inv (ErrorCodes.MissingLocalisation key (lang :> obj)) node]
    
    let checkLocNodeS (keys : (Lang * Set<string>) list) key (node : Node) =
         keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l key node) OK
        
    let checkKeyAndDesc (keys : (Lang * Set<string>) list) (node : Node) =
        let key = node.Key
        let desc = key + "_desc"
        let keyres = keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l key node) OK
        let descres = keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l desc node) OK
        keyres <&&> descres

    let checkLocNodeKeyAdv keys prefix suffix (node : Node) = 
        let key = prefix + node.Key + suffix
        (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l key node) OK)
    
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
                    let titles = event.Leafs "title" |> List.map (checkLocKeys keys) |> List.fold (<&&>) OK
                    let options = event.Childs "option" |> List.collect (fun o -> o.Leafs "name" |> List.map (checkLocKeys keys))
                                                        |> List.fold (<&&>) OK                
                    let usedKeys = event.Children |> List.fold (fun s c -> s <&&> (getLocKeys keys ["desc"; "text"; "custom_tooltip"; "fail_text"; "response_text"] c)) OK
                    let nameres = valEventNameLocs keys event
                    titles <&&> options <&&> usedKeys <&&> nameres
            es <&!&> inner
        
    let valTechLocs : LocalisationValidator =
        fun _ keys es ->
            let entities = es.GlobMatchChildren("**/common/technology/*.txt")
            entities |> List.map
                (fun (node : Node) ->
                let keyres = checkKeyAndDesc keys node
                let innerKeys = node.Childs "prereqfor_desc" |> List.fold (fun s c -> s <&&> (getLocKeys keys ["desc"; "title"] c)) OK
                let flags = node.Child "feature_flags" |> Option.map (fun c -> c.All |> List.choose (function |LeafValueI lv -> Some lv.Value |_ -> None )) |> Option.defaultValue []
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
                        match node.TagText "hidden" with
                        | "yes" -> OK
                        | _ -> node.Leafs "key" |> List.fold (fun s l -> s <&&> (checkLocKeys keys l)) OK
                    let auras = node.Childs "friendly_aura" @ node.Childs "hostile_aura"
                    let aurares = auras |> List.fold (fun s c -> s <&&> (getLocKeys keys ["name"] c)) OK
                    keyres <&&> aurares
            entities |> List.map inner |> List.fold (<&&>) OK


    let valBuildingLocs : LocalisationValidator =
        fun _ keys es ->
            let entities = es.GlobMatchChildren("**/common/buildings/*.txt")
            let inner =
                fun node ->
                    let keyres = checkKeyAndDesc keys node
                    //let failtext = node.Children <&!&> ((checkLocNodeTag keys "text") <&> (checkLocNodeTag keys "fail_text"))
                    //let failtext = node.Children |> List.map ((checkLocNodeTag keys "text") <&> (checkLocNodeTag keys "fail_text")) |> List.fold (<&&>) OK
                    let failtext = node.Children <&!&> (getLocKeys keys ["text"; "fail_text"])
                    keyres <&&> failtext
            entities |> List.map inner |> List.fold (<&&>) OK

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
        let vals = cat.Child "traditions" |> Option.map (fun c -> c.All |> List.choose (function |LeafValueI lv -> Some lv.Value |_ -> None )) |> Option.defaultValue []
        let traditions = vals |> List.map (function |QString s -> s |x -> x.ToString())
        let keyres = keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l key cat) OK
        (start, finish, traditions)

    let valTraditionLocCats : LocalisationValidator = 
        fun entitySet keys nes -> 
            let cats = entitySet.GlobMatch("**/tradition_categories/*.txt") |> List.collect (fun e -> e.Children)
            let newcats = nes.GlobMatch("**/tradition_categories/*.txt") |> List.collect (fun e -> e.Children)
            let starts, finishes, trads = cats |> List.map (processTradCat keys) |> List.fold (fun ( ss, fs, ts) (s, f, t) -> s::ss,  f::fs, ts @ t) ([], [], [])
            let traditions = nes.GlobMatch("**/traditions/*.txt")  |> List.collect (fun e -> e.Children)
            let inner = fun tradition -> valTraditionLocs tradition keys starts finishes trads
            let innerCat = fun (cat : Node) -> keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l cat.Key cat) OK
            newcats |> List.map innerCat |> List.fold (<&&>) OK
            <&&>
            (traditions |> List.map inner |> List.fold (<&&>) OK)

    // let valTraditionLocCats (cats : Node list) (traditions : Node list) (keys : (Lang * Set<string>) list) =
    //     //eprintfn "%A" cats
    //     let catres, starts, finishes, trads = cats |> List.map (processTradCat keys) |> List.fold (fun (rs, ss, fs, ts) (r, s, f, t) -> rs <&&> r, s::ss,  f::fs, ts @ t) (OK, [], [], [])
    //     //eprintfn "%A %A %A" starts finishes trads
    //     let tradres = traditions |> List.fold (fun state trad -> state <&&> (valTraditionLocs trad keys starts finishes trads)) OK
    //     catres <&&> tradres


    let valArmiesLoc : LocalisationValidator =
        fun _ keys es ->
            let armies = es.GlobMatchChildren("**/common/armies/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""; "_plural"; "_desc"]
            armies <&!&> inner

    let valArmyAttachmentLocs : LocalisationValidator =
        fun _ keys es ->
            let armies = es.GlobMatchChildren("**/common/army_attachments/*.txt")
            let inner = checkLocNodeKeyAdvs keys "army_attachment_" [""; "_desc"]
            armies <&!&> inner

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
                        node.Children |> List.map (fun c -> keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l c.Key c) OK)  |> List.fold (<&&>) OK
                         //node.Children <&!&> (fun c -> keys <&!&> (fun l -> checkLocNode keys l c.Key c)
                    | _ -> node.Children <&!&> inner
                    
            diplos <&!&> inner

    let valShipLoc : LocalisationValidator =
        fun _ keys es ->
            let ships = es.GlobMatchChildren("**/common/ship_sizes/*.txt")
            let inner1 = checkLocNodeKeyAdvs keys "" [""; "_plural"]
            let inner2 = checkLocNodeKeyAdvs keys "shipsize_" ["_construction_speed_mult"; "_build_cost_mult"; "_upkeep_mult"]
            ships <&!&> inner1
            <&&>
            (ships <&!&> inner2)

    let valFactionDemands : LocalisationValidator =
        fun _ keys es ->
            let factions = es.GlobMatchChildren("**/common/pop_faction_types/*.txt")
            let demands = factions |> List.collect (fun f -> f.Childs "demand")
            let actions = factions |> List.collect (fun f -> f.Childs "actions")
            let inner = fun c -> (getLocKeys keys ["title"; "desc"; "unfulfilled_title"] c)
            let finner = checkLocNodeKeyAdvs keys "pft_" [""; "_desc"]
            demands <&!&> inner
            <&&>
            (factions <&!&> finner)
            <&&>
            (actions <&!&> (getLocKeys keys ["name"; "title"; "description"; "text"; "custom_tooltip"; "fail_text"; "response_text"]))


    let valSpeciesRightsLocs : LocalisationValidator =
        fun _ keys es ->
            let species = es.GlobMatchChildren("**/common/species_rights/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""; "_tooltip";"_tooltip_delayed"] <&> getLocKeys keys ["text"; "fail_text"]
            species <&!&> inner


    let valMapsLocs : LocalisationValidator = 
        fun _ keys es ->
            let maps = es.GlobMatchChildren("**/map/setup_scenarios/*.txt")
            let inner = checkLocNodeTag keys "name"
            maps <&!&> inner

    let valMegastructureLocs : LocalisationValidator = 
        fun _ keys es ->
            let megas = es.GlobMatchChildren("**/common/megastructures/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""; "_DESC"; "_MEGASTRUCTURE_DETAILS"; "_CONSTRUCTION_INFO_DELAYED"] <&> getLocKeys keys ["text"; "fail_text"]
            megas <&!&> inner
    
    let valModifiers : LocalisationValidator =
        fun _ keys es ->
            let mods = es.GlobMatchChildren("**/common/static_modifiers/*.txt")
            mods <&!&> (checkLocNodeKeyAdvs keys "" [""])
            //TODO: Add desc back behind a "strict" flag
           // mods |> List.fold (fun s c -> s <&&> checkKeyAndDesc c keys) OK

    let valModules : LocalisationValidator = 
        fun _ keys es ->
            let mods = es.GlobMatchChildren("**/common/spaceport_modules/*.txt")
            let inner = checkLocNodeKeyAdvs keys "sm_" [""]
            mods <&!&> inner

    let valTraits : LocalisationValidator =
        fun _ keys es ->
            let traits = es.GlobMatchChildren("**/common/traits/*.txt")
            traits <&!&> (checkKeyAndDesc keys)

    let valGoverments : LocalisationValidator =
        fun _ keys es ->
            let govs = es.GlobMatchChildren("**/common/governments/*.txt")
            let civics = es.GlobMatchChildren("**/common/governments/civics/*.txt")
            let ginner = checkLocNodeKeyAdvs keys "" [""] <&> getLocKeys keys ["ruler_title"; "ruler_title_female"; "heir_title"; "heir_title_female"]
            civics <&!&> checkKeyAndDesc keys
            <&&>
            (civics <&!&> getLocKeys keys ["description"])
            <&&>
            (govs <&!&> ginner)
            
    let valPersonalities : LocalisationValidator =
        fun _ keys es ->
            let pers = es.GlobMatchChildren("**/common/personalities/*.txt")
            let inner = checkLocNodeKeyAdvs keys "personality_" [""; "_desc"]
            pers <&!&> inner

    let valEthics : LocalisationValidator =
        fun _ keys es ->
            let ethics = es.GlobMatchChildren("**/common/ethics/*.txt")
            ethics  |> List.filter (fun e -> e.Key <> "ethic_categories")
                    <&!&> checkKeyAndDesc keys

    let valPlanetClasses : LocalisationValidator =
        fun _ keys es ->
            let planets = es.GlobMatchChildren("**/common/planet_classes/*.txt")
            let inner =
                fun (node : Node) ->
                    let colonizable = node.Tag "colonizable" |> (function |Some (Bool b) when b -> true |_ -> false)
                    checkLocNodeKeyAdvs keys "" [""; "_desc"] node
                    <&&>
                    if not colonizable then OK else
                        checkLocNodeKeyAdvs keys "" ["_tile"; "_tile_desc"; "_habitability"] node
                        <&&>
                        checkLocNodeKeyAdvs keys "trait_" ["_preference"; "_preference_desc"] node
            planets 
                |> List.filter (fun n -> n.Key <> "random_list")
                <&!&> inner

    let valEdicts : LocalisationValidator =
        fun _ keys es ->
            let edicts = es.GlobMatchChildren("**/common/edicts/*.txt")
            let inner = checkLocNodeTagAdvs keys "edict_" [""; "_desc"] "name"
            edicts <&!&> inner

    let valPolicies : LocalisationValidator =
        fun _ keys es ->
            let policies = es.GlobMatchChildren("**/common/policies/*.txt")
            let options = policies |> List.collect (fun p -> p.Childs "option")
            let inner = checkLocNodeKeyAdvs keys "policy_" [""; "_desc"]
            let oinner =
                fun (node : Node) ->
                    let vals = node.Child "policy_flags" |> Option.map (fun c -> c.All |> List.choose (function |LeafValueI lv -> Some lv.Value |_ -> None )) |> Option.defaultValue []
                    let vals2 = vals |> List.map (fun v -> v.ToString() + "_name")
                    vals2 |> List.fold (fun s c -> s <&&> (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l c node) OK)) OK
                    <&&>
                    (checkLocNodeTagAdvs keys "" [""; "_desc"] "name" node)
            policies <&!&> inner
            <&&>
            (options <&!&> oinner)

    let valSectionTemplates : LocalisationValidator =
        fun _ keys es ->
            let secs = es.GlobMatchChildren("**/common/section_templates/*.txt")
            secs <&!&> getLocKeys keys ["key"]

    let valSpeciesNames : LocalisationValidator =
        fun _ keys es ->
            let species = es.GlobMatchChildren("**/common/species_names/*.txt")
            let inner = 
                fun (node : Node) ->
                    let key = node.Key
                    let suff = ["_desc"; "_plural"; "_insult_01"; "_insult_plural_01"; "_compliment_01";"_compliment_plural_01";"_spawn";"_spawn_plural";
                                "_sound_01";"_sound_02";"_sound_03";"_sound_04";"_sound_05";"_organ";"_mouth"]
                    suff |> List.fold (fun s c -> s <&&> (keys |> List.fold (fun state (l, keys)  -> state <&&> checkLocNode keys l (key + c) node) OK)) OK
            species |> List.filter (fun s -> s.Key <> "named_lists")
                    <&!&> inner

    let valStratRes : LocalisationValidator =
        fun _ keys es ->
            let res = es.GlobMatchChildren("**/common/strategic_resources/*.txt") |> List.filter (fun r -> r.Key <> "time")
            res <&!&> (checkKeyAndDesc keys)

    let valAmbient : LocalisationValidator =
        fun _ keys es ->
            let ams = es.GlobMatchChildren("**/common/ambient_objects/*.txt")
            let inner (node : Node) = 
                if node.Tag "show_name" |> (function |Some (Bool b) when b -> true |_ -> false) then checkLocNodeTag keys "name" node else OK
                <&&>
                if node.Tag "selectable" |> (function |Some (Bool b) when b -> true |_ -> false) then 
                    (checkLocNodeTag keys "tooltip" node)
                    <&&>
                    (checkLocNodeTag keys "description" node)
                    else OK
            ams <&!&> inner