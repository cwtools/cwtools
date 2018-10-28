namespace CWTools.Parser


open FParsec
open CWTools.Utilities.Position
open Types
open CWTools.Common
open CWTools.Common.STLConstants
open System.IO
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Utilities.Utils
open System
open System.Globalization


module rec ConfigParser =
    type ValueType =
    | Float of float * float
    | Bool
    | Scalar
    | Int of int * int
    | Enum of string
    | Specific of string
    | Percent
    type NewField<'a> =
    | ValueField of ValueType
    | TypeField of string
    | ScopeField of 'a
    | LocalisationField of synced : bool
    | FilepathField
    | IconField of string
    | AliasField of string
    | SubtypeField of string * bool * NewRule<'a> list
    let specificField x = ValueField(ValueType.Specific x)
    type Options<'a> = {
        min : int
        max : int
        leafvalue : bool
        description : string option
        pushScope : 'a option
        replaceScopes : ReplaceScopes<'a> option
        severity : Severity option
        requiredScopes : 'a list
    }
    type RuleType<'a> =
    |NodeRule of left : NewField<'a> * rules : NewRule<'a> list
    |LeafRule of left : NewField<'a> * right : NewField<'a>
    |LeafValueRule of right : NewField<'a>
    |SubtypeRule of string * bool * NewRule<'a> list
    type NewRule<'a> = RuleType<'a> * Options<'a>
    // type ObjectType =
    // | Tech
    // | ShipSize
    // | StarbaseModule
    // type Rule = string * Options * Field
    // type Field =
    // | ValueField of ValueType
    // | ObjectField of EntityType
    // | TypeField of string
    // | LeftTypeField of string * Field
    // | ClauseField of Rule list
    // | LeftClauseField of ValueType * Field
    // | ScopeField of Scope
    // | LeftScopeField of Rule list
    // | LocalisationField of synced : bool
    // | FilepathField
    // | AliasField of string
    // | SubtypeField of string * bool * Field

    type RootRule<'a> =
    | AliasRule of string * NewRule<'a>
    | TypeRule of string * NewRule<'a>
    // type EffectRule = Rule // Add scopes
    type ReplaceScopes<'a> = {
        root : 'a option
        this : 'a option
        froms : 'a list option
    }
    type SubTypeDefinition<'a> = {
        name : string
        rules : NewRule<'a> list
        typeKeyField : string option
        pushScope : 'a option
    }
    type TypeDefinition<'a> = {
        name : string
        nameField : string option
        filenameName : bool
        path : string
        path_strict : bool
        path_file : string option
        conditions : Node option
        subtypes : SubTypeDefinition<'a> list
        typeKeyFilter : (string * bool) option
        skipRootKey : string option
        warningOnly : bool
    }
    type EnumDefinition = string * string list
    type ComplexEnumDef = {
        name : string
        path : string
        nameTree : Node
    }
    let parseSeverity =
        function
        |"error" -> Severity.Error
        |"warning" -> Severity.Warning
        |"info" -> Severity.Information
        |"hint" -> Severity.Hint
        |s -> failwithf "Invalid severity %s" s
    let defaultOptions = { min = 0; max = 1000; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = [] }
    let requiredSingle = { defaultOptions with min = 1; max = 1 }
    let requiredMany<'a> = { defaultOptions with min = 1; max = 100 }
    let optionalSingle = { defaultOptions with min = 0; max = 1 }
    let optionalMany = { defaultOptions with min = 0; max = 100 }

    let defaultFloat = ValueField (ValueType.Float (-1E+12, 1E+12))
    let defaultInt = ValueField (ValueType.Int (Int32.MinValue, Int32.MaxValue))

    let getNodeComments (node : Node) =
        let findComments t s (a : Child) =
                match (s, a) with
                | ((b, c), _) when b -> (b, c)
                | ((_, c), CommentC nc) when nc.StartsWith("#") -> (false, nc::c)
                | ((_, c), CommentC nc) -> (false, c)
                | ((_, c), NodeC n) when n.Position = t -> (true, c)
                | ((_, c), LeafC v) when v.Position = t -> (true, c)
                | ((_, c), LeafValueC v) when v.Position = t -> (true, c)
                // | ((_, c), LeafValueC lv) when lv.Position = t -> (true, c)
                | ((_, _), _) -> (false, [])
        //let fNode = (fun (node:Node) (children) ->
        let one = node.Values |> List.map (fun e -> LeafC e, node.All |> List.fold (findComments e.Position) (false, []) |> snd)
        //eprintfn "%s %A" node.Key (node.All |> List.rev)
        //eprintfn "%A" one
        let two = node.Children |> List.map (fun e -> NodeC e, node.All |> List.fold (findComments e.Position) (false, []) |> snd |> (fun l -> (l)))
        let three = node.LeafValues |> Seq.toList |> List.map (fun e -> LeafValueC e, node.All |> List.fold (findComments e.Position) (false, []) |> snd)
        let new2 = one @ two @ three
        new2

    let getSettingFromString (full : string) (key : string) =
        let setting = full.Substring(key.Length)
        if not (setting.StartsWith "[" && setting.EndsWith "]") then None else
            Some (setting.Substring(1, setting.Length - 2))

    let getFloatSettingFromString (full : string) =
        match getSettingFromString full "float" with
        |Some s ->
            let split = s.Split([|".."|], 2, StringSplitOptions.None)
            if split.Length < 2 then None else
                try
                    Some ((float split.[0]), (float split.[1]))
                with
                |_ -> None
        |None -> None


    let getIntSettingFromString (full : string) =
        match getSettingFromString full "int" with
        |Some s ->
            let split = s.Split([|".."|], 2, StringSplitOptions.None)
            if split.Length < 2 then None else
                try
                    Some ((int split.[0]), (int split.[1]))
                with
                |_ -> None
        |None -> None

    let getAliasSettingsFromString (full : string) =
        match getSettingFromString full "alias" with
        |Some s ->
            let split = s.Split([|":"|], 2, StringSplitOptions.None)
            if split.Length < 2 then None else Some (split.[0], split.[1])
        |None -> None



    let getOptionsFromComments (parseScope) (allScopes) (anyScope) (comments : string list) =
        let min, max =
            match comments |> List.tryFind (fun s -> s.Contains("cardinality")) with
            |Some c ->
                let nums = c.Substring(c.IndexOf "=" + 1).Trim().Split([|".."|], 2, StringSplitOptions.None)
                try
                    match nums.[0], nums.[1] with
                    |min, "inf" -> (int min), 1000
                    |min, max -> (int min), (int max)
                with
                |_ -> 1, 1
            |None -> 1, 1
        let description =
            match comments |> List.tryFind (fun s -> s.StartsWith "##") with
            |Some d -> Some (d.Trim('#'))
            |None -> None
        let pushScope =
            match comments |> List.tryFind (fun s -> s.Contains("push_scope")) with
            |Some s -> s.Substring(s.IndexOf "=" + 1).Trim() |> parseScope |> Some
            |None -> None
        let reqScope =
            match comments |> List.tryFind (fun s -> s.StartsWith("# scope =")) with
            |Some s ->
                let scope =  s.Substring(s.IndexOf "=" + 1).Trim() |> parseScope
                if scope = anyScope then allScopes else [scope]
            |None -> []
        let severity =
            match comments |> List.tryFind (fun s -> s.Contains("severity")) with
            |Some s -> s.Substring(s.IndexOf "=" + 1).Trim() |> parseSeverity |> Some
            |None -> None
        let replaceScopes =
            match comments |> List.tryFind (fun s -> s.Contains("replace_scope")) with
            |Some s ->
                let s = s.Trim('#')
                let parsed = CKParser.parseString s "config"
                match parsed with
                |Failure(_) -> None
                |Success(s,_,_) ->
                    let n = (STLProcess.shipProcess.ProcessNode<Node> EntityType.Other "root" (mkZeroFile "config") s)
                    match n.Child "replace_scope" with
                    |Some c ->
                        let this = if c.Has "this" then c.TagText "this" |> parseScope |> Some else None
                        let root = if c.Has "root" then c.TagText "root" |> parseScope |> Some else None
                        let from = if c.Has "from" then c.TagText "from" |> parseScope |> Some else None
                        let fromfrom = if c.Has "fromfrom" then c.TagText "fromfrom" |> parseScope |> Some else None
                        let fromfromfrom = if c.Has "fromfromfrom" then c.TagText "fromfromfrom" |> parseScope |> Some else None
                        let fromfromfromfrom = if c.Has "fromfromfromfrom" then c.TagText "fromfromfromfrom" |> parseScope |> Some else None
                        let froms = [from;fromfrom;fromfromfrom;fromfromfromfrom] |> List.choose id
                        Some { root = root; this = this; froms = Some froms }
                    |None -> None
            |None -> None
        { min = min; max = max; leafvalue = false; description = description; pushScope = pushScope; replaceScopes = replaceScopes; severity = severity; requiredScopes = reqScope }

    let processKey parseScope anyScope =
        function
        |"scalar" -> ValueField ValueType.Scalar
        |"bool" -> ValueField ValueType.Bool
        |"percentage_field" -> ValueField ValueType.Percent
        |"localisation" -> LocalisationField false
        |"localisation_synced" -> LocalisationField true
        |"filepath" -> FilepathField
        |x when x.StartsWith "<" && x.EndsWith ">" ->
            TypeField (x.Trim([|'<'; '>'|]))
        |"int" -> defaultInt
        |x when x.StartsWith "int[" ->
            match getIntSettingFromString x with
            |Some (min, max) -> ValueField (ValueType.Int (min, max))
            |None -> (defaultInt)
        |"float" -> defaultFloat
        |x when x.StartsWith "float" ->
            match getFloatSettingFromString x with
            |Some (min, max) -> ValueField (ValueType.Float (min, max))
            |None -> (defaultFloat)
        |x when x.StartsWith "enum[" ->
            match getSettingFromString x "enum" with
            |Some (name) -> ValueField (ValueType.Enum name)
            |None -> ValueField (ValueType.Enum "")
        |x when x.StartsWith "icon[" ->
            match getSettingFromString x "icon" with
            |Some (folder) -> IconField folder
            |None -> ValueField (ValueType.Scalar)
        |x when x.StartsWith "alias_match_left[" ->
            match getSettingFromString x "alias_match_left" with
            |Some alias -> AliasField alias
            |None -> ValueField ValueType.Scalar
        |x when x.StartsWith "alias_name[" ->
            match getSettingFromString x "alias_name" with
            |Some alias -> AliasField alias
            |None -> ValueField ValueType.Scalar
        |"scope_field" -> ScopeField (anyScope)
        |x when x.StartsWith "scope[" ->
            match getSettingFromString x "scope" with
            |Some target ->
                ScopeField (parseScope target)
            |None -> ValueField ValueType.Scalar
        |x when x.StartsWith "event_target" ->
            match getSettingFromString x "event_target" with
            |Some target ->
                ScopeField (parseScope target)
            |None -> ValueField ValueType.Scalar
        |x -> ValueField (ValueType.Specific (x.Trim([|'\"'|])))


    let processChildConfig (parseScope) allScopes (anyScope) ((child, comments) : Child * string list)  =
        match child with
        |NodeC n -> Some (configNode parseScope allScopes anyScope n comments (n.Key))
        |LeafC l -> Some (configLeaf parseScope allScopes anyScope l comments (l.Key))
        |LeafValueC lv -> Some (configLeafValue parseScope allScopes anyScope lv comments)
        |_ -> None

    let configNode (parseScope) (allScopes) (anyScope) (node : Node) (comments : string list) (key : string) =
        let children = getNodeComments node
        let options = getOptionsFromComments parseScope allScopes anyScope comments
        let innerRules = children |> List.choose (processChildConfig parseScope allScopes anyScope)
        let rule =
            match key with
            |x when x.StartsWith "subtype[" ->
                match getSettingFromString x "subtype" with
                |Some st when st.StartsWith "!" -> SubtypeRule (st.Substring(1), false, (innerRules))
                |Some st -> SubtypeRule (st, true, (innerRules))
                |None -> failwith (sprintf "Invalid subtype string %s" x)
            |x -> NodeRule(processKey parseScope anyScope x, innerRules)
            // |"int" -> NodeRule(ValueField(ValueType.Int(Int32.MinValue, Int32.MaxValue)), innerRules)
            // |"float" -> NodeRule(ValueField(ValueType.Float(Double.MinValue, Double.MaxValue)), innerRules)
            // |"scalar" -> NodeRule(ValueField(ValueType.Scalar), innerRules)
            // |"filepath" -> NodeRule(FilepathField, innerRules)
            // |"scope" -> NodeRule(ScopeField(Scope.Any), innerRules)
            // |x when x.StartsWith "enum[" ->
            //     match getSettingFromString x "enum" with
            //     |Some e -> NodeRule(ValueField(ValueType.Enum e), innerRules)
            //     |None -> failwith (sprintf "Invalid enum string %s" x)
            // |x when x.StartsWith "<" && x.EndsWith ">" ->
            //     NodeRule(TypeField(x.Trim([|'<'; '>'|])), innerRules)
            // |x -> NodeRule(ValueField(ValueType.Specific x), innerRules)
        NewRule(rule, options)

    let processChildConfigRoot (parseScope) (allScopes) (anyScope) ((child, comments) : Child * string list) =
        match child with
        |NodeC n when n.Key == "types" -> None
        |NodeC n -> Some (configRootNode parseScope allScopes anyScope n comments)
        |LeafC l -> Some (configRootLeaf parseScope allScopes anyScope l comments)
        //|LeafValueC lv -> Some (configLeafValue lv comments)
        |_ -> None

    let configRootLeaf (parseScope) allScopes (anyScope) (leaf : Leaf) (comments : string list) =
        let rule = configLeaf parseScope allScopes anyScope leaf comments leaf.Key
        match leaf.Key with
        |x when x.StartsWith "alias[" ->
            match getAliasSettingsFromString x with
            |Some (a, rn) ->
                let innerRule = configLeaf parseScope allScopes anyScope leaf comments rn
                AliasRule (a, innerRule)
            |None ->
                TypeRule (x, rule)
        |x ->
            TypeRule (x, rule)

    let configRootNode (parseScope) allScopes (anyScope) (node : Node) (comments : string list) =
        let children = getNodeComments node
        let options = getOptionsFromComments parseScope allScopes anyScope comments
        let innerRules = children |> List.choose (processChildConfig parseScope allScopes anyScope)
        match node.Key with
        |x when x.StartsWith "alias[" ->
            match getAliasSettingsFromString x with
            |Some (a, rn) ->
                let innerRule = configNode parseScope allScopes anyScope node comments rn
                // eprintfn "%s %A" a innerRule
                AliasRule (a, innerRule)
            |None ->
                TypeRule (x, NewRule(NodeRule(ValueField(ValueType.Specific x), innerRules), options))
        |x ->
            TypeRule (x, NewRule(NodeRule(ValueField(ValueType.Specific x), innerRules), options))

    let rgbRule = LeafValueRule (ValueField (ValueType.Int (0, 255))), { min = 3; max = 4; leafvalue = true; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = [] }
    let hsvRule = LeafValueRule (ValueField (ValueType.Float (0.0, 2.0))), { min = 3; max = 4; leafvalue = true; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = [] }

    let configLeaf (parseScope) (allScopes) (anyScope) (leaf : Leaf) (comments : string list) (key : string) =
        let leftfield = processKey parseScope anyScope key
        let options = getOptionsFromComments parseScope allScopes anyScope comments
        let rightkey = leaf.Value.ToString()
        match rightkey with
        |x when x.StartsWith("colour[") ->
            let colourRules =
                match getSettingFromString x "colour" with
                |Some "rgb" -> [rgbRule]
                |Some "hsv" -> [hsvRule]
                |_ -> [rgbRule; hsvRule]
            NewRule(NodeRule(leftfield, colourRules), options)
        |x ->
            let rightfield = processKey parseScope anyScope rightkey
            let leafRule = LeafRule(leftfield, rightfield)
            NewRule(leafRule, options)

    let configLeafValue (parseScope) allScopes (anyScope) (leafvalue : LeafValue) (comments : string list) =
        let field = processKey parseScope anyScope (leafvalue.Value.ToRawString())
            // match leafvalue.Value.ToRawString() with
            // |x when x.StartsWith "<" && x.EndsWith ">" ->
            //     TypeField (x.Trim([|'<'; '>'|]))
            // |x -> ValueField (ValueType.Enum x)
        let options = { getOptionsFromComments parseScope allScopes anyScope comments with leafvalue = true }
        NewRule(LeafValueRule(field), options)

    // Types

    let processType (parseScope) (allScopes) (anyScope) (node : Node) (comments : string list) =
        let parseSubType ((child : Child), comments : string list) =
            match child with
            |NodeC subtype when subtype.Key.StartsWith "subtype" ->
                let typekeyfilter =
                    match comments |> List.tryFind (fun s -> s.Contains "type_key_filter") with
                    |Some c -> Some (c.Substring(c.IndexOf "=" + 1).Trim())
                    |None -> None
                let pushScope =
                    match comments |> List.tryFind (fun s -> s.Contains("push_scope")) with
                    |Some s -> s.Substring(s.IndexOf "=" + 1).Trim() |> parseScope |> Some
                    |None -> None
                match getSettingFromString (subtype.Key) "subtype" with
                |Some key -> Some { name = key; rules =  (getNodeComments subtype |> List.choose (processChildConfig parseScope allScopes anyScope)); typeKeyField = typekeyfilter; pushScope = pushScope }
                |None -> None
            |_ -> None
        match node.Key with
        |x when x.StartsWith("type") ->
            let typename = getSettingFromString node.Key "type"
            let namefield = if node.Has "name_field" then Some (node.TagText "name_field") else None
            let filenameName = node.TagText "name_from_file" == "yes"
            let path = (node.TagText "path").Replace("game/","").Replace("game\\","")
            let path_strict = node.TagText "path_strict" == "yes"
            let path_file = if node.Has "path_file" then Some (node.TagText "path_file") else None
            let skiprootkey = if node.Has "skip_root_key" then Some (node.TagText "skip_root_key") else None
            let subtypes = getNodeComments node |> List.choose parseSubType
            let warningOnly = node.TagText "severity" == "warning"
            //eprintfn "cs %A" comments
            let typekeyfilter =
                match comments |> List.tryFind (fun s -> s.Contains "type_key_filter") with
                |Some c ->
                    //eprintfn "c %A" c
                    match c.Contains "=", c.Contains "<>" with
                    |true, _ -> Some (c.Substring(c.IndexOf "=" + 1).Trim(), false)
                    |_, true -> Some (c.Substring(c.IndexOf "<>" + 2).Trim(), true)
                    |_ -> None
                |None -> None
            match typename with
            |Some tn -> Some { name = tn; nameField = namefield; filenameName = filenameName; path = path; path_file = path_file; conditions = None; subtypes = subtypes; typeKeyFilter = typekeyfilter; skipRootKey = skiprootkey; warningOnly = warningOnly; path_strict = path_strict}
            |None -> None
        |_ -> None



    let processChildType (parseScope) allScopes (anyScope) ((child, comments) : Child * string list) =
        match child with
        |NodeC n when n.Key == "types" ->
            let inner ((child2, comments2) : Child * string list) =
                match child2 with
                |NodeC n2 -> (processType parseScope allScopes anyScope n2 comments2)
                |_ -> None
            Some (getNodeComments n |> List.choose inner)
        |_ -> None

    let processEnum (node : Node) (comments : string list) =
        match node.Key with
        |x when x.StartsWith("enum") ->
            let enumname = getSettingFromString node.Key "enum"
            let values = node.LeafValues |> List.ofSeq |> List.map (fun lv -> lv.Value.ToString().Trim([|'\"'|]))
            match enumname with
            |Some en -> Some (en, values)
            |None -> None
        |_ -> None

    let processChildEnum ((child, comments) : Child * string list) =
        match child with
        |NodeC n when n.Key == "enums" ->
            let inner ((child2, comments2) : Child * string list) =
                match child2 with
                |NodeC n2 -> (processEnum n2 comments2)
                |_ -> None
            Some (getNodeComments n |> List.choose inner)
        |_ -> None

    let processComplexEnum (node : Node) (comments : string list) =
        match node.Key with
        |x when x.StartsWith("complex_enum") ->
            let enumname = getSettingFromString node.Key "complex_enum"
            let path = (node.TagText "path").Replace("game/","").Replace("game\\","")
            let nametree = node.Child "name"
            match enumname, nametree with
            |Some en, Some nt -> Some {name = en; path = path; nameTree = nt}
            |_ -> None
        |_ -> None

    let processComplexChildEnum ((child, comments) : Child * string list) =
        match child with
        |NodeC n when n.Key == "enums" ->
            let inner ((child2, comments2) : Child * string list) =
                match child2 with
                |NodeC n2 -> (processComplexEnum n2 comments2)
                |_ -> None
            Some (getNodeComments n |> List.choose inner)
        |_ -> None

    let processConfig (parseScope) (allScopes) (anyScope) (node : Node) =
        let nodes = getNodeComments node
        let rules = nodes |> List.choose (processChildConfigRoot parseScope allScopes anyScope)
        let types = nodes |> List.choose (processChildType parseScope allScopes anyScope) |> List.collect id
        let enums = nodes |> List.choose processChildEnum |> List.collect id
        let complexenums = nodes |> List.choose processComplexChildEnum |> List.collect id
        rules, types, enums, complexenums

    let parseConfig (parseScope) (allScopes) (anyScope) filename fileString =
        //eprintfn "parse"
        let parsed = CKParser.parseString fileString filename
        match parsed with
        |Failure(e, _, _) -> eprintfn "config file %s failed with %s" filename e; ([], [], [], [])
        |Success(s,_,_) ->
            //eprintfn "parsed %A" s
            let root = simpleProcess.ProcessNode<Node>() "root" (mkZeroFile filename) (s |> List.rev)
            //eprintfn "processConfig"
            processConfig parseScope allScopes anyScope root



// create_starbase = {
// 	owner = <target>
// 	size = <ship_size>
// 	module = <starbase_module>
// 	building = <starbase_building>
// 	effect = { ... }
// }
    let createStarbase =
        let owner = NewRule (LeafRule(specificField "owner", ScopeField Scope.Any), requiredSingle)
        let size = NewRule (LeafRule(specificField "size", ValueField(ValueType.Enum "size")), requiredSingle)
        let moduleR = NewRule (LeafRule(specificField "module", ValueField(ValueType.Enum "module")), optionalMany)
        let building = NewRule (LeafRule(specificField "building", ValueField(ValueType.Enum "building")), optionalMany)
        let effect = NewRule (NodeRule(specificField "effect", [(LeafRule (AliasField "effect", AliasField "effect")), optionalMany]), { optionalSingle with replaceScopes = Some { froms = None; root = Some (Scope.Country); this = Some (Scope.Country) }})
        let rule = NewRule (NodeRule(specificField "create_starbase", [owner; size; moduleR; building; effect]), optionalMany)
        rule
    let createStarbaseAlias = AliasRule ("effect", createStarbase)
    let createStarbaseEnums =
        [("size", ["medium"; "large"]);
         ("module", ["trafficControl"]);
         ("building", ["crew"])]
        |> Map.ofList
    let createStarbaseTypeDef =
        {
            name = "create_starbase"
            nameField = None
            filenameName = false
            path = "events"
            path_strict = false
            path_file = None
            conditions = None
            subtypes = []
            typeKeyFilter = None
            skipRootKey = None
            warningOnly = false
        }
// # strategic_resource: strategic resource, deprecated, strategic resource used by the building.
// # allow: trigger to check for allowing construction of building.
// # prerequisites: Tech requirements for building.
// # empire_unique: boolean, can only build one if set to true.
// # cost: resource table, cost of building.
// # is_orbital: boolean, can only be built in orbital station.
// # modifier: modifier, deprecated, applies a modifier to planet; use planet_modifier instead.
// # planet_modifier, country_modifier, army_modifier: applies modifier to planet/country/armies
// # triggered_planet_modifier = { key (optional), potential (scope: planet), modifier }: applies conditional modifier to planet
// # base_buildtime: int, number of days for construction.
// # requires_pop, boolean, building will require a pop for production.
// # required_resources, resource table, required resources for production.
// # produced_resources, resource table, produced resources in production.
// # upgrades, buildings list, buildings this building can upgrade into.
// # is_listed, boolean, toggles if this building is shown in the non-upgrade buildable list.
// # planet_unique, toggles if one can build multiple of this type on a single planet.
// # ai_weight, weight for AI, default is set to one, weight set to 0 means that AI will never build it
// # is_colony: trigger to check if the building is a colony shelter for country (scope: country, from: planet). default: "always = no"
// # active: trigger to check if a building can be active with a given pop worker (scope: pop) if you add a trigger here, you should also add the requirements in the description
// # show_tech_unlock_if: trigger to show this building only conditionally in the technology screen. scope: country. default: { always = yes }
// # planet_modifier_with_pop_trigger = { key (optional), potential (scope: pop), modifier }: applies modifier to pops on planet that satisfy condition in trigger

    let building =
        let inner =
            [
                NewRule (LeafRule(specificField "allow", ValueField ValueType.Scalar), requiredSingle)
                NewRule (LeafRule(specificField "empire_unique", ValueField ValueType.Bool), optionalSingle)
            ]
        NewRule(NodeRule(specificField "building", inner), optionalMany)

    // formation_priority = @corvette_formation_priority
    // max_speed = @speed_very_fast
    // acceleration = 0.35
    // rotation_speed = 0.1
    // collision_radius = @corvette_collision_radius
    // max_hitpoints = 300
    // modifier = {
    // 	ship_evasion_add = 60
    // }
    // size_multiplier = 1
    // fleet_slot_size = 1
    // section_slots = { "mid" = { locator = "part1" } }
    // num_target_locators = 2
    // is_space_station = no
    // icon_frame = 2
    // base_buildtime = 60
    // can_have_federation_design = yes
    // enable_default_design = yes	#if yes, countries will have an auto-generated design at start

    // default_behavior = swarm

    // prerequisites = { "tech_corvettes" }

    // combat_disengage_chance = 1.75

    // has_mineral_upkeep = yes
    // class = shipclass_military
    // construction_type = starbase_shipyard
    // required_component_set = "power_core"
    // required_component_set = "ftl_components"
    // required_component_set = "thruster_components"
    // required_component_set = "sensor_components"
    // required_component_set = "combat_computers"
    let shipsize =
        let inner =
            [
                NewRule(LeafRule(specificField "formation_priority", defaultInt), optionalSingle);
                NewRule(LeafRule(specificField "max_speed", defaultFloat), requiredSingle);
                NewRule(LeafRule(specificField "acceleration", defaultFloat), requiredSingle);
                NewRule(LeafRule(specificField "rotation_speed", defaultFloat), requiredSingle);
                NewRule(LeafRule(specificField "collision_radius", defaultFloat), optionalSingle);
                NewRule(LeafRule(specificField "max_hitpoints", defaultInt), requiredSingle);
                NewRule(NodeRule(specificField "modifier", []), optionalSingle);
                NewRule(LeafRule(specificField "size_multiplier", defaultInt), requiredSingle);
                NewRule(LeafRule(specificField "fleet_slot_size", defaultInt), requiredSingle);
                NewRule(NodeRule(specificField "section_slots", []), optionalSingle);
                NewRule(LeafRule(specificField "num_target_locators", defaultInt), requiredSingle);
                NewRule(LeafRule(specificField "is_space_station", ValueField ValueType.Bool), requiredSingle);
                NewRule(LeafRule(specificField "icon_frame", defaultInt), requiredSingle);
                NewRule(LeafRule(specificField "base_buildtime", defaultInt), requiredSingle);
                NewRule(LeafRule(specificField "can_have_federation_design", ValueField ValueType.Bool), requiredSingle);
                NewRule(LeafRule(specificField "enable_default_design", ValueField ValueType.Bool), requiredSingle);
                NewRule(LeafRule(specificField "default_behavior", TypeField "ship_behavior"), requiredSingle);
                NewRule(NodeRule(specificField "prerequisites", []), optionalSingle);
                NewRule(LeafRule(specificField "combat_disengage_chance", defaultFloat), optionalSingle);
                NewRule(LeafRule(specificField "has_mineral_upkeep", ValueField ValueType.Bool), requiredSingle);
                NewRule(LeafRule(specificField "class", ValueField ValueType.Scalar), requiredSingle);
                NewRule(LeafRule(specificField "construction_type", ValueField ValueType.Scalar), requiredSingle);
                NewRule(LeafRule(specificField "required_component_set", ValueField ValueType.Scalar), requiredSingle);
            ]
        NewRule(NodeRule(specificField "ship_size", inner), optionalMany)

    let shipBehaviorType =
        {
            name = "ship_behavior";
            nameField = Some "name";
            filenameName = false;
            path = "common/ship_behaviors";
            conditions = None;
            subtypes = [];
            typeKeyFilter = None
            skipRootKey = None
            warningOnly = false
            path_strict = false
            path_file = None
        }
    let shipSizeType =
        {
            name = "ship_size";
            path = "common/ship_sizes";
            nameField = None;
            filenameName = false;
            conditions = None;
            subtypes = [];
            typeKeyFilter = None
            skipRootKey = None
            warningOnly = false
            path_strict = false
            path_file = None
        }
//  type[ship_behavior] = {
//      path = "game/common/ship_behaviors"
//      name_field = "name"
//  }
//  type[leader_trait] = {
//      path = "game/common/traits"
//      conditions = {
//          leader_trait = yes
//      }
//  }
//  type[species_trait] = {
//      path = "game/common/traits"
//  }