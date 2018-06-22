namespace CWTools.Parser


open FParsec
open Microsoft.FSharp.Compiler.Range
open Types
open CWTools.Common.STLConstants
open System.IO
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Utilities.Utils
open System
open System.Globalization
open System.Reflection.Metadata


module rec ConfigParser =
    type ValueType =
    | Float of float * float
    | Bool
    | Scalar
    | Int of int * int
    | Enum of string
    | Specific of string
    type ObjectType =
    | Tech
    | ShipSize
    | StarbaseModule
    type Options = {
        min : int
        max : int
        leafvalue : bool
        description : string option
        pushScope : Scope option
    }
    type Rule = string * Options * Field
    type Field =
    | ValueField of ValueType
    | ObjectField of EntityType
    | TypeField of string
    | LeftTypeField of string * Field
    | ClauseField of Rule list
    | LeftClauseField of ValueType * Rule list
    | ScopeField of Scope
    | LeftScopeField of Rule list
    | LocalisationField of synced : bool
    | FilepathField
    | AliasField of string
    | SubtypeField of string * bool * Field
    type RootRule =
    | AliasRule of string * Rule
    | TypeRule of Rule
    type EffectRule = Rule // Add scopes
    type SubTypeDefinition = {
        name : string
        rules : Rule list
        typeKeyField : string option
        pushScope : Scope option
    }
    type TypeDefinition = {
        name : string
        nameField : string option
        path : string
        conditions : Node option
        subtypes : SubTypeDefinition list
    }
    type EnumDefinition = string * string list
    type ComplexEnumDef = {
        name : string
        path : string
        nameTree : Node
    }

    let defaultOptions = { min = 0; max = 1000; leafvalue = false; description = None; pushScope = None }
    let requiredSingle = { defaultOptions with min = 1; max = 1 }
    let requiredMany = { defaultOptions with min = 1; max = 100 }
    let optionalSingle = { defaultOptions with min = 0; max = 1 }
    let optionalMany = { defaultOptions with min = 0; max = 100 }

    let defaultFloat = ValueField (ValueType.Float (-1E+12, 1E+12))
    let defaultInt = ValueField (ValueType.Int (Int32.MinValue, Int32.MaxValue))

    let parseConfigString filename fileString = runParserOnString CKParser.all () filename fileString

    let getNodeComments (node : Node) =
        let findComments t s (a : Child) =
                match (s, a) with
                | ((b, c), _) when b -> (b, c)
                | ((_, c), CommentC nc) when nc.StartsWith("#") -> (false, nc::c)
                | ((_, c), CommentC nc) -> (false, c)
                | ((_, c), NodeC n) when n.Position = t -> (true, c)
                | ((_, c), LeafC v) when v.Position = t -> (true, c)
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
            Some (setting.Trim [|'['; ']'|])

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



    let getOptionsFromComments (comments : string list) =
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
        { min = min; max = max; leafvalue = false; description = description; pushScope = pushScope }

    let processChildConfig ((child, comments) : Child * string list)  =
        match child with
        |NodeC n -> Some (configNode n comments (n.Key))
        |LeafC l -> Some (configLeaf l comments (l.Key))
        |LeafValueC lv -> Some (configLeafValue lv comments)
        |_ -> None

    let configNode (node : Node) (comments : string list) (key : string) =
        let children = getNodeComments node
        let options = getOptionsFromComments comments
        let field =
            match key with
            |x when x.StartsWith "subtype[" ->
                match getSettingFromString x "subtype" with
                |Some st when st.StartsWith "!" -> SubtypeField (st.Substring(1), false, ClauseField(children |> List.choose processChildConfig))
                |Some st -> SubtypeField (st, true, ClauseField(children |> List.choose processChildConfig))
                |None -> ClauseField []
            |"int" -> LeftClauseField (ValueType.Int (Int32.MinValue, Int32.MaxValue), children |> List.choose processChildConfig)
            |"float" -> LeftClauseField (ValueType.Float (Double.MinValue, Double.MaxValue), children |> List.choose processChildConfig)
            |"scalar" -> LeftClauseField (ValueType.Scalar, children |> List.choose processChildConfig)
            |"scope" -> LeftScopeField (children |> List.choose processChildConfig)
            |x when x.StartsWith "enum[" ->
                match getSettingFromString x "enum" with
                |Some e -> LeftClauseField (ValueType.Enum e, children |> List.choose processChildConfig)
                |None -> ClauseField []
            |x when x.StartsWith "<" && x.EndsWith ">" ->
                LeftTypeField(x.Trim([|'<'; '>'|]), ClauseField(children |> List.choose processChildConfig))
            |_ -> ClauseField(children |> List.choose processChildConfig)
        Rule(key, options, field)

    let processChildConfigRoot ((child, comments) : Child * string list) =
        match child with
        |NodeC n when n.Key == "types" -> None
        |NodeC n -> Some (configRootNode n comments)
        |LeafC l -> Some (configRootLeaf l comments)
        //|LeafValueC lv -> Some (configLeafValue lv comments)
        |_ -> None

    let configRootLeaf (leaf : Leaf) (comments : string list) =
        let key, options, field = configLeaf leaf comments leaf.Key
        match leaf.Key with
        |x when x.StartsWith "alias[" ->
            match getAliasSettingsFromString x with
            |Some (a, rn) ->
                let innerRule = configLeaf leaf comments rn
                AliasRule (a, innerRule)
            |None ->
                TypeRule (Rule(x, options, field))
        |x ->
            TypeRule (Rule(x, options, field))

    let configRootNode (node : Node) (comments : string list) =
        let children = getNodeComments node
        let options = getOptionsFromComments comments
        match node.Key with
        |x when x.StartsWith "alias[" ->
            match getAliasSettingsFromString x with
            |Some (a, rn) ->
                let innerRule = configNode node comments rn
                AliasRule (a, innerRule)
            |None ->
                TypeRule (Rule(x, options, ClauseField(children |> List.choose processChildConfig)))
        |x ->
            TypeRule (Rule(x, options, ClauseField(children |> List.choose processChildConfig)))

    let configLeaf (leaf : Leaf) (comments : string list) (key : string) =
        let rightfield =
            match leaf.Value.ToString() with
            |"scalar" -> ValueField ValueType.Scalar
            |"bool" -> ValueField ValueType.Bool
            |"localisation" -> LocalisationField false
            |"localisation_synced" -> LocalisationField true
            |"filepath" -> FilepathField
            |x when x.StartsWith "<" && x.EndsWith ">" ->
                TypeField (x.Trim([|'<'; '>'|]))
            |x when x.StartsWith "int" ->
                match getIntSettingFromString x with
                |Some (min, max) -> ValueField (ValueType.Int (min, max))
                |None -> (defaultFloat)
            |x when x.StartsWith "float" ->
                match getFloatSettingFromString x with
                |Some (min, max) -> ValueField (ValueType.Float (min, max))
                |None -> (defaultFloat)
            |x when x.StartsWith "enum" ->
                match getSettingFromString x "enum" with
                |Some (name) -> ValueField (ValueType.Enum name)
                |None -> ValueField (ValueType.Enum "")
            |x when x.StartsWith "alias_match_left" ->
                match getSettingFromString x "alias_match_left" with
                |Some alias -> AliasField alias
                |None -> ValueField ValueType.Scalar
            |x when x.StartsWith "scope" ->
                match getSettingFromString x "scope" with
                |Some target ->
                    ScopeField (parseScope target)
                |None -> ValueField ValueType.Scalar
            |x when x.StartsWith "event_target" ->
                match getSettingFromString x "event_target" with
                |Some target ->
                    ScopeField (parseScope target)
                |None -> ValueField ValueType.Scalar
            |x -> ValueField (ValueType.Specific x)
        let options = getOptionsFromComments comments
        let field =
            match key with
            |x when x.StartsWith "<" && x.EndsWith ">" ->
                LeftTypeField (x.Trim([|'<'; '>'|]), rightfield)
            |_ -> rightfield
        Rule(key, options, field)

    let configLeafValue (leafvalue : LeafValue) (comments : string list) =
        let field =
            match leafvalue.Value.ToRawString() with
            |x when x.StartsWith "<" && x.EndsWith ">" ->
                TypeField (x.Trim([|'<'; '>'|]))
            |x -> ValueField (ValueType.Enum x)
        let options = { getOptionsFromComments comments with leafvalue = true }
        Rule("leafvalue", options, field)

    // Types

    let processType (node : Node) (comments : string list) =
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
                |Some key -> Some { name = key; rules =  (getNodeComments subtype |> List.choose processChildConfig); typeKeyField = typekeyfilter; pushScope = pushScope }
                |None -> None
            |_ -> None
        match node.Key with
        |x when x.StartsWith("type") ->
            let typename = getSettingFromString node.Key "type"
            let namefield = if node.Has "name_field" then Some (node.TagText "name_field") else None
            let path = (node.TagText "path").Replace("game/","").Replace("game\\","")
            let subtypes = getNodeComments node |> List.choose parseSubType
            match typename with
            |Some tn -> Some { name = tn; nameField = namefield; path = path; conditions = None; subtypes = subtypes}
            |None -> None
        |_ -> None



    let processChildType ((child, comments) : Child * string list) =
        match child with
        |NodeC n when n.Key == "types" ->
            let inner ((child2, comments2) : Child * string list) =
                match child2 with
                |NodeC n2 -> (processType n2 comments2)
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

    let processConfig (node : Node) =
        let nodes = getNodeComments node
        let rules = nodes |> List.choose processChildConfigRoot
        let types = nodes |> List.choose processChildType |> List.collect id
        let enums = nodes |> List.choose processChildEnum |> List.collect id
        let complexenums = nodes |> List.choose processComplexChildEnum |> List.collect id
        rules, types, enums, complexenums

    let parseConfig filename fileString =
        let parsed = parseConfigString filename fileString
        match parsed with
        |Failure(e, _, _) -> eprintfn "config file %s failed with %s" filename e; ([], [], [], [])
        |Success(s,_,_) ->
            let root = shipProcess.ProcessNode<Node> EntityType.Other "root" (mkZeroFile filename) (s |> List.rev)
            processConfig root



// create_starbase = {
// 	owner = <target>
// 	size = <ship_size>
// 	module = <starbase_module>
// 	building = <starbase_building>
// 	effect = { ... }
// }
    let createStarbase =
        let owner = Rule ("owner", requiredSingle, ScopeField Scope.Any )
        let size = Rule ("size", requiredSingle, ObjectField EntityType.ShipSizes)
        let moduleR = Rule ("module", optionalMany, ObjectField EntityType.StarbaseModules)
        let building = Rule ("building", optionalMany, ObjectField EntityType.StarbaseBuilding)
        let effect =  Rule ("effect", optionalSingle, ValueField ValueType.Scalar)
        EffectRule ("create_starbase", optionalMany, ClauseField [owner; size; moduleR; building; effect])

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
                Rule("allow",  requiredSingle, ValueField ValueType.Scalar);
                Rule("empire_unique", optionalSingle, ValueField ValueType.Bool)
            ]
        Rule("building", optionalMany, ClauseField inner)

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
                Rule("formation_priority", optionalSingle, defaultInt);
                Rule("max_speed", requiredSingle, defaultFloat);
                Rule("acceleration", requiredSingle, defaultFloat);
                Rule("rotation_speed", requiredSingle, defaultFloat);
                Rule("collision_radius", optionalSingle, defaultFloat);
                Rule("max_hitpoints", requiredSingle, defaultInt);
                Rule("modifier", optionalSingle, ClauseField []);
                Rule("size_multiplier", requiredSingle, defaultInt);
                Rule("fleet_slot_size", requiredSingle, defaultInt);
                Rule("section_slots", optionalSingle, ClauseField []);
                Rule("num_target_locators", requiredSingle, defaultInt);
                Rule("is_space_station", requiredSingle, ValueField ValueType.Bool);
                Rule("icon_frame", requiredSingle, defaultInt);
                Rule("base_buildtime", requiredSingle, defaultInt);
                Rule("can_have_federation_design", requiredSingle, ValueField ValueType.Bool);
                Rule("enable_default_design", requiredSingle, ValueField ValueType.Bool);
                Rule("default_behavior", requiredSingle, TypeField "ship_behavior");
                Rule("prerequisites", optionalSingle, ClauseField []);
                Rule("combat_disengage_chance", optionalSingle, defaultFloat);
                Rule("has_mineral_upkeep", requiredSingle, ValueField ValueType.Bool);
                Rule("class", requiredSingle, ValueField ValueType.Scalar);
                Rule("construction_type", requiredSingle, ValueField ValueType.Scalar);
                Rule("required_component_set", requiredSingle, ValueField ValueType.Scalar);
            ]
        Rule("ship_size", optionalMany, ClauseField inner)

    let shipBehaviorType =
        {
            name = "ship_behavior";
            nameField = Some "name";
            path = "common/ship_behaviors";
            conditions = None;
            subtypes = [];
        }
    let shipSizeType =
        {
            name = "ship_size";
            path = "common/ship_sizes";
            nameField = None;
            conditions = None;
            subtypes = [];
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