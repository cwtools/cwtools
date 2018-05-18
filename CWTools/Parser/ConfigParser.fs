namespace CWTools.Parser

    
open FParsec
open Microsoft.FSharp.Compiler.Range
open Types
open CWTools.Common.STLConstants


module rec ConfigParser =
    type ValueType =
    | Float of float * float
    | Bool
    | Scalar
    | Int
    | Enum of string list
    type ObjectType =
    | Tech
    | ShipSize
    | StarbaseModule
    type Options = {
        min : int
        max : int
    }
    type Rule = string * Options * Field
    type Field = 
    | ValueField of ValueType
    | ObjectField of EntityType
    | TargetField
    | ClauseField of Rule list
    | EffectField
    | TriggerField
    type EffectRule = Rule // Add scopes

// create_starbase = {
// 	owner = <target>
// 	size = <ship_size>
// 	module = <starbase_module>
// 	building = <starbase_building>
// 	effect = { ... }
// }
    let requiredSingle = { min = 1; max = 1 }
    let optionalSingle = { min = 0; max = 1 }
    let optionalMany = { min = 0; max = 100 }
    let createStarbase = 
        let owner = Rule ("owner", requiredSingle, TargetField )
        let size = Rule ("size", requiredSingle, ObjectField EntityType.ShipSizes)
        let moduleR = Rule ("module", optionalMany, ObjectField EntityType.StarbaseModules)
        let building = Rule ("building", optionalMany, ObjectField EntityType.StarbaseBuilding)
        let effect =  Rule ("effect", optionalSingle, EffectField)
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
                Rule("allow",  requiredSingle, TriggerField);
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
    let defaultFloat = ValueField (ValueType.Float (0.0, 1000.0))
    let shipsize =
        let inner =
            [
                Rule("formation_priority", optionalSingle, ValueField ValueType.Int);
                Rule("max_speed", requiredSingle, defaultFloat);
                Rule("acceleration", requiredSingle, defaultFloat);
                Rule("rotation_speed", requiredSingle, defaultFloat);
                Rule("collision_radius", optionalSingle, defaultFloat);
                Rule("max_hitpoints", requiredSingle, ValueField ValueType.Int);
                Rule("modifier", optionalSingle, ClauseField []);
                Rule("size_multiplier", requiredSingle, ValueField ValueType.Int);
                Rule("fleet_slot_size", requiredSingle, ValueField ValueType.Int);
                Rule("section_slots", optionalSingle, ClauseField []);
                Rule("num_target_locators", requiredSingle, ValueField ValueType.Int);
                Rule("is_space_station", requiredSingle, ValueField ValueType.Bool);
                Rule("icon_frame", requiredSingle, ValueField ValueType.Int);
                Rule("base_buildtime", requiredSingle, ValueField ValueType.Int);
                Rule("can_have_federation_design", requiredSingle, ValueField ValueType.Bool);
                Rule("enable_default_design", requiredSingle, ValueField ValueType.Bool);
                Rule("default_behavior", requiredSingle, ValueField (ValueType.Enum ["swarm"]));
                Rule("prerequisites", optionalSingle, ClauseField []);
                Rule("combat_disengage_chance", optionalSingle, defaultFloat);
                Rule("has_mineral_upkeep", requiredSingle, ValueField ValueType.Bool);
                Rule("class", requiredSingle, ValueField ValueType.Scalar);
                Rule("construction_type", requiredSingle, ValueField ValueType.Scalar);
                Rule("required_component_set", requiredSingle, ValueField ValueType.Scalar);
            ]
        Rule("shipsize", optionalMany, ClauseField inner)
