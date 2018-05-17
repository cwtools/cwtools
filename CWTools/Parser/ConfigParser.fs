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
    let requiredOptions = { min = 1; max = 1 }
    let optionalSingle = { min = 0; max = 1 }
    let optionalMany = { min = 0; max = 100 }
    let createStarbase = 
        let owner = Rule ("owner", requiredOptions, TargetField )
        let size = Rule ("size", requiredOptions, ObjectField EntityType.ShipSizes)
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
                Rule("allow",  requiredOptions, TriggerField);
                Rule("empire_unique", optionalSingle, ValueField ValueType.Bool)
            ]
        Rule("building", optionalMany, ClauseField inner)

