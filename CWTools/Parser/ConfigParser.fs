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

