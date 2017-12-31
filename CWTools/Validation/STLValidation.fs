namespace CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process.STLProcess

module STLValidation =
    let shipName (ship : Ship) = if ship.Name = "" then Invalid [(ship, "must have name")] else OK
    let shipSize (ship : Ship) = if ship.ShipSize = "" then Invalid [(ship, "must have size")] else OK
    //type ValidationResult = bool * string

    let validateShip : Validator<Ship>  = shipName <&> shipSize
    //let validate (ship : Ship) = validateShip ship