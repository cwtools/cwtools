namespace CWTools.Validation
open CWTools.Process

module ValidationCore =
    

    type ValidationResult<'T when 'T :> Node> = 
        | OK
        | Invalid of ('T * string) list
    type Validator<'T when 'T :> Node> = 'T -> ValidationResult<'T>

    let (<&>) f1 f2 x = 
        match f1 x, f2 x with
        | OK, OK -> OK
        | Invalid e1, Invalid e2 -> Invalid (e1 @ e2)
        | Invalid e, OK | OK, Invalid e -> Invalid e
    let (<&&>) f1 f2 = 
        match f1, f2 with
        | OK, OK -> OK
        | Invalid e1, Invalid e2 -> Invalid (e1 @ e2)
        | Invalid e, OK | OK, Invalid e -> Invalid e