namespace CWTools.Validation
open CWTools.Process

module ValidationCore =
    

    type ValidationResult = 
        | OK
        | Invalid of (CWTools.Parser.Position * int * string) list

    let inline inv (l : ^a) (s : string) = 
        let pos = (^a : (member Position : CWTools.Parser.Position) l)
        let key = (^a : (member Key : string) l)
        pos, key.Length, s

    type Validator<'T when 'T :> Node> = 'T -> ValidationResult

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