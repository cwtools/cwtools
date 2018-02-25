namespace CWTools.Validation
open CWTools.Process

module ValidationCore =
    
    type Severity =
        | Error = 1
        | Warning = 2
        | Information = 3
        | Hint = 4

    type ErrorCode =
        {
            ID : string
            Severity : Severity
            Message : string
        }

    type ErrorCodes =
        static member MissingLocalisation = fun key language -> { ID = "CW100"; Severity = Severity.Warning; Message = sprintf "Localisation key %s is not defined for %O" key language}
        static member UndefinedVariable = fun variable -> { ID = "CW101"; Severity = Severity.Error; Message = sprintf "%s is not defined" variable }
        static member UndefinedTrigger = fun trigger -> { ID = "CW102"; Severity = Severity.Error; Message = sprintf "unknown trigger %s used." trigger }
        static member UndefinedEffect = fun effect -> { ID = "CW103"; Severity = Severity.Error; Message = sprintf "unknown effect %s used." effect }
        static member IncorrectTriggerScope = 
            fun (trigger : string) (actual : string) (expected : string) ->
            { ID = "CW104"; Severity = Severity.Error; Message = sprintf "%s trigger used in incorrect scope. In %s but expected %s" trigger actual expected}

        static member IncorrectEffectScope = 
            fun (trigger : string) (actual : string) (expected : string)->
            { ID = "CW105"; Severity = Severity.Error; Message = sprintf "%s effect used in incorrect scope. In %s but expected %s" trigger actual expected}

        static member IncorrectScopeScope =
            fun (scope : string) (actual : string) (expected : string) ->
            { ID = "CW106"; Severity = Severity.Error; Message = sprintf "%s scope command used in incorrect scope. In %s but expected %s" scope actual expected}
        
        static member EventEveryTick = { ID = "CW107"; Severity = Severity.Information; Message = "This event might affect performance as it runs on every tick, consider adding 'is_triggered_only', 'fire_only_once' or 'mean_time_to_happen'" }
        static member ResearchLeaderArea = { ID = "CW108"; Severity = Severity.Error; Message = "This research_leader is missing required \"area\"" }
        static member ResearchLeaderTech = fun actual expected -> { ID = "CW109"; Severity = Severity.Information; Message = sprintf "This research_leader is uses area %s but the technology uses area %s" actual expected }
        static member TechCatMissing = { ID = "CW110"; Severity = Severity.Error; Message = "No category found for this technology"}
        static member ButtonEffectMissing = fun effect -> { ID = "CW111"; Severity = Severity.Error; Message = sprintf "Button effect %s not found" effect}
        static member SpriteMissing = fun sprite -> { ID = "CW112"; Severity = Severity.Error; Message = sprintf "Sprite type %s not found" sprite}
        static member MissingFile = fun file -> { ID = "CW113"; Severity = Severity.Error; Message = sprintf "File %s not found, this is case sensitive" file}
        static member UndefinedStaticModifier = fun modifier -> { ID = "CW114"; Severity = Severity.Error; Message = sprintf "unknown static modifier %s used." modifier }
        static member IncorrectStaticModifierScope =
            fun (modifier : string) (actual : string) (expected : string) ->
            { ID = "CW115"; Severity = Severity.Warning; Message = sprintf "%s static modifier possibly used in incorrect scope. In %s but expected %s. Please feedback verified usage" modifier actual expected}

        static member CustomError = fun error -> { ID = "CW999"; Severity = Severity.Error; Message = error}
    type ValidationResult = 
        | OK
        | Invalid of (string * Severity * CWTools.Parser.Position * int * string) list
    let inline inv (code : ErrorCode) (l : ^a) =
        let pos = (^a : (member Position : CWTools.Parser.Position) l)
        let key = (^a : (member Key : string) l)
        code.ID, code.Severity, pos, key.Length, code.Message
        
    // let inline inv (sev : Severity) (l : ^a) (s : string) = 
    //     let pos = (^a : (member Position : CWTools.Parser.Position) l)
    //     let key = (^a : (member Key : string) l)
    //     sev, pos, key.Length, s

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

    let (<&!&>) es f = es |> Seq.fold (fun s c -> s <&&> (f c)) OK

    
