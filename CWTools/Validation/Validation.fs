namespace CWTools.Validation
open CWTools.Process
open FSharp.Collections.ParallelSeq

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
        static member IncorrectScopeAsLeaf = fun (scope : string) (leaf : string) -> { ID = "CW116"; Severity = Severity.Error; Message = sprintf "%s scope command used incorrectly, did you mean _%s = { %s }" scope scope leaf }
        static member UndefinedScriptVariable = fun (variable : string) -> { ID = "CW117"; Severity = Severity.Error; Message = sprintf "%s variable is never defined" variable}
        static member UndefinedModifier = fun (modifier : string) -> { ID = "CW118"; Severity = Severity.Error; Message = sprintf "unknown modifier %s used. Experimental, please report errors" modifier }
        static member IncorrectModifierScope =
            fun (modifier : string) (actual : string) (expected : string) ->
            { ID = "CW119"; Severity = Severity.Error; Message = sprintf "%s modifier used in incorrect scope. In %s but expected %s. Experimental, please report errors" modifier actual expected}
        static member UnsavedEventTarget = fun (event : string) (targets : string) -> { ID = "CW220"; Severity = Severity.Error; Message = sprintf "%s or an event it calls require the event target(s) %s but they are not set by this event or by all possible events leading here" event targets}
        static member MaybeUnsavedEventTarget = fun (event : string) (targets : string) -> { ID = "CW221"; Severity = Severity.Warning; Message = sprintf "%s or an event it calls require the event target(s) %s but they may not always be set by this event or by all possible events leading here" event targets}
        static member UndefinedEvent = fun (event : string) -> { ID = "CW222"; Severity = Severity.Warning; Message = sprintf "the event id %s is not defined" event }
        static member CustomError = fun error -> { ID = "CW999"; Severity = Severity.Error; Message = error}
    type ValidationResult = 
        | OK
        | Invalid of (string * Severity * CWTools.Parser.Position * int * string * option<string>) list

    let inline invData (code : ErrorCode) (l : ^a) (data : option<string>) =
        let pos = (^a : (member Position : CWTools.Parser.Position) l)
        let key = (^a : (member Key : string) l)
        code.ID, code.Severity, pos, key.Length, code.Message, data

    let inline inv (code : ErrorCode) (l : ^a) =
        invData code l None
        
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

    let (<&!&>) es f = es |> PSeq.map f |> PSeq.fold (<&&>) OK 
    // |> PSeq.map (fun s c -> s <&&> (f c)) |> PSeq.fold (<&&>) OK

    
