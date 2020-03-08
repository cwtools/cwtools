namespace CWTools.Validation
open CWTools.Process
open FSharp.Collections.ParallelSeq
open CWTools.Common
open CWTools.Utilities.Position
open DotNet.Globbing
open CWTools.Games.Stellaris.STLLookup
open CWTools.Games
open CWTools.Process.STLProcess
open CWTools.Process.ProcessCore
open CWTools.Common.STLConstants
open System


type ErrorCode =
    {
        ID : string
        Severity : Severity
        Message : string
    }

type ErrorCodes =
    static member MixedBlock = { ID = "CW002"; Severity = Severity.Error; Message = "This block has mixed key/values and values, it is probably a missing equals sign inside it." }
    static member MissingLocalisation =
        fun key language ->
            let lang = if language = Lang.STL STLLang.Default then "Default (localisation_synced)" else language.ToString()
            { ID = "CW100"; Severity = Severity.Warning; Message = sprintf "Localisation key %s is not defined for %s" key lang}
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
    static member ResearchLeaderTech = fun actual expected -> { ID = "CW109"; Severity = Severity.Information; Message = sprintf "This research_leader uses area %s but the technology uses area %s" actual expected }
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
    static member PossiblePretrigger = fun (trigger : string) -> { ID = "CW120"; Severity = Severity.Information; Message = sprintf "Trigger %s can be made a pretrigger (see code action to fix)" trigger }
    static member EmptyIf = { ID = "CW121"; Severity = Severity.Warning; Message = "This 'if' trigger contains no effects"}
    static member LocalisationKeyInInline =
        fun key ->
            { ID = "CW122"; Severity = Severity.Information; Message = sprintf "Localisation key %s should not be quoted when used inline, this can cause unexpected behaviour" key}
    static member UnsavedEventTarget = fun (event : string) (targets : string) -> { ID = "CW220"; Severity = Severity.Error; Message = sprintf "%s or an event it calls require the event target(s) %s but they are not set by this event or by all possible events leading here" event targets}
    static member MaybeUnsavedEventTarget = fun (event : string) (targets : string) -> { ID = "CW221"; Severity = Severity.Warning; Message = sprintf "%s or an event it calls require the event target(s) %s but they may not always be set by this event or by all possible events leading here" event targets}
    static member UndefinedEvent = fun (event : string) -> { ID = "CW222"; Severity = Severity.Warning; Message = sprintf "The event id %s is not defined" event }
    static member IncorrectNotUsage = { ID = "CW223"; Severity = Severity.Information; Message = "Do not use NOT with multiple children, replace this with either NOR or NAND to avoid ambiguity"}
    static member RedundantBoolean = { ID = "CW224"; Severity = Severity.Information; Message = "This boolean operator is redundant" }
    static member UndefinedLocReference = fun (thisLoc : string) (otherLoc : string) language -> { ID = "CW225"; Severity = Severity.Error; Message = sprintf "Localisation key \"%s\" references \"%s\" which doesn't exist in %O" thisLoc otherLoc language}
    static member InvalidLocCommand = fun (thisLoc : string) (command : string) -> { ID = "CW226"; Severity = Severity.Error; Message = sprintf "Localisation key \"%s\" uses command \"%s\" which doesn't exist" thisLoc command }
    static member UnknownSectionTemplate = fun (name : string) -> { ID = "CW227"; Severity = Severity.Error; Message = sprintf "Section template %s can not be found" name}
    static member MissingSectionSlot = fun (section : string) (slot : string) -> { ID = "CW228"; Severity = Severity.Error; Message = sprintf "Section template %s does not have a slot %s" section slot}
    static member UnknownComponentTemplate = fun (name : string) -> { ID = "CW229"; Severity = Severity.Error; Message = sprintf "Component template %s can not be found" name}
    static member MismatchedComponentAndSlot = fun (slot : string) (slotsize : string) (template : string) (templatesize : string) -> { ID = "CW230"; Severity = Severity.Warning; Message = sprintf "Component and slot do not match, slot %s has size %s and component %s has size %s" slot slotsize template templatesize}
    static member UnusedTech = fun (tech : string) -> { ID = "CW231"; Severity = Severity.Warning; Message = sprintf "Technology %s is not used" tech}
    static member UndefinedPDXMesh = fun (mesh : string) -> { ID = "CW232"; Severity = Severity.Error; Message = sprintf "Mesh %s is not defined" mesh }
    static member UndefinedSectionEntity = fun (entity : string) (culture : string) -> { ID = "CW233"; Severity = Severity.Error; Message = sprintf "Entity %s is not defined for culture %s" entity culture}
    static member UndefinedSectionEntityFallback = fun (entity : string) (fallback : string) (culture : string)-> { ID = "CW233"; Severity = Severity.Error; Message = sprintf "Entity %s is not defined for culture %s (nor is fallback %s)" entity culture fallback}
    static member UndefinedEntity = fun (entity : string) -> { ID = "CW233"; Severity = Severity.Error; Message = sprintf "Entity %s is not defined" entity }
    static member ReplaceMeLoc = fun (key : string) (language : Lang) ->
        let lang = if language = Lang.STL STLLang.Default then "Default (localisation_synced)" else language.ToString()
        { ID = "CW234"; Severity = Severity.Information; Message = sprintf "Localisation key %s is a placeholder for %s" key lang }
    static member ZeroModifier = fun (modif : string) -> { ID = "CW235"; Severity = Severity.Warning; Message = sprintf "Modifier %s has value 0. Modifiers are additive so likely doesn't do anything" modif }
    static member DeprecatedElse = { ID = "CW236"; Severity = Severity.Warning; Message = "Nested if/else in effects was deprecated with 2.1 and will be removed in a future release" }
    static member AmbiguousIfElse = { ID = "CW237"; Severity = Severity.Information; Message = "2.1 changed nested if = { if else } behaviour in effects. Check this still works as expected" }
    static member IfElseOrder = { ID = "CW238"; Severity = Severity.Error; Message = "An else/else_if is missing a preceding if" }
    static member ConfigRulesUnexpectedValue = fun message severity -> { ID = "CW240"; Severity = severity; Message = message }
    static member ConfigRulesUnexpectedProperty = fun message severity -> { ID = "CW241"; Severity = severity; Message = message }
    static member ConfigRulesWrongNumber = fun message severity -> { ID = "CW242"; Severity = severity; Message = message }
    static member ConfigRulesTargetWrongScope = fun scope expected target -> { ID = "CW243"; Severity = Severity.Error; Message = sprintf "Target \"%s\" has incorrect scope. Is %s but expect %s" target scope expected}
    static member ConfigRulesInvalidTarget = fun expected target -> { ID = "CW244"; Severity = Severity.Error; Message = sprintf "%s is not a target. Expected a target in scope(s) %s" target expected}
    static member ConfigRulesErrorInTarget = fun command scope expected -> { ID = "CW245"; Severity = Severity.Error; Message = sprintf "Error in target. Command %s was used in scope %s but expected %s" command scope expected}
    static member ConfigRulesUnsetVariable = fun var -> { ID = "CW246"; Severity = Severity.Warning; Message = sprintf "The variable %s has not been set" var}
    static member ConfigRulesRuleWrongScope = fun scope (expected : string) actual -> { ID = "CW247"; Severity = Severity.Error; Message = sprintf "Trigger/Effect/Modifier %s used in wrong scope. In %s but expect %s" actual scope expected}
    static member ConfigRulesInvalidScopeCommand = fun command -> { ID = "CW248"; Severity = Severity.Error; Message = sprintf "Invalid scope command %s" command}
    static member ConfigRulesExpectedVariableValue = { ID = "CW249"; Severity = Severity.Warning; Message = "Expecting a variable or number" }
    static member PlanetKillerMissing = fun message -> { ID = "CW250"; Severity = Severity.Error; Message = message }
    static member UnnecessaryBoolean = fun logicOperator -> { ID = "CW251"; Severity = Severity.Warning; Message = sprintf "This %s is unnecessary" logicOperator }
    static member UndefinedFlag = fun (variable : string) (flagType : FlagType) -> { ID = "CW252"; Severity = Severity.Warning; Message = sprintf "%s flag of type %A is never set" variable flagType}
    static member DeprecatedSetName = { ID = "CW253"; Severity = Severity.Information; Message = "Consider using \"set_name\" instead for consistency" }
    static member WrongEncoding = { ID = "CW254"; Severity = Severity.Error; Message = "Localisation files must be UTF-8 BOM, this file is not"}
    static member MissingLocFileLang = { ID = "CW255"; Severity = Severity.Error; Message = "Localisation file name should contain (and ideally end with) \"l_language.yml\""}
    static member MissingLocFileLangHeader = { ID = "CW256"; Severity = Severity.Error; Message = "Localisation file should start with \"l_language:\" on the first line (or a comment)"}
    static member LocFileLangMismatch = fun (name : STLLang) (header : STLLang) -> { ID = "CW257"; Severity = Severity.Error; Message = sprintf "Localisation file's name has language %A doesn't match the header language %A" name header }
    static member LocFileLangWrongPlace = { ID = "CW258"; Severity = Severity.Information; Message = "Localisation file name should end with \"l_language.yml\""}
    static member RecursiveLocRef = { ID = "CW259"; Severity = Severity.Error; Message = "This localisation string refers to itself"}
    static member LocCommandWrongScope = fun command expected actual -> { ID = "CW260"; Severity = Severity.Error; Message = sprintf "Loc command %s used in wrong scope. In %s but expected %s" command actual expected }
    static member DuplicateTypeDef = fun typename actualtype -> { ID = "CW261"; Severity = Severity.Error; Message = sprintf "Key %s of type %s is defined multiple times" actualtype typename}
    static member ConfigRulesUnexpectedPropertyNode = fun message severity -> { ID = "CW262"; Severity = severity; Message = message }
    static member ConfigRulesUnexpectedPropertyLeaf = fun message severity -> { ID = "CW263"; Severity = severity; Message = message }
    static member ConfigRulesUnexpectedPropertyLeafValue = fun message severity -> { ID = "CW264"; Severity = severity; Message = message }
    static member ConfigRulesUnexpectedPropertyValueClause = fun message severity -> { ID = "CW265"; Severity = severity; Message = message }
    static member LocCommandNotInDataType = fun key command datatype confident ->
        let sev = if confident then Severity.Error else Severity.Warning
        let message = sprintf "Localisation key %s uses command %s which does not exist in data type %s." key command datatype
        let message = if confident then message else sprintf "%s (guessed data type)" message
        { ID = "CW266"; Severity = sev; Message = message }
    static member ConfigRulesUnexpectedAliasKeyValue = fun key expected severity -> { ID = "CW267"; Severity = severity; Message = sprintf "Expected a %s value, got %s" key expected }
    static member LocMissingQuote = fun key ->  { ID = "CW268"; Severity = Severity.Warning; Message = sprintf "Localisation key %s doesn't start and end with double quotes" key}
    static member RulesError = fun error severity -> { ID = "CW998"; Severity = severity; Message = error}
    static member CustomError = fun error severity -> { ID = "CW999"; Severity = severity; Message = error}

// open System.Runtime.CompilerServices
// [<Extension>]
// type Utils () =
//     [<Extension>]
//     static member Equals(x : List<CWError>, y : List<CWError>) =
//         let rec inner (x : List<CWError>) (y : List<CWError>) =
//             let mutable x = x
//             let xe = x.IsEmpty
//             let ye = y.IsEmpty
//             if xe && ye then true else
//             if xe <> ye then false else
//             let xs = x.Tail
//             let ys = y.Tail
//             if xs.IsEmpty <> ys.IsEmpty then false
//             else
//                 if x.Head = y.Head then inner xs ys
//                 else false
//         inner x y

[<CustomEqualityAttribute; NoComparisonAttribute>]
type ValidationResult =
    | OK
    | Invalid of Guid *  CWError list
    member x.Equals(y : ValidationResult) =
        match x, y with
        |OK, OK -> true
        |Invalid (g1, e1), Invalid (g2, e2) -> g1 = g2
        | _ -> false
    override x.Equals(y : obj) =
        match y with
        | :? ValidationResult as vr -> x.Equals vr
        | _ -> false

type EntitySet<'T when 'T :> ComputedData>(entities : struct (Entity * Lazy<'T>) list) =
    member __.GlobMatch(pattern : string) =
        let options = new GlobOptions();
        options.Evaluation.CaseInsensitive <- true;
        let glob = Glob.Parse(pattern, options)
        entities |> List.choose (fun struct (es, _) -> if glob.IsMatch(es.filepath) then Some es.entity else None)
    member this.GlobMatchChildren(pattern : string) =
        this.GlobMatch(pattern) |> List.map (fun e -> e.Children) |> List.collect id
    member __.AllOfType (entityType : EntityType) =
        entities |> List.choose(fun struct (es, d) -> if es.entityType = entityType then Some (es.entity, d)  else None)
    member this.AllOfTypeChildren (entityType : EntityType) =
        this.AllOfType(entityType) |> List.map (fun (e, d) -> e.Children) |> List.collect id
    member __.All = entities |> List.map (fun struct (es, _) -> es.entity)
    member __.AllWithData = entities |> List.map (fun struct (es, d) -> es.entity, d)
    member this.AllEffects=
        let fNode = (fun (x : Node) acc ->
                        match x with
                        | :? EffectBlock as e -> e::acc
                        | :? Option as e -> e.AsEffectBlock::acc
                        |_ -> acc
                            )
        entities |> List.collect (fun struct (_, d) -> d.Force().EffectBlocks |> Option.defaultValue [])
    member this.AllTriggers=
        let fNode = (fun (x : Node) acc ->
                        match x with
                        | :? TriggerBlock as e -> e::acc
                        |_ -> acc
                            )
        entities |> List.collect (fun struct (_, d) -> d.Force().TriggerBlocks |> Option.defaultValue [])
    member this.AllModifiers=
        let fNode = (fun (x : Node) acc ->
                        match x with
                        | :? WeightModifierBlock as e -> e::acc
                        |_ -> acc
                            )
        this.All |> List.collect (foldNode7 fNode)

    member __.AddOrGetCached id generator =
        entities |> List.collect (fun struct (e, d) ->
                                let data = d.Force()
                                match data.Cache |> Map.tryFind id with
                                |Some v -> v
                                |None -> let v = generator e in data.Cache <- Map.add id v data.Cache; v)


    member __.Raw = entities

type STLEntitySet = EntitySet<STLComputedData>
type EU4EntitySet = EntitySet<EU4ComputedData>
type VIC2EntitySet = EntitySet<VIC2ComputedData>
type StructureValidator<'T when 'T :> ComputedData> = EntitySet<'T> -> EntitySet<'T> -> ValidationResult
type STLStructureValidator = StructureValidator<STLComputedData>
type EU4StructureValidator = StructureValidator<EU4ComputedData>
type VIC2StructureValidator = StructureValidator<VIC2ComputedData>
type FileValidator<'T when 'T :> ComputedData> = IResourceAPI<'T> -> EntitySet<'T> -> ValidationResult
type STLFileValidator = FileValidator<STLComputedData>
type LookupValidator<'T when 'T :> ComputedData> = Lookup -> StructureValidator<'T>
type LocalisationValidator<'T when 'T :> ComputedData> = EntitySet<'T> -> (Lang * Set<string>) list -> EntitySet<'T> -> ValidationResult

module ValidationCore =


    let inline invData (code : ErrorCode) (l : IKeyPos) (data : option<string>) =
        let pos = l.Position
        let key = l.Key
        { code = code.ID; severity = code.Severity; range = pos; keyLength = key.Length; message = code.Message; data = data; relatedErrors = None }
        // code.ID, code.Severity, pos, key.Length, code.Message, data, None

    let inline inv (code : ErrorCode) (l) =
        invData code l None

    let invLeafValue (code : ErrorCode) (lv : LeafValue) (data : option<string>) =
        let pos = lv.Position
        let value = lv.Value.ToString()
        { code = code.ID; severity = code.Severity; range = pos; keyLength = value.Length; message = code.Message; data = data; relatedErrors = None }
        // code.ID, code.Severity, pos, value.Length, code.Message, data, None

    let invManual (code : ErrorCode) (pos : range) (key : string) (data : string option) =
        { code = code.ID; severity = code.Severity; range = pos; keyLength = key.Length; message = code.Message; data = data; relatedErrors = None }
        // code.ID, code.Severity, pos, key.Length, code.Message, data, None

    let inline invCustom (l) =
        invData (ErrorCodes.CustomError "default error" Severity.Error) l None
    // let inline inv (sev : Severity) (l : ^a) (s : string) =
    //     let pos = (^a : (member Position : CWTools.Parser.Position) l)
    //     let key = (^a : (member Key : string) l)
    //     sev, pos, key.Length, s

    type Validator<'T when 'T :> Node> = 'T -> ValidationResult

    let (<&>) f1 f2 x =
        match f1 x, f2 x with
        | OK, OK -> OK
        | Invalid (_, e1), Invalid (_, e2) -> Invalid (Guid.NewGuid(),(e1 @ e2))
        | Invalid (g, e), OK | OK, Invalid (g, e) -> Invalid (g, e)
    let (<&&>) f1 f2 =
        match f1, f2 with
        | OK, OK -> OK
        | Invalid (_, e1), Invalid (_, e2) -> Invalid (Guid.NewGuid(),(e1 @ e2))
        | Invalid (g, e), OK | OK, Invalid (g, e) -> Invalid (g, e)

    let (<&&&>) e f =
        match f with
        |OK -> Invalid (Guid.NewGuid(), [e])
        |Invalid (g, es) -> Invalid (Guid.NewGuid(), e::es)
    let (<&?&>) f1 f2 =
        match f1, f2 with
        |OK, OK -> OK
        | Invalid (_, e1), Invalid (_, e2) -> Invalid (Guid.NewGuid(),(e1 @ e2))
        |Invalid (_, e), OK -> OK
        |OK, Invalid (_, e) -> OK

    let mergeValidationErrors (errorcode : string) =
        let rec mergeErrorsInner es =
            match es with
            // | [] -> es
            | [] -> failwith "mergeErrorsInner somehow got an empty error list"
            | [res] -> res
            | head::head2::tail ->
                // let (e1c, e1s, e1r, e1l, e1m, e1d, e1rel), (_, _, _, _, e2m, _, _) = head, head2
                let t = { code = head.code; severity = head.severity; range = head.range; keyLength = head.keyLength; message = (sprintf "%s\nor\n%s" (head.message) (head2.message)); data = head.data; relatedErrors = head.relatedErrors }
                mergeErrorsInner (t::tail)
        mergeErrorsInner

    // Parallelising something this small makes it slower!
    let (<&!!&>) es f = es |> PSeq.map f |> Seq.fold (<&&>) OK
    let (<&!&>) es f = es |> Seq.map f |> Seq.fold (<&&>) OK
    let applyToAll es f e = es |> Seq.fold f e

    let (<&??&>) es f = es |> Seq.map f |> Seq.reduce (<&?&>)
    let inline checkNonNull argName arg =
        match box arg with
        | null -> nullArg argName
        | _ -> ()
    let takeWhileOne predicate (source: seq<_>) =
        checkNonNull "source" source
        seq { use e = source.GetEnumerator()
              let latest = ref Unchecked.defaultof<_>
              if e.MoveNext()
              then
                  latest := e.Current
                  yield !latest
                  while e.MoveNext() && (let x = predicate !latest in  latest := e.Current; x) do
                      yield !latest
              else () }

    let merger original news =
        match original, news with
        |_, Invalid (_, []) -> None
        |_, Invalid (_, [res]) -> Some res
        |Invalid (_, []), Invalid (_, es) |OK, Invalid (_, es) -> Some (mergeValidationErrors "CW240" es)
        |Invalid (_, (head::_)), Invalid (_, es) ->
            let t = List.takeWhile ((<>) head) es
            match t with
            |[] -> None
            |[res] -> Some res
            |_ -> Some (mergeValidationErrors "CW240" t)
        |_, OK -> None
    let isSameObject = LanguagePrimitives.PhysicalEquality


    let lazyErrorMerge rs f defValue (errors : ValidationResult) merge =
        // let mutable state = errors
        let mutable foundOK = false
        let mutable any = false        //Take every potential rule
        let res =
            rs |> Seq.fold (fun es r ->
            if foundOK
            then es
            else (let x = f r es in if x.Equals es then foundOK <- true; x else any <- true; x)) errors
        match foundOK, any, merge with
        |true, _, _ -> errors
        |_, false, _ -> defValue()
        |_, true, true -> (merger errors res) |> Option.map (fun e -> e <&&&> errors ) |> Option.defaultValue errors
        |_, true, false -> res
        // if foundOK then errors else (if any then state else defValue())
        // let t = es |> Seq.map f  |> takeWhileOne (fun e -> e |> function |OK -> false |Invalid er -> true) |> List.ofSeq
        // t |> Seq.tryHead |> Option.map (fun s -> Seq.fold (<&?&>) s t) |> Option.defaultValue (defValue())
    // let inline lazyErrorMerge es f defValue merge =
    //     let t = es |> Seq.map f  |> takeWhileOne (fun e -> e |> function |OK -> false |Invalid er -> true) |> List.ofSeq
    //     t |> Seq.tryHead |> Option.map (fun s -> Seq.fold (<&?&>) s t |>  (fun f -> if merge then mergeValidationErrors "CW240" f else f )) |> Option.defaultValue (defValue())
    //     // if t |> Seq.cache |> Seq.isEmpty then defValue() else Seq.reduce (<&?&>) t |> (fun f -> if merge then mergeValidationErrors "CW240" f else f )


