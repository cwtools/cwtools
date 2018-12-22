namespace CWTools.Validation
open CWTools.Process
open FSharp.Collections.ParallelSeq
open CWTools.Parser
open Newtonsoft.Json.Converters
open CWTools.Common
open CWTools.Utilities.Position
open DotNet.Globbing
open CWTools.Games.Stellaris.STLLookup
open CWTools.Games
open CWTools.Process.STLProcess
open CWTools.Process.ProcessCore
open CWTools.Common.STLConstants


module ValidationCore =


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
        static member UnsavedEventTarget = fun (event : string) (targets : string) -> { ID = "CW220"; Severity = Severity.Error; Message = sprintf "%s or an event it calls require the event target(s) %s but they are not set by this event or by all possible events leading here" event targets}
        static member MaybeUnsavedEventTarget = fun (event : string) (targets : string) -> { ID = "CW221"; Severity = Severity.Warning; Message = sprintf "%s or an event it calls require the event target(s) %s but they may not always be set by this event or by all possible events leading here" event targets}
        static member UndefinedEvent = fun (event : string) -> { ID = "CW222"; Severity = Severity.Warning; Message = sprintf "the event id %s is not defined" event }
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
            { ID = "CW234"; Severity = Severity.Information; Message = sprintf "Localisation key %s is \"REPLACE_ME\" for %s" key lang }
        static member ZeroModifier = fun (modif : string) -> { ID = "CW235"; Severity = Severity.Warning; Message = sprintf "Modifier %s has value 0. Modifiers are additive so likely doesn't do anything" modif }
        static member DeprecatedElse = { ID = "CW236"; Severity = Severity.Warning; Message = "Nested if/else in effects was deprecated with 2.1 and will be removed in a future release" }
        static member AmbiguousIfElse = { ID = "CW237"; Severity = Severity.Information; Message = "2.1 changed nested if = { if else } behaviour in effects. Check this still works as expected" }
        static member IfElseOrder = { ID = "CW238"; Severity = Severity.Error; Message = "An else/else_if is missing a preceding if" }
        static member ConfigRulesUnexpectedValue = fun message severity -> { ID = "CW240"; Severity = severity; Message = message }
        static member ConfigRulesUnexpectedProperty = fun message severity -> { ID = "CW241"; Severity = severity; Message = message }
        static member ConfigRulesWrongNumber = fun message severity -> { ID = "CW242"; Severity = severity; Message = message }
        static member ConfigRulesTargetWrongScope = fun scope expected -> { ID = "CW243"; Severity = Severity.Error; Message = sprintf "Target has incorrect scope. Is %s but expect %s" scope expected}
        static member ConfigRulesInvalidTarget = fun expected -> { ID = "CW244"; Severity = Severity.Error; Message = sprintf "This is not a target. Expected a target in scope(s) %s" expected}
        static member ConfigRulesErrorInTarget = fun command scope expected -> { ID = "CW245"; Severity = Severity.Error; Message = sprintf "Error in target. Command %s was used in scope %s but expected %s" command scope expected}
        static member ConfigRulesUnsetVariable = fun var -> { ID = "CW246"; Severity = Severity.Error; Message = sprintf "The variable %s has not been set" var}
        static member ConfigRulesRuleWrongScope = fun scope (expected : string) -> { ID = "CW247"; Severity = Severity.Error; Message = sprintf "Trigger/Effect used in wrong scope. In %s but expect %s" scope expected}
        static member ConfigRulesInvalidScopeCommand = fun command -> { ID = "CW248"; Severity = Severity.Error; Message = sprintf "Invalid scope command %s" command}
        static member PlanetKillerMissing = fun message -> { ID = "CW250"; Severity = Severity.Error; Message = message }
        static member UnnecessaryBoolean = fun bool -> { ID = "CW251"; Severity = Severity.Warning; Message = sprintf "This %s is unnecessary" bool }
        static member UndefinedFlag = fun (variable : string) (flagType : FlagType) -> { ID = "CW252"; Severity = Severity.Warning; Message = sprintf "%s flag of type %A is never set" variable flagType}
        static member DeprecatedSetName = { ID = "CW253"; Severity = Severity.Information; Message = "Consider using \"set_name\" instead for consistency" }
        static member WrongEncoding = { ID = "CW254"; Severity = Severity.Error; Message = "Localisation files must be UTF-8 BOM, this file is not"}
        static member MissingLocFileLang = { ID = "CW255"; Severity = Severity.Error; Message = "Localisation file name should contain (and ideally end with) \"l_language.yml\""}
        static member MissingLocFileLangHeader = { ID = "CW256"; Severity = Severity.Error; Message = "Localisation file should start with \"l_language:\" on the first line (or a comment)"}
        static member LocFileLangMismatch = fun (name : STLLang) (header : STLLang) -> { ID = "CW257"; Severity = Severity.Error; Message = sprintf "Localisation file's name has language %A doesn't match the header language %A" name header }
        static member LocFileLangWrongPlace = { ID = "CW258"; Severity = Severity.Information; Message = "Localisation file name should end with \"l_language.yml\""}

        static member RulesError = fun error severity -> { ID = "CW998"; Severity = severity; Message = error}
        static member CustomError = fun error severity -> { ID = "CW999"; Severity = severity; Message = error}

    type CWError = (string * Severity * range * int * string * option<string>)
    type ValidationResult =
        | OK
        | Invalid of CWError list

    let inline invData (code : ErrorCode) (l : IKeyPos) (data : option<string>) =
        let pos = l.Position
        let key = l.Key
        code.ID, code.Severity, pos, key.Length, code.Message, data

    let inline inv (code : ErrorCode) (l) =
        invData code l None

    let invLeafValue (code : ErrorCode) (lv : LeafValue) (data : option<string>) =
        let pos = lv.Position
        let value = lv.Value.ToString()
        code.ID, code.Severity, pos, value.Length, code.Message, data

    let invManual (code : ErrorCode) (pos : range) (key : string) (data : string option) =
        code.ID, code.Severity, pos, key.Length, code.Message, data

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
        | Invalid e1, Invalid e2 -> Invalid (e1 @ e2)
        | Invalid e, OK | OK, Invalid e -> Invalid e
    let (<&&>) f1 f2 =
        match f1, f2 with
        | OK, OK -> OK
        | Invalid e1, Invalid e2 -> Invalid (e1 @ e2)
        | Invalid e, OK | OK, Invalid e -> Invalid e

    let (<&?&>) f1 f2 =
        match f1, f2 with
        |OK, OK -> OK
        |Invalid e1, Invalid e2 -> Invalid (e1 @ e2)
        |Invalid e, OK -> OK
        |OK, Invalid e -> OK

    let mergeValidationErrors (errorcode : string) =
        let rec mergeErrorsInner es =
            match es with
            | [] -> []
            | [res] -> [res]
            | head::head2::tail ->
                let (e1c, e1s, e1r, e1l, e1m, e1d), (_, _, _, _, e2m, _) = head, head2
                mergeErrorsInner ((e1c, e1s, e1r, e1l, sprintf "%s\nor\n%s" e1m e2m, e1d)::tail)
        function
        |OK -> OK
        |Invalid es -> Invalid (mergeErrorsInner es)

    // Parallelising something this small makes it slower!
    let (<&!!&>) es f = es |> PSeq.map f |> PSeq.fold (<&&>) OK
    let (<&!&>) es f = es |> Seq.map f |> Seq.fold (<&&>) OK

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

    let lazyErrorMerge es f defValue merge =
        let t = es |> Seq.map f  |> takeWhileOne (fun e -> e |> function |OK -> false |Invalid er -> true) |> List.ofSeq
        if List.isEmpty t then defValue() else List.reduce (<&?&>) t |> (fun f -> if merge then mergeValidationErrors "CW240" f else f )


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

            this.All |> List.collect (foldNode7 fNode)
        member this.AllTriggers=
            let fNode = (fun (x : Node) acc ->
                            match x with
                            | :? TriggerBlock as e -> e::acc
                            |_ -> acc
                                )
            this.All |> List.collect (foldNode7 fNode)
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
    type StructureValidator<'T when 'T :> ComputedData> = EntitySet<'T> -> EntitySet<'T> -> ValidationResult
    type STLStructureValidator = StructureValidator<STLComputedData>
    type EU4StructureValidator = StructureValidator<EU4ComputedData>
    type FileValidator<'T when 'T :> ComputedData> = IResourceAPI<'T> -> EntitySet<'T> -> ValidationResult
    type STLFileValidator = FileValidator<STLComputedData>
    type LookupValidator<'T, 'S when 'T :> ComputedData and 'S : comparison and 'S :> IScope<'S>> = Lookup<'S> -> StructureValidator<'T>
    type LocalisationValidator<'T when 'T :> ComputedData> = EntitySet<'T> -> (Lang * Set<string>) list -> EntitySet<'T> -> ValidationResult
