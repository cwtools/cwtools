namespace CWTools.Rules
open CWTools.Common
open CWTools.Process.Scopes
open Microsoft.FSharp.Collections.Tagged
open CWTools.Utilities
open CWTools.Utilities.Utils
open CWTools.Utilities.StringResource
open System.IO

type RuleContext =
        {
            subtypes : string list
            scopes : ScopeContext
            warningOnly : bool
        }

type CheckFieldParams =
    {
        varMap : Collections.Map<string, StringSet>
        enumsMap : Collections.Map<string, string * StringSet>
        typesMap : Collections.Map<string,StringSet>
        linkMap : Map<string,Effect,InsensitiveStringComparer>
        wildcardLinks : ScopedEffect list
        valueTriggerMap : Map<string,Effect,InsensitiveStringComparer>
        varSet : StringSet
        localisation : (Lang * Collections.Set<string>) list
        defaultLocalisation : Collections.Set<string>
        files : Collections.Set<string>
        changeScope : ChangeScope
        anyScope : Scope
        defaultLang : Lang
    }

[<RequireQualifiedAccess>]
module internal FieldValidators =

    open System
    open CWTools.Utilities.Utils
    open CWTools.Process
    open CWTools.Validation
    open CWTools.Validation.ValidationCore
    open CWTools.Rules



    let checkPathDir (t : TypeDefinition) (pathDir : string) (file : string) =
        match t.path_strict with
        |true -> t.path |> List.exists (fun tp -> pathDir == tp.Replace("\\","/"))
        |false -> t.path |> List.exists (fun tp -> pathDir.StartsWith(tp.Replace("\\","/")))
        &&
        match t.path_file with
        |Some f -> file == f
        |None -> true
        &&
        match t.path_extension with
        | Some ext ->  Path.GetExtension file == ext
        | None -> true

    let getValidValues =
        function
        |ValueType.Bool -> Some ["yes"; "no"]
        |ValueType.Enum es -> Some [es]
        |_ -> None

    let checkFileExists (files : Collections.Set<string>) (leaf : Leaf) =
        let file = leaf.ValueText.Trim('"').Replace("\\","/").Replace(".lua",".shader").Replace(".tga",".dds")
        if files.Contains file then OK else Invalid [inv (ErrorCodes.MissingFile file) leaf]

    let checkIconExists (files :Collections.Set<string>) (folder : string) (leaf : Leaf) =
        let value = folder + "/" + leaf.ValueText + ".dds"
        if files.Contains value then OK else Invalid [inv (ErrorCodes.MissingFile value) leaf]



    // type ScopeContext = IScopeContext<Scope>

    // type RuleContext  = RuleContext<Scope>
    let firstCharEqualsAmp (key : StringToken) = (stringManager.GetMetadataForID key).startsWithAmp
    // let firstCharEqualsAmp (s : string) = s.Length > 0 && (s.[0] = '@')// || s.[0] = '$')
    let quoteArray = [|'\"'|]
    let ampArray = [|'@'|]
    let trimQuote (s : string) = s.Trim(quoteArray)
    let checkValidValue (enumsMap : Collections.Map<_, string * Set<_, _>>) (keys : (Lang * Collections.Set<string>) list) (severity : Severity) (vt : CWTools.Rules.ValueType) (id : StringToken) (key : string) leafornode errors =
        if firstCharEqualsAmp id then errors else
                match (vt) with
                // | ValueType.Scalar ->
                //     errors
                | ValueType.Bool ->
                    if key == "yes" || key == "no" then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting yes or no, got %s" key) severity) leafornode <&&&> errors
                | ValueType.Int (min, max) ->
                    match TryParser.parseIntWithDecimal key with
                    | Some i ->  if i <= max && i >= min then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %i and %i" min max) severity) leafornode <&&&> errors
                    | None ->
                        match enumsMap.TryFind "static_values" with
                        | Some (_, es) -> if es.Contains (trimQuote key) then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting an integer, got %s" key) severity) leafornode <&&&> errors
                        | None -> inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting an integer, got %s" key) severity) leafornode <&&&> errors
                | ValueType.Float (min, max) ->
                    match TryParser.parseDouble key with
                    | Some f -> if f <= max && f >= min then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %f and %f" min max) severity) leafornode <&&&> errors
                    | None ->
                        match enumsMap.TryFind "static_values" with
                        | Some (_, es) -> if es.Contains (trimQuote key) then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a float, got %s" key) severity) leafornode <&&&> errors
                        | None -> inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a float, got %s" key) severity) leafornode <&&&> errors
                | ValueType.Enum e ->
                    match enumsMap.TryFind e with
                    | Some (desc, es) -> if es.Contains (trimQuote key) then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a \"%s\" value, e.g. %A" desc es) severity) leafornode <&&&> errors
                    | None -> inv (ErrorCodes.RulesError (sprintf "Configuration error: there are no defined values for the enum %s" e) severity) leafornode <&&&> errors
                // | ValueType.Specific s ->
                //     // if trimQuote key == s then OK else Invalid [inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting value %s" s) severity) leafornode]
                //     if id = s.lower then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting value %s" (StringResource.stringManager.GetStringForID(s.normal))) severity) leafornode <&&&> errors
                | ValueType.Percent ->
                    if key.EndsWith("%") then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting an percentage, got %s" key) severity) leafornode <&&&> errors
                | ValueType.Date ->
                    let parts = key.Split([|'.'|])
                    let ok = (parts.Length = 3) && parts.[0].Length <= 4 && Int32.TryParse(parts.[0]) |> fst && Int32.TryParse(parts.[1]) |> fst && Int32.Parse(parts.[1]) <= 12 && Int32.TryParse(parts.[2]) |> fst && Int32.Parse(parts.[2]) <= 31
                    if ok then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a date, got %s" key) severity) leafornode <&&&> errors
                | ValueType.CK2DNA ->
                    if key.Length = 11 && key |> Seq.forall (Char.IsLetter)
                    then errors
                    else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a dna value, got %s" key) severity) leafornode <&&&> errors
                | ValueType.CK2DNAProperty ->
                    if key.Length <= 39 && key |> Seq.forall (fun c -> Char.IsLetter c || c = '0')
                    then errors
                    else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a portrait properties value, got %s" key) severity) leafornode <&&&> errors
                | ValueType.IRFamilyName ->
                    let parts = key.Split([|'.'|])
                    if (parts.Length <> 4) then
                        inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a family names value, got %s" key) severity) leafornode <&&&> errors
                    else
                        (CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeN keys parts.[0] leafornode errors) |>
                        (CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeN keys parts.[1] leafornode) |>
                        (CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeN keys parts.[2] leafornode) |>
                        (CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeN keys parts.[3] leafornode)

    let checkValidValueNE (enumsMap : Collections.Map<_, string * Set<_, _>>)  (keys : (Lang * Collections.Set<string>) list) (severity : Severity) (vt : CWTools.Rules.ValueType) (id : StringToken) (key : string) =
        // if key |> firstCharEqualsAmp then true else
        (match (vt) with
            // | ValueType.Scalar ->
            //     true
            | ValueType.Bool ->
                key == "yes" || key == "no"
            | ValueType.Int (min, max) ->
                match TryParser.parseIntWithDecimal key with
                | Some i ->  i <= max && i >= min
                | None ->
                        match enumsMap.TryFind "static_values" with
                        | Some (_, es) -> es.Contains (trimQuote key)
                        | None -> false
            | ValueType.Float (min, max) ->
                match TryParser.parseDouble key with
                | Some f -> f <= max && f >= min
                | None ->
                        match enumsMap.TryFind "static_values" with
                        | Some (_, es) -> es.Contains (trimQuote key)
                        | None -> false
            | ValueType.Enum e ->
                match enumsMap.TryFind e with
                | Some (_, es) -> es.Contains (trimQuote key)
                | None -> false
            // | ValueType.Specific s ->
            //     // if trimQuote key == s then true else false
            //     id = s.lower
            | ValueType.Percent ->
                key.EndsWith("%")
            | ValueType.Date ->
                let parts = key.Split([|'.'|])
                (parts.Length = 3) && parts.[0].Length <= 4 && Int32.TryParse(parts.[0]) |> fst && Int32.TryParse(parts.[1]) |> fst && Int32.Parse(parts.[1]) <= 12 && Int32.TryParse(parts.[2]) |> fst && Int32.Parse(parts.[2]) <= 31
            | ValueType.CK2DNA ->
                key.Length = 11 && key |> Seq.forall (Char.IsLetter)
            | ValueType.CK2DNAProperty ->
                key.Length <= 39 && key |> Seq.forall (fun c -> Char.IsLetter c || c = '0')
            | ValueType.IRFamilyName ->
                let parts = key.Split([|'.'|])
                (parts.Length = 4) &&
                CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeNE keys parts.[0] &&
                CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeNE keys parts.[1] &&
                CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeNE keys parts.[2] &&
                CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeNE keys parts.[3]

        )
        || firstCharEqualsAmp id

    let checkLocalisationField (keys : (Lang * Collections.Set<string>) list) (defaultKeys : Collections.Set<string>) defaultLang (synced : bool) (key : string) (leafornode) (errors)=
        match synced with
        |true ->
            // let defaultKeys = keys |> List.choose (fun (l, ks) -> if l = defaultLang then Some ks else None) |> List.tryHead |> Option.defaultValue Set.empty
            //let key = leaf.Value |> (function |QString s -> s |s -> s.ToString())
            CWTools.Validation.LocalisationValidation.checkLocNameN leafornode defaultKeys (defaultLang) key errors
        |false ->
            CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeN keys key leafornode errors
    let checkLocalisationFieldNE (keys : (Lang * Collections.Set<string>) list) (defaultKeys : Collections.Set<string>) defaultLang (synced : bool) (key : string) =
        match synced with
        |true ->
            // let defaultKeys = keys |> List.choose (fun (l, ks) -> if l = defaultLang then Some ks else None) |> List.tryHead |> Option.defaultValue Set.empty
            //let key = leaf.Value |> (function |QString s -> s |s -> s.ToString())
            CWTools.Validation.LocalisationValidation.checkLocNameNE defaultKeys (defaultLang) key
        |false ->
            CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeNE keys key
    let memoize keyFunction memFunction =
        let dict = new System.Collections.Generic.Dictionary<_,_>()
        fun n ->
            match dict.TryGetValue(keyFunction(n)) with
            | (true, v) -> v
            | _ ->
                let temp = memFunction(n)
                dict.Add(keyFunction(n), temp)
                temp
    // let memoizedComplexTypes (typetype : TypeType) (values : StringSet)
    let checkTypeField (typesMap : Collections.Map<_,StringSet>) severity (typetype : TypeType) (id : StringToken) (key : string) leafornode errors =
        let isComplex, fieldType =
            match typetype with
            |TypeType.Simple t -> false, t
            |Complex (_, t, _) -> true, t
        let typeKeyMap v =
            match typetype with
            |TypeType.Simple t -> v
            |Complex(p, _, s) -> p + v + s
        match typesMap.TryFind fieldType with
        |Some values ->
            let value = trimQuote key
            if firstCharEqualsAmp id then errors else
            // let values = if isComplex then values.ToList() |> List.map typeKeyMap |> (fun ts -> StringSet.Create(InsensitiveStringComparer(), ts)) else values
            // let values = if isComplex then values.ToList() |> List.map typeKeyMap |> (fun ts -> StringSet.Create(InsensitiveStringComparer(), ts)) else values
            let found =
                let newvalue =
                    match typetype with
                    | TypeType.Simple t -> Some value
                    | Complex(p, _, s) ->
                        // match value.IndexOf(p, StringComparison.OrdinalIgnoreCase), value.LastIndexOf(s, StringComparison.OrdinalIgnoreCase) with
                        // | -1, -1 ->
                        //     None
                        // | fi, -1 ->
                        //     Some (value.Substring(p.Length))
                        // | -1, si ->
                        //     Some (value.Substring(0, si))
                        // | fi, si ->
                        //     Some (value.Substring(p.Length, (si - p.Length)))
                    match value.StartsWith(p, StringComparison.OrdinalIgnoreCase), value.EndsWith(s, StringComparison.OrdinalIgnoreCase) with
                        | _, false -> None
                        | false, _ -> None
                        | true, true ->
                            Some (value.Substring(p.Length, (value.Length - p.Length - s.Length)))
                // eprintfn "ct %s %A %A" value newvalue typetype
                newvalue |> Option.map (values.Contains) |> Option.defaultValue false
            if found
            then errors
            else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected value of type %s" fieldType) severity) leafornode <&&&> errors

            //let values = values typeKeyMap values
            // if values.Contains value then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected value of type %s" fieldType) severity) leafornode <&&&> errors
        |None -> inv (ErrorCodes.CustomError (sprintf "Unknown type referenced %s" fieldType) Severity.Error) leafornode <&&&> errors
    let checkTypeFieldNE (typesMap : Collections.Map<_,StringSet>) severity (typetype : TypeType) (id : StringToken) (key : string) =
        let isComplex, fieldType =
            match typetype with
            |TypeType.Simple t -> false, t
            |Complex (_, t, _) -> true, t
        let typeKeyMap v =
            match typetype with
            |TypeType.Simple t -> v
            |Complex(p, _, s) -> p + v + s
        match typesMap.TryFind fieldType with
        |Some values ->
            let value = trimQuote key
            if firstCharEqualsAmp id then true else
            let value =
                match typetype with
                | TypeType.Simple t -> Some value
                | Complex(p, _, s) ->
                    // match value.IndexOf(p, StringComparison.OrdinalIgnoreCase), value.LastIndexOf(s, StringComparison.OrdinalIgnoreCase) with
                    // | -1, -1 ->
                    //     None
                    // | fi, -1 ->
                    //     Some (value.Substring(p.Length))
                    // | -1, si ->
                    //     Some (value.Substring(0, si))
                    // | fi, si ->
                    //     Some (value.Substring(p.Length, (si - p.Length)))
                    match value.StartsWith(p, StringComparison.OrdinalIgnoreCase), value.EndsWith(s, StringComparison.OrdinalIgnoreCase) with
                    | _, false -> None
                    | false, _ -> None
                    | true, true ->
                        Some (value.Substring(p.Length, (value.Length - p.Length - s.Length)))

            value |> Option.map (values.Contains) |> Option.defaultValue false
            // let values = if isComplex then values.ToList() |> List.map typeKeyMap |> (fun ts -> StringSet.Create(InsensitiveStringComparer(), ts)) else values
            // match isComplex with
            // | true ->
            //     values.ToList() |> List.map typeKeyMap |> List.exists ((==) value)
            // | false ->
            //     values.Contains value
            // let values = if isComplex then values.ToList() |> List.map typeKeyMap |> (fun ts -> StringSet.Create(InsensitiveStringComparer(), ts)) else values

            //let values = values typeKeyMap values
        |None -> false

    let checkVariableGetField (varMap : Collections.Map<_,StringSet>) severity (varName : string) (id : StringToken) (key : string) leafornode errors =
        match varMap.TryFind varName with
        |Some values ->
            let value = trimQuote key
            if firstCharEqualsAmp id then errors else
            if values.Contains (value.Split(ampArray, 2).[0]) then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected defined value of %s, got %s" varName value) (min Severity.Warning severity)) leafornode <&&&> errors
        |None -> inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected defined value of %s, got %s" varName key) (min Severity.Warning severity)) leafornode <&&&> errors
    let checkVariableGetFieldNE (varMap : Collections.Map<_,StringSet>) severity (varName : string) (id : StringToken) (key : string) =
        match varMap.TryFind varName with
        |Some values ->
            let value = trimQuote key
            if firstCharEqualsAmp id then true else
            values.Contains (value.Split(ampArray, 2).[0])
        |None -> false

    let checkFilepathField (files : Collections.Set<string>) (key : string) (prefix : string option) (extension : string option) (leafornode) errors =
        let file = (trimQuote key).Replace("\\","/")
        let file2 = file.Replace(".lua",".shader").Replace(".tga",".dds")
        let file = if extension.IsSome then file + extension.Value else file
        match prefix with
        | Some pre ->
            if files.Contains file || files.Contains (pre + file) || files.Contains file2 || files.Contains (pre + file2) then errors else inv (ErrorCodes.MissingFile file) leafornode <&&&> errors
        | None ->
            if files.Contains file || files.Contains file2 then errors else inv (ErrorCodes.MissingFile file) leafornode <&&&> errors
    let checkFilepathFieldNE (files : Collections.Set<string>) (key : string) (prefix : string option) (extension : string option) =
        let file = (trimQuote key).Replace("\\","/")
        let file2 = file.Replace(".lua",".shader").Replace(".tga",".dds")
        let file = if extension.IsSome then file + extension.Value else file
        match prefix with
        | Some pre ->
            files.Contains file || files.Contains (pre + file) || files.Contains file2 || files.Contains (pre + file2)
        | None ->
            files.Contains file || files.Contains file2

    let checkIconField (files :Collections.Set<string>) (folder : string) (key : string) (leafornode) errors =
        let value = folder + "/" + key + ".dds"
        if files.Contains value then errors else inv (ErrorCodes.MissingFile value) leafornode <&&&> errors
    let checkIconFieldNE (files :Collections.Set<string>) (folder : string) (key : string) =
        let value = folder + "/" + key + ".dds"
        files.Contains value

    let checkScopeField (linkMap : Map<_,_,_>) (valueTriggerMap : Map<_,_,_>) (wildcardLinks : ScopedEffect list) varSet changeScope anyScope (ctx : RuleContext) (s)  key leafornode errors =
        let scope = ctx.scopes
        match changeScope false true linkMap valueTriggerMap wildcardLinks varSet key scope with
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = ( ^a : (static member AnyScope : ^a) ()) || current = ( ^a : (static member AnyScope : ^a) ()) then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then errors else inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString()) key) leafornode <&&&> errors
        |NotFound _ -> inv (ErrorCodes.ConfigRulesInvalidTarget (s.ToString()) key) leafornode <&&&> errors
        |WrongScope (command, prevscope, expected) -> inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%O" expected) ) leafornode <&&&> errors
        |VarFound -> errors
        |VarNotFound s -> inv (ErrorCodes.ConfigRulesUnsetVariable s) leafornode <&&&> errors
        |ValueFound -> inv (ErrorCodes.CustomError "This is a value, but should be a scope" Severity.Error) leafornode <&&&> errors
        |_ -> errors
    let checkScopeFieldNE (linkMap : Map<_,_,_>) (valueTriggerMap : Map<_,_,_>) (wildcardLinks : ScopedEffect list) varSet changeScope anyScope (ctx : RuleContext) (s)  key =
        // log "scope %s %A"key ctx
        let scope = ctx.scopes
        match changeScope true true linkMap valueTriggerMap wildcardLinks varSet key scope with
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = ( ^a : (static member AnyScope : ^a) ()) || current = ( ^a : (static member AnyScope : ^a) ()) then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        |NewScope ({Scopes = current::_} ,_) -> current = s || s = anyScope || current = anyScope
        |NotFound _ -> false
        |WrongScope (command, prevscope, expected) -> true
        |VarNotFound s -> false
        |ValueFound -> false
        |_ -> true

    let checkVariableField (linkMap : Map<_,_,_>) (valueTriggerMap : Map<_,_,_>) (wildcardLinks : ScopedEffect list) varSet changeScope anyScope (ctx : RuleContext) isInt min max key leafornode errors =
        let scope = ctx.scopes

        match TryParser.parseDouble key, TryParser.parseInt key, changeScope false true linkMap valueTriggerMap wildcardLinks varSet key scope with
        |_, Some i, _ when isInt && min <= float i && max >= float i -> errors
        |Some f, _, _ when min <= f && max >= f -> errors
        |_, _, VarFound -> errors
        |_, _, VarNotFound s -> inv (ErrorCodes.ConfigRulesUnsetVariable s) leafornode <&&&> errors
        //TODO: Better error messages for scope instead of variable
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        // |WrongScope (command, prevscope, expected) -> Invalid [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) leafornode]
        |_, _, NotFound _ -> inv ErrorCodes.ConfigRulesExpectedVariableValue leafornode <&&&> errors
        |_ -> inv (ErrorCodes.CustomError "Expecting a variable, but got a scope" Severity.Error) leafornode <&&&> errors
    let checkVariableFieldNE (linkMap : Map<_,_,_>) (valueTriggerMap : Map<_,_,_>) (wildcardLinks : ScopedEffect list) varSet changeScope anyScope (ctx : RuleContext) isInt min max key =
        let scope = ctx.scopes
        match TryParser.parseDouble key, TryParser.parseInt key, changeScope false true linkMap valueTriggerMap wildcardLinks varSet key scope with
        |_, Some i, _ -> isInt && min <= float i && max >= float i
        |Some f, _, _ -> min <= f && max >= f
        |_, _, VarFound -> true
        |_, _, VarNotFound s -> false
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        // |NotFound _ -> Invalid [inv (ErrorCodes.ConfigRulesInvalidTarget (s.ToString())) leafornode]
        // |WrongScope (command, prevscope, expected) -> Invalid [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) leafornode]
        |_ -> false

    let checkValueScopeField (enumsMap : Collections.Map<_, string * Set<_, _>>) (linkMap : Map<_,_,_>) (valueTriggerMap : Map<_,_,_>) (wildcardLinks : ScopedEffect list) varSet changeScope anyScope (ctx : RuleContext) isInt min max key leafornode errors =
        let scope = ctx.scopes
        // let res = changeScope false true linkMap valueTriggerMap varSet key scope
        match TryParser.parseDouble key, TryParser.parseInt key, changeScope false true linkMap valueTriggerMap wildcardLinks varSet key scope with
        |_, Some i, _ when isInt && min <= float i && max >= float i -> errors
        |Some f, _, _ when min <= f && max >= f -> errors
        |_, _, VarFound -> errors
        |_, _, VarNotFound s -> inv (ErrorCodes.ConfigRulesUnsetVariable s) leafornode <&&&> errors
        |_, _, ValueFound -> errors
        //TODO: Better error messages for scope instead of variable
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        // |WrongScope (command, prevscope, expected) -> Invalid [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) leafornode]
        |_, _, NotFound _ ->
                match enumsMap.TryFind "static_values" with
                | Some (_, es) ->
                    if es.Contains (trimQuote key) then errors else inv ErrorCodes.ConfigRulesExpectedVariableValue leafornode <&&&> errors
                | None -> inv ErrorCodes.ConfigRulesExpectedVariableValue leafornode <&&&> errors
        |_ -> inv (ErrorCodes.CustomError "Expecting a variable, but got a scope" Severity.Error) leafornode <&&&> errors
    let checkValueScopeFieldNE (enumsMap : Collections.Map<_, string * Set<_, _>>) (linkMap : Map<_,_,_>) (valueTriggerMap : Map<_,_,_>) (wildcardLinks : ScopedEffect list) varSet changeScope anyScope (ctx : RuleContext) isInt min max key =
        let scope = ctx.scopes
        match TryParser.parseDouble key, TryParser.parseInt key, changeScope false true linkMap valueTriggerMap wildcardLinks varSet key scope with
        |_, Some i, _ -> isInt && min <= float i && max >= float i
        |Some f, _, _ -> min <= f && max >= f
        |_, _, VarFound -> true
        |_, _, VarNotFound s -> false
        |_, _, ValueFound -> true
        |_, _, NotFound ->
                match enumsMap.TryFind "static_values" with
                | Some (_, es) -> es.Contains (trimQuote key)
                | None -> false

        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        // |NotFound _ -> Invalid [inv (ErrorCodes.ConfigRulesInvalidTarget (s.ToString())) leafornode]
        // |WrongScope (command, prevscope, expected) -> Invalid [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) leafornode]
        |_ -> false
    let checkField (p : CheckFieldParams) (severity : Severity) (ctx : RuleContext) (field : NewField) id (key : string) (leafornode : IKeyPos) errors =
            if (stringManager.GetMetadataForID id).containsDoubleDollar then errors else
            match field with
            |ValueField vt ->
                checkValidValue p.enumsMap p.localisation severity vt id key leafornode errors
            |TypeField t -> checkTypeField p.typesMap severity t id key leafornode errors
            |ScopeField s -> checkScopeField p.linkMap p.valueTriggerMap p.wildcardLinks p.varSet p.changeScope p.anyScope ctx s key leafornode errors
            |LocalisationField synced -> checkLocalisationField p.localisation p.defaultLocalisation p.defaultLang synced key leafornode errors
            |FilepathField (prefix, extension) -> checkFilepathField p.files key prefix extension leafornode errors
            |IconField folder -> checkIconField p.files folder key leafornode errors
            |VariableSetField v -> errors
            |VariableGetField v -> checkVariableGetField p.varMap severity v id key leafornode errors
            |VariableField (isInt, (min, max)) -> checkVariableField p.linkMap p.valueTriggerMap p.wildcardLinks p.varSet p.changeScope p.anyScope ctx isInt min max key leafornode errors
            |ValueScopeField (isInt, (min, max)) -> checkValueScopeField p.enumsMap p.linkMap p.valueTriggerMap p.wildcardLinks p.varSet p.changeScope p.anyScope ctx isInt min max key leafornode errors
            |ScalarField _ -> errors
            |SpecificField (SpecificValue v) -> if id = v.lower then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting value %s" (StringResource.stringManager.GetStringForID(v.normal))) severity) leafornode <&&&> errors
            |AliasField (_)
            |MarkerField (_)
            |SingleAliasField (_)
            |SubtypeField (_)
            |TypeMarkerField (_)
            |ValueScopeMarkerField (_) -> inv (ErrorCodes.CustomError (sprintf "Unexpected rule type %O" field) Severity.Error) leafornode <&&&> errors
    let checkFieldNE (p : CheckFieldParams) (severity : Severity) (ctx : RuleContext) (field : NewField) id (key : string) =
            if (stringManager.GetMetadataForID id).containsDoubleDollar then true else
            match field with
            |ValueField vt ->
                checkValidValueNE p.enumsMap p.localisation severity vt id key
            |TypeField t -> checkTypeFieldNE p.typesMap severity t id key
            |ScopeField s -> checkScopeFieldNE p.linkMap p.valueTriggerMap p.wildcardLinks p.varSet p.changeScope p.anyScope ctx s key
            |LocalisationField synced -> true
            |FilepathField (prefix, extension) -> checkFilepathFieldNE p.files key prefix extension
            |IconField folder -> checkIconFieldNE p.files folder key
            |VariableSetField v -> true
            |VariableGetField v -> checkVariableGetFieldNE p.varMap severity v id key
            |VariableField (isInt, (min, max))-> checkVariableFieldNE p.linkMap p.valueTriggerMap p.wildcardLinks p.varSet p.changeScope p.anyScope ctx isInt min max key
            |ValueScopeField (isInt, (min, max))-> checkValueScopeFieldNE p.enumsMap p.linkMap p.valueTriggerMap p.wildcardLinks p.varSet p.changeScope p.anyScope ctx isInt min max key
            |TypeMarkerField (dummy, _) -> dummy = id
            |ScalarField (_) -> true
            |SpecificField (SpecificValue v) -> v.lower = id
            |AliasField (_)
            |MarkerField (_)
            |SingleAliasField (_)
            |SubtypeField (_)
            |TypeMarkerField (_)
            |ValueScopeMarkerField (_) -> false

    let checkLeftField (p : CheckFieldParams) (severity : Severity) (ctx : RuleContext) (field : NewField) id (key : string) =
        checkFieldNE p severity ctx field id key

    let checkFieldByKey (p : CheckFieldParams) (severity : Severity) (ctx : RuleContext) (field : NewField) id (key : string) =
        checkLeftField p severity ctx field id key

    let inline validateTypeLocalisation (typedefs : TypeDefinition list) (invertedTypeMap : Collections.Map<string, string list>) (localisation) (typeKey : string) (key : string) (leafornode) =
        let typenames = typeKey.Split('.')
        let typename = typenames.[0]
        let actualSubtypes =
            match invertedTypeMap |> Map.tryFind key with
            | Some keytypes ->
                keytypes |> List.filter (fun kt -> kt.StartsWith (typename+".", StringComparison.OrdinalIgnoreCase))
                         |> List.map (fun kt -> kt.Split('.').[1])
            | None -> []
        match typedefs |> List.tryFind (fun t -> t.name == typename) with
        |None -> OK
        |Some typedef ->
            let inner =
                (fun l ->
                let lockey = l.prefix + key + l.suffix
                if l.optional || l.explicitField.IsSome
                then OK
                else
                    CWTools.Validation.Stellaris.STLLocalisationValidation.checkLocKeysLeafOrNode localisation lockey leafornode)
            let subtype =
                let subtypes = (if typenames.Length > 1 then typenames.[1]::actualSubtypes else actualSubtypes) |> List.distinct
                let inner2 (nextSt : string) =
                    match typedef.subtypes |> List.tryFind (fun st -> st.name == nextSt) with
                    |None -> OK
                    |Some st -> st.localisation <&!&> inner
                subtypes <&!&> inner2
            typedef.localisation <&!&> inner
            <&&> subtype

    let typekeyfilter (td : TypeDefinition) (n : string) =
        match td.typeKeyFilter with
        | Some (values, negate) -> ((values |> List.exists ((==) n))) <> negate
        | None -> true
        &&
        match td.startsWith with
        | Some prefix -> n.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)
        | None -> true
