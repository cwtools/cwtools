namespace CWTools.Validation

open CWTools.Parser.ConfigParser
open CWTools.Process
open CWTools.Utilities.Utils
open CWTools.Validation.ValidationCore
open CWTools.Utilities.Position
open CWTools.Utilities
open CWTools.Games
open Stellaris.STLValidation
open FParsec
open CWTools.Parser.Types
open CWTools.Utilities
open CWTools.Common
open CWTools.Validation.Stellaris.ScopeValidation
open Microsoft.FSharp.Collections.Tagged
open System.IO
open FSharp.Data.Runtime
open QuickGraph
open System
open FSharp.Collections.ParallelSeq
open System
open CWTools.Process.Scopes
open FParsec
open System.Collections
open System.Diagnostics
open CWTools.Process.ProcessCore

// type RuleApplicator<'S, 'T when 'T :> ComputedData and 'S : comparison> = {
//     applyNodeRule : NewRule<'S> list * Node -> ValidationResult
//     testSubtype : SubTypeDefinition<'S> list * Node -> 'S option * string list
//     ruleValidate : unit -> StructureValidator<'T>
//}
// type IRuleApplicator<'S> =
//     abstract ApplyNodeRule : NewRule<'S> list * Node -> ValidationResult
//     abstract TestSubtype : SubTypeDefinition<'S> list * Node -> 'S option * string list
//     abstract RuleValidate : unit -> StructureValidator<'T>
type RuleContext<'T when 'T :> IScope<'T>> =
        {
            subtypes : string list
            scopes : ScopeContext<'T>
            warningOnly : bool
        }
module rec Rules =

    let fst3 (a, _, _) = a
    let snd3 (_, b, _) = b
    let thd3 (_, _, c) = c


    let checkPathDir (t : TypeDefinition<_>) (pathDir : string) (file : string) =
        match t.path_strict with
        |true -> t.path |> List.exists (fun tp -> pathDir == tp.Replace("\\","/"))
        |false -> t.path |> List.exists (fun tp -> pathDir.StartsWith(tp.Replace("\\","/")))
        &&
        match t.path_file with
        |Some f -> file == f
        |None -> true

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
    let firstCharEqualsAmp (s : string) = s.Length > 0 && (s.[0] = '@' || s.[0] = '$')
    let quoteArray = [|'\"'|]
    let ampArray = [|'@'|]
    let trimQuote (s : string) = s.Trim(quoteArray)
    let checkValidValue (enumsMap : Collections.Map<_, string * Set<_, _>>) (severity : Severity) (vt : CWTools.Parser.ConfigParser.ValueType) (id : StringToken) (key : string) leafornode errors =
        if key |> firstCharEqualsAmp then errors else
                match (vt) with
                | ValueType.Scalar ->
                    errors
                | ValueType.Bool ->
                    if key == "yes" || key == "no" then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting yes or no, got %s" key) severity) leafornode <&&&> errors
                | ValueType.Int (min, max) ->
                    match TryParser.parseIntWithDecimal key with
                    | Some i ->  if i <= max && i >= min then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %i and %i" min max) severity) leafornode <&&&> errors
                    | None -> inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting an integer, got %s" key) severity) leafornode <&&&> errors
                | ValueType.Float (min, max) ->
                    match TryParser.parseDouble key with
                    | Some f -> if f <= max && f >= min then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %f and %f" min max) severity) leafornode <&&&> errors
                    | None -> inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a float, got %s" key) severity) leafornode <&&&> errors
                | ValueType.Enum e ->
                    match enumsMap.TryFind e with
                    | Some (desc, es) -> if es.Contains (trimQuote key) then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a \"%s\" value, e.g. %O" desc es) severity) leafornode <&&&> errors
                    | None -> inv (ErrorCodes.RulesError (sprintf "Configuration error: there are no defined values for the enum %s" e) severity) leafornode <&&&> errors
                | ValueType.Specific s ->
                    // if trimQuote key == s then OK else Invalid [inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting value %s" s) severity) leafornode]
                    if id = s then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting value %s" (StringResource.stringManager.GetStringForID(s))) severity) leafornode <&&&> errors
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
                    if key.Length <= 14 && key |> Seq.forall (fun c -> Char.IsLetter c || c = '0')
                    then errors
                    else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a portrait properties value, got %s" key) severity) leafornode <&&&> errors

    let checkValidValueNE (enumsMap : Collections.Map<_, string * Set<_, _>>) (severity : Severity) (vt : CWTools.Parser.ConfigParser.ValueType) (id : StringToken) (key : string) =
        // if key |> firstCharEqualsAmp then true else
        (match (vt) with
            | ValueType.Scalar ->
                true
            | ValueType.Bool ->
                key == "yes" || key == "no"
            | ValueType.Int (min, max) ->
                match TryParser.parseIntWithDecimal key with
                | Some i ->  i <= max && i >= min
                | None -> false
            | ValueType.Float (min, max) ->
                match TryParser.parseDouble key with
                | Some f -> f <= max && f >= min
                | None -> false
            | ValueType.Enum e ->
                match enumsMap.TryFind e with
                | Some (_, es) -> es.Contains (trimQuote key)
                | None -> false
            | ValueType.Specific s ->
                // if trimQuote key == s then true else false
                id = s
            | ValueType.Percent ->
                key.EndsWith("%")
            | ValueType.Date ->
                let parts = key.Split([|'.'|])
                (parts.Length = 3) && parts.[0].Length <= 4 && Int32.TryParse(parts.[0]) |> fst && Int32.TryParse(parts.[1]) |> fst && Int32.Parse(parts.[1]) <= 12 && Int32.TryParse(parts.[2]) |> fst && Int32.Parse(parts.[2]) <= 31
            | ValueType.CK2DNA ->
                key.Length = 11 && key |> Seq.forall (Char.IsLetter)
            | ValueType.CK2DNAProperty ->
                key.Length <= 14 && key |> Seq.forall (fun c -> Char.IsLetter c || c = '0')
        )
        || firstCharEqualsAmp key

    let checkLocalisationField (keys : (Lang * Collections.Set<string>) list) defaultLang (synced : bool) (key : string) (leafornode) (errors)=
        match synced with
        |true ->
            let defaultKeys = keys |> List.choose (fun (l, ks) -> if l = defaultLang then Some ks else None) |> List.tryHead |> Option.defaultValue Set.empty
            //let key = leaf.Value |> (function |QString s -> s |s -> s.ToString())
            CWTools.Validation.Stellaris.STLLocalisationValidation.checkLocNameN leafornode defaultKeys (defaultLang) key errors
        |false ->
            CWTools.Validation.Stellaris.STLLocalisationValidation.checkLocKeysLeafOrNodeN keys key leafornode errors
    let checkLocalisationFieldNE (keys : (Lang * Collections.Set<string>) list) defaultLang (synced : bool) (key : string) =
        match synced with
        |true ->
            let defaultKeys = keys |> List.choose (fun (l, ks) -> if l = defaultLang then Some ks else None) |> List.tryHead |> Option.defaultValue Set.empty
            //let key = leaf.Value |> (function |QString s -> s |s -> s.ToString())
            CWTools.Validation.Stellaris.STLLocalisationValidation.checkLocNameNE defaultKeys (defaultLang) key
        |false ->
            CWTools.Validation.Stellaris.STLLocalisationValidation.checkLocKeysLeafOrNodeNE keys key

    let checkTypeField (typesMap : Collections.Map<_,StringSet>) severity (typetype : TypeType) (key : string) leafornode errors =
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
            if value |> firstCharEqualsAmp then errors else
            let values = if isComplex then values.ToList() |> List.map typeKeyMap |> (fun ts -> StringSet.Create(InsensitiveStringComparer(), ts)) else values

            //let values = values typeKeyMap values
            if values.Contains value then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected value of type %s" fieldType) severity) leafornode <&&&> errors
        |None -> inv (ErrorCodes.CustomError (sprintf "Unknown type referenced %s" fieldType) Severity.Error) leafornode <&&&> errors
    let checkTypeFieldNE (typesMap : Collections.Map<_,StringSet>) severity (typetype : TypeType) (key : string) =
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
            if value |> firstCharEqualsAmp then true else
            let values = if isComplex then values.ToList() |> List.map typeKeyMap |> (fun ts -> StringSet.Create(InsensitiveStringComparer(), ts)) else values

            //let values = values typeKeyMap values
            if values.Contains value then true else false
        |None -> false

    let checkVariableGetField (varMap : Collections.Map<_,StringSet>) severity (varName : string) (key : string) leafornode errors =
        match varMap.TryFind varName with
        |Some values ->
            let value = trimQuote key
            if value |> firstCharEqualsAmp then errors else
            if values.Contains (value.Split(ampArray, 2).[0]) then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected defined value of %s, got %s" varName value) (min Severity.Warning severity)) leafornode <&&&> errors
        |None -> inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected defined value of %s, got %s" varName key) (min Severity.Warning severity)) leafornode <&&&> errors
    let checkVariableGetFieldNE (varMap : Collections.Map<_,StringSet>) severity (varName : string) (key : string) =
        match varMap.TryFind varName with
        |Some values ->
            let value = trimQuote key
            if value |> firstCharEqualsAmp then true else
            if values.Contains (value.Split(ampArray, 2).[0]) then true else false
        |None -> false

    let checkFilepathField (files : Collections.Set<string>) (key : string) (leafornode) errors =
        let file = (trimQuote key).Replace("\\","/").Replace(".lua",".shader").Replace(".tga",".dds")
        if files.Contains file then errors else inv (ErrorCodes.MissingFile file) leafornode <&&&> errors
    let checkFilepathFieldNE (files : Collections.Set<string>) (key : string) =
        let file = (trimQuote key).Replace("\\","/").Replace(".lua",".shader").Replace(".tga",".dds")
        if files.Contains file then true else false

    let checkIconField (files :Collections.Set<string>) (folder : string) (key : string) (leafornode) errors =
        let value = folder + "/" + key + ".dds"
        if files.Contains value then errors else inv (ErrorCodes.MissingFile value) leafornode <&&&> errors
    let checkIconFieldNE (files :Collections.Set<string>) (folder : string) (key : string) =
        let value = folder + "/" + key + ".dds"
        if files.Contains value then true else false

    let checkScopeField (effectMap : Map<_,_,_>) (triggerMap : Map<_,_,_>) varSet changeScope anyScope (ctx : RuleContext<_>) (s)  key leafornode errors =
        // log "scope %s %A"key ctx
        let scope = ctx.scopes
        match changeScope false true effectMap triggerMap varSet key scope with
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = ( ^a : (static member AnyScope : ^a) ()) || current = ( ^a : (static member AnyScope : ^a) ()) then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then errors else inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString()) key) leafornode <&&&> errors
        |NotFound _ -> inv (ErrorCodes.ConfigRulesInvalidTarget (s.ToString()) key) leafornode <&&&> errors
        |WrongScope (command, prevscope, expected) -> inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%O" expected) ) leafornode <&&&> errors
        |VarFound -> errors
        |VarNotFound s -> inv (ErrorCodes.ConfigRulesUnsetVariable s) leafornode <&&&> errors
        |_ -> errors
    let checkScopeFieldNE (effectMap : Map<_,_,_>) (triggerMap : Map<_,_,_>) varSet changeScope anyScope (ctx : RuleContext<_>) (s)  key =
        // log "scope %s %A"key ctx
        let scope = ctx.scopes
        match changeScope true true effectMap triggerMap varSet key scope with
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = ( ^a : (static member AnyScope : ^a) ()) || current = ( ^a : (static member AnyScope : ^a) ()) then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then true else false
        |NotFound _ -> false
        |WrongScope (command, prevscope, expected) -> false
        |VarNotFound s -> false
        |_ -> true

    let checkVariableField (effectMap : Map<_,_,_>) (triggerMap : Map<_,_,_>) varSet changeScope anyScope (ctx : RuleContext<_>) isInt min max key leafornode errors =
        let scope = ctx.scopes

        match TryParser.parseDouble key, TryParser.parseInt key, changeScope false true effectMap triggerMap varSet key scope with
        |_, Some i, _ when isInt && min <= float i && max >= float i -> errors
        |Some f, _, _ when min <= f && max >= f -> errors
        |_, _, VarFound -> errors
        |_, _, VarNotFound s -> inv (ErrorCodes.ConfigRulesUnsetVariable s) leafornode <&&&> errors
        //TODO: Better error messages for scope instead of variable
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        // |WrongScope (command, prevscope, expected) -> Invalid [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) leafornode]
        |_, _, NotFound _ -> inv ErrorCodes.ConfigRulesExpectedVariableValue leafornode <&&&> errors
        |_ -> inv (ErrorCodes.CustomError "Expecting a variable, but got a scope" Severity.Error) leafornode <&&&> errors
    let checkVariableFieldNE (effectMap : Map<_,_,_>) (triggerMap : Map<_,_,_>) varSet changeScope anyScope (ctx : RuleContext<_>) isInt min max key =
        let scope = ctx.scopes
        match TryParser.parseDouble key, TryParser.parseInt key, changeScope false true effectMap triggerMap varSet key scope with
        |_, Some i, _ -> isInt && min <= float i && max >= float i
        |Some f, _, _ -> min <= f && max >= f
        |_, _, VarFound -> true
        |_, _, VarNotFound s -> false
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        // |NotFound _ -> Invalid [inv (ErrorCodes.ConfigRulesInvalidTarget (s.ToString())) leafornode]
        // |WrongScope (command, prevscope, expected) -> Invalid [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) leafornode]
        |_ -> false

    type checkFieldParams<'S when 'S :> IScope<'S> and 'S : comparison> =
        {
            varMap : Collections.Map<string, StringSet>
            enumsMap : Collections.Map<string, string * StringSet>
            typesMap : Collections.Map<string,StringSet>
            effectMap : Map<string,Effect<'S>,InsensitiveStringComparer>
            triggerMap : Map<string,Effect<'S>,InsensitiveStringComparer>
            varSet : StringSet
            localisation : (Lang * Collections.Set<string>) list
            files : Collections.Set<string>
            changeScope : bool -> bool -> EffectMap<'S> -> EffectMap<'S> -> StringSet -> string -> ScopeContext<'S> -> ScopeResult<'S>
            anyScope : 'S
            defaultLang : Lang
            severity : Severity
            ctx : RuleContext<'S>
        }
    let checkField (p : checkFieldParams<_>) (field : NewField<_>) id (key : string) (leafornode : IKeyPos) errors =
            match field with
            |ValueField vt ->
                checkValidValue p.enumsMap p.severity vt id key leafornode errors
            |TypeField t -> checkTypeField p.typesMap p.severity t key leafornode errors
            |ScopeField s -> checkScopeField p.effectMap p.triggerMap p.varSet p.changeScope p.anyScope p.ctx s key leafornode errors
            |LocalisationField synced -> checkLocalisationField p.localisation p.defaultLang synced key leafornode errors
            |FilepathField -> checkFilepathField p.files key leafornode errors
            |IconField folder -> checkIconField p.files folder key leafornode errors
            |VariableSetField v -> errors
            |VariableGetField v -> checkVariableGetField p.varMap p.severity v key leafornode errors
            |VariableField (isInt, (min, max)) -> checkVariableField p.effectMap p.triggerMap p.varSet p.changeScope p.anyScope p.ctx isInt min max key leafornode errors
            |_ -> errors
    let checkFieldNE (p : checkFieldParams<_>) (field : NewField<_>) id (key : string) =
            match field with
            |ValueField vt ->
                checkValidValueNE p.enumsMap p.severity vt id key
            |TypeField t -> checkTypeFieldNE p.typesMap p.severity t key
            |ScopeField s -> checkScopeFieldNE p.effectMap p.triggerMap p.varSet p.changeScope p.anyScope p.ctx s key
            |LocalisationField synced -> checkLocalisationFieldNE p.localisation p.defaultLang synced key
            |FilepathField -> checkFilepathFieldNE p.files key
            |IconField folder -> checkIconFieldNE p.files folder key
            |VariableSetField v -> true
            |VariableGetField v -> checkVariableGetFieldNE p.varMap p.severity v key
            |VariableField (isInt, (min, max))-> checkVariableFieldNE p.effectMap p.triggerMap p.varSet p.changeScope p.anyScope p.ctx isInt min max key
            |TypeMarkerField (dummy, _) -> dummy = id
            |_ -> false

    let checkLeftField (p : checkFieldParams<_>) (field : NewField<_>) id (key : string) =
        checkFieldNE p field id key

    let checkFieldByKey (p : checkFieldParams<_>) (field : NewField<_>) id (key : string) =
        checkLeftField p field id key

    let inline validateTypeLocalisation (typedefs : TypeDefinition<_> list) (invertedTypeMap : Collections.Map<string, string list>) (localisation) (typeKey : string) (key : string) (leafornode) =
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


    let typekeyfilter (td : TypeDefinition<_>) (n : string) =
        match td.typeKeyFilter with
        | Some (values, negate) -> ((values |> List.exists ((==) n))) <> negate
        | None -> true
        &&
        match td.startsWith with
        | Some prefix -> n.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)
        | None -> true


    // let inline ruleApplicatorCreator(rootRules : RootRule<_> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>, enums : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<_>,InsensitiveStringComparer>, effects : Map<string,Effect<_>,InsensitiveStringComparer>, anyScope, changeScope, (defaultContext : ScopeContext<_>), checkLocField :( (Lang * Collections.Set<string> )list -> bool -> string -> _ -> ValidationResult)) =
    // let inline ruleApplicatorCreator(rootRules : RootRule< ^T> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>, enums : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<_>,InsensitiveStringComparer>, effects : Map<string,Effect<_>,InsensitiveStringComparer>, anyScope, changeScope, (defaultContext : ScopeContext<_>), defaultLang) =
    type RuleApplicator<'T when 'T :> IScope<'T> and 'T : equality and 'T : comparison>
                                    (rootRules : RootRule<'T> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>,
                                     enums : Collections.Map<string, string * StringSet>, varMap : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<'T>,InsensitiveStringComparer>, effects : Map<string,Effect<'T>,InsensitiveStringComparer>,
                                     anyScope, changeScope, defaultContext : ScopeContext<_>, defaultLang) =

        let mutable errorList : ResizeArray<CWError> = new ResizeArray<CWError>()
        let triggerMap = triggers //|> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))
        let effectMap = effects //|> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))


        let aliases =
            rootRules |> List.choose (function |AliasRule (a, rs) -> Some (a, rs) |_ -> None)
                        |> List.groupBy fst
                        |> List.map (fun (k, vs) -> k, vs |> List.map snd)
                        |> Collections.Map.ofList
        let typeRules =
            rootRules |> List.choose (function |TypeRule (k, rs) -> Some (k, rs) |_ -> None)
        let typesMap = types //|> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))) |> Map.ofSeq
        let enumsMap = enums //|> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), s)) |> Map.ofSeq
        //let varMap = varMap |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), s)) |> Map.ofSeq
        let varSet = varMap.TryFind "variable" |> Option.defaultValue (StringSet.Empty(InsensitiveStringComparer()))

        // let isValidValue (value : Value) =
        //     let key = value.ToString().Trim([|'"'|])
        //     function
        //     |ValueType.Bool ->
        //         key = "yes" || key = "no"
        //     |ValueType.Enum e ->
        //         match enumsMap.TryFind e with
        //         |Some es -> es.Contains key
        //         |None -> true
        //     |ValueType.Float (min, max)->
        //         match value with
        //         |Float f -> true
        //         |Int _ -> true
        //         |_ -> false
        //     |ValueType.Specific s -> key = s
        //     |ValueType.Percent -> key.EndsWith("%")
        //     |_ -> true
        let monitor = new Object()
        let mutable i = 0;

        let memoizeRulesInner memFunction =
            let dict = new System.Runtime.CompilerServices.ConditionalWeakTable<_,System.Collections.Generic.Dictionary<_,_>>()
            fun (rules : NewRule<_> list) (subtypes : string list) ->
                    match dict.TryGetValue(rules) with
                    | (true, v) ->
                        match v.TryGetValue(subtypes) with
                        |(true, v2) -> v2
                        |_ ->
                            let temp = memFunction rules subtypes
                            lock monitor (fun () ->
                                if v.ContainsKey(subtypes) then () else v.Add(subtypes, temp)
                            )
                            temp
                    | _ ->
                        let temp = memFunction rules subtypes
                        let innerDict = new System.Collections.Generic.Dictionary<_,_>()
                        lock monitor (fun () ->
                            innerDict.Add(subtypes, temp)
                            match dict.TryGetValue(rules) with
                            |(true, v2) -> ()
                            |_ ->
                                dict.Add(rules, innerDict)
                        )
                        // i <- i + 1
                        // eprintfn "%i %i" i (((rules) :> Object).GetHashCode())
                        temp

        let memoizeRules =
            let memFunction =
                fun rules subtypes ->
                    let subtypedrules =
                        rules |> List.collect (fun (r,o) -> r |> (function |SubtypeRule (key, shouldMatch, cfs) -> (if (not shouldMatch) <> List.contains key subtypes then cfs else []) | x -> []))
                    let expandedbaserules =
                        rules |> List.collect (
                            function
                            | (LeafRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                            | (NodeRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                            |x -> [])
                    let expandedsubtypedrules =
                        subtypedrules |> List.collect (
                            function
                            | (LeafRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                            | (NodeRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                            |x -> [])
                    let res = expandedsubtypedrules @ subtypedrules @ rules @ expandedbaserules

                    res
                        |> Seq.fold (fun (na, la, lva) r ->
                            match r with
                            | (NodeRule (l, rs), o) -> (l, rs, o)::na, la, lva
                            | (LeafRule (l, r), o) -> na, (l, r, o)::la, lva
                            | (LeafValueRule (lv), o) -> na, la,(lv, o)::lva
                            | _ -> na, la, lva
                            ) ([], [], [])
                    // seq { yield! rules; yield! subtypedrules; yield! expandedbaserules; yield! expandedsubtypedrules }
            memoizeRulesInner memFunction


        let rec applyClauseField (enforceCardinality : bool) (nodeSeverity : Severity option) (ctx : RuleContext<_>) (rules : NewRule<_> list) (startNode : Node) errors =
            let severity = nodeSeverity |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)
            // TODO: Memoize expanded rules depending  on ctx.subtypes ad rules?
            let noderules, leafrules, leafvaluerules = memoizeRules rules ctx.subtypes
            // let subtypedrules =
            //     rules |> Array.collect (fun (r,o) -> r |> (function |SubtypeRule (key, shouldMatch, cfs) -> (if (not shouldMatch) <> List.contains key ctx.subtypes then cfs else [||]) | x -> [||]))
            // let expandedbaserules =
            //     rules |> Array.collect (
            //         function
            //         | (LeafRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [||])
            //         | (NodeRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [||])
            //         |x -> [||])
            // let expandedsubtypedrules =
            //     subtypedrules |> Array.collect (
            //         function
            //         | (LeafRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [||])
            //         | (NodeRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [||])
            //         |x -> [||])
            // let expandedrules = seq { yield! rules; yield! subtypedrules; yield! expandedbaserules; yield! expandedsubtypedrules }
            let p = {
                varMap = varMap
                enumsMap = enumsMap
                typesMap = typesMap
                effectMap = effectMap
                triggerMap = triggerMap
                varSet = varSet
                localisation = localisation
                files = files
                changeScope = changeScope
                anyScope = anyScope
                defaultLang = defaultLang
                ctx = ctx
                severity = Severity.Error
            }
            let valueFun innerErrors (leaf : Leaf) =
                let createDefault() = if enforceCardinality && ((leaf.Key.[0] = '@') |> not) then inv (ErrorCodes.ConfigRulesUnexpectedProperty (sprintf "Unexpected node %s in %s" leaf.Key startNode.Key) severity) leaf <&&&> innerErrors else innerErrors
                leafrules |> Seq.filter (fun (l, r, o) -> checkLeftField p l leaf.KeyId.lower leaf.Key)
                              |> (fun rs -> lazyErrorMerge rs (fun (l, r, o) e -> applyLeafRule ctx o r leaf e) createDefault innerErrors true)
            let nodeFun innerErrors (node : Node) =
                let createDefault() = if enforceCardinality then inv (ErrorCodes.ConfigRulesUnexpectedProperty (sprintf "Unexpected node %s in %s" node.Key startNode.Key) severity) node <&&&> innerErrors else innerErrors
                noderules |> Seq.filter (fun (l, rs, o) -> checkLeftField p l node.KeyId.lower node.Key)
                              |> (fun rs -> lazyErrorMerge rs (fun (l, r, o) e -> applyNodeRule enforceCardinality ctx o l r node e) createDefault innerErrors false)
            let leafValueFun innerErrors (leafvalue : LeafValue) =
                let createDefault() = if enforceCardinality then inv (ErrorCodes.ConfigRulesUnexpectedProperty (sprintf "Unexpected node %s in %s" leafvalue.Key startNode.Key) severity) leafvalue <&&&> innerErrors else innerErrors
                leafvaluerules |> Seq.filter (fun (l, o) -> checkLeftField p l leafvalue.ValueId.lower leafvalue.Key)
                              |> (fun rs -> lazyErrorMerge rs (fun (l, o) e -> applyLeafValueRule ctx o l leafvalue e) createDefault innerErrors true)
            let checkCardinality (node : Node) innerErrors (rule : NewRule<_>) =
                match rule with
                |NodeRule(ValueField (ValueType.Specific key), _), opts
                |LeafRule(ValueField (ValueType.Specific key), _), opts ->
                    let leafcount = node.Values |> List.filter (fun leaf -> leaf.KeyId.lower = key) |> List.length
                    let childcount = node.Children |> List.filter (fun child -> child.KeyId.lower = key) |> List.length
                    let total = leafcount + childcount
                    if opts.min > total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %s, expecting at least %i" (StringResource.stringManager.GetStringForID key) opts.min) (opts.severity |> Option.defaultValue severity)) node <&&&> innerErrors
                    else if opts.max < total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many %s, expecting at most %i" (StringResource.stringManager.GetStringForID key) opts.max) Severity.Warning) node <&&&> innerErrors
                    else innerErrors
                |NodeRule(AliasField(_), _), _
                |LeafRule(AliasField(_), _), _
                |LeafValueRule(AliasField(_)), _ -> innerErrors
                |NodeRule(l, _), opts ->
                    let total = node.Children |> List.filter (fun child -> checkLeftField p l child.KeyId.lower child.Key) |> List.length
                    if opts.min > total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %O, expecting at least %i" l opts.min) (opts.severity |> Option.defaultValue severity)) node <&&&> innerErrors
                    else if opts.max < total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many n %O, expecting at most %i" l opts.max) Severity.Warning) node <&&&> innerErrors
                    else innerErrors
                |LeafRule(l, r), opts ->
                    let total = node.Values |> List.filter (fun leaf -> checkLeftField p l leaf.KeyId.lower leaf.Key) |> List.length
                    if opts.min > total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %O, expecting at least %i" l opts.min) (opts.severity |> Option.defaultValue severity)) node <&&&> innerErrors
                    else if opts.max < total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many l %O %O, expecting at most %i" l r opts.max) Severity.Warning) node <&&&> innerErrors
                    else innerErrors
                |LeafValueRule(l), opts ->
                    let total = node.LeafValues |> List.ofSeq |> List.filter (fun leafvalue -> checkLeftField p l leafvalue.ValueId.lower leafvalue.Key) |> List.length
                    if opts.min > total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %O, expecting at least %i" l opts.min) (opts.severity |> Option.defaultValue severity)) node <&&&> innerErrors
                    else if opts.max < total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many lv %O, expecting at most %i" l opts.max) Severity.Warning) node <&&&> innerErrors
                    else innerErrors
                |_ -> innerErrors
            (applyToAll startNode.Leaves valueFun errors)
            |>
            (applyToAll startNode.Children nodeFun)
            |>
            (applyToAll startNode.LeafValues leafValueFun)
            |>
            (applyToAll rules (checkCardinality startNode))

        and applyValueField severity (vt : CWTools.Parser.ConfigParser.ValueType) (leaf : Leaf) =
            checkValidValue enumsMap severity vt (leaf.ValueId.lower) (leaf.ValueText) leaf

        and applyLeafValueRule (ctx : RuleContext<_>) (options : Options<_>) (rule : NewField<_>) (leafvalue : LeafValue) errors =
            let severity = options.severity |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)
            // let errors = OK
            let p = {
                varMap = varMap
                enumsMap = enumsMap
                typesMap = typesMap
                effectMap = effectMap
                triggerMap = triggerMap
                varSet = varSet
                localisation = localisation
                files = files
                changeScope = changeScope
                anyScope = anyScope
                defaultLang = defaultLang
                ctx = ctx
                severity = severity
            }

            checkField p rule (leafvalue.ValueId.lower) (leafvalue.ValueText) (leafvalue) errors

        and applyLeafRule (ctx : RuleContext<_>) (options : Options<_>) (rule : NewField<_>) (leaf : Leaf) errors =
            let severity = options.severity |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)
            let p = {
                varMap = varMap
                enumsMap = enumsMap
                typesMap = typesMap
                effectMap = effectMap
                triggerMap = triggerMap
                varSet = varSet
                localisation = localisation
                files = files
                changeScope = changeScope
                anyScope = anyScope
                defaultLang = defaultLang
                ctx = ctx
                severity = severity
            }

            // let errors = OK
            (match options.requiredScopes with
            |[] -> OK
            |xs ->
                match ctx.scopes.CurrentScope with
                |x when x = anyScope -> OK
                |s -> if List.exists (fun x -> s.MatchesScope x) xs then OK else Invalid [inv (ErrorCodes.ConfigRulesRuleWrongScope (s.ToString()) (xs |> List.map (fun f -> f.ToString()) |> String.concat ", ") (leaf.Key)) leaf])
            <&&>
            checkField p rule (leaf.ValueId.lower) (leaf.ValueText) (leaf) errors
        and applyNodeRule (enforceCardinality : bool) (ctx : RuleContext<_>) (options : Options<_>) (rule : NewField<_>) (rules : NewRule<_> list) (node : Node) errors =
            let severity = options.severity |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)
            let newCtx  =
                match options.pushScope with
                |Some ps ->
                    {ctx with scopes = {ctx.scopes with Scopes = ps::ctx.scopes.Scopes}}
                |None ->
                    match options.replaceScopes with
                    |Some rs ->
                        let prevctx =
                            match rs.prevs with
                            |Some prevs -> {ctx with scopes = {ctx.scopes with Scopes = prevs}}
                            |None -> ctx
                        let newctx =
                            match rs.this, rs.froms with
                            |Some this, Some froms ->
                                {prevctx with scopes = {prevctx.scopes with Scopes = this::(prevctx.scopes.PopScope); From = froms}}
                            |Some this, None ->
                                {prevctx with scopes = {prevctx.scopes with Scopes = this::(prevctx.scopes.PopScope)}}
                            |None, Some froms ->
                                {prevctx with scopes = {prevctx.scopes with From = froms}}
                            |None, None ->
                                prevctx
                        match rs.root with
                        |Some root ->
                            {newctx with scopes = {newctx.scopes with Root = root}}
                        |None -> newctx
                    |None ->
                        ctx
            (match options.requiredScopes with
            |[] -> OK
            |xs ->
                match ctx.scopes.CurrentScope with
                |x when x = anyScope  -> OK
                |s -> if List.exists (fun x -> s.MatchesScope x) xs then OK else Invalid [inv (ErrorCodes.ConfigRulesRuleWrongScope (s.ToString()) (xs |> List.map (fun f -> f.ToString()) |> String.concat ", ") (node.Key)) node])
            <&&>
            match rule with
            |ScopeField s ->
                let scope = newCtx.scopes
                let key = node.Key
                match changeScope false true effectMap triggerMap varSet key scope with
                |NewScope (newScopes ,_) ->
                    let newCtx = {newCtx with scopes = newScopes}
                    applyClauseField enforceCardinality options.severity newCtx rules node errors
                |NotFound _ ->
                    inv (ErrorCodes.ConfigRulesInvalidScopeCommand key) node <&&&> errors
                |WrongScope (command, prevscope, expected) ->
                    inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) node <&&&> errors
                |VarFound ->
                    let newCtx = {newCtx with scopes = { newCtx.scopes with Scopes = anyScope::newCtx.scopes.Scopes }}
                    applyClauseField enforceCardinality options.severity newCtx rules node errors
                |VarNotFound v ->
                    inv (ErrorCodes.CustomError (sprintf "The variable %s has not been set" v) Severity.Error) node <&&&> errors
                |_ -> inv (ErrorCodes.CustomError "Something went wrong with this scope change" Severity.Hint) node <&&&> errors
            |_ -> applyClauseField enforceCardinality options.severity newCtx rules node errors

        let testSubtype (subtypes : SubTypeDefinition<_> list) (node : Node) =
            let results =
                subtypes |> List.filter (fun st -> st.typeKeyField |> function |Some tkf -> tkf == node.Key |None -> true)
                         |> List.filter (fun st -> st.startsWith |> function | Some sw -> node.Key.StartsWith(sw, StringComparison.OrdinalIgnoreCase) | None -> true )
                        |> List.map (fun s -> s.name, s.pushScope, applyClauseField false None {subtypes = []; scopes = defaultContext; warningOnly = false } (s.rules) node OK)
            let res = results |> List.choose (fun (s, ps, res) -> res |> function |Invalid _ -> None |OK -> Some (ps, s))
            res |> List.tryPick fst, res |> List.map snd

        let rootId = StringResource.stringManager.InternIdentifierToken "root"
        let applyNodeRuleRoot (typedef : TypeDefinition<_>) (rules : NewRule<_> list) (options : Options<_>) (node : Node) =
            let pushScope, subtypes = testSubtype (typedef.subtypes) node
            let startingScopeContext =
                match Option.orElse pushScope options.pushScope with
                |Some ps -> { Root = ps; From = []; Scopes = [ps] }
                |None -> defaultContext
            let context = { subtypes = subtypes; scopes = startingScopeContext; warningOnly = typedef.warningOnly }
            applyNodeRule true context options (ValueField (ValueType.Specific rootId.lower)) rules node OK

        let rootTypeDefs = typedefs |> List.filter (fun td -> td.type_per_file)
        let normalTypeDefs = typedefs |> List.filter (fun td -> td.type_per_file |> not )
        let validate ((path, root) : string * Node) =
            let pathDir = (Path.GetDirectoryName path).Replace("\\","/")
            let file = Path.GetFileName path
            let skiprootkey (skipRootKey : SkipRootKey) (n : Node) =
                match skipRootKey with
                |(SpecificKey key) -> n.Key == key
                |(AnyKey) -> true

            let inner (typedefs : TypeDefinition<_> list) (node : Node) =
                let validateType (typedef : TypeDefinition<_>) (n : Node) =
                    let typerules = typeRules |> List.choose (function |(name, r) when name == typedef.name -> Some r |_ -> None)
                    //let expandedRules = typerules |> List.collect (function | (LeafRule (AliasField a, _),_) -> (aliases.TryFind a |> Option.defaultValue []) |x -> [x])
                    //let expandedRules = typerules |> List.collect (function | _,_,(AliasField a) -> (aliases.TryFind a |> Option.defaultValue []) |x -> [x])
                    //match expandedRules |> List.choose (function |(NodeRule (l, rs), o) when checkLeftField enumsMap typesMap effectMap triggerMap localisation files ctx l node.Key node -> Some (l, rs, o) |_ -> None) with
                    //match expandedRules |> List.tryFind (fun (n, _, _) -> n == typedef.name) with
                    match typerules |> List.tryHead with
                    |Some ((NodeRule ((ValueField (ValueType.Specific (x))), rs), o)) when (StringResource.stringManager.GetStringForID x) == typedef.name->
                        if typekeyfilter typedef n.Key then applyNodeRuleRoot typedef rs o n else OK
                    |_ ->
                        OK
                let pathFilteredTypes = typedefs |> List.filter (fun t -> checkPathDir t pathDir file)
                let rec validateTypeSkipRoot (t : TypeDefinition<_>) (skipRootKeyStack : SkipRootKey list) (n : Node) =
                    match skipRootKeyStack with
                    |[] -> if typekeyfilter t n.Key then validateType t n else OK
                    |head::tail ->
                        if skiprootkey head n
                        then n.Children <&!&> validateTypeSkipRoot t tail
                        else OK
                pathFilteredTypes <&!&> (fun t -> validateTypeSkipRoot t t.skipRootKey node)
            let res = (root.Children <&!&> inner normalTypeDefs)
            let rootres = (inner rootTypeDefs root)
            res <&&> rootres
        member this.ApplyNodeRule(rule, node) = applyNodeRule true {subtypes = []; scopes = defaultContext; warningOnly = false } defaultOptions (ValueField (ValueType.Specific rootId.lower)) rule node OK
        member this.TestSubType(subtypes, node) = testSubtype subtypes node
        member this.RuleValidate() = (fun _ (es : EntitySet<_>) -> es.Raw |> List.map (fun struct(e, _) -> e.logicalpath, e.entity) <&!!&> validate)
        member this.RuleValidateEntity = (fun e -> validate (e.logicalpath, e.entity))
        // {
        //     applyNodeRule = (fun (rule, node) -> applyNodeRule true {subtypes = []; scopes = defaultContext; warningOnly = false } defaultOptions (ValueField (ValueType.Specific "root")) rule node)
        //     testSubtype = (fun ((subtypes), (node)) -> testSubtype subtypes node)
        //     ruleValidate = (fun () -> (fun _ es -> es.Raw |> List.map (fun struct(e, _) -> e.logicalpath, e.entity) <&!!&> validate))
        // }


    // type FoldRules(rootRules : RootRule<_> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>, enums : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<_>,InsensitiveStringComparer>, effects : Map<string,Effect<_>,InsensitiveStringComparer>, ruleApplicator : IRuleApplicator<_>, changeScope, defaultContext, anyScope) =
    type FoldRules<'T when 'T :> IScope<'T> and 'T : equality and 'T : comparison>
                                        (rootRules : RootRule<'T> list, typedefs : TypeDefinition<'T> list , types : Collections.Map<string, StringSet>,
                                         enums : Collections.Map<string, string * StringSet>, varMap : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<'T>,InsensitiveStringComparer>, effects : Map<string,Effect<'T>,InsensitiveStringComparer>, ruleApplicator : RuleApplicator<'T>, changeScope, defaultContext, anyScope, defaultLang) =
        let triggerMap = triggers //|> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))
        let effectMap = effects //|> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))
        let aliases =
            rootRules |> List.choose (function |AliasRule (a, rs) -> Some (a, rs) |_ -> None)
                        |> List.groupBy fst
                        |> List.map (fun (k, vs) -> k, vs |> List.map snd)
                        |> Collections.Map.ofList
        let typeRules =
            rootRules |> List.choose (function |TypeRule (k, rs) -> Some (k, rs) |_ -> None)

        let typesMap = types// |> Map.toSeq |> PSeq.map (fun (k,s) -> k, StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))) |> Map.ofSeq
        let enumsMap = enums //|> Map.toSeq |> PSeq.map (fun (k,s) -> k, StringSet.Create(InsensitiveStringComparer(), s)) |> Map.ofSeq
        let varSet = varMap.TryFind "variable" |> Option.defaultValue (StringSet.Empty(InsensitiveStringComparer()))
        let inner (map : Collections.Map<string,string list>) (subtype : string) (set : StringSet) =
            set.ToList() |> List.fold (fun m v -> Map.tryFind v m |> function | Some ts -> Map.add v (subtype::ts) m | None -> Map.add v [subtype] m ) map
        let invertedTypeMap =
            typesMap |> Map.toList |> List.fold (fun m (t, set) -> inner m t set) Map.empty

        let monitor = new Object()

        let memoizeRulesInner memFunction =
            let dict = new System.Runtime.CompilerServices.ConditionalWeakTable<_,System.Collections.Generic.Dictionary<_,_>>()
            fun (rules : NewRule<_> list) (subtypes : string list) ->
                    match dict.TryGetValue(rules) with
                    | (true, v) ->
                        match v.TryGetValue(subtypes) with
                        |(true, v2) -> v2
                        |_ ->
                            let temp = memFunction rules subtypes
                            lock monitor (fun () ->
                                if v.ContainsKey(subtypes) then () else v.Add(subtypes, temp)
                            )
                            temp
                    | _ ->
                        let temp = memFunction rules subtypes
                        let innerDict = new System.Collections.Generic.Dictionary<_,_>()
                        lock monitor (fun () ->
                            innerDict.Add(subtypes, temp)
                            match dict.TryGetValue(rules) with
                            |(true, v2) -> ()
                            |_ ->
                                dict.Add(rules, innerDict)
                        )
                        temp
        let memoizeRules =
            let memFunction =
                fun rules subtypes ->
                    let subtypedrules =
                        rules |> List.collect (fun (r,o) -> r |> (function |SubtypeRule (key, shouldMatch, cfs) -> (if (not shouldMatch) <> List.contains key subtypes then cfs else []) | x -> []))
                    let expandedbaserules =
                        rules |> List.collect (
                            function
                            | (LeafRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                            | (NodeRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                            |x -> [])
                    let expandedsubtypedrules =
                        subtypedrules |> List.collect (
                            function
                            | (LeafRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                            | (NodeRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                            |x -> [])
                    let res = expandedsubtypedrules @ subtypedrules @ rules @ expandedbaserules

                    res
                        |> Seq.fold (fun (na, la, lva) r ->
                            match r with
                            | (NodeRule (l, rs), o) -> (l, rs, o)::na, la, lva
                            | (LeafRule (l, r), o) -> na, (l, r, o)::la, lva
                            | (LeafValueRule (lv), o) -> na, la,(lv, o)::lva
                            | _ -> na, la, lva
                            ) ([], [], [])

            memoizeRulesInner memFunction

        let rec singleFoldRules fNode fChild fLeaf fLeafValue fComment acc child rule :'r =
            let recurse = singleFoldRules fNode fChild fLeaf fLeafValue fComment
            match child with
            |NodeC node ->
                let finalAcc = fNode acc node rule
                match fChild finalAcc node rule with
                |Some (child, newRule) ->
                    recurse finalAcc child newRule
                |None -> finalAcc
            |LeafC leaf ->
                fLeaf acc leaf rule
            |LeafValueC leafvalue ->
                fLeafValue acc leafvalue rule
            |CommentC comment ->
                fComment acc comment rule

        let rec foldRules fNode fChild fLeaf fLeafValue fComment acc child rule :'r =
            let recurse = foldRules fNode fChild fLeaf fLeafValue fComment
            match child with
            |NodeC node ->
                let finalAcc = fNode acc node rule
                fChild node rule |> Seq.fold (fun a (c, r) -> recurse a c r) finalAcc
            |LeafC leaf ->
                fLeaf acc leaf rule
            |LeafValueC leafvalue ->
                fLeafValue acc leafvalue rule
            |CommentC comment ->
                fComment acc comment rule

        // let rec foldRulesEarlyExit fNode fChild fLeaf fLeafValue fComment acc child rule :'r =
        //     let recurse = foldRulesEarlyExit fNode fChild fLeaf fLeafValue fComment
        //     match child with
        //     |NodeC node ->
        //         let (finalAcc, fin) = fNode acc node rule
        //         if fin
        //         then finalAcc
        //         else fChild node rule |> Seq.fold (fun a (c, r) -> recurse a c r) finalAcc
        //     |LeafC leaf ->
        //         fLeaf acc leaf rule
        //     |LeafValueC leafvalue ->
        //         fLeafValue acc leafvalue rule
        //     |CommentC comment ->
        //         fComment acc comment rule
        let foldWithPos fLeaf fLeafValue fComment fNode acc (pos : pos) (node : Node) (logicalpath : string) =
            let fChild (ctx, _) (node : Node) ((field, options) : NewRule<_>) =
                // log "child acc %A %A" ctx field
                let rules =
                    match field with
                    //| Field.LeftTypeField (t, f) -> inner f newCtx n
                    | NodeRule (_, rs) -> rs
                    // | Field.ClauseField rs -> rs
                    // | Field.LeftClauseField (_, ClauseField rs) -> rs
                    // | Field.LeftScopeField rs -> rs
                    | _ -> []
                let subtypedrules =
                    rules |> List.collect (fun (r,o) -> r |> (function |SubtypeRule (_, _, cfs) -> cfs | x -> [(r, o)]))
                // let subtypedrules =
                //     rules |> List.collect (fun (s,o,r) -> r |> (function |SubtypeField (_, _, ClauseField cfs) -> cfs | x -> [(s, o, x)]))
                //     |None -> rules |> List.choose (fun (s,o,r) -> r |> (function |SubtypeField (key, cf) -> None |x -> Some (s, o, x)))
                let expandedrules =
                    subtypedrules |> List.collect (
                        function
                        | (LeafRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                        | (NodeRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                        |x -> [x])
                let childMatch = node.Children |> List.tryFind (fun c -> rangeContainsPos c.Position pos)
                let leafMatch = node.Leaves |> Seq.tryFind (fun l -> rangeContainsPos l.Position pos)
                let leafValueMatch = node.LeafValues |> Seq.tryFind (fun lv -> rangeContainsPos lv.Position pos)
                // log "child rs %A %A %A %A" (node.Key) childMatch leafMatch leafValueMatch
                // let ctx = { RuleContext.subtypes = []; scop es = defaultContext; warningOnly = false }
                let p = {
                    varMap = varMap
                    enumsMap = enumsMap
                    typesMap = typesMap
                    effectMap = effectMap
                    triggerMap = triggerMap
                    varSet = varSet
                    localisation = localisation
                    files = files
                    changeScope = changeScope
                    anyScope = anyScope
                    defaultLang = defaultLang
                    ctx = ctx
                    severity = Severity.Error
                }

                match childMatch, leafMatch, leafValueMatch with
                |Some c, _, _ ->
                    match expandedrules |> List.tryPick (function |(NodeRule (l, rs), o) when checkLeftField p l c.KeyId.lower c.Key -> Some (l, rs, o) |_ -> None) with
                    | None ->
                            // log "fallback match %s %A" (node.Key) expandedrules
                            Some (NodeC c, (field, options))
                    | Some (l, rs, o) -> Some (NodeC c, ((NodeRule (l, rs)), o))
                |_, Some leaf, _ ->
                    match expandedrules |> List.tryPick (function |(LeafRule (l, r), o) when checkLeftField p l leaf.KeyId.lower leaf.Key -> Some (l, r, o) |_ -> None) with
                    |None ->
                        Some (LeafC leaf, (field, options))
                    |Some (l, rs, o) -> Some (LeafC leaf, ((LeafRule (l, rs)), o))
                |_, _, Some lv -> Some (LeafValueC lv, (field, options))
                |None, None, None -> None
            let pathDir = (Path.GetDirectoryName logicalpath).Replace("\\","/")
            let file = Path.GetFileName logicalpath
            let childMatch = node.Children |> List.tryFind (fun c -> rangeContainsPos c.Position pos)
            //log "%O %A %A" pos pathDir (typedefs |> List.tryHead)
            match childMatch, typedefs |> List.tryFind (fun t -> checkPathDir t pathDir file) with
            |Some c, Some typedef ->
                let typerules = typeRules |> List.filter (fun (name, _) -> name == typedef.name)
                match typerules with
                |[(n, (NodeRule (l, rs), o))] ->
                    Some (singleFoldRules fNode fChild fLeaf fLeafValue fComment acc (NodeC c) ((NodeRule (TypeMarkerField (c.KeyId.lower, typedef), rs), o)))
                |_ -> None
            |_, _ -> None

        let getInfoAtPos (pos : pos) (entity : Entity) =
            let fLeaf (ctx, _) (leaf : Leaf) ((field, o) : NewRule<_>) =
                match field with
                |LeafRule (_, TypeField (TypeType.Simple t)) -> ctx, (Some o, Some (t, leaf.ValueText), Some (LeafC leaf))
                |LeafRule (TypeField (TypeType.Simple t), _) -> ctx, (Some o, Some (t, leaf.Key), Some (LeafC leaf))
                |_ -> ctx, (Some o, None, Some (LeafC leaf))
            let fLeafValue (ctx, _) (leafvalue : LeafValue) (_, o : Options<_>) =
                ctx, (Some o, None, Some (LeafValueC leafvalue))
            let fComment (ctx, _) _ _ = ctx, (None, None, None)
            let fNode (ctx, (_, res, resc)) (node : Node) ((field, options) : NewRule<_>) =
                // let anyScope = ( ^a : (static member AnyScope : ^a) ())
                // log "info fnode inner %s %A %A %A" (node.Key) options field ctx
                let newCtx =
                    match options.pushScope with
                    |Some ps ->
                        {ctx with RuleContext.scopes = {ctx.scopes with Scopes = ps::ctx.scopes.Scopes}}
                    |None ->
                        match options.replaceScopes with
                        |Some rs ->
                            let newctx =
                                match rs.this, rs.froms with
                                |Some this, Some froms ->
                                    {ctx with scopes = {ctx.scopes with Scopes = this::(ctx.scopes.PopScope); From = froms}}
                                |Some this, None ->
                                    {ctx with scopes = {ctx.scopes with Scopes = this::(ctx.scopes.PopScope)}}
                                |None, Some froms ->
                                    {ctx with scopes = {ctx.scopes with From = froms}}
                                |None, None ->
                                    ctx
                            match rs.root with
                            |Some root ->
                                {newctx with scopes = {newctx.scopes with Root = root}}
                            |None -> newctx
                        |None ->
                            if node.Key.StartsWith("event_target:", System.StringComparison.OrdinalIgnoreCase) || node.Key.StartsWith("parameter:", System.StringComparison.OrdinalIgnoreCase)
                            then {ctx with scopes = {ctx.scopes with Scopes = anyScope::ctx.scopes.Scopes}}
                            else ctx
                match field with
                | NodeRule (ScopeField s, f) ->
                    let scope = newCtx.scopes
                    let key = node.Key
                    let newCtx =
                        match changeScope false true effectMap triggerMap varSet key scope with
                        |NewScope ({Scopes = current::_} ,_) ->
                            // log "cs %A %A %A" s node.Key current
                            {newCtx with scopes = {newCtx.scopes with Scopes = current::newCtx.scopes.Scopes}}
                        |VarFound ->
                            // log "cs %A %A %A" s node.Key current
                            {newCtx with scopes = {newCtx.scopes with Scopes = anyScope::newCtx.scopes.Scopes}}
                        |_ -> newCtx
                    newCtx, (Some options, None, Some (NodeC node))
                | NodeRule (TypeMarkerField (_, { name = typename; nameField = None }), _) ->
                    ctx, (Some options, Some (typename, node.Key), Some (NodeC node))
                | NodeRule (TypeMarkerField (_, { name = typename; nameField = Some namefield }), _) ->
                    let typevalue = node.TagText namefield
                    ctx, (Some options, Some (typename, typevalue), Some (NodeC node))
                | NodeRule (TypeField (TypeType.Simple t), _) -> ctx, (Some options, Some (t, node.Key), Some (NodeC node))
                | NodeRule (_, f) -> newCtx, (Some options, None, Some (NodeC node))
                | _ -> newCtx, (Some options, None, Some (NodeC node))

            let pathDir = (Path.GetDirectoryName entity.logicalpath).Replace("\\","/")
            let file = Path.GetFileName entity.logicalpath
            let childMatch = entity.entity.Children |> List.tryFind (fun c -> rangeContainsPos c.Position pos)
            // log "%O %A %A %A" pos pathDir (typedefs |> List.tryHead) (childMatch.IsSome)
            let ctx =
                match childMatch, typedefs |> List.tryFind (fun t -> checkPathDir t pathDir file) with
                |Some c, Some typedef ->
                    let pushScope, subtypes = ruleApplicator.TestSubType (typedef.subtypes, c)
                    match pushScope with
                    |Some ps -> { subtypes = subtypes; scopes = { Root = ps; From = []; Scopes = [ps] }; warningOnly = false}
                    |None -> { subtypes = subtypes; scopes = defaultContext; warningOnly = false }
                |_, _ -> { subtypes = []; scopes = defaultContext; warningOnly = false }

            let ctx = ctx, (None, None, None)
            foldWithPos fLeaf fLeafValue fComment fNode ctx (pos) (entity.entity) (entity.logicalpath)


        let foldCollect fLeaf fLeafValue fComment fNode acc (node : Node) (path: string) =
            let ctx = { subtypes = []; scopes = defaultContext; warningOnly = false  }
            let fChild (node : Node) ((field, options) : NewRule<_>) =
                let rules =
                    match field with
                    | (NodeRule (_, rs)) -> rs
                    | _ -> []
                let noderules, leafrules, leafvaluerules = memoizeRules rules ctx.subtypes
                let p = {
                    varMap = varMap
                    enumsMap = enumsMap
                    typesMap = typesMap
                    effectMap = effectMap
                    triggerMap = triggerMap
                    varSet = varSet
                    localisation = localisation
                    files = files
                    changeScope = changeScope
                    anyScope = anyScope
                    defaultLang = defaultLang
                    ctx = ctx
                    severity = Severity.Error
                }

                let inner (child : Child) =
                    match child with
                    | NodeC c ->
                        noderules |> Seq.choose (fun (l, rs, o) -> if checkLeftField p l c.KeyId.lower c.Key then Some (NodeC c, ((NodeRule (l, rs)), o)) else None)
                    | LeafC leaf ->
                        leafrules |> Seq.choose (fun (l, r, o) -> if checkLeftField p l leaf.KeyId.lower leaf.Key then Some (LeafC leaf, ((LeafRule (l, r)), o)) else None)
                    | LeafValueC leafvalue ->
                        leafvaluerules |> Seq.choose (fun (lv, o) -> if checkLeftField p lv leafvalue.ValueId.lower leafvalue.Key then  Some (LeafValueC leafvalue, ((LeafValueRule (lv)), o)) else None)
                    | CommentC _ -> Seq.empty
                node.AllArray |> Seq.collect inner

            let pathDir = (Path.GetDirectoryName path).Replace("\\","/")
            let file = Path.GetFileName path
            let skiprootkey (skipRootKey : SkipRootKey) (n : Node) =
                match skipRootKey with
                |(SpecificKey key) -> n.Key == key
                |(AnyKey) -> true
            let foldRulesNode typedef rs o =
                (fun a c ->
                    foldRules fNode fChild fLeaf fLeafValue fComment a (NodeC c) (NodeRule (TypeMarkerField (c.KeyId.lower, typedef), rs), o))
            let pathFilteredTypes = typedefs |> List.filter (fun t -> checkPathDir t pathDir file)
            let rec foldRulesSkipRoot rs o (t : TypeDefinition<_>) (skipRootKeyStack : SkipRootKey list) acc (n : Node) =
                match skipRootKeyStack with
                |[] -> if typekeyfilter t n.Key then foldRulesNode t rs o acc n else acc
                |head::tail ->
                    if skiprootkey head n
                    then n.Children |> List.fold (foldRulesSkipRoot rs o t tail) acc
                    else acc
            let foldRulesBase (n : Node) acc (t : TypeDefinition<_>) =
                let typerules = typeRules |> List.filter (fun (name, _) -> name == t.name)
                match typerules with
                |[(_, (NodeRule (_, rs), o))] ->
                    n.Children |> List.fold (foldRulesSkipRoot rs o t t.skipRootKey) acc
                |_ -> acc
            pathFilteredTypes |> List.fold (foldRulesBase node) acc

        // let foldCollectEarly fLeaf fLeafValue fComment fNode acc (node : Node) (path: string) =
        //     let ctx = { subtypes = []; scopes = defaultContext; warningOnly = false  }
        //     let fChild (node : Node) ((field, options) : NewRule<_>) =
        //         let rules =
        //             match field with
        //             | (NodeRule (_, rs)) -> rs
        //             | _ -> []
        //         let noderules, leafrules, leafvaluerules = memoizeRules rules ctx.subtypes
        //         let p = {
        //             varMap = varMap
        //             enumsMap = enumsMap
        //             typesMap = typesMap
        //             effectMap = effectMap
        //             triggerMap = triggerMap
        //             varSet = varSet
        //             localisation = localisation
        //             files = files
        //             changeScope = changeScope
        //             anyScope = anyScope
        //             defaultLang = defaultLang
        //             ctx = ctx
        //             severity = Severity.Error
        //         }
        //         let inner (child : Child) =
        //             match child with
        //             | NodeC c ->
        //                 // expandedrules |> Seq.choose (function |(NodeRule (l, rs), o) when checkLeftField p l c.KeyId.lower c.Key -> Some (NodeC c, ((NodeRule (l, rs)), o)) |_ -> None)
        //                 noderules |> Seq.choose (fun (l, rs, o) -> if checkLeftField p l c.KeyId.lower c.Key then Some (NodeC c, ((NodeRule (l, rs)), o)) else None)
        //             | LeafC leaf ->
        //                 // expandedrules |> Seq.choose (function |(LeafRule (l, r), o) when checkLeftField p l leaf.KeyId.lower leaf.Key -> Some (LeafC leaf, ((LeafRule (l, r)), o)) |_ -> None)
        //                 leafrules |> Seq.choose (fun (l, r, o) -> if checkLeftField p l leaf.KeyId.lower leaf.Key then Some (LeafC leaf, ((LeafRule (l, r)), o)) else None)
        //             | LeafValueC leafvalue ->
        //                 // expandedrules |> Seq.choose (function |(LeafValueRule (lv), o) when checkLeftField p lv leafvalue.ValueId.lower leafvalue.Key ->  Some (LeafValueC leafvalue, ((LeafValueRule (lv)), o)) |_ -> None)
        //                 leafvaluerules |> Seq.choose (fun (lv, o) -> if checkLeftField p lv leafvalue.ValueId.lower leafvalue.Key then  Some (LeafValueC leafvalue, ((LeafValueRule (lv)), o)) else None)
        //             | CommentC _ -> Seq.empty
        //         node.AllArray |> Seq.collect inner
        //     let pathDir = (Path.GetDirectoryName path).Replace("\\","/")
        //     let file = Path.GetFileName path
        //     let typekeyfilter (td : TypeDefinition<_>) (n : Node) =
        //         match td.typeKeyFilter with
        //         |Some (values, negate) -> ((values |> List.exists ((==) n.Key))) <> negate
        //         |None -> true
        //     let skiprootkey (skipRootKey : SkipRootKey) (n : Node) =
        //         match skipRootKey with
        //         |(SpecificKey key) -> n.Key == key
        //         |(AnyKey) -> true
        //     let foldRulesNode typedef rs o =
        //         (fun a c ->
        //             foldRulesEarlyExit fNode fChild fLeaf fLeafValue fComment a (NodeC c) (NodeRule (TypeMarkerField (c.KeyId.lower, typedef), rs), o))
        //     let pathFilteredTypes = typedefs |> List.filter (fun t -> checkPathDir t pathDir file)
        //     let rec foldRulesSkipRoot rs o (t : TypeDefinition<_>) (skipRootKeyStack : SkipRootKey list) acc (n : Node) =
        //         match skipRootKeyStack with
        //         |[] -> if typekeyfilter t n then foldRulesNode t rs o acc n else acc
        //         |head::tail ->
        //             if skiprootkey head n
        //             then n.Children |> List.fold (foldRulesSkipRoot rs o t tail) acc
        //             else acc
        //     let foldRulesBase (n : Node) acc (t : TypeDefinition<_>) =
        //         let typerules = typeRules |> List.filter (fun (name, _) -> name == t.name)
        //         match typerules with
        //         |[(_, (NodeRule (_, rs), o))] ->
        //             n.Children |> List.fold (foldRulesSkipRoot rs o t t.skipRootKey) acc
        //         |_ -> acc
        //     pathFilteredTypes |> List.fold (foldRulesBase node) acc

        let getTypesInEntity = // (entity : Entity) =
            let fLeaf (res : Collections.Map<string, (string * range) list>) (leaf : Leaf) ((field, _) : NewRule<_>) =
                match field with
                |LeafRule (_, TypeField (TypeType.Simple t)) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    res |> (fun m -> m.Add(typename, (leaf.ValueText, leaf.Position)::(m.TryFind(typename) |> Option.defaultValue [])))
                |LeafRule (TypeField (TypeType.Simple t), _) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    res |> (fun m -> m.Add(typename, (leaf.Key, leaf.Position)::(m.TryFind(typename) |> Option.defaultValue [])))
                |_ -> res
            let fLeafValue (res : Collections.Map<string, (string * range) list>) (leafvalue : LeafValue) (field, _) =
                match field with
                |LeafValueRule (TypeField (TypeType.Simple t)) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    res |> (fun m -> m.Add(t, (leafvalue.ValueText, leafvalue.Position)::(m.TryFind(typename) |> Option.defaultValue [])))
                |_ -> res

            let fComment (res) _ _ = res
            let fNode (res) (node : Node) ((field, option) : NewRule<_>) = res
            let fCombine a b = (a |> List.choose id) @ (b |> List.choose id)

            let ctx = typedefs |> List.fold (fun (a : Collections.Map<string, (string * range) list>) t -> a.Add(t.name, [])) Collections.Map.empty
            fLeaf, fLeafValue, fComment, fNode, ctx
            // let res = foldCollect fLeaf fLeafValue fComment fNode ctx (entity.entity) (entity.logicalpath)
            // res

        let getDefVarInEntity = //(ctx : Collections.Map<string, (string * range) list>) (entity : Entity) =
            let getVariableFromString (v : string) (s : string) = if v = "variable" then s.Split('@').[0].Split('.') |> Array.last else s.Split('@').[0]
            let fLeaf (res : Collections.Map<string, (string * range) list>) (leaf : Leaf) ((field, _) : NewRule<_>) =
                match field with
                |LeafRule (_, VariableSetField v) ->
                    res |> (fun m -> m.Add(v, (getVariableFromString v (leaf.ValueText), leaf.Position)::(m.TryFind(v) |> Option.defaultValue [])) )
                |LeafRule (VariableSetField v, _) ->
                    res |> (fun m -> m.Add(v, (getVariableFromString v leaf.Key, leaf.Position)::(m.TryFind(v) |> Option.defaultValue [])) )
                |_ -> res
            let fLeafValue (res : Collections.Map<string, (string * range) list>) (leafvalue : LeafValue) (field, _) =
                match field with
                |LeafValueRule (VariableSetField v) ->
                    res |> (fun m -> m.Add(v, (getVariableFromString v (leafvalue.ValueText), leafvalue.Position)::(m.TryFind(v) |> Option.defaultValue [])) )
                |_ -> res
            let fNode (res : Collections.Map<string, (string * range) list>) (node : Node) ((field, option) : NewRule<_>) =
                match field with
                |NodeRule (VariableSetField v, _) ->
                    res |> (fun m -> m.Add(v, (getVariableFromString v node.Key, node.Position)::(m.TryFind(v) |> Option.defaultValue [])) )
                |_ -> res
            let fComment (res) _ _ = res

            fLeaf, fLeafValue, fComment, fNode, Map.empty
            //let res = foldCollect fLeaf fLeafValue fComment fNode ctx (entity.entity) (entity.logicalpath)
            //res

        let mergeFolds (l1, lv1, c1, n1, ctx1) (l2, lv2, c2, n2, ctx2) =
            let fLeaf = (fun (acc1, acc2) l r -> (l1 acc1 l r, l2 acc2 l r))
            let fLeafValue = (fun (acc1, acc2) lv r -> (lv1 acc1 lv r, lv2 acc2 lv r))
            let fNode = (fun (acc1, acc2) n r -> (n1 acc1 n r, n2 acc2 n r))
            let fComment = (fun (acc1, acc2) c r -> (c1 acc1 c r, c2 acc2 c r))
            fLeaf, fLeafValue, fComment, fNode, (ctx1, ctx2)


        let getEffectsInEntity = //(ctx) (entity : Entity) =
            let fLeaf (res) (leaf : Leaf) ((field, _) : NewRule<_>) = res
            let fLeafValue (res) (leafvalue : LeafValue) (field, _) = res
            let fNode (res : Node list, finished: bool) (node : Node) ((field, option) : NewRule<_>) =
                match finished, field with
                |false, NodeRule (_, rs) when rs |> List.exists (function |LeafRule (AliasField "effect" , _), _-> true |_ -> false) ->
                    node::res, true
                |_ -> res, false
            let fComment (res) _ _ = res

            // let res = foldCollect fLeaf fLeafValue fComment fNode ctx (entity.entity) (entity.logicalpath)
            // res
            fLeaf, fLeafValue, fComment, fNode, ([], false)

        let getTriggersInEntity = //(ctx) (entity : Entity) =
            let fLeaf (res) (leaf : Leaf) ((field, _) : NewRule<_>) = res
            let fLeafValue (res) (leafvalue : LeafValue) (field, _) = res
            let fNode (res : Node list, finished : bool) (node : Node) ((field, option) : NewRule<_>) =
                match finished, field with
                |false, NodeRule (_, rs) when rs |> List.exists (function |LeafRule (AliasField "trigger" , _), _-> true |_ -> false) ->
                    node::res, true
                |_ -> res, false
            let fComment (res) _ _ = res

            fLeaf, fLeafValue, fComment, fNode, ([], false)
            // let res = foldCollect fLeaf fLeafValue fComment fNode ctx (entity.entity) (entity.logicalpath)
            // res
        let allFolds entity =
            let fLeaf, fLeafValue, fComment, fNode, ctx =
                mergeFolds getTriggersInEntity getEffectsInEntity
                |> mergeFolds getDefVarInEntity
                |> mergeFolds getTypesInEntity
            let types, (defvars, (triggers, effects)) = foldCollect fLeaf fLeafValue fComment fNode ctx (entity.entity) (entity.logicalpath)
            (types, defvars, triggers, effects)
        let singleFold (fLeaf, fLeafValue, fComment, fNode, ctx) entity =
            foldCollect fLeaf fLeafValue fComment fNode ctx (entity.entity) (entity.logicalpath)

        let validateLocalisationFromTypes (entity : Entity) =
            let fLeaf (res : ValidationResult) (leaf : Leaf) ((field, _) : NewRule<_>) =
                match field with
                |LeafRule (_, TypeField (TypeType.Simple t)) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    let value = leaf.ValueText
                    // let sets =
                    //     typesMap
                    //     |> Map.filter (fun key values -> key.StartsWith(t, StringComparison.OrdinalIgnoreCase) && values.Contains(value))
                    //     |> Map.toSeq |> Seq.map fst
                    // sets <&!&> (fun s -> validateTypeLocalisation typedefs invertedTypeMap localisation s value leaf) <&&> res
                    if typesMap |> Map.exists (fun key values -> key == t && values.Contains(value))
                    then (validateTypeLocalisation typedefs invertedTypeMap localisation t value leaf) <&&> res
                    else res
                |LeafRule (TypeField (TypeType.Simple t), _) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    let value = leaf.Key
                    // let sets =
                    //     typesMap
                    //     |> Map.filter (fun key values -> key.StartsWith(t, StringComparison.OrdinalIgnoreCase) && values.Contains(value))
                    //     |> Map.toSeq |> Seq.map fst
                    // sets <&!&> (fun s -> validateTypeLocalisation typedefs invertedTypeMap localisation s value leaf) <&&> res
                    if typesMap |> Map.exists (fun key values -> key == t && values.Contains(value))
                    then (validateTypeLocalisation typedefs invertedTypeMap localisation t value leaf) <&&> res
                    else res
                |_ -> res
            let fLeafValue (res : ValidationResult) (leafvalue : LeafValue) (field, _) =
                match field with
                |LeafValueRule (TypeField (TypeType.Simple t)) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    let value = leafvalue.ValueText
                    // let sets =
                    //     typesMap
                    //     |> Map.filter (fun key values -> key.StartsWith(t, StringComparison.OrdinalIgnoreCase) && values.Contains(value))
                    //     |> Map.toSeq |> Seq.map fst
                    // sets <&!&> (fun s -> validateTypeLocalisation typedefs invertedTypeMap localisation s value leafvalue) <&&> res
                    if typesMap |> Map.exists (fun key values -> key == t && values.Contains(value))
                    then (validateTypeLocalisation typedefs invertedTypeMap localisation t value leafvalue) <&&> res
                    else res
                |_ -> res
            let fNode (res : ValidationResult) (node : Node) (field, _) =
                match field with
                |NodeRule (TypeField (TypeType.Simple t), _) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    let value = node.Key
                    // let sets =
                    //     typesMap
                    //     |> Map.filter (fun key values -> key.StartsWith(t, StringComparison.OrdinalIgnoreCase) && values.Contains(value))
                    //     |> Map.toSeq |> Seq.map fst
                    // sets <&!&> (fun s -> validateTypeLocalisation typedefs invertedTypeMap localisation s value node) <&&> res
                    if typesMap |> Map.exists (fun key values -> key == t && values.Contains(value))
                    then (validateTypeLocalisation typedefs invertedTypeMap localisation t value node) <&&> res
                    else res
                |_ -> res

            let fComment (res) _ _ = res
            let fCombine a b = (a |> List.choose id) @ (b |> List.choose id)

            let ctx = OK
            let res = foldCollect fLeaf fLeafValue fComment fNode ctx (entity.entity) (entity.logicalpath)
            res




        let convertToOutput s =
            {
                OutputScopeContext.From = s.From |> List.map (fun f -> f :> obj :?> 'T2)
                Scopes = s.Scopes |> List.map (fun f -> f :> obj :?> 'T2)
                Root = s.Root :> obj :?> 'T2
            }
        let test pos entity = //{OutputScopeContext.From = []; Scopes = []; Root = [] :> obj}
            (getInfoAtPos pos entity ) |> Option.map (fun (s, r) -> (convertToOutput s.scopes), r)
        //((fun (pos, entity) -> (getInfoAtPos pos entity) |> Option.map (fun (p, e) -> p.scopes, e)), (fun (entity) -> getTypesInEntity entity))
        member __.GetInfo(pos : pos, entity : Entity) = (getInfoAtPos pos entity ) |> Option.map (fun (p,e) -> p.scopes, e)
        member __.GetReferencedTypes(entity : Entity) = singleFold getTypesInEntity entity
        member __.GetDefinedVariables(entity : Entity) = singleFold getDefVarInEntity entity
        member __.GetTypeLocalisationErrors(entity : Entity) = validateLocalisationFromTypes entity
        member __.GetEffectBlocks(entity : Entity) = (singleFold getEffectsInEntity entity), (singleFold getTriggersInEntity entity)
        member __.BatchFolds(entity : Entity) = allFolds entity

    // type FoldRules(rootRules : RootRule list, typedefs : TypeDefinition list , types : Collections.Map<string, (string * range) list>, enums : Collections.Map<string, string list>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Effect list, effects : Effect list, ruleApplicator : RuleApplicator) =

    type CompletionService<'T when 'T :> IScope<'T> and 'T : equality and 'T : comparison>
                        (rootRules : RootRule<'T> list, typedefs : TypeDefinition<'T> list , types : Collections.Map<string, StringSet>,
                         enums : Collections.Map<string, string * StringSet>, varMap : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<'T>,InsensitiveStringComparer>, effects : Map<string,Effect<'T>,InsensitiveStringComparer>,
                         globalScriptVariables : string list, changeScope, defaultContext : ScopeContext<'T>, anyScope, oneToOneScopes, defaultLang)  =
        let aliases =
            rootRules |> List.choose (function |AliasRule (a, rs) -> Some (a, rs) |_ -> None)
                        |> List.groupBy fst
                        |> List.map (fun (k, vs) -> k, vs |> List.map snd)
                        |> Collections.Map.ofList
        let typeRules =
            rootRules |> List.choose (function |TypeRule (k, rs) -> Some (k, rs) |_ -> None)
        let typesMap = types //|> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))) |> Map.ofSeq

        //let typesMap = types |> (Map.map (fun _ s -> StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))))
        let enumsMap = enums // |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), s)) |> Map.ofSeq
        let types = types |> Map.map (fun _ s -> s.ToList())

        let triggerMap = triggers// |> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))
        let effectMap = effects// |> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))

        let varSet = varMap.TryFind "variable" |> Option.defaultValue (StringSet.Empty(InsensitiveStringComparer()))

        let getAllKeysInFile (root : Node) =
            let fNode = (fun (x:Node) acc ->
                            let withValues = x.Values |> List.fold (fun a leaf ->  leaf.Key::leaf.ValueText::a) acc
                            let withBoth = x.LeafValues |> Seq.fold (fun a leafvalue -> leafvalue.ValueText::a) withValues
                            x.Key :: withBoth
                        )
            foldNode7 fNode root

        let fieldToCompletionList (field : NewField<_>) =
            match field with
            |ValueField (Enum e) -> enums.TryFind(e) |> Option.bind (fun (_, s) -> if s.IsEmpty then None else Some (s.MaximumElement)) |> Option.defaultValue "x"
            |ValueField v -> getValidValues v |> Option.bind (List.tryHead) |> Option.defaultValue "x"
            |TypeField (TypeType.Simple t) -> types.TryFind(t) |> Option.bind (List.tryHead) |> Option.defaultValue "x"
            |TypeField (TypeType.Complex (p, t, s)) -> types.TryFind(t) |> Option.bind (List.tryHead) |> Option.map (fun n -> p + n + s) |> Option.defaultValue "x"
            |ScopeField _ -> "THIS"
            |_ -> "x"
            //TODO: Expand

        let checkIconField (folder : string) =
            files |> Collections.Set.filter (fun icon -> icon.StartsWith(folder, StringComparison.OrdinalIgnoreCase))
                  |> Collections.Set.toList
                  |> List.map (fun icon -> icon.Replace(".dds",""))
            // let value = folder + "/" + key + ".dds"
            // if files.Contains value then OK else Invalid [inv (ErrorCodes.MissingFile value) leafornode]

        let scopeCompletionList =
            let evs = varMap.TryFind "event_target" |> Option.map (fun l -> l.ToList())
                                                    |> Option.defaultValue []
                                                    |> List.map (fun s -> "event_target:" + s)
            let gevs = varMap.TryFind "global_event_target" |> Option.map (fun l -> l.ToList())
                                                    |> Option.defaultValue []
                                                    |> List.map (fun s -> "event_target:" + s)
            evs @ gevs @ oneToOneScopes

        let createSnippetForClause (scoreFunction : string -> int) (rules : NewRule<_> list) (description : string option) (key : string) =
            let filterToCompletion =
                function
                |LeafRule(ValueField(ValueType.Specific _), _) -> true
                |NodeRule(ValueField(ValueType.Specific _), _) -> true
                |_ -> false
            let ruleToDistinctKey =
                function
                |LeafRule(ValueField(ValueType.Specific s), _) -> StringResource.stringManager.GetStringForID s
                |NodeRule(ValueField(ValueType.Specific s), _) -> StringResource.stringManager.GetStringForID s
                |_ -> ""

            let rulePrint (i : int) =
                function
                |LeafRule(ValueField(ValueType.Specific s), r) ->
                    sprintf "\t%s = ${%i:%s}\n" (StringResource.stringManager.GetStringForID s) (i + 1) (fieldToCompletionList r)
                |NodeRule(ValueField(ValueType.Specific s), _) ->
                    sprintf "\t%s = ${%i:%s}\n" (StringResource.stringManager.GetStringForID s) (i + 1) "{ }"
                |_ -> ""

            let requiredRules = rules |> List.filter (fun (f, o) -> o.min >= 1 && filterToCompletion f)
                                      |> List.distinctBy (fun (f, _) -> ruleToDistinctKey f)
                                      |> List.mapi (fun i (f, _) -> rulePrint i f)
                                      |> String.concat ""

            let score = scoreFunction key
            CompletionResponse.Snippet (key, (sprintf "%s = {\n%s\t$0\n}" key requiredRules), description, Some score)


        let rec getRulePath (pos : pos) (stack : (string * int * string option) list) (node : Node) =
           //log "grp %A %A %A" pos stack (node.Children |> List.map (fun f -> f.ToRaw))
            let countChildren (n2 : Node) (key : string) =
                n2.Childs key |> Seq.length
            match node.Children |> List.tryFind (fun c -> rangeContainsPos c.Position pos) with
            | Some c -> getRulePath pos ((c.Key, countChildren node c.Key, None) :: stack) c
            | None ->
                    /// This handles LHS vs RHS beacuse LHS gets an "x" inserted into it, so fails to match any rules
                    match node.Leaves |> Seq.tryFind (fun l -> rangeContainsPos l.Position pos) with
                    | Some l ->
                        // SHould be <, but for some reason it isn't
                        match l.Position.StartColumn + l.Key.Length + 1 > pos.Column with
                        |true -> (l.Key, countChildren node l.Key, Some l.Key)::stack
                        |false -> (l.Key, countChildren node l.Key, Some l.ValueText)::stack
                    | None -> stack

        and getCompletionFromPath (scoreFunction : 'T list -> ScopeContext<_> -> string -> int) (rules : NewRule<_> list) (stack : (string * int * string option) list) scopeContext =
            // log (sprintf "%A" stack)
            let rec convRuleToCompletion (count : int) (context : ScopeContext<_>) (rule : NewRule<_>) =
                let r, o = rule
                let scoreFunction = scoreFunction o.requiredScopes context
                let createSnippetForClause = createSnippetForClause scoreFunction
                let enough = o.max <= count
                if enough
                then []
                else
                    let keyvalue (inner : string) = CompletionResponse.Snippet (inner, (sprintf "%s = $0" inner), o.description, Some (scoreFunction inner))
                    match r with
                    |NodeRule (ValueField(ValueType.Specific s), innerRules) ->
                        [createSnippetForClause innerRules o.description (StringResource.stringManager.GetStringForID s)]
                    |NodeRule (ValueField(ValueType.Enum e), innerRules) ->
                        enums.TryFind(e) |> Option.map (fun (_, es) -> es.ToList() |> List.map (fun e -> createSnippetForClause innerRules o.description e)) |> Option.defaultValue []
                    |NodeRule (ValueField(_), _) -> []
                    |NodeRule (AliasField(_), _) -> []
                    |NodeRule (FilepathField(_), _) -> []
                    |NodeRule (IconField(folder), innerRules) ->
                        checkIconField folder |> List.map (fun e -> createSnippetForClause innerRules o.description e)
                    |NodeRule (LocalisationField(_), _) -> []
                    |NodeRule (ScopeField(_), innerRules) ->
                        scopeCompletionList |> List.map (fun e -> createSnippetForClause innerRules o.description e)
                    //TODO: Scopes better
                    |NodeRule (SubtypeField(_), _) -> []
                    |NodeRule (TypeField(TypeType.Simple t), innerRules) ->
                        types.TryFind(t) |> Option.map (fun ts -> ts |> List.map (fun e -> createSnippetForClause innerRules o.description e)) |> Option.defaultValue []
                    |NodeRule (TypeField(TypeType.Complex (p,t,s)), innerRules) ->
                        types.TryFind(t) |> Option.map (fun ts -> ts |> List.map (fun e -> createSnippetForClause innerRules o.description (p+e+s))) |> Option.defaultValue []
                    |NodeRule (VariableGetField v, innerRules) ->
                        varMap.TryFind(v) |> Option.map (fun ss -> ss.ToList() |> List.map (fun e -> createSnippetForClause innerRules o.description e)) |> Option.defaultValue []

                    |LeafRule (ValueField(ValueType.Specific s), _) ->
                        [keyvalue (StringResource.stringManager.GetStringForID s)]
                    |LeafRule (ValueField(ValueType.Enum e), _) ->
                        enums.TryFind(e) |> Option.map (fun (_, es) -> es.ToList() |> List.map (fun e -> keyvalue e)) |> Option.defaultValue []
                    |LeafRule (ValueField(_), _) -> []
                    |LeafRule (AliasField(_), _) -> []
                    |LeafRule (FilepathField(_), _) -> []
                    |LeafRule (IconField(folder), _) -> checkIconField folder |> List.map keyvalue
                    |LeafRule (LocalisationField(_), _) -> []
                    |LeafRule (ScopeField(_), _) -> scopeCompletionList |> List.map keyvalue
                        //TODO: Scopes
                    |LeafRule (SubtypeField(_), _) -> []
                    |LeafRule (TypeField(TypeType.Simple t), _) ->
                        types.TryFind(t) |> Option.map (fun ts -> ts |> List.map (fun e -> keyvalue e)) |> Option.defaultValue []
                    |LeafRule (TypeField(TypeType.Complex (p,t,s)), _) ->
                        types.TryFind(t) |> Option.map (fun ts -> ts |> List.map (fun e -> keyvalue (p + e + s))) |> Option.defaultValue []
                    |LeafRule (VariableGetField v, _) ->
                        varMap.TryFind(v) |> Option.map (fun ss -> ss.ToList() |> List.map (fun e -> keyvalue e)) |> Option.defaultValue []
                    |LeafRule (VariableSetField v, _) ->
                        varMap.TryFind(v) |> Option.map (fun ss -> ss.ToList() |> List.map (fun e -> keyvalue e)) |> Option.defaultValue []

                    |LeafValueRule lv ->
                        match lv with
                        |NewField.TypeField (TypeType.Simple t) -> types.TryFind(t) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                        |NewField.TypeField (TypeType.Complex (p,t,s)) -> types.TryFind(t) |> Option.map (fun ns -> List.map (fun n ->  p + n + s) ns) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                        |NewField.ValueField (Enum e) -> enums.TryFind(e) |> Option.map (fun (_, s) -> s.ToList()) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                        |NewField.VariableGetField v -> varMap.TryFind(v) |> Option.map (fun s -> s.ToList()) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                        |NewField.VariableSetField v -> varMap.TryFind(v) |> Option.map (fun s -> s.ToList()) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                        |_ -> []
                    |SubtypeRule(_) -> []
                    |_ -> []
                //TODO: Add leafvalue
                //|_ -> []
                // |LeafValueRule
                // |LeafRule (l, _) -> l
                // |LeafValueRule l -> l
                // |_ -> failwith "Somehow tried to complete into a subtype rule"

                // match o.leafvalue with
                // |false ->
                //     match f with
                //     |Field.ClauseField rs -> [createSnippetForClause rs o.description s]
                //     |Field.ObjectField _ -> [keyvalue s]
                //     |Field.ValueField _ -> [keyvalue s]
                //     |Field.TypeField _ -> [keyvalue s]
                //     |Field.AliasField a -> aliases |> List.choose (fun (al, rs) -> if a == al then Some rs else None) |> List.collect convRuleToCompletion
                //     |Field.LeftClauseField ((ValueType.Enum e), ClauseField rs) -> enums.TryFind(e) |> Option.defaultValue [] |> List.map (fun s -> createSnippetForClause rs o.description s)
                //     |Field.LeftTypeField (t, ClauseField rs) -> types.TryFind(t) |> Option.defaultValue [] |> List.map (fun s -> createSnippetForClause rs o.description s)
                //     |Field.LeftTypeField (t, _) -> types.TryFind(t) |> Option.defaultValue [] |> List.map keyvalue
                //     |_ -> [Simple s]
                // |true ->
                //     match f with
                //     |Field.TypeField t -> types.TryFind(t) |> Option.defaultValue [] |> List.map Simple
                //     |Field.ValueField (Enum e) -> enums.TryFind(e) |> Option.defaultValue [] |> List.map Simple
                //     |_ -> []
            let fieldToRules (field : NewField<'T>) =
                //log "%A" types
                //log "%A" field
                match field with
                |NewField.ValueField (Enum e) -> enums.TryFind(e) |> Option.map (fun (_, s) -> s.ToList()) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                |NewField.ValueField v -> getValidValues v |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                |NewField.TypeField (TypeType.Simple t) -> types.TryFind(t) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                |NewField.TypeField (TypeType.Complex (p,t,s)) -> types.TryFind(t) |>  Option.map (fun ns -> List.map (fun n ->  p + n + s) ns) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                |NewField.LocalisationField s ->
                    match s with
                    |true -> localisation |> List.tryFind (fun (lang, _ ) -> lang = (STL STLLang.Default)) |> Option.map (snd >> Set.toList) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                    |false -> localisation |> List.tryFind (fun (lang, _ ) -> lang <> (STL STLLang.Default)) |> Option.map (snd >> Set.toList) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                |NewField.FilepathField -> files |> Set.toList |> List.map CompletionResponse.CreateSimple
                |NewField.ScopeField _ -> scopeCompletionList |> List.map (CompletionResponse.CreateSimple)
                |NewField.VariableGetField v -> varMap.TryFind v |> Option.map (fun ss -> ss.ToList()) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                |NewField.VariableSetField v -> varMap.TryFind v |> Option.map (fun ss -> ss.ToList()) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                |NewField.VariableField _ -> varMap.TryFind "variable" |> Option.map (fun ss -> ss.ToList()) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                |_ -> []
                //|Field.EffectField -> findRule rootRules rest
                // |Field.ClauseField rs -> findRule rs rest
                // |Field.LeftClauseField (_, ClauseField rs) -> findRule rs rest
                // |Field.ObjectField et ->
                //     match et with
                //     |EntityType.ShipSizes -> [Simple "large"; Simple "medium"]
                //     |_ -> []
                // |Field.ValueField (Enum e) -> if isLeaf then enums.TryFind(e) |> Option.defaultValue [] |> List.map Simple else []
                // |Field.ValueField v -> if isLeaf then getValidValues v |> Option.defaultValue [] |> List.map Simple else []
            let p = {
                varMap = varMap
                enumsMap = enumsMap
                typesMap = typesMap
                effectMap = effectMap
                triggerMap = triggerMap
                varSet = varSet
                localisation = localisation
                files = files
                changeScope = changeScope
                anyScope = anyScope
                defaultLang = defaultLang
                ctx = { subtypes = []; scopes = defaultContext; warningOnly = false }
                severity = Severity.Error
            }

            let rec findRule (rules : NewRule<'T> list) (stack : (string * int * string option) list) (scopeContext) =
                let subtypedRules =
                    rules |> List.collect (
                        function
                        | SubtypeRule(_, _, cfs), _ -> cfs
                        |x -> [x])
                let expandedRules =
                    subtypedRules |> List.collect (
                        function
                        | LeafRule((AliasField a),_),_ -> (aliases.TryFind a |> Option.defaultValue [])
                        | NodeRule((AliasField a),_),_ -> (aliases.TryFind a |> Option.defaultValue [])
                        |x -> [x])
                match stack with
                | [] -> expandedRules |> List.collect (convRuleToCompletion 0 scopeContext)
                | (key, count, None)::rest ->
                    match expandedRules |> List.choose (function | (NodeRule (l, rs), o) when checkFieldByKey p l (StringResource.stringManager.InternIdentifierToken key).lower key -> Some (l, rs, o) | _ -> None) with
                    | [] -> expandedRules |> List.collect (convRuleToCompletion count scopeContext)
                    | fs -> fs |> List.collect (fun (_, innerRules, _) -> findRule innerRules rest scopeContext)
                | (key, count, Some _)::rest ->
                    match expandedRules |> List.choose (function | (LeafRule (l, r), o) when checkFieldByKey p l (StringResource.stringManager.InternIdentifierToken key).lower key -> Some (l, r, o) | _ -> None) with
                    | [] -> expandedRules |> List.collect (convRuleToCompletion count scopeContext)
                    | fs ->
                        //log "%s %A" key fs
                        let res = fs |> List.collect (fun (_, f, _) -> fieldToRules f)
                        //log "res %A" res
                        res
                    // match expandedRules |> List.filter (fun (k,_,_) -> k == key) with
                    // |[] ->
                    //     let leftClauseRule =
                    //         expandedRules |>
                    //         List.tryFind (function |(_, _, LeftClauseField (vt, _)) -> checkValidLeftClauseRule files enumsMap (LeftClauseField (vt, ClauseField [])) key  |_ -> false )
                    //     let leftTypeRule =
                    //         expandedRules |>
                    //         List.tryFind (function |(_, _, LeftTypeField (vt, f)) -> checkValidLeftTypeRule typesMap (LeftTypeField (vt, f)) key  |_ -> false )
                    //     let leftScopeRule =
                    //         expandedRules |>
                    //         List.tryFind (function |(_, _, LeftScopeField (rs)) -> checkValidLeftScopeRule scopes (LeftScopeField (rs)) key  |_ -> false )
                    //     match leftClauseRule, leftTypeRule, leftScopeRule with
                    //     |Some (_, _, f), _, _ ->
                    //         match f with
                    //         |Field.LeftClauseField (_, ClauseField rs) -> findRule rs rest
                    //         |_ -> expandedRules |> List.collect convRuleToCompletion
                    //     |_, Some (_, _, f), _ ->
                    //         match f with
                    //         |Field.LeftTypeField (_, rs) -> fieldToRules rs
                    //         |_ -> expandedRules |> List.collect convRuleToCompletion
                    //     |_, _, Some (_, _, f) ->
                    //         match f with
                    //         |Field.LeftScopeField (rs) -> findRule rs rest
                    //         |_ -> expandedRules |> List.collect convRuleToCompletion
                    //     |None, None, None ->
                    //         expandedRules |> List.collect convRuleToCompletion
                    // |fs -> fs |> List.collect (fun (_, _, f) -> fieldToRules f)
            let res = findRule rules stack scopeContext |> List.distinct
            //log "res2 %A" res
            res
        let scoreFunction (allUsedKeys : string list) (requiredScopes : 'T list) (currentContext : ScopeContext<_>) (key : string) =
            let validScope =
                match requiredScopes with
                | [] -> true
                | xs ->
                    match currentContext.CurrentScope with
                    | x when x = anyScope -> true
                    | s -> List.exists s.MatchesScope xs
            let usedKey = List.contains key allUsedKeys
            match validScope, usedKey with
            | true, true -> 100
            | true, false -> 50
            | false, true -> 10
            | false, false -> 1

        let complete (pos : pos) (entity : Entity) (scopeContext : ScopeContext<_> option) =
            let scopeContext = Option.defaultValue defaultContext scopeContext
            let path = getRulePath pos [] entity.entity |> List.rev
            log (sprintf "%A" path)
            let pathDir = (Path.GetDirectoryName entity.logicalpath).Replace("\\","/")
            let file = Path.GetFileName entity.logicalpath
            // log "%A" typedefs
            // log "%A" pos
            // log "%A" entity.logicalpath
            // log  (sprintf "tb %A" pathDir)
            let skiprootkey (skipRootKey : SkipRootKey) (s : string) =
                match skipRootKey with
                |(SpecificKey key) -> s == key
                |(AnyKey) -> true
            let pathFilteredTypes = typedefs |> List.filter (fun t -> checkPathDir t pathDir file)
            let getCompletion typerules fixedpath = getCompletionFromPath typerules fixedpath
            let allUsedKeys = getAllKeysInFile entity.entity @ globalScriptVariables
            let scoreFunction = scoreFunction allUsedKeys
            let rec validateTypeSkipRoot (t : TypeDefinition<_>) (skipRootKeyStack : SkipRootKey list) (path : (string * int * string option) list) =
                let typerules = typeRules |> List.choose (function |(name, typerule) when name == t.name -> Some typerule |_ -> None)
                match skipRootKeyStack, path with
                |_, [] ->
                    getCompletionFromPath scoreFunction typerules [] scopeContext
                |[], (head, c, _)::tail ->
                    if typekeyfilter t head
                    then
                        getCompletionFromPath scoreFunction typerules ((t.name, c, None)::tail) scopeContext else []
                |head::tail, (pathhead, _, _)::pathtail ->
                    if skiprootkey head pathhead
                    then validateTypeSkipRoot t tail pathtail
                    else []
            let items =
                match path |> List.last with
                |_, count, Some x when x.Length > 0 && x.StartsWith("@x") ->
                    let staticVars = CWTools.Validation.Stellaris.STLValidation.getDefinedVariables entity.entity
                    staticVars |> List.map (fun s -> CompletionResponse.CreateSimple (s))
                |_ ->
                    pathFilteredTypes |> List.collect (fun t -> validateTypeSkipRoot t t.skipRootKey path)
            let scoreForLabel (label : string) =
                if allUsedKeys |> List.contains label then 10 else 1
            items |> List.map
                        (function
                         | Simple (label, None) -> Simple (label, Some (scoreForLabel label))
                         | Detailed (label, desc, None) -> Detailed (label, desc, Some (scoreForLabel label))
                         | Snippet (label, snippet, desc, None) -> Snippet(label, snippet, desc, Some (scoreForLabel label))
                         | x -> x
                         )
            // pathFilteredTypes <&!&> (fun t -> validateTypeSkipRoot t t.skipRootKey node)
            // let skipcomp =
            //     match typedefs |> List.filter (fun t -> checkPathDir t pathDir file && skiprootkey t (if path.Length > 0 then path.Head |> fst else "")) with
            //     |[] -> None
            //     |xs ->
            //         match xs |> List.tryFind (fun t -> checkPathDir t pathDir file && typekeyfilter t (if path.Length > 1 then path.Tail |> List.head |> fst else "")) with
            //         |Some typedef ->
            //             let typerules = typeRules |> List.choose (function |(name, typerule) when name == typedef.name -> Some typerule |_ -> None)
            //             //log "sc %A" path
            //             let fixedpath = if List.isEmpty path then path else (typedef.name, false)::(path |> List.tail |> List.tail)
            //             let completion = getCompletionFromPath typerules fixedpath
            //             Some completion
            //         |None -> None
            // let res =
            //     skipcomp |> Option.defaultWith
            //         (fun () ->
            //         match typedefs |> List.tryFind (fun t -> checkPathDir t pathDir file && typekeyfilter t (if path.Length > 0 then path.Head |> fst else "")) with
            //         |Some typedef ->
            //             let path2 = if typedef.type_per_file then path else if path.Length > 0 then path |> List.tail else path
            //             let typerules = typeRules |> List.choose (function |(name, typerule) when name == typedef.name -> Some typerule |_ -> None)
            //             //log "fc %A" path
            //             let fixedpath = if List.isEmpty path then path else (typedef.name, false)::(path2)
            //             let completion = getCompletionFromPath typerules fixedpath
            //             completion
            //         |None -> getCompletionFromPath (typeRules |> List.map snd) path)
            // //log "res3 %A" res
            // res
        //(fun (pos, entity) -> complete pos entity)
        member __.Complete(pos : pos, entity : Entity, scopeContext) = complete pos entity scopeContext

    let getTypesFromDefinitions (ruleapplicator : RuleApplicator<_>) (types : TypeDefinition<_> list) (es : Entity list) =
        let entities = es |> List.map (fun e -> ((Path.GetDirectoryName e.logicalpath).Replace("\\","/")), e, (Path.GetFileName e.logicalpath), e.validate)
        let getTypeInfo (def : TypeDefinition<_>) =
            entities |> List.choose (fun (path, e, file, validate) -> if checkPathDir def path file then Some (e.entity, file, validate) else None)
                     |> List.collect (fun (e, f, v) ->
                            let inner (n : Node) =
                                let subtypes = ruleapplicator.TestSubType(def.subtypes, n) |> snd |> List.map (fun s -> def.name + "." + s)
                                let key =
                                    match def.nameField with
                                    |Some f -> n.TagText f
                                    |None -> n.Key
                                let result = def.name::subtypes |> List.map (fun s -> s, (v, key, n.Position))
                                if typekeyfilter def n.Key then result else []
                            let childres =
                                let rec skiprootkey (srk : SkipRootKey list) (n : Node)=
                                    match srk with
                                    |[] -> []
                                    |[SpecificKey key] ->
                                        //Too may levels deep
                                        if n.Key == key then n.Children |> List.collect inner else []
                                    |[AnyKey] ->
                                        n.Children |> List.collect inner
                                    |(SpecificKey key)::tail ->
                                        if n.Key == key then n.Children |> List.collect (skiprootkey tail) else []
                                        // n.Children |> List.filter (fun c -> c.Key == key) |> List.collect (fun c -> c.Children |> List.collect (skiprootkey tail))
                                    |(AnyKey)::tail ->
                                        n.Children |> List.collect (skiprootkey tail)
                                        // n.Children |> List.collect (fun c -> c.Children |> List.collect (skiprootkey tail))
                                match def.type_per_file, def.skipRootKey with
                                |true, _ ->
                                    let subtypes = ruleapplicator.TestSubType(def.subtypes, e) |> snd |> List.map (fun s -> def.name + "." + s)
                                    def.name::subtypes |> List.map (fun s -> s, (v, Path.GetFileNameWithoutExtension f, e.Position))
                                |false, [] ->
                                    (e.Children |> List.collect inner)
                                |false, srk ->
                                    e.Children |> List.collect (skiprootkey srk)
                                // |false, _ ->
                            childres
                            @
                            (e.LeafValues |> List.ofSeq |> List.map (fun lv -> def.name, (v ,lv.Value.ToString(), lv.Position))))
        let results = types |> Seq.ofList |> PSeq.collect getTypeInfo |> List.ofSeq |> List.fold (fun m (n, k) -> if Map.containsKey n m then Map.add n (k::m.[n]) m else Map.add n [k] m) Map.empty
        types |> List.map (fun t -> t.name) |> List.fold (fun m k -> if Map.containsKey k m then m else Map.add k [] m ) results

    let getEnumsFromComplexEnums (complexenums : (ComplexEnumDef) list) (es : Entity list) =
        let entities = es |> List.map (fun e -> e.logicalpath.Replace("\\","/"), e)
        let rec inner (enumtree : Node) (node : Node) =
            // log "%A %A" (enumtree.ToRaw) (node.Position.FileName)
            match enumtree.Children with
            |head::_ ->
                if enumtree.Children |> List.exists (fun n -> n.Key = "enum_name")
                then node.Children |> List.map (fun n -> n.Key.Trim([|'\"'|])) else
                node.Children |> List.collect (inner head)
            |[] ->
                if enumtree.LeafValues |> Seq.exists (fun lv -> lv.ValueText = "enum_name")
                then node.LeafValues |> Seq.map (fun lv -> lv.ValueText.Trim([|'\"'|])) |> List.ofSeq
                else
                    match enumtree.Leaves |> Seq.tryFind (fun l -> l.ValueText = "enum_name") with
                    |Some leaf -> node.TagsText (leaf.Key) |> Seq.map (fun k -> k.Trim([|'\"'|])) |> List.ofSeq
                    |None ->
                        match enumtree.Leaves |> Seq.tryFind (fun l -> l.Key == "enum_name") with
                        |Some leaf -> node.Leaves |> Seq.map(fun l -> l.Key.Trim([|'\"'|])) |> List.ofSeq
                        |None -> []
        let getEnumInfo (complexenum : ComplexEnumDef) =
            let cpath = complexenum.path.Replace("\\","/")
            // log "cpath %A %A" cpath (entities |> List.map (fun (_, e) -> e.logicalpath))
            let values = entities |> List.choose (fun (path, e) -> if path.StartsWith(cpath, StringComparison.OrdinalIgnoreCase) then Some e.entity else None)
                                  |> List.collect (fun e -> if complexenum.start_from_root then inner complexenum.nameTree e else  e.Children |> List.collect (inner complexenum.nameTree))
            // log "%A %A" complexenum.name values
            { key = complexenum.name; values = values; description = complexenum.description }
        complexenums |> List.toSeq |> PSeq.map getEnumInfo |> List.ofSeq

    let getDefinedVariables (foldRules : FoldRules<_>) (es : Entity list) =
        // let results = es |> List.toSeq |> PSeq.fold (fun c e -> foldRules.GetDefinedVariables(c,e)) (Collections.Map.empty)//|> List.ofSeq |> List.fold (fun m (n, k) -> if Map.containsKey n m then Map.add n (k::m.[n]) m else Map.add n [k] m) Collections.Map.empty
        let results = es |> List.toSeq |> PSeq.map (fun e -> foldRules.GetDefinedVariables(e))
                            |> Seq.fold (fun m map -> Map.toList map |>  List.fold (fun m2 (n,k) -> if Map.containsKey n m2 then Map.add n (k@m2.[n]) m2 else Map.add n k m2) m) Collections.Map.empty
        results