namespace CWTools.Validation

open CWTools.Parser.ConfigParser
open CWTools.Process
open CWTools.Utilities.Utils
open CWTools.Validation.ValidationCore
open CWTools.Utilities.Position
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
        |true -> pathDir == t.path.Replace("\\","/")
        |false -> pathDir.StartsWith(t.path.Replace("\\","/"))
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
        let file = leaf.Value.ToRawString().Trim('"').Replace("\\","/").Replace(".lua",".shader").Replace(".tga",".dds")
        if files.Contains file then OK else Invalid [inv (ErrorCodes.MissingFile file) leaf]

    let checkIconExists (files :Collections.Set<string>) (folder : string) (leaf : Leaf) =
        let value = folder + "/" + leaf.Value.ToRawString() + ".dds"
        if files.Contains value then OK else Invalid [inv (ErrorCodes.MissingFile value) leaf]


    type CompletionResponse =
    |Simple of label : string
    |Detailed of label : string * desc : string option
    |Snippet of label : string * snippet : string * desc : string option

    // type ScopeContext = IScopeContext<Scope>

    // type RuleContext  = RuleContext<Scope>
    let firstCharEqualsAmp (s : string) = s.Length > 0 && (s.[0] = '@' || s.[0] = '$')
    let quoteArray = [|'\"'|]
    let ampArray = [|'@'|]
    let trimQuote (s : string) = s.Trim(quoteArray)
    let inline checkValidValue (enumsMap : Collections.Map<_, Set<_, _>>) (severity : Severity) (vt : CWTools.Parser.ConfigParser.ValueType) (key : string) leafornode =
        if key |> firstCharEqualsAmp then OK else
                match (vt) with
                    |ValueType.Scalar ->
                        OK
                    |ValueType.Bool ->
                        if key == "yes" || key == "no" then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting yes or no, got %s" key) severity) leafornode]
                    |ValueType.Int (min, max) ->
                        match TryParser.parseInt key with
                        |Some i ->  if i <= max && i >= min then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %i and %i" min max) severity) leafornode]
                        |None -> Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting an integer, got %s" key) severity) leafornode]
                    |ValueType.Float (min, max) ->
                        match TryParser.parseDouble key with
                        |Some f -> if f <= max && f >= min then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %f and %f" min max) severity) leafornode]
                        |None -> Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a float, got %s" key) severity) leafornode]
                    |ValueType.Enum e ->
                        match enumsMap.TryFind e with
                        |Some es -> if es.Contains (trimQuote key) then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a \"%s\" value, e.g. %A" e es) severity) leafornode]
                        |None -> Invalid[inv (ErrorCodes.RulesError (sprintf "Configuration error: there are no defined values for the enum %s" e) severity) leafornode]
                    |ValueType.Specific s ->
                        if trimQuote key == s then OK else Invalid [inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting value %s" s) severity) leafornode]
                    |ValueType.Percent ->
                        if key.EndsWith("%") then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting an percentage, got %s" key) severity) leafornode]
                    |ValueType.Date ->
                        let parts = key.Split([|'.'|])
                        let ok = (parts.Length = 3) && parts.[0].Length <= 4 && Int32.TryParse(parts.[0]) |> fst && Int32.TryParse(parts.[1]) |> fst && Int32.Parse(parts.[1]) <= 12 && Int32.TryParse(parts.[2]) |> fst && Int32.Parse(parts.[2]) <= 31
                        if ok then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a date, got %s" key) severity) leafornode]
    let inline checkValidValueNE (enumsMap : Collections.Map<_, Set<_, _>>) (severity : Severity) (vt : CWTools.Parser.ConfigParser.ValueType) (key : string) leafornode =
        // if key |> firstCharEqualsAmp then true else
        (match (vt) with
            |ValueType.Scalar ->
                true
            |ValueType.Bool ->
                if key == "yes" || key == "no" then true else false
            |ValueType.Int (min, max) ->
                match TryParser.parseInt key with
                |Some i ->  if i <= max && i >= min then true else false
                |None -> false
            |ValueType.Float (min, max) ->
                match TryParser.parseDouble key with
                |Some f -> if f <= max && f >= min then true else false
                |None -> false
            |ValueType.Enum e ->
                match enumsMap.TryFind e with
                |Some es -> if es.Contains (trimQuote key) then true else false
                |None -> false
            |ValueType.Specific s ->
                if trimQuote key == s then true else false
            |ValueType.Percent ->
                if key.EndsWith("%") then true else false
            |ValueType.Date ->
                let parts = key.Split([|'.'|])
                let ok = (parts.Length = 3) && parts.[0].Length <= 4 && Int32.TryParse(parts.[0]) |> fst && Int32.TryParse(parts.[1]) |> fst && Int32.Parse(parts.[1]) <= 12 && Int32.TryParse(parts.[2]) |> fst && Int32.Parse(parts.[2]) <= 31
                if ok then true else false)
        || firstCharEqualsAmp key

    let inline checkLocalisationField (keys : (Lang * Collections.Set<string>) list) defaultLang (synced : bool) (key : string) (leafornode) =
        match synced with
        |true ->
            let defaultKeys = keys |> List.choose (fun (l, ks) -> if l = defaultLang then Some ks else None) |> List.tryHead |> Option.defaultValue Set.empty
            //let key = leaf.Value |> (function |QString s -> s |s -> s.ToString())
            CWTools.Validation.Stellaris.STLLocalisationValidation.checkLocName leafornode defaultKeys (defaultLang) key
        |false ->
            CWTools.Validation.Stellaris.STLLocalisationValidation.checkLocKeysLeafOrNode keys key leafornode
    let inline checkLocalisationFieldNE (keys : (Lang * Collections.Set<string>) list) defaultLang (synced : bool) (key : string) (leafornode) =
        match synced with
        |true ->
            let defaultKeys = keys |> List.choose (fun (l, ks) -> if l = defaultLang then Some ks else None) |> List.tryHead |> Option.defaultValue Set.empty
            //let key = leaf.Value |> (function |QString s -> s |s -> s.ToString())
            CWTools.Validation.Stellaris.STLLocalisationValidation.checkLocNameNE leafornode defaultKeys (defaultLang) key
        |false ->
            CWTools.Validation.Stellaris.STLLocalisationValidation.checkLocKeysLeafOrNodeNE keys key leafornode

    let inline checkTypeField (typesMap : Collections.Map<_,StringSet>) severity (typetype : TypeType) (key : string) leafornode =
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
            if value |> firstCharEqualsAmp then OK else
            let values = if isComplex then values.ToList() |> List.map typeKeyMap |> (fun ts -> StringSet.Create(InsensitiveStringComparer(), ts)) else values

            //let values = values typeKeyMap values
            if values.Contains value then OK else Invalid [inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected value of type %s" fieldType) severity) leafornode]
        |None -> Invalid [inv (ErrorCodes.CustomError (sprintf "Unknown type referenced %s" fieldType) Severity.Error) leafornode]
    let inline checkTypeFieldNE (typesMap : Collections.Map<_,StringSet>) severity (typetype : TypeType) (key : string) leafornode =
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

    let inline checkVariableGetField (varMap : Collections.Map<_,StringSet>) severity (varName : string) (key : string) leafornode =
        match varMap.TryFind varName with
        |Some values ->
            let value = trimQuote key
            if value |> firstCharEqualsAmp then OK else
            if values.Contains (value.Split(ampArray, 2).[0]) then OK else Invalid [inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected defined value of %s, got %s" varName value) (min Severity.Warning severity)) leafornode]
        |None -> Invalid [inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected defined value of %s, got %s" varName key) (min Severity.Warning severity)) leafornode]
    let inline checkVariableGetFieldNE (varMap : Collections.Map<_,StringSet>) severity (varName : string) (key : string) leafornode =
        match varMap.TryFind varName with
        |Some values ->
            let value = trimQuote key
            if value |> firstCharEqualsAmp then true else
            if values.Contains (value.Split(ampArray, 2).[0]) then true else false
        |None -> false

    let inline checkFilepathField (files : Collections.Set<string>) (key : string) (leafornode) =
        let file = (trimQuote key).Replace("\\","/").Replace(".lua",".shader").Replace(".tga",".dds")
        if files.Contains file then OK else Invalid [inv (ErrorCodes.MissingFile file) leafornode]
    let inline checkFilepathFieldNE (files : Collections.Set<string>) (key : string) (leafornode) =
        let file = (trimQuote key).Replace("\\","/").Replace(".lua",".shader").Replace(".tga",".dds")
        if files.Contains file then true else false

    let inline checkIconField (files :Collections.Set<string>) (folder : string) (key : string) (leafornode) =
        let value = folder + "/" + key + ".dds"
        if files.Contains value then OK else Invalid [inv (ErrorCodes.MissingFile value) leafornode]
    let inline checkIconFieldNE (files :Collections.Set<string>) (folder : string) (key : string) (leafornode) =
        let value = folder + "/" + key + ".dds"
        if files.Contains value then true else false

    let inline checkScopeField (effectMap : Map<_,_,_>) (triggerMap : Map<_,_,_>) varSet changeScope anyScope (ctx : RuleContext<_>) (s)  key leafornode =
        // eprintfn "scope %s %A"key ctx
        let scope = ctx.scopes
        match changeScope false true effectMap triggerMap varSet key scope with
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = ( ^a : (static member AnyScope : ^a) ()) || current = ( ^a : (static member AnyScope : ^a) ()) then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        |NotFound _ -> Invalid [inv (ErrorCodes.ConfigRulesInvalidTarget (s.ToString())) leafornode]
        |WrongScope (command, prevscope, expected) -> Invalid [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) leafornode]
        |VarFound -> OK
        |VarNotFound s -> Invalid[inv (ErrorCodes.ConfigRulesUnsetVariable s) leafornode]
        |_ -> OK
    let inline checkScopeFieldNE (effectMap : Map<_,_,_>) (triggerMap : Map<_,_,_>) varSet changeScope anyScope (ctx : RuleContext<_>) (s)  key leafornode =
        // eprintfn "scope %s %A"key ctx
        let scope = ctx.scopes
        match changeScope true true effectMap triggerMap varSet key scope with
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = ( ^a : (static member AnyScope : ^a) ()) || current = ( ^a : (static member AnyScope : ^a) ()) then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then true else false
        |NotFound _ -> false
        |WrongScope (command, prevscope, expected) -> false
        |VarNotFound s -> false
        |_ -> true

    let inline checkVariableField (effectMap : Map<_,_,_>) (triggerMap : Map<_,_,_>) varSet changeScope anyScope (ctx : RuleContext<_>) isInt min max key leafornode =
        let scope = ctx.scopes

        match TryParser.parseDouble key, TryParser.parseInt key, changeScope false true effectMap triggerMap varSet key scope with
        |_, Some i, _ when isInt && min <= float i && max >= float i -> OK
        |Some f, _, _ when min <= f && max >= f -> OK
        |_, _, VarFound -> OK
        |_, _, VarNotFound s -> Invalid[inv (ErrorCodes.ConfigRulesUnsetVariable s) leafornode]
        //TODO: Better error messages for scope instead of variable
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        // |WrongScope (command, prevscope, expected) -> Invalid [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) leafornode]
        |_, _, NotFound _ -> Invalid [inv ErrorCodes.ConfigRulesExpectedVariableValue leafornode]
        |_ -> Invalid [inv (ErrorCodes.CustomError "Expecting a variable, but got a scope" Severity.Error) leafornode]
    let inline checkVariableFieldNE (effectMap : Map<_,_,_>) (triggerMap : Map<_,_,_>) varSet changeScope anyScope (ctx : RuleContext<_>) isInt min max key leafornode =
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


    let checkField varMap (enumsMap : Collections.Map<_, Set<_, _>>) typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang (severity : Severity) (ctx : RuleContext<_>) (field : NewField<_>) (key : string) (leafornode) =
            match field with
            |ValueField vt ->
                checkValidValue enumsMap severity vt key leafornode
            |TypeField t -> checkTypeField typesMap severity t key leafornode
            |ScopeField s -> checkScopeField effectMap triggerMap varSet changeScope anyScope ctx s key leafornode
            |LocalisationField synced -> checkLocalisationField localisation defaultLang synced key leafornode
            |FilepathField -> checkFilepathField files key leafornode
            |IconField folder -> checkIconField files folder key leafornode
            |VariableSetField v -> OK
            |VariableGetField v -> checkVariableGetField varMap severity v key leafornode
            |VariableField (isInt, (min, max)) -> checkVariableField effectMap triggerMap varSet changeScope anyScope ctx isInt min max key leafornode
            |_ -> OK
    let checkFieldNE varMap (enumsMap : Collections.Map<_, Set<_, _>>) typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang (severity : Severity) (ctx : RuleContext<_>) (field : NewField<_>) (key : string) (leafornode) =
            match field with
            |ValueField vt ->
                checkValidValueNE enumsMap severity vt key leafornode
            |TypeField t -> checkTypeFieldNE typesMap severity t key leafornode
            |ScopeField s -> checkScopeFieldNE effectMap triggerMap varSet changeScope anyScope ctx s key leafornode
            |LocalisationField synced -> checkLocalisationFieldNE localisation defaultLang synced key leafornode
            |FilepathField -> checkFilepathFieldNE files key leafornode
            |IconField folder -> checkIconFieldNE files folder key leafornode
            |VariableSetField v -> true
            |VariableGetField v -> checkVariableGetFieldNE varMap severity v key leafornode
            |VariableField (isInt, (min, max))-> checkVariableFieldNE effectMap triggerMap varSet changeScope anyScope ctx isInt min max key leafornode
            |_ -> true

    let checkLeftField varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang (ctx : RuleContext<_>) (field : NewField<_>) (key : string) (leafornode : IKeyPos) =
        checkFieldNE varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang (Severity.Error) ctx field key leafornode

    let checkFieldByKey varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang (ctx : RuleContext<_>) (field : NewField<_>) (key : string) =
        let leaf = LeafValue(Value.String key)
        checkLeftField varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang ctx field key leaf

    let inline validateTypeLocalisation (typedefs : TypeDefinition<_> list) (localisation) (typeKey : string) (key : string) (leafornode) =
        let typenames = typeKey.Split('.')
        let typename = typenames.[0]
        match typedefs |> List.tryFind (fun t -> t.name == typename) with
        |None -> OK
        |Some typedef ->
            let inner =
                (fun l ->
                let lockey = l.prefix + key + l.suffix
                CWTools.Validation.Stellaris.STLLocalisationValidation.checkLocKeysLeafOrNode localisation lockey leafornode)
            let subtype =
                if typenames.Length > 1
                then
                    match typedef.subtypes |> List.tryFind (fun st -> st.name == typenames.[1]) with
                    |None -> OK
                    |Some st -> st.localisation <&!&> inner
                else OK
            typedef.localisation <&!&> inner
            <&&> subtype

    // let inline ruleApplicatorCreator(rootRules : RootRule<_> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>, enums : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<_>,InsensitiveStringComparer>, effects : Map<string,Effect<_>,InsensitiveStringComparer>, anyScope, changeScope, (defaultContext : ScopeContext<_>), checkLocField :( (Lang * Collections.Set<string> )list -> bool -> string -> _ -> ValidationResult)) =
    // let inline ruleApplicatorCreator(rootRules : RootRule< ^T> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>, enums : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<_>,InsensitiveStringComparer>, effects : Map<string,Effect<_>,InsensitiveStringComparer>, anyScope, changeScope, (defaultContext : ScopeContext<_>), defaultLang) =
    type RuleApplicator<'T when 'T :> IScope<'T> and 'T : equality and 'T : comparison>
                                    (rootRules : RootRule<'T> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>,
                                     enums : Collections.Map<string, StringSet>, varMap : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<'T>,InsensitiveStringComparer>, effects : Map<string,Effect<'T>,InsensitiveStringComparer>,
                                     anyScope, changeScope, defaultContext : ScopeContext<_>, defaultLang) =
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

        let isValidValue (value : Value) =
            let key = value.ToString().Trim([|'"'|])
            function
            |ValueType.Bool ->
                key = "yes" || key = "no"
            |ValueType.Enum e ->
                match enumsMap.TryFind e with
                |Some es -> es.Contains key
                |None -> true
            |ValueType.Float (min, max)->
                match value with
                |Float f -> true
                |Int _ -> true
                |_ -> false
            |ValueType.Specific s -> key = s
            |ValueType.Percent -> key.EndsWith("%")
            |_ -> true


        let rec applyClauseField (enforceCardinality : bool) (nodeSeverity : Severity option) (ctx : RuleContext<_>) (rules : NewRule<_> list) (startNode : Node) =
            let severity = nodeSeverity |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)
            let subtypedrules =
                rules |> List.collect (fun (r,o) -> r |> (function |SubtypeRule (key, shouldMatch, cfs) -> (if (not shouldMatch) <> List.contains key ctx.subtypes then cfs else []) | x -> [(r, o)]))
            let expandedrules =
                subtypedrules |> List.collect (
                    function
                    | (LeafRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                    | (NodeRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                    |x -> [x])
            let valueFun (leaf : Leaf) =
                let createDefault() = if enforceCardinality && ((leaf.Key.[0] = '@') |> not) then Invalid [inv (ErrorCodes.ConfigRulesUnexpectedProperty (sprintf "Unexpected node %s in %s" leaf.Key startNode.Key) severity) leaf] else OK
                expandedrules |> Seq.choose (function |(LeafRule (l, r), o) when checkLeftField varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang ctx l leaf.Key (leaf :> IKeyPos) -> Some (l, r, o) |_ -> None)
                              |> (fun rs -> lazyErrorMerge rs (fun (l, r, o) -> applyLeafRule ctx o r leaf) createDefault true)
            let nodeFun (node : Node) =
                let createDefault() = if enforceCardinality then Invalid [inv (ErrorCodes.ConfigRulesUnexpectedProperty (sprintf "Unexpected node %s in %s" node.Key startNode.Key) severity) node] else OK
                expandedrules |> Seq.choose (function |(NodeRule (l, rs), o) when checkLeftField varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang ctx l node.Key node -> Some (l, rs, o) |_ -> None)
                              |> (fun rs -> lazyErrorMerge rs (fun (l, r, o) -> applyNodeRule enforceCardinality ctx o l r node) createDefault false)
            let leafValueFun (leafvalue : LeafValue) =
                let createDefault() = if enforceCardinality then Invalid [inv (ErrorCodes.ConfigRulesUnexpectedProperty (sprintf "Unexpected node %s in %s" leafvalue.Key startNode.Key) severity) leafvalue] else OK
                expandedrules |> Seq.choose (function |(LeafValueRule (l), o) when checkLeftField varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang ctx l leafvalue.Key leafvalue -> Some (l, o) |_ -> None)
                              |> (fun rs -> lazyErrorMerge rs (fun (l, o) -> applyLeafValueRule ctx o l leafvalue) createDefault true)
            let checkCardinality (node : Node) (rule : NewRule<_>) =
                match rule with
                |NodeRule(ValueField (ValueType.Specific key), _), opts
                |LeafRule(ValueField (ValueType.Specific key), _), opts ->
                    let leafcount = node.Values |> List.filter (fun leaf -> leaf.Key == key) |> List.length
                    let childcount = node.Children |> List.filter (fun child -> child.Key == key) |> List.length
                    let total = leafcount + childcount
                    if opts.min > total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %s, expecting at least %i" key opts.min) (opts.severity |> Option.defaultValue severity)) node]
                    else if opts.max < total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many %s, expecting at most %i" key opts.max) Severity.Warning) node]
                    else OK
                |NodeRule(AliasField(_), _), _
                |LeafRule(AliasField(_), _), _
                |LeafValueRule(AliasField(_)), _ -> OK
                |NodeRule(l, _), opts ->
                    let total = node.Children |> List.filter (fun child -> checkLeftField varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang ctx l child.Key child) |> List.length
                    if opts.min > total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %A, expecting at least %i" l opts.min) (opts.severity |> Option.defaultValue severity)) node]
                    else if opts.max < total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many n %A, expecting at most %i" l opts.max) Severity.Warning) node]
                    else OK
                |LeafRule(l, r), opts ->
                    let total = node.Values |> List.filter (fun leaf -> checkLeftField varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang ctx l leaf.Key leaf) |> List.length
                    if opts.min > total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %A, expecting at least %i" l opts.min) (opts.severity |> Option.defaultValue severity)) node]
                    else if opts.max < total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many l %A %A, expecting at most %i" l r opts.max) Severity.Warning) node]
                    else OK
                |LeafValueRule(l), opts ->
                    let total = node.LeafValues |> List.ofSeq |> List.filter (fun leafvalue -> checkLeftField varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang ctx l leafvalue.Key leafvalue) |> List.length
                    if opts.min > total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %A, expecting at least %i" l opts.min) (opts.severity |> Option.defaultValue severity)) node]
                    else if opts.max < total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many lv %A, expecting at most %i" l opts.max) Severity.Warning) node]
                    else OK
                |_ -> OK
            startNode.Leaves <&!&> valueFun
            <&&>
            (startNode.Children <&!&> nodeFun)
            <&&>
            (startNode.LeafValues <&!&> leafValueFun)
            <&&>
            (rules <&!&> checkCardinality startNode)

        and applyValueField severity (vt : CWTools.Parser.ConfigParser.ValueType) (leaf : Leaf) =
            checkValidValue enumsMap severity vt (leaf.Value.ToRawString()) leaf

        and applyLeafValueRule (ctx : RuleContext<_>) (options : Options<_>) (rule : NewField<_>) (leafvalue : LeafValue) =
            let severity = options.severity |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)

            checkField varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang severity ctx rule (leafvalue.Value.ToRawString()) leafvalue

        and applyLeafRule (ctx : RuleContext<_>) (options : Options<_>) (rule : NewField<_>) (leaf : Leaf) =
            let severity = options.severity |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)
            (match options.requiredScopes with
            |[] -> OK
            |xs ->
                match ctx.scopes.CurrentScope with
                |x when x = anyScope -> OK
                |s -> if List.exists (fun x -> s.MatchesScope x) xs then OK else Invalid [inv (ErrorCodes.ConfigRulesRuleWrongScope (s.ToString()) (xs |> List.map (fun f -> f.ToString()) |> String.concat ", ")) leaf])
            <&&>
            checkField varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang severity ctx rule (leaf.Value.ToRawString()) leaf
        and applyNodeRule (enforceCardinality : bool) (ctx : RuleContext<_>) (options : Options<_>) (rule : NewField<_>) (rules : NewRule<_> list) (node : Node) =
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
                        if node.Key.StartsWith("event_target:", System.StringComparison.OrdinalIgnoreCase) || node.Key.StartsWith("parameter:", System.StringComparison.OrdinalIgnoreCase)
                        then {ctx with scopes = {ctx.scopes with Scopes = anyScope::ctx.scopes.Scopes}}
                        else ctx
            (match options.requiredScopes with
            |[] -> OK
            |xs ->
                match ctx.scopes.CurrentScope with
                |x when x = anyScope  -> OK
                |s -> if List.exists (fun x -> s.MatchesScope x) xs then OK else Invalid [inv (ErrorCodes.ConfigRulesRuleWrongScope (s.ToString()) (xs |> List.map (fun f -> f.ToString()) |> String.concat ", ")) node])
            <&&>
            match rule with
            |ScopeField s ->
                let scope = newCtx.scopes
                let key = node.Key
                match changeScope false true effectMap triggerMap varSet key scope with
                |NewScope (newScopes ,_) ->
                    let newCtx = {newCtx with scopes = newScopes}
                    applyClauseField enforceCardinality options.severity newCtx rules node
                |NotFound _ ->
                    Invalid [inv (ErrorCodes.ConfigRulesInvalidScopeCommand key) node]
                |WrongScope (command, prevscope, expected) ->
                    Invalid [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) node]
                |VarFound ->
                    let newCtx = {newCtx with scopes = { newCtx.scopes with Scopes = anyScope::newCtx.scopes.Scopes }}
                    applyClauseField enforceCardinality options.severity newCtx rules node
                |VarNotFound v ->
                    Invalid [inv (ErrorCodes.CustomError (sprintf "The variable %s has not been set" v) Severity.Error) node]
                |_ -> Invalid [inv (ErrorCodes.CustomError "Something went wrong with this scope change" Severity.Hint) node]
            |_ -> applyClauseField enforceCardinality options.severity newCtx rules node

        let testSubtype (subtypes : SubTypeDefinition<_> list) (node : Node) =
            let results =
                subtypes |> List.filter (fun st -> st.typeKeyField |> function |Some tkf -> tkf == node.Key |None -> true)
                        |> List.map (fun s -> s.name, s.pushScope, applyClauseField false None {subtypes = []; scopes = defaultContext; warningOnly = false } (s.rules) node)
            let res = results |> List.choose (fun (s, ps, res) -> res |> function |Invalid _ -> None |OK -> Some (ps, s))
            res |> List.tryPick fst, res |> List.map snd

        let applyNodeRuleRoot (typedef : TypeDefinition<_>) (rules : NewRule<_> list) (options : Options<_>) (node : Node) =
            let pushScope, subtypes = testSubtype (typedef.subtypes) node
            let startingScopeContext =
                match Option.orElse pushScope options.pushScope with
                |Some ps -> { Root = ps; From = []; Scopes = [ps] }
                |None -> defaultContext
            let context = { subtypes = subtypes; scopes = startingScopeContext; warningOnly = typedef.warningOnly }
            applyNodeRule true context options (ValueField (ValueType.Specific "root")) rules node

        let rootTypeDefs = typedefs |> List.filter (fun td -> td.type_per_file)
        let normalTypeDefs = typedefs |> List.filter (fun td -> td.type_per_file |> not )
        let validate ((path, root) : string * Node) =
            let pathDir = (Path.GetDirectoryName path).Replace("\\","/")
            let file = Path.GetFileName path
            let typekeyfilter (td : TypeDefinition<_>) (n : Node) =
                match td.typeKeyFilter with
                |Some (filter, negate) -> n.Key == filter <> negate
                |None -> true
            let skiprootkey (td : TypeDefinition<_>) (n : Node) =
                match td.skipRootKey with
                |Some (SpecificKey key) -> n.Key == key
                |Some (AnyKey) -> true
                |None -> false

            let inner (typedefs : TypeDefinition<_> list) (node : Node) =
                let validateType (typedef : TypeDefinition<_>) (n : Node) =
                    let typerules = typeRules |> List.choose (function |(name, r) when name == typedef.name -> Some r |_ -> None)
                    //let expandedRules = typerules |> List.collect (function | (LeafRule (AliasField a, _),_) -> (aliases.TryFind a |> Option.defaultValue []) |x -> [x])
                    //let expandedRules = typerules |> List.collect (function | _,_,(AliasField a) -> (aliases.TryFind a |> Option.defaultValue []) |x -> [x])
                    //match expandedRules |> List.choose (function |(NodeRule (l, rs), o) when checkLeftField enumsMap typesMap effectMap triggerMap localisation files ctx l node.Key node -> Some (l, rs, o) |_ -> None) with
                    //match expandedRules |> List.tryFind (fun (n, _, _) -> n == typedef.name) with
                    match typerules |> List.tryHead with
                    |Some ((NodeRule ((ValueField (ValueType.Specific (x))), rs), o)) when x == typedef.name->
                        match typedef.typeKeyFilter with
                        |Some (filter, negate) -> if n.Key == filter <> negate then applyNodeRuleRoot typedef rs o n else OK
                        |None -> applyNodeRuleRoot typedef rs o n
                    |_ ->
                        OK

                let skipres =
                    match typedefs |> List.filter (fun t -> checkPathDir t pathDir file && skiprootkey t node) with
                    |[] -> OK
                    |xs ->
                        node.Children <&!&>
                            (fun c ->
                                match xs |> List.tryFind (fun t -> checkPathDir t pathDir file && typekeyfilter t c) with
                                |Some typedef -> validateType typedef c
                                |None -> OK
                            )

                let nonskipres =
                    match typedefs |> List.tryFind (fun t -> checkPathDir t pathDir file && typekeyfilter t node && t.skipRootKey.IsNone) with
                    |Some typedef -> validateType typedef node
                    |None -> OK
                skipres <&&> nonskipres
            let res = (root.Children <&!&> inner normalTypeDefs)
            let rootres = (inner rootTypeDefs root)
            res <&&> rootres
        member this.ApplyNodeRule(rule, node) = applyNodeRule true {subtypes = []; scopes = defaultContext; warningOnly = false } defaultOptions (ValueField (ValueType.Specific "root")) rule node
        member this.TestSubType(subtypes, node) = testSubtype subtypes node
        member this.RuleValidate() = (fun _ (es : EntitySet<_>) -> es.Raw |> List.map (fun struct(e, _) -> e.logicalpath, e.entity) <&!!&> validate)
        // {
        //     applyNodeRule = (fun (rule, node) -> applyNodeRule true {subtypes = []; scopes = defaultContext; warningOnly = false } defaultOptions (ValueField (ValueType.Specific "root")) rule node)
        //     testSubtype = (fun ((subtypes), (node)) -> testSubtype subtypes node)
        //     ruleValidate = (fun () -> (fun _ es -> es.Raw |> List.map (fun struct(e, _) -> e.logicalpath, e.entity) <&!!&> validate))
        // }


    // type FoldRules(rootRules : RootRule<_> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>, enums : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<_>,InsensitiveStringComparer>, effects : Map<string,Effect<_>,InsensitiveStringComparer>, ruleApplicator : IRuleApplicator<_>, changeScope, defaultContext, anyScope) =
    type FoldRules<'T when 'T :> IScope<'T> and 'T : equality and 'T : comparison>
                                        (rootRules : RootRule<'T> list, typedefs : TypeDefinition<'T> list , types : Collections.Map<string, StringSet>,
                                         enums : Collections.Map<string, StringSet>, varMap : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<'T>,InsensitiveStringComparer>, effects : Map<string,Effect<'T>,InsensitiveStringComparer>, ruleApplicator : RuleApplicator<'T>, changeScope, defaultContext, anyScope, defaultLang) =
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
                fChild node rule |> List.fold (fun a (c, r) -> recurse a c r) finalAcc
            |LeafC leaf ->
                fLeaf acc leaf rule
            |LeafValueC leafvalue ->
                fLeafValue acc leafvalue rule
            |CommentC comment ->
                fComment acc comment rule

        let foldWithPos fLeaf fLeafValue fComment fNode acc (pos : pos) (node : Node) (logicalpath : string) =
            let fChild (ctx, _) (node : Node) ((field, options) : NewRule<_>) =
                // eprintfn "child acc %A %A" ctx field
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
                // eprintfn "child rs %A %A %A %A" (node.Key) childMatch leafMatch leafValueMatch
                // let ctx = { RuleContext.subtypes = []; scop es = defaultContext; warningOnly = false }
                match childMatch, leafMatch, leafValueMatch with
                |Some c, _, _ ->
                    match expandedrules |> List.choose (function |(NodeRule (l, rs), o) when checkLeftField varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang ctx l c.Key c -> Some (l, rs, o) |_ -> None) with
                    | [] ->
                            // eprintfn "fallback match %s %A" (node.Key) expandedrules
                            Some (NodeC c, (field, options))
                    | (l, rs, o)::_ -> Some (NodeC c, ((NodeRule (l, rs)), o))
                |_, Some leaf, _ ->
                    match expandedrules |> List.choose (function |(LeafRule (l, r), o) when checkLeftField varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang ctx l leaf.Key leaf -> Some (l, r, o) |_ -> None) with
                    |[] ->
                        Some (LeafC leaf, (field, options))
                    |(l, rs, o)::_ -> Some (LeafC leaf, ((LeafRule (l, rs)), o))
                |_, _, Some lv -> Some (LeafValueC lv, (field, options))
                |None, None, None -> None
            let pathDir = (Path.GetDirectoryName logicalpath).Replace("\\","/")
            let file = Path.GetFileName logicalpath
            let childMatch = node.Children |> List.tryFind (fun c -> rangeContainsPos c.Position pos)
            //eprintfn "%O %A %A" pos pathDir (typedefs |> List.tryHead)
            match childMatch, typedefs |> List.tryFind (fun t -> checkPathDir t pathDir file) with
            |Some c, Some typedef ->
                let typerules = typeRules |> List.filter (fun (name, _) -> name == typedef.name)
                match typerules with
                |[(n, (NodeRule (l, rs), o))] ->
                    Some (singleFoldRules fNode fChild fLeaf fLeafValue fComment acc (NodeC c) ((NodeRule (ValueField (ValueType.Specific (c.Key)), rs), o)))
                |_ -> None
            |_, _ -> None

        let getInfoAtPos (pos : pos) (entity : Entity) =
            let fLeaf (ctx, res) (leaf : Leaf) ((field, _) : NewRule<_>) =
                match field with
                |LeafRule (_, TypeField (TypeType.Simple t)) -> ctx, Some (t, leaf.Value.ToString())
                |_ -> ctx, res
            let fLeafValue (ctx) (leafvalue : LeafValue) _ =
                ctx
            let fComment (ctx) _ _ = ctx
            let fNode (ctx, res) (node : Node) ((field, options) : NewRule<_>) =
                // let anyScope = ( ^a : (static member AnyScope : ^a) ())
                // eprintfn "info fnode inner %s %A %A %A" (node.Key) options field ctx
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
                            // eprintfn "cs %A %A %A" s node.Key current
                            {newCtx with scopes = {newCtx.scopes with Scopes = current::newCtx.scopes.Scopes}}
                        |VarFound ->
                            // eprintfn "cs %A %A %A" s node.Key current
                            {newCtx with scopes = {newCtx.scopes with Scopes = anyScope::newCtx.scopes.Scopes}}
                        |_ -> newCtx
                    newCtx, res
                | NodeRule (_, f) -> newCtx, res
                | _ -> newCtx, res

            let pathDir = (Path.GetDirectoryName entity.logicalpath).Replace("\\","/")
            let file = Path.GetFileName entity.logicalpath
            let childMatch = entity.entity.Children |> List.tryFind (fun c -> rangeContainsPos c.Position pos)
            // eprintfn "%O %A %A %A" pos pathDir (typedefs |> List.tryHead) (childMatch.IsSome)
            let ctx =
                match childMatch, typedefs |> List.tryFind (fun t -> checkPathDir t pathDir file) with
                |Some c, Some typedef ->
                    let pushScope, subtypes = ruleApplicator.TestSubType (typedef.subtypes, c)
                    match pushScope with
                    |Some ps -> { subtypes = subtypes; scopes = { Root = ps; From = []; Scopes = [ps] }; warningOnly = false}
                    |None -> { subtypes = subtypes; scopes = defaultContext; warningOnly = false }
                |_, _ -> { subtypes = []; scopes = defaultContext; warningOnly = false }

            let ctx = ctx, None
            foldWithPos fLeaf fLeafValue fComment fNode ctx (pos) (entity.entity) (entity.logicalpath)


        let foldCollect fLeaf fLeafValue fComment fNode acc (node : Node) (path: string) =
            let ctx = { subtypes = []; scopes = defaultContext; warningOnly = false  }
            let fChild (node : Node) ((field, options) : NewRule<_>) =
                let rules =
                    match field with
                    | (NodeRule (_, rs)) -> rs
                    //| Field.LeftTypeField (t, f) -> inner f newCtx n
                    // | Field.ClauseField rs -> rs
                    // | Field.LeftClauseField (_, ClauseField rs) -> rs
                    // | Field.LeftScopeField rs -> rs
                    | _ -> []
                let subtypedrules =
                    rules |> List.collect (fun (r,o) -> r |> (function |SubtypeRule (_, _, cfs) -> cfs | x -> [(r, o)]))
                // let subtypedrules =
                //     rules |> List.collect (fun (s,o,r) -> r |> (function |SubtypeField (_, _, ClauseField cfs) -> cfs | x -> [(s, o, x)]))
                let expandedrules =
                    subtypedrules |> List.collect (
                        function
                        | (LeafRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                        | (NodeRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                        |x -> [x])
                // let expandedrules =
                //     subtypedrules |> List.collect (
                //         function
                //         | _,_,(AliasField a) -> (aliases.TryFind a |> Option.defaultValue [])
                //         |x -> [x])
                let innerN (c : Node) =
                    // expandedrules |> Seq.choose (function |(NodeRule (l, rs), o) when checkLeftField varMap enumsMap typesMap effectMap triggerMap localisation files changeScope anyScope defaultLang ctx l node.Key node -> Some (l, rs, o) |_ -> None)
                    //       |> (fun rs -> lazyErrorMerge rs (fun (l, r, o) -> applyNodeRule enforceCardinality ctx o l r node) createDefault false)
                    expandedrules |> Seq.tryFind (function |(NodeRule (l, rs), o) -> checkLeftField varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang ctx l c.Key c |_ -> false)
                                  |> Option.bind (function |(NodeRule (l, rs), o) -> Some (NodeC c, ((NodeRule (l, rs)), o)) |_ -> Some (NodeC c, (field, options)))//|> Seq.tryHead |> Option.map (fun (l, rs, o) -> Some (NodeC c, ((NodeRule (l, rs)), o))) |> Option.defaultValue (Some (NodeC c, (field, options)))
                    // | [] ->
                    //     // let leftClauseRule =
                    //     //     expandedrules |>
                    //     //     List.filter (function |(_, _, LeftClauseField (vt, _)) -> checkValidLeftClauseRule files enumsMap (LeftClauseField (vt, ClauseField [])) c.Key  |_ -> false )
                    //     // let leftTypeRule =
                    //     //     expandedrules |>
                    //     //     List.filter (function |(_, _, LeftTypeField (t, r)) -> checkValidLeftTypeRule typesMap (LeftTypeField (t, r)) c.Key  |_ -> false )
                    //     // let leftScopeRule =
                    //     //     expandedrules |>
                    //     //     List.filter (function |(_, _, LeftScopeField (rs)) -> checkValidLeftScopeRule scopes (LeftScopeField (rs)) c.Key  |_ -> false )
                    //     // let leftRules = leftClauseRule @ leftScopeRule @ leftTypeRule
                    //     // match leftRules with
                    //     Some (NodeC c, (field, options))
                    //     // |r::_ -> Some( NodeC c, r)
                    // | (l, rs, o)::_ -> Some (NodeC c, ((NodeRule (l, rs)), o))
                let innerL (leaf : Leaf) =
                    expandedrules |> Seq.tryFind (function |(LeafRule (l, r), o) -> checkLeftField varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang ctx l leaf.Key leaf |_ -> false)
                                  |> Option.bind (function |(LeafRule (l, r), o) -> Some (LeafC leaf, ((LeafRule (l, r)), o)) |_ -> None)
                    // |[] ->
                        // let leftTypeRule =
                        //     expandedrules |>
                        //     List.tryFind (function |(_, _, LeftTypeField (t, r)) -> checkValidLeftTypeRule typesMap (LeftTypeField (t, r)) l.Key  |_ -> false )
                        // let leftClauseRule =
                        //     expandedrules |>
                        //     List.tryFind (function |(_, _, LeftClauseField (vt, _)) -> checkValidLeftClauseRule files enumsMap (LeftClauseField (vt, ClauseField [])) l.Key  |_ -> false )
                        // match Option.orElse leftClauseRule leftTypeRule with
                        // |Some r -> Some (LeafC l, r) //#TODO this doesn't correct handle lefttype
                    //     Some (LeafC leaf, (field, options))
                    // |(l, rs, o)::_ -> Some (LeafC leaf, ((LeafRule (l, rs)), o))
                let innerLV (leafvalue : LeafValue) = //Some (LeafValueC lv, (field, options))
                    expandedrules |> Seq.tryFind (function |(LeafValueRule (lv), o) -> checkLeftField varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang ctx lv leafvalue.Key leafvalue |_ -> false)
                                  |> Option.bind (function |(LeafValueRule (lv), o) -> Some (LeafValueC leafvalue, ((LeafValueRule (lv)), o)) |_ -> None)

                //seq { yield! node.Children |> List.choose innerN; yield! node.Leaves |> List.ofSeq |> List.choose innerL; yield! node.LeafValues |> List.ofSeq |> List.choose innerLV }
                (node.Children |> List.choose innerN) @ (node.Leaves |> List.ofSeq |> List.choose innerL) @ (node.LeafValues |> List.ofSeq |> List.choose innerLV)
            let pathDir = (Path.GetDirectoryName path).Replace("\\","/")
            let file = Path.GetFileName path
            match typedefs |> List.tryFind (fun t -> checkPathDir t pathDir file) with
            |Some typedef when typedef.skipRootKey.IsSome ->
                let typerules = typeRules |> List.filter (fun (name, _) -> name == typedef.name)
                match typedef.skipRootKey.Value, typerules with
                |SkipRootKey.AnyKey, [(n, (NodeRule (l, rs), o))]  ->
                    (node.Children |> List.collect (fun n -> n.Children) |> List.fold (fun a c -> foldRules fNode fChild fLeaf fLeafValue fComment a (NodeC c) (NodeRule (ValueField (ValueType.Specific (c.Key)), rs), o)) acc)
                |SkipRootKey.SpecificKey srk, [(n, (NodeRule (l, rs), o))]  ->
                    (node.Children |> List.collect (fun n -> if n.Key == srk then n.Children else []) |> List.fold (fun a c -> foldRules fNode fChild fLeaf fLeafValue fComment a (NodeC c) (NodeRule (ValueField (ValueType.Specific (c.Key)), rs), o)) acc)
                |_ -> acc
            |Some typedef ->
                let typerules = typeRules |> List.filter (fun (name, _) -> name == typedef.name)
                match typerules with
                |[(n, (NodeRule (l, rs), o))] ->
                    (node.Children |> List.fold (fun a c -> foldRules fNode fChild fLeaf fLeafValue fComment a (NodeC c) (NodeRule (ValueField (ValueType.Specific (c.Key)), rs), o)) acc)
                |_ -> acc
            |_ -> acc

        let getTypesInEntity (entity : Entity) =
            let fLeaf (res : Collections.Map<string, (string * range) list>) (leaf : Leaf) ((field, _) : NewRule<_>) =
                match field with
                |LeafRule (_, TypeField (TypeType.Simple t)) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    res |> (fun m -> m.Add(typename, (leaf.Value.ToRawString(), leaf.Position)::(m.TryFind(typename) |> Option.defaultValue [])))
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
                    res |> (fun m -> m.Add(t, (leafvalue.Value.ToRawString(), leafvalue.Position)::(m.TryFind(typename) |> Option.defaultValue [])))
                |_ -> res

            let fComment (res) _ _ = res
            let fNode (res) (node : Node) ((field, option) : NewRule<_>) = res
            let fCombine a b = (a |> List.choose id) @ (b |> List.choose id)

            let ctx = typedefs |> List.fold (fun (a : Collections.Map<string, (string * range) list>) t -> a.Add(t.name, [])) Collections.Map.empty
            let res = foldCollect fLeaf fLeafValue fComment fNode ctx (entity.entity) (entity.logicalpath)
            res
        let getDefVarInEntity (ctx : Collections.Map<string, (string * range) list>) (entity : Entity) =
            let getVariableFromString (v : string) (s : string) = if v = "variable" then s.Split('@').[0].Split('.') |> Array.last else s.Split('@').[0]
            let fLeaf (res : Collections.Map<string, (string * range) list>) (leaf : Leaf) ((field, _) : NewRule<_>) =
                match field with
                |LeafRule (_, VariableSetField v) ->
                // |Field.TypeField t ->
                    res |> (fun m -> m.Add(v, (getVariableFromString v (leaf.Value.ToRawString()), leaf.Position)::(m.TryFind(v) |> Option.defaultValue [])) )
                |LeafRule (VariableSetField v, _) ->
                // |Field.TypeField t ->
                    res |> (fun m -> m.Add(v, (getVariableFromString v leaf.Key, leaf.Position)::(m.TryFind(v) |> Option.defaultValue [])) )
                |_ -> res
            let fLeafValue (res : Collections.Map<string, (string * range) list>) (leafvalue : LeafValue) (field, _) =
                match field with
                |LeafValueRule (VariableSetField v) ->
                // |Field.TypeField t ->
                    res |> (fun m -> m.Add(v, (getVariableFromString v (leafvalue.Value.ToRawString()), leafvalue.Position)::(m.TryFind(v) |> Option.defaultValue [])) )
                |_ -> res
            let fNode (res : Collections.Map<string, (string * range) list>) (node : Node) ((field, option) : NewRule<_>) =
                match field with
                |NodeRule (VariableSetField v, _) ->
                    res |> (fun m -> m.Add(v, (getVariableFromString v node.Key, node.Position)::(m.TryFind(v) |> Option.defaultValue [])) )
                |_ -> res
            let fComment (res) _ _ = res

            let res = foldCollect fLeaf fLeafValue fComment fNode ctx (entity.entity) (entity.logicalpath)
            res

        let validateLocalisationFromTypes (entity : Entity) =
            let fLeaf (res : ValidationResult) (leaf : Leaf) ((field, _) : NewRule<_>) =
                match field with
                |LeafRule (_, TypeField (TypeType.Simple t)) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    let value = leaf.Value.ToRawString()
                    let sets =
                        typesMap
                        |> Map.filter (fun key values -> key.StartsWith(t, StringComparison.OrdinalIgnoreCase) && values.Contains(value))
                        |> Map.toSeq |> Seq.map fst
                    sets <&!&> (fun s -> validateTypeLocalisation typedefs localisation s value leaf) <&&> res
                |LeafRule (TypeField (TypeType.Simple t), _) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    let value = leaf.Key
                    let sets =
                        typesMap
                        |> Map.filter (fun key values -> key.StartsWith(t, StringComparison.OrdinalIgnoreCase) && values.Contains(value))
                        |> Map.toSeq |> Seq.map fst
                    sets <&!&> (fun s -> validateTypeLocalisation typedefs localisation s value leaf) <&&> res
                |_ -> res
            let fLeafValue (res : ValidationResult) (leafvalue : LeafValue) (field, _) =
                match field with
                |LeafValueRule (TypeField (TypeType.Simple t)) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    let value = leafvalue.Value.ToRawString()
                    let sets =
                        typesMap
                        |> Map.filter (fun key values -> key.StartsWith(t, StringComparison.OrdinalIgnoreCase) && values.Contains(value))
                        |> Map.toSeq |> Seq.map fst
                    sets <&!&> (fun s -> validateTypeLocalisation typedefs localisation s value leafvalue) <&&> res
                |_ -> res
            let fNode (res : ValidationResult) (node : Node) (field, _) =
                match field with
                |NodeRule (TypeField (TypeType.Simple t), _) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    let value = node.Key
                    let sets =
                        typesMap
                        |> Map.filter (fun key values -> key.StartsWith(t, StringComparison.OrdinalIgnoreCase) && values.Contains(value))
                        |> Map.toSeq |> Seq.map fst
                    sets <&!&> (fun s -> validateTypeLocalisation typedefs localisation s value node) <&&> res
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
        member __.GetReferencedTypes(entity : Entity) = getTypesInEntity entity
        member __.GetDefinedVariables(entity : Entity) = getDefVarInEntity (Map.empty) entity
        member __.GetTypeLocalisationErrors(entity : Entity) = validateLocalisationFromTypes entity

    // type FoldRules(rootRules : RootRule list, typedefs : TypeDefinition list , types : Collections.Map<string, (string * range) list>, enums : Collections.Map<string, string list>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Effect list, effects : Effect list, ruleApplicator : RuleApplicator) =

    type CompletionService<'T when 'T :> IScope<'T> and 'T : equality and 'T : comparison>
                        (rootRules : RootRule<'T> list, typedefs : TypeDefinition<'T> list , types : Collections.Map<string, StringSet>,
                         enums : Collections.Map<string, StringSet>, varMap : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<'T>,InsensitiveStringComparer>, effects : Map<string,Effect<'T>,InsensitiveStringComparer>, changeScope, defaultContext : ScopeContext<'T>, anyScope, oneToOneScopes, defaultLang)  =
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
        let fieldToCompletionList (field : NewField<_>) =
            match field with
            |ValueField (Enum e) -> enums.TryFind(e) |> Option.bind (fun s -> if s.IsEmpty then None else Some (s.MaximumElement)) |> Option.defaultValue "x"
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

        let createSnippetForClause (rules : NewRule<_> list) (description : string option) (key : string) =
            let filterToCompletion =
                function
                |LeafRule(ValueField(ValueType.Specific _), _) -> true
                |NodeRule(ValueField(ValueType.Specific _), _) -> true
                |_ -> false
            let ruleToDistinctKey =
                function
                |LeafRule(ValueField(ValueType.Specific s), _) -> s
                |NodeRule(ValueField(ValueType.Specific s), _) -> s
                |_ -> ""

            let rulePrint (i : int) =
                function
                |LeafRule(ValueField(ValueType.Specific s), r) ->
                    sprintf "\t%s = ${%i:%s}\n" s (i + 1) (fieldToCompletionList r)
                |NodeRule(ValueField(ValueType.Specific s), _) ->
                    sprintf "\t%s = ${%i:%s}\n" s (i + 1) "{ }"
                |_ -> ""

            let requiredRules = rules |> List.filter (fun (f, o) -> o.min >= 1 && filterToCompletion f)
                                      |> List.distinctBy (fun (f, _) -> ruleToDistinctKey f)
                                      |> List.mapi (fun i (f, _) -> rulePrint i f)
                                      |> String.concat ""
            Snippet (key, (sprintf "%s = {\n%s\t$0\n}" key requiredRules), description)


        let rec getRulePath (pos : pos) (stack : (string * bool) list) (node : Node) =
           //eprintfn "grp %A %A %A" pos stack (node.Children |> List.map (fun f -> f.ToRaw))
           match node.Children |> List.tryFind (fun c -> rangeContainsPos c.Position pos) with
           | Some c -> getRulePath pos ((c.Key, false) :: stack) c
           | None ->
                match node.Leaves |> Seq.tryFind (fun l -> rangeContainsPos l.Position pos) with
                | Some l -> (l.Key, true)::stack
                | None -> stack

        and getCompletionFromPath (rules : NewRule<_> list) (stack : (string * bool) list) =
            //eprintfn "%A" stack
            let rec convRuleToCompletion (rule : NewRule<_>) =
                let r, o = rule
                let keyvalue (inner : string) = Snippet (inner, (sprintf "%s = $0" inner), o.description)
                match r with
                |NodeRule (ValueField(ValueType.Specific s), innerRules) ->
                    [createSnippetForClause innerRules o.description s]
                |NodeRule (ValueField(ValueType.Enum e), innerRules) ->
                    enums.TryFind(e) |> Option.map (fun es -> es.ToList() |> List.map (fun e -> createSnippetForClause innerRules o.description e)) |> Option.defaultValue []
                |NodeRule (ValueField(_), _) -> []
                |NodeRule (AliasField(_), _) -> []
                |NodeRule (FilepathField(_), _) -> []
                |NodeRule (IconField(folder), innerRules) ->
                    checkIconField folder |> List.map (fun e -> createSnippetForClause innerRules o.description e)
                |NodeRule (LocalisationField(_), _) -> []
                |NodeRule (ScopeField(_), innerRules) -> oneToOneScopes |> List.map (fun e -> createSnippetForClause innerRules o.description e)
                //TODO: Scopes better
                |NodeRule (SubtypeField(_), _) -> []
                |NodeRule (TypeField(TypeType.Simple t), innerRules) ->
                    types.TryFind(t) |> Option.map (fun ts -> ts |> List.map (fun e -> createSnippetForClause innerRules o.description e)) |> Option.defaultValue []
                |NodeRule (TypeField(TypeType.Complex (p,t,s)), innerRules) ->
                    types.TryFind(t) |> Option.map (fun ts -> ts |> List.map (fun e -> createSnippetForClause innerRules o.description (p+e+s))) |> Option.defaultValue []
                |NodeRule (VariableGetField v, innerRules) ->
                    varMap.TryFind(v) |> Option.map (fun ss -> ss.ToList() |> List.map (fun e -> createSnippetForClause innerRules o.description e)) |> Option.defaultValue []

                |LeafRule (ValueField(ValueType.Specific s), _) ->
                    [keyvalue s]
                |LeafRule (ValueField(ValueType.Enum e), _) ->
                    enums.TryFind(e) |> Option.map (fun es -> es.ToList() |> List.map (fun e -> keyvalue e)) |> Option.defaultValue []
                |LeafRule (ValueField(_), _) -> []
                |LeafRule (AliasField(_), _) -> []
                |LeafRule (FilepathField(_), _) -> []
                |LeafRule (IconField(folder), _) -> checkIconField folder |> List.map keyvalue
                |LeafRule (LocalisationField(_), _) -> []
                |LeafRule (ScopeField(_), _) -> [] //TODO: Scopes
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
                    |NewField.TypeField (TypeType.Simple t) -> types.TryFind(t) |> Option.defaultValue [] |> List.map Simple
                    |NewField.TypeField (TypeType.Complex (p,t,s)) -> types.TryFind(t) |> Option.map (fun ns -> List.map (fun n ->  p + n + s) ns) |> Option.defaultValue [] |> List.map Simple
                    |NewField.ValueField (Enum e) -> enums.TryFind(e) |> Option.map (fun s -> s.ToList()) |> Option.defaultValue [] |> List.map Simple
                    |NewField.VariableGetField v -> varMap.TryFind(v) |> Option.map (fun s -> s.ToList()) |> Option.defaultValue [] |> List.map Simple
                    |NewField.VariableSetField v -> varMap.TryFind(v) |> Option.map (fun s -> s.ToList()) |> Option.defaultValue [] |> List.map Simple
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
                //eprintfn "%A" types
                //eprintfn "%A" field
                match field with
                |NewField.ValueField (Enum e) -> enums.TryFind(e) |> Option.map (fun s -> s.ToList()) |> Option.defaultValue [] |> List.map Simple
                |NewField.ValueField v -> getValidValues v |> Option.defaultValue [] |> List.map Simple
                |NewField.TypeField (TypeType.Simple t) -> types.TryFind(t) |> Option.defaultValue [] |> List.map Simple
                |NewField.TypeField (TypeType.Complex (p,t,s)) -> types.TryFind(t) |>  Option.map (fun ns -> List.map (fun n ->  p + n + s) ns) |> Option.defaultValue [] |> List.map Simple
                |NewField.LocalisationField s ->
                    match s with
                    |true -> localisation |> List.tryFind (fun (lang, _ ) -> lang = (STL STLLang.Default)) |> Option.map (snd >> Set.toList) |> Option.defaultValue [] |> List.map Simple
                    |false -> localisation |> List.tryFind (fun (lang, _ ) -> lang <> (STL STLLang.Default)) |> Option.map (snd >> Set.toList) |> Option.defaultValue [] |> List.map Simple
                |NewField.FilepathField -> files |> Set.toList |> List.map Simple
                |NewField.ScopeField _ -> oneToOneScopes |> List.map (Simple)
                |NewField.VariableGetField v -> varMap.TryFind v |> Option.map (fun ss -> ss.ToList()) |> Option.defaultValue [] |> List.map Simple
                |NewField.VariableSetField v -> varMap.TryFind v |> Option.map (fun ss -> ss.ToList()) |> Option.defaultValue [] |> List.map Simple
                |NewField.VariableField _ -> varMap.TryFind "variable" |> Option.map (fun ss -> ss.ToList()) |> Option.defaultValue [] |> List.map Simple
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
            let rec findRule (rules : NewRule<'T> list) (stack) =
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
                |[] -> expandedRules |> List.collect convRuleToCompletion
                |(key, false)::rest ->
                    match expandedRules |> List.choose (function |(NodeRule (l, rs), o) when checkFieldByKey varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang { subtypes = []; scopes = defaultContext; warningOnly = false } l key -> Some (l, rs, o) |_ -> None) with
                    |[] -> expandedRules |> List.collect convRuleToCompletion
                    |fs -> fs |> List.collect (fun (_, innerRules, _) -> findRule innerRules rest)
                |(key, true)::rest ->
                    match expandedRules |> List.choose (function |(LeafRule (l, r), o) when checkFieldByKey varMap enumsMap typesMap effectMap triggerMap varSet localisation files changeScope anyScope defaultLang { subtypes = []; scopes = defaultContext; warningOnly = false } l key -> Some (l, r, o) |_ -> None) with
                    |[] -> expandedRules |> List.collect convRuleToCompletion
                    |fs ->
                        //eprintfn "%s %A" key fs
                        let res = fs |> List.collect (fun (_, f, _) -> fieldToRules f)
                        //eprintfn "res %A" res
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
            let res = findRule rules stack |> List.distinct
            //eprintfn "res2 %A" res
            res

        let complete (pos : pos) (entity : Entity) =
            let path = getRulePath pos [] entity.entity |> List.rev
            let pathDir = (Path.GetDirectoryName entity.logicalpath).Replace("\\","/")
            let file = Path.GetFileName entity.logicalpath
            // eprintfn "%A" typedefs
            // eprintfn "%A" pos
            // eprintfn "%A" entity.logicalpath
            // eprintfn "%A" pathDir
            let typekeyfilter (td : TypeDefinition<_>) (n : string) =
                match td.typeKeyFilter with
                |Some (filter, negate) -> n == filter <> negate
                |None -> true
            let skiprootkey (td : TypeDefinition<_>) (n : string) =
                match td.skipRootKey with
                |Some (SpecificKey key) -> n == key
                |Some (AnyKey) -> true
                |None -> false
            let skipcomp =
                match typedefs |> List.filter (fun t -> checkPathDir t pathDir file && skiprootkey t (if path.Length > 0 then path.Head |> fst else "")) with
                |[] -> None
                |xs ->
                    match xs |> List.tryFind (fun t -> checkPathDir t pathDir file && typekeyfilter t (if path.Length > 1 then path.Tail |> List.head |> fst else "")) with
                    |Some typedef ->
                        let typerules = typeRules |> List.choose (function |(name, typerule) when name == typedef.name -> Some typerule |_ -> None)
                        //eprintfn "sc %A" path
                        let fixedpath = if List.isEmpty path then path else (typedef.name, false)::(path |> List.tail |> List.tail)
                        let completion = getCompletionFromPath typerules fixedpath
                        Some completion
                    |None -> None
            let res =
                skipcomp |> Option.defaultWith
                    (fun () ->
                    match typedefs |> List.tryFind (fun t -> checkPathDir t pathDir file && typekeyfilter t (if path.Length > 0 then path.Head |> fst else "")) with
                    |Some typedef ->
                        let path2 = if typedef.type_per_file then path else if path.Length > 0 then path |> List.tail else path
                        let typerules = typeRules |> List.choose (function |(name, typerule) when name == typedef.name -> Some typerule |_ -> None)
                        //eprintfn "fc %A" path
                        let fixedpath = if List.isEmpty path then path else (typedef.name, false)::(path2)
                        let completion = getCompletionFromPath typerules fixedpath
                        completion
                    |None -> getCompletionFromPath (typeRules |> List.map snd) path)
            //eprintfn "res3 %A" res
            res
        //(fun (pos, entity) -> complete pos entity)
        member __.Complete(pos : pos, entity : Entity) = complete pos entity

    let getTypesFromDefinitions (ruleapplicator : RuleApplicator<_>) (types : TypeDefinition<_> list) (es : Entity list) =
        let entities = es |> List.map (fun e -> ((Path.GetDirectoryName e.logicalpath).Replace("\\","/")), e, (Path.GetFileName e.logicalpath))
        let getTypeInfo (def : TypeDefinition<_>) =
            entities |> List.choose (fun (path, e, file) -> if checkPathDir def path file then Some (e.entity, file) else None)
                     |> List.collect (fun (e, f) ->
                            let inner (n : Node) =
                                let subtypes = ruleapplicator.TestSubType(def.subtypes, n) |> snd |> List.map (fun s -> def.name + "." + s)
                                let key =
                                    match def.nameField with
                                    |Some f -> n.TagText f
                                    |None -> n.Key
                                let result = def.name::subtypes |> List.map (fun s -> s, (key, n.Position))
                                match def.typeKeyFilter with
                                |Some (filter, negate) -> if n.Key == filter <> negate then result else []
                                |None -> result
                            let childres =
                                match def.type_per_file, def.skipRootKey with
                                |true, _ ->
                                    let subtypes = ruleapplicator.TestSubType(def.subtypes, e) |> snd |> List.map (fun s -> def.name + "." + s)
                                    def.name::subtypes |> List.map (fun s -> s, (Path.GetFileNameWithoutExtension f, e.Position))
                                |false, Some (SpecificKey key) ->
                                    e.Children |> List.filter (fun c -> c.Key == key) |> List.collect (fun c -> c.Children |> List.collect inner)
                                |false, Some (AnyKey) ->
                                    let res = e.Children |> List.collect (fun c -> c.Children |> List.collect inner)
                                    res
                                |false, None ->
                                    (e.Children |> List.collect inner)
                            childres
                            @
                            (e.LeafValues |> List.ofSeq |> List.map (fun lv -> def.name, (lv.Value.ToString(), lv.Position))))
        let results = types |> Seq.ofList |> PSeq.collect getTypeInfo |> List.ofSeq |> List.fold (fun m (n, k) -> if Map.containsKey n m then Map.add n (k::m.[n]) m else Map.add n [k] m) Map.empty
        types |> List.map (fun t -> t.name) |> List.fold (fun m k -> if Map.containsKey k m then m else Map.add k [] m ) results

    let getEnumsFromComplexEnums (complexenums : (ComplexEnumDef) list) (es : Entity list) =
        let entities = es |> List.map (fun e -> e.logicalpath.Replace("\\","/"), e)
        let rec inner (enumtree : Node) (node : Node) =
            // eprintfn "%A %A" (enumtree.ToRaw) (node.Position.FileName)
            match enumtree.Children with
            |head::_ ->
                if enumtree.Children |> List.exists (fun n -> n.Key = "enum_name")
                then node.Children |> List.map (fun n -> n.Key.Trim([|'\"'|])) else
                node.Children |> List.collect (inner head)
            |[] ->
                if enumtree.LeafValues |> Seq.exists (fun lv -> lv.Value.ToRawString() = "enum_name")
                then node.LeafValues |> Seq.map (fun lv -> lv.Value.ToRawString().Trim([|'\"'|])) |> List.ofSeq
                else
                    match enumtree.Leaves |> Seq.tryFind (fun l -> l.Value.ToRawString() = "enum_name") with
                    |Some leaf -> node.TagsText (leaf.Key) |> Seq.map (fun k -> k.Trim([|'\"'|])) |> List.ofSeq
                    |None ->
                        match enumtree.Leaves |> Seq.tryFind (fun l -> l.Key == "enum_name") with
                        |Some leaf -> node.Leaves |> Seq.map(fun l -> l.Key.Trim([|'\"'|])) |> List.ofSeq
                        |None -> []
        let getEnumInfo (complexenum : ComplexEnumDef) =
            let cpath = complexenum.path.Replace("\\","/")
            // eprintfn "cpath %A %A" cpath (entities |> List.map (fun (_, e) -> e.logicalpath))
            let values = entities |> List.choose (fun (path, e) -> if path.StartsWith(cpath, StringComparison.OrdinalIgnoreCase) then Some e.entity else None)
                                  |> List.collect (fun e -> if complexenum.start_from_root then inner complexenum.nameTree e else  e.Children |> List.collect (inner complexenum.nameTree))
            // eprintfn "%A %A" complexenum.name values
            complexenum.name, values
        complexenums |> List.toSeq |> PSeq.map getEnumInfo |> List.ofSeq

    let getDefinedVariables (foldRules : FoldRules<_>) (es : Entity list) =
        // let results = es |> List.toSeq |> PSeq.fold (fun c e -> foldRules.GetDefinedVariables(c,e)) (Collections.Map.empty)//|> List.ofSeq |> List.fold (fun m (n, k) -> if Map.containsKey n m then Map.add n (k::m.[n]) m else Map.add n [k] m) Collections.Map.empty
        let results = es |> List.toSeq |> PSeq.map (fun e -> foldRules.GetDefinedVariables(e))
                            |> Seq.fold (fun m map -> Map.toList map |>  List.fold (fun m2 (n,k) -> if Map.containsKey n m2 then Map.add n (k@m2.[n]) m2 else Map.add n k m2) m) Collections.Map.empty
        results