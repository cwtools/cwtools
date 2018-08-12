namespace CWTools.Validation

open CWTools.Parser.ConfigParser
open CWTools.Process
open CWTools.Utilities.Utils
open CWTools.Validation.ValidationCore
open CWTools.Common.STLConstants
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open CWTools.Games
open Stellaris.STLValidation
open FParsec
open CWTools.Parser.Types
open CWTools.Utilities
open CWTools.Process.STLScopes
open CWTools.Common
open CWTools.Validation.Stellaris.STLLocalisationValidation
open CWTools.Validation.Stellaris.ScopeValidation
open Microsoft.FSharp.Collections.Tagged
open System.IO
open FSharp.Data.Runtime
open QuickGraph

module rec Rules =
    type StringSet = Set<string, InsensitiveStringComparer>

    let fst3 (a, _, _) = a
    let snd3 (_, b, _) = b
    let thd3 (_, _, c) = c
    let checkPathDir (t : TypeDefinition) (pathDir : string) =
        match t.path_strict with
        |true -> pathDir == t.path.Replace("\\","/")
        |false -> pathDir.StartsWith(t.path.Replace("\\","/"))

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
    // let checkValidLeftClauseRule (files : Collections.Set<string>) (enums : Collections.Map<string, StringSet>) (field : Field) (key : string) =
    //     let key = key.Trim('"').Replace("/","\\")
    //     match field with
    //     |LeftClauseField (ValueType.Int (min, max), _) ->
    //         match TryParser.parseInt key with
    //         |Some i -> min <= i && i <= max
    //         |None -> false
    //     |LeftClauseField (ValueType.Float (min, max), _) ->
    //         match TryParser.parseDouble key with
    //         |Some i -> min <= i && i <= max
    //         |None -> false
    //     |LeftClauseField (ValueType.Enum e, _) ->
    //         match enums.TryFind e with
    //         |Some es -> es.Contains key
    //         |None -> false
    //     |LeftClauseField (ValueType.Scalar, _) -> true
    //     |LeftClauseField (ValueType.Filepath, _) -> files.Contains (key.Trim('"').Replace("\\","/").Replace(".lua",".shader").Replace(".tga",".dds"))
    //     |_ -> false

    // let checkValidLeftTypeRule (types : Collections.Map<string, StringSet>) (field : Field) (key : string) =
    //     match field with
    //     |LeftTypeField (t, _) ->
    //         match types.TryFind t with
    //         |Some keys -> keys.Contains key
    //         |None -> false
    //     |_ -> false

    // let checkValidLeftScopeRule (scopes : string list) (field : Field) (key : string) =
    //     match field with
    //     |LeftScopeField _ ->
    //         match key with
    //         |x when x.StartsWith "event_target:" -> true
    //         |x when x.StartsWith "parameter:" -> true
    //         |x when x.StartsWith "hidden:" -> true
    //         |x ->
    //             let xs = x.Split '.'
    //             xs |> Array.forall (fun s -> scopes |> List.exists (fun s2 -> s == s2))
    //     |_ -> false

    let scopes = (scopedEffects |> List.map (fun se -> se.Name)) @ (oneToOneScopes |> List.map fst)

    type CompletionResponse =
    |Simple of label : string
    |Detailed of label : string * desc : string option
    |Snippet of label : string * snippet : string * desc : string option
    type RuleContext =
        {
            subtypes : string list
            scopes : ScopeContext
            warningOnly : bool
        }

    let inline checkValidValue (enumsMap : Collections.Map<_, Set<_, _>>) (severity : Severity) (vt : ValueType) (key : string) leafornode =
        if key.StartsWith "@" then OK else
            match vt with
            |ValueType.Bool ->
                if key = "yes" || key = "no" then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting yes or no, got %s" key) severity) leafornode]
            |ValueType.Enum e ->
                match enumsMap.TryFind e with
                |Some es -> if es.Contains key then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a \"%s\" value, e.g. %A" e es) severity) leafornode]
                |None -> OK
            |ValueType.Float (min, max) ->
                // match leaf.Value with
                // |Float f -> if f <= max && f >= min then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %f and %f" min max) severity) leaf]
                // |Int f -> if float f <= max && float f >= min then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %f and %f" min max) severity) leaf]
                // |_ ->
                    match TryParser.parseDouble key with
                    |Some f -> if f <= max && f >= min then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %f and %f" min max) severity) leafornode]
                    |None -> Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a float, got %s" key) severity) leafornode]
            |ValueType.Int (min, max) ->
                // match leaf.Value with
                // |Int i -> if i <= max && i >= min then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %i and %i" min max) severity) leaf]
                // |_ ->
                    match TryParser.parseInt key with
                    |Some i ->  if i <= max && i >= min then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %i and %i" min max) severity) leafornode]
                    |None -> Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting an integer, got %s" key) severity) leafornode]
            |ValueType.Specific s -> if key.Trim([|'\"'|]) == s then OK else Invalid [inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting value %s" s) severity) leafornode]
            |ValueType.Scalar -> OK
            |ValueType.Percent -> if key.EndsWith("%") then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting an percentage, got %s" key) severity) leafornode]
            |_ -> Invalid [inv (ErrorCodes.ConfigRulesUnexpectedValue "Invalid value" severity) leafornode]

    let inline checkLocalisationField (keys : (Lang * Collections.Set<string>) list) (synced : bool) (key : string) (leafornode : ^a) =
        match synced with
        |true ->
            let defaultKeys = keys |> List.choose (fun (l, ks) -> if l = STL STLLang.Default then Some ks else None) |> List.tryHead |> Option.defaultValue Set.empty
            //let key = leaf.Value |> (function |QString s -> s |s -> s.ToString())
            checkLocName leafornode defaultKeys (STL STLLang.Default) key
        |false ->
            checkLocKeysLeafOrNode keys key leafornode

    let inline checkTypeField (typesMap : Collections.Map<_,Set<_, _>>) severity t (key : string) leafornode =
        match typesMap.TryFind t with
        |Some values ->
            let value = key.Trim([|'\"'|])
            if value.StartsWith("@",System.StringComparison.Ordinal) then OK else
            if values.Contains value then OK else Invalid [inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected value of type %s" t) severity) leafornode]
        |None -> Invalid [inv (ErrorCodes.CustomError (sprintf "Unknown type referenced %s" t) Severity.Error) leafornode]

    let inline checkScopeField (effectMap : Map<_,_,_>) (triggerMap : Map<_,_,_>) (ctx : RuleContext) (s : Scope) key leafornode =
        let scope = ctx.scopes
        match changeScope effectMap triggerMap key scope with
        |NewScope ({Scopes = current::_} ,_) -> if current = s || s = Scope.Any || current = Scope.Any then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        |NotFound _ -> Invalid [inv (ErrorCodes.ConfigRulesInvalidTarget (s.ToString())) leafornode]
        |WrongScope (command, prevscope, expected) -> Invalid [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) leafornode]
        |_ -> OK

    let inline checkFilepathField (files : Collections.Set<string>) (key : string) (leafornode) =
        let file = key.Trim('"').Replace("\\","/").Replace(".lua",".shader").Replace(".tga",".dds")
        if files.Contains file then OK else Invalid [inv (ErrorCodes.MissingFile file) leafornode]

    let inline checkIconField (files :Collections.Set<string>) (folder : string) (key : string) (leafornode) =
        let value = folder + "/" + key + ".dds"
        if files.Contains value then OK else Invalid [inv (ErrorCodes.MissingFile value) leafornode]

    let inline checkField enumsMap typesMap effectMap triggerMap localisation files (severity : Severity) (ctx : RuleContext) (field : NewField) (key : string) (leafornode : ^a) =
        match field with
        |ValueField vt -> checkValidValue enumsMap severity vt key leafornode
        |TypeField t -> checkTypeField typesMap severity t key leafornode
        |ScopeField s -> checkScopeField effectMap triggerMap ctx s key leafornode
        |LocalisationField synced -> checkLocalisationField localisation synced key leafornode
        |FilepathField -> checkFilepathField files key leafornode
        |IconField folder -> checkIconField files folder key leafornode
        |_ -> OK

    let inline checkLeftField enumsMap typesMap effectMap triggerMap localisation files (ctx : RuleContext) (field : NewField) (key : string) (leafornode : ^a) =
        match checkField enumsMap typesMap effectMap triggerMap localisation files (Severity.Error) ctx field key leafornode with
        |OK -> true
        |_ -> false

    type RuleApplicator(rootRules : RootRule list, typedefs : TypeDefinition list , types : Collections.Map<string, (string * range) list>, enums : Collections.Map<string, string list>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Effect list, effects : Effect list) =
        let triggerMap = triggers |> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))
        let effectMap = effects |> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))

        let aliases =
            rootRules |> List.choose (function |AliasRule (a, rs) -> Some (a, rs) |_ -> None)
                        |> List.groupBy fst
                        |> List.map (fun (k, vs) -> k, vs |> List.map snd)
                        |> Collections.Map.ofList
        let typeRules =
            rootRules |> List.choose (function |TypeRule (k, rs) -> Some (k, rs) |_ -> None)
        let typesMap = types |> (Map.map (fun _ s -> StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))))
        let enumsMap = enums |> (Map.map (fun _ s -> StringSet.Create(InsensitiveStringComparer(), s)))


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


        let rec applyClauseField (enforceCardinality : bool) (ctx : RuleContext) (rules : NewRule list) (startNode : Node) =
            if startNode.Key == "every_tile" then eprintfn "%s %A %A %A %b" (startNode.Key) rules ctx (startNode.Key) enforceCardinality else ()
            if startNode.Key == "every_tile" then eprintfn "%A" aliases else ()
            let severity = if ctx.warningOnly then Severity.Warning else Severity.Error
            let subtypedrules =
                rules |> List.collect (fun (r,o) -> r |> (function |SubtypeRule (key, shouldMatch, cfs) -> (if (not shouldMatch) <> List.contains key ctx.subtypes then cfs else []) | x -> [(r, o)]))
            let expandedrules =
                subtypedrules |> List.collect (
                    function
                    | (LeafRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                    | (NodeRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue [])
                    |x -> [x])
            let valueFun (leaf : Leaf) =
                match expandedrules |> List.choose (function |(LeafRule (l, r), o) when checkLeftField enumsMap typesMap effectMap triggerMap localisation files ctx l leaf.Key leaf -> Some (l, r, o) |_ -> None) with
                |[] ->
                    if enforceCardinality && (leaf.Key.StartsWith("@", System.StringComparison.Ordinal) |> not) then Invalid [inv (ErrorCodes.ConfigRulesUnexpectedProperty (sprintf "Unexpected node %s in %s" leaf.Key startNode.Key) severity) leaf] else OK
                |rs -> rs <&??&> (fun (l, r, o) -> applyLeafRule ctx r leaf) |> mergeValidationErrors "CW240"
            let nodeFun (node : Node) =
                match expandedrules |> List.choose (function |(NodeRule (l, rs), o) when checkLeftField enumsMap typesMap effectMap triggerMap localisation files ctx l node.Key node -> Some (l, rs, o) |_ -> None) with
                | [] ->
                    if enforceCardinality then Invalid [inv (ErrorCodes.ConfigRulesUnexpectedProperty (sprintf "Unexpected node %s in %s" node.Key startNode.Key) severity) node] else OK
                    //|rs -> rs <&??&> (fun (_, o, f) -> applyNodeRule root enforceCardinality ctx o f node)
                | rs -> rs <&??&> (fun (l, rs, o) -> applyNodeRule enforceCardinality ctx o rs node)
            let leafValueFun (leafvalue : LeafValue) =
                match expandedrules |> List.choose (function |(LeafValueRule (l), o) when checkLeftField enumsMap typesMap effectMap triggerMap localisation files ctx l leafvalue.Key leafvalue -> Some (l, o) |_ -> None) with
                | [] ->
                    if enforceCardinality then Invalid [inv (ErrorCodes.ConfigRulesUnexpectedProperty (sprintf "Unexpected node %s in %s" leafvalue.Key startNode.Key) severity) leafvalue] else OK
                |rs -> rs <&??&> (fun (l, o) -> applyLeafValueRule ctx l leafvalue) |> mergeValidationErrors "CW240"
            let checkCardinality (node : Node) (rule : NewRule) =
                match rule with
                |NodeRule(ValueField (ValueType.Specific key), _), opts
                |LeafRule(ValueField (ValueType.Specific key), _), opts ->
                    let leafcount = node.Values |> List.filter (fun leaf -> leaf.Key == key) |> List.length
                    let childcount = node.Children |> List.filter (fun child -> child.Key == key) |> List.length
                    let total = leafcount + childcount
                    if opts.min > total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %s, expecting at least %i" key opts.min) severity) node]
                    else if opts.max < total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many %s, expecting at most %i" key opts.max) Severity.Warning) node]
                    else OK
                |NodeRule(AliasField(_), _), _
                |LeafRule(AliasField(_), _), _
                |LeafValueRule(AliasField(_)), _ -> OK
                |NodeRule(l, _), opts ->
                    let total = node.Children |> List.filter (fun child -> checkLeftField enumsMap typesMap effectMap triggerMap localisation files ctx l child.Key child) |> List.length
                    if opts.min > total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %A, expecting at least %i" l opts.min) severity) node]
                    else if opts.max < total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many n %A, expecting at most %i" l opts.max) Severity.Warning) node]
                    else OK
                |LeafRule(l, r), opts ->
                    let total = node.Values |> List.filter (fun leaf -> checkLeftField enumsMap typesMap effectMap triggerMap localisation files ctx l leaf.Key leaf) |> List.length
                    if opts.min > total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %A, expecting at least %i" l opts.min) severity) node]
                    else if opts.max < total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many l %A %A, expecting at most %i" l r opts.max) Severity.Warning) node]
                    else OK
                |LeafValueRule(l), opts ->
                    let total = node.LeafValues |> List.ofSeq |> List.filter (fun leafvalue -> checkLeftField enumsMap typesMap effectMap triggerMap localisation files ctx l leafvalue.Key leafvalue) |> List.length
                    if opts.min > total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %A, expecting at least %i" l opts.min) severity) node]
                    else if opts.max < total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many lv %A, expecting at most %i" l opts.max) Severity.Warning) node]
                    else OK
                |_ -> OK
                // let r, opts = rule
                // match r with
                // | LeafValueRule _ -> OK
                // | LeafRule (l, _) ->
                //     let leafcount = node.Values |> List.filter (fun leaf -> checkLeftField ctx l leaf.Key leaf) |> List.length
                // | _ ->
                //     let leafcount = node.Tags key |> Seq.length
                //     let childcount = node.Childs key |> Seq.length
                //     let total = leafcount + childcount
                //     if opts.min > total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %s, expecting at least %i" key opts.min) severity) node]
                //     else if opts.max < total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many %s, expecting at most %i" key opts.max) Severity.Warning) node]
                //     else OK
            startNode.Leaves <&!&> valueFun
            <&&>
            (startNode.Children <&!&> nodeFun)
            <&&>
            (startNode.LeafValues <&!&> leafValueFun)
            <&&>
            (rules <&!&> checkCardinality startNode)

        and applyValueField severity (vt : ValueType) (leaf : Leaf) =
            checkValidValue enumsMap severity vt (leaf.Value.ToRawString()) leaf

        and applyLeafValueRule (ctx : RuleContext) (rule : NewField) (leafvalue : LeafValue) =
            let severity = if ctx.warningOnly then Severity.Warning else Severity.Error
            checkField enumsMap typesMap effectMap triggerMap localisation files severity ctx rule (leafvalue.Value.ToRawString()) leafvalue

        and applyLeafRule (ctx : RuleContext) (rule : NewField) (leaf : Leaf) =
            let severity = if ctx.warningOnly then Severity.Warning else Severity.Error
            checkField enumsMap typesMap effectMap triggerMap localisation files severity ctx rule (leaf.Value.ToRawString()) leaf
            // match rule with
            // | Field.ValueField v -> applyValueField severity v leaf
            // | Field.ObjectField et -> applyObjectField et leaf
            // | Field.TypeField t -> applyTypeField severity t leaf
            // | Field.LeftTypeField (t, f) -> applyLeftTypeFieldLeaf severity t leaf <&&> applyLeafRule root ctx f leaf
            // | Field.ClauseField rs -> OK
            // | Field.LeftClauseField (_, field) -> applyLeafRule root ctx field leaf
            // | Field.LeftScopeField _ -> OK
            // | Field.ScopeField s -> applyScopeField root ctx s leaf
            // | Field.LocalisationField synced -> checkLocalisationField localisation synced leaf
            // | Field.FilepathField -> checkFileExists files leaf
            // | Field.AliasField _ -> OK
            // | Field.SubtypeField _ -> OK

        and applyNodeRule (enforceCardinality : bool) (ctx : RuleContext) (options : Options) (rules : NewRule list) (node : Node) =
            let severity = if ctx.warningOnly then Severity.Warning else Severity.Error
            let newCtx =
                // match oneToOneScopes |> List.tryFind (fun (k, _) -> k == node.Key) with
                // |Some (_, f) ->
                //     match f (ctx.scopes, false) with
                //     |(s, true) -> {ctx with scopes = {ctx.scopes with Scopes = s.CurrentScope::ctx.scopes.Scopes}}
                //     |(s, false) -> {ctx with scopes = s}
                // |None ->
                    match options.pushScope with
                    |Some ps ->
                        {ctx with scopes = {ctx.scopes with Scopes = ps::ctx.scopes.Scopes}}
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
                                {ctx with scopes = {ctx.scopes with Root = root}}
                            |None -> newctx
                        |None ->
                            if node.Key.StartsWith("event_target:", System.StringComparison.OrdinalIgnoreCase) || node.Key.StartsWith("parameter:", System.StringComparison.OrdinalIgnoreCase)
                            then {ctx with scopes = {ctx.scopes with Scopes = Scope.Any::ctx.scopes.Scopes}}
                            else ctx
            applyClauseField enforceCardinality newCtx rules node
            // match rule with
            // | Field.ValueField v -> OK
            // | Field.ObjectField et -> OK
            // | Field.TypeField _ -> OK
            // | Field.LeftTypeField (t, f) -> applyLeftTypeFieldNode severity t node <&&> applyNodeRule root enforceCardinality newCtx options f node
            // | Field.ClauseField rs -> applyClauseField root enforceCardinality newCtx rs node
            // | Field.LeftClauseField (_, ClauseField rs) -> applyClauseField root enforceCardinality newCtx rs node
            // | Field.LeftClauseField (_, _) -> OK
            // | Field.LeftScopeField rs ->
            //     applyLeftScopeField root enforceCardinality newCtx rs node
            // | Field.LocalisationField _ -> OK
            // | Field.FilepathField -> OK
            // | Field.ScopeField _ -> OK
            // | Field.AliasField _ -> OK
            // | Field.SubtypeField _ -> OK

        let testSubtype (subtypes : SubTypeDefinition list) (node : Node) =
            let results =
                subtypes |> List.filter (fun st -> st.typeKeyField |> function |Some tkf -> tkf == node.Key |None -> true)
                        |> List.map (fun s -> s.name, s.pushScope, applyClauseField false {subtypes = []; scopes = defaultContext; warningOnly = false } (s.rules) node)
            let res = results |> List.choose (fun (s, ps, res) -> res |> function |Invalid _ -> None |OK -> Some (ps, s))
            res |> List.tryPick fst, res |> List.map snd

        let applyNodeRuleRoot (typedef : TypeDefinition) (rules : NewRule list) (options : Options) (node : Node) =
            let pushScope, subtypes = testSubtype (typedef.subtypes) node
            let startingScopeContext =
                match Option.orElse pushScope options.pushScope with
                |Some ps -> { Root = ps; From = []; Scopes = [] }
                |None -> defaultContext
            let context = { subtypes = subtypes; scopes = startingScopeContext; warningOnly = typedef.warningOnly }
            applyNodeRule true context options rules node

        let validate ((path, root) : string * Node) =
            let inner (node : Node) =

                let pathDir = (Path.GetDirectoryName path).Replace("\\","/")
                let typekeyfilter (td : TypeDefinition) (n : Node) =
                    match td.typeKeyFilter with
                    |Some (filter, negate) -> n.Key == filter <> negate
                    |None -> true
                let skiprootkey (td : TypeDefinition) (n : Node) =
                    match td.skipRootKey with
                    |Some key -> n.Key == key
                    |None -> false
                let validateType (typedef : TypeDefinition) (n : Node) =
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
                    match typedefs |> List.filter (fun t -> checkPathDir t pathDir && skiprootkey t node) with
                    |[] -> OK
                    |xs ->
                        node.Children <&!&>
                            (fun c ->
                                match xs |> List.tryFind (fun t -> checkPathDir t pathDir && typekeyfilter t c) with
                                |Some typedef -> validateType typedef c
                                |None -> OK
                            )

                let nonskipres =
                    match typedefs |> List.tryFind (fun t -> checkPathDir t pathDir && typekeyfilter t node && t.skipRootKey.IsNone) with
                    |Some typedef -> validateType typedef node
                    |None -> OK
                skipres <&&> nonskipres

            let res = (root.Children <&!&> inner)
            res


        member __.ApplyNodeRule(rule, node) = applyNodeRule true {subtypes = []; scopes = defaultContext; warningOnly = false } defaultOptions rule node
        member __.TestSubtype((subtypes : SubTypeDefinition list), (node : Node)) =
            testSubtype subtypes node
        //member __.ValidateFile(node : Node) = validate node
        member __.RuleValidate : StructureValidator =
            fun _ es -> es.Raw |> List.map (fun struct(e, _) -> e.logicalpath, e.entity) <&!!&> validate


    type FoldRules(rootRules : RootRule list, typedefs : TypeDefinition list , types : Collections.Map<string, (string * range) list>, enums : Collections.Map<string, string list>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Effect list, effects : Effect list, ruleApplicator : RuleApplicator) =
        let triggerMap = triggers |> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))
        let effectMap = effects |> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))

        let aliases =
            rootRules |> List.choose (function |AliasRule (a, rs) -> Some (a, rs) |_ -> None)
                        |> List.groupBy fst
                        |> List.map (fun (k, vs) -> k, vs |> List.map snd)
                        |> Collections.Map.ofList
        let typeRules =
            rootRules |> List.choose (function |TypeRule (k, rs) -> Some (k, rs) |_ -> None)

        let typesMap = types |> (Map.map (fun _ s -> StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))))
        let enumsMap = enums |> (Map.map (fun _ s -> StringSet.Create(InsensitiveStringComparer(), s)))

        let rec singleFoldRules fNode fChild fLeaf fLeafValue fComment acc child rule :'r =
            let recurse = singleFoldRules fNode fChild fLeaf fLeafValue fComment
            match child with
            |NodeC node ->
                let finalAcc = fNode acc node rule
                match fChild node rule with
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
            let fChild (node : Node) ((field, options) : NewRule) =
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
                let childMatch = node.Children |> List.tryFind (fun c -> Range.rangeContainsPos c.Position pos)
                let leafMatch = node.Leaves |> Seq.tryFind (fun l -> Range.rangeContainsPos l.Position pos)
                let leafValueMatch = node.LeafValues |> Seq.tryFind (fun lv -> Range.rangeContainsPos lv.Position pos)
                let ctx = { subtypes = []; scopes = defaultContext; warningOnly = false }
                match childMatch, leafMatch, leafValueMatch with
                |Some c, _, _ ->
                    match expandedrules |> List.choose (function |(NodeRule (l, rs), o) when checkLeftField enumsMap typesMap effectMap triggerMap localisation files ctx l node.Key node -> Some (l, rs, o) |_ -> None) with
                    | [] ->
                        // let leftClauseRule =
                        //     expandedrules |>
                        //     List.filter (function |(_, _, LeftClauseField (vt, _)) -> checkValidLeftClauseRule files enumsMap (LeftClauseField (vt, ClauseField [])) c.Key  |_ -> false )
                        // let leftTypeRule =
                        //     expandedrules |>
                        //     List.filter (function |(_, _, LeftTypeField (t, r)) -> checkValidLeftTypeRule typesMap (LeftTypeField (t, r)) c.Key  |_ -> false )
                        // let leftScopeRule =
                        //     expandedrules |>
                        //     List.filter (function |(_, _, LeftScopeField (rs)) -> checkValidLeftScopeRule scopes (LeftScopeField (rs)) c.Key  |_ -> false )
                        // let leftRules = leftClauseRule @ leftScopeRule @ leftTypeRule
                        // match leftRules with
                        Some (NodeC c, (field, options))
                    | (l, rs, o)::_ -> Some (NodeC c, ((NodeRule (l, rs)), o))
                |_, Some leaf, _ ->
                    match expandedrules |> List.choose (function |(LeafRule (l, r), o) when checkLeftField enumsMap typesMap effectMap triggerMap localisation files ctx l leaf.Key leaf -> Some (l, r, o) |_ -> None) with
                    //match expandedrules |> List.filter (fst3 >> (==) l.Key) with
                    |[] ->
                        // let leftTypeRule =
                        //     expandedrules |>
                        //     List.tryFind (function |(_, _, LeftTypeField (t, r)) -> checkValidLeftTypeRule typesMap (LeftTypeField (t, r)) l.Key  |_ -> false )
                        // let leftClauseRule =
                        //     expandedrules |>
                        //     List.tryFind (function |(_, _, LeftClauseField (vt, _)) -> checkValidLeftClauseRule files enumsMap (LeftClauseField (vt, ClauseField [])) l.Key  |_ -> false )
                        // match Option.orElse leftClauseRule leftTypeRule with
                        // |Some r -> Some (LeafC l, r) //#TODO this doesn't correct handle lefttype
                        Some (LeafC leaf, (field, options))
                    |(l, rs, o)::_ -> Some (LeafC leaf, ((LeafRule (l, rs)), o))
                |_, _, Some lv -> Some (LeafValueC lv, (field, options))
                |None, None, None -> None
            let pathDir = (Path.GetDirectoryName logicalpath).Replace("\\","/")
            let childMatch = node.Children |> List.tryFind (fun c -> Range.rangeContainsPos c.Position pos)
            //eprintfn "%O %A %A" pos pathDir (typedefs |> List.tryHead)
            match childMatch, typedefs |> List.tryFind (fun t -> checkPathDir t pathDir) with
            |Some c, Some typedef ->
                let typerules = typeRules |> List.filter (fun (name, _) -> name == typedef.name)
                match typerules with
                |[(n, (NodeRule (l, rs), o))] ->
                    Some (singleFoldRules fNode fChild fLeaf fLeafValue fComment acc (NodeC c) ((NodeRule (l, rs), o)))
                |_ -> None
            |_, _ -> None

        let getInfoAtPos (pos : pos) (entity : Entity) =
            let fLeaf (ctx, res) (leaf : Leaf) ((field, _) : NewRule) =
                match field with
                |LeafRule (_, TypeField t) -> ctx, Some (t, leaf.Value.ToString())
                |_ -> ctx, res
            let fLeafValue (ctx) (leafvalue : LeafValue) _ =
                ctx
            let fComment (ctx) _ _ = ctx
            let fNode (ctx, res) (node : Node) ((field, options) : NewRule) =
                //eprintfn "nr %A %A %A" name node.Key ctx.scopes
                let rec inner f c (n : Node) =
                    let newCtx =
                        // match oneToOneScopes |> List.tryFind (fun (k, _) -> k == n.Key) with
                        // |Some (_, f) ->
                        //     match f (c.scopes, false) with
                        //     |(s, true) -> {c with scopes = {c.scopes with Scopes = s.CurrentScope::c.scopes.Scopes}}
                        //     |(s, false) -> {c with scopes = s}
                        // |None ->
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
                                    then {ctx with scopes = {ctx.scopes with Scopes = Scope.Any::ctx.scopes.Scopes}}
                                    else ctx
                    match f with
                    | NodeRule (ScopeField s, f) ->
                        let scope = newCtx.scopes
                        let key = n.Key
                        let newCtx =
                            match changeScope effectMap triggerMap key scope with
                            |NewScope ({Scopes = current::_} ,_) ->
                                //eprintfn "cs %A %A %A" name node.Key current
                                {newCtx with scopes = {newCtx.scopes with Scopes = current::newCtx.scopes.Scopes}}
                            |_ -> newCtx
                        newCtx, res
                    | NodeRule (_, f) -> newCtx, res
                    // | Field.LeftTypeField (t, f) -> inner f newCtx n
                    // | Field.ClauseField rs -> newCtx, res
                    // | Field.LeftClauseField (_, rs) -> newCtx, res
                    // | Field.LeftScopeField rs ->
                    //     let scope = newCtx.scopes
                    //     let key = n.Key
                    //     let newCtx =
                    //         match changeScope effectMap triggerMap key scope with
                    //         |NewScope ({Scopes = current::_} ,_) ->
                    //             //eprintfn "cs %A %A %A" name node.Key current
                    //             {newCtx with scopes = {newCtx.scopes with Scopes = current::newCtx.scopes.Scopes}}
                    //         |_ -> newCtx
                    //     newCtx, res
                    | _ -> newCtx, res
                inner field ctx node

            let pathDir = (Path.GetDirectoryName entity.logicalpath).Replace("\\","/")
            let childMatch = entity.entity.Children |> List.tryFind (fun c -> Range.rangeContainsPos c.Position pos)
            // eprintfn "%O %A %A %A" pos pathDir (typedefs |> List.tryHead) (childMatch.IsSome)
            let ctx =
                match childMatch, typedefs |> List.tryFind (fun t -> checkPathDir t pathDir) with
                |Some c, Some typedef ->
                    let pushScope, subtypes = ruleApplicator.TestSubtype (typedef.subtypes, c)
                    match pushScope with
                    |Some ps -> { subtypes = subtypes; scopes = { Root = ps; From = []; Scopes = [ps] }; warningOnly = false}
                    |None -> { subtypes = subtypes; scopes = defaultContext; warningOnly = false }
                |_, _ -> { subtypes = []; scopes = defaultContext; warningOnly = false }

            let ctx = ctx, None
            foldWithPos fLeaf fLeafValue fComment fNode ctx (pos) (entity.entity) (entity.logicalpath)


        let foldCollect fLeaf fLeafValue fComment fNode acc (node : Node) (path: string) =
            let fChild (node : Node) ((field, options) : NewRule) =
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
                let ctx = { subtypes = []; scopes = defaultContext; warningOnly = false  }
                let innerN (c : Node) =
                    match expandedrules |> List.choose (function |(NodeRule (l, rs), o) when checkLeftField enumsMap typesMap effectMap triggerMap localisation files ctx l node.Key node -> Some (l, rs, o) |_ -> None) with
                    | [] ->
                        // let leftClauseRule =
                        //     expandedrules |>
                        //     List.filter (function |(_, _, LeftClauseField (vt, _)) -> checkValidLeftClauseRule files enumsMap (LeftClauseField (vt, ClauseField [])) c.Key  |_ -> false )
                        // let leftTypeRule =
                        //     expandedrules |>
                        //     List.filter (function |(_, _, LeftTypeField (t, r)) -> checkValidLeftTypeRule typesMap (LeftTypeField (t, r)) c.Key  |_ -> false )
                        // let leftScopeRule =
                        //     expandedrules |>
                        //     List.filter (function |(_, _, LeftScopeField (rs)) -> checkValidLeftScopeRule scopes (LeftScopeField (rs)) c.Key  |_ -> false )
                        // let leftRules = leftClauseRule @ leftScopeRule @ leftTypeRule
                        // match leftRules with
                        Some (NodeC c, (field, options))
                        // |r::_ -> Some( NodeC c, r)
                    | (l, rs, o)::_ -> Some (NodeC c, ((NodeRule (l, rs)), o))
                let innerL (leaf : Leaf) =
                    match expandedrules |> List.choose (function |(LeafRule (l, r), o) when checkLeftField enumsMap typesMap effectMap triggerMap localisation files ctx l leaf.Key leaf -> Some (l, r, o) |_ -> None) with
                    |[] ->
                        // let leftTypeRule =
                        //     expandedrules |>
                        //     List.tryFind (function |(_, _, LeftTypeField (t, r)) -> checkValidLeftTypeRule typesMap (LeftTypeField (t, r)) l.Key  |_ -> false )
                        // let leftClauseRule =
                        //     expandedrules |>
                        //     List.tryFind (function |(_, _, LeftClauseField (vt, _)) -> checkValidLeftClauseRule files enumsMap (LeftClauseField (vt, ClauseField [])) l.Key  |_ -> false )
                        // match Option.orElse leftClauseRule leftTypeRule with
                        // |Some r -> Some (LeafC l, r) //#TODO this doesn't correct handle lefttype
                        Some (LeafC leaf, (field, options))
                    |(l, rs, o)::_ -> Some (LeafC leaf, ((LeafRule (l, rs)), o))
                let innerLV lv = Some (LeafValueC lv, (field, options))
                (node.Children |> List.choose innerN) @ (node.Leaves |> List.ofSeq |> List.choose innerL) @ (node.LeafValues |> List.ofSeq |> List.choose innerLV)
            let pathDir = (Path.GetDirectoryName path).Replace("\\","/")
            //eprintfn "%A %A" pathDir (typedefs |> List.tryHead)
            match typedefs |> List.tryFind (fun t -> checkPathDir t pathDir) with
            |Some typedef ->
                let typerules = typeRules |> List.filter (fun (name, _) -> name == typedef.name)
                match typerules with
                |[(n, (NodeRule (l, rs), o))] ->
                    (node.Children |> List.fold (fun a c -> foldRules fNode fChild fLeaf fLeafValue fComment a (NodeC c) (NodeRule (l, rs), o)) acc)
                |_ -> acc
            |_ -> acc

        let getTypesInEntity (entity : Entity) =
            let fLeaf (res : Collections.Map<string, (string * range) list>) (leaf : Leaf) ((field, _) : NewRule) =
                match field with
                |LeafRule (_, TypeField t) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    res |> (fun m -> m.Add(typename, (leaf.Value.ToString(), leaf.Position)::(m.TryFind(typename) |> Option.defaultValue [])))
                |_ -> res
            let fLeafValue (res : Collections.Map<string, (string * range) list>) (leafvalue : LeafValue) (field, _) =
                match field with
                |LeafValueRule (TypeField t) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    res |> (fun m -> m.Add(t, (leafvalue.Value.ToString(), leafvalue.Position)::(m.TryFind(typename) |> Option.defaultValue [])))
                |_ -> res

            let fComment (res) _ _ = res
            let fNode (res) (node : Node) ((field, option) : NewRule) = res
            let fCombine a b = (a |> List.choose id) @ (b |> List.choose id)

            let ctx = typedefs |> List.fold (fun (a : Collections.Map<string, (string * range) list>) t -> a.Add(t.name, [])) Collections.Map.empty
            let res = foldCollect fLeaf fLeafValue fComment fNode ctx (entity.entity) (entity.logicalpath)
            res

        member __.GetInfo(pos : pos, entity : Entity) = getInfoAtPos pos entity
        member __.GetReferencedTypes(entity : Entity) = getTypesInEntity entity


    // type CompletionService(rootRules : RootRule list, typedefs : TypeDefinition list , types : Collections.Map<string, (string * range) list>, enums : Collections.Map<string, string list>, files : Collections.Set<string>)  =
    //     let aliases =
    //         rootRules |> List.choose (function |AliasRule (a, rs) -> Some (a, rs) |_ -> None)
    //     let typeRules =
    //         rootRules |> List.choose (function |TypeRule (k, rs) -> Some (k, rs) |_ -> None)
    //     let typesMap = types |> (Map.map (fun _ s -> StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))))
    //     let types = types |> Map.map (fun k s -> s |> List.map fst)
    //     let enumsMap = enums |> (Map.map (fun _ s -> StringSet.Create(InsensitiveStringComparer(), s)))

    //     let fieldToCompletionList (field : NewField) =
    //         match field with
    //         |ValueField (Enum e) -> enums.TryFind(e) |> Option.bind (List.tryHead) |> Option.defaultValue "x"
    //         |ValueField v -> getValidValues v |> Option.bind (List.tryHead) |> Option.defaultValue "x"
    //         |TypeField t -> types.TryFind(t) |> Option.bind (List.tryHead) |> Option.defaultValue "x"
    //         |ClauseField _ -> "{ }"
    //         |ScopeField _ -> "THIS"
    //         |_ -> "x"


    //     let createSnippetForClause (rules : NewRule list) (description : string option) (key : string) =
    //         let filterToCompletion =
    //             function
    //             |AliasField _ |LeftClauseField _ |Field.LeftClauseField _ |Field.LeftTypeField _ |Field.SubtypeField _ -> false
    //             |_ -> true
    //         let requiredRules = rules |> List.filter (fun (_,o,f) -> o.min >= 1 && filterToCompletion f)
    //                                   |> List.mapi (fun i (k, o, f) -> sprintf "\t%s = ${%i:%s}\n" k (i + 1) (fieldToCompletionList f))
    //                                   |> String.concat ""
    //         Snippet (key, (sprintf "%s = {\n%s\t$0\n}" key requiredRules), description)


    //     let rec getRulePath (pos : pos) (stack : (string * bool) list) (node : Node) =
    //        match node.Children |> List.tryFind (fun c -> Range.rangeContainsPos c.Position pos) with
    //        | Some c -> getRulePath pos ((c.Key, false) :: stack) c
    //        | None ->
    //             match node.Leaves |> Seq.tryFind (fun l -> Range.rangeContainsPos l.Position pos) with
    //             | Some l -> (l.Key, true)::stack
    //             | None -> stack

    //     and getCompletionFromPath (rules : NewRule list) (stack : (string * bool) list) =
    //         let rec convRuleToCompletion (rule : NewRule) =
    //             let s, o, f = rule
    //             let keyvalue (inner : string) = Snippet (inner, (sprintf "%s = $0" inner), o.description)
    //             match o.leafvalue with
    //             |false ->
    //                 match f with
    //                 |Field.ClauseField rs -> [createSnippetForClause rs o.description s]
    //                 |Field.ObjectField _ -> [keyvalue s]
    //                 |Field.ValueField _ -> [keyvalue s]
    //                 |Field.TypeField _ -> [keyvalue s]
    //                 |Field.AliasField a -> aliases |> List.choose (fun (al, rs) -> if a == al then Some rs else None) |> List.collect convRuleToCompletion
    //                 |Field.LeftClauseField ((ValueType.Enum e), ClauseField rs) -> enums.TryFind(e) |> Option.defaultValue [] |> List.map (fun s -> createSnippetForClause rs o.description s)
    //                 |Field.LeftTypeField (t, ClauseField rs) -> types.TryFind(t) |> Option.defaultValue [] |> List.map (fun s -> createSnippetForClause rs o.description s)
    //                 |Field.LeftTypeField (t, _) -> types.TryFind(t) |> Option.defaultValue [] |> List.map keyvalue
    //                 |_ -> [Simple s]
    //             |true ->
    //                 match f with
    //                 |Field.TypeField t -> types.TryFind(t) |> Option.defaultValue [] |> List.map Simple
    //                 |Field.ValueField (Enum e) -> enums.TryFind(e) |> Option.defaultValue [] |> List.map Simple
    //                 |_ -> []
    //         let rec findRule (rules : Rule list) (stack) =
    //             let expandedRules =
    //                 rules |> List.collect (
    //                     function
    //                     | _,_,(AliasField a) -> (aliases |> List.filter (fun (s,_) -> s == a) |> List.map snd)
    //                     | _,_,(SubtypeField (_, _, (ClauseField cf))) -> cf
    //                     |x -> [x])
    //             match stack with
    //             |[] -> expandedRules |> List.collect convRuleToCompletion
    //             |(key, isLeaf)::rest ->
    //                 let fieldToRules (field : Field) =
    //                     match field with
    //                     //|Field.EffectField -> findRule rootRules rest
    //                     |Field.ClauseField rs -> findRule rs rest
    //                     |Field.LeftClauseField (_, ClauseField rs) -> findRule rs rest
    //                     |Field.ObjectField et ->
    //                         match et with
    //                         |EntityType.ShipSizes -> [Simple "large"; Simple "medium"]
    //                         |_ -> []
    //                     |Field.ValueField (Enum e) -> if isLeaf then enums.TryFind(e) |> Option.defaultValue [] |> List.map Simple else []
    //                     |Field.ValueField v -> if isLeaf then getValidValues v |> Option.defaultValue [] |> List.map Simple else []
    //                     |Field.TypeField t -> if isLeaf then types.TryFind(t) |> Option.defaultValue [] |> List.map Simple else []
    //                     |_ -> []

    //                 match expandedRules |> List.filter (fun (k,_,_) -> k == key) with
    //                 |[] ->
    //                     let leftClauseRule =
    //                         expandedRules |>
    //                         List.tryFind (function |(_, _, LeftClauseField (vt, _)) -> checkValidLeftClauseRule files enumsMap (LeftClauseField (vt, ClauseField [])) key  |_ -> false )
    //                     let leftTypeRule =
    //                         expandedRules |>
    //                         List.tryFind (function |(_, _, LeftTypeField (vt, f)) -> checkValidLeftTypeRule typesMap (LeftTypeField (vt, f)) key  |_ -> false )
    //                     let leftScopeRule =
    //                         expandedRules |>
    //                         List.tryFind (function |(_, _, LeftScopeField (rs)) -> checkValidLeftScopeRule scopes (LeftScopeField (rs)) key  |_ -> false )
    //                     match leftClauseRule, leftTypeRule, leftScopeRule with
    //                     |Some (_, _, f), _, _ ->
    //                         match f with
    //                         |Field.LeftClauseField (_, ClauseField rs) -> findRule rs rest
    //                         |_ -> expandedRules |> List.collect convRuleToCompletion
    //                     |_, Some (_, _, f), _ ->
    //                         match f with
    //                         |Field.LeftTypeField (_, rs) -> fieldToRules rs
    //                         |_ -> expandedRules |> List.collect convRuleToCompletion
    //                     |_, _, Some (_, _, f) ->
    //                         match f with
    //                         |Field.LeftScopeField (rs) -> findRule rs rest
    //                         |_ -> expandedRules |> List.collect convRuleToCompletion
    //                     |None, None, None ->
    //                         expandedRules |> List.collect convRuleToCompletion
    //                 |fs -> fs |> List.collect (fun (_, _, f) -> fieldToRules f)
    //         findRule rules stack |> List.distinct

    //     let complete (pos : pos) (entity : Entity) =
    //         let path = getRulePath pos [] entity.entity |> List.rev
    //         let pathDir = (Path.GetDirectoryName entity.logicalpath).Replace("\\","/")
    //         // eprintfn "%A" typedefs
    //         // eprintfn "%A" pos
    //         // eprintfn "%A" entity.logicalpath
    //         // eprintfn "%A" pathDir
    //         let typekeyfilter (td : TypeDefinition) (n : string) =
    //             match td.typeKeyFilter with
    //             |Some (filter, negate) -> n == filter <> negate
    //             |None -> true
    //         let skiprootkey (td : TypeDefinition) (n : string) =
    //             match td.skipRootKey with
    //             |Some key -> n == key
    //             |None -> false
    //         let skipcomp =
    //             match typedefs |> List.filter (fun t -> checkPathDir t pathDir && skiprootkey t (if path.Length > 0 then path.Head |> fst else "")) with
    //             |[] -> None
    //             |xs ->
    //                 match xs |> List.tryFind (fun t -> checkPathDir t pathDir && typekeyfilter t (if path.Length > 1 then path.Tail |> List.head |> fst else "")) with
    //                 |Some typedef ->
    //                     let typerules = typeRules |> List.filter (fun (name, _, _) -> name == typedef.name)
    //                     let fixedpath = if List.isEmpty path then path else (typedef.name, true)::(path |> List.tail |> List.tail)
    //                     let completion = getCompletionFromPath typerules fixedpath
    //                     Some completion
    //                 |None -> None
    //         skipcomp |> Option.defaultWith
    //             (fun () ->
    //             match typedefs |> List.tryFind (fun t -> checkPathDir t pathDir && typekeyfilter t (if path.Length > 0 then path.Head |> fst else "")) with
    //             |Some typedef ->
    //                 let typerules = typeRules |> List.filter (fun (name, _, _) -> name == typedef.name)
    //                 let fixedpath = if List.isEmpty path then path else (typedef.name, true)::(path |> List.tail)
    //                 let completion = getCompletionFromPath typerules fixedpath
    //                 completion
    //             |None -> getCompletionFromPath typeRules path)

    //     member __.Complete(pos : pos, entity : Entity) = complete pos entity

    let getTypesFromDefinitions (ruleapplicator : RuleApplicator) (types : TypeDefinition list) (es : Entity list) =
        let getTypeInfo (def : TypeDefinition) =
            es |> List.choose (fun e -> if checkPathDir def ((Path.GetDirectoryName e.logicalpath).Replace("\\","/")) then Some e.entity else None)
               |> List.collect (fun e ->
                            let inner (n : Node) =
                                let subtypes = ruleapplicator.TestSubtype(def.subtypes, n) |> snd |> List.map (fun s -> def.name + "." + s)
                                let key =
                                    match def.nameField with
                                    |Some f -> n.TagText f
                                    |None -> n.Key
                                let result = def.name::subtypes |> List.map (fun s -> s, (key, n.Position))
                                match def.typeKeyFilter with
                                |Some (filter, negate) -> if n.Key == filter <> negate then result else []
                                |None -> result
                            let childres =
                                match def.skipRootKey with
                                |Some key ->
                                    e.Children |> List.filter (fun c -> c.Key == key) |> List.collect (fun c -> c.Children |> List.collect inner)
                                |None ->
                                    (e.Children |> List.collect inner)
                            childres
                            @
                            (e.LeafValues |> List.ofSeq |> List.map (fun lv -> def.name, (lv.Value.ToString(), lv.Position))))
        let results = types |> List.collect getTypeInfo |> List.fold (fun m (n, k) -> if Map.containsKey n m then Map.add n (k::m.[n]) m else Map.add n [k] m) Map.empty
        types |> List.map (fun t -> t.name) |> List.fold (fun m k -> if Map.containsKey k m then m else Map.add k [] m ) results

    let getEnumsFromComplexEnums (complexenums : (ComplexEnumDef) list) (es : Entity list) =
        let rec inner (enumtree : Node) (node : Node) =
            match enumtree.Children with
            |head::_ ->
                if enumtree.Children |> List.exists (fun n -> n.Key == "enum_name")
                then node.Children |> List.map (fun n -> n.Key.Trim([|'\"'|])) else
                node.Children |> List.collect (inner head)
            |[] ->
                if enumtree.LeafValues |> Seq.exists (fun lv -> lv.Value.ToRawString() == "enum_name")
                then node.LeafValues |> Seq.map (fun lv -> lv.Value.ToRawString().Trim([|'\"'|])) |> List.ofSeq
                else
                    match enumtree.Leaves |> Seq.tryFind (fun l -> l.Value.ToRawString() == "enum_name") with
                    |Some leaf -> node.TagsText (leaf.Key) |> Seq.map (fun k -> k.Trim([|'\"'|])) |> List.ofSeq
                    |None -> []
        let getEnumInfo (complexenum : ComplexEnumDef) =
            let values = es |> List.choose (fun e -> if e.logicalpath.Replace("\\","/").StartsWith(complexenum.path.Replace("\\","/")) then Some e.entity else None)
                            |> List.collect (fun e -> e.Children |> List.collect (inner complexenum.nameTree))
            complexenum.name, values
        complexenums |> List.map getEnumInfo