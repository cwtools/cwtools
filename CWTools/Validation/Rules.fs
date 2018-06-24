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

module rec Rules =
    type StringSet = Set<string, InsensitiveStringComparer>

    let fst3 (a, _, _) = a
    let snd3 (_, b, _) = b
    let thd3 (_, _, c) = c

    let getValidValues =
        function
        |ValueType.Bool -> Some ["yes"; "no"]
        |ValueType.Enum es -> Some [es]
        |_ -> None

    let checkValidLeftClauseRule (enums : Collections.Map<string, StringSet>) (field : Field) (key : string) =
        match field with
        |LeftClauseField (ValueType.Int (min, max), _) ->
            match TryParser.parseInt key with
            |Some i -> min <= i && i <= max
            |None -> false
        |LeftClauseField (ValueType.Float (min, max), _) ->
            match TryParser.parseDouble key with
            |Some i -> min <= i && i <= max
            |None -> false
        |LeftClauseField (ValueType.Enum e, _) ->
            match enums.TryFind e with
            |Some es -> es.Contains key
            |None -> false
        |LeftClauseField (ValueType.Scalar, _) -> true
        |_ -> false

    let checkValidLeftTypeRule (types : Collections.Map<string, StringSet>) (field : Field) (key : string) =
        match field with
        |LeftTypeField (t, _) ->
            match types.TryFind t with
            |Some keys -> keys.Contains key
            |None -> false
        |_ -> false

    let checkValidLeftScopeRule (scopes : string list) (field : Field) (key : string) =
        match field with
        |LeftScopeField _ ->
            match key with
            |x when x.StartsWith "event_target:" -> true
            |x when x.StartsWith "parameter:" -> true
            |x ->
                let xs = x.Split '.'
                xs |> Array.forall (fun s -> scopes |> List.exists (fun s2 -> s == s2))
        |_ -> false

    let checkFileExists (files : Collections.Set<string>) (leaf : Leaf) =
        let file = leaf.Value.ToRawString().Replace("/","\\")
        if files.Contains file then OK else Invalid [inv (ErrorCodes.MissingFile file) leaf]
    let scopes = (scopedEffects |> List.map (fun se -> se.Name)) @ (oneToOneScopes |> List.map fst)

    type CompletionResponse =
    |Simple of label : string
    |Detailed of label : string * desc : string option
    |Snippet of label : string * snippet : string * desc : string option
    type RuleContext =
        {
            subtypes : string list
            scopes : ScopeContext
        }
    type RuleApplicator(rootRules : RootRule list, typedefs : TypeDefinition list , types : Collections.Map<string, (string * range) list>, enums : Collections.Map<string, string list>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Effect list, effects : Effect list) =
        let triggerMap = triggers |> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))
        let effectMap = effects |> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))

        let aliases =
            rootRules |> List.choose (function |AliasRule (a, rs) -> Some (a, rs) |_ -> None)
                        |> List.groupBy fst
                        |> List.map (fun (k, vs) -> k, vs |> List.map snd)
                        |> Collections.Map.ofList
        let typeRules =
            rootRules |> List.choose (function |TypeRule (rs) -> Some (rs) |_ -> None)
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
            |_ -> true

        let checkValidValue (leaf : Leaf) (vt : ValueType) =
            let key = leaf.Value.ToString().Trim([|'"'|])
            if key.StartsWith "@" then OK else
                match vt with
                |ValueType.Bool ->
                    if key = "yes" || key = "no" then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue "Expecting yes or no") leaf]
                |ValueType.Enum e ->
                    match enumsMap.TryFind e with
                    |Some es -> if es.Contains key then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting one of %A" es)) leaf]
                    |None -> OK
                |ValueType.Float (min, max) ->
                    match leaf.Value with
                    |Float f -> if f <= max && f >= min then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %f and %f" min max)) leaf]
                    |Int f -> if float f <= max && float f >= min then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %f and %f" min max)) leaf]
                    |_ ->
                        match TryParser.parseDouble key with
                        |Some f -> if f < max && f > min then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %f and %f" min max)) leaf]
                        |None -> Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a float, got %s" key)) leaf]
                |ValueType.Int (min, max) ->
                    match leaf.Value with
                    |Int i -> if i <= max && i >= min then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %i and %i" min max)) leaf]
                    |_ ->
                        match TryParser.parseInt key with
                        |Some i ->  if i <= max && i >= min then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %i and %i" min max)) leaf]
                        |None -> Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting an integer, got %s" key)) leaf]
                |ValueType.Specific s -> if key.Trim([|'\"'|]) == s then OK else Invalid [inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting value %s" s)) leaf]
                |ValueType.Scalar -> OK
                |_ -> Invalid [inv (ErrorCodes.ConfigRulesUnexpectedValue "Invalid value") leaf]

        let checkLocalisationField (keys : (Lang * Collections.Set<string>) list) (synced : bool) (leaf : Leaf) =
            match synced with
            |true ->
                let defaultKeys = keys |> List.choose (fun (l, ks) -> if l = STL STLLang.Default then Some ks else None) |> List.tryHead |> Option.defaultValue Set.empty
                let key = leaf.Value |> (function |QString s -> s |s -> s.ToString())
                checkLocName leaf defaultKeys (STL STLLang.Default) key
            |false ->
                checkLocKeys keys leaf
        let rec applyClauseField (root : Node) (enforceCardinality : bool) (ctx : RuleContext) (rules : Rule list) (startNode : Node) =
            let subtypedrules =
                rules |> List.collect (fun (s,o,r) -> r |> (function |SubtypeField (key, shouldMatch, ClauseField cfs) -> (if (not shouldMatch) <> List.contains key ctx.subtypes then cfs else []) | x -> [(s, o, x)]))
            // let subtypedrules =
            //     match ctx.subtype with
            //     |Some st -> rules |> List.collect (fun (s,o,r) -> r |> (function |SubtypeField (key, ClauseField cfs) -> (if key = st then cfs else []) |x -> [(s, o, x)]))
            //     |None -> rules |> List.choose (fun (s,o,r) -> r |> (function |SubtypeField (key, cf) -> None |x -> Some (s, o, x)))
            let expandedrules =
                subtypedrules |> List.collect (
                    function
                    | _,_,(AliasField a) -> (aliases.TryFind a |> Option.defaultValue [])
                    |x -> [x])
            let valueFun (leaf : Leaf) =
                match expandedrules |> List.filter (fst3 >> (==) leaf.Key) with
                |[] ->
                    let leftTypeRule =
                        expandedrules |>
                        List.tryFind (function |(_, _, LeftTypeField (t, r)) -> checkValidLeftTypeRule typesMap (LeftTypeField (t, r)) leaf.Key  |_ -> false )
                    match leftTypeRule with
                    |Some (_, _, f) -> applyLeafRule root ctx f leaf
                    |_ ->
                        if enforceCardinality then Invalid [inv (ErrorCodes.ConfigRulesUnexpectedProperty (sprintf "Unexpected node %s in %s" leaf.Key startNode.Key)) leaf] else OK
                |rs -> rs <&??&> (fun (_, _, f) -> applyLeafRule root ctx f leaf) |> mergeValidationErrors "CW240"
            let nodeFun (node : Node) =
                match expandedrules |> List.filter (fst3 >> (==) node.Key) with
                | [] ->
                    let leftClauseRule =
                        expandedrules |>
                        List.filter (function |(_, _, LeftClauseField (vt, _)) -> checkValidLeftClauseRule enumsMap (LeftClauseField (vt, [])) node.Key  |_ -> false )
                    let leftTypeRule =
                        expandedrules |>
                        List.filter (function |(_, _, LeftTypeField (t, r)) -> checkValidLeftTypeRule typesMap (LeftTypeField (t, r)) node.Key  |_ -> false )
                    let leftScopeRule =
                        expandedrules |>
                        List.filter (function |(_, _, LeftScopeField (rs)) -> checkValidLeftScopeRule scopes (LeftScopeField (rs)) node.Key  |_ -> false )
                    let leftRules = leftClauseRule @ leftTypeRule @ leftScopeRule
                    match leftRules with
                    |[] -> if enforceCardinality then Invalid [inv (ErrorCodes.ConfigRulesUnexpectedProperty (sprintf "Unexpected node %s in %s" node.Key startNode.Key)) node] else OK
                    |rs -> rs <&??&> (fun (_, o, f) -> applyNodeRule root enforceCardinality ctx o f node)
                | rs -> rs <&??&> (fun (_, o, f) -> applyNodeRule root enforceCardinality ctx o f node)
            let checkCardinality (node : Node) (rule : Rule) =
                let key, opts, field = rule
                match key, field with
                | "leafvalue", _
                | _, Field.SubtypeField _
                | _, Field.AliasField _
                | _, Field.LeftClauseField _
                | _, Field.LeftTypeField _ -> OK
                | _ ->
                    let leafcount = node.Tags key |> Seq.length
                    let childcount = node.Childs key |> Seq.length
                    let total = leafcount + childcount
                    if opts.min > total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %s, requires %i" key opts.min)) node]
                    else if opts.max < total then Invalid [inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many %s, max %i" key opts.max)) node]
                    else OK
            startNode.Leaves <&!&> valueFun
            <&&>
            (startNode.Children <&!&> nodeFun)
            <&&>
            (rules <&!&> checkCardinality startNode)

        and applyValueField (vt : ValueType) (leaf : Leaf) =
            checkValidValue leaf vt
            // match isValidValue leaf.Value vt with
            // | true -> OK
            // | false -> Invalid [inv (ErrorCodes.CustomError "Invalid value" Severity.Error) leaf]
        and applyObjectField (entityType : EntityType) (leaf : Leaf) =
            let values =
                match entityType with
                | EntityType.ShipSizes -> ["medium"; "large"]
                | EntityType.StarbaseModules -> ["trafficControl"]
                | EntityType.StarbaseBuilding -> ["crew"]
                | _ -> []
            let value = leaf.Value.ToString()
            if values |> List.exists (fun s -> s == value) then OK else Invalid [invCustom leaf]

        and applyTypeField (t : string) (leaf : Leaf) =
            match typesMap.TryFind t with
            |Some values ->
                let value = leaf.Value.ToString().Trim([|'\"'|])
                if values.Contains value then OK else Invalid [inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected value of type %s" t)) leaf]
            |None -> OK

        and applyLeftTypeFieldLeaf (t : string) (leaf : Leaf) =
            match typesMap.TryFind t with
            |Some values ->
                let value = leaf.Key
                if values.Contains value then OK else Invalid [inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected key of type %s" t)) leaf]
            |None -> OK

        and applyScopeField (root : Node) (ctx : RuleContext) (s : Scope) (leaf : Leaf) =
            // let scope2 =
            //     match getScopeContextAtPos leaf.Position.Start triggers effects root with
            //     |Some s -> s
            //     |None -> {Root = Scope.Any; From = []; Scopes = [Scope.Any]}
            let scope = ctx.scopes
            let key = leaf.Value.ToString()
            match changeScope effectMap triggerMap key scope with
            |NewScope ({Scopes = current::_} ,_) -> if current = s || s = Scope.Any || current = Scope.Any then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leaf]
            |NotFound _ -> Invalid [inv (ErrorCodes.ConfigRulesInvalidTarget (s.ToString())) leaf]
            |WrongScope (command, prevscope, expected) -> Invalid [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) leaf]
            |_ -> OK
            // <&&>
            // ( match changeScope effectMap triggerMap key scope2 with
            // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = Scope.Any || current = Scope.Any then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leaf]
            // |NotFound _ -> Invalid [inv (ErrorCodes.ConfigRulesInvalidTarget (s.ToString())) leaf]
            // |WrongScope (command, prevscope, expected) -> Invalid [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) leaf]
            // |_ -> OK)
            // match key with
            // |x when x.StartsWith "event_target:" -> OK
            // |x when x.StartsWith "parameter:" -> OK
            // |x ->
            //     let xs = x.Split '.'
            //     if xs |> Array.forall (fun s -> scopes |> List.exists (fun s2 -> s == s2)) then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected value of scope %s" (s.ToString()))) leaf]
        and applyLeftScopeField (root : Node) (enforceCardinality : bool) (ctx : RuleContext) (rules : Rule list) (startNode : Node) =
            let scope = ctx.scopes
            let key = startNode.Key
            match changeScope effectMap triggerMap key scope with
            |NewScope ({Scopes = current::_} ,_) ->
                let newCtx = {ctx with scopes = {ctx.scopes with Scopes = current::ctx.scopes.Scopes}}
                applyClauseField root enforceCardinality newCtx rules startNode
            |NotFound _ ->
                Invalid [inv (ErrorCodes.CustomError "This scope command is not valid" Severity.Error) startNode]
            |WrongScope (command, prevscope, expected) ->
                Invalid [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) startNode]
            |_ -> OK
               // applyClauseField root enforceCardinality ctx rules startNode

        and applyLeftTypeFieldNode (t : string) (node : Node) =
            match typesMap.TryFind t with
            |Some values ->
                let value = node.Key
                if values.Contains value then OK else Invalid [inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected key of type %s" t)) node]
            |None -> OK

        and applyLeafRule (root : Node) (ctx : RuleContext) (rule : Field) (leaf : Leaf) =
            match rule with
            | Field.ValueField v -> applyValueField v leaf
            | Field.ObjectField et -> applyObjectField et leaf
            | Field.TypeField t -> applyTypeField t leaf
            | Field.LeftTypeField (t, f) -> applyLeftTypeFieldLeaf t leaf <&&> applyLeafRule root ctx f leaf
            | Field.ClauseField rs -> OK
            | Field.LeftClauseField _ -> OK
            | Field.LeftScopeField _ -> OK
            | Field.ScopeField s -> applyScopeField root ctx s leaf
            | Field.LocalisationField synced -> checkLocalisationField localisation synced leaf
            | Field.FilepathField -> checkFileExists files leaf
            | Field.AliasField _ -> OK
            | Field.SubtypeField _ -> OK

        and applyNodeRule (root : Node) (enforceCardinality : bool) (ctx : RuleContext) (options : Options) (rule : Field) (node : Node) =
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
            match rule with
            | Field.ValueField v -> OK
            | Field.ObjectField et -> OK
            | Field.TypeField _ -> OK
            | Field.LeftTypeField (t, f) -> applyLeftTypeFieldNode t node <&&> applyNodeRule root enforceCardinality newCtx options f node
            | Field.ClauseField rs -> applyClauseField root enforceCardinality newCtx rs node
            | Field.LeftClauseField (_, rs) -> applyClauseField root enforceCardinality newCtx rs node
            | Field.LeftScopeField rs ->
                applyLeftScopeField root enforceCardinality newCtx rs node
            | Field.LocalisationField _ -> OK
            | Field.FilepathField -> OK
            | Field.ScopeField _ -> OK
            | Field.AliasField _ -> OK
            | Field.SubtypeField _ -> OK

        let testSubtype (subtypes : SubTypeDefinition list) (node : Node) =
            let results =
                subtypes |> List.filter (fun st -> st.typeKeyField |> function |Some tkf -> tkf == node.Key |None -> true)
                        |> List.map (fun s -> s.name, s.pushScope, applyClauseField node false {subtypes = []; scopes = defaultContext } (s.rules) node)
            let res = results |> List.choose (fun (s, ps, res) -> res |> function |Invalid _ -> None |OK -> Some (ps, s))
            res |> List.tryPick fst, res |> List.map snd

        let applyNodeRuleRoot (typedef : TypeDefinition) (rule : Field) (options : Options) (node : Node) =
            let pushScope, subtypes = testSubtype (typedef.subtypes) node
            let startingScopeContext =
                match Option.orElse pushScope options.pushScope with
                |Some ps -> { Root = ps; From = []; Scopes = [] }
                |None -> defaultContext
            let context = { subtypes = subtypes; scopes = startingScopeContext }
            applyNodeRule node true context options rule node

        let validate ((path, root) : string * Node) =
            let inner (node : Node) =

                let pathDir = (Path.GetDirectoryName path).Replace("/","\\")
                match typedefs |> List.tryFind (fun t -> pathDir = (t.path.Replace("/","\\"))) with
                |Some typedef ->
                    let typerules = typeRules |> List.filter (fun (name, _, _) -> name == typedef.name)
                    //eprintfn "%A" typerules
                    let expandedRules = typerules |> List.collect (function | _,_,(AliasField a) -> (aliases.TryFind a |> Option.defaultValue []) |x -> [x])
                    //eprintfn "%A" expandedRules
                    match expandedRules |> List.tryFind (fun (n, _, _) -> n == typedef.name) with
                    |Some (_, o, f) -> applyNodeRuleRoot typedef f o node
                    |None ->
                        //eprintfn "Couldn't find rules for %s" typedef.name
                        OK
                |None ->
                    //eprintfn "Couldn't find rule for %s" node.Key
                    OK
            let res = (root.Children <&!&> inner)
            res


        member __.ApplyNodeRule(rule, node) = applyNodeRule node true {subtypes = []; scopes = defaultContext } defaultOptions rule node
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
            rootRules |> List.choose (function |TypeRule (rs) -> Some (rs) |_ -> None)
        let typesMap = types |> (Map.map (fun _ s -> StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))))
        let enumsMap = enums |> (Map.map (fun _ s -> StringSet.Create(InsensitiveStringComparer(), s)))

        let rec foldRules fNode fChild fLeaf fLeafValue fComment acc child rule :'r =
            let recurse = foldRules fNode fChild fLeaf fLeafValue fComment
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

        let foldWithPos fLeaf fLeafValue fComment fNode acc (pos : pos) (node : Node) =
            let fChild (node : Node) ((name, options, field) : Rule) =
                let rules =
                    match field with
                    //| Field.LeftTypeField (t, f) -> inner f newCtx n
                    | Field.ClauseField rs -> rs
                    | Field.LeftClauseField (_, rs) -> rs
                    | Field.LeftScopeField rs -> rs
                    | _ -> []
                let subtypedrules =
                    rules |> List.collect (fun (s,o,r) -> r |> (function |SubtypeField (_, _, ClauseField cfs) -> cfs | x -> [(s, o, x)]))
                let expandedrules =
                    subtypedrules |> List.collect (
                        function
                        | _,_,(AliasField a) -> (aliases.TryFind a |> Option.defaultValue [])
                        |x -> [x])
                let childMatch = node.Children |> List.tryFind (fun c -> Range.rangeContainsPos c.Position pos)
                let leafMatch = node.Leaves |> Seq.tryFind (fun l -> Range.rangeContainsPos l.Position pos)
                let leafValueMatch = node.LeafValues |> Seq.tryFind (fun lv -> Range.rangeContainsPos lv.Position pos)
                match childMatch, leafMatch, leafValueMatch with
                |Some c, _, _ ->
                    match expandedrules |> List.filter (fst3 >> (==) c.Key) with
                    | [] ->
                        let leftClauseRule =
                            expandedrules |>
                            List.filter (function |(_, _, LeftClauseField (vt, _)) -> checkValidLeftClauseRule enumsMap (LeftClauseField (vt, [])) c.Key  |_ -> false )
                        let leftTypeRule =
                            expandedrules |>
                            List.filter (function |(_, _, LeftTypeField (t, r)) -> checkValidLeftTypeRule typesMap (LeftTypeField (t, r)) c.Key  |_ -> false )
                        let leftScopeRule =
                            expandedrules |>
                            List.filter (function |(_, _, LeftScopeField (rs)) -> checkValidLeftScopeRule scopes (LeftScopeField (rs)) c.Key  |_ -> false )
                        let leftRules = leftClauseRule @ leftScopeRule @ leftTypeRule
                        match leftRules with
                        |[] -> Some (NodeC c, (name, options, field))
                        |r::_ -> Some( NodeC c, r)
                    | r::_ -> Some (NodeC c, r)
                |_, Some l, _ ->
                    match expandedrules |> List.filter (fst3 >> (==) l.Key) with
                    |[] ->
                        let leftTypeRule =
                            expandedrules |>
                            List.tryFind (function |(_, _, LeftTypeField (t, r)) -> checkValidLeftTypeRule typesMap (LeftTypeField (t, r)) l.Key  |_ -> false )
                        match leftTypeRule with
                        |Some r -> Some (LeafC l, r) //#TODO this doesn't correct handle lefttype
                        |_ -> Some (LeafC l, (name, options, field))
                    |r::_ -> Some (LeafC l, r)
                |_, _, Some lv -> Some (LeafValueC lv, (name, options, field))
                |None, None, None -> None
            let pathDir = (Path.GetDirectoryName node.Position.FileName).Replace("/","\\")
            let childMatch = node.Children |> List.tryFind (fun c -> Range.rangeContainsPos c.Position pos)
            //eprintfn "%O %A %A" pos pathDir (typedefs |> List.tryHead)
            match childMatch, typedefs |> List.tryFind (fun t -> pathDir = (t.path.Replace("/","\\"))) with
            |Some c, Some typedef ->
                let typerules = typeRules |> List.filter (fun (name, _, _) -> name == typedef.name)
                match typerules with
                |[(n, o, ClauseField rs)] ->
                    Some (foldRules fNode fChild fLeaf fLeafValue fComment acc (NodeC c) (n, o, ClauseField rs))
                |_ -> None
            |_, _ -> None

        let getInfoAtPos (pos : pos) (node : Node) =
            let fLeaf (ctx, res) (leaf : Leaf) ((_, _, field) : Rule) =
                match field with
                |Field.TypeField t -> ctx, Some (t, leaf.Value.ToString())
                |_ -> ctx, res
            let fLeafValue (ctx) (leafvalue : LeafValue) _ =
                ctx
            let fComment (ctx) _ _ = ctx
            let fNode (ctx, res) (node : Node) ((name, options, field) : Rule) =
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
                    | Field.LeftTypeField (t, f) -> inner f newCtx n
                    | Field.ClauseField rs -> newCtx, res
                    | Field.LeftClauseField (_, rs) -> newCtx, res
                    | Field.LeftScopeField rs ->
                        let scope = newCtx.scopes
                        let key = n.Key
                        let newCtx =
                            match changeScope effectMap triggerMap key scope with
                            |NewScope ({Scopes = current::_} ,_) ->
                                //eprintfn "cs %A %A %A" name node.Key current
                                {newCtx with scopes = {newCtx.scopes with Scopes = current::newCtx.scopes.Scopes}}
                            |_ -> newCtx
                        newCtx, res
                    | _ -> newCtx, res
                inner field ctx node

            let pathDir = (Path.GetDirectoryName node.Position.FileName).Replace("/","\\")
            let childMatch = node.Children |> List.tryFind (fun c -> Range.rangeContainsPos c.Position pos)
            //eprintfn "%O %A %A" pos pathDir (typedefs |> List.tryHead)
            let ctx =
                match childMatch, typedefs |> List.tryFind (fun t -> pathDir = (t.path.Replace("/","\\"))) with
                |Some c, Some typedef ->
                    let pushScope, subtypes = ruleApplicator.TestSubtype (typedef.subtypes, c)
                    match pushScope with
                    |Some ps -> { subtypes = subtypes; scopes = { Root = ps; From = []; Scopes = [ps] }}
                    |None -> { subtypes = subtypes; scopes = defaultContext }
                |_, _ -> { subtypes = []; scopes = defaultContext }

            let ctx = ctx, None
            foldWithPos fLeaf fLeafValue fComment fNode ctx (pos) (node)
        member __.GetInfo(pos : pos, node : Node) = getInfoAtPos pos node




    type TypeInfo(rootRules : RootRule list, typedefs : TypeDefinition list , types : Collections.Map<string, (string * range) list>, enums : Collections.Map<string, string list>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Effect list, effects : Effect list) =
        let triggerMap = triggers |> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))
        let effectMap = effects |> List.map (fun e -> e.Name, e) |> (fun l -> EffectMap.FromList(InsensitiveStringComparer(), l))

        let aliases =
            rootRules |> List.choose (function |AliasRule (a, rs) -> Some (a, rs) |_ -> None)
                        |> List.groupBy fst
                        |> List.map (fun (k, vs) -> k, vs |> List.map snd)
                        |> Collections.Map.ofList

        let typeRules =
            rootRules |> List.choose (function |TypeRule (rs) -> Some (rs) |_ -> None)
        let typesMap = types |> (Map.map (fun _ s -> StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))))
        let enumsMap = enums |> (Map.map (fun _ s -> StringSet.Create(InsensitiveStringComparer(), s)))

        let rec getInfoFromNode (pos : pos) (ctx : RuleContext) (options : Options) (rule : Field) (node : Node) =
            //eprintfn "%A %A" rule node.Key
            let newCtx =
                match oneToOneScopes |> List.tryFind (fun (k, _) -> k == node.Key) with
                |Some (_, f) ->
                    match f (ctx.scopes, false) with
                    |(s, true) -> {ctx with scopes = {ctx.scopes with Scopes = s.CurrentScope::ctx.scopes.Scopes}}
                    |(s, false) -> {ctx with scopes = s}
                |None ->
                    match options.pushScope with
                    |Some ps ->
                        {ctx with scopes = {ctx.scopes with Scopes = ps::ctx.scopes.Scopes}}
                    |None ->
                        match options.replaceScopes with
                        |Some rs ->
                            match rs.this, rs.froms with
                            |Some this, Some froms ->
                                {ctx with scopes = {ctx.scopes with Scopes = this::(ctx.scopes.PopScope); From = froms}}
                            |Some this, None ->
                                {ctx with scopes = {ctx.scopes with Scopes = this::(ctx.scopes.PopScope)}}
                            |None, Some froms ->
                                {ctx with scopes = {ctx.scopes with From = froms}}
                            |None, None ->
                                ctx
                        |None ->
                            if node.Key.StartsWith("event_target:", System.StringComparison.OrdinalIgnoreCase) || node.Key.StartsWith("parameter:", System.StringComparison.OrdinalIgnoreCase)
                            then {ctx with scopes = {ctx.scopes with Scopes = Scope.Any::ctx.scopes.Scopes}}
                            else ctx
            match rule with
            | Field.LeftTypeField (t, f) -> getInfoFromNode pos newCtx options f node
            | Field.ClauseField rs -> getInfoFromPos pos rs newCtx node
            | Field.LeftClauseField (_, rs) -> getInfoFromPos pos rs newCtx node
            | Field.LeftScopeField rs -> getInfoFromPos pos rs newCtx node
            | _ -> None


        and getRulePath (pos : pos) (stack : (string * bool) list) (node : Node) =
           match node.Children |> List.tryFind (fun c -> Range.rangeContainsPos c.Position pos) with
           | Some c -> getRulePath pos ((c.Key, false) :: stack) c
           | None ->
                match node.Leaves |> Seq.tryFind (fun l -> Range.rangeContainsPos l.Position pos) with
                | Some l -> (l.Key, true)::stack
                | None -> stack
        and getInfoFromLeaf (pos : pos) (field : Field) (ctx : RuleContext) (leaf : Leaf) =
            //let valueMatch =
            //TODO check if on left or right hand side of value
            //eprintfn "%A %A" field leaf.Key
            match field with
            |Field.TypeField t -> Some (t, leaf.Value.ToString())
            |_ -> None
        and getInfoFromPos (pos : pos) (rules : Rule list) (ctx : RuleContext) (node : Node) =
            //eprintfn "ifp %A" node.Key
            let subtypedrules =
                rules |> List.collect (fun (s,o,r) -> r |> (function |SubtypeField (key, shouldMatch, ClauseField cfs) -> (if (not shouldMatch) <> List.contains key ctx.subtypes then cfs else []) | x -> [(s, o, x)]))
            let expandedrules =
                subtypedrules |> List.collect (
                    function
                    | _,_,(AliasField a) -> (aliases.TryFind a |> Option.defaultValue [])
                    |x -> [x])

            let childMatch = node.Children |> List.tryFind (fun c -> Range.rangeContainsPos c.Position pos)
            let leafMatch = node.Leaves |> Seq.tryFind (fun l -> Range.rangeContainsPos l.Position pos)
            let leafValueMatch = node.LeafValues |> Seq.tryFind (fun lv -> Range.rangeContainsPos lv.Position pos)
            //eprintfn "%A %A %A" childMatch leafMatch leafValueMatch
            match childMatch, leafMatch, leafValueMatch with
            |Some c, _, _ ->
                //eprintfn "%A" expandedrules
                match expandedrules |> List.filter (fst3 >> (==) c.Key) with
                | [] ->
                    let leftClauseRule =
                        expandedrules |>
                        List.filter (function |(_, _, LeftClauseField (vt, _)) -> checkValidLeftClauseRule enumsMap (LeftClauseField (vt, [])) c.Key  |_ -> false )
                    let leftTypeRule =
                        expandedrules |>
                        List.filter (function |(_, _, LeftTypeField (t, r)) -> checkValidLeftTypeRule typesMap (LeftTypeField (t, r)) c.Key  |_ -> false )
                    let leftScopeRule =
                        expandedrules |>
                        List.filter (function |(_, _, LeftScopeField (rs)) -> checkValidLeftScopeRule scopes (LeftScopeField (rs)) c.Key  |_ -> false )
                    let leftRules = leftClauseRule @ leftTypeRule @ leftScopeRule
                    match leftRules with
                    |[] -> None
                    |rs -> rs |> List.fold (fun acc (_, o, f) -> acc |> Option.orElse (getInfoFromNode pos ctx o f c)) None
                | rs -> eprintfn "%A" rs; rs |> List.fold (fun acc (_, o, f) -> acc |> Option.orElse (getInfoFromNode pos ctx o f c)) None

            |_, Some l, _ ->
                match expandedrules |> List.filter (fst3 >> (==) l.Key) with
                |[] ->
                    let leftTypeRule =
                        expandedrules |>
                        List.tryFind (function |(_, _, LeftTypeField (t, r)) -> checkValidLeftTypeRule typesMap (LeftTypeField (t, r)) l.Key  |_ -> false )
                    match leftTypeRule with
                    |Some (_, _, f) -> getInfoFromLeaf pos f ctx l
                    |_ -> None
                |rs -> rs |> List.fold (fun acc (_, o, f) -> acc |> Option.orElse (getInfoFromLeaf pos f ctx l)) None


            |_, _, Some lv ->
                None
            |_ -> //Key match!
                None



        let getInfo (pos : pos) (node : Node) =
            let ctx = {subtypes = []; scopes = defaultContext }
            let path = getRulePath pos [] node |> List.rev
            let pathDir = (Path.GetDirectoryName node.Position.FileName).Replace("/","\\")
            let childMatch = node.Children |> List.tryFind (fun c -> Range.rangeContainsPos c.Position pos)
            eprintfn "%O %A %A" pos pathDir (typedefs |> List.tryHead)
            match childMatch, typedefs |> List.tryFind (fun t -> pathDir = (t.path.Replace("/","\\"))) with
            |Some c, Some typedef ->
                let typerules = typeRules |> List.filter (fun (name, _, _) -> name == typedef.name)
                match typerules with
                |[(_, _, ClauseField rs)] ->
                    eprintfn "%A %O" typedef rs
                    let completion = getInfoFromPos pos rs ctx c
                    completion
                |_ -> None
            |_, _ -> None
        member __.GetInfo(pos : pos, node : Node) = getInfo pos node


    type CompletionService(rootRules : RootRule list, typedefs : TypeDefinition list , types : Collections.Map<string, (string * range) list>, enums : Collections.Map<string, string list>) =
        let aliases =
            rootRules |> List.choose (function |AliasRule (a, rs) -> Some (a, rs) |_ -> None)
        let typeRules =
            rootRules |> List.choose (function |TypeRule (rs) -> Some (rs) |_ -> None)
        let typesMap = types |> (Map.map (fun _ s -> StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))))
        let types = types |> Map.map (fun k s -> s |> List.map fst)
        let enumsMap = enums |> (Map.map (fun _ s -> StringSet.Create(InsensitiveStringComparer(), s)))

        let fieldToCompletionList (field : Field) =
            match field with
            |Field.ValueField (Enum e) -> enums.TryFind(e) |> Option.bind (List.tryHead) |> Option.defaultValue "x"
            |Field.ValueField v -> getValidValues v |> Option.bind (List.tryHead) |> Option.defaultValue "x"
            |Field.TypeField t -> types.TryFind(t) |> Option.bind (List.tryHead) |> Option.defaultValue "x"
            |Field.ClauseField _ -> "{ }"
            |Field.ScopeField _ -> "THIS"
            |_ -> "x"


        let createSnippetForClause (rules : Rule list) (description : string option) (key : string) =
            let filterToCompletion =
                function
                |Field.AliasField _ |Field.LeftClauseField _ |Field.LeftClauseField _ |Field.LeftTypeField _ |Field.SubtypeField _ -> false
                |_ -> true
            let requiredRules = rules |> List.filter (fun (_,o,f) -> o.min >= 1 && filterToCompletion f)
                                      |> List.mapi (fun i (k, o, f) -> sprintf "\t%s = ${%i:%s}\n" k (i + 1) (fieldToCompletionList f))
                                      |> String.concat ""
            Snippet (key, (sprintf "%s = {\n%s\t$0\n}" key requiredRules), description)


        let rec getRulePath (pos : pos) (stack : (string * bool) list) (node : Node) =
           match node.Children |> List.tryFind (fun c -> Range.rangeContainsPos c.Position pos) with
           | Some c -> getRulePath pos ((c.Key, false) :: stack) c
           | None ->
                match node.Leaves |> Seq.tryFind (fun l -> Range.rangeContainsPos l.Position pos) with
                | Some l -> (l.Key, true)::stack
                | None -> stack

        and getCompletionFromPath (rules : Rule list) (stack : (string * bool) list) =
            let rec convRuleToCompletion (rule : Rule) =
                let s, o, f = rule
                let keyvalue (inner : string) = Snippet (inner, (sprintf "%s = $0" inner), o.description)
                match o.leafvalue with
                |false ->
                    match f with
                    |Field.ClauseField rs -> [createSnippetForClause rs o.description s]
                    |Field.ObjectField _ -> [keyvalue s]
                    |Field.ValueField _ -> [keyvalue s]
                    |Field.TypeField _ -> [keyvalue s]
                    |Field.AliasField a -> aliases |> List.choose (fun (al, rs) -> if a == al then Some rs else None) |> List.collect convRuleToCompletion
                    |Field.LeftClauseField ((ValueType.Enum e), rs) -> enums.TryFind(e) |> Option.defaultValue [] |> List.map (fun s -> createSnippetForClause rs o.description s)
                    |Field.LeftTypeField (t, ClauseField rs) -> types.TryFind(t) |> Option.defaultValue [] |> List.map (fun s -> createSnippetForClause rs o.description s)
                    |Field.LeftTypeField (t, _) -> types.TryFind(t) |> Option.defaultValue [] |> List.map keyvalue
                    |_ -> [Simple s]
                |true ->
                    match f with
                    |Field.TypeField t -> types.TryFind(t) |> Option.defaultValue [] |> List.map Simple
                    |Field.ValueField (Enum e) -> enums.TryFind(e) |> Option.defaultValue [] |> List.map Simple
                    |_ -> []
            let rec findRule (rules : Rule list) (stack) =
                let expandedRules =
                    rules |> List.collect (
                        function
                        | _,_,(AliasField a) -> (aliases |> List.filter (fun (s,_) -> s == a) |> List.map snd)
                        | _,_,(SubtypeField (_, _, (ClauseField cf))) -> cf
                        |x -> [x])
                match stack with
                |[] -> expandedRules |> List.collect convRuleToCompletion
                |(key, isLeaf)::rest ->
                    let fieldToRules (field : Field) =
                        match field with
                        //|Field.EffectField -> findRule rootRules rest
                        |Field.ClauseField rs -> findRule rs rest
                        |Field.LeftClauseField (_, rs) -> findRule rs rest
                        |Field.ObjectField et ->
                            match et with
                            |EntityType.ShipSizes -> [Simple "large"; Simple "medium"]
                            |_ -> []
                        |Field.ValueField (Enum e) -> if isLeaf then enums.TryFind(e) |> Option.defaultValue [] |> List.map Simple else []
                        |Field.ValueField v -> if isLeaf then getValidValues v |> Option.defaultValue [] |> List.map Simple else []
                        |Field.TypeField t -> if isLeaf then types.TryFind(t) |> Option.defaultValue [] |> List.map Simple else []
                        |_ -> []

                    match expandedRules |> List.filter (fun (k,_,_) -> k == key) with
                    |[] ->
                        let leftClauseRule =
                            expandedRules |>
                            List.tryFind (function |(_, _, LeftClauseField (vt, _)) -> checkValidLeftClauseRule enumsMap (LeftClauseField (vt, [])) key  |_ -> false )
                        let leftTypeRule =
                            expandedRules |>
                            List.tryFind (function |(_, _, LeftTypeField (vt, f)) -> checkValidLeftTypeRule typesMap (LeftTypeField (vt, f)) key  |_ -> false )
                        match leftClauseRule, leftTypeRule with
                        |Some (_, _, f), _ ->
                            match f with
                            |Field.LeftClauseField (_, rs) -> findRule rs rest
                            |_ -> expandedRules |> List.collect convRuleToCompletion
                        |_, Some (_, _, f) ->
                            match f with
                            |Field.LeftTypeField (_, rs) -> fieldToRules rs
                            |_ -> expandedRules |> List.collect convRuleToCompletion
                        |None, None ->
                            expandedRules |> List.collect convRuleToCompletion
                    |fs -> fs |> List.collect (fun (_, _, f) -> fieldToRules f)
            findRule rules stack |> List.distinct

        let complete (pos : pos) (node : Node) =
            let path = getRulePath pos [] node |> List.rev
            let pathDir = (Path.GetDirectoryName node.Position.FileName).Replace("/","\\")
            match typedefs |> List.tryFind (fun t -> pathDir = (t.path.Replace("/","\\"))) with
            |Some typedef ->
                let typerules = typeRules |> List.filter (fun (name, _, _) -> name == typedef.name)
                let fixedpath = if List.isEmpty path then path else (typedef.name, true)::(path |> List.tail)
                let completion = getCompletionFromPath typerules fixedpath
                completion
            |None -> getCompletionFromPath typeRules path

        member __.Complete(pos : pos, node : Node) = complete pos node

    let getTypesFromDefinitions (ruleapplicator : RuleApplicator) (types : TypeDefinition list) (es : Entity list) =
        let getTypeInfo (def : TypeDefinition) =
            es |> List.choose (fun e -> if  e.logicalpath.Replace("/","\\").StartsWith(def.path.Replace("/","\\")) then Some e.entity else None)
               |> List.collect (fun e ->
                            let inner (n : Node) =
                                let subtypes = ruleapplicator.TestSubtype(def.subtypes, n) |> snd |> List.map (fun s -> def.name + "." + s)
                                let key =
                                    match def.nameField with
                                    |Some f -> n.TagText f
                                    |None -> n.Key
                                def.name::subtypes |> List.map (fun s -> s, (key, n.Position))
                            (e.Children |> List.collect inner)
                            @
                            (e.LeafValues |> List.ofSeq |> List.map (fun lv -> def.name, (lv.Value.ToString(), lv.Position))))
        types |> List.collect getTypeInfo |> List.fold (fun m (n, k) -> if Map.containsKey n m then Map.add n (k::m.[n]) m else Map.add n [k] m) Map.empty


    let getEnumsFromComplexEnums (complexenums : (ComplexEnumDef) list) (es : Entity list) =
        let rec inner (enumtree : Node) (node : Node) =
            match enumtree.Children with
            |head::_ ->
                if enumtree.Children |> List.exists (fun n -> n.Key == "enum_name")
                then node.Children |> List.map (fun n -> n.Key) else
                node.Children |> List.collect (inner head)
            |[] ->

                if enumtree.LeafValues |> Seq.exists (fun lv -> lv.Value.ToRawString() == "enum_name")
                then node.LeafValues |> Seq.map (fun lv -> lv.Value.ToRawString().Trim([|'\"'|])) |> List.ofSeq
                else
                    match enumtree.Leaves |> Seq.tryFind (fun l -> l.Value.ToRawString() == "enum_name") with
                    |Some leaf -> node.TagsText (leaf.Key) |> List.ofSeq
                    |None -> []
        let getEnumInfo (complexenum : ComplexEnumDef) =
            let values = es |> List.choose (fun e -> if e.logicalpath.Replace("/","\\").StartsWith(complexenum.path.Replace("/","\\")) then Some e.entity else None)
                            |> List.collect (fun e -> e.Children |> List.collect (inner complexenum.nameTree))
            complexenum.name, values
        complexenums |> List.map getEnumInfo