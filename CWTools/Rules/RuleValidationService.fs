namespace CWTools.Rules

open CWTools.Rules
open CWTools.Process
open CWTools.Utilities.Utils
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Utilities.Position
open CWTools.Utilities
open CWTools.Common
open Microsoft.FSharp.Collections.Tagged
open System.IO
open QuickGraph
open System
open CWTools.Process.Scopes
open CWTools.Games

// let inline ruleValidationServiceCreator(rootRules : RootRule<_> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>, enums : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<_>,InsensitiveStringComparer>, effects : Map<string,Effect<_>,InsensitiveStringComparer>, anyScope, changeScope, (defaultContext : ScopeContext<_>), checkLocField :( (Lang * Collections.Set<string> )list -> bool -> string -> _ -> ValidationResult)) =
// let inline ruleValidationServiceCreator(rootRules : RootRule< ^T> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>, enums : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<_>,InsensitiveStringComparer>, effects : Map<string,Effect<_>,InsensitiveStringComparer>, anyScope, changeScope, (defaultContext : ScopeContext<_>), defaultLang) =
type RuleValidationService
                                (rootRules : RootRule<_> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>,
                                 enums : Collections.Map<string, string * StringSet>, varMap : Collections.Map<string, StringSet>,
                                 localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>,
                                 links : Map<string,Effect,InsensitiveStringComparer>,
                                 valueTriggers : Map<string,Effect,InsensitiveStringComparer>,
                                 anyScope, changeScope : ChangeScope<_>, defaultContext : ScopeContext<_>, defaultLang) =

    let mutable errorList : ResizeArray<CWError> = new ResizeArray<CWError>()
    let linkMap = links
    let valueTriggerMap = valueTriggers
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
    let wildCardLinks = linkMap.ToList() |> List.map snd |> List.choose (function | :? ScopedEffect as e when e.IsWildCard -> Some e |_ -> None )
    let defaultKeys = localisation |> List.choose (fun (l, ks) -> if l = defaultLang then Some ks else None) |> List.tryHead |> Option.defaultValue Set.empty
    let localisationKeys = localisation |> List.choose (fun (l, ks) -> if l = defaultLang then None else Some (l, ks))

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
        let dict = new System.Collections.Concurrent.ConcurrentDictionary<_,System.Collections.Generic.Dictionary<_,_>>()
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
                            dict.TryAdd(rules, innerDict) |> ignore
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
                // let res = expandedsubtypedrules @ subtypedrules @ rules @ expandedbaserules
                // let res = expandedsubtypedrules @ subtypedrules @ rules @ expandedbaserules
                let noderules = new ResizeArray<_>()
                let leafrules = new ResizeArray<_>()
                let leafvaluerules = new ResizeArray<_>()
                let valueclauserules = new ResizeArray<_>()
                let nodeSpecificMap = new System.Collections.Generic.Dictionary<_,_>()
                let leafSpecificMap = new System.Collections.Generic.Dictionary<_,_>()
                let inner =
                    (fun r ->
                        match r with
                        | (NodeRule (SpecificField (SpecificValue v), rs), o) as x ->
                            let found, res = nodeSpecificMap.TryGetValue(v.lower)
                            if found
                            then nodeSpecificMap.[v.lower] <- x::res
                            else nodeSpecificMap.[v.lower] <- [x]
                        | (NodeRule (l, rs), o) as x -> noderules.Add(x)
                        | (LeafRule (SpecificField (SpecificValue v), r), o) as x ->
                            let found, res = leafSpecificMap.TryGetValue(v.lower)
                            if found
                            then leafSpecificMap.[v.lower] <- x::res
                            else leafSpecificMap.[v.lower] <- [x]
                        | (LeafRule (l, r), o) as x -> leafrules.Add(x)
                        | (LeafValueRule (lv), o) as x -> leafvaluerules.Add(x)
                        | (ValueClauseRule (rs), o) as x -> valueclauserules.Add(x)
                        // | (NodeRule (l, rs), o) as x -> noderules.Add(l, rs, o)
                        // | (LeafRule (l, r), o) as x -> leafrules.Add(l, r, o)
                        // | (LeafValueRule (lv), o) as x -> leafvaluerules.Add(lv, o)
                        // | (ValueClauseRule (rs), o) as x -> valueclauserules.Add(rs, o)
                        | _ -> ()
                        )
                // res |> Seq.iter inner
                // expandedres |> Seq.iter inner
                // expandedres2 |> Seq.iter inner
                expandedsubtypedrules |> Seq.iter inner
                subtypedrules |> Seq.iter inner
                rules |> Seq.iter inner
                expandedbaserules |> Seq.iter inner
                noderules, leafrules, leafvaluerules, valueclauserules, nodeSpecificMap, leafSpecificMap
                // seq { yield! rules; yield! subtypedrules; yield! expandedbaserules; yield! expandedsubtypedrules }
        memoizeRulesInner memFunction
    let p = {
        varMap = varMap
        enumsMap = enumsMap
        typesMap = typesMap
        linkMap = linkMap
        valueTriggerMap = valueTriggerMap
        varSet = varSet
        localisation = localisationKeys
        defaultLocalisation = defaultKeys
        files = files
        changeScope = changeScope
        anyScope = anyScope
        defaultLang = defaultLang
        wildcardLinks = wildCardLinks
    }


    let rec applyClauseField (enforceCardinality : bool) (nodeSeverity : Severity option) (ctx : RuleContext<_>) (rules : NewRule<_> list) (startNode : IClause) errors =
        let severity = nodeSeverity |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)
        // TODO: Memoize expanded rules depending  on ctx.subtypes ad rules?
        let noderules, leafrules, leafvaluerules, valueclauserules, nodeSpecificDict, leafSpecificDict = memoizeRules rules ctx.subtypes
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
        // let expandedrules = seq { yield! rules; yield! subtypedrules; yield! expandedbaserules; yield! expandedsubtypedrules } sprintfn "%s is unexpected in %s"
        let valueFun innerErrors (leaf : Leaf) =
            let key = leaf.Key
            let keyId = leaf.KeyId.lower
            let createDefault() = if enforceCardinality && ((leaf.Key.[0] <> '@')) then inv (ErrorCodes.ConfigRulesUnexpectedPropertyNode (sprintf "%s is unexpected in %s" key startNode.Key) severity) leaf <&&&> innerErrors else innerErrors
            let found, value = leafSpecificDict.TryGetValue (keyId)
            let rs =
                if found
                then seq { yield! value; yield! leafrules }
                else upcast leafrules
            rs |> Seq.filter (function |LeafRule (l, r), o -> FieldValidators.checkLeftField p Severity.Error ctx l keyId key |_ -> false)
                          |> (fun rs -> lazyErrorMerge rs (fun (LeafRule (l, r), o) e -> applyLeafRule ctx o r leaf e) createDefault innerErrors true)
        let nodeFun innerErrors (node : Node) =
            let key = node.Key
            let keyId = node.KeyId.lower
            let createDefault() = if enforceCardinality then inv (ErrorCodes.ConfigRulesUnexpectedPropertyLeaf (sprintf "%s is unexpected in %s" key startNode.Key) severity) node <&&&> innerErrors else innerErrors
            let found, value = nodeSpecificDict.TryGetValue (keyId)
            let rs =
                if found
                then seq { yield! value; yield! noderules }
                else upcast noderules
            rs |> Seq.filter (function |NodeRule (l, rs), o -> FieldValidators.checkLeftField p Severity.Error ctx l keyId key |_ -> false)
                          |> (fun rs -> lazyErrorMerge rs (fun (NodeRule (l, r), o) e -> applyNodeRule enforceCardinality ctx o l r node e) createDefault innerErrors false)
        let leafValueFun innerErrors (leafvalue : LeafValue) =
            let createDefault() = if enforceCardinality then inv (ErrorCodes.ConfigRulesUnexpectedPropertyLeafValue (sprintf "%s is unexpected in %s" leafvalue.Key startNode.Key) severity) leafvalue <&&&> innerErrors else innerErrors
            leafvaluerules |> Seq.filter (function |LeafValueRule l, o -> FieldValidators.checkLeftField p Severity.Error ctx l leafvalue.ValueId.lower leafvalue.Key |_ -> false)
                          |> (fun rs -> lazyErrorMerge rs (fun (LeafValueRule l, o) e -> applyLeafValueRule ctx o l leafvalue e) createDefault innerErrors true)
        let valueClauseFun innerErrors (valueclause : ValueClause) =
            let createDefault() = if enforceCardinality then inv (ErrorCodes.ConfigRulesUnexpectedPropertyValueClause (sprintf "Unexpected clause in %s" startNode.Key) severity) valueclause <&&&> innerErrors else innerErrors
            valueclauserules |> (fun rs -> lazyErrorMerge rs (fun (ValueClauseRule r, o) e -> applyValueClauseRule enforceCardinality ctx o r valueclause e) createDefault innerErrors true)
        let checkCardinality (clause : IClause) innerErrors (rule : NewRule<_>) =
            match rule with
            |NodeRule(SpecificField(SpecificValue key), _), opts
            |LeafRule(SpecificField(SpecificValue key), _), opts ->
                let leafcount = clause.Leaves |> Seq.filter (fun leaf -> leaf.KeyId.lower = key.lower) |> Seq.length
                let childcount = clause.Nodes |> Seq.filter (fun child -> child.KeyId.lower = key.lower) |> Seq.length
                let total = leafcount + childcount
                if opts.min > total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %s, expecting at least %i" (StringResource.stringManager.GetStringForID key.normal) opts.min) (opts.severity |> Option.defaultValue severity)) clause <&&&> innerErrors
                else if opts.max < total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many %s, expecting at most %i" (StringResource.stringManager.GetStringForID key.normal) opts.max) Severity.Warning) clause <&&&> innerErrors
                else innerErrors
            |NodeRule(AliasField(_), _), _
            |LeafRule(AliasField(_), _), _
            |LeafValueRule(AliasField(_)), _ -> innerErrors
            |NodeRule(l, _), opts ->
                let total = clause.Nodes |> Seq.filter (fun child -> FieldValidators.checkLeftField p Severity.Error ctx l child.KeyId.lower child.Key) |> Seq.length
                if opts.min > total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %O, expecting at least %i" l opts.min) (opts.severity |> Option.defaultValue severity)) clause <&&&> innerErrors
                else if opts.max < total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many n %O, expecting at most %i" l opts.max) Severity.Warning) clause <&&&> innerErrors
                else innerErrors
            |LeafRule(l, r), opts ->
                let total = clause.Leaves |> Seq.filter (fun leaf -> FieldValidators.checkLeftField p Severity.Error ctx l leaf.KeyId.lower leaf.Key) |> Seq.length
                if opts.min > total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %O, expecting at least %i" l opts.min) (opts.severity |> Option.defaultValue severity)) clause <&&&> innerErrors
                else if opts.max < total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many l %O %O, expecting at most %i" l r opts.max) Severity.Warning) clause <&&&> innerErrors
                else innerErrors
            |LeafValueRule(l), opts ->
                let total = clause.LeafValues |> List.ofSeq |> List.filter (fun leafvalue -> FieldValidators.checkLeftField p Severity.Error ctx l leafvalue.ValueId.lower leafvalue.Key) |> List.length
                if opts.min > total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %O, expecting at least %i" l opts.min) (opts.severity |> Option.defaultValue severity)) clause <&&&> innerErrors
                else if opts.max < total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many lv %O, expecting at most %i" l opts.max) Severity.Warning) clause <&&&> innerErrors
                else innerErrors
            |ValueClauseRule(_), opts ->
                let total = clause.ValueClauses |> Seq.length
                if opts.min > total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing clause, expecting at least %i" opts.min) (opts.severity |> Option.defaultValue severity)) clause <&&&> innerErrors
                else if opts.max < total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many clauses, expecting at most %i" opts.max) Severity.Warning) clause <&&&> innerErrors
                else innerErrors
            |_ -> innerErrors
        (applyToAll startNode.Leaves valueFun errors)
        |>
        (applyToAll startNode.Nodes nodeFun)
        |>
        (applyToAll startNode.LeafValues leafValueFun)
        |>
        (applyToAll startNode.ValueClauses valueClauseFun)
        |>
        (applyToAll rules (checkCardinality startNode))

    and applyValueField severity (vt : CWTools.Rules.ValueType) (leaf : Leaf) =
        FieldValidators.checkValidValue enumsMap localisation severity vt (leaf.ValueId.lower) (leaf.ValueText) leaf

    and applyLeafValueRule (ctx : RuleContext<_>) (options : Options<_>) (rule : NewField<_>) (leafvalue : LeafValue) errors =
        let severity = options.severity |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)
        // let errors = OK
        FieldValidators.checkField p severity ctx rule (leafvalue.ValueId.lower) (leafvalue.ValueText) (leafvalue) errors

    and applyLeafRule (ctx : RuleContext<_>) (options : Options<_>) (rule : NewField<_>) (leaf : Leaf) errors =
        let severity = options.severity |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)

        // let errors = OK
        (match options.requiredScopes with
        |[] -> OK
        |xs ->
            match ctx.scopes.CurrentScope with
            |x when x = anyScope -> OK
            |s -> if List.exists (fun x -> s.IsOfScope x) xs then OK else Invalid [inv (ErrorCodes.ConfigRulesRuleWrongScope (s.ToString()) (xs |> List.map (fun f -> f.ToString()) |> String.concat ", ") (leaf.Key)) leaf])
        <&&>
        FieldValidators.checkField p severity ctx rule (leaf.ValueId.lower) (leaf.ValueText) (leaf) errors
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
            |s -> if List.exists (fun x -> s.IsOfScope x) xs then OK else Invalid [inv (ErrorCodes.ConfigRulesRuleWrongScope (s.ToString()) (xs |> List.map (fun f -> f.ToString()) |> String.concat ", ") (node.Key)) node])
        <&&>
        match rule with
        |ScopeField s ->
            let scope = newCtx.scopes
            let key = node.Key
            match changeScope false true linkMap valueTriggerMap wildCardLinks varSet key scope with
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

    and applyValueClauseRule (enforceCardinality : bool) (ctx : RuleContext<_>) (options : Options<_>) (rules : NewRule<_> list) (valueclause : ValueClause) errors =
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
            |s -> if List.exists (fun x -> s.IsOfScope x) xs then OK else Invalid [inv (ErrorCodes.ConfigRulesRuleWrongScope (s.ToString()) (xs |> List.map (fun f -> f.ToString()) |> String.concat ", ") ("")) valueclause])
        <&&>
        applyClauseField enforceCardinality options.severity newCtx rules valueclause errors

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
        applyNodeRule true context options (SpecificField(SpecificValue rootId)) rules node OK

    let rootTypeDefs = typedefs |> List.filter (fun td -> td.type_per_file)
    let normalTypeDefs = typedefs |> List.filter (fun td -> td.type_per_file |> not )
    let validate ((path, root) : string * Node) =
        let pathDir = (Path.GetDirectoryName path).Replace("\\","/")
        let file = Path.GetFileName path
        let skiprootkey (skipRootKey : SkipRootKey) (n : Node) =
            match skipRootKey with
            |(SpecificKey key) -> n.Key == key
            |(AnyKey) -> true
            |(MultipleKeys (keys, shouldMatch)) ->
                (keys |> List.exists ((==) n.Key)) <> (not shouldMatch)

        let inner (typedefs : TypeDefinition<Scope> list) (node : Node) =
            let validateType (typedef : TypeDefinition<_>) (n : Node) =
                let typerules = typeRules |> List.choose (function |(name, r) when name == typedef.name -> Some r |_ -> None)
                //let expandedRules = typerules |> List.collect (function | (LeafRule (AliasField a, _),_) -> (aliases.TryFind a |> Option.defaultValue []) |x -> [x])
                //let expandedRules = typerules |> List.collect (function | _,_,(AliasField a) -> (aliases.TryFind a |> Option.defaultValue []) |x -> [x])
                //match expandedRules |> List.choose (function |(NodeRule (l, rs), o) when checkLeftField enumsMap typesMap effectMap triggerMap localisation files ctx l node.Key node -> Some (l, rs, o) |_ -> None) with
                //match expandedRules |> List.tryFind (fun (n, _, _) -> n == typedef.name) with
                match typerules |> List.tryHead with
                |Some ((NodeRule ((SpecificField(SpecificValue (x))), rs), o)) when (StringResource.stringManager.GetStringForID x.normal) == typedef.name->
                    if FieldValidators.typekeyfilter typedef n.Key then applyNodeRuleRoot typedef rs o n else OK
                |_ ->
                    OK
            let pathFilteredTypes = typedefs |> List.filter (fun t -> FieldValidators.checkPathDir t pathDir file)
            let rec validateTypeSkipRoot (t : TypeDefinition<_>) (skipRootKeyStack : SkipRootKey list) (n : Node) =
                match skipRootKeyStack with
                |[] -> if FieldValidators.typekeyfilter t n.Key then validateType t n else OK
                |head::tail ->
                    if skiprootkey head n
                    then n.Children <&!&> validateTypeSkipRoot t tail
                    else OK
            pathFilteredTypes <&!&> (fun t -> validateTypeSkipRoot t t.skipRootKey node)
        let res = (root.Children <&!&> inner normalTypeDefs)
        let rootres = (inner rootTypeDefs root)
        res <&&> rootres


    member this.ApplyNodeRule(rule, node) = applyNodeRule true {subtypes = []; scopes = defaultContext; warningOnly = false } CWTools.Rules.RulesParser.defaultOptions (SpecificField(SpecificValue rootId)) rule node OK
    member this.TestSubType(subtypes, node) = testSubtype subtypes node
    member this.RuleValidate() = (fun _ (es : EntitySet<_>) -> es.Raw |> List.map (fun struct(e, _) -> e.logicalpath, e.entity) <&!!&> validate)
    member this.RuleValidateEntity = (fun e -> validate (e.logicalpath, e.entity))
    // {
    //     applyNodeRule = (fun (rule, node) -> applyNodeRule true {subtypes = []; scopes = defaultContext; warningOnly = false } defaultOptions (ValueField (ValueType.Specific "root")) rule node)
    //     testSubtype = (fun ((subtypes), (node)) -> testSubtype subtypes node)
    //     ruleValidate = (fun () -> (fun _ es -> es.Raw |> List.map (fun struct(e, _) -> e.logicalpath, e.entity) <&!!&> validate))
    // }


// type InfoService(rootRules : RootRule<_> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>, enums : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<_>,InsensitiveStringComparer>, effects : Map<string,Effect<_>,InsensitiveStringComparer>, ruleValidationService : IRuleValidationService<_>, changeScope, defaultContext, anyScope) =
