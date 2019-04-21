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
open FSharp.Collections.ParallelSeq
open CWTools.Process.Scopes
open CWTools.Process.ProcessCore
open CWTools.Games

module Rules =

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
                                     enums : Collections.Map<string, string * StringSet>, varMap : Collections.Map<string, StringSet>,
                                     localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>,
                                     links : Map<string,Effect<'T>,InsensitiveStringComparer>,
                                     valueTriggers : Map<string,Effect<'T>,InsensitiveStringComparer>,
                                     anyScope, changeScope : ChangeScope<'T>, defaultContext : ScopeContext<_>, defaultLang) =

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
        let wildCardLinks = linkMap.ToList() |> List.map snd |> List.choose (function | :? ScopedEffect<'T> as e when e.IsWildCard -> Some e |_ -> None )

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
                        |> Seq.fold (fun (na, la, lva, vca) r ->
                            match r with
                            | (NodeRule (l, rs), o) -> (l, rs, o)::na, la, lva, vca
                            | (LeafRule (l, r), o) -> na, (l, r, o)::la, lva, vca
                            | (LeafValueRule (lv), o) -> na, la,(lv, o)::lva, vca
                            | (ValueClauseRule (rs), o) -> na, la, lva, (rs, o)::vca
                            | _ -> na, la, lva, vca
                            ) ([], [], [], [])
                    // seq { yield! rules; yield! subtypedrules; yield! expandedbaserules; yield! expandedsubtypedrules }
            memoizeRulesInner memFunction


        let rec applyClauseField (enforceCardinality : bool) (nodeSeverity : Severity option) (ctx : RuleContext<_>) (rules : NewRule<_> list) (startNode : IClause) errors =
            let severity = nodeSeverity |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)
            // TODO: Memoize expanded rules depending  on ctx.subtypes ad rules?
            let noderules, leafrules, leafvaluerules, valueclauserules = memoizeRules rules ctx.subtypes
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
                linkMap = linkMap
                valueTriggerMap = valueTriggerMap
                varSet = varSet
                localisation = localisation
                files = files
                changeScope = changeScope
                anyScope = anyScope
                defaultLang = defaultLang
                ctx = ctx
                severity = Severity.Error
                wildcardLinks = wildCardLinks
            }
            let valueFun innerErrors (leaf : Leaf) =
                let createDefault() = if enforceCardinality && ((leaf.Key.[0] <> '@')) then inv (ErrorCodes.ConfigRulesUnexpectedProperty (sprintf "Unexpected key for leaf %s in %s" leaf.Key startNode.Key) severity) leaf <&&&> innerErrors else innerErrors
                leafrules |> Seq.filter (fun (l, r, o) -> FieldValidators.checkLeftField p l leaf.KeyId.lower leaf.Key)
                              |> (fun rs -> lazyErrorMerge rs (fun (l, r, o) e -> applyLeafRule ctx o r leaf e) createDefault innerErrors true)
            let nodeFun innerErrors (node : Node) =
                let createDefault() = if enforceCardinality then inv (ErrorCodes.ConfigRulesUnexpectedProperty (sprintf "Unexpected key for node %s in %s" node.Key startNode.Key) severity) node <&&&> innerErrors else innerErrors
                noderules |> Seq.filter (fun (l, rs, o) -> FieldValidators.checkLeftField p l node.KeyId.lower node.Key)
                              |> (fun rs -> lazyErrorMerge rs (fun (l, r, o) e -> applyNodeRule enforceCardinality ctx o l r node e) createDefault innerErrors false)
            let leafValueFun innerErrors (leafvalue : LeafValue) =
                let createDefault() = if enforceCardinality then inv (ErrorCodes.ConfigRulesUnexpectedProperty (sprintf "Unexpected key for value %s in %s" leafvalue.Key startNode.Key) severity) leafvalue <&&&> innerErrors else innerErrors
                leafvaluerules |> Seq.filter (fun (l, o) -> FieldValidators.checkLeftField p l leafvalue.ValueId.lower leafvalue.Key)
                              |> (fun rs -> lazyErrorMerge rs (fun (l, o) e -> applyLeafValueRule ctx o l leafvalue e) createDefault innerErrors true)
            let valueClauseFun innerErrors (valueclause : ValueClause) =
                let createDefault() = if enforceCardinality then inv (ErrorCodes.ConfigRulesUnexpectedProperty (sprintf "Unexpected clause in %s" startNode.Key) severity) valueclause <&&&> innerErrors else innerErrors
                valueclauserules |> (fun rs -> lazyErrorMerge rs (fun (r, o) e -> applyValueClauseRule enforceCardinality ctx o r valueclause e) createDefault innerErrors true)
            let checkCardinality (clause : IClause) innerErrors (rule : NewRule<_>) =
                match rule with
                |NodeRule(ValueField (ValueType.Specific key), _), opts
                |LeafRule(ValueField (ValueType.Specific key), _), opts ->
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
                    let total = clause.Nodes |> Seq.filter (fun child -> FieldValidators.checkLeftField p l child.KeyId.lower child.Key) |> Seq.length
                    if opts.min > total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %O, expecting at least %i" l opts.min) (opts.severity |> Option.defaultValue severity)) clause <&&&> innerErrors
                    else if opts.max < total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many n %O, expecting at most %i" l opts.max) Severity.Warning) clause <&&&> innerErrors
                    else innerErrors
                |LeafRule(l, r), opts ->
                    let total = clause.Leaves |> Seq.filter (fun leaf -> FieldValidators.checkLeftField p l leaf.KeyId.lower leaf.Key) |> Seq.length
                    if opts.min > total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Missing %O, expecting at least %i" l opts.min) (opts.severity |> Option.defaultValue severity)) clause <&&&> innerErrors
                    else if opts.max < total then inv (ErrorCodes.ConfigRulesWrongNumber (sprintf "Too many l %O %O, expecting at most %i" l r opts.max) Severity.Warning) clause <&&&> innerErrors
                    else innerErrors
                |LeafValueRule(l), opts ->
                    let total = clause.LeafValues |> List.ofSeq |> List.filter (fun leafvalue -> FieldValidators.checkLeftField p l leafvalue.ValueId.lower leafvalue.Key) |> List.length
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
            FieldValidators.checkValidValue enumsMap severity vt (leaf.ValueId.lower) (leaf.ValueText) leaf

        and applyLeafValueRule (ctx : RuleContext<_>) (options : Options<_>) (rule : NewField<_>) (leafvalue : LeafValue) errors =
            let severity = options.severity |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)
            // let errors = OK
            let p = {
                varMap = varMap
                enumsMap = enumsMap
                typesMap = typesMap
                linkMap = linkMap
                valueTriggerMap = valueTriggerMap
                varSet = varSet
                localisation = localisation
                files = files
                changeScope = changeScope
                anyScope = anyScope
                defaultLang = defaultLang
                ctx = ctx
                severity = severity
                wildcardLinks = wildCardLinks
            }

            FieldValidators.checkField p rule (leafvalue.ValueId.lower) (leafvalue.ValueText) (leafvalue) errors

        and applyLeafRule (ctx : RuleContext<_>) (options : Options<_>) (rule : NewField<_>) (leaf : Leaf) errors =
            let severity = options.severity |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)
            let p = {
                varMap = varMap
                enumsMap = enumsMap
                typesMap = typesMap
                linkMap = linkMap
                valueTriggerMap = valueTriggerMap
                varSet = varSet
                localisation = localisation
                files = files
                changeScope = changeScope
                anyScope = anyScope
                defaultLang = defaultLang
                ctx = ctx
                severity = severity
                wildcardLinks = wildCardLinks
            }

            // let errors = OK
            (match options.requiredScopes with
            |[] -> OK
            |xs ->
                match ctx.scopes.CurrentScope with
                |x when x = anyScope -> OK
                |s -> if List.exists (fun x -> s.MatchesScope x) xs then OK else Invalid [inv (ErrorCodes.ConfigRulesRuleWrongScope (s.ToString()) (xs |> List.map (fun f -> f.ToString()) |> String.concat ", ") (leaf.Key)) leaf])
            <&&>
            FieldValidators.checkField p rule (leaf.ValueId.lower) (leaf.ValueText) (leaf) errors
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
                |s -> if List.exists (fun x -> s.MatchesScope x) xs then OK else Invalid [inv (ErrorCodes.ConfigRulesRuleWrongScope (s.ToString()) (xs |> List.map (fun f -> f.ToString()) |> String.concat ", ") ("")) valueclause])
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
            applyNodeRule true context options (ValueField (ValueType.Specific rootId)) rules node OK

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
                    |Some ((NodeRule ((ValueField (ValueType.Specific (x))), rs), o)) when (StringResource.stringManager.GetStringForID x.normal) == typedef.name->
                        if typekeyfilter typedef n.Key then applyNodeRuleRoot typedef rs o n else OK
                    |_ ->
                        OK
                let pathFilteredTypes = typedefs |> List.filter (fun t -> FieldValidators.checkPathDir t pathDir file)
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


        member this.ApplyNodeRule(rule, node) = applyNodeRule true {subtypes = []; scopes = defaultContext; warningOnly = false } CWTools.Rules.RulesParser.defaultOptions (ValueField (ValueType.Specific rootId)) rule node OK
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
                                         enums : Collections.Map<string, string * StringSet>, varMap : Collections.Map<string, StringSet>,
                                         localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>,
                                         links : Map<string,Effect<'T>,InsensitiveStringComparer>,
                                         valueTriggers : Map<string,Effect<'T>,InsensitiveStringComparer>,
                                         ruleApplicator : RuleApplicator<'T>, changeScope, defaultContext, anyScope, defaultLang) =
        let linkMap = links
        let wildCardLinks = linkMap.ToList() |> List.map snd |> List.choose (function | :? ScopedEffect<'T> as e when e.IsWildCard -> Some e |_ -> None )
        let valueTriggerMap = valueTriggers
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
                        |> Seq.fold (fun (na, la, lva, vca) r ->
                            match r with
                            | (NodeRule (l, rs), o) -> (l, rs, o)::na, la, lva, vca
                            | (LeafRule (l, r), o) -> na, (l, r, o)::la, lva, vca
                            | (LeafValueRule (lv), o) -> na, la,(lv, o)::lva, vca
                            | (ValueClauseRule (rs), o) -> na, la, lva, (rs, o)::vca
                            | _ -> na, la, lva, vca
                            ) ([], [], [], [])

            memoizeRulesInner memFunction

        let rec singleFoldRules fNode fChild fLeaf fLeafValue fValueClause fComment acc child rule :'r =
            let recurse = singleFoldRules fNode fChild fLeaf fLeafValue fValueClause fComment
            match child with
            |NodeC node ->
                let finalAcc = fNode acc node rule
                match fChild finalAcc (node :> IClause) rule with
                |Some (child, newRule) ->
                    recurse finalAcc child newRule
                |None -> finalAcc
            |ValueClauseC valueClause ->
                let finalAcc = fValueClause acc valueClause rule
                match fChild finalAcc (valueClause :> IClause) rule with
                | Some (child, newRule) ->
                    recurse finalAcc child newRule
                | None -> finalAcc
            |LeafC leaf ->
                fLeaf acc leaf rule
            |LeafValueC leafvalue ->
                fLeafValue acc leafvalue rule
            |CommentC comment ->
                fComment acc comment rule

        let rec foldRules fNode fChild fLeaf fLeafValue fValueClause fComment acc child rule :'r =
            let recurse = foldRules fNode fChild fLeaf fLeafValue fValueClause fComment
            match child with
            |NodeC node ->
                let finalAcc = fNode acc node rule
                fChild (node :> IClause) rule |> Seq.fold (fun a (c, r) -> recurse a c r) finalAcc
            |ValueClauseC valueClause ->
                let finalAcc = fValueClause acc valueClause rule
                fChild (valueClause :> IClause) rule |> Seq.fold (fun a (c, r) -> recurse a c r) finalAcc
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
        let foldWithPos fLeaf fLeafValue fComment fNode fValueClause acc (pos : pos) (node : Node) (logicalpath : string) =
            let fChild (ctx, _) (node : IClause) ((field, options) : NewRule<_>) =
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
                let childMatch = node.Nodes |> Seq.tryFind (fun c -> rangeContainsPos c.Position pos)
                let leafMatch = node.Leaves |> Seq.tryFind (fun l -> rangeContainsPos l.Position pos)
                let leafValueMatch = node.LeafValues |> Seq.tryFind (fun lv -> rangeContainsPos lv.Position pos)
                // log "child rs %A %A %A %A" (node.Key) childMatch leafMatch leafValueMatch
                // let ctx = { RuleContext.subtypes = []; scop es = defaultContext; warningOnly = false }
                let p = {
                    varMap = varMap
                    enumsMap = enumsMap
                    typesMap = typesMap
                    linkMap = linkMap
                    valueTriggerMap = valueTriggerMap
                    varSet = varSet
                    localisation = localisation
                    files = files
                    changeScope = changeScope
                    anyScope = anyScope
                    defaultLang = defaultLang
                    ctx = ctx
                    severity = Severity.Error
                    wildcardLinks = wildCardLinks
                }

                match childMatch, leafMatch, leafValueMatch with
                |Some c, _, _ ->
                    match expandedrules |> List.tryPick (function |(NodeRule (l, rs), o) when FieldValidators.checkLeftField p l c.KeyId.lower c.Key -> Some (l, rs, o) |_ -> None) with
                    | None ->
                            // log "fallback match %s %A" (node.Key) expandedrules
                            Some (NodeC c, (field, options))
                    | Some (l, rs, o) -> Some (NodeC c, ((NodeRule (l, rs)), o))
                |_, Some leaf, _ ->
                    match expandedrules |> List.tryPick (function |(LeafRule (l, r), o) when FieldValidators.checkLeftField p l leaf.KeyId.lower leaf.Key -> Some (l, r, o) |_ -> None) with
                    |None ->
                        Some (LeafC leaf, (field, options))
                    |Some (l, rs, o) -> Some (LeafC leaf, ((LeafRule (l, rs)), o))
                |_, _, Some lv -> Some (LeafValueC lv, (field, options))
                |None, None, None -> None
            let pathDir = (Path.GetDirectoryName logicalpath).Replace("\\","/")
            let file = Path.GetFileName logicalpath
            let childMatch = node.Children |> List.tryFind (fun c -> rangeContainsPos c.Position pos)
            //log "%O %A %A" pos pathDir (typedefs |> List.tryHead)
            match childMatch, typedefs |> List.tryFind (fun t -> FieldValidators.checkPathDir t pathDir file) with
            |Some c, Some typedef ->
                let typerules = typeRules |> List.filter (fun (name, _) -> name == typedef.name)
                match typerules with
                |[(n, (NodeRule (l, rs), o))] ->
                    Some (singleFoldRules fNode fChild fLeaf fLeafValue fValueClause fComment acc (NodeC c) ((NodeRule (TypeMarkerField (c.KeyId.lower, typedef), rs), o)))
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
            //TODO: Actually implement value clause
            let fValueClause (ctx, _) _ _ = ctx, (None, None, None)
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
                        match changeScope false true linkMap valueTriggerMap wildCardLinks varSet key scope with
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
                match childMatch, typedefs |> List.tryFind (fun t -> FieldValidators.checkPathDir t pathDir file) with
                |Some c, Some typedef ->
                    let pushScope, subtypes = ruleApplicator.TestSubType (typedef.subtypes, c)
                    match pushScope with
                    |Some ps -> { subtypes = subtypes; scopes = { Root = ps; From = []; Scopes = [ps] }; warningOnly = false}
                    |None -> { subtypes = subtypes; scopes = defaultContext; warningOnly = false }
                |_, _ -> { subtypes = []; scopes = defaultContext; warningOnly = false }

            let ctx = ctx, (None, None, None)
            foldWithPos fLeaf fLeafValue fComment fNode fValueClause ctx (pos) (entity.entity) (entity.logicalpath)


        let foldCollect fLeaf fLeafValue fComment fNode fValueClause acc (node : Node) (path: string) =
            let ctx = { subtypes = []; scopes = defaultContext; warningOnly = false  }
            let fChild (node : IClause) ((field, options) : NewRule<_>) =
                let rules =
                    match field with
                    | (NodeRule (_, rs)) -> rs
                    | _ -> []
                let noderules, leafrules, leafvaluerules, valueclauserules = memoizeRules rules ctx.subtypes
                let p = {
                    varMap = varMap
                    enumsMap = enumsMap
                    typesMap = typesMap
                    linkMap = linkMap
                    valueTriggerMap = valueTriggerMap
                    varSet = varSet
                    localisation = localisation
                    files = files
                    changeScope = changeScope
                    anyScope = anyScope
                    defaultLang = defaultLang
                    ctx = ctx
                    severity = Severity.Error
                    wildcardLinks = wildCardLinks
                }

                let inner (child : Child) =
                    match child with
                    | NodeC c ->
                        noderules |> Seq.choose (fun (l, rs, o) -> if FieldValidators.checkLeftField p l c.KeyId.lower c.Key then Some (NodeC c, ((NodeRule (l, rs)), o)) else None)
                    | ValueClauseC vc ->
                        valueclauserules |> Seq.map (fun (rs, o) -> ValueClauseC vc, ((ValueClauseRule (rs)), o))
                    | LeafC leaf ->
                        leafrules |> Seq.choose (fun (l, r, o) -> if FieldValidators.checkLeftField p l leaf.KeyId.lower leaf.Key then Some (LeafC leaf, ((LeafRule (l, r)), o)) else None)
                    | LeafValueC leafvalue ->
                        leafvaluerules |> Seq.choose (fun (lv, o) -> if FieldValidators.checkLeftField p lv leafvalue.ValueId.lower leafvalue.Key then  Some (LeafValueC leafvalue, ((LeafValueRule (lv)), o)) else None)
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
                    foldRules fNode fChild fLeaf fLeafValue fValueClause fComment a (NodeC c) (NodeRule (TypeMarkerField (c.KeyId.lower, typedef), rs), o))
            let pathFilteredTypes = typedefs |> List.filter (fun t -> FieldValidators.checkPathDir t pathDir file)
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
            let fValueClause (res) _ _ = res
            let fCombine a b = (a |> List.choose id) @ (b |> List.choose id)

            let ctx = typedefs |> List.fold (fun (a : Collections.Map<string, (string * range) list>) t -> a.Add(t.name, [])) Collections.Map.empty
            fLeaf, fLeafValue, fComment, fNode, fValueClause, ctx
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
            let fValueClause (res) _ _ = res

            fLeaf, fLeafValue, fComment, fNode, fValueClause, Map.empty
            //let res = foldCollect fLeaf fLeafValue fComment fNode ctx (entity.entity) (entity.logicalpath)
            //res

        let mergeFolds (l1, lv1, c1, n1, vc1, ctx1) (l2, lv2, c2, n2, vc2, ctx2) =
            let fLeaf = (fun (acc1, acc2) l r -> (l1 acc1 l r, l2 acc2 l r))
            let fLeafValue = (fun (acc1, acc2) lv r -> (lv1 acc1 lv r, lv2 acc2 lv r))
            let fNode = (fun (acc1, acc2) n r -> (n1 acc1 n r, n2 acc2 n r))
            let fComment = (fun (acc1, acc2) c r -> (c1 acc1 c r, c2 acc2 c r))
            let fValueClause = (fun (acc1, acc2) vc r -> (vc1 acc1 vc r, vc2 acc2 vc r))
            fLeaf, fLeafValue, fComment, fNode, fValueClause, (ctx1, ctx2)


        let getEffectsInEntity = //(ctx) (entity : Entity) =
            let fLeaf (res) (leaf : Leaf) ((field, _) : NewRule<_>) = res
            let fLeafValue (res) (leafvalue : LeafValue) (field, _) = res
            let fNode (res : Node list, finished: bool) (node : Node) ((field, option) : NewRule<_>) =
                match finished, field with
                |false, NodeRule (_, rs) when rs |> List.exists (function |LeafRule (AliasField "effect" , _), _-> true |_ -> false) ->
                    node::res, true
                |_ -> res, false
            let fComment (res) _ _ = res
            let fValueClause (res) (valueclause : ValueClause) ((field, option) : NewRule<_>) = res

            // let res = foldCollect fLeaf fLeafValue fComment fNode ctx (entity.entity) (entity.logicalpath)
            // res
            fLeaf, fLeafValue, fComment, fNode, fValueClause, ([], false)

        let getTriggersInEntity = //(ctx) (entity : Entity) =
            let fLeaf (res) (leaf : Leaf) ((field, _) : NewRule<_>) = res
            let fLeafValue (res) (leafvalue : LeafValue) (field, _) = res
            let fNode (res : Node list, finished : bool) (node : Node) ((field, option) : NewRule<_>) =
                match finished, field with
                |false, NodeRule (_, rs) when rs |> List.exists (function |LeafRule (AliasField "trigger" , _), _-> true |_ -> false) ->
                    node::res, true
                |_ -> res, false
            let fValueClause (res) (valueclause : ValueClause) ((field, option) : NewRule<_>) = res
            let fComment (res) _ _ = res

            fLeaf, fLeafValue, fComment, fNode, fValueClause, ([], false)
            // let res = foldCollect fLeaf fLeafValue fComment fNode ctx (entity.entity) (entity.logicalpath)
            // res
        let allFolds entity =
            let fLeaf, fLeafValue, fComment, fNode, fValueClause, ctx =
                mergeFolds getTriggersInEntity getEffectsInEntity
                |> mergeFolds getDefVarInEntity
                |> mergeFolds getTypesInEntity
            let types, (defvars, (triggers, effects)) = foldCollect fLeaf fLeafValue fComment fNode fValueClause ctx (entity.entity) (entity.logicalpath)
            (types, defvars, triggers, effects)
        let singleFold (fLeaf, fLeafValue, fComment, fNode, fValueClause, ctx) entity =
            foldCollect fLeaf fLeafValue fComment fNode fValueClause ctx (entity.entity) (entity.logicalpath)

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
                    then (FieldValidators.validateTypeLocalisation typedefs invertedTypeMap localisation t value leaf) <&&> res
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
                    then (FieldValidators.validateTypeLocalisation typedefs invertedTypeMap localisation t value leaf) <&&> res
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
                    then (FieldValidators.validateTypeLocalisation typedefs invertedTypeMap localisation t value leafvalue) <&&> res
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
                    then (FieldValidators.validateTypeLocalisation typedefs invertedTypeMap localisation t value node) <&&> res
                    else res
                |_ -> res

            let fComment (res) _ _ = res
            let fValueClause (res) _ _ = res
            let fCombine a b = (a |> List.choose id) @ (b |> List.choose id)

            let ctx = OK
            let res = foldCollect fLeaf fLeafValue fComment fNode fValueClause ctx (entity.entity) (entity.logicalpath)
            res



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
                         enums : Collections.Map<string, string * StringSet>, varMap : Collections.Map<string, StringSet>,
                         localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>,
                         links : Map<string,Effect<'T>,InsensitiveStringComparer>,
                         valueTriggers : Map<string,Effect<'T>,InsensitiveStringComparer>,
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


        let linkMap = links
        let wildCardLinks = linkMap.ToList() |> List.map snd |> List.choose (function | :? ScopedEffect<'T> as e when e.IsWildCard -> Some e |_ -> None )
        let valueTriggerMap = valueTriggers
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
            |ValueField v -> FieldValidators.getValidValues v |> Option.bind (List.tryHead) |> Option.defaultValue "x"
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
            let scopedEffects = linkMap.ToList() |> List.choose (fun (_, s) -> s |> function | :? ScopedEffect<'T> as x -> Some x.Name | _ -> None )
            evs @ gevs @ scopedEffects

        let createSnippetForClause (scoreFunction : string -> int) (rules : NewRule<_> list) (description : string option) (key : string) =
            let filterToCompletion =
                function
                |LeafRule(ValueField(ValueType.Specific _), _) -> true
                |NodeRule(ValueField(ValueType.Specific _), _) -> true
                |_ -> false
            let ruleToDistinctKey =
                function
                |LeafRule(ValueField(ValueType.Specific s), _) -> StringResource.stringManager.GetStringForID s.normal
                |NodeRule(ValueField(ValueType.Specific s), _) -> StringResource.stringManager.GetStringForID s.normal
                |_ -> ""

            let rulePrint (i : int) =
                function
                |LeafRule(ValueField(ValueType.Specific s), r) ->
                    sprintf "\t%s = ${%i:%s}\n" (StringResource.stringManager.GetStringForID s.normal) (i + 1) (fieldToCompletionList r)
                |NodeRule(ValueField(ValueType.Specific s), _) ->
                    sprintf "\t%s = ${%i:%s}\n" (StringResource.stringManager.GetStringForID s.normal) (i + 1) "{ }"
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
                        [createSnippetForClause innerRules o.description (StringResource.stringManager.GetStringForID s.normal)]
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
                        [keyvalue (StringResource.stringManager.GetStringForID s.normal)]
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
            let fieldToRules (field : NewField<'T>) =
                //log "%A" types
                //log "%A" field
                match field with
                |NewField.ValueField (Enum e) -> enums.TryFind(e) |> Option.map (fun (_, s) -> s.ToList()) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                |NewField.ValueField v -> FieldValidators.getValidValues v |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
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
                |NewField.ValueScopeField _ -> enums.TryFind("static_values") |> Option.map (fun (_, s) -> s.ToList()) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                |_ -> []
            let p = {
                varMap = varMap
                enumsMap = enumsMap
                typesMap = typesMap
                linkMap = linkMap
                valueTriggerMap = valueTriggerMap
                varSet = varSet
                localisation = localisation
                files = files
                changeScope = changeScope
                anyScope = anyScope
                defaultLang = defaultLang
                ctx = { subtypes = []; scopes = defaultContext; warningOnly = false }
                severity = Severity.Error
                wildcardLinks = wildCardLinks
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
                    match expandedRules |> List.choose (function | (NodeRule (l, rs), o) when FieldValidators.checkFieldByKey p l (StringResource.stringManager.InternIdentifierToken key).lower key -> Some (l, rs, o) | _ -> None) with
                    | [] -> expandedRules |> List.collect (convRuleToCompletion count scopeContext)
                    | fs -> fs |> List.collect (fun (_, innerRules, _) -> findRule innerRules rest scopeContext)
                | (key, count, Some _)::rest ->
                    match expandedRules |> List.choose (function | (LeafRule (l, r), o) when FieldValidators.checkFieldByKey p l (StringResource.stringManager.InternIdentifierToken key).lower key -> Some (l, r, o) | _ -> None) with
                    | [] -> expandedRules |> List.collect (convRuleToCompletion count scopeContext)
                    | fs ->
                        //log "%s %A" key fs
                        let res = fs |> List.collect (fun (_, f, _) -> fieldToRules f)
                        //log "res %A" res
                        res
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
            let pathFilteredTypes = typedefs |> List.filter (fun t -> FieldValidators.checkPathDir t pathDir file)
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
        member __.Complete(pos : pos, entity : Entity, scopeContext) = complete pos entity scopeContext

