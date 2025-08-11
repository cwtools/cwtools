namespace CWTools.Rules

open System.Collections.Frozen
open System.Collections.Generic
open CWTools.Rules
open CWTools.Process.Localisation
open CWTools.Process
open CWTools.Rules.RulesWrapper
open CWTools.Utilities.Utils
open CWTools.Utilities.Utils2
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Utilities
open CWTools.Common
open System.IO
open QuickGraph
open System
open CWTools.Process.Scopes
open CWTools.Games
open CWTools.Utilities.StringResource
open CSharpHelpers

// let inline ruleValidationServiceCreator(rootRules : RootRule<_> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>, enums : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<_>,InsensitiveStringComparer>, effects : Map<string,Effect<_>,InsensitiveStringComparer>, anyScope, changeScope, (defaultContext : ScopeContext<_>), checkLocField :( (Lang * Collections.Set<string> )list -> bool -> string -> _ -> ValidationResult)) =
// let inline ruleValidationServiceCreator(rootRules : RootRule< ^T> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>, enums : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<_>,InsensitiveStringComparer>, effects : Map<string,Effect<_>,InsensitiveStringComparer>, anyScope, changeScope, (defaultContext : ScopeContext<_>), defaultLang) =
type RuleValidationService
    (
        rootRules: RulesWrapper,
        typedefs: TypeDefinition list,
        types: FrozenDictionary<string, PrefixOptimisedStringSet>,
        enums: FrozenDictionary<string, string * PrefixOptimisedStringSet>,
        varMap: FrozenDictionary<string, PrefixOptimisedStringSet>,
        localisation: (Lang * Collections.Set<string>) list,
        files: Collections.Set<string>,
        links: EffectMap,
        valueTriggers: EffectMap,
        anyScope,
        changeScope: ChangeScope,
        defaultContext: ScopeContext,
        defaultLang,
        processLocalisation:
            Lang * Collections.Map<string, CWTools.Localisation.Entry> -> Lang * Collections.Map<string, LocEntry>,
        validateLocalisation: LocEntry -> ScopeContext -> ValidationResult
    ) =

    let mutable errorList: ResizeArray<CWError> = new ResizeArray<CWError>()
    let linkMap = links
    let valueTriggerMap = valueTriggers

    let typesMap = types //|> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))) |> Map.ofSeq
    let enumsMap = enums //|> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), s)) |> Map.ofSeq
    //let varMap = varMap |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), s)) |> Map.ofSeq
    let varSet =
        varMap.TryFind "variable" |> Option.defaultValue (PrefixOptimisedStringSet())

    let wildCardLinks =
        linkMap.Values
        |> Seq.choose (function
            | :? ScopedEffect as e when e.IsWildCard -> Some e
            | _ -> None)
        |> List.ofSeq

    let defaultKeys =
        localisation
        |> List.choose (fun (l, ks) -> if l = defaultLang then Some ks else None)
        |> List.tryHead
        |> Option.defaultValue Set.empty

    let localisationKeys =
        localisation
        |> List.choose (fun (l, ks) -> if l = defaultLang then None else Some(l, ks))

    let ruleToCompletionListHelper =
        function
        | LeafRule(SpecificField(SpecificValue x), _), _ -> seq { yield x.lower }
        | NodeRule(SpecificField(SpecificValue x), _), _ -> seq { yield x.lower }
        | LeafRule(NewField.TypeField(TypeType.Simple t), _), _
        | NodeRule(NewField.TypeField(TypeType.Simple t), _), _ ->
            types.TryFind(t)
            |> Option.map (fun s -> s.IdValues |> Seq.map _.lower)
            |> Option.defaultValue (Seq.empty)
        | LeafRule(NewField.TypeField(TypeType.Complex(p, t, suff)), _), _
        | NodeRule(NewField.TypeField(TypeType.Complex(p, t, suff)), _), _ ->
            types.TryFind(t)
            |> Option.map (fun s ->
                s.IdValues
                |> Seq.map (fun i ->
                    let s = stringManager.GetStringForID i.normal
                    stringManager.InternIdentifierToken(p + s + suff).lower))
            |> Option.defaultValue Seq.empty
        | LeafRule(NewField.ValueField(Enum e), _), _
        | NodeRule(NewField.ValueField(Enum e), _), _ ->
            enums.TryFind(e)
            |> Option.map (fun (_, s) -> s.IdValues |> Seq.map _.lower)
            |> Option.defaultValue Seq.empty
        | _ -> Seq.empty


    let aliasKeyMap =
        rootRules.Aliases
        |> Map.toList
        |> List.map (fun (key, rules) -> key, (rules |> Seq.collect ruleToCompletionListHelper |> HashSet<StringToken>))
        |> Map.ofList

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
    let mutable i = 0

    let memoizeRulesInner memFunction =
        let dict =
            new System.Collections.Concurrent.ConcurrentDictionary<_, Dictionary<_, _>>()

        fun (rules: NewRule array) (subtypes: string list) ->
            match dict.TryGetValue(rules) with
            | true, v ->
                match v.TryGetValue(subtypes) with
                | true, v2 -> v2
                | _ ->
                    let temp = memFunction rules subtypes

                    lock monitor (fun () ->
                        if v.ContainsKey(subtypes) then
                            ()
                        else
                            v.Add(subtypes, temp))

                    temp
            | _ ->
                let temp = memFunction rules subtypes
                let innerDict = new System.Collections.Generic.Dictionary<_, _>()

                lock monitor (fun () ->
                    innerDict.Add(subtypes, temp)

                    match dict.TryGetValue(rules) with
                    | true, v2 -> ()
                    | _ -> dict.TryAdd(rules, innerDict) |> ignore)
                // i <- i + 1
                // eprintfn "%i %i" i (((rules) :> Object).GetHashCode())
                temp

    let memoizeRules =
        let memFunction =
            fun rules subtypes ->
                let subtypedrules =
                    rules
                    |> Array.collect (fun (r, o) ->
                        r
                        |> (function
                        | SubtypeRule(key, shouldMatch, cfs) ->
                            (if (not shouldMatch) <> List.contains key subtypes then
                                 cfs
                             else
                                 [||])
                        | x -> [||]))

                let expandedbaserules =
                    rules
                    |> Array.collect (function
                        | LeafRule(AliasField a, _), _ -> (rootRules.Aliases.TryFind a |> Option.defaultValue [||])
                        | NodeRule(AliasField a, _), _ -> (rootRules.Aliases.TryFind a |> Option.defaultValue [||])
                        | x -> [||])

                let expandedsubtypedrules =
                    subtypedrules
                    |> Array.collect (function
                        | LeafRule(AliasField a, _), _ -> (rootRules.Aliases.TryFind a |> Option.defaultValue [||])
                        | NodeRule(AliasField a, _), _ -> (rootRules.Aliases.TryFind a |> Option.defaultValue [||])
                        | x -> [||])
                // let res = expandedsubtypedrules @ subtypedrules @ rules @ expandedbaserules
                // let res = expandedsubtypedrules @ subtypedrules @ rules @ expandedbaserules
                let noderules = new ResizeArray<_>()
                let leafrules = new ResizeArray<_>()
                let leafvaluerules = new ResizeArray<_>()
                let valueclauserules = new ResizeArray<_>()
                let nodeSpecificMap = new Dictionary<_, _>()
                let leafSpecificMap = new Dictionary<_, _>()

                let inner =
                    (fun r ->
                        match r with
                        | NodeRule(SpecificField(SpecificValue v), rs), o as x ->
                            let found, res = nodeSpecificMap.TryGetValue(v.lower)

                            if found then
                                nodeSpecificMap.[v.lower] <- x :: res
                            else
                                nodeSpecificMap.[v.lower] <- [ x ]
                        | NodeRule(l, rs), o as x -> noderules.Add(x)
                        | LeafRule(SpecificField(SpecificValue v), r), o as x ->
                            let found, res = leafSpecificMap.TryGetValue(v.lower)

                            if found then
                                leafSpecificMap.[v.lower] <- x :: res
                            else
                                leafSpecificMap.[v.lower] <- [ x ]
                        | LeafRule(l, r), o as x -> leafrules.Add(x)
                        | LeafValueRule lv, o as x -> leafvaluerules.Add(x)
                        | ValueClauseRule rs, o as x -> valueclauserules.Add(x)
                        | _ -> ())
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

    let p =
        { varMap = varMap
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
          aliasKeyList = aliasKeyMap
          processLocalisation = processLocalisation
          validateLocalisation = validateLocalisation }

    let checkQuotes (quoted: bool) (optionRequiredQuotes: bool) = quoted || (not optionRequiredQuotes)


    let rec applyClauseField
        (enforceCardinality: bool)
        (nodeSeverity: Severity option)
        (ctx: RuleContext)
        (rules: NewRule array)
        (startNode: IClause)
        errors
        =
        let severity =
            nodeSeverity
            |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)
        // TODO: Memoize expanded rules depending  on ctx.subtypes ad rules?
        let noderules, leafrules, leafvaluerules, valueclauserules, nodeSpecificDict, leafSpecificDict =
            memoizeRules rules ctx.subtypes

        let inline valueFun innerErrors (leaf: Leaf) =
            let key = leaf.Key
            let keyIds = leaf.KeyId

            let inline createDefault () =
                if enforceCardinality && (leaf.Key.[0] <> '@') then
                    inv
                        (ErrorCodes.ConfigRulesUnexpectedPropertyNode
                            $"%s{key} is unexpected in %s{startNode.Key}"
                            severity)
                        leaf
                    <&&&> innerErrors

                else
                    innerErrors

            let found, value = leafSpecificDict.TryGetValue keyIds.lower

            let rs =
                if found then
                    seq {
                        yield! value
                        yield! leafrules
                    }
                else
                    upcast leafrules

            rs
            |> Seq.filter (function
                | LeafRule(l, _), o ->
                    checkQuotes leaf.KeyId.quoted o.keyRequiredQuotes
                    && FieldValidators.checkLeftField p Severity.Error ctx l keyIds
                | _ -> false)
            |> (fun rs ->
                lazyErrorMerge
                    rs
                    (function
                    | (LeafRule(l, r), o) -> applyLeafRule ctx o l r leaf
                    | _ -> failwith "Unexpected")
                    createDefault
                    innerErrors
                    true)

        let inline nodeFun innerErrors (node: Node) =
            let key = node.Key
            let keyIds = node.KeyId

            let createDefault () =
                if enforceCardinality then
                    inv
                        (ErrorCodes.ConfigRulesUnexpectedPropertyLeaf
                            $"%s{key} is unexpected in %s{startNode.Key}"
                            severity)
                        node
                    <&&&> innerErrors
                else
                    innerErrors

            let found, value = nodeSpecificDict.TryGetValue keyIds.lower

            let rs =
                if found then
                    seq {
                        yield! value
                        yield! noderules
                    }
                else
                    upcast noderules

            rs
            |> Seq.filter (function
                | NodeRule(l, rs), o ->
                    checkQuotes node.KeyId.quoted o.keyRequiredQuotes
                    && FieldValidators.checkLeftField p Severity.Error ctx l keyIds
                | _ -> false)
            |> (fun rs ->
                lazyErrorMerge
                    rs
                    (function
                    | (NodeRule(l, r), o) -> applyNodeRule enforceCardinality ctx o l r node
                    | _ -> failwith "Unexpected")
                    createDefault
                    innerErrors
                    false)

        let inline leafValueFun innerErrors (leafvalue: LeafValue) =
            let createDefault () =
                if
                    enforceCardinality
                    && not (stringManager.GetMetadataForID leafvalue.ValueId.lower).startsWithSquareBracket
                then
                    inv
                        (ErrorCodes.ConfigRulesUnexpectedPropertyLeafValue
                            $"%s{leafvalue.Key} is unexpected in %s{startNode.Key}"
                            severity)
                        leafvalue
                    <&&&> innerErrors
                else
                    innerErrors

            leafvaluerules
            |> Seq.filter (function
                | LeafValueRule l, o -> FieldValidators.checkLeftField p Severity.Error ctx l leafvalue.ValueId
                | _ -> false)
            |> (fun rs ->
                lazyErrorMerge
                    rs
                    (function
                    | (LeafValueRule l, o) -> applyLeafValueRule ctx o l leafvalue
                    | _ -> failwith "Unexpected")
                    createDefault
                    innerErrors
                    true)

        let inline valueClauseFun innerErrors (valueclause: ValueClause) =
            let createDefault () =
                if enforceCardinality then
                    inv
                        (ErrorCodes.ConfigRulesUnexpectedPropertyValueClause
                            $"Unexpected clause in %s{startNode.Key}"
                            severity)
                        valueclause
                    <&&&> innerErrors
                else
                    innerErrors

            valueclauserules
            |> (fun rs ->
                lazyErrorMerge
                    rs
                    (function
                    | (ValueClauseRule r, o) -> applyValueClauseRule enforceCardinality ctx o r valueclause
                    | _ -> failwith "Unexpected")
                    createDefault
                    innerErrors
                    true)

        let inline checkCardinality (clause: IClause) innerErrors (rule: NewRule) =
            match rule with
            | NodeRule(SpecificField(SpecificValue key), _), opts
            | LeafRule(SpecificField(SpecificValue key), _), opts ->
                let leafcount =
                    clause.Leaves
                    |> Seq.sumBy (fun leaf -> if leaf.KeyId.lower = key.lower then 1 else 0)

                let childcount =
                    clause.Nodes
                    |> Seq.sumBy (fun child -> if child.KeyId.lower = key.lower then 1 else 0)

                let total = leafcount + childcount

                if opts.min > total then
                    let minSeverity =
                        if opts.strictMin then
                            (opts.severity |> Option.defaultValue severity)
                        else
                            Severity.Warning

                    inv
                        (ErrorCodes.ConfigRulesWrongNumber
                            $"Missing %s{StringResource.stringManager.GetStringForID key.normal}, expecting at least %i{opts.min}"
                            minSeverity)
                        clause
                    <&&&> innerErrors
                else if opts.max < total then
                    inv
                        (ErrorCodes.ConfigRulesWrongNumber
                            $"Too many %s{stringManager.GetStringForID key.normal}, expecting at most %i{opts.max}"
                            Severity.Warning)
                        clause
                    <&&&> innerErrors
                else
                    innerErrors
            | NodeRule(AliasField _, _), _
            | LeafRule(AliasField _, _), _
            | LeafValueRule(AliasField _), _ -> innerErrors
            | NodeRule(l, _), opts ->
                let total =
                    clause.Nodes
                    |> Seq.sumBy (fun child ->
                        if FieldValidators.checkLeftField p Severity.Error ctx l child.KeyId then
                            1
                        else
                            0)

                if opts.min > total then
                    let minSeverity =
                        if opts.strictMin then
                            (opts.severity |> Option.defaultValue severity)
                        else
                            Severity.Warning

                    inv
                        (ErrorCodes.ConfigRulesWrongNumber $"Missing {l}, expecting at least %i{opts.min}" minSeverity)
                        clause
                    <&&&> innerErrors
                else if opts.max < total then
                    inv
                        (ErrorCodes.ConfigRulesWrongNumber
                            $"Too many n {l}, expecting at most %i{opts.max}"
                            Severity.Warning)
                        clause
                    <&&&> innerErrors
                else
                    innerErrors
            | LeafRule(l, r), opts ->
                let total =
                    clause.Leaves
                    |> Seq.sumBy (fun leaf ->
                        if FieldValidators.checkLeftField p Severity.Error ctx l leaf.KeyId then
                            1
                        else
                            0)

                if opts.min > total then
                    let minSeverity =
                        if opts.strictMin then
                            (opts.severity |> Option.defaultValue severity)
                        else
                            Severity.Warning

                    inv
                        (ErrorCodes.ConfigRulesWrongNumber $"Missing {l}, expecting at least %i{opts.min}" minSeverity)
                        clause
                    <&&&> innerErrors
                else if opts.max < total then
                    inv
                        (ErrorCodes.ConfigRulesWrongNumber
                            $"Too many l {l} {r}, expecting at most %i{opts.max}"
                            Severity.Warning)
                        clause
                    <&&&> innerErrors
                else
                    innerErrors
            | LeafValueRule(l), opts ->
                let total =
                    clause.LeafValues
                    |> Seq.sumBy (fun leafValue ->
                        if FieldValidators.checkLeftField p Severity.Error ctx l leafValue.ValueId then
                            1
                        else
                            0)

                if opts.min > total then
                    let minSeverity =
                        if opts.strictMin then
                            (opts.severity |> Option.defaultValue severity)
                        else
                            Severity.Warning

                    inv
                        (ErrorCodes.ConfigRulesWrongNumber $"Missing {l}, expecting at least %i{opts.min}" minSeverity)
                        clause
                    <&&&> innerErrors
                else if opts.max < total then
                    inv
                        (ErrorCodes.ConfigRulesWrongNumber
                            $"Too many lv {l}, expecting at most %i{opts.max}"
                            Severity.Warning)
                        clause
                    <&&&> innerErrors
                else
                    innerErrors
            | ValueClauseRule _, opts ->
                let total = clause.ValueClauses |> Seq.length

                if opts.min > total then
                    let minSeverity =
                        if opts.strictMin then
                            (opts.severity |> Option.defaultValue severity)
                        else
                            Severity.Warning

                    inv
                        (ErrorCodes.ConfigRulesWrongNumber
                            $"Missing clause, expecting at least %i{opts.min}"
                            minSeverity)
                        clause
                    <&&&> innerErrors
                else if opts.max < total then
                    inv
                        (ErrorCodes.ConfigRulesWrongNumber
                            $"Too many clauses, expecting at most %i{opts.max}"
                            Severity.Warning)
                        clause
                    <&&&> innerErrors
                else
                    innerErrors
            | _ -> innerErrors

        (applyToAll startNode.Leaves valueFun errors)
        |> (applyToAll startNode.Nodes nodeFun)
        |> (applyToAll startNode.LeafValues leafValueFun)
        |> (applyToAll startNode.ValueClauses valueClauseFun)
        |> (applyToAll rules (checkCardinality startNode))

    and applyValueField severity (vt: CWTools.Rules.ValueType) (leaf: Leaf) =
        FieldValidators.checkValidValue varMap enumsMap localisation severity vt leaf.ValueId leaf

    and applyLeafValueRule (ctx: RuleContext) (options: Options) (rule: NewField) (leafvalue: LeafValue) errors =
        let severity =
            options.severity
            |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)
        // let errors = OK
        FieldValidators.checkField p severity ctx rule leafvalue.ValueId leafvalue errors
        <&&> (if checkQuotes leafvalue.ValueId.quoted options.valueRequiredQuotes then
                  OK
              else
                  Invalid(
                      Guid.NewGuid(),
                      [ inv (ErrorCodes.CustomError "This value is expected to be quoted" Severity.Error) leafvalue ]
                  ))

    and applyLeafRule
        (ctx: RuleContext)
        (options: Options)
        (leftRule: NewField)
        (rightRule: NewField)
        (leaf: Leaf)
        errors
        =
        ((FieldValidators.checkField p Severity.Error ctx leftRule leaf.KeyId leaf OK)
         <&%&> (let severity =
                    options.severity
                    |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)
                // let errors = OK
                (match options.requiredScopes with
                 | [] -> OK
                 | xs ->
                     match ctx.scopes.CurrentScope with
                     | x when x = anyScope -> OK
                     | s ->
                         if List.exists (fun x -> s.IsOfScope x) xs then
                             OK
                         else
                             Invalid(
                                 Guid.NewGuid(),
                                 [ inv
                                       (ErrorCodes.ConfigRulesRuleWrongScope
                                           (s.ToString())
                                           (xs |> List.map (fun f -> f.ToString()) |> String.concat ", ")
                                           leaf.Key)
                                       leaf ]
                             ))
                <&&> (if checkQuotes leaf.ValueId.quoted options.valueRequiredQuotes then
                          OK
                      else
                          Invalid(
                              Guid.NewGuid(),
                              [ inv (ErrorCodes.CustomError "This value is expected to be quoted" Severity.Error) leaf ]
                          ))
                <&&> (if options.errorIfOnlyMatch.IsSome then
                          let res = FieldValidators.checkField p severity ctx rightRule leaf.ValueId leaf OK

                          if res = OK then
                              inv (ErrorCodes.FromRulesCustomError options.errorIfOnlyMatch.Value severity) leaf
                              <&&&> OK
                          else
                              res
                      else
                          FieldValidators.checkField p severity ctx rightRule leaf.ValueId leaf OK)))
        <&&> errors

    and applyNodeRule
        (enforceCardinality: bool)
        (ctx: RuleContext)
        (options: Options)
        (rule: NewField)
        (rules: NewRule array)
        (node: IClause)
        errors
        =
        let severity =
            options.severity
            |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)

        let newCtx =
            match options.pushScope with
            | Some ps ->
                { ctx with
                    scopes =
                        { ctx.scopes with
                            Scopes = ps :: ctx.scopes.Scopes } }
            | None ->
                match options.replaceScopes with
                | Some rs ->
                    let prevctx =
                        match rs.prevs with
                        | Some prevs ->
                            { ctx with
                                scopes = { ctx.scopes with Scopes = prevs } }
                        | None -> ctx

                    let newctx =
                        match rs.this, rs.froms with
                        | Some this, Some froms ->
                            { prevctx with
                                scopes =
                                    { prevctx.scopes with
                                        Scopes = this :: prevctx.scopes.PopScope
                                        From = froms } }
                        | Some this, None ->
                            { prevctx with
                                scopes =
                                    { prevctx.scopes with
                                        Scopes = this :: prevctx.scopes.PopScope } }
                        | None, Some froms ->
                            { prevctx with
                                scopes = { prevctx.scopes with From = froms } }
                        | None, None -> prevctx

                    match rs.root with
                    | Some root ->
                        { newctx with
                            scopes = { newctx.scopes with Root = root } }
                    | None -> newctx
                | None -> ctx

        let oldErrors = errors
        let errors = OK

        let newErrors =
            (match options.requiredScopes with
             | [] -> OK
             | xs ->
                 match ctx.scopes.CurrentScope with
                 | x when x = anyScope -> OK
                 | s ->
                     if List.exists (fun x -> s.IsOfScope x) xs then
                         OK
                     else
                         Invalid(
                             Guid.NewGuid(),
                             [ inv
                                   (ErrorCodes.ConfigRulesRuleWrongScope
                                       (s.ToString())
                                       (xs |> List.map (fun f -> f.ToString()) |> String.concat ", ")
                                       node.Key)
                                   node ]
                         ))
            <&&> match rule with
                 | IgnoreField _ -> OK
                 | ScopeField s ->
                     let scope = newCtx.scopes
                     let key = node.Key.Trim('"')

                     match
                         changeScope false true linkMap valueTriggerMap wildCardLinks varSet key scope,
                         (stringManager.GetMetadataForID node.KeyId.lower).containsDoubleDollar
                     with
                     | _, true ->
                         let newCtx =
                             { newCtx with
                                 scopes =
                                     { newCtx.scopes with
                                         Scopes = anyScope :: newCtx.scopes.Scopes } }

                         applyClauseField enforceCardinality options.severity newCtx rules node errors
                     | NewScope(newScopes, _, _), _ ->
                         let newCtx = { newCtx with scopes = newScopes }
                         applyClauseField enforceCardinality options.severity newCtx rules node errors
                     | NotFound, _ -> inv (ErrorCodes.ConfigRulesInvalidScopeCommand key) node <&&&> errors
                     | WrongScope(command, prevscope, expected, _), _ ->
                         inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) $"%A{expected}") node
                         <&&&> errors
                     | VarFound, _ ->
                         let newCtx =
                             { newCtx with
                                 scopes =
                                     { newCtx.scopes with
                                         Scopes = anyScope :: newCtx.scopes.Scopes } }

                         applyClauseField enforceCardinality options.severity newCtx rules node errors
                     | VarNotFound v, _ ->
                         inv (ErrorCodes.CustomError $"The variable %s{v} has not been set" Severity.Error) node
                         <&&&> errors
                     | _ ->
                         inv (ErrorCodes.CustomError "Something went wrong with this scope change" Severity.Hint) node
                         <&&&> errors
                 | _ -> applyClauseField enforceCardinality options.severity newCtx rules node errors

        match newErrors, node.Trivia |> Option.bind (_.originalSource) with
        | OK, _ -> oldErrors
        | newErrors, None -> oldErrors <&&> newErrors
        | Invalid(guid, cwErrors), Some source ->
            let updatedErrors =
                cwErrors
                |> List.map (fun e ->
                    { e with
                        relatedErrors =
                            Some
                                [ { location = source
                                    message = "Related source" } ] })

            let markerError =
                invManual (ErrorCodes.InlineScriptResultsInError severity) source "inline_script" None

            let markerError =
                { markerError with
                    relatedErrors =
                        Some(
                            updatedErrors
                            |> List.map (fun x ->
                                { location = x.range
                                  message = x.message })
                        ) }

            oldErrors <&&> Invalid(Guid.NewGuid(), markerError :: updatedErrors)

    and applyValueClauseRule
        (enforceCardinality: bool)
        (ctx: RuleContext)
        (options: Options)
        (rules: NewRule array)
        (valueclause: ValueClause)
        errors
        =
        let severity =
            options.severity
            |> Option.defaultValue (if ctx.warningOnly then Severity.Warning else Severity.Error)

        let newCtx =
            match options.pushScope with
            | Some ps ->
                { ctx with
                    scopes =
                        { ctx.scopes with
                            Scopes = ps :: ctx.scopes.Scopes } }
            | None ->
                match options.replaceScopes with
                | Some rs ->
                    let prevctx =
                        match rs.prevs with
                        | Some prevs ->
                            { ctx with
                                scopes = { ctx.scopes with Scopes = prevs } }
                        | None -> ctx

                    let newctx =
                        match rs.this, rs.froms with
                        | Some this, Some froms ->
                            { prevctx with
                                scopes =
                                    { prevctx.scopes with
                                        Scopes = this :: prevctx.scopes.PopScope
                                        From = froms } }
                        | Some this, None ->
                            { prevctx with
                                scopes =
                                    { prevctx.scopes with
                                        Scopes = this :: prevctx.scopes.PopScope } }
                        | None, Some froms ->
                            { prevctx with
                                scopes = { prevctx.scopes with From = froms } }
                        | None, None -> prevctx

                    match rs.root with
                    | Some root ->
                        { newctx with
                            scopes = { newctx.scopes with Root = root } }
                    | None -> newctx
                | None -> ctx

        (match options.requiredScopes with
         | [] -> OK
         | xs ->
             match ctx.scopes.CurrentScope with
             | x when x = anyScope -> OK
             | s ->
                 if List.exists (fun x -> s.IsOfScope x) xs then
                     OK
                 else
                     Invalid(
                         Guid.NewGuid(),
                         [ inv
                               (ErrorCodes.ConfigRulesRuleWrongScope
                                   (s.ToString())
                                   (xs |> List.map (fun f -> f.ToString()) |> String.concat ", ")
                                   "")
                               valueclause ]
                     ))
        <&&> applyClauseField enforceCardinality options.severity newCtx rules valueclause errors

    let testSubtype (subtypes: SubTypeDefinition list) (node: IClause) =
        let results =
            subtypes
            |> List.filter (fun st ->
                st.typeKeyField
                |> function
                    | Some tkf -> tkf == node.Key
                    | None -> true)
            |> List.filter (fun st ->
                st.startsWith
                |> function
                    | Some sw -> node.Key.StartsWith(sw, StringComparison.OrdinalIgnoreCase)
                    | None -> true)
            |> List.map (fun s ->
                s,
                if s.rules.Length = 0 then
                    OK
                else
                    applyClauseField
                        false
                        None
                        { subtypes = []
                          scopes = defaultContext
                          warningOnly = false }
                        s.rules
                        node
                        OK)

        let res =
            results
            |> List.choose (fun (s, res) ->
                res
                |> function
                    | Invalid _ -> None
                    | OK -> Some s)

        let allSubtypes = res |> List.map (fun s -> s.name)

        let checkOnlyNotIf (s: SubTypeDefinition) =
            s.onlyIfNot |> List.exists (fun s2 -> List.contains s2 allSubtypes) |> not

        let res = res |> List.filter checkOnlyNotIf
        let firstPushScope = res |> List.tryPick (fun s -> s.pushScope)
        firstPushScope, res |> List.map (fun s -> s.name)

    let rootId = stringManager.InternIdentifierToken "root"

    let applyNodeRuleRoot (typedef: TypeDefinition) (rules: NewRule array) (options: Options) (node: IClause) =
        let pushScope, subtypes = testSubtype typedef.subtypes node

        let startingScopeContext =
            match Option.orElse pushScope options.pushScope, options.replaceScopes with
            | Some ps, _ ->
                { Root = ps
                  From = []
                  Scopes = [ ps ] }
            | _, Some rs ->
                let replaceContext =
                    { Root = rs.root |> Option.orElse rs.this |> Option.defaultValue anyScope
                      From = rs.froms |> Option.defaultValue []
                      Scopes = rs.prevs |> Option.defaultValue [] }

                if rs.this |> Option.isSome then
                    { replaceContext with
                        Scopes = rs.this.Value :: replaceContext.Scopes }
                else
                    replaceContext
            | None, None -> defaultContext

        let context =
            { subtypes = subtypes
              scopes = startingScopeContext
              warningOnly = typedef.warningOnly }

        applyNodeRule true context options (SpecificField(SpecificValue rootId)) rules node OK

    let rootTypeDefs = typedefs |> List.filter (fun td -> td.type_per_file)
    let normalTypeDefs = typedefs |> List.filter (fun td -> td.type_per_file |> not)

    let validate ((path, root): string * Node) =
        let skiprootkey (skipRootKey: SkipRootKey) (n: IClause) =
            match skipRootKey with
            | SpecificKey key -> n.Key == key
            | AnyKey -> true
            | MultipleKeys(keys, shouldMatch) -> (keys |> List.exists ((==) n.Key)) <> (not shouldMatch)

        let directory = Path.GetDirectoryName(path).Replace('\\', '/')
        let fileName = Path.GetFileName(path)

        let inner (typedefs: TypeDefinition list) (node: IClause) =
            let validateType (typedef: TypeDefinition) (n: IClause) =
                let typerules =
                    rootRules.TypeRules
                    |> Seq.choose (function
                        | name, r when name == typedef.name -> Some r
                        | _ -> None)
                let filterKey =
                    match n with
                    | :? ValueClause as vc -> vc.FirstKey |> Option.defaultValue "clause"
                    | _ -> n.Key

                let prefixKey =
                    match n with
                    | :? Node as n -> n.KeyPrefix
                    | _ -> None

                match typerules |> Seq.tryHead with
                | Some(NodeRule(SpecificField(SpecificValue x), rs), o) when
                    (stringManager.GetStringForID x.normal) == typedef.name
                    ->
                    if FieldValidators.typekeyfilter typedef filterKey prefixKey then
                        applyNodeRuleRoot typedef rs o n
                    else
                        OK
                | _ -> OK

            let pathFilteredTypes =
                typedefs
                |> List.filter (fun t ->
                    FieldValidatorsHelper.CheckPathDir(t.pathOptions, directory, fileName))

            let rec validateTypeSkipRoot (t: TypeDefinition) (skipRootKeyStack: SkipRootKey list) (n: IClause) =
                let prefixKey =
                    match n with
                    | :? Node as n -> n.KeyPrefix
                    | _ -> None

                match skipRootKeyStack with
                | [] ->
                    if FieldValidators.typekeyfilter t n.Key prefixKey then
                        validateType t n
                    else
                        OK
                | head :: tail ->
                    if skiprootkey head n then
                        n.ClauseList <&!&> validateTypeSkipRoot t tail
                    else
                        OK

            pathFilteredTypes <&!&> (fun t -> validateTypeSkipRoot t t.skipRootKey node)

        let res = (root.Clauses |> List.ofSeq <&!&> inner normalTypeDefs)
        let rootres = (inner rootTypeDefs root)
        res <&&> rootres


    member this.ApplyNodeRule(rule, node) =
        applyNodeRule
            true
            { subtypes = []
              scopes = defaultContext
              warningOnly = false }
            CWTools.Rules.RulesParser.defaultOptions
            (SpecificField(SpecificValue rootId))
            rule
            node
            OK

    member this.TestSubType(subtypes, node) = testSubtype subtypes node

    member this.RuleValidate() =
        (fun _ (es: EntitySet<_>) ->
            es.Raw |> List.map (fun struct (e, _) -> e.logicalpath, e.entity)
            <&!!&> validate)

    member this.RuleValidateEntity = (fun e -> validate (e.logicalpath, e.entity))
    member this.ManualRuleValidate = validate
