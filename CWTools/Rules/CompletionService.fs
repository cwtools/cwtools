namespace CWTools.Rules

open System.Collections.Generic
open System.IO
open CSharpHelpers
open CWTools.Common
open CWTools.Rules.RulesWrapper
open CWTools.Utilities.Utils2
open CWTools.Utilities.Utils
open CWTools.Process.Localisation
open CWTools.Process
open CWTools.Process.Scopes
open CWTools.Process.ProcessCore
open CWTools.Utilities
open System
open CWTools.Games
open CWTools.Utilities.Position
open CWTools.Utilities.StringResource

type CompletionContext =
    | NodeLHS
    | NodeRHS
    | LeafLHS
    | LeafRHS


type private CompletionScopeOutput =
    | Variable
    | Value
    | Scope of scope: Scope
    | Nothing

type private CompletionScopeExpectation =
    | VariableOrValue
    | Scopes of scopes: Scope list
    | Nothing

type private CompletionLinkItem =
    { key: string
      requiredScopes: Scope list
      outputScope: CompletionScopeOutput
      desc: string option
      kind: CompletionCategory }


type CompletionService
    (
        rootRules: RulesWrapper,
        typedefs: TypeDefinition list,
        types: Collections.Map<string, PrefixOptimisedStringSet>,
        enums: Collections.Map<string, string * PrefixOptimisedStringSet>,
        varMap: Collections.Map<string, PrefixOptimisedStringSet>,
        localisation: (Lang * Collections.Set<string>) list,
        files: Collections.Set<string>,
        links: EffectMap,
        valueTriggers: EffectMap,
        globalScriptVariables: string list,
        changeScope: ChangeScope,
        defaultContext: ScopeContext,
        anyScope,
        oneToOneScopes,
        defaultLang,
        dataTypes: CWTools.Parser.DataTypeParser.JominiLocDataTypes,
        processLocalisation:
            Lang * Collections.Map<string, CWTools.Localisation.Entry> -> Lang * Collections.Map<string, LocEntry>,
        validateLocalisation: LocEntry -> ScopeContext -> CWTools.Validation.ValidationResult
    ) =

    let typesMap = types //|> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))) |> Map.ofSeq

    //let typesMap = types |> (Map.map (fun _ s -> StringSet.Create(InsensitiveStringComparer(), (s |> List.map fst))))
    let enumsMap = enums // |> Map.toSeq |> PSeq.map (fun (k, s) -> k, StringSet.Create(InsensitiveStringComparer(), s)) |> Map.ofSeq
    let types = types |> Map.map (fun _ s -> s.StringValues |> List.ofSeq)

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
            typesMap.TryFind(t)
            |> Option.map (fun s -> s.IdValues |> Seq.map _.lower)
            |> Option.defaultValue (Seq.empty)
        | LeafRule(NewField.TypeField(TypeType.Complex(p, t, suff)), _), _
        | NodeRule(NewField.TypeField(TypeType.Complex(p, t, suff)), _), _ ->
            typesMap.TryFind(t)
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

    let linkMap = links

    let wildCardLinks =
        linkMap.Values
        |> Seq.choose (function
            | :? ScopedEffect as e when e.IsWildCard -> Some e
            | _ -> None)
        |> Seq.toList

    let valueTriggerMap = valueTriggers

    let varSet =
        varMap.TryFind "variable" |> Option.defaultValue (PrefixOptimisedStringSet())

    let getAllKeysInFile (root: Node) =
        let fNode =
            (fun (x: Node) acc ->
                let withValues =
                    x.Values |> List.fold (fun a leaf -> leaf.Key :: leaf.ValueText :: a) acc

                let withBoth =
                    x.LeafValues
                    |> Seq.fold (fun a leafvalue -> leafvalue.ValueText :: a) withValues

                x.Key :: withBoth)

        foldNode7 fNode root

    let fieldToCompletionList (field: NewField) =
        match field with
        | ValueField(Enum e) ->
            enums.TryFind(e)
            |> Option.bind (fun (_, s) ->
                if s.Count = 0 then
                    None
                else
                    Some(s.StringValues |> Seq.head))
            |> Option.defaultValue "x"
        | ValueField v ->
            FieldValidators.getValidValues v
            |> Option.bind List.tryHead
            |> Option.defaultValue "x"
        | TypeField(TypeType.Simple t) -> types.TryFind(t) |> Option.bind List.tryHead |> Option.defaultValue "x"
        | TypeField(TypeType.Complex(p, t, s)) ->
            types.TryFind(t)
            |> Option.bind List.tryHead
            |> Option.map (fun n -> p + n + s)
            |> Option.defaultValue "x"
        | ScopeField _ -> "THIS"
        | _ -> "x"
    //TODO: Expand

    let checkIconField (folder: string) =
        files
        |> Collections.Set.filter (fun icon -> icon.StartsWith(folder, StringComparison.OrdinalIgnoreCase))
        |> Collections.Set.toList
        |> List.map (fun icon -> icon.Replace(".dds", ""))
    // let value = folder + "/" + key + ".dds"
    // if files.Contains value then OK else Invalid (Guid.NewGuid(), [inv (ErrorCodes.MissingFile value) leafornode])


    //////Loc Complete
    ///
    ///
    ///
    // let promoteMatch() =
    //     dataTypes.promotes |> Map.tryFind nextKey
    //     |> Option.orElse (dataTypes.promotes |> Map.tryFind (nextKey.ToUpperInvariant()))
    //     |> Option.map (fun (newType) -> if Set.contains newType dataTypes.dataTypeNames then NewDataType (newType, true) else Found newType)
    // let globalFunctionMatch() =
    //     dataTypes.functions |> Map.tryFind nextKey
    //     |> Option.map (fun (newType) -> if Set.contains newType dataTypes.dataTypeNames then NewDataType (newType, true) else Found newType)
    // let functionMatch() =
    //     dataTypes.dataTypes |> Map.tryFind dataType
    //     |> Option.bind (fun dataTypeMap -> dataTypeMap |> Map.tryFind nextKey)
    //     |> Option.map (fun (newType) -> if Set.contains newType dataTypes.dataTypeNames then NewDataType (newType, true) else Found newType)
    let promotes = dataTypes.promotes |> Map.toList |> List.map fst
    let functions = dataTypes.functions |> Map.toList |> List.map fst

    let dataTypeFunctions =
        dataTypes.dataTypes
        |> Map.toList
        |> List.map snd
        |> List.collect (fun l -> l |> Map.toList |> List.map fst)

    let allPossibles = promotes @ functions @ dataTypeFunctions

    let locCompleteInner (textBeforeCursor: string) =
        if textBeforeCursor.LastIndexOf "[" > textBeforeCursor.LastIndexOf "]" then
            (allPossibles |> List.map CompletionResponse.CreateSimple)
        else
            // let completionForScopeDotChain (key : string) (startingContext : ScopeContext) innerRules description =
            // let completionForScopeDotChain (key : string) innerRules description =
            //     let createSnippetForClauseWithCustomScopeReq scopeContext = (fun (r) -> createSnippetForClause (scoreFunction r scopeContext))

            //     let defaultRes = scopeCompletionList |> List.map (fun (l, r) -> createSnippetForClauseWithCustomScopeReq startingContext r innerRules description l)
            //     // eprintfn "dr %A" defaultRes
            //     if key.Contains(".")
            //     then
            //         let splitKey = key.Split([|'.'|])
            //         let changeScopeRes =
            //                 splitKey |> Array.take (splitKey.Length - 1)
            //                  |> String.concat "."
            //                  |> (fun next -> changeScope false true linkMap valueTriggerMap wildCardLinks varSet next startingContext)
            //         match changeScopeRes with
            //         | NewScope (newscope, _) ->
            //             scopeCompletionList |> List.map (fun (l, r) -> createSnippetForClauseWithCustomScopeReq newscope r innerRules description l)
            //         | ValueFound
            //         | VarFound
            //         | VarNotFound _
            //         | WrongScope _
            //         | NotFound -> defaultRes
            //     else
            //         defaultRes
            //let actualText = textBeforeCursor.Substring(textBeforeCursor.LastIndexOf "[" + 1)
            //Some (completionForScopeDotChain actualText )
            []


    let locComplete (pos: pos) (filetext: string) =
        let split = filetext.Split('\n')
        let targetLine = split.[pos.Line - 1]
        let textBeforeCursor = targetLine.Remove pos.Column
        locCompleteInner textBeforeCursor
    //// Normal complete
    ///
    ///


    let valueFieldAll, valueFieldNonGlobal, scopeFieldAll, scopeFieldNonGlobal, variableFieldAll, variableFieldNonGlobal =
        let evs =
            varMap.TryFind "event_target"
            |> Option.map (fun l -> l.StringValues |> List.ofSeq)
            |> Option.defaultValue []
            |> List.map (fun s ->
                { key = "event_target:" + s
                  requiredScopes = [ anyScope ]
                  outputScope = Scope anyScope
                  desc = None
                  kind = CompletionCategory.Global })

        let gevs =
            varMap.TryFind "global_event_target"
            |> Option.map (fun l -> l.StringValues |> List.ofSeq)
            |> Option.defaultValue []
            |> List.map (fun s ->
                { key = "event_target:" + s
                  requiredScopes = [ anyScope ]
                  outputScope = Scope anyScope
                  desc = None
                  kind = CompletionCategory.Global })

        let vars =
            varMap.TryFind "variable"
            |> Option.map (fun l -> l.StringValues |> List.ofSeq)
            |> Option.defaultValue []
            |> List.map (fun s ->
                { key = s
                  requiredScopes = [ anyScope ]
                  outputScope = CompletionScopeOutput.Variable
                  desc = None
                  kind = CompletionCategory.Variable })
        //        linkMap.ToList() |> List.iter (fun x -> log (sprintf "iop %A" x))
        let scopedEffectsExtra =
            [ "this"
              "root"
              "prev"
              "prevprev"
              "prevprevprev"
              "prevprevprevprev"
              "from"
              "fromfrom"
              "fromfromfrom"
              "fromfromfromfrom" ]
            |> List.map (fun s ->
                { key = s
                  requiredScopes = [ anyScope ]
                  outputScope = Scope anyScope
                  desc = None
                  kind = CompletionCategory.Link })

        let scopedEffects =
            linkMap.Values
            |> Seq.choose (fun s ->
                s
                |> function
                    | :? ScopedEffect as x when x.Type = EffectType.Link && x.Target.IsSome ->
                        Some
                            { key = x.Name.GetString()
                              requiredScopes = x.Scopes
                              outputScope = Scope x.Target.Value
                              desc = Some x.Desc
                              kind = CompletionCategory.Link }
                    | :? ScopedEffect as x when x.Type = EffectType.Link && x.Target.IsNone ->
                        Some
                            { key = x.Name.GetString()
                              requiredScopes = x.Scopes
                              outputScope = CompletionScopeOutput.Nothing
                              desc = Some x.Desc
                              kind = CompletionCategory.Link }
                    | _ -> None)
            |> Seq.toList

        let valueTriggers =
            valueTriggerMap.Values
            |> Seq.choose (fun s ->
                s
                |> function
                    | :? DocEffect as x when x.Type = EffectType.ValueTrigger ->
                        Some
                            { key = x.Name.GetString()
                              requiredScopes = x.Scopes
                              outputScope = CompletionScopeOutput.Value
                              desc = Some x.Desc
                              kind = CompletionCategory.Value }
                    | _ -> None)
            |> Seq.toList
        //        let scopedEffects = scopedEffects @ valueTriggers
        let scopedEffects = scopedEffects @ scopedEffectsExtra
        let valueFieldAll = evs @ gevs @ scopedEffects @ vars @ valueTriggers
        let valueFieldNonGlobal = scopedEffects @ vars @ valueTriggers
        let scopeFieldAll = evs @ gevs @ scopedEffects
        let scopeFieldNonGlobal = scopedEffects
        let variableFieldAll = evs @ gevs @ scopedEffects @ vars
        let variableFieldNonGlobal = scopedEffects @ vars
        valueFieldAll, valueFieldNonGlobal, scopeFieldAll, scopeFieldNonGlobal, variableFieldAll, variableFieldNonGlobal

    let createSnippetForClause
        (scoreFunction: string -> int)
        (rules: NewRule list)
        (description: string option)
        (key: string)
        =
        let filterToCompletion =
            function
            | LeafRule(SpecificField(SpecificValue _), _) -> true
            | NodeRule(SpecificField(SpecificValue _), _) -> true
            | _ -> false

        let ruleToDistinctKey =
            function
            | LeafRule(SpecificField(SpecificValue s), _) -> StringResource.stringManager.GetStringForID s.normal
            | NodeRule(SpecificField(SpecificValue s), _) -> StringResource.stringManager.GetStringForID s.normal
            | _ -> ""

        let rulePrint (i: int) =
            function
            | LeafRule(SpecificField(SpecificValue s), r) ->
                $"\t%s{StringResource.stringManager.GetStringForID s.normal} = ${{%i{i + 1}:%s{fieldToCompletionList r}}}\n"
            | NodeRule(SpecificField(SpecificValue s), _) ->
                sprintf "\t%s = ${%i:%s}\n" (StringResource.stringManager.GetStringForID s.normal) (i + 1) "{ }"
            | _ -> ""

        let requiredRules =
            rules
            |> List.filter (fun (f, o) -> o.min >= 1 && filterToCompletion f)
            |> List.distinctBy (fun (f, _) -> ruleToDistinctKey f)
            |> List.mapi (fun i (f, _) -> rulePrint i f)
            |> String.concat ""

        let requiredRules = if requiredRules = "" then "\t${0}\n" else requiredRules

        let score = scoreFunction key
        CompletionResponse.Snippet(key, $"%s{key} = {{\n%s{requiredRules}}}", description, Some score, Other)


    // | LeafValue
    let rec getRulePath
        (pos: pos)
        (stack: (string * int * string option * CompletionContext * string option) list)
        (node: IClause)
        =
        //log "grp %A %A %A" pos stack (node.Children |> List.map (fun f -> f.ToRaw))
        let countChildren (n2: IClause) (key: string) =
            n2.Nodes
            |> Seq.choose (function
                | c when c.Key == key -> Some c
                | _ -> None)
            |> Seq.length

        match node.Nodes |> Seq.tryFind (fun c -> rangeContainsPos c.Position pos) with
        | Some c ->
            //            log (sprintf "%s %A %A" c.Key c.Position pos)
            match
                (c.Position.StartLine = pos.Line)
                && ((c.Position.StartColumn + c.Key.Length + 1) > pos.Column)
            with
            | true -> getRulePath pos ((c.Key, countChildren node c.Key, None, NodeLHS, c.KeyPrefix) :: stack) c
            | false -> getRulePath pos ((c.Key, countChildren node c.Key, None, NodeRHS, c.KeyPrefix) :: stack) c
        | None ->
            /// This handles LHS vs RHS beacuse LHS gets an "x" inserted into it, so fails to match any rules
            match node.Leaves |> Seq.tryFind (fun l -> rangeContainsPos l.Position pos) with
            | Some l ->
                // SHould be <, but for some reason it isn't
                match l.Position.StartColumn + l.Key.Length + 1 > pos.Column with
                | true -> (l.Key, countChildren node l.Key, Some l.Key, LeafLHS, None) :: stack
                | false -> (l.Key, countChildren node l.Key, Some l.ValueText, LeafRHS, None) :: stack
            | None ->
                match node.ClauseList |> List.tryFind (fun c -> rangeContainsPos c.Position pos) with
                | Some vc ->
                    match
                        (vc.Position.StartLine = pos.Line)
                        && ((vc.Position.StartColumn + vc.Key.Length + 1) > pos.Column)
                    with
                    | true -> getRulePath pos ((vc.Key, countChildren node vc.Key, None, NodeLHS, None) :: stack) vc
                    | false -> getRulePath pos ((vc.Key, countChildren node vc.Key, None, NodeRHS, None) :: stack) vc
                | None -> stack
    // match node.LeafValues |> Seq.tryFind (fun lv -> rangeContainsPos lv.Position pos) with
    // | Some lv -> (lv.Key, countChildren node lv.Key, Some lv.ValueText)::stack
    // | None -> stack

    and getCompletionFromPath
        (scoreFunction: ScopeContext -> _ list -> CompletionScopeOutput -> CompletionScopeExpectation -> string -> int)
        (rules: NewRule list)
        (stack: (string * int * string option * CompletionContext * string option) list)
        scopeContext
        =
        // log (sprintf "%A" stack)

        let completionDotChainInner (key: string) (startingContext: ScopeContext) =
            if key.Contains(".") then
                let splitKey = key.Split([| '.' |])

                let changeScopeRes =
                    let substringBefore =
                        splitKey |> Array.takeWhile (fun x -> x.Contains magicCharString |> not)

                    match substringBefore.Length = 0 with
                    | true -> None
                    | false ->
                        substringBefore
                        |> String.concat "."
                        |> (fun next ->
                            changeScope false true linkMap valueTriggerMap wildCardLinks varSet next startingContext)
                        |> Some

                //                        let res =
                //                            substringBefore
                //                            |> (fun x -> log x; x)
                //                            |> (fun next -> changeScope false true linkMap valueTriggerMap wildCardLinks varSet next startingContext)
                //                        res, substringBefore.Contains(".")
                //                log (sprintf "REW %A %A %A" key changeScopeRes targetScopes)
                changeScopeRes
            //                match changeScopeRes with
            //                | None -> defaultRes
            //                | Some (NewScope (newscope, _, _)) ->
            //                    log (sprintf "%A %A" key newscope)
            //                    scopeCompletionListNonGlobal |> List.map (fun x -> createSnippetWithScore newscope x.requiredScopes x.outputScope targetScopes x.key x.desc x.kind)
            //                | Some (ValueFound _)
            //                | Some VarFound
            //                | Some (VarNotFound _)
            //                | Some (WrongScope _)
            //                | Some (NotFound) -> defaultResNonGlobal
            else
                None

        let inline convertScopeResToList
            startingContext
            (targetScopes: CompletionScopeExpectation)
            linkList
            terminalLinkList
            createSnippetFun
            (scopeRes: ScopeResult option)
            =

            let defaultRes =
                linkList
                |> List.map (fun x ->
                    createSnippetFun startingContext x.requiredScopes x.outputScope targetScopes x.key x.desc x.kind)

            let defaultResNonGlobal =
                terminalLinkList
                |> List.map (fun x ->
                    createSnippetFun startingContext x.requiredScopes x.outputScope targetScopes x.key x.desc x.kind)

            match scopeRes with
            | None -> defaultRes
            | Some(NewScope(newscope, _, _)) ->
                //                log (sprintf "%A %A" key newscope)
                // The magic char probably breaks any scope matches, so we'll never be on the first item here
                terminalLinkList
                |> List.map (fun x ->
                    createSnippetFun newscope x.requiredScopes x.outputScope targetScopes x.key x.desc x.kind)
            | Some(ValueFound _)
            | Some VarFound
            | Some(VarNotFound _)
            | Some(WrongScope _)
            | Some NotFound -> defaultResNonGlobal

        let completionForScopeDotChain
            (key: string)
            (startingContext: ScopeContext)
            innerRules
            (description: string option)
            (targetScopes: CompletionScopeExpectation)
            =
            let createSnippetForClauseWithCustomScopeReq scopeContext i o r key desc kind =
                createSnippetForClause (scoreFunction scopeContext i o r) innerRules description key

            completionDotChainInner key startingContext
            |> convertScopeResToList
                startingContext
                targetScopes
                scopeFieldAll
                scopeFieldNonGlobal
                createSnippetForClauseWithCustomScopeReq

        let completionForRHSScopeChain
            (key: string)
            (startingContext: ScopeContext)
            (targetScopes: CompletionScopeExpectation)
            =
            let createSnippetWithScore scopeContext =
                (fun i o r key desc kind -> Detailed(key, desc, Some(scoreFunction scopeContext i o r key), kind))

            completionDotChainInner key startingContext
            |> convertScopeResToList
                startingContext
                targetScopes
                scopeFieldAll
                scopeFieldNonGlobal
                createSnippetWithScore

        let completionForRHSValueChain (key: string) (startingContext: ScopeContext) =
            let createSnippetWithScore scopeContext =
                (fun i o r key desc kind -> Detailed(key, desc, Some(scoreFunction scopeContext i o r key), kind))

            completionDotChainInner key startingContext
            |> convertScopeResToList
                startingContext
                CompletionScopeExpectation.VariableOrValue
                valueFieldAll
                valueFieldNonGlobal
                createSnippetWithScore

        let completionForRHSVariableChain (key: string) (startingContext: ScopeContext) =
            let createSnippetWithScore scopeContext =
                (fun i o r key desc kind -> Detailed(key, desc, Some(scoreFunction scopeContext i o r key), kind))

            completionDotChainInner key startingContext
            |> convertScopeResToList
                startingContext
                CompletionScopeExpectation.VariableOrValue
                variableFieldAll
                variableFieldNonGlobal
                createSnippetWithScore

        let rec convRuleToCompletion (key: string) (count: int) (context: ScopeContext) (rule: NewRule) =
            //            eprintfn "crtc %A %A %A" key count rule
            let r, o = rule

            let scoreFunctioni =
                scoreFunction context o.requiredScopes CompletionScopeOutput.Nothing CompletionScopeExpectation.Nothing

            let createSnippetForClausei = createSnippetForClause scoreFunctioni

            let createSnippetForClauseWithCustomScopeReq =
                (fun r ->
                    createSnippetForClause (
                        scoreFunction context r CompletionScopeOutput.Nothing CompletionScopeExpectation.Nothing
                    ))

            let enough = o.max < count

            if enough then
                []
            else
                let keyvalue (inner: string) =
                    CompletionResponse.Snippet(
                        inner,
                        $"%s{inner} = $0",
                        o.description,
                        Some(scoreFunctioni inner),
                        Other
                    )

                let keyvalueWithCustomScopeReq r (inner: string) =
                    CompletionResponse.Snippet(
                        inner,
                        $"%s{inner} = $0",
                        o.description,
                        Some(
                            (scoreFunction context r CompletionScopeOutput.Nothing CompletionScopeExpectation.Nothing)
                                inner
                        ),
                        Other
                    )

                match r with
                | NodeRule(SpecificField(SpecificValue s), innerRules) ->
                    [ createSnippetForClausei
                          innerRules
                          o.description
                          (StringResource.stringManager.GetStringForID s.normal) ]
                | NodeRule(ValueField(ValueType.Enum e), innerRules) ->
                    enums.TryFind(e)
                    |> Option.map (fun (_, es) ->
                        es.StringValues
                        |> List.ofSeq
                        |> List.map (fun e -> createSnippetForClausei innerRules o.description e))
                    |> Option.defaultValue []
                | NodeRule(ValueField _, _) -> []
                | NodeRule(AliasField _, _) -> []
                | NodeRule(FilepathField _, _) -> []
                | NodeRule(IconField(folder), innerRules) ->
                    checkIconField folder
                    |> List.map (fun e -> createSnippetForClausei innerRules o.description e)
                | NodeRule(LocalisationField _, _) -> []
                | NodeRule(ScopeField(x), innerRules) ->
                    completionForScopeDotChain
                        key
                        context
                        innerRules
                        o.description
                        (CompletionScopeExpectation.Scopes x)
                //TODO: Scopes better
                | NodeRule(SubtypeField _, _) -> []
                | NodeRule(TypeField(TypeType.Simple t), innerRules) ->
                    types.TryFind(t)
                    |> Option.map (fun ts ->
                        ts |> List.map (fun e -> createSnippetForClausei innerRules o.description e))
                    |> Option.defaultValue []
                | NodeRule(TypeField(TypeType.Complex(p, t, s)), innerRules) ->
                    types.TryFind(t)
                    |> Option.map (fun ts ->
                        ts
                        |> List.map (fun e -> createSnippetForClausei innerRules o.description (p + e + s)))
                    |> Option.defaultValue []
                | NodeRule(VariableGetField v, innerRules) ->
                    varMap.TryFind(v)
                    |> Option.map (fun ss ->
                        ss.StringValues
                        |> List.ofSeq
                        |> List.map (fun e -> createSnippetForClausei innerRules o.description e))
                    |> Option.defaultValue []

                | LeafRule(SpecificField(SpecificValue s), _) ->
                    [ keyvalue (StringResource.stringManager.GetStringForID s.normal) ]
                | LeafRule(ValueField(ValueType.Enum e), _) ->
                    enums.TryFind(e)
                    |> Option.map (fun (_, es) -> es.StringValues |> List.ofSeq |> List.map (fun e -> keyvalue e))
                    |> Option.defaultValue []
                | LeafRule(ValueField _, _) -> []
                | LeafRule(AliasField _, _) -> []
                | LeafRule(FilepathField _, _) -> []
                | LeafRule(IconField(folder), _) -> checkIconField folder |> List.map keyvalue
                | LeafRule(LocalisationField _, _) -> []
                | LeafRule(ScopeField _, _) ->
                    scopeFieldAll
                    |> List.map (fun x -> keyvalueWithCustomScopeReq x.requiredScopes x.key)
                //TODO: Scopes
                | LeafRule(SubtypeField _, _) -> []
                | LeafRule(TypeField(TypeType.Simple t), _) ->
                    types.TryFind(t)
                    |> Option.map (fun ts -> ts |> List.map (fun e -> keyvalue e))
                    |> Option.defaultValue []
                | LeafRule(TypeField(TypeType.Complex(p, t, s)), _) ->
                    types.TryFind(t)
                    |> Option.map (fun ts -> ts |> List.map (fun e -> keyvalue (p + e + s)))
                    |> Option.defaultValue []
                | LeafRule(VariableGetField v, _) ->
                    varMap.TryFind(v)
                    |> Option.map (fun ss -> ss.StringValues |> List.ofSeq |> List.map (fun e -> keyvalue e))
                    |> Option.defaultValue []
                | LeafRule(VariableSetField v, _) ->
                    varMap.TryFind(v)
                    |> Option.map (fun ss -> ss.StringValues |> List.ofSeq |> List.map (fun e -> keyvalue e))
                    |> Option.defaultValue []

                | LeafValueRule lv ->
                    match lv with
                    | NewField.TypeField(TypeType.Simple t) ->
                        types.TryFind(t)
                        |> Option.defaultValue []
                        |> List.map CompletionResponse.CreateSimple
                    | NewField.TypeField(TypeType.Complex(p, t, s)) ->
                        types.TryFind(t)
                        |> Option.map (fun ns -> List.map (fun n -> p + n + s) ns)
                        |> Option.defaultValue []
                        |> List.map CompletionResponse.CreateSimple
                    | NewField.ValueField(Enum e) ->
                        enums.TryFind(e)
                        |> Option.map (fun (_, s) -> s.StringValues |> List.ofSeq)
                        |> Option.defaultValue []
                        |> List.map CompletionResponse.CreateSimple
                    | NewField.VariableGetField v ->
                        varMap.TryFind(v)
                        |> Option.map (fun s -> s.StringValues |> List.ofSeq)
                        |> Option.defaultValue []
                        |> List.map CompletionResponse.CreateSimple
                    | NewField.VariableSetField v ->
                        varMap.TryFind(v)
                        |> Option.map (fun s -> s.StringValues |> List.ofSeq)
                        |> Option.defaultValue []
                        |> List.map CompletionResponse.CreateSimple
                    | _ -> []
                | SubtypeRule _ -> []
                | _ -> []
        //TODO: Add leafvalue
        let fieldToRules (field: NewField) (value: string) (scopeContext: ScopeContext) =
            //            log (sprintf "%A %A" field value)
            //            eprintfn "%A" value
            match field with
            | NewField.ValueField(Enum e) ->
                enums.TryFind(e)
                |> Option.map (fun (_, s) -> s.StringValues |> List.ofSeq)
                |> Option.defaultValue []
                |> List.map CompletionResponse.CreateSimple
            | NewField.ValueField v ->
                FieldValidators.getValidValues v
                |> Option.defaultValue []
                |> List.map CompletionResponse.CreateSimple
            | NewField.TypeField(TypeType.Simple t) ->
                types.TryFind(t)
                |> Option.defaultValue []
                |> List.map CompletionResponse.CreateSimple
            | NewField.TypeField(TypeType.Complex(p, t, s)) ->
                types.TryFind(t)
                |> Option.map (fun ns -> List.map (fun n -> p + n + s) ns)
                |> Option.defaultValue []
                |> List.map CompletionResponse.CreateSimple
            | NewField.LocalisationField(s, _) ->
                match s, value.Contains "[" with
                | false, true -> (allPossibles |> List.map CompletionResponse.CreateSimple)
                | true, _ ->
                    localisation
                    |> List.tryFind (fun (lang, _) -> lang = (STL STLLang.Default))
                    |> Option.map (snd >> Set.toList)
                    |> Option.defaultValue []
                    |> List.map CompletionResponse.CreateSimple
                | false, _ ->
                    localisation
                    |> List.tryFind (fun (lang, _) -> lang <> (STL STLLang.Default))
                    |> Option.map (snd >> Set.toList)
                    |> Option.defaultValue []
                    |> List.map CompletionResponse.CreateSimple
            | NewField.FilepathField _ -> files |> Set.toList |> List.map CompletionResponse.CreateSimple
            | NewField.ScopeField x ->
                completionForRHSScopeChain value scopeContext (CompletionScopeExpectation.Scopes x)
            //            |NewField.ScopeField _ -> scopeCompletionList |> List.map (fst >> (CompletionResponse.CreateSimple))
            | NewField.VariableGetField v ->
                varMap.TryFind v
                |> Option.map (fun ss -> ss.StringValues |> List.ofSeq)
                |> Option.defaultValue []
                |> List.map CompletionResponse.CreateSimple
            | NewField.VariableSetField v ->
                varMap.TryFind v
                |> Option.map (fun ss -> ss.StringValues |> List.ofSeq)
                |> Option.defaultValue []
                |> List.map CompletionResponse.CreateSimple
            | NewField.VariableField _ -> completionForRHSVariableChain value scopeContext
            | NewField.ValueScopeField _ ->
                completionForRHSValueChain value scopeContext
                @ (enums.TryFind("static_values")
                   |> Option.map (fun (_, s) -> s.StringValues |> List.ofSeq)
                   |> Option.defaultValue []
                   |> List.map CompletionResponse.CreateSimple)
            | _ -> []

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

        let ctx =
            { subtypes = []
              scopes = defaultContext
              warningOnly = false }

        let severity = Severity.Error

        let rec findRule
            (rules: NewRule list)
            (stack: (string * int * string option * CompletionContext) list)
            scopeContext
            =
            let subtypedRules =
                rules
                |> List.collect (function
                    | SubtypeRule(_, _, cfs), _ -> cfs
                    | x -> [ x ])

            let expandedRules =
                subtypedRules
                |> List.collect (function
                    | LeafRule(AliasField a, _), o ->
                        (rootRules.Aliases.TryFind a |> Option.defaultValue [])
                        |> List.map (fun (r, oi) -> (r, { oi with min = o.min; max = oi.max }))
                    | NodeRule(AliasField a, _), o ->
                        (rootRules.Aliases.TryFind a |> Option.defaultValue [])
                        |> List.map (fun (r, oi) -> (r, { oi with min = o.min; max = oi.max }))
                    | x -> [ x ])
            //eprintfn "fr %A %A" stack (expandedRules |> List.truncate 10)
            match stack with
            | [] -> expandedRules |> List.collect (convRuleToCompletion "" 0 scopeContext)
            | [ (key, count, None, NodeLHS) ] ->
                expandedRules |> List.collect (convRuleToCompletion key count scopeContext)
            | [ (key, count, None, NodeRHS) ] ->
                match
                    expandedRules
                    |> List.choose (function
                        | NodeRule(l, rs), o when
                            FieldValidators.checkFieldByKey
                                p
                                severity
                                ctx
                                l
                                (StringResource.stringManager.InternIdentifierToken key)
                            ->
                            Some(l, rs, o)
                        | _ -> None)
                with
                | [] -> expandedRules |> List.collect (convRuleToCompletion key count scopeContext)
                | fs ->
                    fs
                    |> List.collect (fun (_, innerRules, _) -> findRule innerRules [] scopeContext)
            | [ (key, count, Some _, LeafLHS) ] ->
                expandedRules |> List.collect (convRuleToCompletion key count scopeContext)
            | [ (key, count, Some value, LeafRHS) ] ->
                match
                    expandedRules
                    |> List.choose (function
                        | LeafRule(l, r), o when
                            FieldValidators.checkFieldByKey
                                p
                                severity
                                ctx
                                l
                                (StringResource.stringManager.InternIdentifierToken key)
                            ->
                            Some(l, r, o)
                        | _ -> None)
                with
                | [] -> expandedRules |> List.collect (convRuleToCompletion key count scopeContext)
                | fs ->
                    //log "%s %A" key fs
                    let res = fs |> List.collect (fun (_, f, _) -> fieldToRules f value scopeContext)
                    //log "res %A" res
                    res
            | (key, count, _, NodeRHS) :: rest ->
                match
                    expandedRules
                    |> List.choose (function
                        | NodeRule(l, rs), o when
                            FieldValidators.checkFieldByKey
                                p
                                severity
                                ctx
                                l
                                (StringResource.stringManager.InternIdentifierToken key)
                            ->
                            Some(l, rs, o)
                        | _ -> None)
                with
                | [] -> expandedRules |> List.collect (convRuleToCompletion key count scopeContext)
                | fs ->
                    fs
                    |> List.collect (fun (_, innerRules, _) -> findRule innerRules rest scopeContext)
            | (key, count, _, t) :: rest ->
                log $"Completion error %A{key} %A{t}"
                expandedRules |> List.collect (convRuleToCompletion key count scopeContext)

        let stack = stack |> List.map (fun (a, b, c, d, e) -> (a, b, c, d))
        let res = findRule rules stack scopeContext |> List.distinct
        //log "res2 %A" res
        res

    let scoreFunction
        (allUsedKeys: Collections.Set<string>)
        (startingContext: ScopeContext)
        (inputScopes: Scope list)
        (outputScope: CompletionScopeOutput)
        (expectedScope: CompletionScopeExpectation)
        (key: string)
        =

        let validInputScopeScore =
            match inputScopes with
            | [] -> 0 // This item doesn't care what scope it's in
            | [ x ] when x = anyScope -> 10 // It supports any scope, so, non-specific
            | xs -> // This item expects these scopes
                match startingContext.CurrentScope with
                | x when x = anyScope -> 20 // We're in any scope, so non-specific
                | s ->
                    if List.exists s.IsOfScope xs then
                        50 // It supports the scope we're in
                    else
                        0 // It doesn't support the scope we're in

        let validOutputScopeScore =
            match expectedScope, outputScope with
            | VariableOrValue, CompletionScopeOutput.Value -> 25
            | VariableOrValue, CompletionScopeOutput.Variable -> 20
            | Nothing, CompletionScopeOutput.Nothing -> 25
            | Scopes expectedScopes, CompletionScopeOutput.Scope scope ->
                match expectedScopes, scope with
                | [ x ], _ when x = anyScope -> 5 // The context expects any scope, so, non-specific
                | [], _ -> 0 // The context doesn't want a scope
                | xs, y when y = anyScope -> 15 // We're in any scope, so, non-specific
                | xs, y ->
                    if List.exists y.IsOfScope xs then
                        25 // It expects the scope we'll output
                    else
                        0
            | _ -> 0

        let usedKeyBonus = if Set.contains key allUsedKeys then 10 else 0
        let score = validInputScopeScore + validOutputScopeScore + usedKeyBonus
        //        if key = "test_variable"
        //        then log (sprintf "sf %A %A %A %A %A %A %A" startingContext.CurrentScope inputScopes outputScope expectedScope validInputScopeScore validOutputScopeScore usedKeyBonus)
        //        else ()
        max score 1
    //        let validInputScope =
    //            match inputScopes with
    //            | [] -> true
    //            | xs ->
    //                match startingContext.CurrentScope with
    //                | x when x = anyScope -> true
    //                | s -> List.exists s.IsOfScope xs
    //        let validOutputScope =
    //            match requiredScopes with
    //            | [] -> true
    //            | xs ->
    //                match outputScope with
    //                | Some x when x = anyScope -> true
    //                | Some x -> List.exists x.IsOfScope xs
    //                | _ -> false
    //        //TODO add shortcutting
    //
    //        let usedKey = List.contains key allUsedKeys
    //        match validOutputScope, usedKey, validInputScope with
    //        | true, true, true -> 110
    //        | true, true, false -> 60
    //        | true, false, true -> 100
    //        | true, false, false -> 50
    //        | false, true, true -> 60
    //        | false, true, false -> 10
    //        | false, false, true -> 50
    //        | false, false, false -> 1

    let complete (pos: pos) (entity: Entity) (scopeContext: ScopeContext option) =
        let scopeContext = Option.defaultValue defaultContext scopeContext
        let path = getRulePath pos [] entity.entity |> List.rev
        //        log (sprintf "%A" path)

        let dir = Path.GetDirectoryName(entity.logicalpath).Replace('\\', '/')
        let fileName = Path.GetFileName(entity.logicalpath)
        // log "%A" typedefs
        // log "%A" pos
        // log "%A" entity.logicalpath
        // log  (sprintf "tb %A" pathDir)
        let skiprootkey (skipRootKey: SkipRootKey) (s: string) =
            match skipRootKey with
            | SpecificKey key -> s == key
            | AnyKey -> true
            | MultipleKeys(keys, shouldMatch) -> (keys |> List.exists ((==) s)) <> (not shouldMatch)

        let pathFilteredTypes =
            typedefs
            |> List.filter (fun t -> FieldValidatorsCs.CheckPathDir(t.pathOptions, dir, fileName))

        let allUsedKeys =
            getAllKeysInFile entity.entity @ globalScriptVariables |> Set.ofList

        let scoreFunction = scoreFunction allUsedKeys

        let rec validateTypeSkipRoot
            (t: TypeDefinition)
            (skipRootKeyStack: SkipRootKey list)
            (path: (string * int * string option * CompletionContext * string option) list)
            =
            let typerules =
                rootRules.TypeRules
                |> List.choose (function
                    | name, typerule when name == t.name -> Some typerule
                    | _ -> None)

            match skipRootKeyStack, t.type_per_file, path with
            | _, false, [] -> getCompletionFromPath scoreFunction typerules [] scopeContext
            | _, true, (head, c, b, nt, keyprefix) :: tail ->
                // getCompletionFromPath scoreFunction typerules ((head, c, b, nt)::tail) scopeContext
                getCompletionFromPath
                    scoreFunction
                    typerules
                    ((t.name, 1, None, NodeRHS, None) :: (head, c, b, nt, keyprefix) :: tail)
                    scopeContext
            | _, true, [] ->
                getCompletionFromPath scoreFunction typerules [ t.name, 1, None, NodeRHS, None ] scopeContext
            | [], false, (head, c, _, _, keyprefix) :: tail ->
                //TODO: Handle key prefix
                if FieldValidators.typekeyfilter t head keyprefix then
                    getCompletionFromPath
                        scoreFunction
                        typerules
                        ((t.name, c, None, NodeRHS, None) :: tail)
                        scopeContext
                else
                    []
            | head :: tail, false, (pathhead, _, _, _, _) :: pathtail ->
                if skiprootkey head pathhead then
                    validateTypeSkipRoot t tail pathtail
                else
                    []

        let items =
            match path |> List.tryLast, path.Length with
            | Some(_, count, Some x, _, _), _ when x.Length > 0 && x.StartsWith("@" + magicCharString) ->
                let staticVars =
                    CWTools.Validation.Stellaris.STLValidation.getDefinedVariables entity.entity

                staticVars |> List.map (fun s -> CompletionResponse.CreateSimple s)
            | Some(_, _, _, CompletionContext.NodeLHS, _), 1 -> []
            | _ ->
                pathFilteredTypes
                |> List.collect (fun t -> validateTypeSkipRoot t t.skipRootKey path)
        //TODO: Expand this to use a snippet not just the name of the type
        let createSnippetForType (typeDef: TypeDefinition) =
            let rootSnippets =
                match typeDef.typeKeyFilter with
                | Some(keys: string list, false) -> keys
                | _ -> [ typeDef.name ]

            rootSnippets
            @ (typeDef.subtypes
               |> List.choose (fun st ->
                   if st.typeKeyField.IsSome then
                       (Some st.typeKeyField.Value)
                   else
                       None))
            |> List.map (fun s -> createSnippetForClause (fun _ -> 1) [] None s)

        let rootTypeItems =
            match path with
            | [ (_, _, _, CompletionContext.NodeLHS, _) ] -> pathFilteredTypes |> List.collect createSnippetForType
            | y when y.Length = 0 -> pathFilteredTypes |> List.collect createSnippetForType
            | _ -> []
        // eprintfn "%A" path
        // eprintfn "%A" rootTypeItems
        let scoreForLabel (label: string) =
            if allUsedKeys |> Set.contains label then 10 else 1

        (items @ rootTypeItems)
        |> List.map (function
            | Simple(label, None, kind) -> Simple(label, Some(scoreForLabel label), kind)
            | Detailed(label, desc, None, kind) -> Detailed(label, desc, Some(scoreForLabel label), kind)
            | Snippet(label, snippet, desc, None, kind) ->
                Snippet(label, snippet, desc, Some(scoreForLabel label), kind)
            | x -> x)



    member __.Complete(pos: pos, entity: Entity, scopeContext) = complete pos entity scopeContext
    member __.LocalisationComplete(pos: pos, filetext: string) = locComplete pos filetext
