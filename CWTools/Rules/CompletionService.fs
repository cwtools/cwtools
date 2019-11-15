namespace CWTools.Rules
open CWTools.Common
open Microsoft.FSharp.Collections.Tagged
open CWTools.Utilities.Utils
open CWTools.Process.Localisation
open CWTools.Process
open CWTools.Process.Scopes
open CWTools.Process.ProcessCore
open CWTools.Utilities
open System
open CWTools.Games
open CWTools.Utilities.Position
open System.IO

type CompletionContext =
    | NodeLHS
    | NodeRHS
    | LeafLHS
    | LeafRHS

type CompletionService
                    (rootRules : RootRule list, typedefs : TypeDefinition list , types : Collections.Map<string, StringSet>,
                     enums : Collections.Map<string, string * StringSet>, varMap : Collections.Map<string, StringSet>,
                     localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>,
                     links : Map<string,Effect,InsensitiveStringComparer>,
                     valueTriggers : Map<string,Effect,InsensitiveStringComparer>,
                     globalScriptVariables : string list, changeScope : ChangeScope, defaultContext : ScopeContext, anyScope, oneToOneScopes, defaultLang,
                     dataTypes : CWTools.Parser.DataTypeParser.JominiLocDataTypes,
                     processLocalisation : (Lang * Collections.Map<string,CWTools.Localisation.Entry> -> Lang * Collections.Map<string,LocEntry>),
                     validateLocalisation : (LocEntry -> ScopeContext -> CWTools.Validation.ValidationResult)) =
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
    let defaultKeys = localisation |> List.choose (fun (l, ks) -> if l = defaultLang then Some ks else None) |> List.tryHead |> Option.defaultValue Set.empty
    let localisationKeys = localisation |> List.choose (fun (l, ks) -> if l = defaultLang then None else Some (l, ks))

    let aliasKeyMap =
        aliases |> Map.toList |> List.map (fun (key, rules) -> key, (rules |> List.choose (function | LeafRule (SpecificField (SpecificValue x), _), _ -> Some x.lower | NodeRule (SpecificField (SpecificValue x), _), _ -> Some x.lower | _ -> None)))
                |> List.map (fun (key, values) -> key, Collections.Set.ofList values)
                |> Map.ofList

    let linkMap = links
    let wildCardLinks = linkMap.ToList() |> List.map snd |> List.choose (function | :? ScopedEffect as e when e.IsWildCard -> Some e |_ -> None )
    let valueTriggerMap = valueTriggers
    let varSet = varMap.TryFind "variable" |> Option.defaultValue (StringSet.Empty(InsensitiveStringComparer()))

    let getAllKeysInFile (root : Node) =
        let fNode = (fun (x:Node) acc ->
                        let withValues = x.Values |> List.fold (fun a leaf ->  leaf.Key::leaf.ValueText::a) acc
                        let withBoth = x.LeafValues |> Seq.fold (fun a leafvalue -> leafvalue.ValueText::a) withValues
                        x.Key :: withBoth
                    )
        foldNode7 fNode root

    let fieldToCompletionList (field : NewField) =
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
    let dataTypeFunctions = dataTypes.dataTypes |> Map.toList |> List.map snd |> List.collect (fun l -> l |> Map.toList |> List.map fst)
    let allPossibles = promotes @ functions @ dataTypeFunctions

    let locCompleteInner (textBeforeCursor : string) =
        if textBeforeCursor.LastIndexOf "[" > textBeforeCursor.LastIndexOf "]"
        then
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


    let locComplete (pos : pos) (filetext : string) =
        let split = filetext.Split('\n')
        let targetLine = split.[pos.Line - 1]
        let textBeforeCursor = targetLine.Remove (pos.Column)
        locCompleteInner textBeforeCursor
    //// Normal complete
    ///
    ///
    let scopeCompletionList =
        let evs = varMap.TryFind "event_target" |> Option.map (fun l -> l.ToList())
                                                |> Option.defaultValue []
                                                |> List.map (fun s -> "event_target:" + s, [anyScope])
        let gevs = varMap.TryFind "global_event_target" |> Option.map (fun l -> l.ToList())
                                                |> Option.defaultValue []
                                                |> List.map (fun s -> "event_target:" + s, [anyScope])
        let scopedEffects = linkMap.ToList() |> List.choose (fun (_, s) -> s |> function | :? ScopedEffect as x -> Some (x.Name, x.Scopes) | _ -> None )
        evs @ gevs @ scopedEffects

    let createSnippetForClause (scoreFunction : string -> int) (rules : NewRule list) (description : string option) (key : string) =
        let filterToCompletion =
            function
            |LeafRule(SpecificField(SpecificValue _), _) -> true
            |NodeRule(SpecificField(SpecificValue _), _) -> true
            |_ -> false
        let ruleToDistinctKey =
            function
            |LeafRule(SpecificField(SpecificValue s), _) -> StringResource.stringManager.GetStringForID s.normal
            |NodeRule(SpecificField(SpecificValue s), _) -> StringResource.stringManager.GetStringForID s.normal
            |_ -> ""

        let rulePrint (i : int) =
            function
            |LeafRule(SpecificField(SpecificValue s), r) ->
                sprintf "\t%s = ${%i:%s}\n" (StringResource.stringManager.GetStringForID s.normal) (i + 1) (fieldToCompletionList r)
            |NodeRule(SpecificField(SpecificValue s), _) ->
                sprintf "\t%s = ${%i:%s}\n" (StringResource.stringManager.GetStringForID s.normal) (i + 1) "{ }"
            |_ -> ""

        let requiredRules = rules |> List.filter (fun (f, o) -> o.min >= 1 && filterToCompletion f)
                                  |> List.distinctBy (fun (f, _) -> ruleToDistinctKey f)
                                  |> List.mapi (fun i (f, _) -> rulePrint i f)
                                  |> String.concat ""

        let score = scoreFunction key
        CompletionResponse.Snippet (key, (sprintf "%s = {\n%s}" key requiredRules), description, Some score)


    // | LeafValue
    let rec getRulePath (pos : pos) (stack : (string * int * string option * CompletionContext) list) (node : IClause) =
       //log "grp %A %A %A" pos stack (node.Children |> List.map (fun f -> f.ToRaw))
        let countChildren (n2 : IClause) (key : string) =
            n2.Nodes |> Seq.choose (function |c when c.Key == key -> Some c |_ -> None) |> Seq.length
        match node.Nodes |> Seq.tryFind (fun c -> rangeContainsPos c.Position pos) with
        | Some c ->
            log (sprintf "%s %A %A" c.Key c.Position pos)
            match (c.Position.StartLine = (pos.Line)) && ((c.Position.StartColumn + c.Key.Length + 1) > pos.Column) with
            | true -> getRulePath pos ((c.Key, countChildren node c.Key, None, NodeLHS) :: stack) c
            | false -> getRulePath pos ((c.Key, countChildren node c.Key, None, NodeRHS) :: stack) c
        | None ->
                /// This handles LHS vs RHS beacuse LHS gets an "x" inserted into it, so fails to match any rules
                match node.Leaves |> Seq.tryFind (fun l -> rangeContainsPos l.Position pos) with
                | Some l ->
                    // SHould be <, but for some reason it isn't
                    match l.Position.StartColumn + l.Key.Length + 1 > pos.Column with
                    |true -> (l.Key, countChildren node l.Key, Some l.Key, LeafLHS)::stack
                    |false -> (l.Key, countChildren node l.Key, Some l.ValueText, LeafRHS)::stack
                | None ->
                    match node.ClauseList |> List.tryFind (fun c -> rangeContainsPos c.Position pos) with
                    | Some vc ->
                        match (vc.Position.StartLine = (pos.Line)) && ((vc.Position.StartColumn + vc.Key.Length + 1) > pos.Column) with
                        | true -> getRulePath pos ((vc.Key, countChildren node vc.Key, None, NodeLHS) :: stack) vc
                        | false -> getRulePath pos ((vc.Key, countChildren node vc.Key, None, NodeRHS) :: stack) vc
                    | None ->
                        stack
                    // match node.LeafValues |> Seq.tryFind (fun lv -> rangeContainsPos lv.Position pos) with
                    // | Some lv -> (lv.Key, countChildren node lv.Key, Some lv.ValueText)::stack
                    // | None -> stack

    and getCompletionFromPath (scoreFunction : _ list -> ScopeContext -> string -> int) (rules : NewRule list) (stack : (string * int * string option * CompletionContext) list) scopeContext =
        // log (sprintf "%A" stack)
        let completionForScopeDotChain (key : string) (startingContext : ScopeContext) innerRules description =
            let createSnippetForClauseWithCustomScopeReq scopeContext = (fun (r) -> createSnippetForClause (scoreFunction r scopeContext))

            let defaultRes = scopeCompletionList |> List.map (fun (l, r) -> createSnippetForClauseWithCustomScopeReq startingContext r innerRules description l)
            // eprintfn "dr %A" defaultRes
            if key.Contains(".")
            then
                let splitKey = key.Split([|'.'|])
                let changeScopeRes =
                        splitKey |> Array.take (splitKey.Length - 1)
                         |> String.concat "."
                         |> (fun next -> changeScope false true linkMap valueTriggerMap wildCardLinks varSet next startingContext)
                match changeScopeRes with
                | NewScope (newscope, _) ->
                    scopeCompletionList |> List.map (fun (l, r) -> createSnippetForClauseWithCustomScopeReq newscope r innerRules description l)
                | ValueFound
                | VarFound
                | VarNotFound _
                | WrongScope _
                | NotFound -> defaultRes
            else
                defaultRes
        let rec convRuleToCompletion (key : string) (count : int) (context : ScopeContext) (rule : NewRule) =
            // eprintfn "crtc %A %A" key rule
            let r, o = rule
            let scoreFunctioni = scoreFunction o.requiredScopes context
            let createSnippetForClausei = createSnippetForClause scoreFunctioni
            let createSnippetForClauseWithCustomScopeReq = (fun (r) -> createSnippetForClause (scoreFunction r context))
            let enough = o.max <= count
            if enough
            then []
            else
                let keyvalue (inner : string) = CompletionResponse.Snippet (inner, (sprintf "%s = $0" inner), o.description, Some (scoreFunctioni inner))
                let keyvalueWithCustomScopeReq r (inner : string) = CompletionResponse.Snippet (inner, (sprintf "%s = $0" inner), o.description, Some ((scoreFunction r context) inner))
                match r with
                |NodeRule (SpecificField(SpecificValue s), innerRules) ->
                    [createSnippetForClausei innerRules o.description (StringResource.stringManager.GetStringForID s.normal)]
                |NodeRule (ValueField(ValueType.Enum e), innerRules) ->
                    enums.TryFind(e) |> Option.map (fun (_, es) -> es.ToList() |> List.map (fun e -> createSnippetForClausei innerRules o.description e)) |> Option.defaultValue []
                |NodeRule (ValueField(_), _) -> []
                |NodeRule (AliasField(_), _) -> []
                |NodeRule (FilepathField(_), _) -> []
                |NodeRule (IconField(folder), innerRules) ->
                    checkIconField folder |> List.map (fun e -> createSnippetForClausei innerRules o.description e)
                |NodeRule (LocalisationField(_), _) -> []
                |NodeRule (ScopeField(_), innerRules) ->
                    completionForScopeDotChain key context innerRules o.description
                //TODO: Scopes better
                |NodeRule (SubtypeField(_), _) -> []
                |NodeRule (TypeField(TypeType.Simple t), innerRules) ->
                    types.TryFind(t) |> Option.map (fun ts -> ts |> List.map (fun e -> createSnippetForClausei innerRules o.description e)) |> Option.defaultValue []
                |NodeRule (TypeField(TypeType.Complex (p,t,s)), innerRules) ->
                    types.TryFind(t) |> Option.map (fun ts -> ts |> List.map (fun e -> createSnippetForClausei innerRules o.description (p+e+s))) |> Option.defaultValue []
                |NodeRule (VariableGetField v, innerRules) ->
                    varMap.TryFind(v) |> Option.map (fun ss -> ss.ToList() |> List.map (fun e -> createSnippetForClausei innerRules o.description e)) |> Option.defaultValue []

                |LeafRule (SpecificField(SpecificValue s), _) ->
                    [keyvalue (StringResource.stringManager.GetStringForID s.normal)]
                |LeafRule (ValueField(ValueType.Enum e), _) ->
                    enums.TryFind(e) |> Option.map (fun (_, es) -> es.ToList() |> List.map (fun e -> keyvalue e)) |> Option.defaultValue []
                |LeafRule (ValueField(_), _) -> []
                |LeafRule (AliasField(_), _) -> []
                |LeafRule (FilepathField(_), _) -> []
                |LeafRule (IconField(folder), _) -> checkIconField folder |> List.map keyvalue
                |LeafRule (LocalisationField(_), _) -> []
                |LeafRule (ScopeField(_), _) -> scopeCompletionList |> List.map (fun (l, r) -> keyvalueWithCustomScopeReq r l)
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
        let fieldToRules (field : NewField) (value : string) =
            //log "%A" types
            //log "%A" field
            match field with
            |NewField.ValueField (Enum e) -> enums.TryFind(e) |> Option.map (fun (_, s) -> s.ToList()) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
            |NewField.ValueField v -> FieldValidators.getValidValues v |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
            |NewField.TypeField (TypeType.Simple t) -> types.TryFind(t) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
            |NewField.TypeField (TypeType.Complex (p,t,s)) -> types.TryFind(t) |>  Option.map (fun ns -> List.map (fun n ->  p + n + s) ns) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
            |NewField.LocalisationField s ->
                match s, value.Contains "[" with
                |false, true -> (allPossibles |> List.map CompletionResponse.CreateSimple)
                |true, _ -> localisation |> List.tryFind (fun (lang, _ ) -> lang = (STL STLLang.Default)) |> Option.map (snd >> Set.toList) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
                |false, _ -> localisation |> List.tryFind (fun (lang, _ ) -> lang <> (STL STLLang.Default)) |> Option.map (snd >> Set.toList) |> Option.defaultValue [] |> List.map CompletionResponse.CreateSimple
            |NewField.FilepathField _ -> files |> Set.toList |> List.map CompletionResponse.CreateSimple
            |NewField.ScopeField _ -> scopeCompletionList |> List.map (fst >> (CompletionResponse.CreateSimple))
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
            localisation = localisationKeys
            defaultLocalisation = defaultKeys
            files = files
            changeScope = changeScope
            anyScope = anyScope
            defaultLang = defaultLang
            wildcardLinks = wildCardLinks
            aliasKeyList = aliasKeyMap
            processLocalisation = processLocalisation
            validateLocalisation = validateLocalisation
        }
        let ctx = { subtypes = []; scopes = defaultContext; warningOnly = false }
        let severity = Severity.Error

        let rec findRule (rules : NewRule list) (stack : (string * int * string option * CompletionContext) list) (scopeContext) =
            let subtypedRules =
                rules |> List.collect (
                    function
                    | SubtypeRule(_, _, cfs), _ -> cfs
                    |x -> [x])
            let expandedRules =
                subtypedRules |> List.collect (
                    function
                    | LeafRule((AliasField a),_),o -> (aliases.TryFind a |> Option.defaultValue []) |> List.map (fun (r, oi) -> (r, { oi with min =  o.min; max = oi.max}))
                    | NodeRule((AliasField a),_),o -> (aliases.TryFind a |> Option.defaultValue []) |> List.map (fun (r, oi) -> (r, { oi with min =  o.min; max = oi.max}))
                    |x -> [x])
            //eprintfn "fr %A %A" stack (expandedRules |> List.truncate 10)
            match stack with
            | [] -> expandedRules |> List.collect (convRuleToCompletion "" 0 scopeContext)
            | [(key, count, None, NodeLHS)] ->
                expandedRules |> List.collect (convRuleToCompletion key count scopeContext)
            | [(key, count, None, NodeRHS)] ->
                match expandedRules |> List.choose (function | (NodeRule (l, rs), o) when FieldValidators.checkFieldByKey p severity ctx l (StringResource.stringManager.InternIdentifierToken key).lower key -> Some (l, rs, o) | _ -> None) with
                | [] -> expandedRules |> List.collect (convRuleToCompletion key count scopeContext)
                | fs -> fs |> List.collect (fun (_, innerRules, _) -> findRule innerRules [] scopeContext)
            | [(key, count, Some _, LeafLHS)] ->
                expandedRules |> List.collect (convRuleToCompletion key count scopeContext)
            | [(key, count, Some value, LeafRHS)] ->
                match expandedRules |> List.choose (function | (LeafRule (l, r), o) when FieldValidators.checkFieldByKey p severity ctx l (StringResource.stringManager.InternIdentifierToken key).lower key -> Some (l, r, o) | _ -> None) with
                | [] -> expandedRules |> List.collect (convRuleToCompletion key count scopeContext)
                | fs ->
                    //log "%s %A" key fs
                    let res = fs |> List.collect (fun (_, f, _) -> fieldToRules f value)
                    //log "res %A" res
                    res
            | (key, count, _, NodeRHS)::rest ->
                match expandedRules |> List.choose (function | (NodeRule (l, rs), o) when FieldValidators.checkFieldByKey p severity ctx l (StringResource.stringManager.InternIdentifierToken key).lower key -> Some (l, rs, o) | _ -> None) with
                | [] -> expandedRules |> List.collect (convRuleToCompletion key count scopeContext)
                | fs -> fs |> List.collect (fun (_, innerRules, _) -> findRule innerRules rest scopeContext)
            | (key, count, _, t)::rest ->
                log (sprintf "Completion error %A %A" key t)
                expandedRules |> List.collect (convRuleToCompletion key count scopeContext)
        let res = findRule rules stack scopeContext |> List.distinct
        //log "res2 %A" res
        res
    let scoreFunction (allUsedKeys : string list) (requiredScopes : 'T list) (currentContext : ScopeContext) (key : string) =
        let validScope =
            match requiredScopes with
            | [] -> true
            | xs ->
                match currentContext.CurrentScope with
                | x when x = anyScope -> true
                | s -> List.exists s.IsOfScope xs
        let usedKey = List.contains key allUsedKeys
        match validScope, usedKey with
        | true, true -> 100
        | true, false -> 50
        | false, true -> 10
        | false, false -> 1

    let complete (pos : pos) (entity : Entity) (scopeContext : ScopeContext option) =
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
            |(MultipleKeys (keys, shouldMatch)) ->
                (keys |> List.exists ((==) s)) <> (not shouldMatch)

        let pathFilteredTypes = typedefs |> List.filter (fun t -> FieldValidators.checkPathDir t.pathOptions pathDir file)
        let getCompletion typerules fixedpath = getCompletionFromPath typerules fixedpath
        let allUsedKeys = getAllKeysInFile entity.entity @ globalScriptVariables
        let scoreFunction = scoreFunction allUsedKeys
        let rec validateTypeSkipRoot (t : TypeDefinition) (skipRootKeyStack : SkipRootKey list) (path : (string * int * string option * CompletionContext) list) =
            let typerules = typeRules |> List.choose (function |(name, typerule) when name == t.name -> Some typerule |_ -> None)
            match skipRootKeyStack, t.type_per_file, path with
            |_, false, [] ->
                getCompletionFromPath scoreFunction typerules [] scopeContext
            |_, true, (head, c, b, nt)::tail ->
                // getCompletionFromPath scoreFunction typerules ((head, c, b, nt)::tail) scopeContext
                getCompletionFromPath scoreFunction typerules ((t.name, 1, None, NodeRHS)::(head, c, b, nt)::tail) scopeContext
            |_, true, [] ->
                getCompletionFromPath scoreFunction typerules ([t.name, 1, None, NodeRHS]) scopeContext
            |[], false, (head, c, _, _)::tail ->
                if FieldValidators.typekeyfilter t head
                then
                    getCompletionFromPath scoreFunction typerules ((t.name, c, None, NodeRHS)::tail) scopeContext else []
            |head::tail, false, (pathhead, _, _,_)::pathtail ->
                if skiprootkey head pathhead
                then validateTypeSkipRoot t tail pathtail
                else []
        let items =
            match path |> List.tryLast, path.Length with
            |Some (_, count, Some x, _), _ when x.Length > 0 && x.StartsWith("@x") ->
                let staticVars = CWTools.Validation.Stellaris.STLValidation.getDefinedVariables entity.entity
                staticVars |> List.map (fun s -> CompletionResponse.CreateSimple (s))
            |Some (_, _, _, CompletionContext.NodeLHS), 1 ->
                []
            | _ ->
                pathFilteredTypes |> List.collect (fun t -> validateTypeSkipRoot t t.skipRootKey path)
        //TODO: Expand this to use a snippet not just the name of the type
        let rootTypeItems =
            match path with
            | [(_,_,_,CompletionContext.NodeLHS)] ->
                pathFilteredTypes |> List.map (fun t -> t.name |> CompletionResponse.CreateSimple )
            | y when y.Length = 0 ->
                pathFilteredTypes |> List.map (fun t -> t.name |> CompletionResponse.CreateSimple )
            | _ -> []
        // eprintfn "%A" path
        // eprintfn "%A" rootTypeItems
        let scoreForLabel (label : string) =
            if allUsedKeys |> List.contains label then 10 else 1
        (items @ rootTypeItems) |> List.map
                    (function
                     | Simple (label, None) -> Simple (label, Some (scoreForLabel label))
                     | Detailed (label, desc, None) -> Detailed (label, desc, Some (scoreForLabel label))
                     | Snippet (label, snippet, desc, None) -> Snippet(label, snippet, desc, Some (scoreForLabel label))
                     | x -> x
                     )



    member __.Complete(pos : pos, entity : Entity, scopeContext) = complete pos entity scopeContext
    member __.LocalisationComplete(pos : pos, filetext : string) = locComplete pos filetext

