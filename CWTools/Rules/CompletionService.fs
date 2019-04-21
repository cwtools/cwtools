namespace CWTools.Rules
open CWTools.Common
open Microsoft.FSharp.Collections.Tagged
open CWTools.Utilities.Utils
open CWTools.Process
open CWTools.Process.Scopes
open CWTools.Process.ProcessCore
open CWTools.Utilities
open System
open CWTools.Games
open CWTools.Utilities.Position
open System.IO

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
            wildcardLinks = wildCardLinks
        }
        let ctx = { subtypes = []; scopes = defaultContext; warningOnly = false }
        let severity = Severity.Error

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
                match expandedRules |> List.choose (function | (NodeRule (l, rs), o) when FieldValidators.checkFieldByKey p severity ctx l (StringResource.stringManager.InternIdentifierToken key).lower key -> Some (l, rs, o) | _ -> None) with
                | [] -> expandedRules |> List.collect (convRuleToCompletion count scopeContext)
                | fs -> fs |> List.collect (fun (_, innerRules, _) -> findRule innerRules rest scopeContext)
            | (key, count, Some _)::rest ->
                match expandedRules |> List.choose (function | (LeafRule (l, r), o) when FieldValidators.checkFieldByKey p severity ctx l (StringResource.stringManager.InternIdentifierToken key).lower key -> Some (l, r, o) | _ -> None) with
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
                if FieldValidators.typekeyfilter t head
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

