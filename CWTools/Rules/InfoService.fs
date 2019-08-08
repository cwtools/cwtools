namespace CWTools.Rules
open Microsoft.FSharp.Collections.Tagged
open CWTools.Utilities.Utils
open CWTools.Common
open System
open CWTools.Utilities.Position
open System.IO
open CWTools.Games
open CWTools.Process
open CWTools.Process.Scopes
open CWTools.Validation
open CWTools.Validation.ValidationCore

type InfoService<'T when 'T :> IScope<'T> and 'T : equality and 'T : comparison>
                                    (rootRules : RootRule<'T> list, typedefs : TypeDefinition<'T> list , types : Collections.Map<string, StringSet>,
                                     enums : Collections.Map<string, string * StringSet>, varMap : Collections.Map<string, StringSet>,
                                     localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>,
                                     links : Map<string,Effect<'T>,InsensitiveStringComparer>,
                                     valueTriggers : Map<string,Effect<'T>,InsensitiveStringComparer>,
                                     ruleValidationService : RuleValidationService<'T>, changeScope, defaultContext, anyScope, defaultLang) =
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
    let defaultKeys = localisation |> List.choose (fun (l, ks) -> if l = defaultLang then Some ks else None) |> List.tryHead |> Option.defaultValue Set.empty
    let localisationKeys = localisation |> List.choose (fun (l, ks) -> if l = defaultLang then None else Some (l, ks))

    let monitor = new Object()

    let memoizeRulesInner memFunction =
        // let dict = new System.Runtime.CompilerServices.ConditionalWeakTable<_,System.Collections.Generic.Dictionary<_,_>>()
        let dict = new System.Collections.Generic.Dictionary<_,System.Collections.Generic.Dictionary<_,_>>()
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
                // eprintfn "%A %A," (rules.GetHashCode()) (subtypes.GetHashCode())
                /// All subtypes in this context
                let subtypedrules =
                    rules |> List.collect (fun (r,o) -> r |> (function |SubtypeRule (_, _, cfs) -> cfs | x -> []))
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

    let rec singleInfoService fNode fChild fLeaf fLeafValue fValueClause fComment acc child rule :'r =
        let recurse = singleInfoService fNode fChild fLeaf fLeafValue fValueClause fComment
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

    let rec infoService fNode fChild fLeaf fLeafValue fValueClause fComment acc child rule :'r =
        let recurse = infoService fNode fChild fLeaf fLeafValue fValueClause fComment
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

    // let rec infoServiceEarlyExit fNode fChild fLeaf fLeafValue fComment acc child rule :'r =
    //     let recurse = infoServiceEarlyExit fNode fChild fLeaf fLeafValue fComment
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

            match childMatch, leafMatch, leafValueMatch with
            |Some c, _, _ ->
                match expandedrules |> List.tryPick (function |(NodeRule (l, rs), o) when FieldValidators.checkLeftField p Severity.Error ctx l c.KeyId.lower c.Key -> Some (l, rs, o) |_ -> None) with
                | None ->
                        // log "fallback match %s %A" (node.Key) expandedrules
                        Some (NodeC c, (field, options))
                | Some (l, rs, o) -> Some (NodeC c, ((NodeRule (l, rs)), o))
            |_, Some leaf, _ ->
                match expandedrules |> List.tryPick (function |(LeafRule (l, r), o) when FieldValidators.checkLeftField p Severity.Error ctx l leaf.KeyId.lower leaf.Key -> Some (l, r, o) |_ -> None) with
                |None ->
                    Some (LeafC leaf, (field, options))
                |Some (l, rs, o) -> Some (LeafC leaf, ((LeafRule (l, rs)), o))
            |_, _, Some lv -> Some (LeafValueC lv, (field, options))
            |None, None, None -> None
        let pathDir = (Path.GetDirectoryName logicalpath).Replace("\\","/")
        let file = Path.GetFileName logicalpath
        let childMatch = node.Children |> List.tryFind (fun c -> rangeContainsPos c.Position pos)
        // eprintfn "%O %A %A" pos pathDir (typedefs |> List.tryHead)
        // let rec skipRootKeySkipper
        let skiprootkey (skipRootKey : SkipRootKey) (n : Node) =
            match skipRootKey with
            |(SpecificKey key) -> n.Key == key
            |(AnyKey) -> true
            |(MultipleKeys (keys, shouldMatch)) ->
                (keys |> List.exists ((==) n.Key)) <> (not shouldMatch)

        let rec foldAtPosSkipRoot rs o (t : TypeDefinition<_>) (skipRootKeyStack : SkipRootKey list) acc (n : Node) =
            match skipRootKeyStack with
            |[] ->
                if FieldValidators.typekeyfilter t n.Key
                then Some (singleInfoService fNode fChild fLeaf fLeafValue fValueClause fComment acc (NodeC n) ((NodeRule (TypeMarkerField (n.KeyId.lower, t), rs), o)))
                else None
            |head::tail ->
                if skiprootkey head n
                then node.Children |> List.tryFind (fun c -> rangeContainsPos c.Position pos) |> Option.bind (foldAtPosSkipRoot rs o t tail acc)
                else None

        match childMatch, typedefs |> List.tryFind (fun t -> FieldValidators.checkPathDir t pathDir file) with
        |Some c, Some typedef ->
            let typerules = typeRules |> List.filter (fun (name, _) -> name == typedef.name)
            match typerules, typedef.type_per_file with
            |[(n, (NodeRule (l, rs), o))], false ->
                foldAtPosSkipRoot rs o typedef typedef.skipRootKey acc c
            |[(n, (NodeRule (l, rs), o))], true ->
                Some (singleInfoService fNode fChild fLeaf fLeafValue fValueClause fComment acc (NodeC node) ((NodeRule (TypeMarkerField (node.KeyId.lower, typedef), rs), o)))

            |_ -> None
        |None, Some typedef when typedef.type_per_file ->
            let typerules = typeRules |> List.filter (fun (name, _) -> name == typedef.name)
            match typerules with
            | [(n, (NodeRule (l, rs), o))] ->
                Some (singleInfoService fNode fChild fLeaf fLeafValue fValueClause fComment acc (NodeC node) ((NodeRule (TypeMarkerField (node.KeyId.lower, typedef), rs), o)))
            | _ -> None
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
                let pushScope, subtypes = ruleValidationService.TestSubType (typedef.subtypes, c)
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

            let inner (child : Child) =
                match child with
                | NodeC c ->
                    let key = c.Key
                    let keyId = c.KeyId.lower
                    noderules |> Seq.choose (fun (l, rs, o) -> if FieldValidators.checkLeftField p Severity.Error ctx l keyId key then Some (NodeC c, ((NodeRule (l, rs)), o)) else None)
                | ValueClauseC vc ->
                    valueclauserules |> Seq.map (fun (rs, o) -> ValueClauseC vc, ((ValueClauseRule (rs)), o))
                | LeafC leaf ->
                    let key = leaf.Key
                    let keyId = leaf.KeyId.lower
                    leafrules |> Seq.choose (fun (l, r, o) -> if FieldValidators.checkLeftField p Severity.Error ctx l keyId key then Some (LeafC leaf, ((LeafRule (l, r)), o)) else None)
                | LeafValueC leafvalue ->
                    let key = leafvalue.Key
                    let keyId = leafvalue.ValueId.lower
                    leafvaluerules |> Seq.choose (fun (lv, o) -> if FieldValidators.checkLeftField p Severity.Error ctx lv keyId key then  Some (LeafValueC leafvalue, ((LeafValueRule (lv)), o)) else None)
                | CommentC _ -> Seq.empty
            node.AllArray |> Seq.collect inner

        let pathDir = (Path.GetDirectoryName path).Replace("\\","/")
        let file = Path.GetFileName path
        let skiprootkey (skipRootKey : SkipRootKey) (n : Node) =
            match skipRootKey with
            |(SpecificKey key) -> n.Key == key
            |(AnyKey) -> true
            |(MultipleKeys (keys, shouldMatch)) ->
                (keys |> List.exists ((==) n.Key)) <> (not shouldMatch)

        let infoServiceNode typedef rs o =
            (fun a c ->
                infoService fNode fChild fLeaf fLeafValue fValueClause fComment a (NodeC c) (NodeRule (TypeMarkerField (c.KeyId.lower, typedef), rs), o))
        let pathFilteredTypes = typedefs |> List.filter (fun t -> FieldValidators.checkPathDir t pathDir file)
        let rec infoServiceSkipRoot rs o (t : TypeDefinition<_>) (skipRootKeyStack : SkipRootKey list) acc (n : Node) =
            match skipRootKeyStack with
            |[] -> if FieldValidators.typekeyfilter t n.Key then infoServiceNode t rs o acc n else acc
            |head::tail ->
                if skiprootkey head n
                then n.Nodes |> Seq.fold (infoServiceSkipRoot rs o t tail) acc
                else acc
        let infoServiceBase (n : Node) acc (t : TypeDefinition<_>) =
            let typerules = typeRules |> List.filter (fun (name, _) -> name == t.name)
            match typerules, t.type_per_file with
            |[(_, (NodeRule (_, rs), o))], false  ->
                n.Nodes |> Seq.fold (infoServiceSkipRoot rs o t t.skipRootKey) acc
            |[(_, (NodeRule (_, rs), o))], true  ->
                infoServiceSkipRoot rs o t t.skipRootKey acc n
            |_ -> acc
        pathFilteredTypes |> List.fold (infoServiceBase node) acc

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
    //     let infoServiceNode typedef rs o =
    //         (fun a c ->
    //             infoServiceEarlyExit fNode fChild fLeaf fLeafValue fComment a (NodeC c) (NodeRule (TypeMarkerField (c.KeyId.lower, typedef), rs), o))
    //     let pathFilteredTypes = typedefs |> List.filter (fun t -> checkPathDir t pathDir file)
    //     let rec infoServiceSkipRoot rs o (t : TypeDefinition<_>) (skipRootKeyStack : SkipRootKey list) acc (n : Node) =
    //         match skipRootKeyStack with
    //         |[] -> if typekeyfilter t n then infoServiceNode t rs o acc n else acc
    //         |head::tail ->
    //             if skiprootkey head n
    //             then n.Children |> List.fold (infoServiceSkipRoot rs o t tail) acc
    //             else acc
    //     let infoServiceBase (n : Node) acc (t : TypeDefinition<_>) =
    //         let typerules = typeRules |> List.filter (fun (name, _) -> name == t.name)
    //         match typerules with
    //         |[(_, (NodeRule (_, rs), o))] ->
    //             n.Children |> List.fold (infoServiceSkipRoot rs o t t.skipRootKey) acc
    //         |_ -> acc
    //     pathFilteredTypes |> List.fold (infoServiceBase node) acc

    let getTypesInEntity = // (entity : Entity) =
        let res = Collections.Generic.Dictionary<string ,ResizeArray<string * range>>()
        let fLeaf (_) (leaf : Leaf) ((field, _) : NewRule<_>) =
            match field with
            |LeafRule (_, TypeField (TypeType.Simple t)) ->
            // |Field.TypeField t ->
                let typename = t.Split('.').[0]
                if res.ContainsKey(typename)
                then res.[typename].Add(leaf.ValueText, leaf.Position); res
                else
                    let newArr = ResizeArray<string * range>()
                    newArr.Add((leaf.ValueText, leaf.Position))
                    res.Add(typename, newArr)
                    res
                // res |> (fun m -> m.Add(typename, (leaf.ValueText, leaf.Position)::(m.TryFind(typename) |> Option.defaultValue [])))
            |LeafRule (TypeField (TypeType.Simple t), _) ->
            // |Field.TypeField t ->
                let typename = t.Split('.').[0]
                if res.ContainsKey(typename)
                then res.[typename].Add(leaf.Key, leaf.Position); res
                else
                    let newArr = ResizeArray<string * range>()
                    newArr.Add((leaf.Key, leaf.Position))
                    res.Add(typename, newArr)
                    res
            |_ -> res
        let fLeafValue (_) (leafvalue : LeafValue) (field, _) =
            match field with
            |LeafValueRule (TypeField (TypeType.Simple t)) ->
            // |Field.TypeField t ->
                let typename = t.Split('.').[0]
                if res.ContainsKey(typename)
                then res.[typename].Add(leafvalue.ValueText, leafvalue.Position); res
                else
                    let newArr = ResizeArray<string * range>()
                    newArr.Add((leafvalue.ValueText, leafvalue.Position))
                    res.Add(typename, newArr)
                    res
            |_ -> res

        let fComment (_) _ _ = res
        let fNode (_) (node : Node) ((field, option) : NewRule<_>) = res
        let fValueClause (_) _ _ = res
        let fCombine a b = (a |> List.choose id) @ (b |> List.choose id)

        // let ctx = typedefs |> List.fold (fun (a : Collections.Map<string, (string * range) list>) t -> a.Add(t.name, [])) Collections.Map.empty
        fLeaf, fLeafValue, fComment, fNode, fValueClause, res
        // let res = foldCollect fLeaf fLeafValue fComment fNode ctx (entity.entity) (entity.logicalpath)
        // res

    let getDefVarInEntity = //(ctx : Collections.Map<string, (string * range) list>) (entity : Entity) =
        let getVariableFromString (v : string) (s : string) = if v = "variable" then s.Split('@').[0].Split('.') |> Array.last else s.Split('@').[0]
        let fLeaf (res : Collections.Map<string, ResizeArray<(string * range)>>) (leaf : Leaf) ((field, _) : NewRule<_>) =
            match field with
            |LeafRule (_, VariableSetField v) ->
                res |> (fun m -> m.Add(v, (m.TryFind(v) |> Option.defaultValue (new ResizeArray<_>()) |> (fun i -> i.Add((getVariableFromString v (leaf.ValueText), leaf.Position)); i))) )
            |LeafRule (VariableSetField v, _) ->
                res |> (fun m -> m.Add(v, (m.TryFind(v) |> Option.defaultValue (new ResizeArray<_>()) |> (fun i -> i.Add(((getVariableFromString v leaf.Key, leaf.Position))); i))) )
            |_ -> res
        let fLeafValue (res : Collections.Map<string, ResizeArray<(string * range)>>) (leafvalue : LeafValue) (field, _) =
            match field with
            |LeafValueRule (VariableSetField v) ->
                res |> (fun m -> m.Add(v, (m.TryFind(v) |> Option.defaultValue (new ResizeArray<_>()) |> (fun i -> i.Add((getVariableFromString v (leafvalue.ValueText), leafvalue.Position)); i )) ))
            |_ -> res
        let fNode (res : Collections.Map<string, ResizeArray<(string * range)>>) (node : Node) ((field, option) : NewRule<_>) =
            match field with
            |NodeRule (VariableSetField v, _) ->
                res |> (fun m -> m.Add(v, (m.TryFind(v) |> Option.defaultValue (new ResizeArray<_>()) |> (fun i -> i.Add((getVariableFromString v node.Key, node.Position)); i) )))
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
        // TODO: Consider adding a case for "non-trigger rule after trigger rule" to reset for inner
            match finished, field with
            |false, NodeRule (_, rs) when rs |> List.exists (function |LeafRule (AliasField "trigger" , _), _-> true |_ -> false) ->
                node::res, true
            |false, _ -> res, false
            |true, _ -> res, true
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
        let (types), (defvars, (effects, triggers)) = foldCollect fLeaf fLeafValue fComment fNode fValueClause ctx (entity.entity) (entity.logicalpath)
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
            |LeafRule (LocalisationField synced, _) ->
                FieldValidators.checkLocalisationField p.localisation p.defaultLocalisation p.defaultLang synced leaf.Key leaf res
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
            |NodeRule (LocalisationField synced, _) ->
                FieldValidators.checkLocalisationField p.localisation p.defaultLocalisation p.defaultLang synced node.Key node res
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

// type InfoService(rootRules : RootRule list, typedefs : TypeDefinition list , types : Collections.Map<string, (string * range) list>, enums : Collections.Map<string, string list>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Effect list, effects : Effect list, ruleValidationService : RuleValidationService) =

