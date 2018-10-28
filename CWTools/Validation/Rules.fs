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

type IRuleApplicator<'S> =
    abstract ApplyNodeRule : NewRule<'S> list * Node -> ValidationResult
    abstract TestSubtype : SubTypeDefinition<'S> list * Node -> 'S option * string list
    abstract RuleValidate : unit -> StructureValidator<'T>
type RuleContext< ^T when ^T : (static member AnyScope : ^T) and ^T : comparison> =
        {
            subtypes : string list
            scopes : ScopeContext< ^T>
            warningOnly : bool
        }
module rec Rules =
    type StringSet = Set<string, InsensitiveStringComparer>

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
    let firstCharEquals (c : char) = (fun s -> s |> Seq.tryHead |> Option.map ((=) c) |> Option.defaultValue false)

    let inline checkValidValue (enumsMap : Collections.Map<_, Set<_, _>>) (severity : Severity) (vt : CWTools.Parser.ConfigParser.ValueType) (key : string) leafornode =
        if key |> firstCharEquals '@' then OK else
            match (vt) with
            |ValueType.Bool ->
                if key = "yes" || key = "no" then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting yes or no, got %s" key) severity) leafornode]
            |ValueType.Enum e ->
                match enumsMap.TryFind e with
                |Some es -> if es.Contains (key.Trim([|'\"'|])) then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a \"%s\" value, e.g. %A" e es) severity) leafornode]
                |None -> OK
            |ValueType.Float (min, max) ->
                match TryParser.parseDouble key with
                |Some f -> if f <= max && f >= min then OK else Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a value between %f and %f" min max) severity) leafornode]
                |None -> Invalid[inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting a float, got %s" key) severity) leafornode]
            |ValueType.Int (min, max) ->
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
            CWTools.Validation.Stellaris.STLLocalisationValidation.checkLocName leafornode defaultKeys (STL STLLang.Default) key
        |false ->
            CWTools.Validation.Stellaris.STLLocalisationValidation.checkLocKeysLeafOrNode keys key leafornode

    let inline checkTypeField (typesMap : Collections.Map<_,Set<_, _>>) severity t (key : string) leafornode =
        match typesMap.TryFind t with
        |Some values ->
            let value = key.Trim([|'\"'|])
            if value |> firstCharEquals '@' then OK else
            if values.Contains value then OK else Invalid [inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected value of type %s" t) severity) leafornode]
        |None -> Invalid [inv (ErrorCodes.CustomError (sprintf "Unknown type referenced %s" t) Severity.Error) leafornode]


    let inline checkFilepathField (files : Collections.Set<string>) (key : string) (leafornode) =
        let file = key.Trim('"').Replace("\\","/").Replace(".lua",".shader").Replace(".tga",".dds")
        if files.Contains file then OK else Invalid [inv (ErrorCodes.MissingFile file) leafornode]

    let inline checkIconField (files :Collections.Set<string>) (folder : string) (key : string) (leafornode) =
        let value = folder + "/" + key + ".dds"
        if files.Contains value then OK else Invalid [inv (ErrorCodes.MissingFile value) leafornode]

    let inline checkScopeField (effectMap : Map<_,_,_>) (triggerMap : Map<_,_,_>) changeScope anyScope (ctx : RuleContext<_>) (s : ^a )  key leafornode =
        // eprintfn "scope %s %A"key ctx
        let scope = ctx.scopes
        match changeScope true effectMap triggerMap key scope with
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = ( ^a : (static member AnyScope : ^a) ()) || current = ( ^a : (static member AnyScope : ^a) ()) then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then OK else Invalid [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode]
        |NotFound _ -> Invalid [inv (ErrorCodes.ConfigRulesInvalidTarget (s.ToString())) leafornode]
        |WrongScope (command, prevscope, expected) -> Invalid [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) leafornode]
        |_ -> OK


    let inline checkField enumsMap typesMap effectMap triggerMap localisation files changeScope anyScope (severity : Severity) (ctx : RuleContext<_>) (field : NewField<_>) (key : string) (leafornode : ^a) =
        match field with
        |ValueField vt -> checkValidValue enumsMap severity vt key leafornode
        |TypeField t -> checkTypeField typesMap severity t key leafornode
        |ScopeField s -> checkScopeField effectMap triggerMap changeScope anyScope ctx s key leafornode
        |LocalisationField synced -> checkLocalisationField localisation synced key leafornode
        |FilepathField -> checkFilepathField files key leafornode
        |IconField folder -> checkIconField files folder key leafornode
        |_ -> OK

    let inline checkLeftField enumsMap typesMap effectMap triggerMap localisation files changeScope anyScope (ctx : RuleContext<_>) (field : NewField<_>) (key : string) (leafornode : ^a) =
        match checkField enumsMap typesMap effectMap triggerMap localisation files changeScope anyScope (Severity.Error) ctx field key leafornode with
        |OK -> true
        |_ -> false

    let inline checkFieldByKey enumsMap typesMap effectMap triggerMap localisation files changeScope anyScope (ctx : RuleContext<_>) (field : NewField<_>) (key : string) =
        let leaf = LeafValue(Value.String key)
        checkLeftField enumsMap typesMap effectMap triggerMap localisation files changeScope anyScope ctx field key leaf



    // type FoldRules(rootRules : RootRule<_> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>, enums : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<_>,InsensitiveStringComparer>, effects : Map<string,Effect<_>,InsensitiveStringComparer>, ruleApplicator : IRuleApplicator<_>, changeScope, defaultContext, anyScope) =
    let inline foldRules (rootRules : RootRule<_> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>, enums : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<_>,InsensitiveStringComparer>, effects : Map<string,Effect<_>,InsensitiveStringComparer>, ruleApplicator : IRuleApplicator<_>, changeScope, defaultContext, anyScope) =
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
            let fChild (node : Node) ((field, options) : NewRule<_>) =
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
                let ctx = { RuleContext.subtypes = []; scopes = defaultContext; warningOnly = false }
                match childMatch, leafMatch, leafValueMatch with
                |Some c, _, _ ->
                    match expandedrules |> List.choose (function |(NodeRule (l, rs), o) when checkLeftField enumsMap typesMap effectMap triggerMap localisation files changeScope anyScope ctx l c.Key c -> Some (l, rs, o) |_ -> None) with
                    | [] ->
                            // eprintfn "fallback match %s %A" (node.Key) expandedrules
                            Some (NodeC c, (field, options))
                    | (l, rs, o)::_ -> Some (NodeC c, ((NodeRule (l, rs)), o))
                |_, Some leaf, _ ->
                    match expandedrules |> List.choose (function |(LeafRule (l, r), o) when checkLeftField enumsMap typesMap effectMap triggerMap localisation files changeScope anyScope ctx l leaf.Key leaf -> Some (l, r, o) |_ -> None) with
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
                |LeafRule (_, TypeField t) -> ctx, Some (t, leaf.Value.ToString())
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
                        match changeScope true effectMap triggerMap key scope with
                        |NewScope ({Scopes = current::_} ,_) ->
                            //eprintfn "cs %A %A %A" name node.Key current
                            {newCtx with scopes = {newCtx.scopes with Scopes = current::newCtx.scopes.Scopes}}
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
                    let pushScope, subtypes = ruleApplicator.TestSubtype (typedef.subtypes, c)
                    match pushScope with
                    |Some ps -> { subtypes = subtypes; scopes = { Root = ps; From = []; Scopes = [ps] }; warningOnly = false}
                    |None -> { subtypes = subtypes; scopes = defaultContext; warningOnly = false }
                |_, _ -> { subtypes = []; scopes = defaultContext; warningOnly = false }

            let ctx = ctx, None
            foldWithPos fLeaf fLeafValue fComment fNode ctx (pos) (entity.entity) (entity.logicalpath)


        let foldCollect fLeaf fLeafValue fComment fNode acc (node : Node) (path: string) =
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
                        | (NodeRule((AliasField a),_), _) -> (aliases.TryFind a |> Option.defaultValue []))
                       // |x -> [x])
                // let expandedrules =
                //     subtypedrules |> List.collect (
                //         function
                //         | _,_,(AliasField a) -> (aliases.TryFind a |> Option.defaultValue [])
                //         |x -> [x])
                let ctx = { subtypes = []; scopes = defaultContext; warningOnly = false  }
                let innerN (c : Node) =
                    match expandedrules |> List.choose (function |(NodeRule (l, rs), o) when checkLeftField enumsMap typesMap effectMap triggerMap localisation files changeScope anyScope ctx l c.Key c -> Some (l, rs, o) |_ -> None) with
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
                    match expandedrules |> List.choose (function |(LeafRule (l, r), o) when checkLeftField enumsMap typesMap effectMap triggerMap localisation files changeScope anyScope ctx l leaf.Key leaf -> Some (l, r, o) |_ -> None) with
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
            let file = Path.GetFileName path
            match typedefs |> List.tryFind (fun t -> checkPathDir t pathDir file) with
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
                |LeafRule (_, TypeField t) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    res |> (fun m -> m.Add(typename, (leaf.Value.ToRawString(), leaf.Position)::(m.TryFind(typename) |> Option.defaultValue [])))
                |LeafRule (TypeField t, _) ->
                // |Field.TypeField t ->
                    let typename = t.Split('.').[0]
                    res |> (fun m -> m.Add(typename, (leaf.Key, leaf.Position)::(m.TryFind(typename) |> Option.defaultValue [])))
                |_ -> res
            let fLeafValue (res : Collections.Map<string, (string * range) list>) (leafvalue : LeafValue) (field, _) =
                match field with
                |LeafValueRule (TypeField t) ->
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
        let convertToOutput s =
            {
                OutputScopeContext.From = s.From |> List.map (fun f -> f :> obj :?> 'T2)
                Scopes = s.Scopes |> List.map (fun f -> f :> obj :?> 'T2)
                Root = s.Root :> obj :?> 'T2
            }
        let test pos entity = //{OutputScopeContext.From = []; Scopes = []; Root = [] :> obj}
            (getInfoAtPos pos entity ) |> Option.map (fun (s, r) -> (convertToOutput s.scopes), r)
        ((fun (pos, entity) -> (getInfoAtPos pos entity) |> Option.map (fun (p, e) -> p.scopes, e)), (fun (entity) -> getTypesInEntity entity))
        // member __.GetInfo(pos : pos, entity : Entity) = (getInfoAtPos pos entity )
        // member __.GetReferencedTypes(entity : Entity) = getTypesInEntity entity

    // type FoldRules(rootRules : RootRule list, typedefs : TypeDefinition list , types : Collections.Map<string, (string * range) list>, enums : Collections.Map<string, string list>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Effect list, effects : Effect list, ruleApplicator : RuleApplicator) =

    let inline completionServiceCreator(rootRules : RootRule<_> list, typedefs : TypeDefinition<_> list , types : Collections.Map<string, StringSet>, enums : Collections.Map<string, StringSet>, localisation : (Lang * Collections.Set<string>) list, files : Collections.Set<string>, triggers : Map<string,Effect<_>,InsensitiveStringComparer>, effects : Map<string,Effect<_>,InsensitiveStringComparer>, changeScope, defaultContext, anyScope)  =
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

        let fieldToCompletionList (field : NewField<_>) =
            match field with
            |ValueField (Enum e) -> enums.TryFind(e) |> Option.bind (fun s -> if s.IsEmpty then None else Some (s.MaximumElement)) |> Option.defaultValue "x"
            |ValueField v -> getValidValues v |> Option.bind (List.tryHead) |> Option.defaultValue "x"
            |TypeField t -> types.TryFind(t) |> Option.bind (List.tryHead) |> Option.defaultValue "x"
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
            let rulePrint (i : int) =
                function
                |LeafRule(ValueField(ValueType.Specific s), r) ->
                    sprintf "\t%s = ${%i:%s}\n" s (i + 1) (fieldToCompletionList r)
                |NodeRule(ValueField(ValueType.Specific s), _) ->
                    sprintf "\t%s = ${%i:%s}\n" s (i + 1) "{ }"
                |_ -> ""

            let requiredRules = rules |> List.filter (fun (f, o) -> o.min >= 1 && filterToCompletion f)
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
                |NodeRule (ScopeField(_), _) -> [] //TODO: Scopes
                |NodeRule (SubtypeField(_), _) -> []
                |NodeRule (TypeField(t), innerRules) ->
                    types.TryFind(t) |> Option.map (fun ts -> ts |> List.map (fun e -> createSnippetForClause innerRules o.description e)) |> Option.defaultValue []
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
                |LeafRule (TypeField(t), _) ->
                    types.TryFind(t) |> Option.map (fun ts -> ts |> List.map (fun e -> keyvalue e)) |> Option.defaultValue []

                |LeafValueRule lv ->
                    match lv with
                    |NewField.TypeField t -> types.TryFind(t) |> Option.defaultValue [] |> List.map Simple
                    |NewField.ValueField (Enum e) -> enums.TryFind(e) |> Option.map (fun s -> s.ToList()) |> Option.defaultValue [] |> List.map Simple
                    |_ -> []
                |SubtypeRule(_) -> []
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
            let fieldToRules (field : NewField<_>) =
                //eprintfn "%A" types
                //eprintfn "%A" field
                match field with
                |NewField.ValueField (Enum e) -> enums.TryFind(e) |> Option.map (fun s -> s.ToList()) |> Option.defaultValue [] |> List.map Simple
                |NewField.ValueField v -> getValidValues v |> Option.defaultValue [] |> List.map Simple
                |NewField.TypeField t -> types.TryFind(t) |> Option.defaultValue [] |> List.map Simple
                |NewField.LocalisationField s ->
                    match s with
                    |true -> localisation |> List.tryFind (fun (lang, _ ) -> lang = (STL STLLang.Default)) |> Option.map (snd >> Set.toList) |> Option.defaultValue [] |> List.map Simple
                    |false -> localisation |> List.tryFind (fun (lang, _ ) -> lang <> (STL STLLang.Default)) |> Option.map (snd >> Set.toList) |> Option.defaultValue [] |> List.map Simple
                |NewField.FilepathField -> files |> Set.toList |> List.map Simple
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
            let rec findRule (rules : NewRule<'a> list) (stack) =
                let expandedRules =
                    rules |> List.collect (
                        function
                        | LeafRule((AliasField a),_),_ -> (aliases.TryFind a |> Option.defaultValue [])
                        | NodeRule((AliasField a),_),_ -> (aliases.TryFind a |> Option.defaultValue [])
                        | SubtypeRule(_, _, cfs), _ -> cfs
                        |x -> [x])
                match stack with
                |[] -> expandedRules |> List.collect convRuleToCompletion
                |(key, false)::rest ->
                    match expandedRules |> List.choose (function |(NodeRule (l, rs), o) when checkFieldByKey enumsMap typesMap effectMap triggerMap localisation files changeScope anyScope { RuleContext.subtypes = []; scopes = defaultContext; warningOnly = false } l key -> Some (l, rs, o) |_ -> None) with
                    |[] -> expandedRules |> List.collect convRuleToCompletion
                    |fs -> fs |> List.collect (fun (_, innerRules, _) -> findRule innerRules rest)
                |(key, true)::rest ->
                    match expandedRules |> List.choose (function |(LeafRule (l, r), o) when checkFieldByKey enumsMap typesMap effectMap triggerMap localisation files changeScope anyScope { RuleContext.subtypes = []; scopes = defaultContext; warningOnly = false } l key -> Some (l, r, o) |_ -> None) with
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
                        let typerules = typeRules |> List.choose (function |(name, typerule) when name == typedef.name -> Some typerule |_ -> None)
                        //eprintfn "fc %A" path
                        let fixedpath = if List.isEmpty path then path else (typedef.name, false)::(path |> List.tail)
                        let completion = getCompletionFromPath typerules fixedpath
                        completion
                    |None -> getCompletionFromPath (typeRules |> List.map snd) path)
            //eprintfn "res3 %A" res
            res
        (fun (pos, entity) -> complete pos entity)
        // member inline __.Complete(pos : pos, entity : Entity) = complete pos entity

    let getTypesFromDefinitions (ruleapplicator : IRuleApplicator<_>) (types : TypeDefinition<_> list) (es : Entity list) =
        let entities = es |> List.map (fun e -> ((Path.GetDirectoryName e.logicalpath).Replace("\\","/")), e, (Path.GetFileName e.logicalpath))
        let getTypeInfo (def : TypeDefinition<_>) =
            entities |> List.choose (fun (path, e, file) -> if checkPathDir def path file then Some (e.entity, file) else None)
                     |> List.collect (fun (e, f) ->
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
                                match def.filenameName, def.skipRootKey with
                                |true, _ -> 
                                    let subtypes = ruleapplicator.TestSubtype(def.subtypes, e) |> snd |> List.map (fun s -> def.name + "." + s)
                                    def.name::subtypes |> List.map (fun s -> s, (Path.GetFileNameWithoutExtension f, e.Position))
                                |false, Some (SpecificKey key) ->
                                    e.Children |> List.filter (fun c -> c.Key == key) |> List.collect (fun c -> c.Children |> List.collect inner)
                                |false, Some (AnyKey) ->
                                    e.Children |> List.collect (fun c -> c.Children |> List.collect inner)
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
                    |None -> []
        let getEnumInfo (complexenum : ComplexEnumDef) =
            let cpath = complexenum.path.Replace("\\","/")
            let values = entities |> List.choose (fun (path, e) -> if path.StartsWith(cpath, StringComparison.OrdinalIgnoreCase) then Some e.entity else None)
                                  |> List.collect (fun e -> e.Children |> List.collect (inner complexenum.nameTree))
            complexenum.name, values
        complexenums |> List.toSeq |> PSeq.map getEnumInfo |> List.ofSeq