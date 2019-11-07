namespace CWTools.Process.Scopes

open System
open CWTools.Utilities.Position
open CWTools.Common
open CWTools.Utilities.Utils
open Microsoft.FSharp.Collections.Tagged
type EffectMap = Map<string, Effect, InsensitiveStringComparer>
type UsageScopeContext = Scope list
type ScopeContext =
    {
        Root : Scope
        From : Scope list
        Scopes : Scope list
    }
    member this.CurrentScope = match this.Scopes with |[] -> this.Root.AnyScope | x::_ -> x
    member this.PopScope : Scope list = match this.Scopes with |[] -> [] |_::xs -> xs
    member this.GetFrom i =
         if this.From.Length >= i then (this.From.Item (i - 1)) else this.Root.AnyScope


type LocContextResult =
    | Start of startContext : ScopeContext
    | NewScope of newScope : ScopeContext
    | WrongScope of command : string * scope : Scope * expected : Scope list
    //Not sure what this should be
    | Found of endContext : string
    | LocNotFound of key : string
    | LocNotFoundInType of key : string * dataType : string * confident : bool
    // Jomini loc
    | NewDataType of newDataType : string * confident : bool

// type JominiLocContextResult =
//     | Start of startDataType : string
//     | NotFound of key : string * context : string

[<Struct>]
type LocEntry = {
    key : string
    value : char option
    desc : string
    position : range
    scopes : LocContextResult list
    refs : string list
}
type JominiLocCommandParam =
    |Commands of JominiLocCommand list
    |Param of string
and JominiLocCommand =
    |Command of string * JominiLocCommandParam list


type ScopeResult =
    | NewScope of newScope : ScopeContext * ignoreKeys : string list
    | WrongScope of command : string * scope : Scope * expected : Scope list
    | NotFound
    | VarFound
    | VarNotFound of var : string
    | ValueFound

type ChangeScope = bool -> bool -> EffectMap -> EffectMap -> ScopedEffect list -> StringSet -> string -> ScopeContext -> ScopeResult

module Scopes =

    let defaultContext =
        { Root = scopeManager.AnyScope; From = []; Scopes = [] }
    let noneContext =
        { Root = scopeManager.InvalidScope; From = []; Scopes = [scopeManager.InvalidScope] }

    // type EffectMap<'T> = Map<string, Effect<'T>, InsensitiveStringComparer>
    let simpleVarPrefixFun prefix =
        let varStartsWith = (fun (k : string) -> k.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
        let varSubstring = (fun (k : string) -> k.Substring(prefix.Length ))
        (fun key -> if varStartsWith key then varSubstring key, true else key, false)

    let complexVarPrefixFun prefix1 prefix2 =
        let varStartsWith1 = (fun (k : string) -> k.StartsWith(prefix1, StringComparison.OrdinalIgnoreCase))
        let varStartsWith2 = (fun (k : string) -> k.StartsWith(prefix2, StringComparison.OrdinalIgnoreCase))
        let varSubstring1 = (fun (k : string) -> k.Substring(prefix1.Length ))
        let varSubstring2 = (fun (k : string) -> k.Substring(prefix2.Length ))
        (fun key -> if varStartsWith1 key then varSubstring1 key, true else if varStartsWith2 key then varSubstring2 key, true else key, false)

    let private applyTargetScope (scope : _ option) (context : _ list) =
        match scope with
        | None -> context
        | Some ps -> ps::context
    let createJominiChangeScope (oneToOneScopes) (varPrefixFun : string -> string * bool) =
        // let varStartsWith = (fun (k : string) -> k.StartsWith(varPrefix, StringComparison.OrdinalIgnoreCase))
        // let varSubstring = (fun (k : string) -> k.Substring(varPrefix.Length ))
        let amp = [|'@'|]
        (fun (varLHS : bool) (skipEffect : bool) (eventTargetLinks : EffectMap) (valueTriggers : EffectMap) (wildcardLinks : ScopedEffect list) (vars : StringSet) (key : string) (source : ScopeContext) ->
            let key = if key.StartsWith("hidden:", StringComparison.OrdinalIgnoreCase) then key.Substring(7) else key
            if
                key.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase)
                || key.StartsWith("parameter:", StringComparison.OrdinalIgnoreCase)
                || key.StartsWith("@", StringComparison.OrdinalIgnoreCase)
            then NewScope ({ Root = source.Root; From = source.From; Scopes = source.Root.AnyScope::source.Scopes }, [])
            else
                let key, varOnly = varPrefixFun key
                let beforeAmp, afterAmp, hasAmp =
                    if key.IndexOf('@') >= 0
                    then let x = key.Split(amp, 2) in x.[0], x.[1], true
                    else key, "", false
                // let ampersandSplit = if key.Contains('@') then key.Split(amp, 2) else
                let keys = key.Split('.')
                let keylength = keys.Length - 1
                let keys = keys |> Array.mapi (fun i k -> k, i = keylength)
                let inner ((context : ScopeContext), (changed : bool)) (nextKey : string) (last : bool) =
                    let onetoone = oneToOneScopes |> List.tryFind (fun (k, _) -> k == nextKey)
                    // eprintfn "o2o %A" onetoone
                    match onetoone with
                    | Some (_, f) -> f (context, false), NewScope (f (context, false) |> fst, [])
                    | None ->
                        // let effectMatch = effects.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e when not e.IsValueScope -> Some e |_ -> None)
                        // let triggerMatch = triggers.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e when not e.IsValueScope -> Some e |_ -> None)
                        let eventTargetLinkMatch = eventTargetLinks.TryFind nextKey |> Option.bind (function | :? ScopedEffect as e -> Some e |_ -> None)
                        let valueScopeMatch = valueTriggers.TryFind nextKey
                        let wildcardScopeMatch = wildcardLinks |> List.tryFind (fun l -> nextKey.StartsWith(l.Name, StringComparison.OrdinalIgnoreCase))
                        // let effect = (effects @ triggers)
                        //             |> List.choose (function | :? ScopedEffect as e -> Some e |_ -> None)
                        //             |> List.tryFind (fun e -> e.Name == nextKey)
                        // if skipEffect then (context, false), NotFound else
                        // eprintfn "vsm %A" valueScopeMatch
                        match eventTargetLinkMatch |> Option.orElse wildcardScopeMatch, valueScopeMatch with
                        | _, Some e ->
                            if last
                            then
                                let possibleScopes = e.Scopes
                                let currentScope = context.CurrentScope
                                let exact = possibleScopes |> List.exists (fun x -> currentScope.IsOfScope x)
                                match context.CurrentScope, possibleScopes, exact with
                                | x, _, _ when x = source.Root.AnyScope -> (context, false), ValueFound
                                | _, [], _ -> (context, false), NotFound
                                | _, _, true -> (context, false), ValueFound
                                | current, ss, false -> (context, false), WrongScope (nextKey, current, ss)
                            else
                                (context, false), NotFound
                        | None, _ ->
                            if last && vars.Contains nextKey
                            then
                                (context, false), VarFound
                            else
                                if varOnly
                                then
                                    (context, false), VarNotFound nextKey
                                else
                                    (context, false), NotFound
                        | Some e, _ ->
                            let possibleScopes = e.Scopes
                            let currentScope = context.CurrentScope
                            let exact = possibleScopes |> List.exists (fun x -> currentScope.IsOfScope x)
                            match context.CurrentScope, possibleScopes, exact, e.IsScopeChange with
                            | x, _, _, true when x = source.Root.AnyScope -> ({context with Scopes = applyTargetScope e.Target context.Scopes}, true), NewScope ({source with Scopes = applyTargetScope e.Target context.Scopes}, e.IgnoreChildren)
                            | x, _, _, false when x = source.Root.AnyScope-> (context, false), NewScope (context, e.IgnoreChildren)
                            | _, [], _, _ -> (context, false), NotFound
                            | _, _, true, true -> ({context with Scopes = applyTargetScope e.Target context.Scopes}, true), NewScope ({source with Scopes = applyTargetScope e.Target context.Scopes}, e.IgnoreChildren)
                            | _, _, true, false -> (context, false), NewScope (context, e.IgnoreChildren)
                            | current, ss, false, _ -> (context, false), WrongScope (nextKey, current, ss)
                let inner2 = fun a b l -> inner a b l |> (fun (c, d) -> c, Some d)
                let res = keys |> Array.fold (fun ((c,b), r) (k, l) -> match r with |None -> inner2 (c, b) k l |Some (NewScope (x, i)) -> inner2 (x, b) k l |Some x -> (c,b), Some x) ((source, false), None)// |> snd |> Option.defaultValue (NotFound)
                let res2 =
                    match res with
                    |(_, _), None -> NotFound
                    |(_, true), Some r -> r |> function |NewScope (x, i) -> NewScope ({ source with Scopes = x.CurrentScope::source.Scopes }, i) |x -> x
                    |(_, false), Some r -> r
                if hasAmp
                then
                    let keys = afterAmp.Split('.')
                    let keylength = keys.Length - 1
                    let keys = keys |> Array.mapi (fun i k -> k, i = keylength)
                    let tres = keys |> Array.fold (fun ((c,b), r) (k, l) -> match r with |None -> inner2 (c, b) k l |Some (NewScope (x, i)) -> inner2 (x, b) k l |Some x -> (c,b), Some x) ((source, false), None)// |> snd |> Option.defaultValue (NotFound)
                    let tres2 =
                        match tres with
                        |(_, _), None -> NotFound
                        |(_, true), Some r -> r |> function |NewScope (x, i) -> NewScope ({ source with Scopes = x.CurrentScope::source.Scopes }, i) |x -> x
                        |(_, false), Some r -> r
                    match res2, tres2 with
                    |_, NotFound -> NotFound
                    |_, VarNotFound s -> VarNotFound s
                    |VarFound, _ -> VarFound
                    |_, _ -> NotFound
                else
                // let x = res |> function |NewScope x -> NewScope { source with Scopes = x.CurrentScope::source.Scopes } |x -> x
                // x
                res2)

    let createChangeScope (oneToOneScopes) (varPrefixFun : string -> string * bool) (hoi4TargetedHardcodedVariables : bool) =
        // let varStartsWith = (fun (k : string) -> k.StartsWith(varPrefix, StringComparison.OrdinalIgnoreCase))
        // let varSubstring = (fun (k : string) -> k.Substring(varPrefix.Length ))
        (fun (varLHS : bool) (skipEffect : bool) (eventTargetLinks : EffectMap) (_ : EffectMap) (_ : ScopedEffect list) (vars : StringSet) (key : string) (source : ScopeContext) ->
            let key = if key.StartsWith("hidden:", StringComparison.OrdinalIgnoreCase) then key.Substring(7) else key
            if
                key.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase)
                || key.StartsWith("parameter:", StringComparison.OrdinalIgnoreCase)
                || key.StartsWith("@", StringComparison.OrdinalIgnoreCase)
            then NewScope ({ Root = source.Root; From = source.From; Scopes = source.Root.AnyScope::source.Scopes }, [])
            else
                let key, varOnly = varPrefixFun key
                let inner ((context : ScopeContext), (changed : bool)) (nextKey : string) (last : bool) =
                    let onetoone = oneToOneScopes |> List.tryFind (fun (k, _) -> k == nextKey)
                    match onetoone with
                    | Some (_, f) -> f (context, false), NewScope (f (context, false) |> fst, [])
                    | None ->
                        // let effectMatch = effects.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e when not e.IsValueScope  -> Some e |_ -> None)
                        // let triggerMatch = triggers.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e when not e.IsValueScope -> Some e |_ -> None)
                        let eventTargetLinkMatch = eventTargetLinks.TryFind nextKey |> Option.bind (function | :? ScopedEffect as e -> Some e |_ -> None)
                        // let effect = (effects @ triggers)
                        //             |> List.choose (function | :? ScopedEffect as e -> Some e |_ -> None)
                        //             |> List.tryFind (fun e -> e.Name == nextKey)
                        // if skipEffect then (context, false), NotFound else
                        match eventTargetLinkMatch with
                        | None ->
                            if last && (vars.Contains nextKey)
                            then
                                (context, false), VarFound
                            else
                                if varOnly
                                then
                                    (context, false), VarNotFound nextKey
                                else
                                    (context, false), NotFound
                        | Some e ->
                            let possibleScopes = e.Scopes
                            let currentScope = context.CurrentScope
                            let exact = possibleScopes |> List.exists (fun x -> currentScope.IsOfScope x)
                            match context.CurrentScope, possibleScopes, exact, e.IsScopeChange with
                            | x, _, _, true when x = source.Root.AnyScope -> ({context with Scopes = applyTargetScope e.Target context.Scopes}, true), NewScope ({source with Scopes = applyTargetScope e.Target context.Scopes}, e.IgnoreChildren)
                            | x, _, _, false when x = source.Root.AnyScope-> (context, false), NewScope (context, e.IgnoreChildren)
                            | _, [], _, _ -> (context, false), NotFound
                            | _, _, true, true -> ({context with Scopes = applyTargetScope e.Target context.Scopes}, true), NewScope ({source with Scopes = applyTargetScope e.Target context.Scopes}, e.IgnoreChildren)
                            | _, _, true, false -> (context, false), NewScope (context, e.IgnoreChildren)
                            | current, ss, false, _ -> (context, false), WrongScope (nextKey, current, ss)
                let inner2 = fun a b l -> inner a b l |> (fun (c, d) -> c, Some d)
                // Try just the raw string first
                let rawKeys = key.Split('.')
                let rawKeyLength = rawKeys.Length - 1
                let rawKeys = rawKeys |> Array.mapi (fun i k -> k, i = rawKeyLength)
                let rawRes2 =
                    if hoi4TargetedHardcodedVariables then
                        let rawRes = rawKeys |> Array.fold (fun ((c,b), r) (k, l) -> match r with |None -> inner2 (c, b) k l |Some (NewScope (x, i)) -> inner2 (x, b) k l |Some x -> (c,b), Some x) ((source, false), None)// |> snd |> Option.defaultValue (NotFound)
                        match rawRes with
                        |(_, _), None -> NotFound
                        |(_, true), Some r -> r |> function |NewScope (x, i) -> NewScope ({ source with Scopes = x.CurrentScope::source.Scopes }, i) |x -> x
                        |(_, false), Some r -> r
                    else NotFound
                match rawRes2 with
                | NotFound
                | VarNotFound _
                    ->

                    let ampersandSplit = key.Split([|'@'|], 2)
                    let keys = ampersandSplit.[0].Split('.')
                    let keylength = keys.Length - 1
                    let keys = keys |> Array.mapi (fun i k -> k, i = keylength)
                    let res = keys |> Array.fold (fun ((c,b), r) (k, l) -> match r with |None -> inner2 (c, b) k l |Some (NewScope (x, i)) -> inner2 (x, b) k l |Some x -> (c,b), Some x) ((source, false), None)// |> snd |> Option.defaultValue (NotFound)
                    let res2 =
                        match res with
                        |(_, _), None -> NotFound
                        |(_, true), Some r -> r |> function |NewScope (x, i) -> NewScope ({ source with Scopes = x.CurrentScope::source.Scopes }, i) |x -> x
                        |(_, false), Some r -> r
                    if ampersandSplit.Length > 1
                    then
                        if vars.Contains key then VarFound else
                        let keys = ampersandSplit.[1].Split('.')
                        let keylength = keys.Length - 1
                        let keys = keys |> Array.mapi (fun i k -> k, i = keylength)
                        let tres = keys |> Array.fold (fun ((c,b), r) (k, l) -> match r with |None -> inner2 (c, b) k l |Some (NewScope (x, i)) -> inner2 (x, b) k l |Some x -> (c,b), Some x) ((source, false), None)// |> snd |> Option.defaultValue (NotFound)
                        let tres2 =
                            match tres with
                            |(_, _), None -> NotFound
                            |(_, true), Some r -> r |> function |NewScope (x, i) -> NewScope ({ source with Scopes = x.CurrentScope::source.Scopes }, i) |x -> x
                            |(_, false), Some r -> r
                        match res2, tres2 with
                        |_, NotFound -> NotFound
                        |_, VarNotFound s -> VarNotFound s
                        |VarFound, _ -> VarFound
                        |_, _ -> NotFound
                    else
                    // let x = res |> function |NewScope x -> NewScope { source with Scopes = x.CurrentScope::source.Scopes } |x -> x
                    // x
                    res2
                | _ -> rawRes2)


    let createLocalisationCommandValidator (locPrimaryScopes : (string * (ScopeContext * bool -> ScopeContext * bool)) list) (scopedLocEffectsMap : EffectMap) =
        fun (commands : string list) (eventtargets : string list) (setvariables : string list) (extraOneToOne : string list) (source : ScopeContext) (command : string) ->
        let keys = command.Split('.') |> List.ofArray
        let inner ((first : bool), (context : ScopeContext)) (nextKey : string) =
            let onetooneMatch() =
                locPrimaryScopes |> List.tryFind (fun (k, _) -> k == nextKey)
                |> Option.map (fun (_, f) -> (LocContextResult.NewScope (f (context, false) |> fst)))
            let effectMatch() =
                scopedLocEffectsMap.TryFind nextKey |> Option.bind (function | :? ScopedEffect as e -> Some e |_ -> None)
                |> Option.map (fun e ->
                                    let validScopes = e.Scopes
                                    let currentScope = context.CurrentScope
                                    let exact = validScopes |> List.exists (fun x -> currentScope.IsOfScope x)
                                    match context.CurrentScope, validScopes, exact with
                                        | x, _, _ when x = context.Root.AnyScope -> (LocContextResult.NewScope ({source with Scopes =applyTargetScope e.Target context.Scopes}))
                                        // | _, [], _ -> (context, false), NotFound
                                        | _, _, true -> (LocContextResult.NewScope ({source with Scopes = applyTargetScope e.Target context.Scopes}))
                                        | current, ss, false -> LocContextResult.WrongScope (nextKey, current, ss)
                                )
            let commandMatch() =
                let matchedCommand = commands |> List.tryFind (fun c -> c == nextKey)
                match matchedCommand, first, nextKey.StartsWith("parameter:", StringComparison.OrdinalIgnoreCase) with
                |Some _, _, _ -> LocContextResult.Found (context.CurrentScope.ToString())
                | _, _, true -> LocContextResult.Found (context.CurrentScope.ToString())
                |None, false, false ->
                    match setvariables |> List.exists (fun sv -> sv == (nextKey.Split('@').[0])) with
                    | true -> LocContextResult.Found (context.CurrentScope.ToString())
                    | false ->
                        match extraOneToOne |> List.exists (fun et -> et == nextKey) with
                        | true -> LocContextResult.NewScope ({source with Scopes = context.Root.AnyScope::source.Scopes })
                        | false ->
                            LocNotFound (nextKey)
                |None, true, false ->
                    match eventtargets |> List.exists (fun et -> et == nextKey) with
                    | true -> LocContextResult.NewScope ({source with Scopes = context.Root.AnyScope::source.Scopes })
                    | false -> LocNotFound (nextKey)
            onetooneMatch()
            |> Option.orElseWith effectMatch
            |> Option.defaultWith commandMatch
        let locKeyFolder (result : LocContextResult) (nextKey : string) =
            match result with
            | LocContextResult.Start startContext -> inner (true, startContext) nextKey
            | LocContextResult.NewScope newContext -> inner (false, newContext) nextKey
            // | LocContextResult.Found endContext -> inner (false, endContext) nextKey
            | LocContextResult.Found endContext -> LocContextResult.LocNotFound nextKey //inner (false, endContext) nextKey
            | res -> res
        keys |> List.fold locKeyFolder (LocContextResult.Start source)

    // type LocContext
    let createJominiLocalisationCommandValidator (dataTypes : CWTools.Parser.DataTypeParser.JominiLocDataTypes) =
        fun (eventtargets : Collections.Map<string, Scope list>) (setvariables : string list) (source : ScopeContext) (command : JominiLocCommand list) ->
        // let keys = command.Split('.') |> List.ofArray
        // let keys =
        //     match
        // eprintfn "%A" command
        let convScopeToDataType (scope : Scope) =
            let s = scope.ToString()
            match s with
            | s -> s
        let command = command |> List.map (function |Command (key, cs) -> key, cs)
        let keys = command |> List.map fst
        let inner ((context : bool * string * bool)) (nextKey : string) =
            let first, dataType, confident = context
            // TODO: Replace this with some smarter
            let savedScopedMatch() =
                Map.tryFind nextKey eventtargets
                    |> Option.map (fun ets ->
                                            let scope, confident =
                                                match ets with
                                                | [x] when x = scopeManager.AnyScope -> "Country", false
                                                | [x] -> convScopeToDataType x, true
                                                | [x; y] when x = scopeManager.AnyScope -> convScopeToDataType y, true
                                                | x::y::_ when x = scopeManager.AnyScope -> convScopeToDataType y, false
                                                | x::_ -> convScopeToDataType x, false
                                                | _ -> "Country", false
                                            NewDataType ((scope, confident)))
            let promoteMatch() =
                dataTypes.promotes |> Map.tryFind nextKey
                |> Option.orElse (dataTypes.promotes |> Map.tryFind (nextKey.ToUpperInvariant()))
                |> Option.map (fun (newType) -> if Set.contains newType dataTypes.dataTypeNames then NewDataType (newType, true) else Found newType)
            let globalFunctionMatch() =
                dataTypes.functions |> Map.tryFind nextKey
                |> Option.map (fun (newType) -> if Set.contains newType dataTypes.dataTypeNames then NewDataType (newType, true) else Found newType)
            let functionMatch() =
                dataTypes.dataTypes |> Map.tryFind dataType
                |> Option.bind (fun dataTypeMap -> dataTypeMap |> Map.tryFind nextKey)
                |> Option.map (fun (newType) -> if Set.contains newType dataTypes.dataTypeNames then NewDataType (newType, true) else Found newType)
            let res =
                match first with
                | true ->
                    (promoteMatch()) |> Option.orElse (globalFunctionMatch()) |> Option.orElse (savedScopedMatch())
                | false ->
                    functionMatch()
            res |> Option.defaultWith (fun _ -> LocNotFoundInType (nextKey, dataType, confident))
        let locKeyFolder (result : LocContextResult) (nextKey : string) =
            match result with
            | Start startDataType when startDataType.CurrentScope = scopeManager.AnyScope ->
                inner (true, "None", true) nextKey
            | Start startDataType ->
                inner (true, startDataType.CurrentScope.ToString(), true) nextKey
            | NewDataType (newDataType, confident) -> inner (false, newDataType, confident) nextKey
            | Found endDataType -> LocNotFound (nextKey)
            | LocNotFound (n) -> LocNotFound (n)
            | res -> res
        // TODO: Better scopecontext to starting datatype
        keys |> List.fold locKeyFolder (Start (source))
        // match res with
        // | Start _ -> LocContextResult.Start source
        // | NewDataType newDataType -> LocContextResult.LocNotFound newDataType
        // | Found endDataType -> LocContextResult.Found
        // keys |> List.fold (fun r k -> match r with | (Found (r, s) , f) -> inner ((f, r, s)) k |LocNotFound s, _ -> LocNotFound s, false) (Found ("this", []), true) |> fst

    // let createLocalisationScopeValidator<'T when 'T :> IScope<'T> and 'T : comparison > (locPrimaryScopes : (string * (ScopeContext<'T> * bool -> ScopeContext<'T> * bool)) list) (scopedLocEffectsMap : EffectMap<'T>) =
    //     fun (commands : string list) (eventtargets : string list) (setvariables : string list) (entry : Entry) (startingScopeContext : ScopeContext<'T>) (command : string) ->
    //     let keys = command.Split('.') |> List.ofArray
    //     let inner ((first : bool), (rootScope : string), (scopes : 'T list)) (nextKey : string) =
    //         let onetoone = locPrimaryScopes |> List.tryFind (fun (k, _) -> k == nextKey)
    //         match onetoone with
    //         | Some (_) -> Found (nextKey, []), false
    //         | None ->
    //             let effectMatch = scopedLocEffectsMap.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e -> Some e |_ -> None)
    //             match effectMatch with
    //             | Some e -> Found (rootScope, e.Scopes), false
    //             | None ->
    //                 let matchedCommand = (commands)  |> List.tryFind (fun c -> c == nextKey)
    //                 match matchedCommand, first, nextKey.StartsWith("parameter:", StringComparison.OrdinalIgnoreCase) with
    //                 |Some _, _, _ -> Found (rootScope, scopes), false
    //                 | _, _, true -> Found (rootScope, scopes), false
    //                 |None, false, false ->
    //                     match setvariables |> List.exists (fun sv -> sv == (nextKey.Split('@').[0])) with
    //                     | true -> Found (rootScope, scopes), false
    //                     | false -> LocNotFound (nextKey), false
    //                 |None, true, false ->
    //                     match eventtargets |> List.exists (fun et -> et == nextKey) with
    //                     | true -> Found (rootScope, scopes), false
    //                     | false -> LocNotFound (nextKey), false
    //     keys |> List.fold (fun r k -> match r with | (Found (r, s) , f) -> inner ((f, r, s)) k |LocNotFound s, _ -> LocNotFound s, false) (Found ("this", []), true) |> fst

