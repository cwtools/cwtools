namespace CWTools.Process

open System
open CWTools.Utilities.Position
open CWTools.Common
module Scopes =
    open CWTools.Utilities.Utils
    open Microsoft.FSharp.Collections.Tagged
    type EffectMap<'S when 'S : comparison> = Map<string, Effect<'S>, InsensitiveStringComparer>
    type UsageScopeContext<'S> = 'S list
    // type ContextResult<'S> =
    // | Found of string * ('S list)
    // | LocNotFound of string


    //| Failed

    // type IScopeContext<'S when 'S : comparison> =
    //     abstract CurrentScope : 'S
    //     abstract PopScope : 'S list
    //     abstract GetFrom : int -> 'S
    //     abstract Root : 'S
    //     abstract From :'S list
    //     abstract Scopes :'S list
    // type RuleContext<'T when 'T : comparison> =
    //     {
    //         subtypes : string list
    //         scopes : IScopeContext<'T>
    //         warningOnly : bool
    //     }

    type OutputScopeContext<'T> =
        {
            Root : 'T
            From : 'T list
            Scopes : 'T list
        }
        member inline this.CurrentScope = match this.Scopes with |[] -> None | x::_ -> Some x
        member inline this.PopScope : 'T list = match this.Scopes with |[] -> [] |_::xs -> xs
        member inline this.GetFrom i =
            if this.From.Length >= i then Some (this.From.Item (i - 1)) else None

    type ScopeContext< 'T when 'T :> IScope<'T> > =
        {
            Root : 'T
            From : 'T list
            Scopes : 'T list
        }
        member this.CurrentScope = match this.Scopes with |[] -> this.Root.AnyScope | x::_ -> x
        member this.PopScope : 'T list = match this.Scopes with |[] -> [] |_::xs -> xs
        member this.GetFrom i =
             if this.From.Length >= i then (this.From.Item (i - 1)) else this.Root.AnyScope


    type LocContextResult<'S when 'S :> IScope<'S>> =
        | Start of startContext : ScopeContext<'S>
        | NewScope of newScope : ScopeContext<'S>
        | WrongScope of command : string * scope : 'S * expected : 'S list
        //Not sure what this should be
        | Found of endContext : ScopeContext<'S>
        | LocNotFound of key : string

    [<Struct>]
    type LocEntry<'S when 'S :> IScope<'S>> = {
        key : string
        value : char option
        desc : string
        position : range
        scopes : LocContextResult<'S> list
        refs : string list
    }


    type ScopeResult<'T when 'T :> IScope<'T>> =
        | NewScope of newScope : ScopeContext<'T> * ignoreKeys : string list
        | WrongScope of command : string * scope : 'T * expected : 'T list
        | NotFound
        | VarFound
        | VarNotFound of var : string
        | ValueFound
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
    let createJominiChangeScope<'T when 'T :> IScope<'T> and 'T : comparison > (oneToOneScopes) (varPrefixFun : string -> string * bool) =
        // let varStartsWith = (fun (k : string) -> k.StartsWith(varPrefix, StringComparison.OrdinalIgnoreCase))
        // let varSubstring = (fun (k : string) -> k.Substring(varPrefix.Length ))
        (fun (varLHS : bool) (skipEffect : bool) (eventTargetLinks : EffectMap<_>) (valueTriggers : EffectMap<_>) (wildcardLinks : ScopedEffect<_> list) (vars : StringSet) (key : string) (source : ScopeContext<'T>) ->
            let key = if key.StartsWith("hidden:", StringComparison.OrdinalIgnoreCase) then key.Substring(7) else key
            if
                key.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase)
                || key.StartsWith("parameter:", StringComparison.OrdinalIgnoreCase)
                || key.StartsWith("@", StringComparison.OrdinalIgnoreCase)
            then NewScope ({ Root = source.Root; From = source.From; Scopes = source.Root.AnyScope::source.Scopes }, [])
            else
                let key, varOnly = varPrefixFun key
                let ampersandSplit = key.Split([|'@'|], 2)
                let keys = ampersandSplit.[0].Split('.')
                let keylength = keys.Length - 1
                let keys = keys |> Array.mapi (fun i k -> k, i = keylength)
                let inner ((context : ScopeContext<'T>), (changed : bool)) (nextKey : string) (last : bool) =
                    let onetoone = oneToOneScopes |> List.tryFind (fun (k, _) -> k == nextKey)
                    // eprintfn "o2o %A" onetoone
                    match onetoone with
                    | Some (_, f) -> f (context, false), NewScope (f (context, false) |> fst, [])
                    | None ->
                        // let effectMatch = effects.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e when not e.IsValueScope -> Some e |_ -> None)
                        // let triggerMatch = triggers.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e when not e.IsValueScope -> Some e |_ -> None)
                        let eventTargetLinkMatch = eventTargetLinks.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e -> Some e |_ -> None)
                        let valueScopeMatch = valueTriggers.TryFind nextKey |> Option.bind (function | :? Effect<'T> as e -> Some e |_ -> None)
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
                                let currentScope = context.CurrentScope :> IScope<_>
                                let exact = possibleScopes |> List.exists (fun x -> currentScope.MatchesScope x)
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
                            let currentScope = context.CurrentScope :> IScope<_>
                            let exact = possibleScopes |> List.exists (fun x -> currentScope.MatchesScope x)
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
                if ampersandSplit.Length > 1
                then
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
                res2)

    let createChangeScope<'T when 'T :> IScope<'T> and 'T : comparison > (oneToOneScopes) (varPrefixFun : string -> string * bool) =
        // let varStartsWith = (fun (k : string) -> k.StartsWith(varPrefix, StringComparison.OrdinalIgnoreCase))
        // let varSubstring = (fun (k : string) -> k.Substring(varPrefix.Length ))
        (fun (varLHS : bool) (skipEffect : bool) (eventTargetLinks : EffectMap<_>) (_ : EffectMap<'T>) (_ : ScopedEffect<'T> list) (vars : StringSet) (key : string) (source : ScopeContext<'T>) ->
            let key = if key.StartsWith("hidden:", StringComparison.OrdinalIgnoreCase) then key.Substring(7) else key
            if
                key.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase)
                || key.StartsWith("parameter:", StringComparison.OrdinalIgnoreCase)
                || key.StartsWith("@", StringComparison.OrdinalIgnoreCase)
            then NewScope ({ Root = source.Root; From = source.From; Scopes = source.Root.AnyScope::source.Scopes }, [])
            else
                let key, varOnly = varPrefixFun key
                let ampersandSplit = key.Split([|'@'|], 2)
                let keys = ampersandSplit.[0].Split('.')
                let keylength = keys.Length - 1
                let keys = keys |> Array.mapi (fun i k -> k, i = keylength)
                let inner ((context : ScopeContext<'T>), (changed : bool)) (nextKey : string) (last : bool) =
                    let onetoone = oneToOneScopes |> List.tryFind (fun (k, _) -> k == nextKey)
                    match onetoone with
                    | Some (_, f) -> f (context, false), NewScope (f (context, false) |> fst, [])
                    | None ->
                        // let effectMatch = effects.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e when not e.IsValueScope  -> Some e |_ -> None)
                        // let triggerMatch = triggers.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e when not e.IsValueScope -> Some e |_ -> None)
                        let eventTargetLinkMatch = eventTargetLinks.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e -> Some e |_ -> None)
                        // let effect = (effects @ triggers)
                        //             |> List.choose (function | :? ScopedEffect as e -> Some e |_ -> None)
                        //             |> List.tryFind (fun e -> e.Name == nextKey)
                        // if skipEffect then (context, false), NotFound else
                        match eventTargetLinkMatch with
                        | None ->
                            if last && vars.Contains nextKey
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
                            let currentScope = context.CurrentScope :> IScope<_>
                            let exact = possibleScopes |> List.exists (fun x -> currentScope.MatchesScope x)
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
                if ampersandSplit.Length > 1
                then
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
                res2)


    let createLocalisationCommandValidator<'T when 'T :> IScope<'T> and 'T : comparison > (locPrimaryScopes : (string * (ScopeContext<'T> * bool -> ScopeContext<'T> * bool)) list) (scopedLocEffectsMap : EffectMap<'T>) =
        fun (commands : string list) (eventtargets : string list) (setvariables : string list) (source : ScopeContext<'T>) (command : string) ->
        let keys = command.Split('.') |> List.ofArray
        let inner ((first : bool), (context : ScopeContext<'T>)) (nextKey : string) =
            let onetooneMatch() =
                locPrimaryScopes |> List.tryFind (fun (k, _) -> k == nextKey)
                |> Option.map (fun (_, f) -> (LocContextResult.NewScope (f (context, false) |> fst)))
            let effectMatch() =
                scopedLocEffectsMap.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e -> Some e |_ -> None)
                |> Option.map (fun e ->
                                    let validScopes = e.Scopes
                                    let currentScope = context.CurrentScope :> IScope<_>
                                    let exact = validScopes |> List.exists (fun x -> currentScope.MatchesScope x)
                                    match context.CurrentScope, validScopes, exact with
                                        | x, _, _ when x = context.Root.AnyScope -> (LocContextResult.NewScope ({source with Scopes =applyTargetScope e.Target context.Scopes}))
                                        // | _, [], _ -> (context, false), NotFound
                                        | _, _, true -> (LocContextResult.NewScope ({source with Scopes = applyTargetScope e.Target context.Scopes}))
                                        | current, ss, false -> LocContextResult.WrongScope (nextKey, current, ss)
                                )
            let commandMatch() =
                let matchedCommand = commands |> List.tryFind (fun c -> c == nextKey)
                match matchedCommand, first, nextKey.StartsWith("parameter:", StringComparison.OrdinalIgnoreCase) with
                |Some _, _, _ -> Found (context)
                | _, _, true -> Found (context)
                |None, false, false ->
                    match setvariables |> List.exists (fun sv -> sv == (nextKey.Split('@').[0])) with
                    | true -> Found (context)
                    | false -> LocNotFound (nextKey)
                |None, true, false ->
                    match eventtargets |> List.exists (fun et -> et == nextKey) with
                    | true -> Found (context)
                    | false -> LocNotFound (nextKey)
            onetooneMatch()
            |> Option.orElseWith effectMatch
            |> Option.defaultWith commandMatch

            // match onetoone with
            // | Some (_, f) -> (f (context, false), NewScope (f (context, false) |> fst))
            // | None ->
            //     let effectMatch = scopedLocEffectsMap.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e -> Some e |_ -> None)
            //     match effectMatch with
            //     | Some e -> Found (rootScope, e.Scopes), false
            //     | None ->
            //         let matchedCommand = (commands)  |> List.tryFind (fun c -> c == nextKey)
            //         match matchedCommand, first, nextKey.StartsWith("parameter:", StringComparison.OrdinalIgnoreCase) with
            //         |Some _, _, _ -> Found (rootScope, scopes), false
            //         | _, _, true -> Found (rootScope, scopes), false
            //         |None, false, false ->
            //             match setvariables |> List.exists (fun sv -> sv == (nextKey.Split('@').[0])) with
            //             | true -> Found (rootScope, scopes), false
            //             | false -> LocNotFound (nextKey), false
            //         |None, true, false ->
            //             match eventtargets |> List.exists (fun et -> et == nextKey) with
            //             | true -> Found (rootScope, scopes), false
            //             | false -> LocNotFound (nextKey), false
        let locKeyFolder (result : LocContextResult<_>) (nextKey : string) =
            match result with
            | Start startContext -> inner (true, startContext) nextKey
            | LocContextResult.NewScope newContext -> inner (false, newContext) nextKey
            | Found endContext -> inner (false, endContext) nextKey
            | res -> res
        keys |> List.fold locKeyFolder (Start source)
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

    type ChangeScope<'S when 'S :> IScope<'S> and 'S : comparison> = bool -> bool -> EffectMap<'S> -> EffectMap<'S> -> ScopedEffect<'S> list -> StringSet -> string -> ScopeContext<'S> -> ScopeResult<'S>