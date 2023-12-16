namespace CWTools.Process.Scopes

open System
open System.Collections.Generic
open System.Globalization
open CWTools.Common
open CWTools.Utilities
open CWTools.Utilities.Utils
open CWTools.Utilities.Utils2
open Microsoft.FSharp.Collections.Tagged
open VDS.Common.Tries


// type EffectMapSparseTrie() =
//     inherit
//         AbstractTrie<string, char, Effect>(
//             (fun s -> s.ToLower(CultureInfo.InvariantCulture)),
//             new SparseCharacterTrieNode<Effect>(null, Unchecked.defaultof<char>)
//         )
//
//     override this.CreateRoot(key) =
//         new SparseCharacterTrieNode<Effect>(null, key)
//     member this.TryFind(key) =
//         let res = this.Find(key)
//         if res <> null && res.HasValue then Some res.Value else None
//     static member FromList<'a when 'a :> Effect> (effects : 'a seq) =
//         let tree = EffectMapSparseTrie()
//         effects |> Seq.iter (fun x -> tree.Add(x.Name, x))
//         tree

type EffectDictionary(effects : Effect seq) =

    let dictionary : Dictionary<int, Effect> = Dictionary<int, Effect>()
    do
        for e in effects do
            dictionary[e.Name.lower] <- e
    new() = EffectDictionary(Seq.empty)
    member this.TryFind(key : StringTokens) : Effect option =
        // let lowerKey = key.ToLowerInvariant()
        let found, value = dictionary.TryGetValue key.lower
        if found then Some value else None
    member this.TryFind(key : string) =
        let s = StringResource.stringManager.InternIdentifierToken key
        this.TryFind s
    member this.Values with get () = dictionary.Values
    // static member FromListE(effects : Effect seq) =
        // EffectDictionary(effects)
    static member FromList(effects : #Effect seq) : EffectDictionary =
        EffectDictionary (effects |> Seq.map (fun e -> e :> Effect))

type EffectMap = EffectDictionary

type UsageScopeContext = Scope list

type ScopeContext =
    { Root: Scope
      From: Scope list
      Scopes: Scope list }

    member this.CurrentScope =
        match this.Scopes with
        | [] -> this.Root.AnyScope
        | x :: _ -> x

    member this.PopScope: Scope list =
        match this.Scopes with
        | [] -> []
        | _ :: xs -> xs

    member this.GetFrom i =
        if this.From.Length >= i then
            (this.From.Item(i - 1))
        else
            this.Root.AnyScope

type ScopeResult =
    | NewScope of newScope: ScopeContext * ignoreKeys: string list * refHint: ReferenceHint option
    | WrongScope of command: string * scope: Scope * expected: Scope list * refHint: ReferenceHint option
    | NotFound
    | VarFound
    | VarNotFound of var: string
    | ValueFound of refHint: ReferenceHint option

type ChangeScope =
    bool -> bool -> EffectMap -> EffectMap -> ScopedEffect list -> StringSet -> string -> ScopeContext -> ScopeResult

module Scopes =

    let defaultContext =
        { Root = scopeManager.AnyScope
          From = []
          Scopes = [] }

    let noneContext =
        { Root = scopeManager.InvalidScope
          From = []
          Scopes = [ scopeManager.InvalidScope ] }

    // type EffectMap<'T> = Map<string, Effect<'T>, InsensitiveStringComparer>
    let simpleVarPrefixFun prefix =
        let varStartsWith =
            (fun (k: string) -> k.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))

        let varSubstring = (fun (k: string) -> k.Substring(prefix.Length))

        (fun key ->
            if varStartsWith key then
                varSubstring key, true
            else
                key, false)

    let complexVarPrefixFun prefix1 prefix2 =
        let varStartsWith1 =
            (fun (k: string) -> k.StartsWith(prefix1, StringComparison.OrdinalIgnoreCase))

        let varStartsWith2 =
            (fun (k: string) -> k.StartsWith(prefix2, StringComparison.OrdinalIgnoreCase))

        let varSubstring1 = (fun (k: string) -> k.Substring(prefix1.Length))
        let varSubstring2 = (fun (k: string) -> k.Substring(prefix2.Length))

        (fun key ->
            if varStartsWith1 key then varSubstring1 key, true
            else if varStartsWith2 key then varSubstring2 key, true
            else key, false)

    let private applyTargetScope (scope: _ option) (context: _ list) =
        match scope with
        | None -> context
        | Some ps -> ps :: context

    let createJominiChangeScope oneToOneScopes (varPrefixFun: string -> string * bool) =
        // let varStartsWith = (fun (k : string) -> k.StartsWith(varPrefix, StringComparison.OrdinalIgnoreCase))
        // let varSubstring = (fun (k : string) -> k.Substring(varPrefix.Length ))
        let amp = [| '@' |]

        (fun
            (varLHS: bool)
            (skipEffect: bool)
            (eventTargetLinks: EffectMap)
            (valueTriggers: EffectMap)
            (wildcardLinks: ScopedEffect list)
            (vars: StringSet)
            (key: string)
            (source: ScopeContext) ->
            let key =
                if key.StartsWith("hidden:", StringComparison.OrdinalIgnoreCase) then
                    key.Substring(7)
                else
                    key

            if
                key.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase)
                || key.StartsWith("parameter:", StringComparison.OrdinalIgnoreCase)
                || key.StartsWith("@", StringComparison.OrdinalIgnoreCase)
            then
                NewScope(
                    { Root = source.Root
                      From = source.From
                      Scopes = source.Root.AnyScope :: source.Scopes },
                    [],
                    None
                )
            else
                let key, varOnly = varPrefixFun key

                let beforeAmp, afterAmp, hasAmp =
                    if key.IndexOf('@') >= 0 then
                        let x = key.Split(amp, 2) in x.[0], x.[1], true
                    else
                        key, "", false
                // let ampersandSplit = if key.Contains('@') then key.Split(amp, 2) else
                let keys = key.Split('.')
                let keylength = keys.Length - 1
                let keys = keys |> Array.mapi (fun i k -> k, i = keylength)

                let inner (context: ScopeContext, changed: bool) (nextKey: string) (last: bool) =
                    let onetoone = oneToOneScopes |> List.tryFind (fun (k, _) -> k == nextKey)
                    // eprintfn "o2o %A" onetoone
                    match onetoone with
                    | Some(_, f) -> f (context, false), NewScope(f (context, false) |> fst, [], None)
                    | None ->
                        // let effectMatch = effects.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e when not e.IsValueScope -> Some e |_ -> None)
                        // let triggerMatch = triggers.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e when not e.IsValueScope -> Some e |_ -> None)
                        let eventTargetLinkMatch =
                            eventTargetLinks.TryFind nextKey
                            |> Option.bind (function
                                | :? ScopedEffect as e -> Some e
                                | _ -> None)

                        let valueScopeMatch = valueTriggers.TryFind nextKey

                        let wildcardScopeMatch =
                            wildcardLinks
                            |> List.tryFind (fun l -> nextKey.StartsWith(StringResource.stringManager.GetStringForID l.Name.normal, StringComparison.OrdinalIgnoreCase))
                        // let effect = (effects @ triggers)
                        //             |> List.choose (function | :? ScopedEffect as e -> Some e |_ -> None)
                        //             |> List.tryFind (fun e -> e.Name == nextKey)
                        // if skipEffect then (context, false), NotFound else
                        // eprintfn "vsm %A" valueScopeMatch
                        match eventTargetLinkMatch |> Option.orElse wildcardScopeMatch, valueScopeMatch with
                        | _, Some e ->
                            if last then
                                let possibleScopes = e.Scopes
                                let currentScope = context.CurrentScope
                                let exact = possibleScopes |> List.exists (fun x -> currentScope.IsOfScope x)
                                let refHint = e.RefHint

                                match context.CurrentScope, possibleScopes, exact with
                                | x, _, _ when x = source.Root.AnyScope -> (context, false), ValueFound refHint
                                | _, [], _ -> (context, false), NotFound
                                | _, _, true -> (context, false), ValueFound refHint
                                | current, ss, false -> (context, false), WrongScope(nextKey, current, ss, refHint)
                            else
                                (context, false), NotFound
                        | None, _ ->
                            if last && vars.ContainsKey nextKey then
                                (context, false), VarFound
                            else if varOnly then
                                (context, false), VarNotFound nextKey
                            else
                                (context, false), NotFound
                        | Some e, _ ->
                            let possibleScopes = e.Scopes
                            let currentScope = context.CurrentScope
                            let exact = possibleScopes |> List.exists (fun x -> currentScope.IsOfScope x)
                            let refHint = e.RefHint

                            match context.CurrentScope, possibleScopes, exact, e.IsScopeChange with
                            | x, _, _, true when x = source.Root.AnyScope ->
                                ({ context with
                                    Scopes = applyTargetScope e.Target context.Scopes },
                                 true),
                                NewScope(
                                    { source with
                                        Scopes = applyTargetScope e.Target context.Scopes },
                                    e.IgnoreChildren,
                                    refHint
                                )
                            | x, _, _, false when x = source.Root.AnyScope ->
                                (context, false), NewScope(context, e.IgnoreChildren, refHint)
                            | _, [], _, _ -> (context, false), NotFound
                            | _, _, true, true ->
                                ({ context with
                                    Scopes = applyTargetScope e.Target context.Scopes },
                                 true),
                                NewScope(
                                    { source with
                                        Scopes = applyTargetScope e.Target context.Scopes },
                                    e.IgnoreChildren,
                                    refHint
                                )
                            | _, _, true, false -> (context, false), NewScope(context, e.IgnoreChildren, refHint)
                            | current, ss, false, _ -> (context, false), WrongScope(nextKey, current, ss, refHint)

                let inner2 = fun a b l -> inner a b l |> (fun (c, d) -> c, Some d)

                let res =
                    keys
                    |> Array.fold
                        (fun ((c, b), r) (k, l) ->
                            match r with
                            | None -> inner2 (c, b) k l
                            | Some(NewScope(x, i, rh)) -> inner2 (x, b) k l
                            | Some x -> (c, b), Some x)
                        ((source, false), None) // |> snd |> Option.defaultValue (NotFound)

                let res2 =
                    match res with
                    | (_, _), None -> NotFound
                    | (_, true), Some r ->
                        r
                        |> function
                            | NewScope(x, i, rh) ->
                                NewScope(
                                    { source with
                                        Scopes = x.CurrentScope :: source.Scopes },
                                    i,
                                    rh
                                )
                            | x -> x
                    | (_, false), Some r -> r

                if hasAmp then
                    let keys = afterAmp.Split('.')
                    let keylength = keys.Length - 1
                    let keys = keys |> Array.mapi (fun i k -> k, i = keylength)

                    let tres =
                        keys
                        |> Array.fold
                            (fun ((c, b), r) (k, l) ->
                                match r with
                                | None -> inner2 (c, b) k l
                                | Some(NewScope(x, i, rh)) -> inner2 (x, b) k l
                                | Some x -> (c, b), Some x)
                            ((source, false), None) // |> snd |> Option.defaultValue (NotFound)

                    let tres2 =
                        match tres with
                        | (_, _), None -> NotFound
                        | (_, true), Some r ->
                            r
                            |> function
                                | NewScope(x, i, rh) ->
                                    NewScope(
                                        { source with
                                            Scopes = x.CurrentScope :: source.Scopes },
                                        i,
                                        rh
                                    )
                                | x -> x
                        | (_, false), Some r -> r

                    match res2, tres2 with
                    | _, NotFound -> NotFound
                    | _, VarNotFound s -> VarNotFound s
                    | VarFound, _ -> VarFound
                    | _, _ -> NotFound
                else
                    // let x = res |> function |NewScope x -> NewScope { source with Scopes = x.CurrentScope::source.Scopes } |x -> x
                    // x
                    res2)

    let createChangeScope
        oneToOneScopes
        (varPrefixFun: string -> string * bool)
        (hoi4TargetedHardcodedVariables: bool)
        =
        // let varStartsWith = (fun (k : string) -> k.StartsWith(varPrefix, StringComparison.OrdinalIgnoreCase))
        // let varSubstring = (fun (k : string) -> k.Substring(varPrefix.Length ))
        (fun
            (varLHS: bool)
            (skipEffect: bool)
            (eventTargetLinks: EffectMap)
            (valueTriggers: EffectMap)
            (_: ScopedEffect list)
            (vars: StringSet)
            (key: string)
            (source: ScopeContext) ->
            let key =
                if key.StartsWith("hidden:", StringComparison.OrdinalIgnoreCase) then
                    key.Substring(7)
                else
                    key

            if
                key.StartsWith("parameter:", StringComparison.OrdinalIgnoreCase)
                || key.StartsWith("@", StringComparison.OrdinalIgnoreCase)
            then
                NewScope(
                    { Root = source.Root
                      From = source.From
                      Scopes = source.Root.AnyScope :: source.Scopes },
                    [],
                    None
                )
            else
                let key, varOnly = varPrefixFun key

                let inner (context: ScopeContext, (first: bool, changed: bool)) (nextKey: string) (last: bool) =
                    let onetoone = oneToOneScopes |> List.tryFind (fun (k, _) -> k == nextKey)

                    match onetoone with
                    | Some(_, f) -> f (context, (first, false)), NewScope(f (context, (false, false)) |> fst, [], None)
                    | None ->
                        // let effectMatch = effects.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e when not e.IsValueScope  -> Some e |_ -> None)
                        // let triggerMatch = triggers.TryFind nextKey |> Option.bind (function | :? ScopedEffect<'T> as e when not e.IsValueScope -> Some e |_ -> None)
                        let eventTargetLinkMatch =
                            eventTargetLinks.TryFind nextKey
                            |> Option.bind (function
                                | :? ScopedEffect as e -> Some e
                                | _ -> None)

                        let valueScopeMatch = valueTriggers.TryFind nextKey
                        // let effect = (effects @ triggers)
                        //             |> List.choose (function | :? ScopedEffect as e -> Some e |_ -> None)
                        //             |> List.tryFind (fun e -> e.Name == nextKey)
                        // if skipEffect then (context, false), NotFound else
                        match
                            first && nextKey.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase),
                            eventTargetLinkMatch,
                            valueScopeMatch
                        with
                        | true, _, _ ->
                            (context, (true, true)),
                            NewScope(
                                { Root = source.Root
                                  From = source.From
                                  Scopes = source.Root.AnyScope :: source.Scopes },
                                [],
                                None
                            )
                        | _, _, Some e ->
                            if last then
                                let possibleScopes = e.Scopes
                                let currentScope = context.CurrentScope
                                let exact = possibleScopes |> List.exists (fun x -> currentScope.IsOfScope x)
                                let refHint = e.RefHint

                                match context.CurrentScope, possibleScopes, exact with
                                | x, _, _ when x = source.Root.AnyScope ->
                                    (context, (false, false)), ValueFound refHint
                                | _, [], _ -> (context, (false, false)), NotFound
                                | _, _, true -> (context, (false, false)), ValueFound refHint
                                | current, ss, false ->
                                    (context, (false, false)), WrongScope(nextKey, current, ss, refHint)
                            else
                                (context, (false, false)), NotFound
                        | _, None, _ ->
                            if last && (vars.ContainsKey nextKey) then
                                (context, (false, false)), VarFound
                            else if varOnly then
                                (context, (false, false)), VarNotFound nextKey
                            else
                                (context, (false, false)), NotFound
                        | _, Some e, _ ->
                            let possibleScopes = e.Scopes
                            let currentScope = context.CurrentScope
                            let exact = possibleScopes |> List.exists currentScope.IsOfScope

                            match context.CurrentScope, possibleScopes, exact, e.IsScopeChange with
                            | x, _, _, true when x = source.Root.AnyScope ->
                                ({ context with
                                    Scopes = applyTargetScope e.Target context.Scopes },
                                 (false, true)),
                                NewScope(
                                    { source with
                                        Scopes = applyTargetScope e.Target context.Scopes },
                                    e.IgnoreChildren,
                                    None
                                )
                            | x, _, _, false when x = source.Root.AnyScope ->
                                (context, (false, false)), NewScope(context, e.IgnoreChildren, None)
                            | _, [], _, _ -> (context, (false, false)), NotFound
                            | _, _, true, true ->
                                ({ context with
                                    Scopes = applyTargetScope e.Target context.Scopes },
                                 (false, true)),
                                NewScope(
                                    { source with
                                        Scopes = applyTargetScope e.Target context.Scopes },
                                    e.IgnoreChildren,
                                    None
                                )
                            | _, _, true, false -> (context, (false, false)), NewScope(context, e.IgnoreChildren, None)
                            | current, ss, false, _ ->
                                (context, (false, false)), WrongScope(nextKey, current, ss, None)

                let inner2 = fun a b l -> inner a b l |> (fun (c, d) -> c, Some d)
                // Try just the raw string first
                let rawKeys = key.Split('.')
                let rawKeyLength = rawKeys.Length - 1
                let rawKeys = rawKeys |> Array.mapi (fun i k -> k, i = rawKeyLength)

                let rawRes2 =
                    if hoi4TargetedHardcodedVariables then
                        let rawRes =
                            rawKeys
                            |> Array.fold
                                (fun ((c, b), r) (k, l) ->
                                    match r with
                                    | None -> inner2 (c, b) k l
                                    | Some(NewScope(x, i, _)) -> inner2 (x, b) k l
                                    | Some x -> (c, b), Some x)
                                ((source, (true, false)), None) // |> snd |> Option.defaultValue (NotFound)

                        match rawRes with
                        | (_, _), None -> NotFound
                        | (_, (_, true)), Some r ->
                            r
                            |> function
                                | NewScope(x, i, _) ->
                                    NewScope(
                                        { source with
                                            Scopes = x.CurrentScope :: source.Scopes },
                                        i,
                                        None
                                    )
                                | x -> x
                        | (_, (_, false)), Some r -> r
                    else
                        NotFound

                match rawRes2 with
                | NotFound
                | VarNotFound _ ->

                    let ampersandSplit = key.Split([| '@' |], 2)
                    let keys = ampersandSplit.[0].Split('.')
                    let keylength = keys.Length - 1
                    let keys = keys |> Array.mapi (fun i k -> k, i = keylength)

                    let res =
                        keys
                        |> Array.fold
                            (fun ((c, b), r) (k, l) ->
                                match r with
                                | None -> inner2 (c, b) k l
                                | Some(NewScope(x, i, _)) -> inner2 (x, b) k l
                                | Some x -> (c, b), Some x)
                            ((source, (true, false)), None) // |> snd |> Option.defaultValue (NotFound)

                    let res2 =
                        match res with
                        | (_, _), None -> NotFound
                        | (_, (_, true)), Some r ->
                            r
                            |> function
                                | NewScope(x, i, _) ->
                                    NewScope(
                                        { source with
                                            Scopes = x.CurrentScope :: source.Scopes },
                                        i,
                                        None
                                    )
                                | x -> x
                        | (_, (_, false)), Some r -> r

                    if ampersandSplit.Length > 1 then
                        if vars.ContainsKey key then
                            VarFound
                        else
                            let keys = ampersandSplit.[1].Split('.')
                            let keylength = keys.Length - 1
                            let keys = keys |> Array.mapi (fun i k -> k, i = keylength)

                            let tres =
                                keys
                                |> Array.fold
                                    (fun ((c, b), r) (k, l) ->
                                        match r with
                                        | None -> inner2 (c, b) k l
                                        | Some(NewScope(x, i, _)) -> inner2 (x, b) k l
                                        | Some x -> (c, b), Some x)
                                    ((source, (true, false)), None) // |> snd |> Option.defaultValue (NotFound)

                            let tres2 =
                                match tres with
                                | (_, _), None -> NotFound
                                | (_, (_, true)), Some r ->
                                    r
                                    |> function
                                        | NewScope(x, i, _) ->
                                            NewScope(
                                                { source with
                                                    Scopes = x.CurrentScope :: source.Scopes },
                                                i,
                                                None
                                            )
                                        | x -> x
                                | (_, (_, false)), Some r -> r

                            match res2, tres2 with
                            | _, NotFound -> NotFound
                            | _, VarNotFound s -> VarNotFound s
                            | VarFound, _ -> VarFound
                            | _, _ -> NotFound
                    else
                        // let x = res |> function |NewScope x -> NewScope { source with Scopes = x.CurrentScope::source.Scopes } |x -> x
                        // x
                        res2
                | _ -> rawRes2)



    let defaultDesc = "Scope (/context) switch"
