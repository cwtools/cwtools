namespace CWTools.Process.Scopes

open System
open System.Collections.Frozen
open System.Linq
open CSharpHelpers
open CWTools.Common
open CWTools.Utilities
open CWTools.Utilities.Utils
open CWTools.Utilities.Utils2


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
type varPrefixFunc = delegate of ReadOnlySpan<char> -> struct (string * bool)

type EffectDictionary(effects: Effect seq) =

    let mutable dictionary: FrozenDictionary<int, Effect> =
        FrozenDictionary<int, Effect>.Empty

    do dictionary <- effects.DistinctBy(_.Name.lower).ToFrozenDictionary(_.Name.lower)

    new() = EffectDictionary(Seq.empty)

    member this.TryFind(key: StringTokens) : Effect option =
        let found, value = dictionary.TryGetValue key.lower
        if found then Some value else None

    member this.TryFind(key: string) =
        let s = StringResource.stringManager.InternIdentifierToken key
        this.TryFind s

    member this.Values = dictionary.Values

    static member FromList(effects: #Effect seq) : EffectDictionary =
        EffectDictionary(effects |> Seq.cast<Effect>)

type EffectMap = EffectDictionary

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
    delegate of
        bool *
        bool *
        EffectMap *
        EffectMap *
        ScopedEffect list *
        PrefixOptimisedStringSet *
        ReadOnlySpan<char> *
        ScopeContext ->
            ScopeResult

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
    let simpleVarPrefixFun (prefix: string) =
        varPrefixFunc (fun key ->
            if key.StartsWith(prefix, StringComparison.OrdinalIgnoreCase) then
                key.Slice(prefix.Length).ToString(), true
            else
                key.ToString(), false)

    let complexVarPrefixFun (prefix1: string) (prefix2: string) : varPrefixFunc =
        varPrefixFunc (fun key ->
            if key.StartsWith(prefix1, StringComparison.OrdinalIgnoreCase) then
                key.Slice(prefix1.Length).ToString(), true
            else if key.StartsWith(prefix2, StringComparison.OrdinalIgnoreCase) then
                key.Slice(prefix2.Length).ToString(), true
            else
                key.ToString(), false)

    let private applyTargetScope (scope: _ option) (context: _ list) =
        match scope with
        | None -> context
        | Some ps -> ps :: context

    let createJominiChangeScope oneToOneScopes (varPrefixFun: varPrefixFunc) =
        ChangeScope
            (fun
                (varLHS: bool)
                (skipEffect: bool)
                (eventTargetLinks: EffectMap)
                (valueTriggers: EffectMap)
                (wildcardLinks: ScopedEffect list)
                (vars: PrefixOptimisedStringSet)
                (key: ReadOnlySpan<char>)
                (source: ScopeContext) ->
                let key =
                    if key.StartsWith("hidden:", StringComparison.OrdinalIgnoreCase) then
                        key.Slice(7)
                    else
                        key

                if
                    key.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase)
                    || key.StartsWith("parameter:", StringComparison.OrdinalIgnoreCase)
                    || key.StartsWith('@')
                then
                    NewScope(
                        { Root = source.Root
                          From = source.From
                          Scopes = source.Root.AnyScope :: source.Scopes },
                        [],
                        None
                    )
                else
                    let struct (key, varOnly) = varPrefixFun.Invoke(key)

                    let afterAmp, hasAmp =
                        if key.IndexOf('@') >= 0 then
                            let x = key.Split('@', 2) in x[1], true
                        else
                            "", false

                    let keys = key.Split('.')
                    let keylength = keys.Length - 1
                    let keys = keys |> Array.mapi (fun i k -> k, i = keylength)

                    let inner (context: ScopeContext, changed: bool) (nextKey: string) (last: bool) =
                        let onetoone = oneToOneScopes |> List.tryFind (fun (k, _) -> k == nextKey)

                        match onetoone with
                        | Some(_, f) -> f (context, false), NewScope(f (context, false) |> fst, [], None)
                        | None ->
                            let eventTargetLinkMatch =
                                eventTargetLinks.TryFind nextKey
                                |> Option.bind (function
                                    | :? ScopedEffect as e -> Some e
                                    | _ -> None)

                            let valueScopeMatch = valueTriggers.TryFind nextKey

                            let wildcardScopeMatch =
                                wildcardLinks
                                |> List.tryFind (fun l ->
                                    nextKey.StartsWith(
                                        StringResource.stringManager.GetStringForID l.Name.normal,
                                        StringComparison.OrdinalIgnoreCase
                                    ))

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
                                if last && vars.Contains nextKey then
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
                                | x, _, _, true when x.Equals source.Root.AnyScope ->
                                    ({ context with
                                        Scopes = applyTargetScope e.Target context.Scopes },
                                     true),
                                    NewScope(
                                        { source with
                                            Scopes = applyTargetScope e.Target context.Scopes },
                                        e.IgnoreChildren,
                                        refHint
                                    )
                                | x, _, _, false when x.Equals source.Root.AnyScope ->
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
                            ((source, false), None)

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
                                ((source, false), None)

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
                        res2)

    let createChangeScope
        (oneToOneScopes: (string * (ScopeContext * struct (bool * bool) -> ScopeContext * struct (bool * bool))) list)
        (varPrefixFun: varPrefixFunc)
        (hoi4TargetedHardcodedVariables: bool)
        =
        ChangeScope
            (fun
                (varLHS: bool)
                (skipEffect: bool)
                (eventTargetLinks: EffectMap)
                (valueTriggers: EffectMap)
                (_: ScopedEffect list)
                (vars: PrefixOptimisedStringSet)
                (key: ReadOnlySpan<char>)
                (source: ScopeContext) ->
                let key =
                    if key.StartsWith("hidden:", StringComparison.OrdinalIgnoreCase) then
                        key.Slice(7)
                    else
                        key

                if
                    key.StartsWith("parameter:", StringComparison.OrdinalIgnoreCase)
                    || key.StartsWith('@')
                then
                    NewScope(
                        { Root = source.Root
                          From = source.From
                          Scopes = source.Root.AnyScope :: source.Scopes },
                        [],
                        None
                    )
                else
                    let struct (key, varOnly) = varPrefixFun.Invoke(key)

                    let inner
                        (context: ScopeContext, struct (first: bool, changed: bool))
                        (nextKey: string)
                        (last: bool)
                        =
                        let onetoone = oneToOneScopes |> List.tryFind (fun (k, _) -> k == nextKey)

                        match onetoone with
                        | Some(_, f) ->
                            f (context, struct (first, false)),
                            NewScope(f (context, struct (false, false)) |> fst, [], None)
                        | None ->
                            let eventTargetLinkMatch =
                                eventTargetLinks.TryFind nextKey
                                |> Option.bind (function
                                    | :? ScopedEffect as e -> Some e
                                    | _ -> None)

                            let valueScopeMatch = valueTriggers.TryFind nextKey

                            match
                                first && nextKey.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase),
                                eventTargetLinkMatch,
                                valueScopeMatch
                            with
                            | true, _, _ ->
                                (context, struct (true, true)),
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
                                    | x, _, _ when x.Equals source.Root.AnyScope ->
                                        (context, struct (false, false)), ValueFound refHint
                                    | _, [], _ -> (context, struct (false, false)), NotFound
                                    | _, _, true -> (context, struct (false, false)), ValueFound refHint
                                    | current, ss, false ->
                                        (context, struct (false, false)), WrongScope(nextKey, current, ss, refHint)
                                else
                                    (context, struct (false, false)), NotFound
                            | _, None, _ ->
                                if last && (vars.Contains nextKey) then
                                    (context, struct (false, false)), VarFound
                                else if varOnly then
                                    (context, struct (false, false)), VarNotFound nextKey
                                else
                                    (context, struct (false, false)), NotFound
                            | _, Some e, _ ->
                                let possibleScopes = e.Scopes
                                let currentScope = context.CurrentScope
                                let exact = possibleScopes |> List.exists currentScope.IsOfScope

                                match context.CurrentScope, possibleScopes, exact, e.IsScopeChange with
                                | x, _, _, true when x.Equals source.Root.AnyScope ->
                                    ({ context with
                                        Scopes = applyTargetScope e.Target context.Scopes },
                                     struct (false, true)),
                                    NewScope(
                                        { source with
                                            Scopes = applyTargetScope e.Target context.Scopes },
                                        e.IgnoreChildren,
                                        None
                                    )
                                | x, _, _, false when x.Equals source.Root.AnyScope ->
                                    (context, struct (false, false)), NewScope(context, e.IgnoreChildren, None)
                                | _, [], _, _ -> (context, struct (false, false)), NotFound
                                | _, _, true, true ->
                                    ({ context with
                                        Scopes = applyTargetScope e.Target context.Scopes },
                                     struct (false, true)),
                                    NewScope(
                                        { source with
                                            Scopes = applyTargetScope e.Target context.Scopes },
                                        e.IgnoreChildren,
                                        None
                                    )
                                | _, _, true, false ->
                                    (context, struct (false, false)), NewScope(context, e.IgnoreChildren, None)
                                | current, ss, false, _ ->
                                    (context, struct (false, false)), WrongScope(nextKey, current, ss, None)

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
                                    ((source, struct (true, false)), None)

                            match rawRes with
                            | (_, _), None -> NotFound
                            | (_, struct (_, true)), Some r ->
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
                            | (_, struct (_, false)), Some r -> r
                        else
                            NotFound

                    match rawRes2 with
                    | NotFound
                    | VarNotFound _ ->

                        let ampersandSplit = key.Split('@', 2)
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
                                ((source, struct (true, false)), None)

                        let res2 =
                            match res with
                            | (_, _), None -> NotFound
                            | (_, struct (_, true)), Some r ->
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
                            | (_, struct (_, false)), Some r -> r

                        if ampersandSplit.Length > 1 then
                            if vars.Contains key then
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
                                        ((source, (true, false)), None)

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
                            res2
                    | _ -> rawRes2)

    let defaultDesc = "Scope (/context) switch"
