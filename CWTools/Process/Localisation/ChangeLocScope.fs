namespace CWTools.Process.Localisation

open CWTools.Process.Scopes
open CWTools.Common
open CWTools.Utilities.Position
open CWTools.Utilities.Utils
open System
open CWTools.Utilities.Utils2

type LocContextResult =
    | Start of startContext: ScopeContext
    | NewScope of newScope: ScopeContext
    | VariableScope of lastContext: ScopeContext
    | WrongScope of command: string * scope: Scope * expected: Scope list
    //Not sure what this should be
    | Found of endContext: string
    | LocNotFound of key: string
    | LocNotFoundInType of key: string * dataType: string * confident: bool
    // Jomini loc
    | NewDataType of newDataType: string * confident: bool

// type JominiLocContextResult =
//     | Start of startDataType : string
//     | NotFound of key : string * context : string

type JominiLocCommandParam =
    | Commands of JominiLocCommand list
    | Param of string

and JominiLocCommand = Command of string * JominiLocCommandParam list

type LocEntry =
    { key: string
      value: char option
      desc: string
      position: range
      refs: string list
      commands: string list
      jominiCommands: JominiLocCommand list list
      scopes: LocContextResult list
      errorRanges: range option }

type LegacyLocDynamicsSettings =
    {
        /// Terminal, scope specific, scripted loc, name and required scopes
        scriptedLocCommands: (string * Scope list) seq
        /// Event targets, must be first, can have a scope
        eventTargets: (string * Scope) seq
        /// Variables
        setVariables: IgnoreCaseStringSet
    }

type LegacyLocStaticSettings =
    {
        /// Are variables begun with a `?`
        questionMarkVariable: bool
        /// Do variables need commands like GetValue
        usesVariableCommands: bool
        /// Parameter variables? `parameter:` goes to a scope
        parameterVariables: bool
        /// 1:2:1 like FROM, THIS, PREV
        locPrimaryScopes: (string * (ScopeContext * bool -> ScopeContext * bool)) list
        /// 1:2:1 like OWNER
        scopedLocEffectsMap: EffectMap
        /// Terminal, scope specific, like GetRulerName, name and required scopes
        commands: (string * Scope list) list
        /// Variable commands (when required, eg. EU4, CK2)
        variableCommands: string list
    }

module ChangeLocScope =
    let private applyTargetScope (scope: _ option) (context: _ list) =
        match scope with
        | None -> context
        | Some ps -> ps :: context

    let createLegacyLocalisationCommandValidator (staticSettings: LegacyLocStaticSettings) =
        fun (dynamicSettings: LegacyLocDynamicsSettings) (source: ScopeContext) (command: string) ->
            let command, res =
                if staticSettings.questionMarkVariable then
                    if
                        command.StartsWith('?')
                        && command.TrimStart('?')
                           |> CWTools.Utilities.TryParser.parseIntWithDecimal
                           |> (fun s -> s.IsSome)
                    then
                        command, Some(LocContextResult.Found "number")
                    else
                        command.TrimStart('?'), None
                else
                    command, None

            let keys = command.Split('.') |> List.ofArray

            let inner (first: bool, inVariable: bool, context: ScopeContext) (nextKey: string) =
                let onetooneMatch () =
                    staticSettings.locPrimaryScopes
                    |> List.tryFind (fun (k, _) -> k == nextKey)
                    |> Option.map (fun (_, f) -> (LocContextResult.NewScope(f (context, false) |> fst)))

                let effectMatch () =
                    staticSettings.scopedLocEffectsMap.TryFind nextKey
                    |> Option.bind (function
                        | :? ScopedEffect as e -> Some e
                        | _ -> None)
                    |> Option.map (fun e ->
                        let validScopes = e.Scopes
                        let currentScope = context.CurrentScope
                        let exact = validScopes |> List.exists currentScope.IsOfScope

                        match context.CurrentScope, validScopes, exact with
                        | x, _, _ when x.Equals context.Root.AnyScope ->
                            (LocContextResult.NewScope
                                { source with
                                    Scopes = applyTargetScope e.Target context.Scopes })
                        // | _, [], _ -> (context, false), NotFound
                        | _, _, true ->
                            (LocContextResult.NewScope
                                { source with
                                    Scopes = applyTargetScope e.Target context.Scopes })
                        | current, ss, false -> LocContextResult.WrongScope(nextKey, current, ss))

                let commandMatch () =
                    // TODO: Add scope checking
                    let matchedCommand =
                        staticSettings.commands
                        |> List.tryFind (fun (c, _) -> c == nextKey)
                        |> Option.orElse (
                            dynamicSettings.scriptedLocCommands |> Seq.tryFind (fun (c, _) -> c == nextKey)
                        )

                    match
                        matchedCommand,
                        first,
                        staticSettings.parameterVariables
                        && nextKey.StartsWith("parameter:", StringComparison.OrdinalIgnoreCase)
                    with
                    | Some _, _, _ -> LocContextResult.Found(context.CurrentScope.ToString())
                    | _, _, true -> LocContextResult.Found(context.CurrentScope.ToString())
                    | None, false, false ->
                        let firstPart = nextKey.Split('@').[0]

                        match staticSettings.usesVariableCommands, dynamicSettings.setVariables.Contains firstPart with
                        | true, true -> LocContextResult.Found(context.CurrentScope.ToString())
                        | false, true -> LocContextResult.VariableScope context
                        | _, false -> LocNotFound nextKey
                    | None, true, false ->
                        // TODO: Add scope push
                        match dynamicSettings.eventTargets |> Seq.exists (fun (et, _) -> et == nextKey) with
                        | true ->
                            LocContextResult.NewScope
                                { source with
                                    Scopes = context.Root.AnyScope :: source.Scopes }
                        | false -> LocNotFound nextKey

                let variableCommandMatch () =
                    match staticSettings.variableCommands |> List.tryFind (fun c -> c == nextKey) with
                    | Some _ -> LocContextResult.Found "variable"
                    | None -> LocNotFound nextKey

                if inVariable then
                    variableCommandMatch ()
                else
                    onetooneMatch ()
                    |> Option.orElseWith effectMatch
                    |> Option.defaultWith commandMatch

            let locKeyFolder (result: LocContextResult) (nextKey: string) =
                match result with
                | LocContextResult.Start startContext -> inner (true, false, startContext) nextKey
                | LocContextResult.NewScope newContext -> inner (false, false, newContext) nextKey
                | LocContextResult.VariableScope lastContext ->
                    // Assumes this is allowed in this game
                    inner (false, true, lastContext) nextKey
                // | LocContextResult.Found endContext -> inner (false, endContext) nextKey
                | LocContextResult.Found endContext -> LocContextResult.LocNotFound nextKey //inner (false, endContext) nextKey
                | res -> res

            res
            |> Option.defaultWith (fun () -> keys |> List.fold locKeyFolder (LocContextResult.Start source))

    open System.Globalization

    // type LocContext
    let createJominiLocalisationCommandValidator (dataTypes: CWTools.Parser.DataTypeParser.JominiLocDataTypes) =
        fun
            (source: ScopeContext)
            (eventtargets: Collections.Map<string, Scope list>)
            (command: JominiLocCommand list) ->
            let convScopeToDataType (scope: Scope) = scopeManager.DataTypeForScope(scope)

            let command =
                command
                |> List.map (function
                    | Command(key, cs) -> key, cs)

            let keys = command |> List.map fst

            let inner (context: bool * string * bool) (nextKey: string) =
                let first, dataType, confident = context
                // TODO: Replace this with some smarter
                let savedScopedMatch () =
                    Map.tryFind nextKey eventtargets
                    |> Option.map (fun ets ->
                        let scope, confident =
                            match ets with
                            | [ x ] when x = scopeManager.AnyScope -> "Character", false
                            | [ x ] -> convScopeToDataType x, true
                            | [ x; y ] when x = scopeManager.AnyScope -> convScopeToDataType y, true
                            | x :: y :: _ when x = scopeManager.AnyScope -> convScopeToDataType y, false
                            | x :: _ -> convScopeToDataType x, false
                            | _ -> "Character", false

                        NewDataType((scope, confident)))

                let promoteMatch () =
                    dataTypes.promotes
                    |> Map.tryFind nextKey
                    |> Option.orElse (dataTypes.promotes |> Map.tryFind nextKey)
                    |> Option.map (fun newType ->
                        if Set.contains newType dataTypes.dataTypeNames then
                            NewDataType(newType, true)
                        else
                            Found newType)

                let globalFunctionConfidentMatch () =
                    dataTypes.confidentFunctions
                    |> Map.tryFind nextKey
                    |> Option.map (fun newType ->
                        if Set.contains newType dataTypes.dataTypeNames then
                            NewDataType(newType, true)
                        else
                            Found newType)

                let globalFunctionMatch () =
                    dataTypes.functions
                    |> Map.tryFind nextKey
                    |> Option.map (fun newType ->
                        if Set.contains newType dataTypes.dataTypeNames then
                            NewDataType(newType, true)
                        else
                            Found newType)

                let functionMatch () =
                    dataTypes.dataTypes
                    |> Map.tryFind dataType
                    |> Option.bind (fun dataTypeMap -> dataTypeMap |> Map.tryFind nextKey)
                    |> Option.map (fun newType ->
                        if Set.contains newType dataTypes.dataTypeNames then
                            NewDataType(newType, true)
                        else
                            Found newType)

                let rawTypeMatch () =
                    if dataTypes.dataTypes |> Map.containsKey nextKey then
                        Some(NewDataType(nextKey, true))
                    else
                        None

                let res =
                    match first with
                    | true ->
                        (globalFunctionConfidentMatch ()
                         |> Option.orElse (savedScopedMatch ())
                         |> Option.orElse (promoteMatch ())
                         |> Option.orElse (globalFunctionMatch ())
                         |> Option.orElse (rawTypeMatch ()))
                    | false -> functionMatch ()

                res
                |> Option.defaultWith (fun _ -> LocNotFoundInType(nextKey, dataType, confident))

            let locKeyFolder (result: LocContextResult) (nextKey: string) =
                match result with
                | Start startDataType when startDataType.CurrentScope = scopeManager.AnyScope ->
                    inner (true, "None", true) nextKey
                | Start startDataType -> inner (true, startDataType.CurrentScope.ToString(), true) nextKey
                | NewDataType(newDataType, confident) -> inner (false, newDataType, confident) nextKey
                | Found endDataType -> LocNotFound nextKey
                | LocNotFound n -> LocNotFound n
                | res -> res
            // TODO: Better scopecontext to starting datatype
            keys |> List.fold locKeyFolder (Start source)
// match res with
// | Start _ -> LocContextResult.Start source
// | NewDataType newDataType -> LocContextResult.LocNotFound newDataType
// | Found endDataType -> LocContextResult.Found
// keys |> List.fold (fun r k -> match r with | (Found (r, s) , f) -> inner ((f, r, s)) k |LocNotFound s, _ -> LocNotFound s, false) (Found ("this", []), true) |> fst
