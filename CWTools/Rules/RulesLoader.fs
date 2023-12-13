namespace CWTools.Rules

open System.IO
open CWTools.Parser
open CWTools.Utilities
open CWTools.Common.NewScope
open CWTools.Common



module RulesLoader =
    let private loadFromConfigFilesInner defaultScopeInputs defaultModifiersInputs (configs: (string * string) list) =
        configs
        |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
        |> (fun f -> UtilityParser.initializeScopes f (Some defaultScopeInputs))

        configs
        |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifier_categories.cwt")
        |> (fun f -> UtilityParser.initializeModifierCategories f (Some(defaultModifiersInputs ())))


        let triggers, effects =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "trigger_docs.log")
            |> Option.map (fun (fn, ft) -> DocsParser.parseDocsFile fn)
            |> Option.bind (function
                | FParsec.CharParsers.ParserResult.Success(p, _, _) ->
                    Some(DocsParser.processDocs scopeManager.ParseScopes p)
                | FParsec.CharParsers.ParserResult.Failure(e, _, _) ->
                    eprintfn $"%A{e}"
                    None)
            |> Option.defaultWith (fun () ->
                Utils.logError "trigger_docs.log was not found in stellaris config"
                ([], []))

        let modifiers =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "setup.log")
            |> Option.map (fun (fn, ft) -> SetupLogParser.parseLogsFile fn)
            |> Option.bind (function
                | FParsec.CharParsers.ParserResult.Success(p, _, _) -> Some(SetupLogParser.processLogs p)
                | FParsec.CharParsers.ParserResult.Failure(e, _, _) -> None)
            |> Option.defaultWith (fun () ->
                Utils.logError "setup.log was not found in stellaris config"
                [])

        let locCommands =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "localisation.cwt")
            |> Option.map (fun (fn, ft) -> UtilityParser.loadLocCommands fn ft)
            |> Option.defaultValue ([], [], [])

        let eventTargetLinks =
            configs
            |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "links.cwt")
            |> Option.map (fun (fn, ft) ->
                UtilityParser.loadEventTargetLinks
                    scopeManager.AnyScope
                    (scopeManager.ParseScope())
                    scopeManager.AllScopes
                    fn
                    ft)
            |> Option.defaultValue []

        (triggers, effects, modifiers, locCommands, eventTargetLinks)

    let loadAndInitializeFromConfigFiles (configs: (string * string) list) (game: Game) =
        match game with
        | Game.STL ->
            loadFromConfigFilesInner STLConstants.defaultScopeInputs STLConstants.defaultModifiersInputs configs
        | _ -> failwith "Other games not supported here yet"
