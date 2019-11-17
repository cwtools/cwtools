namespace CWToolsCLI
open CWTools.Common.STLConstants
open CWTools.Games.Stellaris
open CWTools.Games.Stellaris.STLLookup
open CWTools.Games.HOI4
open Chiron.Builder
open Chiron
open CWTools.Utilities.Position
open System.Security.Cryptography
open System.Text
module Validator =
    open CWTools
    open CWTools.Games
    open CWTools.Parser
    open CWTools.Localisation
    open CWTools.Common
    open CWTools.Games.Files

    type range with
        static member ToJson (r:range) =
            json {
                do! Json.write "startLine" r.StartLine
                do! Json.write "startColumn" r.StartColumn
                do! Json.write "endLine" r.EndLine
                do! Json.write "endColumn" r.EndColumn
            }

    [<StructuredFormatDisplay("{position}\n{category}: {error}")>]
    type ValidationViewModelErrorRow =
        {
            category : string
            message : string
            position : range
            severity : Severity
            hash : string
        }
        override x.ToString() = x.category + ", " + x.message + ", " + x.position.ToString()

    type ValidationViewModelParseRow =
        {
            file : string
            message : string
            hash : string
        }
        override x.ToString() = x.file + "\n" + x.message + "\n"
    type ValidationViewModelFileRow =
        {
            file : string
            scope : string
        }
        override x.ToString() = x.file + "," + x.scope.ToString()
    type ValidationViewModelRow =
    |Error of ValidationViewModelErrorRow
    |Parse of ValidationViewModelParseRow
        static member ToJson (r:ValidationViewModelRow) =
            match r with
            | Error r ->
                let pos = r.position |> Json.serializeWith range.ToJson
                json {
                    do! Json.write "category" r.category
                    do! Json.write "message" r.message
                    do! Json.write "position" pos
                    do! Json.write "severity" (r.severity.ToString())
                    do! Json.write "hash" r.hash
                }
            | Parse r ->
                let pos = mkRange r.file pos0 pos0 |> Json.serializeWith range.ToJson
                json {
                    do! Json.write "category" "CW001"
                    do! Json.write "message" r.message
                    do! Json.write "position" pos
                    do! Json.write "severity" "error"
                    do! Json.write "hash" r.hash
                }
    let sha = SHA256.Create()
    let createHash (path : string) (position : range) (message : string) =
        let combined = path + (position.ToString()) + message
        let hashed = sha.ComputeHash(Encoding.UTF8.GetBytes(combined))
        let sb = StringBuilder()
        let res = hashed |> Array.fold(fun (acc : StringBuilder) b -> acc.Append(b.ToString("x2"))) sb
        res.ToString()

    type STL (dir : string, scope : FilesScope, modFilter : string, triggers : DocEffect list, effects : DocEffect list, config) =
        //let langs = [Lang.STL STLLang.English; Lang.STL STLLang.German; Lang.STL STLLang.French; Lang.STL STLLang.Spanish; Lang.STL STLLang.Russian; Lang.STL STLLang.Polish; Lang.STL STLLang.BrazPor]
        let langs = [Lang.STL STLLang.English; Lang.STL STLLang.German; Lang.STL STLLang.French; Lang.STL STLLang.Spanish;]
        let STLoptions : StellarisSettings = {
            rootDirectories = [ {path = dir; name = "undefined"}]
            modFilter = Some modFilter
            validation = {
                validateVanilla = false
                experimental = true
                langs = langs
            }
            rules = Some { ruleFiles = config; validateRules = false; debugRulesOnly = false; debugMode = false }
            embedded = ManualSettings {
                triggers = triggers
                effects = effects
                modifiers = []
                embeddedFiles = []
                cachedResourceData = []
                localisationCommands = Legacy ([], [])
                eventTargetLinks = []
            }
            scriptFolders = None
            excludeGlobPatterns = None
            maxFileSize = None
        }
        let HOI4options : HOI4Settings = {
            rootDirectories = [ {path = dir; name = "undefined"}]
            modFilter = Some modFilter
            validation = {
                validateVanilla = false
                experimental = true
                langs = langs
            }
            rules = Some { ruleFiles = config; validateRules = false; debugRulesOnly = false; debugMode = false }
            embedded = ManualSettings {
                modifiers = []
                embeddedFiles = []
                cachedResourceData = []
                triggers = []
                effects = []
                localisationCommands = Legacy ([], [])
                eventTargetLinks = []
            }
            scriptFolders = None
            excludeGlobPatterns = None
            maxFileSize = None
        }
        let game = STLGame(STLoptions) :> IGame<STLComputedData>
            // |Game.HOI4 -> HOI4Game(HOI4options) :> IGame<HOI4ComputedData, HOI4Constants.Scope>
        let parserErrors = game.ParserErrors
        member val folders = game.Folders
        member val parserErrorList = parserErrors() |> List.map (fun (f, e, p) -> {file = f; message = e; hash = (createHash f (range.Zero) e)})
        member __.validationErrorList() = game.ValidationErrors() |> List.map (fun e -> {category = e.code ; message = e.message; position = e.range; severity = e.severity; hash = (createHash e.range.FileName e.range e.message )})
        member __.allFileList =
            game.AllFiles()
                |> List.map (function |EntityResource(f, p) -> {file = p.filepath; scope = p.scope} |FileResource(f, p) -> {file = p.filepath; scope = p.scope} |FileWithContentResource(f, p) -> {file = p.filepath; scope = p.scope})
        member val scriptedTriggerList = game.ScriptedTriggers
        member val scriptedEffectList = game.ScriptedEffects
        member __.localisationErrorList() = game.LocalisationErrors (true, true) |> List.map (fun e -> {category = e.code ; message = e.message; position = e.range; severity = e.severity; hash = (createHash e.range.FileName e.range e.message )})
        member val references = game.References
        member __.entities() = game.AllEntities()
        member __.recompute() = game.ForceRecompute()
    type ErrorGame (dir : string, scope : FilesScope, modFilter : string, config, game : Game, cached, cachedFiles) =
        let langs = [Lang.STL STLLang.English; Lang.STL STLLang.German; Lang.STL STLLang.French; Lang.STL STLLang.Spanish;]
        let langs = [Lang.HOI4 HOI4Lang.English; Lang.HOI4 HOI4Lang.German; Lang.HOI4 HOI4Lang.French; Lang.HOI4 HOI4Lang.Spanish;]
        let STLoptions : StellarisSettings = {
            rootDirectories = [ {path = dir; name = "undefined"}]
            modFilter = Some modFilter
            validation = {
                validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
                experimental = true
                langs = langs
            }
            rules = Some { ruleFiles = config; validateRules = false; debugRulesOnly = false; debugMode = false }
            embedded = FromConfig (cachedFiles, cached)
            scriptFolders = None
            excludeGlobPatterns = None
            maxFileSize = None
        }
        let HOI4options : HOI4Settings = {
            rootDirectories = [ {path = dir; name = "game"}]
            modFilter = Some modFilter
            validation = {
                validateVanilla = scope = FilesScope.All || scope = FilesScope.Vanilla
                experimental = true
                langs = langs
            }
            rules = Some { ruleFiles = config; validateRules = true; debugRulesOnly = false; debugMode = false }
            embedded = FromConfig (cachedFiles, cached)
            scriptFolders = None
            excludeGlobPatterns = None
            maxFileSize = Some 8

        }
        let game =
            match game with
            |Game.HOI4 -> HOI4Game(HOI4options) :> IGame
            |Game.STL -> STLGame(STLoptions) :> IGame
            // |Game.HOI4 -> HOI4Game(HOI4options) :> IGame<HOI4ComputedData, HOI4Constants.Scope>
        let parserErrors = game.ParserErrors
        member val folders = game.Folders
        member val parserErrorList = parserErrors() |> List.map (fun (f, e, p) -> {file = f; message = e;  hash = (createHash f (range.Zero) e)})
        member __.validationErrorList() = game.ValidationErrors() |> List.map (fun e -> {category = e.code ; message = e.message; position = e.range; severity = e.severity; hash = (createHash e.range.FileName e.range e.message )})
        member __.allFileList = game.AllFiles() |> List.map (function |EntityResource(f, p) -> {file = p.filepath; scope = p.scope} |FileResource(f, p) -> {file = p.filepath; scope = p.scope} |FileWithContentResource(f, p) -> {file = p.filepath; scope = p.scope})
        member __.localisationErrorList() = game.LocalisationErrors (true, true) |> List.map (fun e -> {category = e.code ; message = e.message; position = e.range; severity = e.severity; hash = (createHash e.range.FileName e.range e.message )})
        member __.recompute() = game.ForceRecompute()