namespace CWToolsCLI
open CWTools.Common.STLConstants
open CWTools.Games.Stellaris
open CWTools.Games.Stellaris.STLLookup
module Validator =
    open CWTools
    open CWTools.Games
    open CWTools.Parser
    open CWTools.Localisation
    open CWTools.Common
    open CWTools.Games.Files

    [<StructuredFormatDisplay("{position}\n{category}: {error}")>]
    type ValidationViewModelErrorRow =
        {
            category : string
            error : string
            position : string
        }
        override x.ToString() = x.category + ", " + x.error + ", " + x.position
    type ValidationViewModelParseRow =
        {
            file : string
            error : string
        }
        override x.ToString() = x.file + "\n" + x.error + "\n"
    type ValidationViewModelFileRow =
        {
            file : string
            scope : string
        }
        override x.ToString() = x.file + "," + x.scope.ToString()
    type STL (dir : string, scope : FilesScope, modFilter : string, triggers : DocEffect list, effects : DocEffect list) =
        //let langs = [Lang.STL STLLang.English; Lang.STL STLLang.German; Lang.STL STLLang.French; Lang.STL STLLang.Spanish; Lang.STL STLLang.Russian; Lang.STL STLLang.Polish; Lang.STL STLLang.BrazPor]
        let langs = [Lang.STL STLLang.English; Lang.STL STLLang.German; Lang.STL STLLang.French; Lang.STL STLLang.Spanish;]
        let options = {
            rootDirectory = dir
            scope = scope
            modFilter = Some modFilter
            validation = {
                validateVanilla = false
                experimental = true
                langs = langs
            }
            rules = None
            embedded = {
                triggers = triggers
                effects = effects
                modifiers = []
                embeddedFiles = []
                cachedResourceData = []
            }
        }
        let game = STLGame(options) :> IGame<STLComputedData>
        let parserErrors = game.ParserErrors
        member val folders = game.Folders
        member val parserErrorList = parserErrors() |> List.map (fun (f, e, p) -> {file = f; error = e})
        member __.validationErrorList() = game.ValidationErrors() |> List.map (fun (s,c,n,l,e, _) -> {category = s.GetType().Name ; error = e; position = n.ToString()})
        member val allFileList = game.AllFiles() |> List.map (function |EntityResource(f, p) -> {file = p.filepath; scope = p.scope} |FileResource(f, p) -> {file = p.filepath; scope = p.scope})
        member val scriptedTriggerList = game.ScriptedTriggers
        member val scriptedEffectList = game.ScriptedEffects
        member val localisationErrorList = game.LocalisationErrors true |> List.map (fun (s,c,n,l,e,_) -> {category = s.GetType().Name ; error = e; position = n.ToString()})
        member val references = game.References