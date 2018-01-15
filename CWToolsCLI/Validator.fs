namespace CWToolsCLI
module Validator =
    open CWTools
    open CWTools.Games
    open CWTools.Parser
    open CWTools.Localisation
    open CWTools.Common

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
            pass : bool
            time : int64
        }
        override x.ToString() = x.file + " , " + x.pass.ToString() + " in " + x.time.ToString()
    type STL (dir : string, scope : CWTools.Games.FilesScope, modFilter : string, triggers : Effect list, effects : Effect list) =
        //let langs = [Lang.STL STLLang.English; Lang.STL STLLang.German; Lang.STL STLLang.French; Lang.STL STLLang.Spanish; Lang.STL STLLang.Russian; Lang.STL STLLang.Polish; Lang.STL STLLang.BrazPor]
        let langs = [Lang.STL STLLang.English; Lang.STL STLLang.German; Lang.STL STLLang.French; Lang.STL STLLang.Spanish;]
        let game = STLGame(dir, scope, modFilter, triggers, effects, [], langs)
        let parserErrors = game.ParserErrors
        member val folders = game.Folders
        member val parserErrorList = parserErrors |> List.map (fun (f, e, p) -> {file = f; error = e})
        member __.validationErrorList() = game.ValidationErrors |> List.map (fun (s,e) -> {category = s.GetType().Name ; error = e; position = s.Position.ToString()})
        member val allFileList = game.AllFiles |> List.map (fun (f, p, t) -> {file = f; pass = p; time = t})
        member val scriptedTriggerList = game.ScripteTriggers
        member val scriptedEffectList = game.ScriptedEffects
        member val localisationErrorList = game.LocalisationErrors |> List.map (fun (s,e) -> {category = s.GetType().Name ; error = e; position = s.Position.ToString()})