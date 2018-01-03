namespace CWToolsCLI
module Validator =
    open CWTools
    open CWTools.Games
    open CWTools.Parser

    [<StructuredFormatDisplay("{position}\n{category}: {error}")>]
    type ValidationViewModelErrorRow = 
        {
            category : string
            error : string
            position : string
        }
        override x.ToString() = x.position + "\n" + x.category + ", " + x.error
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
    type STL (dir : string, scope : CWTools.Games.FilesScope, triggers : Effect list, effects : Effect list) =
        let game = STLGame(dir, scope, triggers, effects)
        let validationErrors = game.ValidationErrors
        member val folders = game.Folders
        member val parserErrorList = game.ParserErrors |> List.map (fun (f, e) -> {file = f; error = e})
        member val validationErrorList = validationErrors |> List.map (fun (s,e) -> {category = s.GetType().Name ; error = e; position = s.Position.ToString()})
        member val allFileList = game.AllFiles |> List.map (fun (f, p, t) -> {file = f; pass = p; time = t})