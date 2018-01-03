namespace CWToolsCLI
open System.Text
open CWTools.Common
open System.IO
open CWTools.Parser
open FParsec

module CWToolsCLI =
    open Argu
    open Validator

    type Exiter() =
        interface IExiter with
            member __.Name = "paket exiter"
            member __.Exit (msg,code) =
                if code = ErrorCode.HelpText then
                    printfn "%s" msg ; exit 0
                else eprintfn "%s" msg ; exit 1

    type ListTypes =
        | Folders = 1
        | Files = 2
        | Triggers = 3
        | Effects = 4
    type ListSort =
        | Path = 1
        | Time = 2
    type ListArgs =
        | [<MainCommand; ExactlyOnce; Last>] ListType of ListTypes
        | Sort of ListSort option
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | ListType _ -> "Thing to list"
                | Sort _ -> "Sort by"
    type ValidateType =
        | ParseErrors = 1
        | Errors = 2
        | Warnings = 3
        | Info = 4
        | All = 5
    type ValidateArgs =
        | [<MainCommand; ExactlyOnce; Last>] ValType of ValidateType
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | ValType _ -> "Which errors to output"
    type Arguments =
        | Directory of path : string
        | Game of Game
        | Scope of CWTools.Games.FilesScope
        | [<CliPrefix(CliPrefix.None)>] Validate of ParseResults<ValidateArgs>
        | [<CliPrefix(CliPrefix.None)>] List of ParseResults<ListArgs>

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Directory _ -> "specify the main game directory"
                | Game _ -> "specify the game"
                | Validate _ -> "Validate all mod files"
                | List _ -> "List things"
                | Scope _ -> "which files to include"

    let parser = ArgumentParser.Create<Arguments>(programName = "CWToolsCLI.exe", errorHandler = new Exiter())

    let list game directory scope (results : ParseResults<ListArgs>) =
        let gameObj = STL(directory, scope, [], [])
        let sortOrder = results.GetResult <@ Sort @>
        match results.GetResult <@ ListType @> with
        | ListTypes.Folders -> printfn "%A" gameObj.folders
        | ListTypes.Files -> 
            match sortOrder with
            | None
            | Some ListSort.Path -> printfn "%A" gameObj.allFileList
            | Some ListSort.Time -> printfn "%A" (gameObj.allFileList |> List.sortByDescending (fun {time = t} -> t))
            | _ -> failwith "Unexpected sort order"
        | ListTypes.Triggers ->
            let triggers = DocsParser.parseDocs "C:\Users\Jennifer\Documents\Thomas\CK2Events\CK2EventsTests\game_triggers (1).txt"
            let t = triggers |>  (function |Success(p, _, _) -> p |_ -> [])
            printfn "%A" t
        | ListTypes.Effects ->
            let effects = DocsParser.parseDocs "C:\Users\Jennifer\Documents\Thomas\CK2Events\CK2EventsTests\game_effects (1).txt"
            let t = effects |>  (function |Success(p, _, _) -> p |_ -> [])
            printfn "%A" t
        | _ -> failwith "Unexpected list type"

    let validate game directory scope (results : ParseResults<_>) =
        let triggers = DocsParser.parseDocs "C:\Users\Jennifer\Documents\Thomas\CK2Events\CK2EventsTests\game_triggers (1).txt"
        let t = triggers |>  (function |Success(p, _, _) -> p |_ -> [])
        let effects = DocsParser.parseDocs "C:\Users\Jennifer\Documents\Thomas\CK2Events\CK2EventsTests\game_effects (1).txt"
        let e = effects |>  (function |Success(p, _, _) -> p |Failure(msg,_,_) -> failwith msg)
        let valType = results.GetResult <@ ValType @>
        let gameObj = STL(directory, scope, t, e)
        match valType with
        | ValidateType.ParseErrors -> printfn "%A" gameObj.parserErrorList
        | ValidateType.Errors -> printfn "%A" gameObj.validationErrorList
        | ValidateType.All -> printfn "%A" gameObj.parserErrorList;  printfn "%A" gameObj.validationErrorList
        | _ -> failwith "Unexpected validation type"

    [<EntryPoint>]
    let main argv =
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

        let results = parser.Parse argv
        let directory = results.GetResult <@ Directory @>
        let game = results.GetResult <@ Game @>
        let scope = results.GetResult <@ Scope @>
        match results.GetSubCommand() with
        | List r -> list game directory scope r
        | Validate r -> validate game directory scope r
        | Directory _
        | Game _ -> failwith "internal error: this code should never be reached"
        
        //printfn "%A" argv
        0 // return an integer exit code
