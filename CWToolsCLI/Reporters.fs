namespace Reporters
open Chiron.Builder
open Chiron
open CWToolsCLI.Validator
open Pastel
open System.Drawing
open System.IO
open System.Text
open System


type Reporter =
| CLI
| CSV
| JSON

module Reporters =

    let ne = Environment.NewLine
    let cliReporter outputFile (errors : ValidationViewModelRow list) =
        let grouped = errors |> List.groupBy (fun e -> e |> function |ValidationViewModelRow.Parse(r) -> r.file |ValidationViewModelRow.Error(e) -> e.position.FileName)
                             |> List.sortBy (fun (k, _) -> k)
        let sb = new StringBuilder()
        let printFileErrors ((file : string), (errorList : ValidationViewModelRow list)) =
            sb.AppendLine(sprintf "%s, %i errors" file (errorList.Length)) |> ignore
            errorList
                |> List.map (fun r -> r |>
                                        function
                                        |ValidationViewModelRow.Error(e) -> sprintf "%s: %s,%s" (e.category) (e.message) (e.position.ToShortString())
                                        |ValidationViewModelRow.Parse(e) -> sprintf "%s: %s" ("CW001") (e.message))
                |> List.iter (fun es -> sb.AppendLine(sprintf "%s" (es.Pastel(Color.Red))) |> ignore)
        grouped |> List.iter printFileErrors
        match outputFile with
        |Some file ->
            File.WriteAllText(file, sb.ToString())
        |None -> printf "%s" (sb.ToString())


    let csvReporter outputFile (errors : ValidationViewModelRow list) =
        let result =
                let sb = new StringBuilder()
                sb.Append("file,error") |> ignore
                errors
                    |> List.iter (fun e -> e |> function |ValidationViewModelRow.Parse(r) -> sb.Append(ne).Append(r.file).Append(",\"").Append(r.message.Replace(ne, "")).Append("\"") |> ignore |Error(r) ->  sb.Append(ne).Append(r.position).Append(",").Append(r.message.Replace(ne,"")) |> ignore)
                sb.ToString()
                // List.fold (fun s e -> e |> function |ValidationViewModelRow.Parse(r) -> s + ne + r.file + ",\"" + r.error.Replace(System.Environment.NewLine,"") + "\"" |Error(r) -> s + ne + r.position + "," + r.error.Replace(ne, "")) "file,error" errors
        match outputFile with
        |Some file ->
            File.WriteAllText(file, result)
        |None -> printf "%s" result

    type GroupedValdationViewModelRows = {
        file : string
        errors : ValidationViewModelRow list
    }
    type GroupedValdationViewModelRows with
        static member ToJson ( g : GroupedValdationViewModelRows) =
            json {
                do! Json.write "file" g.file
                do! Json.write "errors" g.errors
            }
    type JsonErrorReport = {
        errors : GroupedValdationViewModelRows list
        errorCount : int
        supressedErrorCount : int
        rootDirectory : string
        }
    type JsonErrorReport with
        static member ToJson ( jer : JsonErrorReport) =
            json {
                do! Json.write "errorCount" jer.errorCount
                do! Json.write "supressedErrorCount" jer.supressedErrorCount
                do! Json.write "files" jer.errors
                do! Json.write "rootDirectory" jer.rootDirectory
            }
    let jsonReporter rootDirectory outputFile (errors : ValidationViewModelRow list) (supressedErrors : ValidationViewModelRow list) =
        let grouped = errors |> List.groupBy (fun e -> e |> function |ValidationViewModelRow.Parse(r) -> r.file |ValidationViewModelRow.Error(e) -> e.position.FileName)
                             |> List.map (fun (k, v) -> { file = k; errors = v })
                             |> List.sortBy (fun r -> r.file)
        let jsonErrorReport =
            {
                errors = grouped
                errorCount = errors.Length
                supressedErrorCount = supressedErrors.Length
                rootDirectory = rootDirectory
            }
        let outputText = jsonErrorReport |> Json.serialize |> Json.format
        match outputFile with
        |Some file ->
            File.WriteAllText(file, outputText)
        |None -> printf "%s" outputText
