namespace Reporters
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

    let cliReporter (errors : ValidationViewModelRow list) =
        let grouped = errors |> List.groupBy (fun e -> e |> function |ValidationViewModelRow.Parse(r) -> r.file |ValidationViewModelRow.Error(e) -> e.position.FileName)
                             |> List.sortBy (fun (k, _) -> k)
        let printFileErrors ((file : string), (errorList : ValidationViewModelRow list)) =
            printfn "%s, %i errors" file (errorList.Length)
            errorList
                |> List.map (fun r -> r |>
                                        function
                                        |ValidationViewModelRow.Error(e) -> sprintf "%s: %s,%s" (e.category) (e.error) (e.position.ToShortString())
                                        |ValidationViewModelRow.Parse(e) -> sprintf "%s: %s" ("CW001") (e.error))
                |> List.iter (fun es -> printfn "%s" (es.Pastel(Color.Red)))
        grouped |> List.iter printFileErrors

    let ne = Environment.NewLine
    let csvReporter outputFile (errors : ValidationViewModelRow list) =
        let result =
                let sb = new StringBuilder()
                sb.Append("file,error") |> ignore
                errors
                    |> List.iter (fun e -> e |> function |ValidationViewModelRow.Parse(r) -> sb.Append(ne).Append(r.file).Append(",\"").Append(r.error.Replace(ne, "")).Append("\"") |> ignore |Error(r) ->  sb.Append(ne).Append(r.position).Append(",").Append(r.error.Replace(ne,"")) |> ignore)
                sb.ToString()
                // List.fold (fun s e -> e |> function |ValidationViewModelRow.Parse(r) -> s + ne + r.file + ",\"" + r.error.Replace(System.Environment.NewLine,"") + "\"" |Error(r) -> s + ne + r.position + "," + r.error.Replace(ne, "")) "file,error" errors
        match outputFile with
        |Some file ->
            File.WriteAllText(file, result)
        |None -> printf "%s" result

    let jsonReporter outputFile (errors : ValidationViewModelRow list) =
        ()