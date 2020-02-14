namespace Reporters
open Chiron.Builder
open Chiron
open CWToolsCLI.Validator
open Pastel
open System.Drawing
open System.IO
open System.Text
open System
open CWTools.Parser

type Reporter =
| CLI
| CSV
| JSON

module Json =
    let writeNested key (toJson : _ -> Json<unit>) value : Json<unit> =
      fun json ->
        let inner = snd (toJson value json)
        Json.write key inner json

    let fold (f: _ -> Json<_>) xs : Json<unit> =
         fun json ->
            JsonResult.Value (), List.fold (fun json x ->
                snd ((f x) json)) json xs

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
                sb.Append("file,line,severity,code,message") |> ignore
                errors
                    |> List.iter (fun e ->
                        e |> function |ValidationViewModelRow.Parse(r) -> sb.Append(sprintf "%s%s,%s,%s,%s,\"%s\"" ne r.file "0" "error" "CW001" (r.message.Replace(ne,""))) |> ignore
                                      |Error(r) ->  sb.Append(sprintf "%s%s,%s,%s,%s,\"%s\"" ne r.position.FileName (r.position.StartLine.ToString()) (r.severity.ToString()) r.category (r.message.Replace(ne,"").Replace("\n",""))) |> ignore)
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

    type Value with
        static member ToJson (v : Value) =
                    match v with
                    | String s -> Json.write "value" s
                    | QString s -> Json.write "value" ("\""+s+"\"")
                    | Bool b -> Json.write "value" b
                    | Int i -> Json.write "value" i
                    | Float f -> Json.write "value" f
                    | Clause c -> Json.fold (fun x -> Json.writeNested "value" Statement.ToJson x) c
                    //Json.Optic.set Aether.Optics.id_ (Chiron.Array (c |> List.map (fun c -> Json.writeNested "value" Statement.ToJson c )))

    and Statement with
        static member ToJson ( s : Statement) =
            match s with
            | Comment (_, c) -> Json.write "comment" c
            | Value (_, v) -> Json.writeNested "value" Value.ToJson v
            | KeyValue kvi -> Json.writeNested "keyvalue" PosKeyValue.ToJson kvi
    and KeyValueItem with
        static member ToJson ( kvi : KeyValueItem ) =
            let k, v, o = kvi |> function | KeyValueItem (k, v, o) -> k, v, o
            let key = k |> function | Key k -> k
            json {
                do! Json.write "key" key
                do! Json.write "operator" (operatorToString o)
                do! Json.writeNested "value" Value.ToJson v
            }
    and PosKeyValue with
        static member ToJson ( pkv : PosKeyValue ) =
            let (r, kv) = pkv |> function | PosKeyValue (r, kv) -> r, kv
            json {
                //do! Json.write "range" r
                do! Json.writeNested "kv" KeyValueItem.ToJson kv
            }
    let listToJsonWith serialize lst =
        Json.Array <| List.map serialize lst

    type StatementList = {
        statements : Statement list
    } with
        static member ToJson (statements : StatementList) =
            Json.write "statements" (listToJsonWith (fun s -> Json.serializeWith Statement.ToJson s) statements.statements)

    
    open FParsec

    let newParse file =
        match CKParser.parseFile file with
        |Success(s,_,_) -> 
            let outputText = { statements = s } |> Json.serialize |> Json.format
            //let outputText = s |> Json.serializeWith (fun ss -> Json.fold (fun x -> Statement.ToJson x) ss ) |> Json.format
            true, outputText
        |Failure(msg,_,_) -> false, msg
