namespace CWTools.Parser

open Types
open FParsec

module CKPrinter =
    let private tabs n = String.replicate n "\t"

    let private printTroop depth t = (tabs depth) + t.ToString()  + "\n"
    let private printValuelist depth is =
        let printOne = (fun i -> tabs (depth) + (string i) + "\n")
        List.map printOne is |> List.fold (+) ""


    let rec printValue v depth =
        match v with
        | Clause kvl -> "{ \n" + printKeyValueList kvl (depth + 1) + tabs (depth) + "}\n"
        | x -> x.ToString() + "\n"
    and printKeyValue kv depth =
        match kv with
        | Comment c -> (tabs depth) + "#" + c + "\n"
        | KeyValue (PosKeyValue(_, KeyValueItem (key, v, op))) -> (tabs depth) + key.ToString() + " " + operatorToString op + " " + (printValue v depth)
        | Value (_, v) -> (tabs depth) + (printValue v depth)
    and printKeyValueList kvl depth =
        kvl |> List.map (fun kv -> printKeyValue kv depth) |> List.fold (+) ""
    let printTopLevelKeyValueList kvl =
        kvl |> List.map (
            function
            | KeyValue (PosKeyValue(_, KeyValueItem(_, Clause _, _))) as x -> printKeyValue x 0, true
            | x -> printKeyValue x 0, false
        ) |> List.fold (fun (acc, start) (nextString, newline) -> if newline && (not start) then acc + nextString + "\n", false else acc + nextString, false) ("", true)
        |> fst
    let prettyPrint ef =
        let (ParsedFile sl) = ef
        printKeyValueList sl 0
    let prettyPrintResult =
        function
        | Success (v,_,_) ->
            let (ParsedFile ev) = v
            printKeyValueList ev 0
        | Failure (msg, _, _) -> msg

    let api =
        {
        prettyPrintFile = prettyPrint
        prettyPrintStatements = (fun f -> printKeyValueList f 0)
        prettyPrintFileResult = prettyPrintResult
        }
