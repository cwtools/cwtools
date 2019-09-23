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
        | Clause kvl -> "{\n" + printKeyValueList kvl (depth + 1) + tabs (depth) + "}"
        | x -> x.ToString() + ""
    and printKeyValue (acc, leadingNewline, prevStart, prevEnd) kv depth =
        match kv with
        | Comment (r, c) ->
            if r.StartLine = prevStart && r.StartLine = prevEnd || (not leadingNewline)
            then acc + (tabs depth) + "#" + c, true, r.StartLine, r.EndLine
            else acc + "\n" + (tabs depth) + "#" + c, true, r.StartLine, r.EndLine
        | KeyValue (PosKeyValue(r, KeyValueItem (key, v, op))) ->
            acc + (if leadingNewline then "\n" else "") + (tabs depth) + key.ToString() + " " + operatorToString op + " " + (printValue v depth), true, r.StartLine, r.EndLine
        | Value (r, v) ->
            acc + (if leadingNewline then "\n" else "") + (tabs depth) + (printValue v depth), true, r.StartLine, r.EndLine
    and printKeyValueList kvl depth =
        kvl |> List.fold (fun acc kv -> printKeyValue acc kv depth) ("", false, -1, -1)
            |> (fun (res, leadingNewline, _, _) -> if leadingNewline then res + "\n" else res)
    let printTopLevelKeyValueList kvl =
        kvl |> List.fold (fun acc kv ->
            match kv with
            | KeyValue (PosKeyValue(_, KeyValueItem(_, Clause _, _))) as x ->
                let (res, a, b, c) = printKeyValue acc kv 0
                (res + "\n", a, b, c)
            | x -> printKeyValue acc kv 0
            ) ("", false, -1, -1)
            |> (fun (res, _, _, _) -> res)
            // |> (fun (res, leadingNewline, _, _) -> if leadingNewline then res + "\n" else res)

        // kvl |> List.map (
        //     function
        //     | KeyValue (PosKeyValue(_, KeyValueItem(_, Clause _, _))) as x -> printKeyValue x 0, true
        //     | x -> printKeyValue x 0, false
        // ) |> List.fold (fun (acc, start) (nextString, newline) -> if newline && (not start) then acc + nextString + "\n", false else acc + nextString, false) ("", true)
        // |> fst
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
