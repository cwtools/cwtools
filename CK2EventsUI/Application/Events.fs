namespace CK2Events.Application

open System.IO
open FParsec
open CWTools.Common
open CWTools.Parser
open CWTools.Localisation
open CWTools.Process.CK2Process
open CWTools.Parser.Types

module Events =
    let rand = System.Random()

    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    // shuffle an array (in-place)
    let shuffle a =
        Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a

    let parseAll directory (printer : PrinterAPI) =
        Directory.EnumerateFiles(directory)
        |> Array.ofSeq
        |> Array.Parallel.map (fun f -> (f, (CKParser.parseEventFile >> printer.prettyPrintFileResult) f))
        |> List.ofArray

    let parseTen directory (printer : PrinterAPI) =
            let all = Directory.EnumerateFiles(directory)
                    |> Array.ofSeq
            shuffle all
            Array.take 10 all
            |> Array.Parallel.map (fun f -> (f, (CKParser.parseEventFile >> printer.prettyPrintFileResult) f))
            |> List.ofArray
    
    let getFileList directory =
        Directory.EnumerateFiles(directory)
        |> List.ofSeq
        |> List.sort
        |> List.map Path.GetFileNameWithoutExtension    

    let getProcessedEvent (settings : CK2Settings) (game : Game) (localisation : ILocalisationAPI) file =
        let filePath = settings.Directory(game).eventDirectory + file + ".txt"
        let fileString = File.ReadAllText(filePath, System.Text.Encoding.GetEncoding(1252))
        let t = (CKParser.parseEventString fileString file)
        match t with
            | Success(v, _, _) -> 
                let ck2 = processEventFile v 
                let comments = getEventComments ck2
                let immediates = getAllImmediates ck2
                let options = getEventsOptions localisation ck2
                let pretties = ck2.Events |> List.map (fun e -> (e.ID, CKPrinter.api.prettyPrintStatements e.ToRaw))
                let ck3 = addLocalisedDescAll ck2 localisation
                let events = getEventViews ck3
                (true, events, immediates, options, pretties, comments, "")
            | ParserResult.Failure(msg, _, _) -> 
                (false,[],[],[],[],[], msg)

    let getNamespace (settings : CK2Settings) (game : Game) file =
        let filePath = settings.Directory(game).eventDirectory + file + ".txt"
        let fileString = File.ReadAllText(filePath, System.Text.Encoding.GetEncoding(1252))
        let t = (CKParser.parseEventString fileString file)
        match t with
            | Success(v, _, _) -> processEventFile v |> (fun r -> r.Namespace)
            | _ -> ""