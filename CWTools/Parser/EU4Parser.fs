namespace CWTools.Parser


open FParsec
open System.IO
open CWTools.Common
open CWTools.Common.EU4Constants
open FSharp.Data
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Utilities.Utils

module EU4Parser =
    // let loadModifiers (fileStream : StreamReader) =
    //     let csv = CsvFile.Load(fileStream, hasHeaders=false)
    let private parseModifier =
        function
        |"province" -> ModifierCategory.Province
        |"country" -> ModifierCategory.Country
        |_ -> ModifierCategory.Any
    //     csv.Rows |> Seq.map(fun r -> r.Columns.[0])

    let loadModifiers filename fileString =
        let parsed = CKParser.parseString fileString filename
        match parsed with
        |Failure(e, _, _) -> eprintfn "modifier file %s failed with %s" filename e; ([])
        |Success(s,_,_) ->
            let root = simpleProcess.ProcessNode<Node>() "root" (mkZeroFile filename) (s |> List.rev)
            root.Child "modifiers"
                |> Option.map (fun ms ->  ms.Values |> List.map(fun l -> {tag = l.Key; categories = [parseModifier (l.Value.ToRawString())]; core = true}))
                |> Option.defaultValue []

