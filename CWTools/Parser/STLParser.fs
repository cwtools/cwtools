namespace CWTools.Parser


open FParsec
open System.IO
open CWTools.Common
open CWTools.Common.STLConstants
open FSharp.Data
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Utilities.Utils

module STLParser =
    let getLocCommands (node : Node) =
        let simple = node.Values |> List.map (fun v -> v.Key, [parseScope (v.Value.ToRawString())])
        let complex = node.Children |> List.map (fun v -> v.Key, (v.LeafValues |> Seq.map (fun lv -> parseScope (lv.Value.ToRawString())) |> List.ofSeq))
        simple @ complex

    let loadLocCommands filename fileString =
        let parsed = CKParser.parseString fileString filename
        match parsed with
        |Failure(e, _, _) -> log (sprintf "loccommands file %s failed with %s" filename e); ([])
        |Success(s,_,_) ->
            let root = simpleProcess.ProcessNode() "root" (mkZeroFile filename) (s |> List.rev)
            root.Child "localisation_commands"
                |> Option.map getLocCommands
                |> Option.defaultValue []
