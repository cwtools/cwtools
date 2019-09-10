namespace CWTools.Parser


open FParsec
open CWTools.Common.STLConstants
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Utilities.Utils
open CWTools.Common.NewScope

module STLParser =
    let getLocCommands (node : Node) =
        let simple = node.Values |> List.map (fun v -> v.Key, [scopeManager.ParseScope() (v.Value.ToRawString())])
        let complex = node.Children |> List.map (fun v -> v.Key, (v.LeafValues |> Seq.map (fun lv -> scopeManager.ParseScope() (lv.Value.ToRawString())) |> List.ofSeq))
        simple @ complex

    let loadLocCommands filename fileString =
        let parsed = CKParser.parseString fileString filename
        match parsed with
        |Failure(e, _, _) -> log (sprintf "loccommands file %s failed with %s" filename e); ([])
        |Success(s,_,_) ->
            let root = simpleProcess.ProcessNode() "root" (mkZeroFile filename) (s)
            root.Child "localisation_commands"
                |> Option.map getLocCommands
                |> Option.defaultValue []

