namespace CWTools.Parser


open FParsec
open CWTools.Common.VIC2Constants
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Utilities.Utils

module VIC2Parser =

    let private parseModifier =
        function
        | "character" -> ModifierCategory.Character
        | "province" -> ModifierCategory.Province
        | "unit" -> ModifierCategory.Unit
        // |"country" -> ModifierCategory.Country
        |_ -> ModifierCategory.Any


    let loadModifiers filename fileString =
        let parsed = CKParser.parseString fileString filename
        match parsed with
        |Failure(e, _, _) -> log (sprintf "modifier file %s failed with %s" filename e); ([])
        |Success(s,_,_) ->
            let root = simpleProcess.ProcessNode() "root" (mkZeroFile filename) (s)
            root.Child "modifiers"
                |> Option.map (fun ms ->  ms.Values |> List.map(fun l -> {tag = l.Key; categories = [parseModifier (l.Value.ToRawString())]; core = true}))
                |> Option.defaultValue []

    let getLocCommands (node : Node) =
        let simple = node.Values |> List.map (fun v -> v.Key, [parseScope (v.Value.ToRawString())])
        let complex = node.Children |> List.map (fun v -> v.Key, (v.LeafValues |> Seq.map (fun lv -> parseScope (lv.Value.ToRawString())) |> List.ofSeq))
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


