namespace CWTools.Parser


open FParsec
open System.IO
open CWTools.Common
open CWTools.Common.CK2Constants
open FSharp.Data
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Utilities.Utils

module CK2Parser =

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

    let parseLink(node : Node) =
        let name = node.Key
        let desc = node.TagText "desc"
        let inputScopes =
            match node.TagText "input_scopes", node.Child "input_scopes" with
            | x, _ when x <> "" -> [parseScope (node.TagText "input_scopes")]
            | "", Some child -> child.LeafValues |> List.ofSeq |> List.map (fun lv -> lv.ValueText |> parseScope)
            | _ -> allScopes
        let outputScope = if node.Has "output_scope" then node.TagText "output_scope" |> parseScope else Scope.Any
        ScopedEffect(name, inputScopes, outputScope, EffectType.Both, desc, "", true)

    let loadEventTargetLinks filename fileString =
        let parsed = CKParser.parseString fileString filename
        match parsed with
        |Failure(e, _, _) -> log (sprintf "link file %s failed with %s" filename e); ([])
        |Success(s,_,_) ->
            let root = simpleProcess.ProcessNode() "root" (mkZeroFile filename) (s)
            root.Child "links"
                |> Option.map (fun l -> l.Children |> List.map parseLink)
                |> Option.defaultValue []
