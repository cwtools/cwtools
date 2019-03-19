namespace CWTools.Parser
open CWTools.Process
open CWTools.Common
open FParsec
open CWTools.Utilities.Utils

module UtilityParser =
    let parseLink (anyScope) (parseScope) (allScopes) (node : Node) =
        let name = node.Key
        let desc = node.TagText "desc"
        let inputScopes =
            match node.TagText "input_scopes", node.Child "input_scopes" with
            | x, _ when x <> "" -> [parseScope (node.TagText "input_scopes")]
            | "", Some child -> child.LeafValues |> List.ofSeq |> List.map (fun lv -> lv.ValueText |> parseScope)
            | _ -> allScopes
        let outputScope = if node.Has "output_scope" then node.TagText "output_scope" |> parseScope else anyScope
        match node.TagText "from_data" with
        |"yes" ->
            DataLink {
                EventTargetDataLink.name = name
                inputScopes = inputScopes
                outputScope = outputScope
                description = desc
                dataPrefix = node.TagText "prefix" |> (fun s -> if s = "" then None else Some s)
                sourceRuleType = node.TagText "data_source"
            }
        |_ ->
            SimpleLink (ScopedEffect(name, inputScopes, outputScope, EffectType.Both, desc, "", true))

    let loadEventTargetLinks anyScope parseScope allScopes filename fileString =
        let parsed = CKParser.parseString fileString filename
        match parsed with
        |Failure(e, _, _) -> log (sprintf "link file %s failed with %s" filename e); ([])
        |Success(s,_,_) ->
            let root = CWTools.Process.STLProcess.simpleProcess.ProcessNode() "root" (mkZeroFile filename) (s)
            root.Child "links"
                |> Option.map (fun l -> l.Children |> List.map (parseLink anyScope parseScope allScopes))
                |> Option.defaultValue []
