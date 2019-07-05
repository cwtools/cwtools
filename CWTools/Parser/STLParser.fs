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

    // let parseLink(node : Node) =
    //     let name = node.Key
    //     let desc = node.TagText "desc"
    //     let inputScopes =
    //         match node.TagText "input_scopes", node.Child "input_scopes" with
    //         | x, _ when x <> "" -> [parseScope (node.TagText "input_scopes")]
    //         | "", Some child -> child.Values |> List.map (fun lv -> lv.ValueText |> parseScope)
    //         | _ -> allScopes
    //     let outputScope = if node.Has "output_scope" then node.TagText "output_scope" |> parseScope else Scope.Any
    //     match node.TagText "from_data" with
    //     |"yes" ->
    //         DataLink {
    //             EventTargetDataLink.name = name
    //             inputScopes = inputScopes
    //             outputScope = outputScope
    //             description = desc
    //             dataPrefix = node.TagText "prefix" |> (fun s -> if s = "" then None else Some s)
    //             sourceRuleType = node.TagText "data_source"
    //         }
    //     |_ ->
    //         SimpleLink (ScopedEffect(name, inputScopes, outputScope, EffectType.Both, desc, "", true))

    // let loadEventTargetLinks filename fileString =
    //     let parsed = CKParser.parseString fileString filename
    //     match parsed with
    //     |Failure(e, _, _) -> log (sprintf "link file %s failed with %s" filename e); ([])
    //     |Success(s,_,_) ->
    //         let root = simpleProcess.ProcessNode() "root" (mkZeroFile filename) (s)
    //         root.Child "links"
    //             |> Option.map (fun l -> l.Children |> List.map parseLink)
    //             |> Option.defaultValue []
