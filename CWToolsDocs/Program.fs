open System.IO
open CWTools.Parser
open CWTools.Common.NewScope

let rec getAllFolders dirs =
    if Seq.isEmpty dirs then Seq.empty else
        seq { yield! dirs |> Seq.collect Directory.EnumerateDirectories
              yield! dirs |> Seq.collect Directory.EnumerateDirectories |> getAllFolders }
let getAllFoldersUnion dirs =
    seq {
        yield! dirs
        yield! getAllFolders dirs
    }

let getConfigFiles(rulesDir : string) =
    let configFiles = (if Directory.Exists rulesDir then getAllFoldersUnion ([rulesDir] |> Seq.ofList) else Seq.empty)
                        |> Seq.collect (Directory.EnumerateFiles)
                        |> List.ofSeq
                        |> List.filter (fun f -> Path.GetExtension f = ".cwt")

    let configs =
        match configFiles.Length > 0 with
        | true -> configFiles |> List.map (fun f -> f, File.ReadAllText(f)) //["./config.cwt", File.ReadAllText("./config.cwt")]
        | false -> []
    configs

open CWTools.Rules

[<EntryPoint>]
let main argv =
    let configPath =
        match argv with
        | [||] -> "./testconfig/"
        | [|x|] -> x
        | _ -> "./testconfig/"

    let rulesFiles = getConfigFiles(configPath)

    rulesFiles |> List.tryFind (fun (fileName, _) -> Path.GetFileName fileName = "scopes.cwt")
        |> (fun f -> UtilityParser.initializeScopes f (Some []) )

    rulesFiles |> List.tryFind (fun (fileName, _) -> Path.GetFileName fileName = "modifier_categories.cwt")
        |> (fun f -> UtilityParser.initializeModifierCategories f (Some []) )

    let rules, types, enums, _, _ =
        rulesFiles
            |> List.filter (fun (fileName, _) -> Path.GetExtension fileName = ".cwt" )
            |> RulesParser.parseConfigs (scopeManager.ParseScope()) scopeManager.AllScopes scopeManager.AnyScope scopeManager.ScopeGroups

    let renderedHtml = HtmlTemplates.rootRules rules enums types

    File.WriteAllText ("output.html", renderedHtml)

    0 // return an integer exit code
