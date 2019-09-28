// Learn more about F# at http://fsharp.org

open System
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
    let configFiles = (if Directory.Exists rulesDir then getAllFoldersUnion ([rulesDir] |> Seq.ofList) else Seq.empty) |> Seq.collect (Directory.EnumerateFiles)
    let configFiles = configFiles |> List.ofSeq |> List.filter (fun f -> Path.GetExtension f = ".cwt")
    let configs =
        match true, configFiles.Length > 0 with
        |false, _ -> []
        |_, true ->
            configFiles |> List.map (fun f -> f, File.ReadAllText(f))
            //["./config.cwt", File.ReadAllText("./config.cwt")]
        |_, false -> []
    configs
open CWTools.Rules
let printTypeRule ((typeName : string), ((rule, options) : NewRule)) =

    eprintfn "%A" rule

[<EntryPoint>]
let main argv =
    let configPath, sourceRepoUrl =
        match argv with
        | [||] -> failwith "Expected two arguments, a path to rules, and an optional URL to the source repository"
        | [|x|] -> x, None
        | [|x; y|] -> x, Some y
        | _ -> "./testconfig/", None
    // let configPath = "./testconfig"
    let rulesFiles = getConfigFiles(configPath)
    rulesFiles |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "scopes.cwt")
        |> (fun f -> UtilityParser.initializeScopes f (Some []) )

    rulesFiles |> List.tryFind (fun (fn, _) -> Path.GetFileName fn = "modifier_categories.cwt")
        |> (fun f -> UtilityParser.initializeModifierCategories f (Some []) )

    let rules, types, enums, complexenums, values =
        rulesFiles
            |> List.filter (fun (fn, ft) -> Path.GetExtension fn = ".cwt" )
            |> CWTools.Rules.RulesParser.parseConfigs (scopeManager.ParseScope()) scopeManager.AllScopes scopeManager.AnyScope
    let urlCreator = sourceRepoUrl |> Option.map (fun sru -> HtmlTemplates.getURLForPosition configPath (sprintf "%s/edit/master" sru))
    // let urlCreator = HtmlTemplates.getURLForPosition configPath "https://www.github.com/tboby/cwtools-stellaris-config/edit/master"
    let renderedHtml = HtmlTemplates.rootRules rules enums types
    let renderedWikitext = HtmlTemplates.rootRulesWT rules enums types
    File.WriteAllText ("output.html", renderedHtml urlCreator)
    File.WriteAllText ("output.txt", renderedWikitext urlCreator)
    // printfn "The rendered html document: \n\n%s\n" renderedHtml


    0 // return an integer exit code
