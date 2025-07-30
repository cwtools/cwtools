#i "nuget:C:\Users\Thomas\Git\cwtools\pkg"

#r "nuget: CWTools, 0.5.0-alpha"
#r "../CWTools/bin/Debug/netstandard2.0/CWTools.dll"

open System

open System.IO
open System.Security.Cryptography

open CWTools.Parser
open FParsec

// let triggers, effects = DocsParser.parseDocsFilesRes @"C:\Users\Thomas\git\cwtools\CWToolsTests\testfiles\validationtests\trigger_docs_2.1.0.txt"
let triggers, effects =
    DocsParser.parseDocsFilesRes @"C:\Users\Thomas\git\cwtools/Scripts/hoi4_trigger_docs.txt"

let tinner =
    """{
	alias_name[effect] = alias_match_left[effect]
}
"""

let tout =
    triggers
    |> List.map (fun t ->
        let scopes = t.scopes |> List.map (fun s -> s.ToString()) |> String.concat " "

        let scopes =
            if scopes = "" then
                ""
            else
                "## scopes = { " + scopes + " }\n"

        let any = t.name.StartsWith("any_")
        let rhs = if any then tinner else "replace_me"
        sprintf "###%s\n%salias[trigger:%s] = %s\n\r" t.desc scopes t.name rhs)
    |> String.concat ("")

let einner =
    """{
    ## cardinality = 0..1
	limit = {
		alias_name[trigger] = alias_match_left[trigger]
	}
	alias_name[effect] = alias_match_left[effect]
}
"""

let eout =
    effects
    |> List.map (fun t ->
        let scopes = t.scopes |> List.map (fun s -> s.ToString()) |> String.concat " "

        let scopes =
            if scopes = "" then
                ""
            else
                "## scopes = { " + scopes + " }\n"

        let every = t.name.StartsWith("every_") || t.name.StartsWith("random_")
        let rhs = if every then einner else "replace_me"
        sprintf "###%s\n%salias[effect:%s] = %s\n\r" t.desc scopes t.name rhs)
    |> String.concat ("")

File.WriteAllText("triggers.cwt", tout)
File.WriteAllText("effects.cwt", eout)


printfn "%A" triggers
