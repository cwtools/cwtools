#r @"C:\Users\Thomas\.nuget\packages\dotnet.glob\3.0.5\lib\netstandard1.1\DotNet.Glob.dll"
#r @"C:\Users\Thomas\.nuget\packages\fparsec\1.0.4-rc3\lib\netstandard1.6\FParsec.dll"
#r @"C:\Users\Thomas\.nuget\packages\fparsec\1.0.4-rc3\lib\netstandard1.6\FParsecCS.dll"
#r @"C:\Users\Thomas\.nuget\packages\fsharp.collections.parallelseq\1.1.2\lib\netstandard2.0\FSharp.Collections.ParallelSeq.dll"
#r @"C:\Users\Thomas\.nuget\packages\fsharp.data\3.0.1\lib\netstandard2.0\FSharp.Data.dll"
#r @"C:\Users\Thomas\.nuget\packages\fsharpx.collections\2.0.0\lib\netstandard2.0\FSharpx.Collections.dll"
#r @"C:\Users\Thomas\.nuget\packages\sandwych.quickgraph.core\1.0.0\lib\netstandard2.0\Sandwych.QuickGraph.Core.dll"
#r @"C:\Users\Thomas\.nuget\packages\system.buffers\4.5.0\ref\netstandard2.0\System.Buffers.dll"
#r @"C:\Users\Thomas\.nuget\packages\system.reflection.emit.ilgeneration\4.3.0\ref\netstandard1.0\System.Reflection.Emit.ILGeneration.dll"
#r @"C:\Users\Thomas\.nuget\packages\system.reflection.emit.lightweight\4.3.0\ref\netstandard1.0\System.Reflection.Emit.Lightweight.dll"
#r @"C:\Users\Thomas\.nuget\packages\system.reflection.emit\4.3.0\ref\netstandard1.1\System.Reflection.Emit.dll"
#r @"C:\Users\Thomas\.nuget\packages\system.reflection.typeextensions\4.5.1\ref\netstandard2.0\System.Reflection.TypeExtensions.dll"
#r @"C:\Users\Thomas\.nuget\packages\system.runtime.compilerservices.unsafe\4.5.2\ref\netstandard2.0\System.Runtime.CompilerServices.Unsafe.dll"
#r @"C:\Users\Thomas\git\cwtools/CWTools/bin/Debug/netcoreapp3.0/CWTools.dll"

open System

open System.IO

open CWTools.Common
open FParsec
open CWTools.Parser
open CWTools.Rules
open CWTools.Utilities

open System.Text
Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

/// Previous rules
let rulesFiles =
    [
        @"C:\Users\Thomas\git\cwtools-ir-config\triggers.cwt"
        @"C:\Users\Thomas\git\cwtools-ir-config\list_triggers.cwt"
        @"C:\Users\Thomas\git\cwtools-ir-config\effects.cwt"
        @"C:\Users\Thomas\git\cwtools-ir-config\list_effects.cwt"
    ] |> List.map (fun fn -> fn, (System.IO.File.ReadAllText fn))

let rules, types, enums, complexenums, values =
            rulesFiles
                |> CWTools.Rules.RulesParser.parseConfigs (scopeManager.ParseScope()) (scopeManager.AllScopes) (scopeManager.AnyScope)
let oldTriggers = rules |> List.choose (function
                                        |AliasRule ("trigger", (LeafRule(SpecificField (SpecificValue(x)),_), _)) -> Some (StringResource.stringManager.GetStringForIDs x)
                                        |AliasRule ("trigger", (NodeRule(SpecificField (SpecificValue(x)),_), _)) -> Some (StringResource.stringManager.GetStringForIDs x)
                                        |_ -> None)
let oldEffects = rules |> List.choose (function
                                        |AliasRule ("effect", (LeafRule(SpecificField (SpecificValue(x)),_), _)) -> Some (StringResource.stringManager.GetStringForIDs x)
                                        |AliasRule ("effect", (NodeRule(SpecificField (SpecificValue(x)),_), _)) -> Some (StringResource.stringManager.GetStringForIDs x)
                                        |_ -> None)

eprintfn "%A" oldTriggers

let triggers = JominiParser.parseTriggerFilesRes @"C:\Users\Thomas\git\cwtools-ir-config/triggers.log"
let effects = JominiParser.parseEffectFilesRes @"C:\Users\Thomas\git\cwtools-ir-config/effects.log"

type Trait =
| Comparison
| Bool
| Date
| Omen
| CharacterScope
| Province
| Country
| Religion
| Culture
| Diplo
| Subject
| Heritage
| Loyalty
| PartyScope
| Party
| Deity
| Mission
| MissionTask
| Area
| Region
| Legion
| Pop

let traitParse (x : string) =
    match x with
        | "<, <=, =, !=, >, >="-> Comparison
        | "yes/no"-> Bool
        | "<, =, > valid date"-> Date
        | "class COmenDataBase key"-> Omen
        | "character scope"-> CharacterScope
        | "province id/province scope"-> Province
        | "country tag/country scope"-> Country
        | "class CReligionDatabase key/religion scope"-> Religion
        | "culture db key/culture scope"-> Culture
        | "class CDiplomaticStanceDatabase key"-> Diplo
        | "class CSubjectTypeDatabase key"-> Subject
        | "class CHeritageDatabase key" -> Heritage
        | "class CLoyaltyTypeDatabase key" -> Loyalty
        | "class CPartyTypeDataBase key" -> Party
        | "party scope" -> PartyScope
        | "deity scope" -> Deity
        | "class CMissionDefinitionDatabase key" -> Mission
        | "class CMissionTaskDefinitionDatabase key" -> MissionTask
        | "area id/area tag/area scope" -> Area
        | "region id/region tag/area scope" -> Region
        | "class CLegionDistinctionDatabase key" -> Legion
        | "class CPopTypeDataBase key" -> Pop
        | _ -> failwithf "%A" x

let traitToRHS (x : Trait) =
    match x with
        | Comparison -> "==", ["replace_me_comparison"]
        | Bool -> "=", ["replace_me_bool"]
        | Date -> "==", ["replace_me_date"]
        | Omen -> "=", ["replace_me_omen"]
        | CharacterScope -> "=", ["replace_me_character"]
        | Province -> "=", ["replace_me_province_id"; "replace_me_province_scope"]
        | Country -> "=", ["replace_me_country_tag"; "replace_me_country_scope"]
        | Religion -> "=", ["replace_me_religion_tag"; "replace_me_religion_scope"]
        | Culture -> "=", ["replace_me_culture_tag"; "replace_me_culture_scope"]
        | Diplo -> "=", ["replace_me_diplo"]
        | Subject -> "=", ["replace_me_subject_type"]
        | Heritage -> "=", ["replace_me_heritage_type"]
        | Loyalty -> "=", ["replace_me_loyalty_type"]
        | Party -> "=", ["replace_me_party_type"]
        | PartyScope -> "=", ["replace_me_party_scope"]
        | Deity -> "=", ["replace_me_deity_scope"]
        | Mission -> "=", ["replace_me_mission_type"]
        | MissonTask -> "=", ["replace_me_mission_task_type"]
        | Area -> "=", ["replace_me_area_scope"]
        | Region -> "=", ["replace_me_region_scope"]
        | Legion -> "=", ["replace_me_legion_type"]
        | Pop -> "=", ["replace_me_pop_type"]

let tscope = true

let anytemplate =
        """{
    ## cardinality = 0..1
    percent = value_float[0.0..1.0]
    ## cardinality = 0..1
    count = int_value_field
    ## cardinality = 0..1
    count = all
    alias_name[trigger] = alias_match_left[trigger]
}"""

let everytemplate =
        """{
    ## cardinality = 0..1
    limit = {
        alias_name[trigger] = alias_match_left[trigger]
    }
    ## cardinality = 0..inf
    alternative_limit = {
        alias_name[trigger] = alias_match_left[trigger]
    }
    alias_name[effect] = alias_match_left[effect]
}"""

let randomtemplate =
        """{
    ## cardinality = 0..1
    limit = {
        alias_name[trigger] = alias_match_left[trigger]
    }
    ## cardinality = 0..inf
    alternative_limit = {
        alias_name[trigger] = alias_match_left[trigger]
    }
    ## cardinality = 0..1
    weight = single_alias_right[weight_block]
    alias_name[effect] = alias_match_left[effect]
}"""

let orderedtemplate =
        """{
    ## cardinality = 0..1
    limit = {
        alias_name[trigger] = alias_match_left[trigger]
    }
    ## cardinality = 0..1
    # TODO: Work out what exactly this is restricted to
    order_by = value_field
    ## cardinality = 0..1
    max = int_value_field
    ## cardinality = 0..1
    max = int_value_field
    ## cardinality = 0..1
    position = int
    ## cardinality = 0..1
    check_range_bounds = no
    alias_name[effect] = alias_match_left[effect]
}"""
let tinner =
            """{
	alias_name[trigger] = alias_match_left[trigger]
}
"""
let anytriggers = triggers |> List.filter (fun (t : RawEffect) -> t.name.StartsWith("any_"))
let othertriggers = triggers |> List.filter (fun (t : RawEffect) -> t.name.StartsWith("any_") |> not)
let tout =  (fun (t : RawEffect) ->
                        let scopes =
                            match t.scopes with
                            | [] -> ""
                            | [x] -> "## scopes = " + x + "\n"
                            | xs ->
                                let scopes = xs |> List.map (fun s -> s.ToString()) |> String.concat " "
                                "## scopes = { " + scopes + " }\n"
                        let scopes = if tscope then scopes else ""
                        let any = t.name.StartsWith("any_")
                        let traitEq, traitRHSs =  t.traits |> Option.map (traitParse >> traitToRHS) |> Option.defaultValue ("=", ["replace_me"])
                        let rhs =
                            if any
                            then [anytemplate]
                            else traitRHSs
                        let desc = t.desc.Replace("\n", " ")
                        // sprintf "###%s\n%salias[trigger:%s] = %s\n\r" desc scopes t.name rhs)
                        rhs |> List.map (fun rhs -> sprintf "### %s\nalias[trigger:%s] %s %s\n\r" desc t.name traitEq rhs))
let atout = anytriggers |>  List.filter (fun e -> oldTriggers |> List.contains e.name |> not) |> List.collect tout |> String.concat("")
let otout = othertriggers |>  List.filter (fun e -> oldTriggers |> List.contains e.name |> not) |> List.collect tout |> String.concat("")
                // |> String.concat("")

oldTriggers |> List.filter (fun e -> anytriggers |> List.exists (fun t -> t.name = e) |> not)
            |> List.filter (fun e -> othertriggers |> List.exists (fun t -> t.name = e) |> not)
            |> List.iter (fun e -> eprintfn "removed: %s" e)

let filterfun (s : string) = if s.StartsWith "every_" || s.StartsWith "random_" || s.StartsWith "ordered_" then true else false

let itereffects = effects |> List.filter (fun (e : RawEffect) -> filterfun e.name)
let othereffects = effects |> List.filter (fun (e : RawEffect) -> filterfun e.name |> not)
let efun = (fun (t : RawEffect) ->
        let scopes =
            match t.scopes with
            | [] -> ""
            | [x] -> "## scopes = " + x + "\n"
            | xs ->
                let scopes = xs |> List.map (fun s -> s.ToString()) |> String.concat " "
                "## scopes = { " + scopes + " }\n"
        let scopes = if tscope then scopes else ""
        let rhs =
            match t.name with
            | x when x.StartsWith "every_" -> everytemplate
            | x when x.StartsWith "random_" -> randomtemplate
            | x when x.StartsWith "ordered_" -> orderedtemplate
            | _ -> "replace_me"
        let desc = t.desc.Replace("\n", " ")
        // sprintf "###%s\n%salias[effect:%s] = %s\n\r" desc scopes t.name rhs)
        sprintf "### %s\nalias[effect:%s] = %s\n\r" desc t.name rhs)
                // |> String.concat("")
let ieout = itereffects |>  List.filter (fun e -> oldEffects |> List.contains e.name |> not) |> List.map efun |> String.concat("")
let oeout = othereffects |>  List.filter (fun e -> oldEffects |> List.contains e.name |> not) |> List.map efun |> String.concat("")

oldEffects |> List.filter (fun e -> itereffects |> List.exists (fun t -> t.name = e) |> not)
            |> List.filter (fun e -> othereffects |> List.exists (fun t -> t.name = e) |> not)
            |> List.iter (fun e -> eprintfn "removed: %s" e)

File.WriteAllText("triggers.cwt", otout)
File.WriteAllText("list_triggers.cwt", atout)
File.WriteAllText("effects.cwt", oeout)
File.WriteAllText("list_effects.cwt", ieout)

// File.WriteAllText("test.test", triggers |> List.choose (fun t -> t.traits) |> List.distinct |> String.concat("\n"))