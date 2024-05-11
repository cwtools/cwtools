module CWToolsScripts.Vic3


open System.IO

open System.Text
open CWTools.Common
open CWTools.Parser

open CWTools.Rules
open CWTools.Utilities
type Trait =
    | Comparison
    | Bool
    | Date
    | Country
    | Front
    | LawType
    | Culture
    | Goods
    | Religion
    | Party
    | InterestGroup
    
    
let generateVic3() =
    let root = @"C:\Users\Thomas\Git\cwtools-vic3-config\"
    let scopePath = root + @"config/scopes.cwt"
    let triggerPath = root + @"config/triggers.cwt"
    let listTriggerPath =    root + @"config/list_triggers.cwt"
    let effectPath = root + @"config/effects.cwt"
            
    let listEffectPath = root + @"config/list_effects.cwt"
    UtilityParser.initializeScopes (Some (scopePath, System.IO.File.ReadAllText scopePath)) (Some [])
    let rulesFiles =
        [
            triggerPath; listTriggerPath; effectPath; listEffectPath
        ] |> List.map (fun fn -> fn, (System.IO.File.ReadAllText fn))
    
    
    let rules, types, enums, complexenums, values =
                rulesFiles
                    |> CWTools.Rules.RulesParser.parseConfigs (scopeManager.ParseScope()) (scopeManager.AllScopes) (scopeManager.AnyScope) (scopeManager.ScopeGroups) true false
    
    // eprintfn "%A" (JominiParser.parseTriggerFile @"script-docs\triggers.log")
    let triggerDocsPath = root + @"script-docs\triggers.log"
    let triggers = JominiParser.parseTriggerFilesRes triggerDocsPath
    let effectDocsPath = root + @"script-docs\effects.log"
    let effects = JominiParser.parseEffectFilesRes effectDocsPath
    

    let traitParse (x : string) =
        match x with
            | "<, <=, =, !=, >, >="-> Comparison
            | "yes/no"-> Bool
            | "<, =, > valid date"-> Date
            | "country scope" -> Country
            | "front scope" -> Front
            | "law_type scope" -> LawType
            | "culture scope" -> Culture
            | "goods scope" -> Goods
            | "religion scope" -> Religion
            | "party scope" -> Party
            | "interest_group scope" -> InterestGroup
            | x -> failwithf "%A missing trait" x
    
    let traitToRHS (x : Trait) =
        match x with
            | Comparison -> "==", ["replace_me_comparison"]
            | Bool -> "=", ["replace_me_bool"]
            | Date -> "==", ["replace_me_date"]
            | Country -> "=", ["replace_me_country"]
            | Front -> "=", ["replace_me_front"]
            | LawType -> "=", ["replace_me_law_type"]
            | Culture -> "=", ["replace_me_culture"]
            | Goods -> "=", ["replace_me_goods"]
            | Religion -> "=", ["replace_me_religion"]
            | Party -> "=", ["replace_me_party"]
            | InterestGroup -> "=", ["replace_me_interest_group"]
    
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
                            // eprintfn "%A" t.traits
                            let traitEq, traitRHSs =  t.traits |> Option.map (traitParse >> traitToRHS) |> Option.defaultValue ("=", ["replace_me"])
                            let rhs =
                                if any
                                then [anytemplate]
                                else traitRHSs
                            let desc = t.desc.Replace("\n", " ")
                            // sprintf "###%s\n%salias[trigger:%s] = %s\n\r" desc scopes t.name rhs)
                            rhs |> List.map (fun rhs -> sprintf "### %s\nalias[trigger:%s] %s %s\n\r" desc t.name traitEq rhs))
                    // |> String.concat("")
    
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
    let oldTriggers = rules |> List.choose (function
                                            |AliasRule ("trigger", (LeafRule(SpecificField (SpecificValue(x)),_), _)) -> Some (StringResource.stringManager.GetStringForIDs x)
                                            |AliasRule ("trigger", (NodeRule(SpecificField (SpecificValue(x)),_), _)) -> Some (StringResource.stringManager.GetStringForIDs x)
                                            |_ -> None)
    let oldEffects = rules |> List.choose (function
                                            |AliasRule ("effect", (LeafRule(SpecificField (SpecificValue(x)),_), _)) -> Some (StringResource.stringManager.GetStringForIDs x)
                                            |AliasRule ("effect", (NodeRule(SpecificField (SpecificValue(x)),_), _)) -> Some (StringResource.stringManager.GetStringForIDs x)
                                            |_ -> None)
    
    let atout = anytriggers |>  List.filter (fun e -> oldTriggers |> List.contains e.name |> not) |> List.collect tout |> String.concat("")
    let otout = othertriggers |>  List.filter (fun e -> oldTriggers |> List.contains e.name |> not) |> List.collect tout |> String.concat("")
    
    oldTriggers |> List.filter (fun e -> anytriggers |> List.exists (fun t -> t.name = e) |> not)
                |> List.filter (fun e -> othertriggers |> List.exists (fun t -> t.name = e) |> not)
                |> List.iter (fun e -> eprintfn "removed: %s" e)
    
    let ieout = itereffects |>  List.filter (fun e -> oldEffects |> List.contains e.name |> not) |> List.map efun |> String.concat("")
    let oeout = othereffects |>  List.filter (fun e -> oldEffects |> List.contains e.name |> not) |> List.map efun |> String.concat("")
    
    oldEffects |> List.filter (fun e -> itereffects |> List.exists (fun t -> t.name = e) |> not)
                |> List.filter (fun e -> othereffects |> List.exists (fun t -> t.name = e) |> not)
                |> List.iter (fun e -> eprintfn "removed: %s" e)
    
    File.AppendAllText(triggerPath, otout)
    File.AppendAllText(listTriggerPath, atout)
    File.AppendAllText(effectPath, oeout)
    File.AppendAllText(listEffectPath, ieout)
    0 
        // File.WriteAllText("test.test", triggers |> List.choose (fun t -> t.traits) |> List.distinct |> String.concat("\n"))