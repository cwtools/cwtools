namespace CWToolsScripts
open System.Text


module CWToolsScripts =
    open System

    open System.IO

    open CWTools.Common
    open FParsec
    open CWTools.Parser
    open CWTools.Rules
    open CWTools.Utilities

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

    [<EntryPoint>]
    let main argv =
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
        CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose


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
        let oldTriggers =
            rules
            |> List.choose
                (function |AliasRule ("trigger", (LeafRule(SpecificField (SpecificValue(x)),_), _)) -> Some (StringResource.stringManager.GetStringForIDs x)
                          |AliasRule ("trigger", (NodeRule(SpecificField (SpecificValue(x)),_), _)) -> Some (StringResource.stringManager.GetStringForIDs x) |_ -> None)
        let oldEffects =
            rules
            |> List.choose
                (function |AliasRule ("effect", (LeafRule(SpecificField (SpecificValue(x)),_), _)) -> Some (StringResource.stringManager.GetStringForIDs x)
                          |AliasRule ("effect", (NodeRule(SpecificField (SpecificValue(x)),_), _)) -> Some (StringResource.stringManager.GetStringForIDs x) |_ -> None)


        let triggers = JominiParser.parseTriggerFilesRes @"C:\Users\Thomas\git\cwtools/Scripts/triggers.log"
                        |> List.filter (fun t -> List.contains t.name oldTriggers |> not)
        let effects = JominiParser.parseEffectFilesRes @"C:\Users\Thomas\git\cwtools/Scripts/effects.log"
                        |> List.filter (fun t -> List.contains t.name oldEffects |> not)


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
                | x -> failwithf "%A" x

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
                | Heritage -> "=", ["replace_me_heritage"]

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
        let atout = anytriggers |> List.collect tout |> String.concat("")
        let otout = othertriggers |> List.collect tout |> String.concat("")
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
        let ieout = itereffects |> List.map efun |> String.concat("")
        let oeout = othereffects |> List.map efun |> String.concat("")

        File.WriteAllText("triggers.cwt", otout)
        File.WriteAllText("list_triggers.cwt", atout)
        File.WriteAllText("effects.cwt", oeout)
        File.WriteAllText("list_effects.cwt", ieout)

        // File.WriteAllText("test.test", triggers |> List.choose (fun t -> t.traits) |> List.distinct |> String.concat("\n"))
        //printfn "%A" argv
        0 // return an integer exit code
