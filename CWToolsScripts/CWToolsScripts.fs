namespace CWToolsScripts

open CWTools.Utilities.StringResource
open CWTools.Games.Files
open FSharp.Data
open FSharp.Data.JsonExtensions

open FSharpPlus.Control
open Vic3

module CWToolsScripts =
    open System

    open System.IO

    open CWTools.Common
    open CWTools.Parser
    open CWTools.Rules
    open CWTools.Utilities
    open CWTools.Games

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

    let generateIRFiles () =

        /// Previous rules
        let rulesFiles =
            [ @"C:\Users\Thomas\git\cwtools-ir-config\triggers.cwt"
              @"C:\Users\Thomas\git\cwtools-ir-config\list_triggers.cwt"
              @"C:\Users\Thomas\git\cwtools-ir-config\effects.cwt"
              @"C:\Users\Thomas\git\cwtools-ir-config\list_effects.cwt" ]
            |> List.map (fun fn -> fn, (System.IO.File.ReadAllText fn))

        let rules, types, enums, complexenums, values =
            rulesFiles
            |> CWTools.Rules.RulesParser.parseConfigs
                (scopeManager.ParseScope())
                (scopeManager.AllScopes)
                (scopeManager.AnyScope)
                (scopeManager.ScopeGroups)
                true
                false

        let oldTriggers =
            rules
            |> Array.choose (function
                | AliasRule("trigger", (LeafRule(SpecificField(SpecificValue(x)), _), _)) ->
                    Some(StringResource.stringManager.GetStringForIDs x)
                | AliasRule("trigger", (NodeRule(SpecificField(SpecificValue(x)), _), _)) ->
                    Some(StringResource.stringManager.GetStringForIDs x)
                | _ -> None)

        let oldEffects =
            rules
            |> Array.choose (function
                | AliasRule("effect", (LeafRule(SpecificField(SpecificValue(x)), _), _)) ->
                    Some(StringResource.stringManager.GetStringForIDs x)
                | AliasRule("effect", (NodeRule(SpecificField(SpecificValue(x)), _), _)) ->
                    Some(StringResource.stringManager.GetStringForIDs x)
                | _ -> None)

        let triggers =
            JominiParser.parseTriggerFilesRes @"C:\Users\Thomas\git\cwtools/Scripts/triggers.log"
            |> List.filter (fun t -> Array.contains t.name oldTriggers |> not)

        let effects =
            JominiParser.parseEffectFilesRes @"C:\Users\Thomas\git\cwtools/Scripts/effects.log"
            |> List.filter (fun t -> Array.contains t.name oldEffects |> not)


        let traitParse (x: string) =
            match x with
            | "<, <=, =, !=, >, >=" -> Comparison
            | "yes/no" -> Bool
            | "<, =, > valid date" -> Date
            | "class COmenDataBase key" -> Omen
            | "character scope" -> CharacterScope
            | "province id/province scope" -> Province
            | "country tag/country scope" -> Country
            | "class CReligionDatabase key/religion scope" -> Religion
            | "culture db key/culture scope" -> Culture
            | "class CDiplomaticStanceDatabase key" -> Diplo
            | "class CSubjectTypeDatabase key" -> Subject
            | "class CHeritageDatabase key" -> Heritage
            | x -> failwithf "%A" x

        let traitToRHS (x: Trait) =
            match x with
            | Comparison -> "==", [ "replace_me_comparison" ]
            | Bool -> "=", [ "replace_me_bool" ]
            | Date -> "==", [ "replace_me_date" ]
            | Omen -> "=", [ "replace_me_omen" ]
            | CharacterScope -> "=", [ "replace_me_character" ]
            | Province -> "=", [ "replace_me_province_id"; "replace_me_province_scope" ]
            | Country -> "=", [ "replace_me_country_tag"; "replace_me_country_scope" ]
            | Religion -> "=", [ "replace_me_religion_tag"; "replace_me_religion_scope" ]
            | Culture -> "=", [ "replace_me_culture_tag"; "replace_me_culture_scope" ]
            | Diplo -> "=", [ "replace_me_diplo" ]
            | Subject -> "=", [ "replace_me_subject_type" ]
            | Heritage -> "=", [ "replace_me_heritage" ]

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

        let anytriggers =
            triggers |> List.filter (fun (t: RawEffect) -> t.name.StartsWith("any_"))

        let othertriggers =
            triggers |> List.filter (fun (t: RawEffect) -> t.name.StartsWith("any_") |> not)

        let tout =
            (fun (t: RawEffect) ->
                let scopes =
                    match t.scopes with
                    | [] -> ""
                    | [ x ] -> "## scopes = " + x + "\n"
                    | xs ->
                        let scopes = xs |> List.map (fun s -> s.ToString()) |> String.concat " "
                        "## scopes = { " + scopes + " }\n"

                let scopes = if tscope then scopes else ""
                let any = t.name.StartsWith("any_")

                let traitEq, traitRHSs =
                    t.traits
                    |> Option.map (traitParse >> traitToRHS)
                    |> Option.defaultValue ("=", [ "replace_me" ])

                let rhs = if any then [ anytemplate ] else traitRHSs
                let desc = t.desc.Replace("\n", " ")
                // sprintf "###%s\n%salias[trigger:%s] = %s\n\r" desc scopes t.name rhs)
                rhs
                |> List.map (fun rhs -> sprintf "### %s\nalias[trigger:%s] %s %s\n\r" desc t.name traitEq rhs))

        let atout = anytriggers |> List.collect tout |> String.concat ("")
        let otout = othertriggers |> List.collect tout |> String.concat ("")
        // |> String.concat("")

        let filterfun (s: string) =
            if s.StartsWith "every_" || s.StartsWith "random_" || s.StartsWith "ordered_" then
                true
            else
                false

        let itereffects = effects |> List.filter (fun (e: RawEffect) -> filterfun e.name)

        let othereffects =
            effects |> List.filter (fun (e: RawEffect) -> filterfun e.name |> not)

        let efun =
            (fun (t: RawEffect) ->
                let scopes =
                    match t.scopes with
                    | [] -> ""
                    | [ x ] -> "## scopes = " + x + "\n"
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
        let ieout = itereffects |> List.map efun |> String.concat ("")
        let oeout = othereffects |> List.map efun |> String.concat ("")

        File.WriteAllText("triggers.cwt", otout)
        File.WriteAllText("list_triggers.cwt", atout)
        File.WriteAllText("effects.cwt", oeout)
        File.WriteAllText("list_effects.cwt", ieout)

        // File.WriteAllText("test.test", triggers |> List.choose (fun t -> t.traits) |> List.distinct |> String.concat("\n"))
        //printfn "%A" argv
        0 // return an integer exit code

    let emptyStellarisSettings (rootDirectory) =
        { CWTools.Games.GameSetupSettings.rootDirectories = [ WD { name = "test"; path = rootDirectory } ]
          modFilter = None
          validation =
            { validateVanilla = false
              experimental = true
              langs = [ STL STLLang.English ] }
          rules = None
          embedded = FromConfig([], [])
          scriptFolders = None
          excludeGlobPatterns = None
          maxFileSize = None
          debugSettings = DebugSettings.Default }

    let emptyHOI4Settings (rootDirectory) =
        { CWTools.Games.GameSetupSettings.rootDirectories = [ WD { name = "test"; path = rootDirectory } ]
          modFilter = None
          validation =
            { validateVanilla = false
              experimental = true
              langs = [ HOI4 HOI4Lang.English ] }
          rules = None
          embedded = FromConfig([], [])
          scriptFolders = None
          excludeGlobPatterns = None
          maxFileSize = None
          debugSettings = DebugSettings.Default }

    let rec getAllFolders dirs =
        if Seq.isEmpty dirs then
            Seq.empty
        else
            seq {
                yield! dirs |> Seq.collect Directory.EnumerateDirectories
                yield! dirs |> Seq.collect Directory.EnumerateDirectories |> getAllFolders
            }

    let getAllFoldersUnion dirs =
        seq {
            yield! dirs
            yield! getAllFolders dirs
        }

    let getFolderList (filename: string, filetext: string) =
        if Path.GetFileName filename = "folders.cwt" then
            Some(
                filetext.Split(([| "\r\n"; "\r"; "\n" |]), StringSplitOptions.None)
                |> List.ofArray
                |> List.filter (fun s -> s <> "")
            )
        else
            None

    let findUndefinedEffects () =
        let configFiles =
            (if Directory.Exists @"C:\Users\Thomas\git\cwtools-stellaris-config\" then
                 getAllFoldersUnion ([ @"C:\Users\Thomas\git\cwtools-stellaris-config\" ] |> Seq.ofList)
             else
                 Seq.empty)
            |> Seq.collect (Directory.EnumerateFiles)

        let configFiles =
            configFiles
            |> List.ofSeq
            |> List.filter (fun f -> Path.GetExtension f = ".cwt" || Path.GetExtension f = ".log")

        let configs = configFiles |> List.map (fun f -> f, File.ReadAllText(f))
        let folders = configs |> List.tryPick getFolderList

        let settings =
            { emptyStellarisSettings ("./") with
                rules =
                    Some
                        { ruleFiles = configs
                          validateRules = true
                          debugRulesOnly = false
                          debugMode = false } }

        let stl = CWTools.Games.Stellaris.STLGame(settings)
        let lookup = stl.Lookup
        let effects = lookup.effects |> List.map (fun e -> e.Name) |> Set.ofList
        let triggers = lookup.triggers |> List.map (fun e -> e.Name) |> Set.ofList

        let getEffect (rule: RootRule) =
            match rule with
            | AliasRule("effect", (LeafRule(SpecificField(SpecificValue vc), _), _)) -> Some(vc)
            | AliasRule("effect", (NodeRule(SpecificField(SpecificValue vc), _), _)) -> Some(vc)
            | _ -> None

        let getTrigger (rule: RootRule) =
            match rule with
            | AliasRule("trigger", (LeafRule(SpecificField(SpecificValue vc), _), _)) -> Some(vc)
            | AliasRule("trigger", (NodeRule(SpecificField(SpecificValue vc), _), _)) -> Some(vc)
            | _ -> None

        let triggerAliases = lookup.configRules |> Array.choose getTrigger |> Set.ofArray
        let effectAliases = lookup.configRules |> Array.choose getEffect |> Set.ofArray
        let undefinedTriggers = Set.difference triggers triggerAliases
        let undefinedEffects = Set.difference effects effectAliases

        if undefinedTriggers.IsEmpty then
            ()
        else
            printfn "The following triggers are present in trigger_docs but not in rules"

            undefinedTriggers
            |> Set.iter (fun t -> printfn "%s" (stringManager.GetStringForIDs t))

        if undefinedEffects.IsEmpty then
            ()
        else
            printfn "The following effects are present in trigger_docs but not in rules"

            undefinedEffects
            |> Set.iter (fun t -> printfn "%s" (stringManager.GetStringForIDs t))

        0

    let parseEffect ((effectName, effectInner): (string * JsonValue)) =
        let desc = effectInner?description
        let scopes = effectInner?supported_scope
        let targets = effectInner?supported_target

        { RawEffect.name = effectName
          desc = desc.AsString()
          usage = ""
          scopes = scopes.AsArray() |> List.ofArray |> List.map (fun j -> j.AsString())
          targets = targets.AsArray() |> List.ofArray |> List.map (fun j -> j.AsString())
          traits = None }

    let parseScriptDocsJson (path: string) =
        let value = JsonValue.Load path

        let effects =
            match value?effects with
            | JsonValue.Record es -> es |> Array.map parseEffect |> List.ofArray
            | _ -> []

        let triggers =
            match value?triggers with
            | JsonValue.Record ts -> ts |> Array.map parseEffect |> List.ofArray
            | _ -> []

        effects, triggers

    let tinner =
        """{
	alias_name[effect] = alias_match_left[effect]
}
"""

    let tout (triggers: RawEffect list) =
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
            let safeDesc = t.desc.Replace("\n", "")
            sprintf "###%s\n%salias[trigger:%s] = %s\n\r" safeDesc scopes t.name rhs)
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

    let eout (effects: RawEffect list) =
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
            let safeDesc = t.desc.Replace("\n", "")
            sprintf "###%s\n%salias[effect:%s] = %s\n\r" safeDesc scopes t.name rhs)
        |> String.concat ("")


    let findUndefinedEffectsHOI4 () =
        let configFiles =
            (if Directory.Exists @"C:\Users\Thomas\git\cwtools-hoi4-config\" then
                 getAllFoldersUnion ([ @"C:\Users\Thomas\git\cwtools-hoi4-config\" ] |> Seq.ofList)
             else
                 Seq.empty)
            |> Seq.collect (Directory.EnumerateFiles)

        let configFiles2 =
            configFiles
            |> List.ofSeq
            |> List.filter (fun f -> Path.GetExtension f = ".cwt" || Path.GetExtension f = ".log")

        let configs = configFiles2 |> List.map (fun f -> f, File.ReadAllText(f))
        let folders = configs |> List.tryPick getFolderList

        let settings =
            { emptyHOI4Settings ("./") with
                rules =
                    Some
                        { ruleFiles = configs
                          validateRules = true
                          debugRulesOnly = false
                          debugMode = false } }

        let hoi4 = CWTools.Games.HOI4.HOI4Game(settings)
        let lookup = hoi4.Lookup

        let effects, triggers =
            configFiles
            |> Seq.find (fun f -> Path.GetFileName f = "script_documentation.json")
            |> parseScriptDocsJson

        let effectsNames = effects |> List.map (fun e -> e.name) |> Set.ofList
        let triggersNames = triggers |> List.map (fun e -> e.name) |> Set.ofList

        let getEffect (rule: RootRule) =
            match rule with
            | AliasRule("effect", (LeafRule(SpecificField(SpecificValue vc), _), _)) ->
                Some(stringManager.GetStringForIDs vc)
            | AliasRule("effect", (NodeRule(SpecificField(SpecificValue vc), _), _)) ->
                Some(stringManager.GetStringForIDs vc)
            | _ -> None

        let getTrigger (rule: RootRule) =
            match rule with
            | AliasRule("trigger", (LeafRule(SpecificField(SpecificValue vc), _), _)) ->
                Some(stringManager.GetStringForIDs vc)
            | AliasRule("trigger", (NodeRule(SpecificField(SpecificValue vc), _), _)) ->
                Some(stringManager.GetStringForIDs vc)
            | _ -> None

        let triggerAliases = lookup.configRules |> Seq.choose getTrigger |> Set.ofSeq
        let effectAliases = lookup.configRules |> Seq.choose getEffect |> Set.ofSeq
        let undefinedTriggers = Set.difference triggersNames triggerAliases
        let undefinedEffects = Set.difference effectsNames effectAliases

        if undefinedTriggers.IsEmpty then
            ()
        else
            printfn "The following triggers are present in trigger_docs but not in rules"
            undefinedTriggers |> Set.iter (fun t -> printfn "%s" t)

            let res =
                triggers |> List.filter (fun t -> Set.contains t.name undefinedTriggers) |> tout

            File.WriteAllText(@"C:\Users\Thomas\git\cwtools-hoi4-config\\Config\triggers_new.cwt", res)

        if undefinedEffects.IsEmpty then
            ()
        else
            printfn "The following effects are present in trigger_docs but not in rules"
            undefinedEffects |> Set.iter (fun t -> printfn "%s" t)

            let res =
                effects |> List.filter (fun t -> Set.contains t.name undefinedEffects) |> eout

            File.WriteAllText(@"C:\Users\Thomas\git\cwtools-hoi4-config\\Config\effects_new.cwt", res)

        0

    let generateHOI4Modifiers () =
        let docs =
            File.ReadAllText(@"C:\Users\Thomas\git\cwtools-hoi4-config\Config\script_documentation.json")

        let json = JsonValue.Parse docs
        let modifiers = json?modifiers
        let mutable modifierList = []

        for modifier in modifiers do
            let mutable scopeList = []

            for scope in modifier?categories do
                scopeList <- scope.AsString() :: scopeList

            modifierList <- (modifier?name.AsString(), scopeList) :: modifierList

        let modifierList =
            modifierList
            |> List.map (fun (k, ls) -> k, ls |> Seq.map (fun s -> s.Replace("_", "")) |> String.concat "_")
        // let modifiers = modifiers |> Seq.map (fun m -> m?name.AsString(), m?categories |> Seq.map (fun s -> s.AsString()))
        //eprintfn "%A" (modifierList |> List.groupBy snd |> List.map (fun (k, g) -> k, g |> List.length))
        let modifierCategories = modifierList |> List.map snd |> List.distinct

        let modifierCategoriesFile =
            """modifierCategories = {
                """

        let modifierCategoryTemplate =
            """ = {

            }
            """

        let t = Printf.TextWriterFormat<string -> unit>(modifierCategoryTemplate)

        let modifierCategoryFile =
            modifierCategoriesFile :: modifierCategories
            |> List.map (fun s -> s + modifierCategoryTemplate)
            |> String.concat ""

        let modifiersFile = modifierList |> List.map (fun (k, vs) -> sprintf "")
        eprintfn "%A" modifierCategoryFile
        0

    [<EntryPoint>]
    let main argv =
        CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose

        if Array.tryHead argv = Some "ir" then
            generateIRFiles ()
        elif Array.tryHead argv = Some "unusedstl" then
            findUndefinedEffects ()
        elif Array.tryHead argv = Some "unusedhoi4" then
            findUndefinedEffectsHOI4 ()
        elif Array.tryHead argv = Some "hoi4modifiers" then
            generateHOI4Modifiers ()
        elif Array.tryHead argv = Some "vic3" then
            generateVic3 ()
        else
            0
