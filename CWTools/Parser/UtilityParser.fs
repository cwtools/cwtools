namespace CWTools.Parser
open CWTools.Process
open CWTools.Common
open FParsec
open CWTools.Utilities.Utils
open CWTools.Process.STLProcess
open CWTools.Common.NewScope
open CWTools.Utilities

module UtilityParser =
    let private parseLink (anyScope) (parseScope) (allScopes) (node : Node) =
        let name = node.Key
        let desc = node.TagText "desc"
        let inputScopes =
            match node.TagText "input_scopes", node.Child "input_scopes" with
            | x, _ when x <> "" -> [parseScope (node.TagText "input_scopes")]
            | "", Some child -> child.LeafValues |> List.ofSeq |> List.map (fun lv -> lv.ValueText |> parseScope)
            | _ -> allScopes
        let outputScope = if node.Has "output_scope" then node.TagText "output_scope" |> parseScope else anyScope
        let dataLinkType =
            match node.TagText "type" with
            | "scope" -> DataLinkType.Scope
            | "value" -> DataLinkType.Value
            | "both" -> DataLinkType.Both
            | _ -> DataLinkType.Scope
        match node.TagText "from_data" with
        |"yes" ->
            DataLink {
                EventTargetDataLink.name = name
                inputScopes = inputScopes
                outputScope = outputScope
                description = desc
                dataPrefix = node.TagText "prefix" |> (fun s -> if s = "" then None else Some s)
                sourceRuleType = node.TagText "data_source"
                dataLinkType = dataLinkType
            }
        |_ ->
            let effectType = if dataLinkType = DataLinkType.Value then EffectType.ValueTrigger else EffectType.Link
            SimpleLink (ScopedEffect(name, inputScopes, Some outputScope, effectType, desc, "", true))

    let loadEventTargetLinks anyScope parseScope allScopes filename fileString =
        let parsed = CKParser.parseString fileString filename
        match parsed with
        |Failure(e, _, _) -> log (sprintf "link file %s failed with %s" filename e); ([])
        |Success(s,_,_) ->
            let root = CWTools.Process.STLProcess.simpleProcess.ProcessNode() "root" (mkZeroFile filename) (s)
            root.Child "links"
                |> Option.map (fun l -> l.Children |> List.map (parseLink anyScope parseScope allScopes))
                |> Option.defaultValue []

    let private parseScopeDef (node : Node) =
        let name = node.Key
        let aliases = node.Child "aliases" |> Option.map (fun c -> c.LeafValues |> Seq.map (fun lv -> lv.ValueText) |> List.ofSeq)
                                          |> Option.defaultValue []
        let subscopes = node.Child "is_subscope_of" |> Option.map (fun c -> c.LeafValues |> Seq.map (fun lv -> lv.ValueText) |> List.ofSeq)
                                          |> Option.defaultValue []
        let dataTypeName = if node.Has "data_type_name" then Some (node.TagText "data_type_name") else None
        { NewScope.ScopeInput.name = name; NewScope.ScopeInput.aliases = aliases; NewScope.ScopeInput.isSubscopeOf = subscopes; dataTypeName = dataTypeName }


    let private loadScopeDefinitions filename fileString =
        let parsed = CKParser.parseString fileString filename
        match parsed with
        | Failure(e, _, _) -> log (sprintf "scopedefinitions file %s failed with %s" filename e); ([])
        | Success(s,_,_) ->
            let root = simpleProcess.ProcessNode() "root" (mkZeroFile filename) (s)
            root.Child "scopes"
                |> Option.map (fun c -> c.Children |> List.map parseScopeDef)
                |> Option.defaultValue []

    let initializeScopes configFile fallbackScopes =
        let scopes =
            match configFile with
            | Some (filename, fileString) -> loadScopeDefinitions filename fileString
            | None -> []
        let fallbackScopes =
            match scopes, fallbackScopes with
            | [], Some fallbackScopes -> fallbackScopes
            | _ -> scopes
        scopeManager.ReInit(fallbackScopes)

    let private parseModCatDef (node : Node) =
        let name = node.Key
        let internalID = node.TagText "internal_id" |> TryParser.parseInt
        let scopes = node.Child "supported_scopes" |> Option.map (fun c -> c.LeafValues |> Seq.map (fun lv -> lv.ValueText |> scopeManager.ParseScope()) |> List.ofSeq)
                                          |> Option.defaultValue []
        { NewScope.ModifierCategoryInput.name = name; NewScope.ModifierCategoryInput.internalID = internalID; NewScope.ModifierCategoryInput.scopes = scopes }


    let private loadModCatDefinitions filename fileString =
        let parsed = CKParser.parseString fileString filename
        match parsed with
        | Failure(e, _, _) -> log (sprintf "modifiercategorydefinitions file %s failed with %s" filename e); ([])
        | Success(s,_,_) ->
            let root = simpleProcess.ProcessNode() "root" (mkZeroFile filename) (s)
            root.Child "modifier_categories"
                |> Option.map (fun c -> c.Children |> List.map parseModCatDef)
                |> Option.defaultValue []

    let initializeModifierCategories configFile fallbackModifierCategories =
        let modcats =
            match configFile with
            | Some (filename, fileString) -> loadModCatDefinitions filename fileString
            | None -> []
        let fallbackModifierCategories =
            match modcats, fallbackModifierCategories with
            | [], Some fallbackModifierCategories -> fallbackModifierCategories
            | _ -> modcats
        modifierCategoryManager.ReInit(fallbackModifierCategories)
        // fallbackScopes

    let private getLocCommands (node : Node) =
        let simple = node.Values |> List.choose (fun v -> let x = v.Value.ToRawString() in if x == "variable" then None else Some (v.Key, [scopeManager.ParseScope() (v.Value.ToRawString())]))
        let variableCommands = node.Values |> List.choose (fun v -> let x = v.Value.ToRawString() in if x == "variable" then Some v.Key else None)
        let complex = node.Children |> List.map (fun v -> v.Key, (v.LeafValues |> Seq.map (fun lv -> scopeManager.ParseScope() (lv.Value.ToRawString())) |> List.ofSeq))
        (simple @ complex), variableCommands

    let private getLocLinks (node : Node) =
        node.Children
            |> List.choose (fun v ->
                            let t = (v.Child "input_scopes") |> Option.map (fun n -> n.LeafValues |> Seq.map (fun lv -> scopeManager.ParseScope() lv.ValueText) |> List.ofSeq)
                            t |> Option.map (fun inputs ->
                                                v.Key,
                                                inputs,
                                                scopeManager.ParseScope() (v.TagText "output_scope")))
    let loadLocCommands filename fileString =
        let parsed = CKParser.parseString fileString filename
        match parsed with
        |Failure(e, _, _) -> log (sprintf "loccommands file %s failed with %s" filename e); ([], [], [])
        |Success(s,_,_) ->
            let root = simpleProcess.ProcessNode() "root" (mkZeroFile filename) (s)
            let a, b =
                root.Child "localisation_commands"
                |> Option.map getLocCommands
                |> Option.defaultValue ([], [])
            let c =
                root.Child "localisation_links"
                |> Option.map getLocLinks
                |> Option.defaultValue []
            a, b, c

    let loadModifiers filename fileString =
        let parsed = CKParser.parseString fileString filename
        match parsed with
        |Failure(e, _, _) -> log (sprintf "modifier file %s failed with %s" filename e); ([])
        |Success(s,_,_) ->
            let root = simpleProcess.ProcessNode() "root" (mkZeroFile filename) (s)
            root.Child "modifiers"
                |> Option.map (fun ms ->  ms.Values |> List.map(fun l -> {ActualModifier.tag = l.Key; category = modifierCategoryManager.ParseModifier() (l.Value.ToRawString())}))
                |> Option.defaultValue []

    type ListMergeOptimisationDefinition = {
        StartingKey : string
        ConnectingKeys : string list
        TargetKey : string
        SupportedValues : string list
    }

    type FeatureSettings = {
        ListMergeOptimisations : ListMergeOptimisationDefinition list
    }
    with
        static member Default =
            {
                ListMergeOptimisations = []
            }


    let private supportedValues = [ "ROOT"; "FROM"; "FROMFROM"; ]
    let private loadListMergeOptimisation (node : Node) =
        let targetKey = node.Key
        let innerKeys = node.LeafValues |> Seq.map (fun lv -> lv.ValueText) |> List.ofSeq
        match innerKeys with
        | []
        | [_] ->
            log (sprintf "list merge optimisation %s is too short" targetKey)
            None
        | head::tail ->
            Some { StartingKey = head; ConnectingKeys = tail; TargetKey = targetKey; SupportedValues = supportedValues }

    let loadSettingsFile filename fileString =
        let parsed = CKParser.parseString fileString filename
        match parsed with
        |Failure(e, _, _) ->
            log (sprintf "settings file %s failed with %s" filename e);
            None
        |Success(s,_,_) ->
            let root = simpleProcess.ProcessNode() "root" (mkZeroFile filename) (s)
            let listMergeOptimisations =
                root.Child "list_merge_optimisations"
                |> Option.map (fun ms ->  ms.Children |> List.choose loadListMergeOptimisation)
                |> Option.defaultValue []
            Some
                {
                    ListMergeOptimisations = listMergeOptimisations
                }
