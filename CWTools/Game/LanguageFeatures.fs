namespace CWTools.Games

open CWTools.Utilities.Position
open System.IO
open Files
open CWTools.Utilities.Utils
open CWTools.Process
open CWTools.Rules
open CWTools.Common

module LanguageFeatures =

    let getNodeForTypeDefAndType
        (resourceManager: ResourceManager<_>)
        (lookup: Lookup)
        (typedef: TypeDefinition)
        (typename: string)
        =
        let findNode (pos: range) (startNode: Node) =
            let rec findChild (node: Node) =
                if node.Position.Equals pos then
                    Some node
                else
                    match node.Nodes |> Seq.tryFind (fun n -> rangeContainsRange n.Position pos) with
                    | Some c -> findChild c
                    | None -> None

            findChild startNode

        lookup.typeDefInfo.TryFind typedef.name
        |> Option.bind (Array.tryFind (fun tdi -> tdi.id = typename))
        |> Option.bind (fun typeDefInfoForTypeName ->
            resourceManager.Api.GetEntityByFilePath typeDefInfoForTypeName.range.FileName
            |> Option.bind (fun struct (e, _) -> findNode typeDefInfoForTypeName.range e.entity))

    let makeEntityResourceInput (fileManager: FileManager) filepath filetext =
        let filepath = Path.GetFullPath(filepath)
        let logicalpath = fileManager.ConvertPathToLogicalPath filepath
        let scope = fileManager.GetScopeForPath filepath |> Option.defaultValue "unknown"

        EntityResourceInput
            { scope = scope
              filepath = filepath
              logicalpath = logicalpath
              filetext = filetext
              validate = true }

    let makeFileWithContentResourceInput (fileManager: FileManager) filepath filetext =
        let filepath = Path.GetFullPath(filepath)
        let logicalpath = fileManager.ConvertPathToLogicalPath filepath
        let scope = fileManager.GetScopeForPath filepath |> Option.defaultValue "unknown"

        FileWithContentResourceInput
            { scope = scope
              filepath = filepath
              logicalpath = logicalpath
              filetext = filetext
              validate = true }


    let completion
        (fileManager: FileManager)
        (completionService: CompletionService option)
        (infoService: InfoService option)
        (resourceManager: ResourceManager<_>)
        (pos: pos)
        (filepath: string)
        (filetext: string)
        =
        let split = filetext.Split('\n')

        let filetext =
            split
            |> Array.mapi (fun i s ->
                if i = (pos.Line - 1) then
                    log (sprintf "%s" s)
                    let s = s.Insert(pos.Column, magicCharString) in
                    log (sprintf "%s" s)
                    s
                else
                    s)
            |> String.concat "\n"

        let resource = makeEntityResourceInput fileManager filepath filetext

        match
            Path.GetExtension filepath, resourceManager.ManualProcessResource resource, completionService, infoService
        with
        | ".yml", _, Some completion, _ -> completion.LocalisationComplete(pos, filetext) |> List.ofArray
        | _, Some e, Some completion, Some info ->
            log (sprintf "completion %s %s" (fileManager.ConvertPathToLogicalPath filepath) filepath)

            match info.GetInfo(pos, e) with
            | Some(ctx, _) -> completion.Complete(pos, e, Some ctx)
            | None -> completion.Complete(pos, e, None)
        | _, Some e, Some completion, None -> completion.Complete(pos, e, None)
        | _, _, _, _ -> []


    let getInfoAtPos
        (fileManager: FileManager)
        (resourceManager: ResourceManager<_>)
        (infoService: InfoService option)
        (localisationManager: LocalisationManager<_>)
        (lookup: Lookup)
        (lang: Lang)
        (pos: pos)
        (filepath: string)
        (filetext: string)
        =
        let resource = makeEntityResourceInput fileManager filepath filetext

        match resourceManager.ManualProcessResource resource, infoService with
        | Some e, Some info ->
            logDiag (sprintf "getInfo %s %s" (fileManager.ConvertPathToLogicalPath filepath) filepath)

            match info.GetInfo(pos, e) with
            | Some(_, (_, Some(LocRef(tv)), _)) ->
                match
                    localisationManager.LocalisationEntries()
                    |> List.tryFind (fun (l, _) -> l = lang)
                with
                | Some(_, entries) ->
                    match entries |> List.tryFind (fun struct (k, _) -> k = tv) with
                    | Some(_, entry) -> Some entry.position
                    | _ -> None
                | None -> None
            | Some(_, (_, Some(TypeRef(t, tv)), _)) ->
                lookup.typeDefInfo
                |> Map.tryFind t
                |> Option.defaultValue [||]
                |> Array.tryPick (fun tdi -> if tdi.id = tv then Some tdi.range else None)
            | Some(_, (_, Some(EnumRef(enumName, enumValue)), _)) ->
                let enumValues = lookup.enumDefs[enumName] |> snd
                enumValues |> Array.tryPick (fun (ev, r) -> if ev == enumValue then r else None)
            | Some(_, (_, Some(FileRef f), _)) ->
                resourceManager.Api.AllEntities()
                |> Seq.map structFst
                |> Seq.tryFind (fun x -> x.logicalpath = f)
                |> Option.map (fun x -> mkRange x.filepath pos pos)
            | _ -> None
        | _, _ -> None

    let findAllRefsFromPos
        (fileManager: FileManager)
        (resourceManager: ResourceManager<_>)
        (infoService: InfoService option)
        (pos: pos)
        (filepath: string)
        (filetext: string)
        =
        let resource = makeEntityResourceInput fileManager filepath filetext

        match resourceManager.ManualProcessResource resource, infoService with
        | Some e, Some info ->
            log (sprintf "findRefs %s %s" (fileManager.ConvertPathToLogicalPath filepath) filepath)

            match info.GetInfo(pos, e) with
            | Some(_, (_, Some(TypeRef(t: string, tv)), _)) ->
                //log "tv %A %A" t tv
                let t = t.Split('.').[0]

                resourceManager.Api.ValidatableEntities()
                |> List.choose (fun struct (e, l) ->
                    let x = l.Force().Referencedtypes in

                    if x.IsSome then
                        (x.Value.TryFind t)
                    else
                        let contains, value = (info.GetReferencedTypes e).TryGetValue t in
                        if contains then Some(value |> List.ofSeq) else None)
                |> List.collect id
                |> List.choose (fun ref ->
                    if ref.name.GetString() == tv && ref.referenceType = ReferenceType.TypeDef then
                        Some ref.position
                    else
                        None)
                |> Some
            | _ -> None
        | _, _ -> None



    let scopesAtPos
        (fileManager: FileManager)
        (resourceManager: ResourceManager<_>)
        (infoService: InfoService option)
        (anyScope: Scope)
        (pos: pos)
        (filepath: string)
        (filetext: string)
        =
        let resource = makeEntityResourceInput fileManager filepath filetext

        match resourceManager.ManualProcessResource resource, infoService with
        | Some e, Some info ->
            // match info.GetInfo(pos, e) with
            match info.GetInfo(pos, e) with
            | Some(ctx, _) when
                ctx
                <> { Root = anyScope
                     From = []
                     Scopes = [] }
                ->
                Some ctx
            | _ -> None
        | _ -> None


    let symbolInformationAtPos
        (fileManager: FileManager)
        (resourceManager: ResourceManager<_>)
        (infoService: InfoService option)
        (lookup: Lookup)
        (pos: pos)
        (filepath: string)
        (filetext: string)
        : SymbolInformation option =
        let resource = makeEntityResourceInput fileManager filepath filetext

        match resourceManager.ManualProcessResource resource, infoService with
        | Some e, Some info ->
            log (sprintf "symbolInfoAtPos %s %s" (fileManager.ConvertPathToLogicalPath filepath) filepath)

            let ruleOptions, typeInfo, nodeAtPos =
                info.GetInfo(pos, e)
                |> Option.map snd
                |> Option.map (fun (rd, ti, child) ->
                    child
                    |> function
                        | Some(NodeC node) -> rd, ti, Some node
                        | _ -> rd, ti, None)
                |> Option.defaultValue (None, None, None)

            let tv, t, locs =
                match typeInfo with
                | Some(TypeRef(t, tv)) ->
                    let splitType = t.Split '.'
                    let typename = splitType.[0]
                    let subtype = if splitType.Length > 1 then splitType.[1] else ""

                    match lookup.typeDefs |> List.tryFind (fun td -> td.name = typename) with
                    | Some td ->
                        let locs =
                            td.localisation
                            @ (td.subtypes
                               |> List.collect (fun st -> if st.name = subtype then st.localisation else []))

                        let typeDefNode = getNodeForTypeDefAndType resourceManager lookup td tv

                        match typeDefNode |> Option.orElse nodeAtPos with
                        | Some node ->
                            tv,
                            tv,
                            locs
                            |> List.map (fun l ->
                                match l.explicitField with
                                | None ->
                                    { key = l.name
                                      value = (l.prefix + tv + l.suffix) }
                                | Some field ->
                                    { key = l.name
                                      value = node.TagText field })
                        | None ->
                            tv,
                            t,
                            locs
                            |> List.choose (fun l ->
                                if l.explicitField.IsNone then
                                    Some
                                        { key = l.name
                                          value = (l.prefix + tv + l.suffix) }
                                else
                                    None)
                    | None -> "", "", []
                | None -> "", "", []
                | _ -> "", "", []

            Some
                { name = tv
                  typename = t
                  localisation = locs
                  ruleDescription = ruleOptions |> Option.bind (fun ro -> ro.description)
                  ruleRequiredScopes =
                    ruleOptions
                    |> Option.map (fun ro -> ro.requiredScopes |> List.map (fun s -> s.ToString()))
                    |> Option.defaultValue [] }
        | _, _ -> None


    let getPreTriggerPossible
        (fileManager: FileManager)
        (resourceManager: ResourceManager<_>)
        (filepath: string)
        (filetext: string)
        =
        let resource = makeEntityResourceInput fileManager filepath filetext

        match resourceManager.ManualProcessResource resource with
        | Some e ->
            if e.logicalpath.StartsWith "events" then
                let findEvents (event: Node) =
                    match event.Key == "planet_event", event.Child "trigger" with
                    | _, None -> None
                    | false, _ -> None
                    | true, Some trigger ->
                        if
                            Array.exists
                                trigger.Has
                                CWTools.Validation.Stellaris.STLValidation.stellarisEventPreTriggers
                        then
                            Some event.Position
                        else
                            None

                e.entity.Children |> List.choose findEvents
            else if e.logicalpath.StartsWith "common/pop_jobs" then
                let findPopJobs (popjob: Node) =
                    match popjob.Child "possible" with
                    | None -> None
                    | Some possible ->
                        if
                            Array.exists
                                possible.Has
                                CWTools.Validation.Stellaris.STLValidation.stellarisPopJobPreTriggers
                        then
                            Some popjob.Position
                        else
                            None

                e.entity.Children |> List.choose findPopJobs
            else
                []
        | None -> []

    let getFastTrigger
        (fileManager: FileManager)
        (resourceManager: ResourceManager<_>)
        (filepath: string)
        (filetext: string)
        =
        let resource = makeEntityResourceInput fileManager filepath filetext

        let extractPreTrigger (node: Node) (key: string) =
            if node.Has key then
                let leaf = node.Leafs key |> Seq.head
                let text = leaf.Key + " = " + leaf.ValueText
                let range = leaf.Position
                Some(range, text)
            else
                None

        let pretriggerBlockForEvent
            (targetblock: string)
            (sourceblock: string)
            (event: Node)
            (pretriggers: string seq)
            =
            let ptTemplate (tabs: string) (pt: string) = sprintf "\t%s\n%s" pt tabs

            let createAllPts (tabs: string) =
                pretriggers |> Seq.map (ptTemplate tabs) |> String.concat ""

            if event.Has targetblock then
                let endPos = event.Childs targetblock |> Seq.head |> (fun c -> c.Position.End)
                let tabCount = endPos.Column - 1
                let tabString = String.replicate tabCount "\t"
                let ptText = createAllPts tabString
                let ptInsert = mkPos endPos.Line (endPos.Column - 1), ptText
                ptInsert
            else
                let startTriggerPos =
                    event.Childs sourceblock |> Seq.head |> (fun c -> c.Position.Start)

                let tabCount = startTriggerPos.Column
                let tabString = String.replicate tabCount "\t"
                let ptText = createAllPts tabString
                let ptBlock = sprintf "%s = {\n%s%s}\n%s" targetblock tabString ptText tabString
                let ptInsert = startTriggerPos, ptBlock
                ptInsert

        match resourceManager.ManualProcessResource resource with
        | Some e ->
            if e.logicalpath.StartsWith "events" then
                let eventToChanges (event: Node) =
                    match event.Key == "planet_event", event.Child "trigger" with
                    | true, Some trigger ->
                        let triggers =
                            CWTools.Validation.Stellaris.STLValidation.stellarisEventPreTriggers
                            |> Seq.choose (extractPreTrigger trigger)
                            |> List.ofSeq

                        match triggers with
                        | [] -> None
                        | triggers ->
                            let insertPos, insertText =
                                pretriggerBlockForEvent "pre_triggers" "trigger" event (triggers |> Seq.map snd)

                            let deletes = triggers |> Seq.map fst
                            Some(deletes, insertPos, insertText)
                    | _, _ -> None

                Some(e.entity.Children |> List.choose eventToChanges)
            else if e.logicalpath.StartsWith "common/pop_jobs" then
                let eventToChanges (event: Node) =
                    match event.Child "possible" with
                    | Some trigger ->
                        let triggers =
                            CWTools.Validation.Stellaris.STLValidation.stellarisPopJobPreTriggers
                            |> Seq.choose (extractPreTrigger trigger)
                            |> List.ofSeq

                        match triggers with
                        | [] -> None
                        | triggers ->
                            let insertPos, insertText =
                                pretriggerBlockForEvent
                                    "possible_pre_triggers"
                                    "possible"
                                    event
                                    (triggers |> Seq.map snd)

                            let deletes = triggers |> Seq.map fst
                            Some(deletes, insertPos, insertText)
                    | _ -> None

                Some(e.entity.Children |> List.choose eventToChanges)
            else
                None
        | _ -> None

    // type CachedRuleMetadata = {
    //     typeDefs : Map<string,list<TypeDefInfo>>
    //     enumDefs : Map<string,string * list<string>>
    //     varDefs : Map<string,list<string * range>>
    //     loc : (Lang * Set<string>) list
    //     files : Set<string>
    // }
    let getEmbeddedMetadata (lookup: Lookup) (localisation: LocalisationManager<_>) (resources: ResourceManager<_>) =
        { typeDefs = lookup.typeDefInfo
          enumDefs = lookup.enumDefs |> Map.map (fun _ (s, v) -> s, v |> Array.map fst)
          varDefs = lookup.varDefInfo
          loc = localisation.LocalisationKeys()
          files = resources.Api.GetFileNames() |> Set.ofArray
          scriptedLoc = lookup.scriptedLoc }


    let graphEventDataForFiles
        (referenceManager: References<_>)
        (resourceManager: ResourceManager<_>)
        (lookup: Lookup)
        (files: string list)
        (sourceType: string)
        (depth: int)
        : GraphDataItem list =
        let sourceTypes =
            lookup.typeDefs
            |> List.tryPick (fun td ->
                if td.name = sourceType then
                    Some(sourceType :: td.graphRelatedTypes)
                else
                    None)
            |> Option.defaultValue [ sourceType ]

        let entitiesInSelectedFiles =
            resourceManager.Api.AllEntities()
            |> List.filter (fun struct (e, _) -> files |> List.contains e.filepath)

        let locSet = referenceManager.Localisation |> Map.ofList

        let findLoc key =
            locSet |> Map.tryFind key |> Option.map (fun l -> l.desc)

        let getSourceTypesTD (x: Map<string, TypeDefInfo array>) =
            Map.toList x
            |> List.filter (fun (k, _) -> sourceTypes |> List.contains k)
            |> List.collect (fun (k, vs) ->
                vs
                |> Array.map (fun tdi -> k, tdi.id, tdi.range, tdi.explicitLocalisation, tdi.subtypes)
                |> List.ofSeq)

        let getSourceTypes (x: Map<string, ReferenceDetails list>) =
            Map.toList x
            |> List.filter (fun (k, _) -> sourceTypes |> List.contains k)
            |> List.collect (fun (k, vs) ->
                vs
                |> List.map (fun rd -> k, rd.name, rd.position, rd.isOutgoing, rd.referenceLabel, []))

        let getNonExplicitLoc (id: string) (typeName: string) =
            lookup.typeDefs
            |> List.tryPick (fun td -> if td.name = typeName then Some td.localisation else None)
            |> Option.bind (fun ls -> ls |> List.tryFind (fun l -> l.explicitField.IsNone && l.primary))
            |> Option.bind (fun l -> l.prefix + id + l.suffix |> findLoc)

        let getDisplayNameFromID (id: string) (el: (string * string * bool) list) (entityType: string) =
            el
            |> List.tryFind (fun (_, _, primary) -> primary)
            |> Option.bind (fun (_, key, _) -> findLoc key)
            |> Option.orElseWith (fun _ -> getNonExplicitLoc id entityType)
            |> Option.orElseWith (fun _ -> findLoc id)

        let getAbbrevInfo (typeName: string) (subtypes: string list) =
            lookup.typeDefs
            |> List.tryPick (fun td -> if td.name = typeName then Some td.subtypes else None)
            |> Option.defaultValue []
            |> List.filter (fun st -> st.abbreviation.IsSome || st.displayName.IsSome)
            |> List.tryFind (fun st -> subtypes |> List.contains st.name)


        match lookup.typeDefInfo |> getSourceTypesTD with
        | [] -> []
        | t ->
            let typesDefinedInFiles =
                t |> List.filter (fun (_, _, r, el, _) -> files |> List.contains r.FileName)

            let allInternalOrOutgoingRefs =
                entitiesInSelectedFiles
                |> List.collect (fun struct (_, data) ->
                    data.Force().Referencedtypes
                    |> Option.map (
                        getSourceTypes
                        >> List.map (fun (_, v, r, isOutgoing, refLabel, _) -> (v, r, isOutgoing, refLabel))
                    )
                    |> Option.defaultValue [])

            let definedVariables =
                entitiesInSelectedFiles
                |> List.collect (fun struct (_, data) ->
                    data.Force().Definedvariables
                    |> Option.map (
                        Map.toList
                        >> List.collect (fun (k, vs) -> vs |> List.ofSeq |> List.map (fun (v1, v2) -> k, v1, v2))
                    )
                    |> Option.defaultValue [])

            let results =
                typesDefinedInFiles
                |> List.map (fun (entityType, event, r, el, sts) ->
                    entityType,
                    event,
                    r,
                    (allInternalOrOutgoingRefs
                     |> List.choose (fun (reference, r2, isOutgoing, refLabel) ->
                         if rangeContainsRange r r2 then
                             Some(reference.GetString(), isOutgoing, refLabel)
                         else
                             None)
                     |> List.distinct),
                    (definedVariables
                     |> List.choose (fun (varname, defVar, r2) ->
                         if rangeContainsRange r r2 then
                             Some(varname, defVar)
                         else
                             None)
                     |> List.distinct),
                    el,
                    sts)

            let primaries =
                results
                |> List.map (fun (entityType, event, r, refs, defvar, el, sts) ->
                    let subtypeName, abbrev =
                        getAbbrevInfo entityType sts
                        |> Option.map (fun st -> st.displayName, st.abbreviation)
                        |> Option.defaultValue (None, None)

                    { GraphDataItem.id = event
                      displayName = getDisplayNameFromID event el entityType
                      documentation = None
                      references = refs
                      location = Some r
                      details =
                        Some(
                            defvar
                            |> List.groupBy fst
                            |> List.map (fun (k, vs) -> k, vs |> List.map snd)
                            |> Map.ofList
                        )
                      isPrimary = true
                      entityType = entityType
                      entityTypeDisplayName = subtypeName
                      abbreviation = abbrev })

            let allInternalOrOutgoingRefNames =
                allInternalOrOutgoingRefs
                |> List.map (fun (r, _, _, _) -> r.GetString())
                |> List.distinct

            let allTypeNamesInFiles = typesDefinedInFiles |> List.map (fun (k, n, r, _, _) -> n)

            let allOutgoingRefs =
                t
                |> List.filter (fun (k, name, _, _, _) ->
                    allInternalOrOutgoingRefNames |> List.contains name
                    && (allTypeNamesInFiles |> List.contains name |> not))
                |> List.distinctBy (fun (_, name, _, _, _) -> name)

            let secondaryNames = allOutgoingRefs |> List.map (fun (k, n, r, _, _) -> n)
            // let allOtherFiles = t |> List.filter (fun (_, id, range, _, _) -> files |> List.contains range.FileName |> not)
            //                              |> List.map (fun (_, _, range, _, _) -> range.FileName)
            // let get
            let getNewIncomingRefs (excludedFiles: string list) (targetTypeNames: string list) =
                let allOtherFiles =
                    t
                    |> List.filter (fun (_, id, range, _, _) -> excludedFiles |> List.contains range.FileName |> not)
                    |> List.map (fun (_, _, range, _, _) -> range.FileName)

                let newIncomingRefs =
                    resourceManager.Api.AllEntities()
                    |> List.filter (fun struct (e, _) -> allOtherFiles |> List.contains e.filepath)
                    |> List.collect (fun struct (_, data) ->
                        data.Force().Referencedtypes
                        |> Option.map (
                            getSourceTypes
                            >> List.map (fun (_, v, r, isOutgoing, refLabel, _) ->
                                (v.GetString(), r, isOutgoing, refLabel))
                        )
                        |> Option.defaultValue [])
                    |> List.filter (fun (v, r, isOutgoing, refLabel) -> targetTypeNames |> List.contains v)

                newIncomingRefs

            let getTypesFromRefs (previouslyScannedTypes: string list) refs =
                t
                |> List.filter (fun (_, name, r, el, _) ->
                    (previouslyScannedTypes |> List.contains name |> not)
                    && (secondaryNames |> List.contains name |> not))
                |> List.filter (fun (_, _, r, _, _) ->
                    refs
                    |> List.exists (fun (_, r2, isOutgoing, refLabel) -> rangeContainsRange r r2))
            // let allIncomingRefs = getNewIncomingRefs files allTypeNamesInFiles
            // let typesNotDefinedInFiles = getTypesFromRefs allTypeNamesInFiles allIncomingRefs
            // let typesNotDefinedInFilesNames = typesNotDefinedInFiles |> List.map (fun (k, n, r, _, _) -> n)
            // let allIncomingMinusOneRefs = getNewIncomingRefs files typesNotDefinedInFilesNames
            // let typesNotDefinedInFilesMinusOne = getTypesFromRefs (allTypeNamesInFiles @ typesNotDefinedInFilesNames) allIncomingMinusOneRefs
            // let typesNotDefinedInFilesMinusOneNames = typesNotDefinedInFilesMinusOne |> List.map (fun (k, n, r, _, _) -> n)
            // let allIncomingMinusTwoRefs = getNewIncomingRefs files typesNotDefinedInFilesMinusOneNames
            // let typesNotDefinedInFilesMinusTwo = getTypesFromRefs (allTypeNamesInFiles @ typesNotDefinedInFilesNames @ typesNotDefinedInFilesMinusOneNames) allIncomingMinusTwoRefs
            let searchBackwards typesToSearchBackFrom allPreviousTypes =
                let typesToSearchBackFromNames =
                    typesToSearchBackFrom |> List.map (fun (k, n, r, _, _) -> n)

                let allIncoming = getNewIncomingRefs files typesToSearchBackFromNames
                let allNewTypes = getTypesFromRefs allPreviousTypes allIncoming
                allIncoming, allNewTypes, typesToSearchBackFromNames
            // let directBackRefs, directBackTypes, directNowSearched = searchBackwards typesNotDefinedInFiles allTypeNamesInFiles
            // let minusOneBackRefs, minusOneBackTypes, minusOneNowSearched = searchBackwards directBackTypes (directNowSearched @ allTypeNamesInFiles)
            let folder (allRefs, backTypes, backSearched, allBackTypes) =
                let nextBackRefs, nextBackTypes, nextNowSearched =
                    searchBackwards backTypes backSearched

                allRefs @ nextBackRefs, nextBackTypes, nextNowSearched @ backSearched, nextBackTypes @ allBackTypes

            let allIncomingRefs, _, _, allIncomingTypes =
                repeatN folder depth ([], typesDefinedInFiles, allTypeNamesInFiles, [])
            // let allIncomingRefs = resourceManager.Api.AllEntities() |> List.filter (fun struct(e, _) -> allOtherFiles |> List.contains e.filepath)
            //                         |> List.collect (fun struct(_, data) -> data.Force().Referencedtypes |> Option.map (getSourceTypes >> List.map (fun (_, v, r, isOutgoing, refLabel, _) -> (v, r, isOutgoing, refLabel))) |> Option.defaultValue [])
            //                         |> List.filter (fun (v, r, isOutgoing, refLabel) -> allTypeNamesInFiles |> List.contains v)
            // let typesNotDefinedInFiles = t |> List.filter (fun (_, name, r, el, _) -> (allTypeNamesInFiles |> List.contains name |> not) && (secondaryNames |> List.contains name |> not))
            //                                |> List.filter (fun (_, _, r, _, _) -> allIncomingRefs |> List.exists (fun (_, r2, isOutgoing, refLabel) -> rangeContainsRange r r2))
            // let allRefs = allIncomingRefs @ allIncomingMinusOneRefs @ allIncomingMinusTwoRefs
            let secondaries =
                allOutgoingRefs @ allIncomingTypes
                |> List.map (fun (entityType, name, range, el, sts) ->
                    let subtypeName, abbrev =
                        getAbbrevInfo entityType sts
                        |> Option.map (fun st -> st.displayName, st.abbreviation)
                        |> Option.defaultValue (None, None)

                    { GraphDataItem.id = name
                      displayName = getDisplayNameFromID name el entityType
                      documentation = None
                      references =
                        (allIncomingRefs
                         |> List.choose (fun (reference, r2, isOutgoing, refLabel) ->
                             if rangeContainsRange range r2 then
                                 Some(reference, isOutgoing, refLabel)
                             else
                                 None)
                         |> List.distinct)
                      location = Some range
                      details = None
                      isPrimary = false
                      entityType = entityType
                      entityTypeDisplayName = subtypeName |> Option.orElse (Some entityType)
                      abbreviation = abbrev })

            primaries @ secondaries
