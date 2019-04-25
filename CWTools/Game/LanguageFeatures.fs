namespace CWTools.Games
open CWTools.Utilities.Position
open System.IO
open Files
open CWTools.Utilities.Utils
open CWTools.Process
open CWTools.Rules

module LanguageFeatures =
    let makeEntityResourceInput (fileManager : FileManager) filepath filetext  =
        let filepath = Path.GetFullPath(filepath)
        let indexOfScope = filepath.IndexOf(fileManager.ScopeDirectory)
        let rootedpath =
            if indexOfScope = -1
            then filepath
            else filepath.Substring(indexOfScope + (fileManager.ScopeDirectory.Length))
        let logicalpath = fileManager.ConvertPathToLogicalPath rootedpath
        EntityResourceInput {scope = ""; filepath = filepath; logicalpath = logicalpath; filetext = filetext; validate = true}
    let makeFileWithContentResourceInput (fileManager : FileManager) filepath filetext  =
        let filepath = Path.GetFullPath(filepath)
        let indexOfScope = filepath.IndexOf(fileManager.ScopeDirectory)
        let rootedpath =
            if indexOfScope = -1
            then filepath
            else filepath.Substring(indexOfScope + (fileManager.ScopeDirectory.Length))
        let logicalpath = fileManager.ConvertPathToLogicalPath rootedpath
        FileWithContentResourceInput {scope = ""; filepath = filepath; logicalpath = logicalpath; filetext = filetext; validate = true}

    let completion (fileManager : FileManager) (completionService : CompletionService<_> option) (infoService : InfoService<_> option) (resourceManager : ResourceManager<_>) (pos : pos) (filepath : string) (filetext : string) =
        let split = filetext.Split('\n')
        let filetext = split |> Array.mapi (fun i s -> if i = (pos.Line - 1) then log (sprintf "%s" s); let s = s.Insert(pos.Column, "x") in log(sprintf "%s" s); s else s) |> String.concat "\n"
        let resource = makeEntityResourceInput fileManager filepath filetext
        match (resourceManager.ManualProcessResource resource, completionService, infoService) with
        | Some e, Some completion, Some info ->
            log (sprintf "completion %s %s" (fileManager.ConvertPathToLogicalPath filepath) filepath)
            match (info.GetInfo)(pos, e) with
            | Some (ctx, _) ->
                completion.Complete(pos, e, Some ctx)
            | _ ->
                []
        | Some e, Some completion, None ->
            completion.Complete(pos, e, None)
        | _, _, _ -> []


    let getInfoAtPos (fileManager : FileManager) (resourceManager : ResourceManager<_>) (infoService : InfoService<_> option) (lookup : Lookup<_, _>) (pos : pos) (filepath : string) (filetext : string) =
        let resource = makeEntityResourceInput fileManager filepath filetext
        match resourceManager.ManualProcessResource resource, infoService with
        |Some e, Some info ->
            log (sprintf "getInfo %s %s" (fileManager.ConvertPathToLogicalPath filepath) filepath)
            match (info.GetInfo)(pos, e) with
            |Some (_, (_, Some (t, tv), _)) ->
                lookup.typeDefInfo.[t] |> List.tryPick (fun (n, v) -> if n = tv then Some v else None)
            |_ -> None
        |_, _ -> None

    let findAllRefsFromPos (fileManager : FileManager) (resourceManager : ResourceManager<_>) (infoService : InfoService<_> option) (pos : pos) (filepath : string) (filetext : string) =
        let resource = makeEntityResourceInput fileManager filepath filetext
        match resourceManager.ManualProcessResource resource, infoService with
        |Some e, Some info ->
            log (sprintf "findRefs %s %s" (fileManager.ConvertPathToLogicalPath filepath) filepath)
            match (info.GetInfo)(pos, e) with
            |Some (_,( _, Some ((t : string), tv), _)) ->
                //log "tv %A %A" t tv
                let t = t.Split('.').[0]
                resourceManager.Api.ValidatableEntities() |> List.choose (fun struct(e, l) -> let x = l.Force().Referencedtypes in if x.IsSome then (x.Value.TryFind t) else ((info.GetReferencedTypes) e).TryFind t)
                               |> List.collect id
                               |> List.choose (fun (tvk, r) -> if tvk == tv then Some r else None)
                               |> Some
            |_ -> None
        |_, _ -> None

    let scopesAtPos (fileManager : FileManager) (resourceManager : ResourceManager<_>) (infoService : InfoService<_> option) (anyScope : 'a) (pos : pos) (filepath : string) (filetext : string) =
        let resource = makeEntityResourceInput fileManager filepath filetext
        match resourceManager.ManualProcessResource resource, infoService with
        |Some e, Some info ->
            // match info.GetInfo(pos, e) with
            match (info.GetInfo)(pos, e) with
            |Some (ctx, _) when ctx <> { Root = anyScope; From = []; Scopes = [] } ->
                Some (ctx)
            |_ ->
                None
        |_ -> None


    let symbolInformationAtPos (fileManager : FileManager) (resourceManager : ResourceManager<_>) (infoService : InfoService<_> option) (lookup : Lookup<_, _>) (pos : pos) (filepath : string) (filetext : string) : SymbolInformation option =
        let resource = makeEntityResourceInput fileManager filepath filetext
        match resourceManager.ManualProcessResource resource, infoService with
        |Some e, Some info ->
            log (sprintf "symbolInfoAtPos %s %s" (fileManager.ConvertPathToLogicalPath filepath) filepath)
            let (ruleOptions, typeInfo, nodeAtPos) =
                (info.GetInfo)(pos, e)
                |> Option.map snd
                |> Option.map (fun (rd, ti, child) -> child |> function | Some (NodeC node) -> rd, ti, Some node | _ -> rd, ti, None)
                |> Option.defaultValue (None, None, None)
            let (tv, t, locs) =
                match typeInfo with
                | Some (t, tv) ->
                    let splitType = t.Split '.'
                    let typename = splitType.[0]
                    let subtype = if splitType.Length > 1 then splitType.[1] else ""
                    match lookup.typeDefs |> List.tryFind (fun td -> td.name = typename) with
                    | Some td ->
                        let locs = td.localisation @ (td.subtypes |> List.collect (fun st -> if st.name = subtype then st.localisation else []))
                        match nodeAtPos with
                        | Some node ->
                           tv, tv,
                           locs |> List.map (fun l ->
                                match l.explicitField with
                                | None -> { key = l.name; value = (l.prefix + tv + l.suffix) }
                                | Some field -> { key = l.name; value = node.TagText field})
                        | None ->
                            tv, t, locs |> List.choose (fun l -> if l.explicitField.IsNone then Some { key = l.name; value = (l.prefix + tv + l.suffix) } else None)
                    | None -> "", "", []
                | None -> "", "", []
            Some {
                name = tv
                typename = t
                localisation = locs
                ruleDescription = ruleOptions |> Option.bind (fun ro -> ro.description)
                ruleRequiredScopes = ruleOptions |> Option.map (fun ro -> ro.requiredScopes |> List.map (fun s -> s.ToString())) |> Option.defaultValue []
            }
            // match (info.GetInfo)(pos, e) with
            // |Some (_, (ruleDesc, Some (t, tv), (None))) ->
            //     let splitType = t.Split '.'
            //     let typename = splitType.[0]
            //     let subtype = if splitType.Length > 1 then splitType.[1] else ""
            //     match lookup.typeDefs |> List.tryFind (fun td -> td.name = typename) with
            //     |Some td ->
            //         let locs = td.localisation @ (td.subtypes |> List.collect (fun st -> if st.name = subtype then st.localisation else []))
            //                 |> List.choose (fun l -> if l.explicitField.IsNone then Some { key = l.name; value = (l.prefix + tv + l.suffix) } else None)
            //         Some {
            //                 name = tv
            //                 typename = t
            //                 localisation = locs
            //                 ruleDescription = ruleDesc
            //             }
            //     |None -> None
            // |Some (_, (ruleDesc, Some (t, tv), (Some (NodeC node)))) ->
            //     let splitType = t.Split '.'
            //     let typename = splitType.[0]
            //     let subtype = if splitType.Length > 1 then splitType.[1] else ""
            //     match lookup.typeDefs |> List.tryFind (fun td -> td.name = typename) with
            //     |Some td ->
            //         let locs = td.localisation @ (td.subtypes |> List.collect (fun st -> if st.name = subtype then st.localisation else []))
            //                 |> List.map (fun l ->
            //                     match l.explicitField with
            //                     | None -> { key = l.name; value = (l.prefix + tv + l.suffix) }
            //                     | Some field -> { key = l.name; value = node.TagText field})
            //         Some {
            //                 name = tv
            //                 typename = t
            //                 localisation = locs
            //                 ruleDescription = ruleDesc
            //             }
            //     |None -> None
            // | Some (_, (ruleDesc, None, None)) ->
            //     Some {
            //         name = ""
            //         typename = ""
            //         localisation = []
            //         ruleDescription = ruleDesc
            //     }
            // |_ -> None
        |_, _ -> None

