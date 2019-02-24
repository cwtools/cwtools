namespace CWTools.Games
open CWTools.Common
open CWTools.Utilities.Position
open System.IO
open Files
open CWTools.Validation.Rules
open CWTools.Utilities.Utils
open CWTools.Validation.Stellaris.ScopeValidation

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

    let completion (fileManager : FileManager) (completionService : CompletionService<_> option) (infoService : FoldRules<_> option) (resourceManager : ResourceManager<_>) (pos : pos) (filepath : string) (filetext : string) =
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


    let getInfoAtPos (fileManager : FileManager) (resourceManager : ResourceManager<_>) (infoService : FoldRules<_> option) (lookup : Lookup<_, _>) (pos : pos) (filepath : string) (filetext : string) =
        let resource = makeEntityResourceInput fileManager filepath filetext
        match resourceManager.ManualProcessResource resource, infoService with
        |Some e, Some info ->
            log (sprintf "getInfo %s %s" (fileManager.ConvertPathToLogicalPath filepath) filepath)
            match (info.GetInfo)(pos, e) with
            |Some (_, Some (t, tv)) ->
                lookup.typeDefInfo.[t] |> List.tryPick (fun (n, v) -> if n = tv then Some v else None)
            |_ -> None
        |_, _ -> None

    let findAllRefsFromPos (fileManager : FileManager) (resourceManager : ResourceManager<_>) (infoService : FoldRules<_> option) (pos : pos) (filepath : string) (filetext : string) =
        let resource = makeEntityResourceInput fileManager filepath filetext
        match resourceManager.ManualProcessResource resource, infoService with
        |Some e, Some info ->
            log (sprintf "findRefs %s %s" (fileManager.ConvertPathToLogicalPath filepath) filepath)
            match (info.GetInfo)(pos, e) with
            |Some (_, Some ((t : string), tv)) ->
                //log "tv %A %A" t tv
                let t = t.Split('.').[0]
                resourceManager.Api.ValidatableEntities() |> List.choose (fun struct(e, l) -> let x = l.Force().Referencedtypes in if x.IsSome then (x.Value.TryFind t) else ((info.GetReferencedTypes) e).TryFind t)
                               |> List.collect id
                               |> List.choose (fun (tvk, r) -> if tvk == tv then Some r else None)
                               |> Some
            |_ -> None
        |_, _ -> None

    let scopesAtPos (fileManager : FileManager) (resourceManager : ResourceManager<_>) (infoService : FoldRules<_> option) (anyScope : 'a) (pos : pos) (filepath : string) (filetext : string) =
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


    let symbolInformationAtPos (fileManager : FileManager) (resourceManager : ResourceManager<_>) (infoService : FoldRules<_> option) (lookup : Lookup<_, _>) (pos : pos) (filepath : string) (filetext : string) : SymbolInformation option =
        let resource = makeEntityResourceInput fileManager filepath filetext
        match resourceManager.ManualProcessResource resource, infoService with
        |Some e, Some info ->
            log (sprintf "symbolInfoAtPos %s %s" (fileManager.ConvertPathToLogicalPath filepath) filepath)
            match (info.GetInfo)(pos, e) with
            |Some (_, Some (t, tv)) ->
                match lookup.typeDefs |> List.tryFind (fun td -> td.name = t) with
                |Some td ->
                    let locs = td.localisation |> List.map (fun l -> { key = l.name; value = (l.prefix + tv + l.suffix) })
                    Some {
                            name = tv
                            typename = t
                            localisation = locs
                        }
                |None -> None
            |_ -> None
        |_, _ -> None
