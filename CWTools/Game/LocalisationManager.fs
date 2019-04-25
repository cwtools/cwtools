namespace CWTools.Games
open CWTools.Common
open CWTools.Localisation
open CWTools.Utilities.Utils
open CWTools.Process.Scopes


type LocalisationManager<'S, 'T, 'M when 'S : comparison and 'S :> IScope<'S> and 'T :> ComputedData and 'M :> IModifier>
    (resources : IResourceAPI<'T>, localisationService : _ -> ILocalisationAPICreator, langs : Lang list, lookup: Lookup<_,_>,
     processLocalisation : (Lookup<'S,'M> -> Lang * Map<string,Entry>-> Lang * Map<string,LocEntry<_>>),
     localisationExtension : string) as this =
    let mutable localisationAPIMap : Map<string * Lang, (bool * ILocalisationAPI)> = Map.empty
    let allLocalisation() = this.LocalisationAPIs() |> List.map snd
    let validatableLocalisation() = this.LocalisationAPIs() |> List.choose (fun (validate, api) -> if validate then Some api else None)
    let parseLocFile (locFile : FileWithContentResource) =
        if locFile.overwrite <> Overwritten && locFile.extension = localisationExtension
        then
            let locService = [locFile.filepath, locFile.filetext] |> localisationService
            Some (langs |> List.map (fun lang -> (locFile.filepath, lang), (locFile.validate, locService.Api(lang))))
        else None
    let updateAllLocalisationSources() =
        localisationAPIMap <-
            let allLocs = resources.GetResources()
                        |> List.choose (function |FileWithContentResource (_, e) -> Some e |_ -> None)
                        |> List.choose parseLocFile
                        |> List.collect id
            allLocs |> Map.ofList
        this.localisationKeys <-allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |>List.collect (fun ls -> ls.GetKeys) |> Set.ofList )
        this.taggedLocalisationKeys <- allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.GetKeys) |> List.fold (fun (s : LocKeySet) v -> s.Add v) (LocKeySet.Empty(InsensitiveStringComparer())) )
    let updateLocalisationSource (locFile : FileWithContentResource) =
        let loc = parseLocFile locFile |> Option.defaultValue []
        let newMap = loc |> List.fold (fun map (key, value) -> Map.add key value map) localisationAPIMap
        localisationAPIMap <- newMap
        this.localisationKeys <-allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |>List.collect (fun ls -> ls.GetKeys) |> Set.ofList )
        this.taggedLocalisationKeys <- allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.GetKeys) |> List.fold (fun (s : LocKeySet) v -> s.Add v) (LocKeySet.Empty(InsensitiveStringComparer())) )
    let updateProcessedLocalisation() =
        let validatableEntries = validatableLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList) |> Map.ofList)
        let processLoc = processLocalisation lookup
        lookup.proccessedLoc <- validatableEntries |> List.map processLoc

    // member val localisationAPIs : (bool * ILocalisationAPI) list = [] with get, set
    member val localisationErrors : CWError list option = None with get, set
    member val globalLocalisationErrors : CWError list option = None with get, set
    member val rulesLocalisationErrors : CWError list option = None with get, set
    member val localisationKeys : (Lang * Set<string>) list= [] with get, set
    member val taggedLocalisationKeys : (Lang * LocKeySet) list = [] with get, set
    member this.UpdateAllLocalisation() = updateAllLocalisationSources(); updateProcessedLocalisation()
    member __.UpdateProcessedLocalisation() = updateProcessedLocalisation()
    member __.UpdateLocalisationFile (locFile : FileWithContentResource) = updateLocalisationSource locFile
    member __.LocalisationAPIs() : (bool * ILocalisationAPI) list = localisationAPIMap |> Map.toList |> List.map snd
    member __.LocalisationFileNames() : string list = localisationAPIMap |> Map.toList |> List.map fst |> List.map (fun (f, l) -> sprintf "%A, %s" l f)
    member this.LocalisationKeys() = this.localisationKeys
