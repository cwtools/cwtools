namespace CWTools.Games
open CWTools.Common
open CWTools.Localisation
open CWTools.Utilities.Utils
open CWTools.Validation.ValidationCore
open System.Resources
open CWTools.Process.Scopes


type LocalisationManager<'S, 'T, 'M when 'S : comparison and 'S :> IScope<'S> and 'T :> ComputedData and 'M :> IModifier>
    (resources : IResourceAPI<'T>, localisationService : _ -> ILocalisationAPICreator, langs : Lang list, lookup: Lookup<_,_>,
     processLocalisation : (Lookup<'S,'M> -> (Lang * LocKeySet) list -> Lang * Map<string,Entry>-> Lang * Map<string,LocEntry<_>>)) as this =
    let allLocalisation() = this.localisationAPIs |> List.map snd
    let validatableLocalisation() = this.localisationAPIs |> List.choose (fun (validate, api) -> if validate then Some api else None)
    let updateAllLocalisationSources() =
        this.localisationAPIs <-
            let locs = resources.GetResources()
                        |> List.choose (function |FileWithContentResource (_, e) -> Some e |_ -> None)
                        |> List.filter (fun f -> f.overwrite <> Overwritten && f.extension = ".yml")
                        |> List.groupBy (fun f -> f.validate)
                        |> List.map (fun (b, fs) -> b, fs |> List.map (fun f -> f.filepath, f.filetext) |> localisationService)
            let allLocs = locs |> List.collect (fun (b,l) -> (langs)|> List.map (fun lang -> b, l.Api(lang)))
            allLocs
        this.localisationKeys <-allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |>List.collect (fun ls -> ls.GetKeys) |> Set.ofList )
        this.taggedLocalisationKeys <- allLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.GetKeys) |> List.fold (fun (s : LocKeySet) v -> s.Add v) (LocKeySet.Empty(InsensitiveStringComparer())) )

    let updateProcessedLocalisation() =
        let validatableEntries = validatableLocalisation() |> List.groupBy (fun l -> l.GetLang) |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList) |> Map.ofList)
        let processLoc = processLocalisation lookup this.taggedLocalisationKeys
        lookup.proccessedLoc <- validatableEntries |> List.map processLoc


    member val localisationAPIs : (bool * ILocalisationAPI) list = [] with get, set
    member val localisationErrors : CWError list option = None with get, set
    member val localisationKeys : (Lang * Set<string>) list= [] with get, set
    member val taggedLocalisationKeys : (Lang * LocKeySet) list = [] with get, set
    member this.UpdateAllLocalisation() = updateAllLocalisationSources(); updateProcessedLocalisation()
