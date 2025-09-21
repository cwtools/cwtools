namespace CWTools.Games

open System
open CWTools.Common
open CWTools.Localisation
open CWTools.Utilities.Utils
open FSharp.Collections.ParallelSeq

[<Sealed>]
type LocalisationManager<'T when 'T :> ComputedData>
    (
        resources: IResourceAPI<'T>,
        localisationService: _ -> ILocalisationAPICreator,
        langs: Lang array,
        lookup: Lookup,
        processLocalisation,
        localisationExtension: string
    ) as this =
    let mutable localisationAPIMap: Map<string * Lang, struct (bool * ILocalisationAPI)> =
        Map.empty

    let validatableLocalisation () =
        this.GetLocalisationAPIs()
        |> List.choose (fun struct (validate, api) -> if validate then Some api else None)

    let parseLocFile (locFile: FileWithContentResource) =
        if
            locFile.overwrite <> Overwrite.Overwritten
            && locFile.extension = localisationExtension
        then
            let locService = [ locFile.filepath, locFile.filetext ] |> localisationService

            Some(
                langs
                |> Array.map (fun lang -> (locFile.filepath, lang), struct (locFile.validate, locService.Api(lang)))
            )
        else
            None

    let updateAllLocalisationSources () =
        localisationAPIMap <-
            let allLocs =
                resources.GetResources()
                |> List.choose (function
                    | FileWithContentResource(_, e) -> Some e
                    | _ -> None)
                |> PSeq.choose parseLocFile
                |> Seq.collect id

            allLocs |> Map.ofSeq

        let groupedLocalisation = this.GetCleanLocalisationAPIs() |> List.groupBy _.GetLang

        this.localisationKeys <-
            groupedLocalisation
            |> List.map (fun (k, g) -> k, g |> Seq.collect _.GetKeys |> Set.ofSeq)

        this.taggedLocalisationKeys <-
            groupedLocalisation
            |> List.map (fun (k, g) ->
                k,
                g
                |> Seq.collect _.GetKeys
                |> Seq.fold (fun (s: LocKeySet) v -> s.Add v) (LocKeySet.Empty(StringComparer.OrdinalIgnoreCase)))

    let updateLocalisationSource (locFile: FileWithContentResource) =
        let loc = parseLocFile locFile |> Option.defaultValue [||]

        let newMap =
            loc
            |> Array.fold (fun map (key, value) -> Map.add key value map) localisationAPIMap

        localisationAPIMap <- newMap

        let groupedLocalisation = this.GetCleanLocalisationAPIs() |> List.groupBy _.GetLang

        this.localisationKeys <-
            groupedLocalisation
            |> List.map (fun (k, g) -> k, g |> Seq.collect (fun ls -> ls.GetKeys) |> Set.ofSeq)

        this.taggedLocalisationKeys <-
            groupedLocalisation
            |> List.map (fun (k, g) ->
                k,
                g
                |> Seq.collect (fun ls -> ls.GetKeys)
                |> Seq.fold (fun (s: LocKeySet) v -> s.Add v) (LocKeySet.Empty(StringComparer.OrdinalIgnoreCase)))

    let updateProcessedLocalisation () =
        let validatableEntries =
            validatableLocalisation ()
            |> List.groupBy (fun l -> l.GetLang)
            |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList) |> Map.ofList)

        let processLoc = processLocalisation lookup
        lookup.proccessedLoc <- validatableEntries |> List.map processLoc

    member val localisationErrors: CWError list option = None with get, set
    member val globalLocalisationErrors: CWError list option = None with get, set
    member val localisationKeys: (Lang * Set<string>) list = [] with get, set
    member val taggedLocalisationKeys: (Lang * LocKeySet) list = [] with get, set

    member this.UpdateAllLocalisation() =
        updateAllLocalisationSources ()
        updateProcessedLocalisation ()

    member _.UpdateProcessedLocalisation() = updateProcessedLocalisation ()
    member _.UpdateLocalisationFile(locFile: FileWithContentResource) = updateLocalisationSource locFile

    member _.GetLocalisationAPIs() : (struct (bool * ILocalisationAPI)) list = localisationAPIMap.Values |> Seq.toList

    member this.GetCleanLocalisationAPIs() : ILocalisationAPI list =
        this.GetLocalisationAPIs() |> List.map structSnd

    member _.LocalisationFileNames() : string list =
        localisationAPIMap
        |> Map.toList
        |> List.map (fun ((f, l), (_, a)) -> sprintf "%A, %s, %i" l f a.GetKeys.Length)

    member this.LocalisationKeys() = this.localisationKeys

    member this.LocalisationEntries() =
        this.GetCleanLocalisationAPIs()
        |> List.groupBy (fun l -> l.GetLang)
        |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList))
