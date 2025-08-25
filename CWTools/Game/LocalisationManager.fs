namespace CWTools.Games

open CWTools.Common
open CWTools.Localisation
open CWTools.Utilities.Utils
open FSharp.Collections.ParallelSeq

[<Sealed>]
type LocalisationManager<'T when 'T :> ComputedData>
    (
        resources: IResourceAPI<'T>,
        localisationService: _ -> ILocalisationAPICreator,
        langs: Lang list,
        lookup: Lookup,
        processLocalisation,
        localisationExtension: string
    ) as this =
    let mutable localisationAPIMap: Map<string * Lang, bool * ILocalisationAPI> =
        Map.empty

    let allLocalisation () = this.LocalisationAPIs() |> List.map snd

    let validatableLocalisation () =
        this.LocalisationAPIs()
        |> List.choose (fun (validate, api) -> if validate then Some api else None)

    let parseLocFile (locFile: FileWithContentResource) =
        if locFile.overwrite <> Overwrite.Overwritten && locFile.extension = localisationExtension then
            let locService = [ locFile.filepath, locFile.filetext ] |> localisationService

            Some(
                langs
                |> List.map (fun lang -> (locFile.filepath, lang), (locFile.validate, locService.Api(lang)))
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

        let groupedLocalisation = allLocalisation () |> List.groupBy _.GetLang

        this.localisationKeys <-
            groupedLocalisation
            |> List.map (fun (k, g) -> k, g |> Seq.collect _.GetKeys |> Set.ofSeq)

        this.taggedLocalisationKeys <-
            groupedLocalisation
            |> List.map (fun (k, g) ->
                k,
                g
                |> Seq.collect _.GetKeys
                |> Seq.fold (fun (s: LocKeySet) v -> s.Add v) (LocKeySet.Empty(InsensitiveStringComparer())))

    let updateLocalisationSource (locFile: FileWithContentResource) =
        let loc = parseLocFile locFile |> Option.defaultValue []

        let newMap =
            loc
            |> List.fold (fun map (key, value) -> Map.add key value map) localisationAPIMap

        localisationAPIMap <- newMap

        let groupedLocalisation = allLocalisation () |> List.groupBy _.GetLang

        this.localisationKeys <-
            groupedLocalisation
            |> List.map (fun (k, g) -> k, g |> Seq.collect (fun ls -> ls.GetKeys) |> Set.ofSeq)

        this.taggedLocalisationKeys <-
            groupedLocalisation
            |> List.map (fun (k, g) ->
                k,
                g
                |> Seq.collect (fun ls -> ls.GetKeys)
                |> Seq.fold (fun (s: LocKeySet) v -> s.Add v) (LocKeySet.Empty(InsensitiveStringComparer())))

    let updateProcessedLocalisation () =
        let validatableEntries =
            validatableLocalisation ()
            |> List.groupBy (fun l -> l.GetLang)
            |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList) |> Map.ofList)

        let processLoc = processLocalisation lookup
        lookup.proccessedLoc <- validatableEntries |> List.map processLoc

    member val localisationErrors: CWError list option = None with get, set
    member val globalLocalisationErrors: CWError list option = None with get, set
    member val rulesLocalisationErrors: CWError list option = None with get, set
    member val localisationKeys: (Lang * Set<string>) list = [] with get, set
    member val taggedLocalisationKeys: (Lang * LocKeySet) list = [] with get, set

    member this.UpdateAllLocalisation() =
        updateAllLocalisationSources ()
        updateProcessedLocalisation ()

    member __.UpdateProcessedLocalisation() = updateProcessedLocalisation ()
    member __.UpdateLocalisationFile(locFile: FileWithContentResource) = updateLocalisationSource locFile

    member __.LocalisationAPIs() : (bool * ILocalisationAPI) list = localisationAPIMap.Values |> Seq.toList

    member __.LocalisationFileNames() : string list =
        localisationAPIMap
        |> Map.toList
        |> List.map (fun ((f, l), (_, a)) -> sprintf "%A, %s, %i" l f a.GetKeys.Length)

    member this.LocalisationKeys() = this.localisationKeys

    member this.LocalisationEntries() =
        allLocalisation ()
        |> List.groupBy (fun l -> l.GetLang)
        |> List.map (fun (k, g) -> k, g |> List.collect (fun ls -> ls.ValueMap |> Map.toList))
