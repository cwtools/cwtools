namespace CK2Events.Application

open Microsoft.Extensions.FileSystemGlobbing.Internal.PathSegments
type CK2Settings () =
    member val gameDirectory : string Option = None with get, set
    member this.eventDirectory with get () = this.gameDirectory |> Option.map(fun d -> d+"/events/")
    member this.localisationDirectory with get () = this.gameDirectory |> Option.map(fun d -> d+"/localisation/")
