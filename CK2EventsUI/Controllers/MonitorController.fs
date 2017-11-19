namespace CK2_Events.Controllers

open Microsoft.AspNetCore.Mvc.Infrastructure
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Mvc.Routing

open Utils
type MonitorController (provider : IActionDescriptorCollectionProvider) =
    inherit Controller()

    member val provider = provider
    member this.GetRoutes () =
        let routes = provider.ActionDescriptors.Items
                    |> List.ofSeq
                    |> List.map (fun f -> (f.RouteValues.["Action"], f.RouteValues.["Controller"]))
        this.Json(Ok(routes).ToJson)

