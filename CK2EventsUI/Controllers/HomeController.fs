namespace CK2_Events.Controllers

open System
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc
open CK2_Events.Application
open FParsec
open CK2_Events.Application.Process
open System.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.FSharp
open Microsoft.AspNetCore.Mvc.Infrastructure
open FSharp.Core

module Utils = 
    let opts = [| TupleArrayConverter() :> JsonConverter |] // this goes global
    type System.Object with
        member x.ToJson =
            JsonConvert.SerializeObject(x,opts)
        
open Utils

type HomeController (provider : IActionDescriptorCollectionProvider) =
    inherit Controller()

    member val provider = provider
    

    member this.Index () =
         let files = Events.getFileList "./events/"
         this.View(files)


    member this.GetData (file) =
        let t = (CKParser.parseEventFile file)
        let ck2 = match t with
                    | Success(v, _, _) -> processEventFile v 
                    | _ -> failwith "No root"
        let ck3 = addLocalisedDescAll ck2
        //let triggers = getTriggeredEventsAll ck2
        let immediates = getAllImmediates ck3
        let options = getEventsOptions ck3
        let pretties = ck3.Events |> List.map (fun e -> (e.ID, CKParser.printKeyValueList e.Raw 0))
        this.Json((ck3.Events.ToJson, immediates.ToJson, options.ToJson, pretties.ToJson))

    member this.Graph (file : string) =
        this.View(model = file);
        
    member this.Error () =
        this.View();
