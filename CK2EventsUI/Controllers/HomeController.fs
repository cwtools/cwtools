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

module Utils = 
    let opts = [| TupleArrayConverter() :> JsonConverter |] // this goes global
    type System.Object with
        member x.ToJson =
            JsonConvert.SerializeObject(x,opts)
        
open Utils

type HomeController () =
    inherit Controller()
    

    //static let ck2 = Events.parseTen

    member this.Index () =
        //eprintfn "Test"
        //let ck2 = Events.parseTen "events"
        let t = (CKParser.parseEventFile "wol_business_events.txt")
        let ck2 = match t with
                    | Success(v, _, _) -> processEventFile v
                    | _ -> failwith "No root"
        this.View(ck2)


    member this.GetData () =
        let t = (CKParser.parseEventFile "wol_business_events.txt")
        let ck2 = match t with
                    | Success(v, _, _) -> processEventFile v 
                    | _ -> failwith "No root"
        let ck3 = addLocalisedDescAll ck2
        let triggers = getTriggeredEventsAll ck2
        //ck2.Events |> list.map ()
        (ck3.Events.ToJson, triggers.ToJson)
           
    member this.Test () =
        this.View();

    member this.Graph () =
        this.View();
    
    member this.Graph2 () =
        this.View();
        
    member this.Error () =
        this.View();
