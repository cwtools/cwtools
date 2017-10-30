namespace CK2_Events.Controllers

open System
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc
open CK2_Events.Application
open FParsec


type HomeController () =
    inherit Controller()

    member this.Index () =
        let ck2 = CKParser.y
        match ck2 with
        | Success (v,_,_) ->
            this.View(model = CKParser.prettyPrint v)
        | Failure (msg,_,_) ->
            this.View(model = sprintf "%O" msg)
        

    member this.About () =
        this.ViewData.["Message"] <- "Your application description page."
        let ck2 = sprintf "%A" CKParser.y
        this.ViewData.["Message"] <- ck2
        this.View()

    member this.Contact () =
        this.ViewData.["Message"] <- "Your contact page."
        this.View()

    member this.Error () =
        this.View();
