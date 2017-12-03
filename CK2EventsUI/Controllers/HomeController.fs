namespace CK2Events.Controllers

open System.Linq
open Microsoft.AspNetCore.Mvc
open CK2Events.Application
open FParsec
open Process
open Newtonsoft.Json
open Newtonsoft.Json.FSharp
open Microsoft.AspNetCore.Mvc.Infrastructure
open FSharp.Core
open ElectronNET.API
open CK2Events.ViewModels
open System.IO

module Utils = 
    let opts = [| TupleArrayConverter() :> JsonConverter |] // this goes global
    type System.Object with
        member x.ToJson =
            JsonConvert.SerializeObject(x,opts)
        
open Utils
open ElectronNET.API.Entities



type HomeController (provider : IActionDescriptorCollectionProvider) =
    inherit Controller()

    member val provider = provider
    static member val settings = Settings() with get, set

    member this.Index () =
        let files = Events.getFileList HomeController.settings.eventDirectory
        this.View(files)
    
    member this.SetFolder () =
        let folderPrompt() = 
            let mainWindow = Electron.WindowManager.BrowserWindows.First()
            let options = OpenDialogOptions (Properties = Array.ofList [OpenDialogProperty.openDirectory])
            let folder = Electron.Dialog.ShowOpenDialogAsync(mainWindow, options) |> Async.AwaitTask
            folder 
        let files = match HybridSupport.IsElectronActive with
                    | true -> Async.RunSynchronously(folderPrompt()).[0]
                    | false -> "./events/" 
        HomeController.settings.eventDirectory <- files
        this.RedirectToAction("Settings")

    member this.GetData (file) =
        let filePath = HomeController.settings.eventDirectory + file + ".txt"
        let fileString = File.ReadAllText(filePath)
        let t = (CKParser.parseEventString fileString file)
        //let t = (CKParser.parseEventFile filePath)
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

    [<HttpGet>]
    member this.Settings () =
        this.View(BaseViewModel(HomeController.settings))
    
    [<HttpPost>]
    member this.Settings (settings : Settings) =
        HomeController.settings <- settings
        this.View(BaseViewModel(HomeController.settings))
