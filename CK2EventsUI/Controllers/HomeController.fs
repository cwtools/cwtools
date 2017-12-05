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
open Microsoft.Extensions.Options
open CK2Events.Application.Localisation



type HomeController (provider : IActionDescriptorCollectionProvider, settings : IOptions<CK2Settings>, localisation : LocalisationService) =
    inherit Controller()

    let settings : CK2Settings = settings.Value

    member val provider = provider

    member this.Index () : ActionResult =
        match settings.gameDirectory with
        | None -> upcast this.RedirectToAction("Settings")
        | Some _ -> 
            let files = Events.getFileList settings.eventDirectory.Value
            upcast this.View(files)

    member this.FirstRun() =
        this.Settings()
    
    member this.SetFolder () =
        let folderPrompt() = 
            let mainWindow = Electron.WindowManager.BrowserWindows.First()
            let options = OpenDialogOptions (Properties = Array.ofList [OpenDialogProperty.openDirectory])
            let folder = Electron.Dialog.ShowOpenDialogAsync(mainWindow, options) |> Async.AwaitTask
            folder 
        let files = match HybridSupport.IsElectronActive with
                    | true -> Async.RunSynchronously(folderPrompt()).[0]
                    | false -> "./events/" 
        settings.gameDirectory <- Some files
        this.RedirectToAction("Settings")

    member this.GetData (file) =
        let filePath = settings.eventDirectory.Value + file + ".txt"
        let fileString = File.ReadAllText(filePath)
        let t = (CKParser.parseEventString fileString file)
        let ck2 = match t with
                    | Success(v, _, _) -> processEventFile v 
                    | _ -> failwith "No root"
        let ck3 = addLocalisedDescAll ck2 localisation
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
        this.View(BaseViewModel(settings))
    
    [<HttpPost>]
    member this.Settings (settings : CK2Settings) =
        let json = settings.ToJson
        File.WriteAllText("./CK2Events.json", json)
        this.View(BaseViewModel(settings))
