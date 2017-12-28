namespace CK2Events.Controllers

open System.Linq
open Microsoft.AspNetCore.Mvc
open CK2Events.Application
open CK2Events.Application.Localisation
open Newtonsoft.Json
open Newtonsoft.Json.FSharp
open Microsoft.AspNetCore.Mvc.Infrastructure
open FSharp.Core
open ElectronNET.API
open CK2Events.ViewModels
open System.IO

module Utils = 
    open Microsoft.AspNetCore.Mvc.ViewFeatures
    let opts = [| TupleArrayConverter() :> JsonConverter |] // this goes global
    type System.Object with
        member x.ToJson =
            JsonConvert.SerializeObject(x,opts)

    type ITempDataDictionary with
        member this.Put<'T>(key : string, value : 'T) = this.[key] <- JsonConvert.SerializeObject(value)
        member this.Get<'T>(key : string) = 
            match this.ContainsKey(key) with
            | true -> Some (JsonConvert.DeserializeObject<'T>(string this.[key]))
            | false -> None
        
open Utils
open ElectronNET.API.Entities
open Microsoft.Extensions.Options
open Microsoft.AspNetCore.Hosting



type HomeController (provider : IActionDescriptorCollectionProvider, settings : IOptionsSnapshot<CK2Settings>, localisation : LocalisationDomain.LocalisationAPI, hostingEnvironment : IHostingEnvironment, appSettings : AppSettings) =
    inherit BaseController()

    //let settings : CK2Settings = settings.Value
 
    member val provider = provider

    member this.Index () : ActionResult =
        match Directory.Exists(settings.Value.Directory(appSettings.currentGame).eventDirectory) with
        | false -> upcast this.RedirectToAction("Settings")
        | true -> 
            let files = Events.getFileList (settings.Value.Directory(appSettings.currentGame).eventDirectory)
            let viewmodel = IndexViewModel(settings.Value, files, appSettings)
            upcast this.View(viewmodel)
    
    member this.Localisation () =
        let viewmodel = LocalisationViewModel(settings.Value, localisation)
        this.View(model = viewmodel)

    member this.FirstRun() =
        this.Settings()

        
    
    member this.SetFolder (game : Game) =
        let setGameSettings x = 
            match game with
            |Game.CK2 -> settings.Value.ck2Directory <- x
            |Game.HOI4 -> settings.Value.hoi4Directory <- x
            |Game.EU4 -> settings.Value.eu4Directory <- x
            |Game.STL -> settings.Value.stlDirectory <- x
        let folderPrompt() = 
            let mainWindow = Electron.WindowManager.BrowserWindows.First()
            let options = OpenDialogOptions (Properties = Array.ofList [OpenDialogProperty.openDirectory])
            let folder = Electron.Dialog.ShowOpenDialogAsync(mainWindow, options) |> Async.AwaitTask
            folder 
        let processFiles = function
            | [|x|] -> setGameSettings x
            | _ -> ()
        match HybridSupport.IsElectronActive with
            | true -> Async.RunSynchronously(folderPrompt()) |> processFiles
            | false -> () 
        this.TempData.Put("settings", settings)
        this.RedirectToAction("Settings")

    member this.GetData (game : Game, ``files[]`` : string[]) =
        let files = ``files[]`` |> List.ofSeq
        let combineResults (s, e, i, o, p, c, m) (ns, ne, ni, no, np, nc, nm)=
            match ns with
            | false -> (ns && s, e, i, o, p, c, nm::m)
            | true -> (ns && s, e @ ne, i @ ni, o @ no, p @ np, c @ nc, nm::m)
        let processed = files |> List.map (Events.getProcessedEvent settings.Value game localisation)
        let results = processed |> List.fold combineResults (true, [], [], [], [], [], [])
        let formatted = results |> (fun (s, e, i, o, p, c, m) -> (s, e.ToJson, i.ToJson, o.ToJson, p.ToJson, c.ToJson, m.ToJson))
        this.Json(formatted)

    member this.Graph (files : System.Collections.Generic.List<System.String>, game : Game) =
        let viewmodel = EventsViewModel(settings.Value, files.ToJson, game)
        this.View(model = viewmodel);
        
    member this.Error () =
        this.View();

    [<HttpGet>]
    member this.Settings () =        
        let newSettings = this.TempData.Get("settings") |> Option.defaultValue settings.Value
        this.View(SettingsViewModel(newSettings))
    
    [<HttpPost>]
    member this.Settings (settings : CK2Settings) =
        let settingsWrap = { userSettings = UserSettings settings}
        let json = settingsWrap.ToJson
        
        File.WriteAllText(hostingEnvironment.ContentRootPath+"/userSettings.json", json)
        this.RedirectToAction("Index")