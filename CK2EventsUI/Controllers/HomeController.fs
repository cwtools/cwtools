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
open Microsoft.AspNetCore.Hosting



type HomeController (provider : IActionDescriptorCollectionProvider, settings : IOptions<CK2Settings>, localisation : Localisation.LocalisationService, hostingEnvironment : IHostingEnvironment) =
    inherit Controller()

    let settings : CK2Settings = settings.Value
 
    member val provider = provider

    member this.Index (game : Game) : ActionResult =
        match Directory.Exists(settings.Directory(game).gameDirectory) with
        | false -> upcast this.RedirectToAction("Settings")
        | true -> 
            let files = Events.getFileList (settings.Directory(game).eventDirectory)
            let viewmodel = IndexViewModel(settings, files, game)
            upcast this.View(viewmodel)
    
    member this.Localisation () =
        let viewmodel = LocalisationViewModel(settings, localisation)
        this.View(model = viewmodel)

    member this.FirstRun() =
        this.Settings()

        
    
    member this.SetFolder (game : Game) =
        let setGameSettings x = 
            match game with
            |Game.CK2 -> settings.ck2Directory <- x
            |Game.HOI4 -> settings.hoi4Directory <- x
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
        this.RedirectToAction("Settings")

    member this.GetData (game : Game, ``files[]`` : string[]) =
        let files = ``files[]`` |> List.ofSeq
        let processFile file = 
            let filePath = settings.Directory(game).eventDirectory + file + ".txt"
            let fileString = File.ReadAllText(filePath)
            let t = (CKParser.parseEventString fileString file)
            match t with
                | Success(v, _, _) -> 
                    let ck2 = processEventFile v 
                    let comments = getEventComments ck2
                    let immediates = getAllImmediates ck2
                    let options = getEventsOptions localisation ck2
                    let pretties = ck2.Events |> List.map (fun e -> (e.ID, CKPrinter.api.prettyPrintStatements e.ToRaw))
                    let ck3 = addLocalisedDescAll ck2 localisation
                    (true, ck3.Events, immediates, options, pretties, comments, "")
                | ParserResult.Failure(msg, _, _) -> 
                    (false,[],[],[],[],[], msg)
        let combineResults (s, e, i, o, p, c, m) (ns, ne, ni, no, np, nc, nm)=
            match ns with
            | false -> (ns && s, e, i, o, p, c, nm::m)
            | true -> (ns && s, e @ ne, i @ ni, o @ no, p @ np, c @ nc, nm::m)
        let processed = files |> List.map processFile
        let results = processed |> List.fold combineResults (true, [], [], [], [], [], [])
        let formatted = results |> (fun (s, e, i, o, p, c, m) -> (s, e.ToJson, i.ToJson, o.ToJson, p.ToJson, c.ToJson, m.ToJson))
        this.Json(formatted)

    member this.Graph (files : System.Collections.Generic.List<System.String>, game : Game) =
        let viewmodel = EventsViewModel(settings, files.ToJson, game)
        this.View(model = viewmodel);
        
    member this.Error () =
        this.View();

    [<HttpGet>]
    member this.Settings () =        
        this.View(SettingsViewModel(settings))
    
    [<HttpPost>]
    member this.Settings (settings : CK2Settings) =
        let settingsWrap = { userSettings = UserSettings settings}
        let json = settingsWrap.ToJson
        
        File.WriteAllText(hostingEnvironment.ContentRootPath+"/userSettings.json", json)
        this.RedirectToAction("Index")