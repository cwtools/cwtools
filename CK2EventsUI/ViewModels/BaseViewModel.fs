namespace CK2Events.ViewModels

open ElectronNET.API
open CK2Events.Application
open System.IO
open CK2Events.Application.Localisation.LocalisationDomain
open System
open Microsoft.AspNetCore.Mvc.Rendering
open System.Linq

type BaseViewModel (settings) =
    member val settings : CK2Settings = settings
    member val isElectronActive = HybridSupport.IsElectronActive

type SettingsViewModel (settings) = 
    inherit BaseViewModel (settings)
    let testGameDirectory dir =
            let locExists = Directory.EnumerateDirectories dir |> List.ofSeq |> List.exists (fun f -> (DirectoryInfo(f)).Name = "localisation")
            let eveExists = Directory.EnumerateDirectories dir |> List.ofSeq |> List.exists (fun f -> (DirectoryInfo(f)).Name = "events")
            locExists && eveExists
    
    member val validCK2Directory = settings.CK2Directory.gameDirectory = "" || testGameDirectory settings.CK2Directory.gameDirectory
    member val validHOI4Directory = settings.HOI4Directory.gameDirectory = "" || testGameDirectory settings.HOI4Directory.gameDirectory
    member val validEU4Directory = settings.EU4Directory.gameDirectory = "" || testGameDirectory settings.EU4Directory.gameDirectory
    member val validSTLDirectory = settings.STLDirectory.gameDirectory = "" || testGameDirectory settings.STLDirectory.gameDirectory
    member val ck2Languages = 
        let enums : CK2Lang seq = unbox (Enum.GetValues(typeof<CK2Lang>))
        enums |> Seq.map (fun e -> SelectListItem(Value = (int e).ToString(), Text = e.ToString()))
              |> (fun l -> SelectList(l, "Value", "Text"))

type IndexViewModel (settings, files, appSettings : AppSettings) =
    inherit BaseViewModel (settings)
    member val files : string list = files
    member val game : Game = appSettings.currentGame
    
    
type EventsViewModel (settings, files, game) =
    inherit BaseViewModel (settings)
    member val files : string = files
    member val game : Game = game

type LocalisationViewModelRow =
    {
        key : string
        pass : bool
        entries : int
        error : string
    }

type LocalisationViewModel (settings, localisation) =
    inherit BaseViewModel (settings)

    member val localisation : LocalisationAPI = localisation
    member val displayList = localisation.results.Select(fun d -> (d.Key, d.Value)) |> List.ofSeq |> List.map (fun (key, (pass, entries, error)) -> {key = key; pass = pass; entries = entries; error = error})