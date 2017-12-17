namespace CK2Events.ViewModels

open ElectronNET.API
open CK2Events.Application
open System.IO
open SQLitePCL
open CK2Events.Application.Localisation

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

type IndexViewModel (settings, files, game) =
    inherit BaseViewModel (settings)
    member val files : string list = files
    member val game : Game = game
    
type EventsViewModel (settings, files, game) =
    inherit BaseViewModel (settings)
    member val files : string = files
    member val game : Game = game

type LocalisationViewModel (settings, localisation) =
    inherit BaseViewModel (settings)
    member val localisation : LocalisationService = localisation