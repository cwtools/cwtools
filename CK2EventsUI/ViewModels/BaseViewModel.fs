namespace CK2Events.ViewModels

open ElectronNET.API
open CK2Events.Application
open System.IO
open System
open Microsoft.AspNetCore.Mvc.Rendering
open System.Linq
open CWTools.Common
open CWTools.Localisation
open CWTools.Games
open CWTools.Games.Stellaris
open CWTools.Parser
open CWTools.Games.Files


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

    member val stlLanguages =
        let enums : STLLang seq = unbox (Enum.GetValues(typeof<STLLang>))
        enums |> Seq.map (fun e -> SelectListItem(Value = (int e).ToString(), Text = e.ToString()))
              |> (fun l -> SelectList(l, "Value", "Text"))

type IndexViewModel (settings, files, filenamespaces, appSettings : AppSettings) =
    inherit BaseViewModel (settings)
    member val files : List<String> = files
    member val filenamespaces : List<String> = filenamespaces
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

    member val localisation : ILocalisationAPI = localisation
    member val displayList = localisation.Results.Select(fun d -> (d.Key, d.Value)) |> List.ofSeq |> List.map (fun (key, (pass, entries, error)) -> {key = key; pass = pass; entries = entries; error = error})

type ValidationViewModelErrorRow =
    {
        category : string
        error : string
        position : string
    }
type ValidationViewModelParseRow =
    {
        file : string
        error : string
    }
type ValidationViewModelFileRow =
    {
        file : string
        pass : bool
    }
type ValidationViewModel (settings, appSettings : AppSettings) =
    inherit BaseViewModel(settings)
    let emptyStellarisSettings (rootDirectory) = {
        rootDirectory = rootDirectory
        scope = FilesScope.All
        modFilter = None
        validation = {
            validateVanilla = false
            experimental = true
            langs = [STL STLLang.English]
        }
        rules = None
        embedded = {
            triggers = []
            effects = []
            modifiers = []
            embeddedFiles = []
        }
    }

    let game = STLGame(emptyStellarisSettings settings.STLDirectory.gameDirectory) :> IGame
    let validationErrors = game.ValidationErrors()
    member val folders = game.Folders()
    member val parserErrorList = game.ParserErrors() |> List.map (fun (f, e, _) -> {file = f; error = e})
    member val validationErrorList = validationErrors |> List.map (fun (s, _, p, _, e, _) -> {category = s ; error = e; position = p.ToString()})
    member val allFileList = game.AllFiles() |> List.choose (function |EntityResource (s,e) -> Some (s,e) |_ -> None) |> List.map (fun (f, p) -> {file = f; pass = p.result |> function |Pass _ -> true |_ -> false})