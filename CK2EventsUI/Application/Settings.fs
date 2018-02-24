namespace CK2Events.Application
open CWTools.Common

type GameDirectory (directory : string) =
    member val gameDirectory : string = directory with get, set
    member this.eventDirectory with get () = this.gameDirectory + "/events/"
    member this.localisationDirectory with get () = this.gameDirectory + "/localisation/"

type CK2Settings () =
    member val ck2Directory : string = "" with get, set
    member this.CK2Directory = GameDirectory(this.ck2Directory)
    member val ck2Language : CK2Lang = CK2Lang.English with get, set
    member val hoi4Directory : string = "" with get, set
    member this.HOI4Directory = GameDirectory(this.hoi4Directory)
    member val eu4Directory : string = "" with get, set
    member this.EU4Directory = GameDirectory(this.eu4Directory)
    member val stlDirectory : string = "" with get, set
    member val stlLanguage : STLLang = STLLang.English with get, set
    member this.STLDirectory = GameDirectory(this.stlDirectory)
    member this.Directory x = 
        match x with
        |Game.CK2 -> this.CK2Directory
        |Game.HOI4 -> this.HOI4Directory
        |Game.EU4 -> this.EU4Directory
        |Game.STL -> this.STLDirectory
        |_ -> failwith ("Unknown game enum value " + x.ToString())
    member val bundleEdges : bool = false with get, set

type UserSettings (settings:CK2Settings) =
    member this.ck2Directory = settings.ck2Directory
    member this.ck2Language = settings.ck2Language
    member this.hoi4Directory = settings.hoi4Directory
    member this.eu4Directory = settings.eu4Directory
    member this.stlDirectory = settings.stlDirectory
    member this.stlLanguage = settings.stlLanguage
    member this.bundleEdges = settings.bundleEdges

type CK2SettingsJson =
    { userSettings : UserSettings }

type AppSettings () =
    member val currentGame : Game = Game.CK2 with get, set
