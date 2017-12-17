namespace CK2Events.Application

open System.Text.RegularExpressions
type Game = |CK2 = 0 |HOI4 = 1

type GameDirectory (directory : string) =
    member val gameDirectory : string = directory with get, set
    member this.eventDirectory with get () = this.gameDirectory + "/events/"
    member this.localisationDirectory with get () = this.gameDirectory + "/localisation/"

type CK2Settings () =
    member val ck2Directory : string = "" with get, set
    member this.CK2Directory = GameDirectory(this.ck2Directory)
    member val hoi4Directory : string = "" with get, set
    member this.HOI4Directory = GameDirectory(this.hoi4Directory)
    member this.Directory x = 
        match x with
        |Game.CK2 -> this.CK2Directory
        |Game.HOI4 -> this.HOI4Directory
    member val bundleEdges : bool = false with get, set

type UserSettings (settings:CK2Settings) =
    member this.ck2Directory = settings.ck2Directory
    member this.hoi4Directory = settings.hoi4Directory
    member this.bundleEdges = settings.bundleEdges

type CK2SettingsJson =
    { userSettings : UserSettings }