namespace CK2Events.Application

type CK2Settings () =
    member val gameDirectory : string = "" with get, set
    member this.eventDirectory with get () = this.gameDirectory + "/events/"
    member this.localisationDirectory with get () = this.gameDirectory + "/localisation/"

type UserSettings (settings:CK2Settings) =
    member this.gameDirectory = settings.gameDirectory

type CK2SettingsJson =
    { userSettings : UserSettings }