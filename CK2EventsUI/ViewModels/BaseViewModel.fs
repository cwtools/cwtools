namespace CK2Events.ViewModels

open ElectronNET.API
open CK2Events.Application


type BaseViewModel (settings) =
    member val settings : CK2Settings = settings
    member val isElectronActive = HybridSupport.IsElectronActive