namespace CK2_Events.ViewModels

open ElectronNET.API
open CK2_Events.Application


type BaseViewModel (settings) =
    member val settings : Settings = settings
    member val isElectronActive = HybridSupport.IsElectronActive