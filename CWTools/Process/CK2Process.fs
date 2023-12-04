namespace CWTools.Process
open CWTools.Parser.Types
open CWTools.Localisation
open CWTools.Process.ProcessCore
open CWTools.Process
open CWTools.Utilities.Position

module CK2Process =
    let ck2Process = BaseProcess()

    let processEventFile (ev : ParsedFile) =
        let (ParsedFile e) = ev
        (ck2Process.ProcessNode()) "" range.Zero e





