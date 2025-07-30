namespace CWTools.Process

open CWTools.Parser.Types
open CWTools.Process.ProcessCore
open CWTools.Utilities.Position

module CK2Process =
    let ck2Process = BaseProcess()

    let processEventFile (ev: ParsedFile) =
        let (ParsedFile e) = ev
        ck2Process.ProcessNode () "" Range.range0 e
