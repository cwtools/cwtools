namespace CWTools.Parser


open FParsec
open CWTools.Common
open CWTools.Common.CK2Constants
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Utilities.Utils
open CWTools.Common.NewScope

module CK2Parser =


    let loadModifiers filename fileString =
        let parsed = CKParser.parseString fileString filename
        match parsed with
        |Failure(e, _, _) -> log (sprintf "modifier file %s failed with %s" filename e); ([])
        |Success(s,_,_) ->
            let root = simpleProcess.ProcessNode() "root" (mkZeroFile filename) (s)
            root.Child "modifiers"
                |> Option.map (fun ms ->  ms.Values |> List.map(fun l -> {ActualModifier.tag = l.Key; category = modifierCategoryManager.ParseModifier() (l.Value.ToRawString())}))
                |> Option.defaultValue []



