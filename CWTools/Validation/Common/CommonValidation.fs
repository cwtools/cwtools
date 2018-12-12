
namespace CWTools.Validation.Common
open CWTools.Process
open CWTools.Validation.ValidationCore
open CWTools.Process.ProcessCore
open CWTools.Utilities.Utils
open CWTools.Common


module CommonValidation =
    let validateMixedBlocks : StructureValidator<_> =
        fun _ es ->
            let fNode = (fun (x : Node) children ->
                if (x.LeafValues |> Seq.isEmpty |> not && (x.Leaves |> Seq.isEmpty |> not || x.Children |> Seq.isEmpty |> not)) |> not
                then children
                else Invalid [inv ErrorCodes.MixedBlock x] <&&> children
                )
            let fCombine = (<&&>)
            es.All <&!&> foldNode2 fNode fCombine OK
    let validateEU4NaiveNot : StructureValidator<_> =
        fun _ es ->
            let fNode = (fun (x : Node) children ->
                if x.Key == "NOT" && x.All.Length > 1
                 then Invalid [inv (ErrorCodes.CustomError "Reminder: NOT does not mean NOT AND" Severity.Information) x]
                else children
                )
            let fCombine = (<&&>)
            es.All <&!&> foldNode2 fNode fCombine OK
