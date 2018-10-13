
namespace CWTools.Validation.Common
open CWTools.Process
open CWTools.Validation.ValidationCore
open CWTools.Process.ProcessCore


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
