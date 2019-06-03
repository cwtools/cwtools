
namespace CWTools.Validation.Common
open CWTools.Process
open CWTools.Validation
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
                if x.Key == "NOT" && (x.All.Length - (x.Comments |> Seq.length)) > 1
                 then Invalid [inv (ErrorCodes.CustomError "Reminder: NOT does not mean NOT AND" Severity.Information) x]
                else children
                )
            let fCombine = (<&&>)
            es.All <&!&> foldNode2 fNode fCombine OK

    let validateNOTMultiple : StructureValidator<_> =
        fun _ es ->
            let fNode = (fun (x : Node) children ->
                if x.Key == "NOT" && (x.All.Length - (x.Comments |> Seq.length)) > 1
                then inv ErrorCodes.IncorrectNotUsage x <&&&> children else children
                )
            let fCombine = (<&&>)
            (es.AllEffects <&!&> foldNode2 fNode fCombine OK)
            <&&>
            (es.AllTriggers <&!&> foldNode2 fNode fCombine OK)
    let valUniqueTypes : LookupValidator<_, _, _> =
        (fun lu _ _ ->
            let types = lu.typeDefs |> List.filter (fun td -> td.unique)
            eprintfn "vut %A" types
            let zipped = types |> List.map (fun td -> td.name, lu.typeDefInfo.[td.name])
            eprintfn "vut2 %A" zipped
            let groupFun =
                List.groupBy fst >> List.filter (fun (k, g) -> g.Length > 1)
                                 >> List.collect snd
            let res = zipped |> List.collect (fun (tn, ts) -> (groupFun ts) |> List.map (fun t -> tn, t))
            eprintfn "vut3 %A" res
            res <&!&> (fun (typename, (typeactual, range)) -> 
                            let error = sprintf "Key %s of type %s has been defined multiple types" typeactual typename
                            Invalid [invManual (ErrorCodes.CustomError error Severity.Error) range "" None])
                            //    |> List.map (fun td, ts -> )
            )
