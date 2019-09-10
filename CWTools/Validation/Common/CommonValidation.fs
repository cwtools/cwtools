
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

    let validateIfWithNoEffect : StructureValidator<_> =
        fun _ es ->
            let keyID = CWTools.Utilities.StringResource.stringManager.InternIdentifierToken "limit"
            let keyIdIF = CWTools.Utilities.StringResource.stringManager.InternIdentifierToken "if"
            let keyIdelIF = CWTools.Utilities.StringResource.stringManager.InternIdentifierToken "else_if"
            let fNode = (fun (x : Node) children ->
                if (x.KeyId = keyIdIF || x.KeyId = keyIdelIF) && (x.Values |> List.isEmpty) && (x.Children |> List.exists (fun c -> c.KeyId <> keyID) |> not)
                then inv ErrorCodes.EmptyIf x <&&&> children
                else children
                )
            let fCombine = (<&&>)
            (es.AllEffects <&!&> foldNode2 fNode fCombine OK)
            <&&>
            (es.AllTriggers <&!&> foldNode2 fNode fCombine OK)


    let valUniqueTypes : LookupValidator<_> =
        (fun lu _ _ ->
            let types = lu.typeDefs |> List.filter (fun td -> td.unique)
            let zipped = types |> List.map (fun td -> td.name, lu.typeDefInfo.[td.name])
            let groupFun =
                List.groupBy (fun (tdi : TypeDefInfo) -> tdi.id) >> List.filter (fun (k, g) -> g.Length > 1)
                                 >> List.collect snd
            let res = zipped |> List.collect (fun (tn, ts) -> (groupFun ts) |> List.map (fun t -> tn, t))
            res <&!&> (fun (typename, (tdi)) ->
                            Invalid [invManual (ErrorCodes.DuplicateTypeDef typename tdi.id) tdi.range "" None])
                            //    |> List.map (fun td, ts -> )
            )
