
namespace CWTools.Validation.Common
open CWTools.Process
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process.ProcessCore
open CWTools.Utilities.Utils
open CWTools.Common
open CWTools.Utilities.Position
open CWTools.Games
open CWTools.Parser

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

    let valScriptedEffectParams<'T when 'T :> ComputedData> : CWTools.Games.LookupFileValidator<'T> =
        (fun fileManager rulesValidator lu res es ->

            let findParams (pos : CWTools.Utilities.Position.range) (key : string) =
                match res.AllEntities() |> List.tryFind (fun struct(e, _) -> e.filepath = pos.FileName) with
                | Some struct (e, _) ->
                    let rec findChild (node: Node) =
                        if node.Position = pos
                        then Some node
                        else
                            match node.Children |> List.tryFind (fun n -> rangeContainsRange n.Position pos) with
                            | Some c -> findChild c
                            | None -> None
                    findChild e.entity
                | None -> None
                //|> Option.map (fun s -> eprintfn "vsep %A %A" s.Key key; s)
                |> Option.map (fun s -> s.Values |> List.map (fun l -> "$" + l.Key + "$", l.ValueText))
            let findSE (pos : range) =
                match res.AllEntities() |> List.tryFind (fun struct(e, _) -> e.filepath = pos.FileName) with
                | Some struct (e, _) ->
                    let rec findChild (node: Node) =
                        if node.Position = pos
                        then Some node
                        else
                            match node.Children |> List.tryFind (fun n -> rangeContainsRange n.Position pos) with
                            | Some c -> findChild c
                            | None -> None
                    findChild e.entity
                | None -> None
            match rulesValidator, lu.typeDefInfo |> Map.tryFind "scripted_effect" with
            | Some rv, Some ses ->
                let allScriptedEffects = ses |> List.map (fun se -> se.id, se.range.FileName, findSE se.range)
                let getRefsFromRefTypes (referencedtypes: Map<string, ReferenceDetails list>) =
                    //eprintfn "grfrt %A" referencedtypes
                    referencedtypes |> (fun refMap -> Map.tryFind "scripted_effect" refMap) |> Option.defaultValue []
                let allRefs = res.AllEntities() |> List.collect (fun struct (n, d) -> d.Force().Referencedtypes |> Option.map getRefsFromRefTypes |> Option.defaultValue [])
                                             |> List.map (fun ref -> {| effectName = ref.name; callSite = ref.position; seParams = findParams ref.position ref.name|})
                                             |> List.groupBy (fun ref -> ref.effectName)
                                             |> Map.ofList
                // eprintfn "ar %A" allRefs
                let scriptedEffects = allScriptedEffects |> List.map (fun (name, filename, node) -> name, fileManager.ConvertPathToLogicalPath(filename), node, allRefs |> Map.tryFind name |> Option.defaultValue [])

                let stringReplace (seParams : (string * string) list) (key : string) =
                    seParams |> List.fold (fun (key : string) (par, value) -> key.Replace(par, value)) key
                let rec foldOverNode (stringReplacer) (node : Node) =
                    // eprintfn "fov %A" node.Key
                    node.Key <- stringReplacer node.Key
                    node.Values |> List.iter (fun (l : Leaf) -> l.Key <- stringReplacer l.Key; l.Value |> (function |Value.String s -> l.Value <- String (stringReplacer s) |Value.QString s -> l.Value <- QString (stringReplacer s) |_ -> ()))
                    node.LeafValues |> Seq.iter (fun (l : LeafValue) -> l.Value |> (function |Value.String s -> l.Value <- String (stringReplacer s) |Value.QString s -> l.Value <- QString (stringReplacer s) |_ -> ()))
                    node.Children |> List.iter (foldOverNode stringReplacer)
                let validateSESpecific (name : string) (logicalpath : string) (node : Node) (callSite : range) (seParams : (string * string) list option) =
                    let newNode = CWTools.Process.STLProcess.cloneNode node
                    let rootNode = Node("root")
                    rootNode.AllArray <- [|NodeC newNode|]
                    match seParams with
                    | Some seps ->
                        foldOverNode (stringReplace seps) newNode
                    | None -> ()
                    // eprintfn "%A %A" (CKPrinter.api.prettyPrintStatements newNode.ToRaw) (seParams)
                    let res = rv.ManualRuleValidate(logicalpath, rootNode)
                    // eprintfn "%A %A" logicalpath res
                    let message = {
                        location = callSite
                        message = sprintf "This call of scripted effect %s results in an error" name
                    }
                    res |> (function
                            | OK -> OK
                            | Invalid (inv) -> Invalid (inv |> List.map (fun (a, b, c, d, e, f, _) -> (a, b, c, d, e, f, Some message))))
                let validateSE (name: string) (logicalpath : string) (node : Node option) (refs : {|callSite : range ; effectName : string ; seParams : (string * string) list option|} list) =
                    // eprintfn "vse %A %A %A" name node refs
                    match node with
                    | Some node ->
                        refs <&!&> (fun ref -> validateSESpecific name logicalpath node ref.callSite ref.seParams)
                    | None -> OK
                scriptedEffects <&!&> (fun (name, lp, node, refs) -> validateSE name lp node refs)
            | _ -> OK
            )