namespace CWTools.Validation.Stellaris
open CWTools.Validation.ValidationCore
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser.Types
open CWTools.Process.STLScopes
open CWTools.Common
open CWTools.Common.STLConstants
open DotNet.Globbing
open CWTools.Games
open Newtonsoft.Json.Linq
open CWTools.Utilities.Utils
open System
open Microsoft.FSharp.Collections.Tagged
open System.Collections
open STLValidation


module Graphics =
    let valMeshFiles : FileValidator =
        fun rm es ->
            let pdxmesh = es.AllOfTypeChildren EntityType.GfxGfx
                            |> List.filter (fun e -> e.Key == "objectTypes")
                            |> List.collect (fun e -> e.Children |> List.choose (fun c -> if c.Key == "pdxmesh" then Some c else None))
            let filenames = rm.GetResources() |> List.choose (function |FileResource (f, _) -> Some f |EntityResource (f, _) -> Some f)
            let inner = 
                fun (x : Node) ->
                    match x.Leafs "file" |> Seq.tryHead with
                    |None -> OK
                    |Some fn -> 
                        let filename = fn.Value.ToRawString().Replace("/","\\")
                        match filenames |> List.exists (fun f -> f.EndsWith(filename)) with
                        | true -> OK
                        | false -> Invalid [inv (ErrorCodes.MissingFile (filename)) fn]
            pdxmesh <&!&> inner

    let valAssetFiles : FileValidator =
        fun rm es ->
            let os = EntitySet (rm.AllEntities())
            let pdxmesh =   os.AllOfTypeChildren EntityType.GfxGfx @
                            es.AllOfTypeChildren EntityType.GfxGfx
                            |> List.filter (fun e -> e.Key == "objectTypes")
                            |> List.collect (fun e -> e.Children |> List.choose (fun c -> if c.Key == "pdxmesh" then Some c else None))
            let names = pdxmesh |> List.map (fun m -> m.Tag "name") |> List.choose (fun t -> t |> Option.map(fun t2 -> t2.ToRawString()))
            let assets = es.AllOfTypeChildren EntityType.GfxAsset
            let inner =
                fun (x : Node) ->
                    match x.Leafs "pdxmesh" |> Seq.tryHead with
                    |None -> OK
                    |Some lv -> if names |> List.contains (lv.Value.ToRawString()) then OK else Invalid [inv (ErrorCodes.UndefinedPDXMesh (lv.Value.ToRawString())) lv]
            assets <&!&> inner

    let inline validateEntityCulture (entities : Collections.Set<string>) (cultures : (string * string) list) (entity : string) (leaf) (culture : string) =
        let secondKey = cultures |> List.tryPick (fun (c, f) -> if c == culture then Some f else None)
                                 |> Option.map (fun k -> k + "_" + entity)
        let firstkey = culture + "_" + entity
        match (entities.Contains entity) || (entities.Contains firstkey), secondKey with
        |true, _ -> OK
        |false, None ->
            Invalid [inv (ErrorCodes.UndefinedSectionEntity firstkey culture) leaf]
        |false, Some fallback ->
            if entities.Contains fallback then OK else Invalid [inv (ErrorCodes.UndefinedSectionEntityFallback firstkey fallback culture ) leaf]

    let inline validateEntity (entities : Collections.Set<string>) (entity : string) (node) =
        if entities.Contains entity then OK else Invalid [inv (ErrorCodes.UndefinedEntity entity) node]


    let getGraphicalCultures (es : STLEntitySet) =
        es.AllOfTypeChildren EntityType.GraphicalCulture
                        |> List.map (fun c -> c.Key, c.TagText "fallback")
        
    let valSectionGraphics : StructureValidator =
        fun os es ->
            let shipsizes = os.AllOfTypeChildren EntityType.ShipSizes
                            |> List.map (fun ss -> ss, ss.Key, (ss.Child "graphical_culture" |> Option.map (fun gc -> gc.LeafValues |> Seq.map (fun lv -> lv.Value.ToRawString()) |> List.ofSeq) |> Option.defaultValue []))
                            
            let sections = os.AllOfTypeChildren EntityType.SectionTemplates
            let cultures = getGraphicalCultures os
            let assets = os.AllOfTypeChildren EntityType.GfxAsset
                            |> List.map (fun a -> a.TagText "name")
                            |> Set.ofList
            let inner = 
                fun (s : Node) ->
                    match s.TagText "ship_size", s.Leafs "entity" |> Seq.tryHead with
                    |"", _ -> OK
                    |_, None -> OK
                    |shipsize, Some entity ->
                        match shipsizes |> List.tryFind (fun (_, n, _) -> n == shipsize) with
                        |None -> OK
                        |Some (_, _, shipsizeinfo) ->
                            shipsizeinfo <&!&> validateEntityCulture assets cultures (s.TagText "entity") entity
                            
            sections <&!&> inner
            <&&>
            (shipsizes <&!&> (fun (ss, n, c) -> c <&!&> validateEntityCulture assets cultures (n + "_entity") ss))

    let valComponentGraphics : StructureValidator =
        fun os es ->
            let shipsizes = os.AllOfTypeChildren EntityType.ShipSizes
                            |> List.map (fun ss -> ss, ss.Key, (ss.Child "graphical_culture" |> Option.map (fun gc -> gc.LeafValues |> Seq.map (fun lv -> lv.Value.ToRawString()) |> List.ofSeq) |> Option.defaultValue []))
                            
            let components = os.AllOfTypeChildren EntityType.ComponentTemplates
                                |> List.filter (fun c -> (c.Tag "hidden") |> Option.bind (function |Value.Bool x -> Some (not x) |_ -> Some true) |> Option.defaultValue true)
            let cultures = getGraphicalCultures os
            let assets = os.AllOfTypeChildren EntityType.GfxAsset
                            |> List.map (fun a -> a.TagText "name")
                            |> Set.ofList
            let inner = 
                fun (s : Node) ->
                    match s.Leafs "entity" |> Seq.tryHead with
                    |None -> OK
                    |Some entity ->
                        (cultures |> List.map fst) <&!&> validateEntityCulture assets cultures (s.TagText "entity") entity                            
            components <&!&> inner

    let validateAmbientGraphics : StructureValidator = 
        fun os es ->
            let assets = os.AllOfTypeChildren EntityType.GfxAsset
                        |> List.map (fun a -> a.TagText "name")
                        |> Set.ofList
            es.AllOfTypeChildren EntityType.AmbientObjects
            |> List.collect (fun ao -> ao.Leafs "entity" |> List.ofSeq)
            <&!&> (fun l -> validateEntity assets (l.Value.ToRawString()) l)
