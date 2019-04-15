namespace CWTools.Validation.Stellaris
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process
open CWTools.Parser.Types
open CWTools.Common
open CWTools.Common.STLConstants
open CWTools.Games
open CWTools.Utilities.Utils
open System
open FSharpx.Collections


module Graphics =
    let valMeshFiles : STLFileValidator =
        fun rm es ->
            let pdxmesh = es.AllOfTypeChildren EntityType.GfxGfx
                            |> List.filter (fun e -> e.Key == "objectTypes")
                            |> List.collect (fun e -> e.Children |> List.choose (fun c -> if c.Key == "pdxmesh" then Some c else None))
            let filenames = rm.GetFileNames()
            let inner =
                fun (x : Node) ->
                    match x.Leafs "file" |> Seq.tryHead with
                    |None -> OK
                    |Some fn ->
                        let filename = fn.Value.ToRawString().Replace("\\","/")
                        match filenames |> List.exists (fun f -> f.EndsWith(filename, StringComparison.Ordinal)) with
                        | true -> OK
                        | false -> Invalid [inv (ErrorCodes.MissingFile (filename)) fn]
            pdxmesh <&!&> inner

    let valAssetFiles : STLFileValidator =
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

    let inline validateEntityCulture (entities : Collections.Set<string>) (cultures : (string * string list) list) (entity : string) (leaf) (culture : string) =
        let secondKey = cultures |> List.tryPick (fun (c, f) -> if c == culture then Some f else None)
                                 |> Option.defaultValue []
                                 //|> Option.map (fun k -> k + "_" + entity)
        let firstkey = culture + "_" + entity
        let res = secondKey |> List.fold (fun (s : string option) c -> if s.IsNone || entities.Contains (c + "_" + entity) then None else Some c) (Some firstkey)
        match (entities.Contains entity) || (entities.Contains firstkey), res with
        |true, _ -> None
        |false, None -> None
            //Some (Invalid [inv (ErrorCodes.UndefinedSectionEntity firstkey culture) leaf])
        |false, Some fallback ->
            if entities.Contains fallback then None else Some (Invalid [inv (ErrorCodes.UndefinedSectionEntityFallback firstkey fallback culture ) leaf])

    let inline validateEntityCultures (entities : Collections.Set<string>) (allcultures : (string * string list) list) (entity : string) (leaf) (cultures : string list) =
        let errors = cultures |> List.choose (validateEntityCulture entities allcultures entity leaf)
        match errors with
        |[] -> OK
        |[x] -> x
        |[x1; x2] -> x1 <&&> x2
        |x1::x2::_ -> x1 <&&> x2 <&&> (Invalid [inv (ErrorCodes.CustomError "and more errors hidden" Severity.Error) leaf])

    let inline validateEntity (entities : Collections.Set<string>) (entity : string) (node) =
        if entities.Contains entity then OK else Invalid [inv (ErrorCodes.UndefinedEntity entity) node]


    let getGraphicalCultures (es : STLEntitySet) =
        es.AllOfTypeChildren EntityType.GraphicalCulture
                        |> List.map (fun c -> c.Key, c.TagText "fallback")

    let valSectionGraphics : STLStructureValidator =
        fun os es ->
            let shipsizes = os.AllOfTypeChildren EntityType.ShipSizes
                            |> List.map (fun ss -> ss, ss.Key, (ss.Child "graphical_culture" |> Option.map (fun gc -> gc.LeafValues |> Seq.map (fun lv -> lv.Value.ToRawString()) |> List.ofSeq) |> Option.defaultValue []))
            let shipsizesOV = os.AllOfTypeChildren EntityType.ShipSizes |> List.map (fun ss -> ss.Key)
            let shipsizesV = es.AllOfTypeChildren EntityType.ShipSizes
                            |> List.filter (fun ss -> not (ss.Has "entity"))
                            |> List.map (fun ss -> ss, ss.Key, (ss.Child "graphical_culture" |> Option.map (fun gc -> gc.LeafValues |> Seq.map (fun lv -> lv.Value.ToRawString()) |> List.ofSeq) |> Option.defaultValue []))
            let shipsizesEV = es.AllOfTypeChildren EntityType.ShipSizes
                            |> List.filter (fun ss -> (ss.Has "entity"))

            let sections = es.AllOfTypeChildren EntityType.SectionTemplates
            let cultures = getGraphicalCultures os
            let cultureMap = cultures |> List.filter (fun (_, f) -> f <> "") |> Map.ofList
            let cultures = cultures |> List.map (fun (c, f) -> c, Seq.unfold (fun nc -> cultureMap.TryFind nc |> Option.map (fun nf -> nf, nf)) c |> List.ofSeq)
            let assets = os.AllOfTypeChildren EntityType.GfxAsset
                            |> List.map (fun a -> a.TagText "name")
                            |> Set.ofList
            //log "assets %A" assets
            let inner =
                fun (s : Node) ->
                    match s.TagText "ship_size", s.Leafs "entity" |> Seq.tryHead with
                    |"", _ -> OK
                    |_, None -> OK
                    |shipsize, Some entity ->
                        match shipsizes |> List.tryFind (fun (_, n, _) -> n == shipsize) with
                        |None -> OK
                        |Some (_, _, shipsizeinfo) ->
                            shipsizeinfo |> validateEntityCultures assets cultures (s.TagText "entity") entity

            let shipsizeentities = shipsizesOV |> List.fold (fun (s : Collections.Set<string>) n -> s.Add (n + "_entity")) assets
            let ssinner =
                fun (s : Node) ->
                    s.Leafs "entity" <&!&> fun e -> if shipsizeentities |> Set.contains (e.Value.ToRawString()) then OK else Invalid [inv (ErrorCodes.UndefinedEntity (e.Value.ToRawString())) e]
            sections <&!&> inner
            <&&>
            (shipsizesV <&!&> (fun (ss, n, c) -> c |> validateEntityCultures assets cultures (n + "_entity") ss))
            <&&>
            (shipsizesEV <&!&> ssinner)

    let valComponentGraphics : STLStructureValidator =
        fun os es ->
            let components = es.AllOfTypeChildren EntityType.ComponentTemplates
                                |> List.filter (fun c -> (c.Tag "hidden") |> Option.bind (function |Value.Bool x -> Some (not x) |_ -> Some true) |> Option.defaultValue true)
            let cultures = getGraphicalCultures os
            let cultureMap = cultures |> List.filter (fun (_, f) -> f <> "") |> Map.ofList
            let cultures = cultures |> List.map (fun (c, f) -> c, Seq.unfold (fun nc -> cultureMap.TryFind nc |> Option.map (fun nf -> nf, nf)) c |> List.ofSeq)
            // let assets = os.AllOfTypeChildren EntityType.GfxAsset
            //                 |> List.map (fun a -> a.TagText "name")
            //                 |> Set.ofList
            let assetGenerator (e : Entity) : obj list =
                if e.entityType = EntityType.GfxAsset then e.entity.Children |> List.map (fun a -> upcast a.TagText "name") else []
            let assets = os.AddOrGetCached "componentgraphicsassets" assetGenerator |> List.map (fun s -> s :?> string) |> Set.ofList
            let inner =
                fun (s : Node) ->
                    match s.Leafs "entity" |> Seq.tryHead with
                    |None -> OK
                    |Some entity ->
                        (cultures |> List.map fst) |> validateEntityCultures assets cultures (s.TagText "entity") entity
            components <&!&> inner

    let valMegastructureGraphics : STLStructureValidator =
        fun os es ->
            let megastructures = es.AllOfTypeChildren EntityType.Megastructures
                                    //|> List.collect (fun a -> a.Leafs "entity" |> List.ofSeq)
            let cultures = getGraphicalCultures os
            let cultureMap = cultures |> List.filter (fun (_, f) -> f <> "") |> Map.ofList
            let cultures = cultures |> List.map (fun (c, f) -> c, Seq.unfold (fun nc -> cultureMap.TryFind nc |> Option.map (fun nf -> nf, nf)) c |> List.ofSeq)
            let assets = os.AllOfTypeChildren EntityType.GfxAsset
                            |> List.map (fun a -> a.TagText "name")
                            |> Set.ofList
            let inner =
                fun (m : Node) ->
                    let check =
                        fun (e : Leaf) ->
                            (cultures |> List.map fst) |> validateEntityCultures assets cultures (e.Value.ToRawString()) m
                    m.Leafs "entity" <&!&> check
                    <&&>
                    (m.Leafs "construction_entity" <&!&> check)

            megastructures <&!&> inner

    let valPlanetClassGraphics : STLStructureValidator =
        fun os es ->
            let assets = os.AllOfTypeChildren EntityType.GfxAsset
                        |> List.map (fun a -> a.TagText "name")
                        |> Set.ofList
            es.AllOfTypeChildren EntityType.PlanetClasses
            |> List.collect (fun ao -> ao.Leafs "entity" |> List.ofSeq)
            <&!&> (fun l -> if assets.Contains (l.Value.ToRawString()) || assets.Contains (l.Value.ToRawString() + "_01_entity") then OK else Invalid [inv (ErrorCodes.UndefinedEntity (l.Value.ToRawString())) l])
    let validateAmbientGraphics : STLStructureValidator =
        fun os es ->
            let assets = os.AllOfTypeChildren EntityType.GfxAsset
                        |> List.map (fun a -> a.TagText "name")
                        |> Set.ofList
            es.AllOfTypeChildren EntityType.AmbientObjects
            |> List.collect (fun ao -> ao.Leafs "entity" |> List.ofSeq)
            <&!&> (fun l -> validateEntity assets (l.Value.ToRawString()) l)
    let valIconLeaf (sprites : Collections.Set<string>) (leaf : Leaf) =
        if Set.contains (leaf.Value.ToRawString()) sprites then OK else Invalid [inv (ErrorCodes.SpriteMissing (leaf.Value.ToString())) leaf]

    let valIcon (sprites : Collections.Set<string>) (key : string) (node : Node) =
        let results =
            match node.Leafs key |> List.ofSeq with
            | [] -> OK
            | xs ->
                xs <&!&> valIconLeaf sprites
        results

    type gfxFolders =
    |Buildings

    let doesFileExist (files : Collections.Set<string>) (folder : gfxFolders) (file : string) =
        let path =
            match folder with
            |Buildings -> "gfx/interface/icons/buildings/" + file + ".dds"
        //log "%A" files
        files |> Set.contains path

    let valIconWithInterface (sprites : Collections.Set<string>) (files : Collections.Set<string>) (folder : gfxFolders) (key : string) (node : Node) =
        let inner =
            fun (leaf : Leaf) ->
                let value = leaf.Value.ToRawString()
                if doesFileExist files folder value then OK
                else valIconLeaf sprites leaf
        node.Leafs key <&!&> inner

    let valComponentIcons : STLFileValidator =
        //let spriteKeys = ["spriteType"; "portraitType"; "corneredTileSpriteType"; "flagSpriteType"]
        fun res es ->
            let os = EntitySet (res.AllEntities())
            let files = res.GetFileNames() |> Set.ofList
            // log "%A" files
            // files |> Set.filter (fun s -> s.Contains("great")) |> log "%A"
            let spriteGenerator (e : Entity) : obj list =
                if e.entityType = EntityType.Interface
                then e.entity.Children |> List.filter (fun e -> e.Key = "spriteTypes")
                      |> Seq.collect (fun es -> es.Children |> Seq.collect (fun e -> (e.TagsText "name")) )
                      |> List.ofSeq
                      |> List.map (fun s -> upcast s)
                else []
            // let sprites = os.AllOfTypeChildren EntityType.Interface //os.GlobMatchChildren("**/interface/*.gfx") @ os.GlobMatchChildren("**/interface/**/*.gfx")
            //                 |> List.filter (fun e -> e.Key = "spriteTypes")
            //                 |> List.collect (fun e -> e.Children)
            // let spriteNames = sprites |> Seq.collect (fun s -> s.TagsText "name") |> Set.ofSeq
            let spriteNames = os.AddOrGetCached "componenticonsspritenames" spriteGenerator |> List.map (fun s -> s :?> string) |> Set.ofList
            let components = es.AllOfTypeChildren EntityType.ComponentTemplates
            let componentsets = es.AllOfTypeChildren EntityType.ComponentSets
                                |> List.filter (fun cs -> cs.Tag "required_component_set" |> Option.map (function |Value.Bool b -> not b |_ -> false) |> Option.defaultValue false)
            // let fNode = (fun (x : Node) children ->
            //                 let results =
            //                     match x.Leafs "icon" |> List.ofSeq with
            //                     | [] -> OK
            //                     | xs ->
            //                         xs <&!&> (fun e -> if List.contains (e.Value.ToRawString()) spriteNames then OK else Invalid [inv (ErrorCodes.SpriteMissing (e.Value.ToString())) e])
            //                 results <&&> children
            //                     )
            // let fCombine = (<&&>)
            components <&!&> valIcon spriteNames "icon"
            <&&>
            (componentsets <&!&> valIcon spriteNames "icon")
            <&&>
            (es.AllOfTypeChildren EntityType.Buildings <&!&> valIconWithInterface spriteNames files Buildings "icon")
            <&&>
            (es.AllOfTypeChildren EntityType.MapModes <&!&> valIcon spriteNames "icon")
            <&&>
            (es.AllOfTypeChildren EntityType.StarbaseBuilding <&!&> valIcon spriteNames "icon")
            <&&>
            (es.AllOfTypeChildren EntityType.StarbaseModules <&!&> valIcon spriteNames "icon")