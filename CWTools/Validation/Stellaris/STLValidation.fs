namespace CWTools.Validation.Stellaris
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser.Types
open CWTools.Common
open CWTools.Common.STLConstants
open CWTools.Games
open CWTools.Utilities.Utils
open System
open CWTools.Games.Stellaris.STLLookup
open CWTools.Process.Scopes
open FSharpx.Collections
open CWTools.Common.NewScope


module STLValidation =
    type S = Severity
    let shipName (ship : Ship) = if ship.Name = "" then Invalid (Guid.NewGuid(), [(inv (ErrorCodes.CustomError "must have name" Severity.Error) ship)]) else OK
    let shipSize (ship : Ship) = if ship.ShipSize = "" then Invalid (Guid.NewGuid(), [(inv (ErrorCodes.CustomError "must have size" Severity.Error) ship)]) else OK

    // let validateShip : Validator<Ship>  = shipName <&> shipSize

    let validateShips : STLStructureValidator =
        fun _ es ->
            let ships =
                es.All |> List.collect (fun n -> n.Children)
                   |> List.choose (function | :? Ship as s -> Some s |_ -> None)
            ships <&!&> (fun s -> (shipName <&> shipSize) s)

    let getDefinedVariables (node : Node) =
        let fNode = (fun (x:Node) acc ->
                        x.Values |> List.fold (fun a n -> if n.Key.StartsWith("@", StringComparison.OrdinalIgnoreCase) then n.Key::a else a) acc
                        )
        node |> (foldNode7 fNode) |> List.ofSeq

    let checkUsedVariables (node : Node) (variables : string list) =
        let fNode = (fun (x:Node) children ->
                        let values = x.Values |> List.choose (fun v -> match v.Value with |String s when s.StartsWith("@", StringComparison.OrdinalIgnoreCase) -> Some v |_ -> None)
                        match values with
                        | [] -> children
                        | x ->
                            x |> List.map ((fun f -> f, f.Value.ToString()) >> (fun (l, v) -> if variables |> List.contains v then OK else Invalid (Guid.NewGuid(), [inv (ErrorCodes.UndefinedVariable v) l])))
                              |> List.fold (<&&>) children)
        let fCombine = (<&&>)
        node |> (foldNode2 fNode fCombine OK)



    let validateVariables : STLStructureValidator =
        fun os es ->
            let globalVars = os.GlobMatch("**/common/scripted_variables/*.txt") @ es.GlobMatch("**/common/scripted_variables/*.txt")
                            |> List.map getDefinedVariables
                            |> Seq.collect id |> List.ofSeq
            es.All <&!!&>
            // let x =
            //     es.All
            //     |> List.map
                    (fun node ->
                        let defined = getDefinedVariables node
                        let errors = checkUsedVariables node (defined @ globalVars)
                        errors
                    )
            //x |> List.fold (<&&>) OK

    /// Make sure an event either has a mean_time_to_happen or is stopped from checking all the time
    /// Not mandatory, but performance reasons, suggested by Caligula
    /// Check "mean_time_to_happen", "is_triggered_only", "fire_only_once" and "trigger = { always = no }".
    /// Create issue if none are true
    let validateEventValsInternal (event : Node) =
        let isMTTH = event.Has "mean_time_to_happen"
        let isTrig = event.Has "is_triggered_only"
        let isOnce = event.Has "fire_only_once"
        let isAlwaysNo =
            match event.Child "trigger" with
            | Some t ->
                match t.Tag "always" with
                | Some (Bool b) when b = false -> true
                | _ -> false
            | None -> false
        let e =
            match isMTTH || isTrig || isOnce || isAlwaysNo with
            | false -> Invalid (Guid.NewGuid(), [inv ErrorCodes.EventEveryTick event])
            | true -> OK
        e
    let validateEvents : STLStructureValidator =
        fun _ es ->
            let ships =
                es.AllOfTypeChildren EntityType.Events
            ships <&!&> (fun s -> (validateEventValsInternal) s)
    let stellarisEventPreTriggers =
        [|
            "has_owner"
            "is_homeworld"
            "original_owner"
            "is_ai"
            "has_ground_combat"
            "is_capital"
            "is_occupied_flag"
        |]
    let stellarisPopJobPreTriggers =
        [|
            "has_owner"
            "is_enslaved"
            "is_being_purged"
            "is_being_assimilated"
            "has_planet"
            "is_sapient"
        |]

    let validatePreTriggers : STLStructureValidator =
        fun _ es ->
            let events = es.AllOfTypeChildren EntityType.Events |> List.filter (fun e -> e.Key == "planet_event")
            let eventToErrors (event : Node) =
                match event.Child "trigger" with
                    | None -> OK
                    | Some trigger ->
                        trigger.Leaves <&!&> (fun l ->
                                                         if Array.contains l.Key stellarisEventPreTriggers
                                                         then Invalid (Guid.NewGuid(), [inv (ErrorCodes.PossiblePretrigger l.Key) l])
                                                         else OK)
            let popjobs = es.GlobMatchChildren "**/common/pop_jobs/*.txt"
            let popjobToError (popjob : Node) =
                match popjob.Child "possible" with
                    | None -> OK
                    | Some possible ->
                        possible.Leaves <&!&> (fun l ->
                                                         if Array.contains l.Key stellarisPopJobPreTriggers
                                                         then Invalid (Guid.NewGuid(), [inv (ErrorCodes.PossiblePretrigger l.Key) l])
                                                         else OK)

            (events <&!&> eventToErrors) <&&> (popjobs <&!&> popjobToError)

    let valResearchLeader (area : string) (cat : string option) (node : Node) =
        let fNode = (fun (x:Node) children ->
                        let results =
                            match x.Key with
                            | "research_leader" ->
                                match x.TagText "area" with
                                | "" -> Invalid (Guid.NewGuid(), [inv ErrorCodes.ResearchLeaderArea x])
                                | area2 when area <> area2 -> Invalid (Guid.NewGuid(), [inv (ErrorCodes.ResearchLeaderTech area area2) x])
                                | _ -> OK
                                /// These aren't really required
                                // <&&>
                                // match cat, x.TagText "has_trait" with
                                // | None, _ -> OK
                                // | _, "" -> Invalid (Guid.NewGuid(), [inv S.Error x "This research_leader is missing required \"has_trait\""])
                                // | Some c, t when ("leader_trait_expertise_" + c) <> t -> Invalid (Guid.NewGuid(), [inv S.Warning x "This research_leader has the wrong expertise"])
                                // | _ -> OK
                            | _ -> OK
                        results <&&> children)
        let fCombine = (<&&>)
        node |> (foldNode2 fNode fCombine OK)

    let valTechnology : STLStructureValidator =
        fun _ es ->
            let techs = es.GlobMatchChildren("**/common/technology/*.txt")
            let inner =
                fun (node : Node) ->
                    let area = node.TagText "area"
                    let cat = node.Child "category" |> Option.bind (fun c -> c.All |> List.tryPick (function |LeafValueC lv -> Some (lv.Value.ToString()) |_ -> None))
                    let catres =
                        match cat with
                        | None -> Invalid (Guid.NewGuid(), [inv ErrorCodes.TechCatMissing node])
                        | Some _ -> OK
                    catres <&&> valResearchLeader area cat node
            techs <&!&> inner
            //techs |> List.map inner |> List.fold (<&&>) OK

    let valButtonEffects : STLStructureValidator =
        fun os es ->
            let effects = (os.GlobMatchChildren("**/common/button_effects/*.txt"))
                            |> List.map (fun e -> e.Key)
            let buttons = es.AllOfTypeChildren EntityType.Interface
            let fNode = (fun (x : Node) children ->
                            let results =
                                match x.Key with
                                | "effectButtonType" ->
                                    x.Leafs "effect" <&!&> (fun e -> if List.contains (e.Value.ToRawString()) effects then OK else Invalid (Guid.NewGuid(), [inv (ErrorCodes.ButtonEffectMissing (e.Value.ToString())) e]))
                                | _ -> OK
                            results <&&> children
                                )
            let fCombine = (<&&>)
            buttons <&!&> (foldNode2 fNode fCombine OK)

    let valSprites : STLStructureValidator =
        //let spriteKeys = ["spriteType"; "portraitType"; "corneredTileSpriteType"; "flagSpriteType"]
        fun os es ->
            let sprites = os.GlobMatchChildren("**/interface/*.gfx") @ os.GlobMatchChildren("**/interface/*/*.gfx")
                            |> List.filter (fun e -> e.Key = "spriteTypes")
                            |> List.collect (fun e -> e.Children)
            let spriteNames = sprites |> Seq.collect (fun s -> s.TagsText "name") |> List.ofSeq
            let gui = es.GlobMatchChildren("**/interface/*.gui") @ es.GlobMatchChildren("**/interface/*/*.gui")
            let fNode = (fun (x : Node) children ->
                            let results =
                                match x.Leafs "spriteType" |> List.ofSeq with
                                | [] -> OK
                                | xs ->
                                    xs <&!&> (fun e -> if List.contains (e.Value.ToRawString()) spriteNames then OK else Invalid (Guid.NewGuid(), [inv (ErrorCodes.SpriteMissing (e.Value.ToString())) e]))
                            results <&&> children
                                )
            let fCombine = (<&&>)
            gui <&!&> (foldNode2 fNode fCombine OK)

    let valSpriteFiles : STLFileValidator =
        fun rm es ->
            let sprites = es.AllOfTypeChildren EntityType.Interface // es.GlobMatchChildren("**/interface/*.gfx") @ es.GlobMatchChildren("**/interface/*/*.gfx")
                            |> List.filter (fun e -> e.Key = "spriteTypes")
                            |> List.collect (fun e -> e.Children)
            let filenames = rm.GetFileNames()
            //log "sprite filename %A" filenames
            let inner =
                fun (x : Node) ->
                   Seq.append (x.Leafs "textureFile") (x.Leafs "effectFile")
                    <&!&> (fun l ->
                        let filename = l.Value.ToRawString().Replace("\\","/")
                        let filenamefallback = filename.Replace(".lua",".shader").Replace(".tga",".dds")
                        match filenames |> List.exists (fun f -> f.EndsWith(filename, StringComparison.Ordinal ) || f.EndsWith(filenamefallback, StringComparison.Ordinal)) with
                        | true -> OK
                        | false -> Invalid (Guid.NewGuid(), [inv (ErrorCodes.MissingFile (l.Value.ToRawString())) l]))
            sprites <&!&> inner

    let addGeneratedModifiers (modifiers : ActualModifier list) (es : STLEntitySet) : ActualModifier list =
        let ships = es.GlobMatchChildren("**/common/ship_sizes/*.txt")
        let shipKeys = ships |> List.map (fun f -> f.Key)
        let shipModifierCreate =
            (fun k ->
            [
                {ActualModifier.tag = "shipsize_"+k+"_build_speed_mult"; category = modifierCategoryManager.ParseModifier() "Starbase" }
                // {tag = "shipsize_"+k+"_build_cost_mult"; category = modifierCategoryManager.ParseModifier() "Starbase" }
                {tag = "shipsize_"+k+"_hull_mult"; category = modifierCategoryManager.ParseModifier() "Ship" }
                {tag = "shipsize_"+k+"_hull_add"; category = modifierCategoryManager.ParseModifier() "Ship" }
                // {tag = "shipsize_"+k+"_damage_mult"; category = modifierCategoryManager.ParseModifier() "Ship" }
                // {tag = "shipsize_"+k+"_evasion_addt"; category = modifierCategoryManager.ParseModifier() "Ship" }
                // {tag = "shipsize_"+k+"_disengage_mult"; category = modifierCategoryManager.ParseModifier() "Ship" }
            ])
        let shipModifiers = shipKeys |> List.collect shipModifierCreate
        let weaponTags = es.GlobMatch("**/common/component_tags/*.txt") |> List.collect (fun f -> f.LeafValues |> List.ofSeq)
        let weaponTagsModifierCreate =
            (fun k ->
            [
                {ActualModifier.tag = k+"_weapon_damage_mult"; category = modifierCategoryManager.ParseModifier() "Ship" }
                {tag = k+"_weapon_fire_rate_mult"; category = modifierCategoryManager.ParseModifier() "Ship" }
                {tag = k+"_speed_mult"; category = modifierCategoryManager.ParseModifier() "Ship" }
            ])
        let weaponModifiers = weaponTags |> List.map (fun l -> l.Value.ToRawString())
                                             |> List.collect weaponTagsModifierCreate
        let econCategoryTriggeredModifierCreate (modType : string) (res : string) (node : Node) =
            let triggeredCat = node.TagText "key"
            seq {
                match node.Child "modifier_types" with
                | Some mt ->
                        if mt.LeafValues |> Seq.exists (fun lv -> lv.Key == "mult") then
                            yield {ActualModifier.tag = triggeredCat+res+modType+"_mult"; category = modifierCategoryManager.ParseModifier() "Resource" }
                        else ()
                        if mt.LeafValues |> Seq.exists (fun lv -> lv.Key == "add") then
                            yield {ActualModifier.tag = triggeredCat+res+modType+"_add"; category = modifierCategoryManager.ParseModifier() "Resource" }
                        else ()
                | None -> ()
            }

        let econCategoryModifierCreateGenerator (node : Node) =
            let econCat = node.Key
            (fun res ->
                [
                    match node.Child "generate_mult_modifiers" with
                    | Some gen ->
                        if gen.LeafValues |> Seq.exists (fun lv -> lv.Key == "cost") then
                            yield {ActualModifier.tag = econCat+res+"_cost_mult"; category = modifierCategoryManager.ParseModifier() "Resource" }
                        else ()
                        if gen.LeafValues |> Seq.exists (fun lv -> lv.Key == "produces") then
                            yield {tag = econCat+res+"_produces_mult"; category = modifierCategoryManager.ParseModifier() "Resource" }
                        else ()
                        if gen.LeafValues |> Seq.exists (fun lv -> lv.Key == "upkeep") then
                            yield {tag = econCat+res+"_upkeep_mult"; category = modifierCategoryManager.ParseModifier() "Resource" }
                        else ()
                    | None -> ()
                    match node.Child "generate_add_modifiers" with
                    | Some gen ->
                        if gen.LeafValues |> Seq.exists (fun lv -> lv.Key == "cost") then
                            yield {ActualModifier.tag = econCat+res+"_cost_add"; category = modifierCategoryManager.ParseModifier() "Resource" }
                        else ()
                        if gen.LeafValues |> Seq.exists (fun lv -> lv.Key == "produces") then
                            yield {ActualModifier.tag = econCat+res+"_produces_add"; category = modifierCategoryManager.ParseModifier() "Resource" }
                        else ()
                        if gen.LeafValues |> Seq.exists (fun lv -> lv.Key == "upkeep") then
                            yield {ActualModifier.tag = econCat+res+"_upkeep_add"; category = modifierCategoryManager.ParseModifier() "Resource" }
                        else ()
                    | None -> ()
                    yield! (node.Childs "triggered_upkeep_modifier" |> Seq.collect (econCategoryTriggeredModifierCreate "_upkeep" res))
                    yield! (node.Childs "triggered_cost_modifier" |> Seq.collect (econCategoryTriggeredModifierCreate "_cost" res))
                    yield! (node.Childs "triggered_produces_modifier" |> Seq.collect (econCategoryTriggeredModifierCreate "_produces" res))
                ]
            )
        let economicCategories = es.GlobMatchChildren("**/common/economic_categories/*.txt")
        let baseEconCategoryModifiersCreates = economicCategories |> List.map econCategoryModifierCreateGenerator
        let stratres = es.GlobMatchChildren("**/common/strategic_resources/*.txt")
        let srKeys = stratres |> List.map (fun f -> f.Key)
        let srModifierCreate =
            (fun k ->
            [
                yield {ActualModifier.tag = "country_resource_max_"+k+"_add"; category = modifierCategoryManager.ParseModifier() "Country" }
                yield! (baseEconCategoryModifiersCreates |> List.collect (fun f -> f (k)))
                yield! (baseEconCategoryModifiersCreates |> List.collect (fun f -> f ("_"+k)))
            ])

        let srModifiers = (baseEconCategoryModifiersCreates |> List.collect (fun f -> f "")) @ (srKeys |> List.collect srModifierCreate)

        let pop_cats = es.GlobMatchChildren("**/common/pop_categories/*.txt") |> List.map (fun f -> f.Key)
        let popCatModifierCreate =
            (fun k ->
                [
                    {ActualModifier.tag = "pop_cat_" + k + "_happiness"; category = modifierCategoryManager.ParseModifier() "Pop"}
                    {tag = "pop_cat_" + k + "_political_power"; category = modifierCategoryManager.ParseModifier() "Pop"}
                ])
        let popCatModifiers = pop_cats |> List.collect popCatModifierCreate

        let jobs = es.GlobMatchChildren("**/common/pop_jobs/*.txt") |> List.map (fun f -> f.Key)
        let jobModifierCreate =
            (fun k ->
                [
                    {ActualModifier.tag = "job_" + k + "_add"; category = modifierCategoryManager.ParseModifier() "Planet"}
                    {tag = "job_" + k + "_per_pop"; category = modifierCategoryManager.ParseModifier() "Planet"}
                    {tag = "job_" + k + "_per_crime"; category = modifierCategoryManager.ParseModifier() "Planet"}
                ])
        let jobModifiers = jobs |> List.collect jobModifierCreate

        let planetclasses = es.GlobMatchChildren("**/common/planet_classes/*.txt")
        let pcKeys = planetclasses |> List.map (fun f -> f.Key)
        let pcModifiers = pcKeys |> List.map (fun k -> {ActualModifier.tag = k+"_habitability"; category = modifierCategoryManager.ParseModifier() "PlanetClass"})
        let buildingTags = es.GlobMatch("**/common/building_tags/*.txt") |> List.collect (fun f -> f.LeafValues |> List.ofSeq)
        let buildingTagModifierCreate =
            (fun k ->
            [
                {ActualModifier.tag = k+"_construction_speed_mult"; category = modifierCategoryManager.ParseModifier() "Planet" }
                {tag = k+"_build_cost_mult"; category = modifierCategoryManager.ParseModifier() "Planet" }
            ])
        let buildingModifiers = buildingTags |> List.map (fun l -> l.Value.ToRawString())
                                             |> List.collect buildingTagModifierCreate
        let buildingWithModCap = es.GlobMatchChildren("**/common/buildings/*.txt") |> List.filter (fun n -> n.TagText "is_capped_by_modifier" == "yes")
        let buildingWithModCapCreate =
            (fun k ->
            [
                {ActualModifier.tag = k+"_max"; category = modifierCategoryManager.ParseModifier() "Planet" }
            ])
        let buildingWithModCapModifiers = buildingWithModCap |> List.map (fun n -> n.Key)
                                                             |> List.collect buildingWithModCapCreate
        let countryTypeKeys = es.GlobMatchChildren("**/common/country_types/*.txt") |> List.map (fun f -> f.Key)
        let countryTypeModifiers = countryTypeKeys |> List.map (fun k -> {ActualModifier.tag = "damage_vs_country_type_"+k+"_mult"; category = modifierCategoryManager.ParseModifier() "Ship"})
        let speciesKeys = es.GlobMatchChildren("**/common/species_archetypes/*.txt")
                            //|> List.filter (fun s -> not (s.Has "inherit_traits_from"))
                            |> List.map (fun s -> s.Key)
        let speciesModifiers = speciesKeys |> List.map (fun k -> {ActualModifier.tag = k+"_species_trait_points_add"; category = modifierCategoryManager.ParseModifier() "Country"})
        let districts = es.GlobMatchChildren("**/common/districts/*.txt") |> List.filter (fun d -> not (d.TagText "is_capped_by_modifier" == "no"))
        let districtModifiers = districts |> List.map (fun k -> {ActualModifier.tag = k.Key+"_max"; category = modifierCategoryManager.ParseModifier() "Planet"})
        let popEthicKeys = es.GlobMatchChildren("**/common/ethics/*.txt") |> List.map (fun s -> s.Key)
        let popEthicModifiers = popEthicKeys |> List.map (fun k -> { ActualModifier.tag = "pop_" + k + "_attraction_mult"; category = modifierCategoryManager.ParseModifier() "Pop"})
        let techCategoryKeys = es.GlobMatchChildren("**/common/technology/category/*.txt") |> List.map (fun s -> s.Key)
        let techCatModifiers = techCategoryKeys |> List.map (fun k -> { ActualModifier.tag = "category_"+  k + "_research_speed_mult"; category = modifierCategoryManager.ParseModifier() "Country"})
        shipModifiers @  weaponModifiers @ srModifiers @ popCatModifiers @ jobModifiers @ pcModifiers @ buildingModifiers @ countryTypeModifiers @ speciesModifiers @ modifiers @ buildingWithModCapModifiers
                    @ districtModifiers @ popEthicModifiers @ techCatModifiers

    let getTechnologies (es : STLEntitySet) =
        let techs = es.AllOfTypeChildren EntityType.Technology
        let inner =
            fun (t : Node) ->
                let name = t.Key
                let prereqs = t.Child "prerequisites" |> Option.map (fun c -> c.LeafValues |> Seq.toList |> List.map (fun v -> v.Value.ToRawString()))
                                                      |> Option.defaultValue []
                name, prereqs
        techs |> List.map inner



    let validateTechnologies : STLStructureValidator =
        fun os es ->
            // let timer = new System.Diagnostics.Stopwatch()
            // timer.Start()
            let getPrereqs (b : Node) : obj list =
                match b.Child "prerequisites" with
                |None -> []
                |Some p ->
                    p.LeafValues |> List.ofSeq |> List.map (fun lv -> upcast lv.Value.ToRawString())
            // let getPrereqsPar lists = lists |> PSeq.collect (fun ns -> List.collect getPrereqs ns) |> List.ofSeq
            let getPrereqGen (e : Entity) : obj list =
                if List.contains e.entityType [EntityType.Buildings; EntityType.ShipSizes; EntityType.SectionTemplates; EntityType.ComponentTemplates;
                                                EntityType.StrategicResources; EntityType.Armies; EntityType.Edicts; EntityType.TileBlockers; EntityType.Deposits; EntityType.Decisions; EntityType.Traits]
                then List.collect getPrereqs (e.entity.Children)
                else []

            // let buildingPrereqs = os.AllOfTypeChildren EntityType.Buildings @ es.AllOfTypeChildren EntityType.Buildings// |> List.collect getPrereqs
            // let shipsizePrereqs = os.AllOfTypeChildren EntityType.ShipSizes @ es.AllOfTypeChildren EntityType.ShipSizes// |> List.collect getPrereqs
            // let sectPrereqs = os.AllOfTypeChildren EntityType.SectionTemplates @ es.AllOfTypeChildren EntityType.SectionTemplates// |> List.collect getPrereqs
            // let compPrereqs = os.AllOfTypeChildren EntityType.ComponentTemplates @ es.AllOfTypeChildren EntityType.ComponentTemplates// |> List.collect getPrereqs
            // let stratResPrereqs = os.AllOfTypeChildren EntityType.StrategicResources @ es.AllOfTypeChildren EntityType.StrategicResources// |> List.collect getPrereqs
            // let armyPrereqs = os.AllOfTypeChildren EntityType.Armies @ es.AllOfTypeChildren EntityType.Armies// |> List.collect getPrereqs
            // let edictPrereqs = os.AllOfTypeChildren EntityType.Edicts @ es.AllOfTypeChildren EntityType.Edicts// |> List.collect getPrereqs
            // let tileBlockPrereqs = os.AllOfTypeChildren EntityType.TileBlockers @ es.AllOfTypeChildren EntityType.TileBlockers// |> List.collect getPrereqs
            // let allPrereqs = getPrereqsPar [buildingPrereqs; shipsizePrereqs;sectPrereqs; compPrereqs; stratResPrereqs; armyPrereqs; edictPrereqs; tileBlockPrereqs; ] |> Set.ofList
            // log "Tech validator time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let allPrereqs = os.AddOrGetCached "techprereqs" getPrereqGen |> List.map (fun s -> s :?> string) |> Set.ofList
            let hastechs = (es.AllWithData |> List.collect (fun (_, d) -> d.Force().Hastechs)) @ (os.AllWithData |> List.collect (fun (_, d) -> d.Force().Hastechs))
            // log "Tech validator time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let allPrereqs = (hastechs) |> List.fold (fun (set : Collections.Set<string>) key -> set.Add key) allPrereqs
            //let allPrereqs = buildingPrereqs @ shipsizePrereqs @ sectPrereqs @ compPrereqs @ stratResPrereqs @ armyPrereqs @ edictPrereqs @ tileBlockPrereqs @ getAllTechPreqreqs os @ getAllTechPreqreqs es |> Set.ofList
            let techList = getTechnologies os @ getTechnologies es
            // log "Tech validator time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let techPrereqs = techList |> List.collect snd |> Set.ofList
            // log "Tech validator time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let techChildren = techList |> List.map (fun (name, _) -> name, Set.contains name techPrereqs)
            // let techChildren = getTechnologies os @ getTechnologies es
            //                     |> (fun l -> l |> List.map (fun (name, _) -> name, l |> List.exists (fun (_, ts2) -> ts2 |> List.contains name)))
                                |> List.filter snd
                                |> List.map fst
                                |> Set.ofList

            // log "Tech validator time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let techs = es.AllOfTypeChildren EntityType.Technology
            // log "Tech validator time: %i" timer.ElapsedMilliseconds; timer.Restart()
            let inner (t : Node) =
                let isPreReq = t.Has "prereqfor_desc"
                let isMod = t.Has "modifier"
                let hasChildren = techChildren |> Set.contains t.Key
                let isUsedElsewhere = allPrereqs |> Set.contains t.Key
                let isWeightZero = t.Tag "weight" |> (function |Some (Value.Int 0) -> true |_ -> false)
                let isWeightFactorZero = t.Child "weight_modifier" |> Option.map (fun wm -> wm.Tag "factor" |> (function |Some (Value.Float 0.00m) -> true |_ -> false)) |> Option.defaultValue false
                let hasFeatureFlag = t.Has "feature_flags"
                if isPreReq || isMod || hasChildren || isUsedElsewhere || isWeightZero || isWeightFactorZero || hasFeatureFlag then OK else Invalid (Guid.NewGuid(), [inv (ErrorCodes.UnusedTech (t.Key)) t])
            let res = techs <&!&> inner
            // log "Tech validator time: %i" timer.ElapsedMilliseconds; timer.Restart()
            res



    let validateShipDesigns : STLStructureValidator =
        fun os es ->
            let ship_designs = es.AllOfTypeChildren EntityType.GlobalShipDesigns
            let section_templates = os.AllOfTypeChildren EntityType.SectionTemplates @ es.AllOfTypeChildren EntityType.SectionTemplates
            let weapons = os.AllOfTypeChildren EntityType.ComponentTemplates @ es.AllOfTypeChildren EntityType.ComponentTemplates
            let getWeaponInfo (w : Node) =
                match w.Key with
                | "weapon_component_template" ->
                    Some (w.TagText "key", ("weapon", w.TagText "size"))
                | "strike_craft_component_template" ->
                    Some (w.TagText "key", ("strike_craft", w.TagText "size"))
                | "utility_component_template" ->
                    Some (w.TagText "key", ("utility", w.TagText "size"))
                | _ -> None
            let weaponInfo = weapons |> List.choose getWeaponInfo |> Map.ofList
            let getSectionInfo (s : Node) =
                match s.Key with
                | "ship_section_template" ->
                    let inner (n : Node) =
                        n.TagText "name", (n.TagText "slot_type", n.TagText "slot_size")
                    let component_slots = s.Childs "component_slot" |> List.ofSeq |> List.map inner
                    let createUtilSlot (prefix : string) (size : string) (i : int) =
                        List.init i (fun i -> prefix + sprintf "%i" (i+1), ("utility", size))
                    let smalls = s.Tag "small_utility_slots" |> (function |Some (Value.Int i) -> createUtilSlot "SMALL_UTILITY_" "small" i |_ -> [])
                    let med = s.Tag "medium_utility_slots" |> (function |Some (Value.Int i) -> createUtilSlot "MEDIUM_UTILITY_" "medium" i |_ -> [])
                    let large = s.Tag "large_utility_slots" |> (function |Some (Value.Int i) -> createUtilSlot "LARGE_UTILITY_" "large" i |_ -> [])
                    let aux = s.Tag "aux_utility_slots" |> (function |Some (Value.Int i) -> createUtilSlot "AUX_UTILITY_" "aux" i |_ -> [])
                    let all = (component_slots @ smalls @ med @ large @ aux ) |> Map.ofList
                    Some (s.TagText "key", all)
                | _ -> None
            let sectionInfo = section_templates |> List.choose getSectionInfo |> Map.ofList

            let validateComponent (section : string) (sectionMap : Collections.Map<string, (string * string)>) (c : Node) =
                let slot = c.TagText "slot"
                let slotFound = sectionMap |> Map.tryFind slot
                let template = c.TagText "template"
                let templateFound = weaponInfo |> Map.tryFind template
                match slotFound, templateFound with
                | None, _ -> Invalid (Guid.NewGuid(), [inv (ErrorCodes.MissingSectionSlot section slot) c])
                | _, None -> Invalid (Guid.NewGuid(), [inv (ErrorCodes.UnknownComponentTemplate template) c])
                | Some (sType, sSize), Some (tType, tSize) ->
                    OK
                    // TODO: Rewrite this
                    // if sType == tType && sSize == tSize then OK else Invalid (Guid.NewGuid(), [inv (ErrorCodes.MismatchedComponentAndSlot slot sSize template tSize) c])

            let defaultTemplates = [ "DEFAULT_COLONIZATION_SECTION"; "DEFAULT_CONSTRUCTION_SECTION"]
            let validateSection (s : Node) =
                let section = s.TagText "template"
                if defaultTemplates |> List.contains section then OK else
                    let sectionFound = sectionInfo |> Map.tryFind section
                    match sectionFound with
                    | None -> Invalid (Guid.NewGuid(), [inv (ErrorCodes.UnknownSectionTemplate section) s])
                    | Some smap ->
                        s.Childs "component" <&!&> validateComponent section smap

            let validateDesign (d : Node) =
                d.Childs "section" <&!&> validateSection

            ship_designs <&!&> validateDesign



    let validateSolarSystemInitializers : STLStructureValidator =
        fun os es ->
            let inits = es.AllOfTypeChildren EntityType.SolarSystemInitializers |> List.filter (fun si -> not(si.Key == "random_list"))
            let starclasses =
                 es.AllOfTypeChildren EntityType.StarClasses @ os.AllOfTypeChildren EntityType.StarClasses
                |> List.map (fun sc -> if sc.Key == "random_list" then sc.TagText "name" else sc.Key)
            let fNode =
                fun (x : Node) ->
                    match x.Has "class", starclasses |> List.contains (x.TagText "class") with
                    |true, true -> OK
                    |false, _ -> Invalid (Guid.NewGuid(), [inv (ErrorCodes.CustomError "This initializer is missing a class" Severity.Error) x])
                    |_, false -> Invalid (Guid.NewGuid(), [inv (ErrorCodes.CustomError (sprintf "The star class %s does not exist" (x.TagText "class")) Severity.Error) x])
            inits <&!&> fNode

    let validatePlanetKillers : STLStructureValidator =
        fun os es ->
            let planetkillers = es.AllOfTypeChildren EntityType.ComponentTemplates |> List.filter (fun ct -> ct.TagText "type" = "planet_killer")
            let onactions = (os.AllOfTypeChildren EntityType.OnActions @ es.AllOfTypeChildren EntityType.OnActions) |> List.map (fun c -> c.Key)
            let scriptedtriggers = (os.AllOfTypeChildren EntityType.ScriptedTriggers @ es.AllOfTypeChildren EntityType.ScriptedTriggers) |> List.map (fun c -> c.Key)
            let inner (node : Node) =
                let key = node.TagText "key"
                let on_action = "on_destroy_planet_with_" + key
                let trigger = "can_destroy_planet_with_" + key
                if List.exists ((==) on_action) onactions then OK else Invalid (Guid.NewGuid(), [inv (ErrorCodes.PlanetKillerMissing (sprintf "Planet killer %s is missing on_action %s" key on_action)) node])
                <&&>
                (if List.exists ((==) trigger) scriptedtriggers then OK else Invalid (Guid.NewGuid(), [inv (ErrorCodes.PlanetKillerMissing (sprintf "Planet killer %s is missing scripted trigger %s" key trigger)) node]))
            planetkillers <&!&> inner

    let validateAnomaly210 : STLStructureValidator =
        fun _ es ->
            let anomalies = es.GlobMatchChildren("**/anomalies/*.txt")
            let fNode =
                fun (x : Node) -> if x.Key == "anomaly" || x.Key == "anomaly_category" then Invalid (Guid.NewGuid(), [inv ((ErrorCodes.CustomError "This style of anomaly was removed with 2.1.0, please see vanilla for details") Severity.Error) x]) else OK
            anomalies <&!&> fNode


    let validateIfElse210 : STLStructureValidator =
        fun _ es ->
            let codeBlocks = (es.AllEffects)// @ (es.AllTriggers |> List.map (fun n -> n :> Node))
            let fNode =
                (fun (x : Node) children ->
                    if x.Key == "limit" || x.Key == "modifier" then OK else
                    let res = if x.Key == "if" && x.Has "else" && not(x.Has "if") then Invalid (Guid.NewGuid(), [inv ErrorCodes.DeprecatedElse x]) else OK
                    let res2 = if x.Key == "else_if" && x.Has "else" && not(x.Has "if") then Invalid (Guid.NewGuid(), [inv ErrorCodes.DeprecatedElse x]) else OK
                    let res3 = if x.Key == "if" && x.Has "else" && x.Has "if" then Invalid (Guid.NewGuid(), [inv ErrorCodes.AmbiguousIfElse x]) else OK
                    (res <&&> res2 <&&> res3) <&&> children
                )
            codeBlocks <&!!&> (foldNode2 fNode (<&&>) OK)

    let validateIfElse : STLStructureValidator =
        fun _ es ->
            let codeBlocks = (es.AllEffects)
            let fNode =
                (fun (x : Node) children ->
                    if x.Key == "if" && x.Has "else" && not(x.Has "if") then
                        children
                    else
                        let nodes = x.Children |> List.map (fun n -> n.Key)
                                                |> List.filter (fun n -> n == "if" || n == "else" || n == "else_if")
                        let checkNext (prevWasIf : bool) (key : string) =
                            match prevWasIf with
                            |true -> (key == "if" || key == "else_if"), None
                            |false ->
                                match key with
                                |y when y == "if" -> true, None
                                |y when y == "else" || y == "else_if" ->
                                    false, Some (Invalid (Guid.NewGuid(), [inv ErrorCodes.IfElseOrder x]))
                                |_ -> false, None
                        let _, res = nodes |> List.fold (fun (s, (r : ValidationResult option)) n -> if r.IsSome then s, r else checkNext s n) (false, None)
                        match res with |None -> children |Some r -> r <&&> children
                )
            codeBlocks <&!!&> (foldNode2 fNode (<&&>) OK)



    let validateDeprecatedSetName : STLStructureValidator =
        fun _ es ->
            let effects = (es.AllEffects)
            let fNode =
                fun (x : Node) children ->
                    if x.Key == "set_empire_name" || x.Key == "set_planet_name" then
                        Invalid (Guid.NewGuid(), [inv ErrorCodes.DeprecatedSetName x]) <&&> children
                    else children
            effects <&!&> (foldNode2 fNode (<&&>) OK)
    let validateEconomicCatAIBudget : LookupValidator<_> =
        fun lu os es ->
            let budgets = os.GlobMatchChildren("**/ai_budget/*.txt") |> List.map (fun b -> b.TagText "category") |> Set.ofList
            let econs = es.GlobMatchChildren("**/economic_categories/*.txt")
            let econWithParent = econs |> List.choose (fun e -> if e.Has "parent" then Some (e.Key, e.TagText "parent") else None) |> Map.ofList
            let rec checkKey (key : string) =
                if Set.contains key budgets
                then true
                else
                    match econWithParent |> Map.tryFind key with
                    | Some k -> checkKey k
                    | None -> false
            let checkEcon (econ : Node) =
                let key = econ.Key
                if checkKey key
                then OK
                else
                    Invalid (Guid.NewGuid(), [inv ((ErrorCodes.CustomError (sprintf "Economic category %s nor any of its parents have an ai_budget" econ.Key)) Severity.Warning) econ])
            econs <&!!&> checkEcon
