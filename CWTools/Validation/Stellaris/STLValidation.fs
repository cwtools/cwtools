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


module STLValidation =
    type S = Severity
    let shipName (ship : Ship) = if ship.Name = "" then Invalid [(inv (ErrorCodes.CustomError "must have name" Severity.Error) ship)] else OK
    let shipSize (ship : Ship) = if ship.ShipSize = "" then Invalid [(inv (ErrorCodes.CustomError "must have size" Severity.Error) ship)] else OK

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
                            x |> List.map ((fun f -> f, f.Value.ToString()) >> (fun (l, v) -> if variables |> List.contains v then OK else Invalid [inv (ErrorCodes.UndefinedVariable v) l]))
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



    let inline checkCategoryInScope (modifier : string) (scope : Scope) (node : ^a) (cat : ModifierCategory) =
        match List.tryFind (fun (c, _) -> c = cat) categoryScopeList, scope with
        |None, _ -> OK
        |Some _, s when s = Scope.Any -> OK
        |Some (c, ss), s -> if List.contains s ss then OK else Invalid [inv (ErrorCodes.IncorrectStaticModifierScope modifier (s.ToString()) (ss |> List.map (fun f -> f.ToString()) |> String.concat ", ")) node]


    let inline valStaticModifier (modifiers : Modifier list) (scopes : ScopeContext<Scope>) (modifier : string) (node) =
        let exists = modifiers |> List.tryFind (fun m -> m.tag = modifier && not m.core )
        match exists with
        |None -> Invalid [inv (ErrorCodes.UndefinedStaticModifier modifier) node]
        |Some m -> m.categories <&!&>  (checkCategoryInScope modifier scopes.CurrentScope node)

    let valNotUsage (node : Node) = if (node.Values.Length + node.Children.Length) > 1 then Invalid [inv ErrorCodes.IncorrectNotUsage node] else OK


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
            | false -> Invalid [inv ErrorCodes.EventEveryTick event]
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
                                                         then Invalid [inv (ErrorCodes.PossiblePretrigger l.Key) l]
                                                         else OK)
            let popjobs = es.GlobMatchChildren "**/common/pop_jobs/*.txt"
            let popjobToError (popjob : Node) =
                match popjob.Child "possible" with
                    | None -> OK
                    | Some possible ->
                        possible.Leaves <&!&> (fun l ->
                                                         if Array.contains l.Key stellarisPopJobPreTriggers
                                                         then Invalid [inv (ErrorCodes.PossiblePretrigger l.Key) l]
                                                         else OK)

            (events <&!&> eventToErrors) <&&> (popjobs <&!&> popjobToError)

    let valResearchLeader (area : string) (cat : string option) (node : Node) =
        let fNode = (fun (x:Node) children ->
                        let results =
                            match x.Key with
                            | "research_leader" ->
                                match x.TagText "area" with
                                | "" -> Invalid [inv ErrorCodes.ResearchLeaderArea x]
                                | area2 when area <> area2 -> Invalid [inv (ErrorCodes.ResearchLeaderTech area area2) x]
                                | _ -> OK
                                /// These aren't really required
                                // <&&>
                                // match cat, x.TagText "has_trait" with
                                // | None, _ -> OK
                                // | _, "" -> Invalid [inv S.Error x "This research_leader is missing required \"has_trait\""]
                                // | Some c, t when ("leader_trait_expertise_" + c) <> t -> Invalid [inv S.Warning x "This research_leader has the wrong expertise"]
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
                        | None -> Invalid [inv ErrorCodes.TechCatMissing node]
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
                                    x.Leafs "effect" <&!&> (fun e -> if List.contains (e.Value.ToRawString()) effects then OK else Invalid [inv (ErrorCodes.ButtonEffectMissing (e.Value.ToString())) e])
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
                                    xs <&!&> (fun e -> if List.contains (e.Value.ToRawString()) spriteNames then OK else Invalid [inv (ErrorCodes.SpriteMissing (e.Value.ToString())) e])
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
                        | false -> Invalid [inv (ErrorCodes.MissingFile (l.Value.ToRawString())) l])
            sprites <&!&> inner



    let findAllSetVariables (node : Node) =
        let keys = ["set_variable"; "change_variable"; "subtract_variable"; "multiply_variable"; "divide_variable"]
        let fNode = (fun (x : Node) acc ->
                    x.Children |> List.fold (fun a n -> if List.contains (n.Key) keys then n.TagText "which" :: a else a) acc
                     )
        foldNode7 fNode node |> List.ofSeq

    let  validateUsedVariables (variables : string list) (node : Node) =
        let fNode = (fun (x : Node) children ->
                    match x.Childs "check_variable" |> List.ofSeq with
                    | [] -> children
                    | t ->
                        t <&!&> (fun node -> node |> (fun n -> n.Leafs "which" |> List.ofSeq) <&!&> (fun n -> if List.contains (n.Value.ToRawString()) variables then OK else Invalid [inv (ErrorCodes.UndefinedScriptVariable (n.Value.ToRawString())) node] ))
                        <&&> children
                    )
        let fCombine = (<&&>)
        foldNode2 fNode fCombine OK node

    let getDefinedScriptVariables (es : STLEntitySet) =
        let fNode = (fun (x : Node) acc ->
                    match x with
                    | (:? EffectBlock as x) -> x::acc
                    | _ -> acc
                    )
        let ftNode = (fun (x : Node) acc ->
                    match x with
                    | (:? TriggerBlock as x) -> x::acc
                    | _ -> acc
                    )
        let foNode = (fun (x : Node) acc ->
                    match x with
                    | (:? Option as x) -> x::acc
                    | _ -> acc
                    )
        let opts = es.All |> List.collect (foldNode7 foNode) |> List.map filterOptionToEffects
        let effects = es.All |> List.collect (foldNode7 fNode) |> List.map (fun f -> f :> Node)
        //effects @ opts |> List.collect findAllSetVariables
        es.AllEffects |> List.collect findAllSetVariables

    let getEntitySetVariables (e : Entity) =
        let fNode = (fun (x : Node) acc ->
                    match x with
                    | (:? EffectBlock as x) -> x::acc
                    | _ -> acc
                    )
        let foNode = (fun (x : Node) acc ->
                    match x with
                    | (:? Option as x) -> x::acc
                    | _ -> acc
                    )
        let opts = e.entity |> (foldNode7 foNode) |> List.map filterOptionToEffects |> List.map (fun n -> n :> Node)
        let effects = e.entity |> (foldNode7 fNode) |> List.map (fun f -> f :> Node)
        effects @ opts |> List.collect findAllSetVariables

    let valVariables : STLStructureValidator =
        fun os es ->
            let ftNode = (fun (x : Node) acc ->
                    match x with
                    | (:? TriggerBlock as x) -> x::acc
                    | _ -> acc
                    )
            let triggers = es.All |> List.collect (foldNode7 ftNode) |> List.map (fun f -> f :> Node)
            let defVars = (os.AllWithData @ es.AllWithData) |> List.collect (fun (_, d) -> d.Force().Setvariables)
            //let defVars = effects @ opts |> List.collect findAllSetVariables
            triggers <&!!&> (validateUsedVariables defVars)

    let hasFlagMap =
        fun (flags : Collections.Map<FlagType, string list>) (leaf : Leaf) ->
            let validate (flag : string) (flagType : FlagType) =
                let flag = if flag.Contains "@" then flag.Split('@').[0] else flag
                //Hack due to testing files
                if flag == "yes" || flag == "true" || flag == "test" then OK else
                flags.TryFind flagType |> Option.map (fun values -> if List.exists (fun f -> f == flag) values then OK else Invalid [inv (ErrorCodes.UndefinedFlag flag flagType) leaf])
                                       |> Option.defaultValue (Invalid [inv (ErrorCodes.UndefinedFlag flag flagType) leaf])
            match leaf.Key with
            |"has_planet_flag" -> validate (leaf.Value.ToRawString()) FlagType.Planet
            |"has_country_flag" -> validate (leaf.Value.ToRawString()) FlagType.Country
            |"has_fleet_flag" -> validate (leaf.Value.ToRawString()) FlagType.Fleet
            |"has_ship_flag" -> validate (leaf.Value.ToRawString()) FlagType.Ship
            |"has_pop_flag" -> validate (leaf.Value.ToRawString()) FlagType.Pop
            |"has_global_flag" -> validate (leaf.Value.ToRawString()) FlagType.Global
            |"has_star_flag" -> validate (leaf.Value.ToRawString()) FlagType.Star
            |"has_relation_flag" -> validate (leaf.Value.ToRawString()) FlagType.Relation
            |"has_leader_flag" -> validate (leaf.Value.ToRawString()) FlagType.Leader
            |"has_ambient_object_flag" -> validate (leaf.Value.ToRawString()) FlagType.AmbientObject
            |"has_megastructure_flag" -> validate (leaf.Value.ToRawString()) FlagType.Megastructure
            |"has_species_flag" -> validate (leaf.Value.ToRawString()) FlagType.Species
            |"has_pop_faction_flag" -> validate (leaf.Value.ToRawString()) FlagType.PopFaction
            |"remove_planet_flag" -> validate (leaf.Value.ToRawString()) FlagType.Planet
            |"remove_country_flag" -> validate (leaf.Value.ToRawString()) FlagType.Country
            |"remove_fleet_flag" -> validate (leaf.Value.ToRawString()) FlagType.Fleet
            |"remove_ship_flag" -> validate (leaf.Value.ToRawString()) FlagType.Ship
            |"remove_pop_flag" -> validate (leaf.Value.ToRawString()) FlagType.Pop
            |"remove_global_flag" -> validate (leaf.Value.ToRawString()) FlagType.Global
            |"remove_star_flag" -> validate (leaf.Value.ToRawString()) FlagType.Star
            |"remove_relation_flag" -> validate (leaf.Value.ToRawString()) FlagType.Relation
            |"remove_leader_flag" -> validate (leaf.Value.ToRawString()) FlagType.Leader
            |"remove_ambient_object_flag" -> validate (leaf.Value.ToRawString()) FlagType.AmbientObject
            |"remove_megastructure_flag" -> validate (leaf.Value.ToRawString()) FlagType.Megastructure
            |"remove_species_flag" -> validate (leaf.Value.ToRawString()) FlagType.Species
            |"remove_pop_faction_flag" -> validate (leaf.Value.ToRawString()) FlagType.PopFaction
            |_ -> OK

    let validateUsedFlags (flags : Collections.Map<FlagType, string list>) (node : Node) =
        let fNode = (fun (x : Node) children ->
                        (x.Values <&!&> hasFlagMap flags) <&&> children
                    )
        let fCombine = (<&&>)
        foldNode2 fNode fCombine OK node


    let valTest : STLStructureValidator =
        fun os es ->
            let fNode = (fun (x : Node) acc ->
                        match x with
                        | (:? EffectBlock as x) -> x::acc
                        | _ -> acc
                        )
            let ftNode = (fun (x : Node) acc ->
                        match x with
                        | (:? TriggerBlock as x) -> x::acc
                        | _ -> acc
                        )
            let foNode = (fun (x : Node) acc ->
                        match x with
                        | (:? Option as x) -> x::acc
                        | _ -> acc
                        )
            let opts = es.All |> List.collect (foldNode7 foNode) |> List.map filterOptionToEffects
            let effects = es.All |> List.collect (foldNode7 fNode) |> List.map (fun f -> f :> Node)
            let triggers = es.All |> List.collect (foldNode7 ftNode) |> List.map (fun f -> f :> Node)
            OK
            // opts @ effects <&!&> (fun x -> Invalid [inv (ErrorCodes.CustomError "effect") x])
            // <&&> (triggers <&!&> (fun x -> Invalid [inv (ErrorCodes.CustomError "trigger") x]))

    let inline checkModifierInScope (modifier : string) (scope : Scope) (node : ^a) (cat : ModifierCategory) =
        match List.tryFind (fun (c, _) -> c = cat) categoryScopeList, scope with
        |None, _ -> OK
        |Some _, s when s = Scope.Any -> OK
        |Some (c, ss), s -> if List.contains s ss then OK else Invalid [inv (ErrorCodes.IncorrectModifierScope modifier (s.ToString()) (ss |> List.map (fun f -> f.ToString()) |> String.concat ", ")) node]

    let valModifier (modifiers : Modifier list) (scope : Scope) (leaf : Leaf) =
        match modifiers |> List.tryFind (fun m -> m.tag == leaf.Key) with
        |None -> Invalid [inv (ErrorCodes.UndefinedModifier (leaf.Key)) leaf]
        |Some m ->
            m.categories <&!&> checkModifierInScope (leaf.Key) (scope) leaf
            <&&> (leaf.Value |> (function |Value.Int x when x = 0 -> Invalid [inv (ErrorCodes.ZeroModifier leaf.Key) leaf] | _ -> OK))
            // match m.categories |> List.contains (modifierCategory) with
            // |true -> OK
            // |false -> Invalid [inv (ErrorCodes.IncorrectModifierScope (leaf.Key) (modifierCategory.ToString()) (m.categories.ToString())) leaf]


    let valModifiers (modifiers : Modifier list) (node : ModifierBlock) =
        let filteredModifierKeys = ["description"; "key"]
        let filtered = node.Values |> List.filter (fun f -> not (filteredModifierKeys |> List.exists (fun k -> k == f.Key)))
        filtered <&!&> valModifier modifiers node.Scope
    let valAllModifiers : LookupValidator<_, _, _> =
        (fun lu _ es ->
            let modifiers = lu.coreModifiers
            let fNode = (fun (x : Node) children ->
                match x with
                | (:? ModifierBlock as x) -> valModifiers modifiers x
                | _ -> OK
                <&&> children)
            let fCombine = (<&&>)
            es.All <&!!&> foldNode2 fNode fCombine OK)

    let addGeneratedModifiers (modifiers : Modifier list) (es : STLEntitySet) =
        let ships = es.GlobMatchChildren("**/common/ship_sizes/*.txt")
        let shipKeys = ships |> List.map (fun f -> f.Key)
        let shipModifierCreate =
            (fun k ->
            [
                {tag = "shipsize_"+k+"_build_speed_mult"; categories = [ModifierCategory.Starbase]; core = true }
                // {tag = "shipsize_"+k+"_build_cost_mult"; categories = [ModifierCategory.Starbase]; core = true }
                {tag = "shipsize_"+k+"_hull_mult"; categories = [ModifierCategory.Ship]; core = true }
                {tag = "shipsize_"+k+"_hull_add"; categories = [ModifierCategory.Ship]; core = true }
                // {tag = "shipsize_"+k+"_damage_mult"; categories = [ModifierCategory.Ship]; core = true }
                // {tag = "shipsize_"+k+"_evasion_addt"; categories = [ModifierCategory.Ship]; core = true }
                // {tag = "shipsize_"+k+"_disengage_mult"; categories = [ModifierCategory.Ship]; core = true }
            ])
        let shipModifiers = shipKeys |> List.collect shipModifierCreate
        let weaponTags = es.GlobMatch("**/common/component_tags/*.txt") |> List.collect (fun f -> f.LeafValues |> List.ofSeq)
        let weaponTagsModifierCreate =
            (fun k ->
            [
                {tag = k+"_weapon_damage_mult"; categories = [ModifierCategory.Ship]; core = true }
                {tag = k+"_weapon_fire_rate_mult"; categories = [ModifierCategory.Ship]; core = true }
                {tag = k+"_speed_mult"; categories = [ModifierCategory.Ship]; core = true }
            ])
        let weaponModifiers = weaponTags |> List.map (fun l -> l.Value.ToRawString())
                                             |> List.collect weaponTagsModifierCreate

        let economicCategories = es.GlobMatchChildren("**/common/economic_categories/*.txt")
        let costExtra = economicCategories |> Seq.collect (fun n -> n.Childs "triggered_cost_modifier" |> Seq.map (fun c -> c.TagText "key"))
        let producesExtra = economicCategories |> Seq.collect (fun n -> n.Childs "triggered_produces_modifier" |> Seq.map (fun c -> c.TagText "key"))
        let upkeepExtra = economicCategories |> Seq.collect (fun n -> n.Childs "triggered_upkeep_modifier" |> Seq.map (fun c -> c.TagText "key"))
        let allCats = economicCategories |> List.map (fun f -> f.Key)
        let stratres = es.GlobMatchChildren("**/common/strategic_resources/*.txt")
        let srKeys = stratres |> List.map (fun f -> f.Key)
        let resourceModifiersCreate prefix cost produces upkeep =
            (fun k ->
            [
                if cost then
                    yield {tag = prefix+k+"_cost_add"; categories = [ModifierCategory.Resource]; core = true }
                    yield {tag = prefix+k+"_cost_mult"; categories = [ModifierCategory.Resource]; core = true }
                else ()
                if produces then
                    yield {tag = prefix+k+"_produces_add"; categories = [ModifierCategory.Resource]; core = true }
                    yield {tag = prefix+k+"_produces_mult"; categories = [ModifierCategory.Resource]; core = true }
                else ()
                if upkeep then
                    yield {tag = prefix+k+"_upkeep_add"; categories = [ModifierCategory.Resource]; core = true }
                    yield {tag = prefix+k+"_upkeep_mult"; categories = [ModifierCategory.Resource]; core = true }
                else ()
            ]
            )
        let globalEconomicModifierCreate =
            [
                yield! (allCats |> List.collect (fun ec -> resourceModifiersCreate ec true true true ""))
                yield! (costExtra |> Seq.collect (fun ec -> resourceModifiersCreate ec true true true ""))
                yield! (producesExtra |> Seq.collect (fun ec -> resourceModifiersCreate ec true true true ""))
                yield! (upkeepExtra |> Seq.collect (fun ec -> resourceModifiersCreate ec true true true ""))
            ]
        let srModifierCreate =
            (fun k ->
            [
                yield {tag = "country_resource_max_"+k+"_add"; categories = [ModifierCategory.Country]; core = true }
                yield! (allCats |> List.collect (fun ec -> resourceModifiersCreate (ec + "_") true true true k))
                yield! (costExtra |> Seq.collect (fun ec -> resourceModifiersCreate (ec + "_") true true true k))
                yield! (producesExtra |> Seq.collect (fun ec -> resourceModifiersCreate (ec + "_") true true true k))
                yield! (upkeepExtra |> Seq.collect (fun ec -> resourceModifiersCreate (ec + "_") true true true k))
            ])

        let rModifiers = globalEconomicModifierCreate
        let srModifiers = srKeys |> List.collect srModifierCreate

        let pop_cats = es.GlobMatchChildren("**/common/pop_categories/*.txt") |> List.map (fun f -> f.Key)
        let popCatModifierCreate =
            (fun k ->
                [
                    {tag = "pop_cat_" + k + "_happiness"; categories = [ModifierCategory.Pop]; core = true}
                    {tag = "pop_cat_" + k + "_political_power"; categories = [ModifierCategory.Pop]; core = true}
                ])
        let popCatModifiers = pop_cats |> List.collect popCatModifierCreate

        let jobs = es.GlobMatchChildren("**/common/pop_jobs/*.txt") |> List.map (fun f -> f.Key)
        let jobModifierCreate =
            (fun k ->
                [
                    {tag = "job_" + k + "_add"; categories = [ModifierCategory.Planet]; core = true}
                    {tag = "job_" + k + "_per_pop"; categories = [ModifierCategory.Planet]; core = true}
                    {tag = "job_" + k + "_per_crime"; categories = [ModifierCategory.Planet]; core = true}
                ])
        let jobModifiers = jobs |> List.collect jobModifierCreate

        let planetclasses = es.GlobMatchChildren("**/common/planet_classes/*.txt")
        let pcKeys = planetclasses |> List.map (fun f -> f.Key)
        let pcModifiers = pcKeys |> List.map (fun k -> {tag = k+"_habitability"; categories = [ModifierCategory.PlanetClass]; core = true})
        let buildingTags = es.GlobMatch("**/common/building_tags/*.txt") |> List.collect (fun f -> f.LeafValues |> List.ofSeq)
        let buildingTagModifierCreate =
            (fun k ->
            [
                {tag = k+"_construction_speed_mult"; categories = [ModifierCategory.Planet]; core = true }
                {tag = k+"_build_cost_mult"; categories = [ModifierCategory.Planet]; core = true }
            ])
        let buildingModifiers = buildingTags |> List.map (fun l -> l.Value.ToRawString())
                                             |> List.collect buildingTagModifierCreate
        let buildingWithModCap = es.GlobMatchChildren("**/common/buildings/*.txt") |> List.filter (fun n -> n.TagText "is_capped_by_modifier" == "yes")
        let buildingWithModCapCreate =
            (fun k ->
            [
                {tag = k+"_max"; categories = [ModifierCategory.Planet]; core = true }
            ])
        let buildingWithModCapModifiers = buildingWithModCap |> List.map (fun n -> n.Key)
                                                             |> List.collect buildingWithModCapCreate
        let countryTypeKeys = es.GlobMatchChildren("**/common/country_types/*.txt") |> List.map (fun f -> f.Key)
        let countryTypeModifiers = countryTypeKeys |> List.map (fun k -> {tag = "damage_vs_country_type_"+k+"_mult"; categories = [ModifierCategory.Ship]; core = true})
        let speciesKeys = es.GlobMatchChildren("**/common/species_archetypes/*.txt")
                            //|> List.filter (fun s -> not (s.Has "inherit_traits_from"))
                            |> List.map (fun s -> s.Key)
        let speciesModifiers = speciesKeys |> List.map (fun k -> {tag = k+"_species_trait_points_add"; categories = [ModifierCategory.Country]; core = true})
        let districts = es.GlobMatchChildren("**/common/districts/*.txt") |> List.filter (fun d -> not (d.TagText "is_capped_by_modifier" == "no"))
        let districtModifiers = districts |> List.map (fun k -> {tag = k.Key+"_max"; categories = [ModifierCategory.Planet]; core = true})
        let popEthicKeys = es.GlobMatchChildren("**/common/ethics/*.txt") |> List.map (fun s -> s.Key)
        let popEthicModifiers = popEthicKeys |> List.map (fun k -> { tag = "pop_" + k + "_attraction_mult"; categories = [ModifierCategory.Pop]; core = true})
        let techCategoryKeys = es.GlobMatchChildren("**/common/technology/category/*.txt") |> List.map (fun s -> s.Key)
        let techCatModifiers = techCategoryKeys |> List.map (fun k -> { tag = "category_"+  k + "_research_speed_mult"; categories = [ModifierCategory.Country]; core = true})
        shipModifiers @  weaponModifiers @ rModifiers @ srModifiers @ popCatModifiers @ jobModifiers @ pcModifiers @ buildingModifiers @ countryTypeModifiers @ speciesModifiers @ modifiers @ buildingWithModCapModifiers
                    @ districtModifiers @ popEthicModifiers @ techCatModifiers

    let findAllSavedEventTargets (event : Node) =
        let fNode = (fun (x : Node) children ->
                        let inner (leaf : Leaf) = if leaf.Key == "save_event_target_as" || leaf.Key == "save_global_event_target_as" then Some (leaf.Value.ToRawString()) else None
                        (x.Values |> List.choose inner) @ children
                        )
        let fCombine = (@)
        event |> (foldNode2 fNode fCombine [])

    let findAllSavedEventTargetsInEntity (e : Entity) =
        let fNode = (fun (x : Node) acc ->
                    match x with
                    | (:? EffectBlock as x) -> x::acc
                    | _ -> acc
                    )
        let foNode = (fun (x : Node) acc ->
                    match x with
                    | (:? Option as x) -> x::acc
                    | _ -> acc
                    )
        let opts = e.entity |> (foldNode7 foNode) |> List.map filterOptionToEffects |> List.map (fun n -> n :> Node)
        let effects = e.entity |> (foldNode7 fNode) |> List.map (fun f -> f :> Node)
        effects @ opts |> List.collect findAllSavedEventTargets


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
                let isWeightFactorZero = t.Child "weight_modifier" |> Option.map (fun wm -> wm.Tag "factor" |> (function |Some (Value.Float 0.00) -> true |_ -> false)) |> Option.defaultValue false
                let hasFeatureFlag = t.Has "feature_flags"
                if isPreReq || isMod || hasChildren || isUsedElsewhere || isWeightZero || isWeightFactorZero || hasFeatureFlag then OK else Invalid [inv (ErrorCodes.UnusedTech (t.Key)) t]
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
                | None, _ -> Invalid [inv (ErrorCodes.MissingSectionSlot section slot) c]
                | _, None -> Invalid [inv (ErrorCodes.UnknownComponentTemplate template) c]
                | Some (sType, sSize), Some (tType, tSize) ->
                    if sType == tType && sSize == tSize then OK else Invalid [inv (ErrorCodes.MismatchedComponentAndSlot slot sSize template tSize) c]

            let defaultTemplates = [ "DEFAULT_COLONIZATION_SECTION"; "DEFAULT_CONSTRUCTION_SECTION"]
            let validateSection (s : Node) =
                let section = s.TagText "template"
                if defaultTemplates |> List.contains section then OK else
                    let sectionFound = sectionInfo |> Map.tryFind section
                    match sectionFound with
                    | None -> Invalid [inv (ErrorCodes.UnknownSectionTemplate section) s]
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
                    |false, _ -> Invalid [inv (ErrorCodes.CustomError "This initializer is missing a class" Severity.Error) x]
                    |_, false -> Invalid [inv (ErrorCodes.CustomError (sprintf "The star class %s does not exist" (x.TagText "class")) Severity.Error) x]
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
                if List.exists ((==) on_action) onactions then OK else Invalid [inv (ErrorCodes.PlanetKillerMissing (sprintf "Planet killer %s is missing on_action %s" key on_action)) node]
                <&&>
                (if List.exists ((==) trigger) scriptedtriggers then OK else Invalid [inv (ErrorCodes.PlanetKillerMissing (sprintf "Planet killer %s is missing scripted trigger %s" key trigger)) node])
            planetkillers <&!&> inner

    let validateAnomaly210 : STLStructureValidator =
        fun _ es ->
            let anomalies = es.GlobMatchChildren("**/anomalies/*.txt")
            let fNode =
                fun (x : Node) -> if x.Key == "anomaly" || x.Key == "anomaly_category" then Invalid [inv ((ErrorCodes.CustomError "This style of anomaly was removed with 2.1.0, please see vanilla for details") Severity.Error) x] else OK
            anomalies <&!&> fNode


    let validateIfElse210 : STLStructureValidator =
        fun _ es ->
            let codeBlocks = (es.AllEffects)// @ (es.AllTriggers |> List.map (fun n -> n :> Node))
            let fNode =
                (fun (x : Node) children ->
                    if x.Key == "limit" || x.Key == "modifier" then OK else
                    let res = if x.Key == "if" && x.Has "else" && not(x.Has "if") then Invalid [inv ErrorCodes.DeprecatedElse x] else OK
                    let res2 = if x.Key == "else_if" && x.Has "else" && not(x.Has "if") then Invalid [inv ErrorCodes.DeprecatedElse x] else OK
                    let res3 = if x.Key == "if" && x.Has "else" && x.Has "if" then Invalid [inv ErrorCodes.AmbiguousIfElse x] else OK
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
                                    false, Some (Invalid [inv ErrorCodes.IfElseOrder x])
                                |_ -> false, None
                        let _, res = nodes |> List.fold (fun (s, (r : ValidationResult option)) n -> if r.IsSome then s, r else checkNext s n) (false, None)
                        match res with |None -> children |Some r -> r <&&> children
                )
            codeBlocks <&!!&> (foldNode2 fNode (<&&>) OK)

    type BoolState = | AND | OR
    let validateRedundantAND : STLStructureValidator =
        fun _ es ->
            let effects = (es.AllEffects)
            let triggers = (es.AllTriggers)
            let fNode =
                fun (last : BoolState) (x : Node) ->
                    match last, x.Key with
                    |AND, k when k == "AND" -> AND, Some (inv (ErrorCodes.UnnecessaryBoolean "AND") x)
                    |OR, k when k == "OR" -> OR, Some (inv (ErrorCodes.UnnecessaryBoolean "OR") x)
                    |_, k when k == "OR" || k == "NOR" -> OR, None
                    |_, _ -> AND, None
            (effects @ triggers) <&!&> (foldNodeWithState fNode AND >> Invalid)

    let validateDeprecatedSetName : STLStructureValidator =
        fun _ es ->
            let effects = (es.AllEffects)
            let fNode =
                fun (x : Node) children ->
                    if x.Key == "set_empire_name" || x.Key == "set_planet_name" then
                        Invalid [inv ErrorCodes.DeprecatedSetName x] <&&> children
                    else children
            effects <&!&> (foldNode2 fNode (<&&>) OK)
