namespace CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser
open CWTools.Process.STLScopes
open CWTools.Common
open CWTools.Common.STLConstants
open DotNet.Globbing
open CWTools.Games
open Newtonsoft.Json.Linq
open CWTools.Utilities.Utils


module STLValidation =
    type S = Severity
    type EntitySet(entities : Entity list) =
        member __.GlobMatch(pattern : string) =
            let glob = Glob.Parse(pattern)
            entities |> List.choose (fun es -> if glob.IsMatch(es.filepath) then Some es.entity else None)
        member this.GlobMatchChildren(pattern : string) =
            this.GlobMatch(pattern) |> List.map (fun e -> e.Children) |> List.collect id
        member __.AllOfType (entityType : EntityType) =
            entities |> List.choose(fun es -> if es.entityType = entityType then Some es.entity else None)
        member this.AllOfTypeChildren (entityType : EntityType) =
            this.AllOfType(entityType) |> List.map (fun e -> e.Children) |> List.collect id
        member __.All = entities |> List.map (fun es -> es.entity)
        member this.AllEffects= 
            let fNode = (fun (x : Node) children ->
                            match x with
                            | :? EffectBlock as e -> [e]
                            |_ -> children
                                )
            let fCombine = (@)
            this.All |> List.collect (foldNode2 fNode fCombine [])
        member this.AllTriggers= 
            let fNode = (fun (x : Node) children ->
                            match x with
                            | :? TriggerBlock as e -> [e]
                            |_ -> children
                                )
            let fCombine = (@)
            this.All |> List.collect (foldNode2 fNode fCombine [])

        member __.Raw = entities
        member this.Merge(y : EntitySet) = EntitySet(this.Raw @ y.Raw)

    type StructureValidator = EntitySet -> EntitySet -> ValidationResult
    type FileValidator = IResourceAPI -> EntitySet -> ValidationResult
    let shipName (ship : Ship) = if ship.Name = "" then Invalid [(inv (ErrorCodes.CustomError "must have name") ship)] else OK
    let shipSize (ship : Ship) = if ship.ShipSize = "" then Invalid [(inv (ErrorCodes.CustomError "must have size") ship)] else OK

    let validateShip : Validator<Ship>  = shipName <&> shipSize


    let getDefinedVariables (node : Node) =
        let fNode = (fun (x:Node) children ->
                        let values = x.Values |> List.choose (function  kv when kv.Key.StartsWith("@") -> Some kv.Key |_ -> None)
                        values @ children)
        let fCombine = (@)
        node |> (foldNode2 fNode fCombine [])

    let checkUsedVariables (node : Node) (variables : string list) =
        let fNode = (fun (x:Node) children ->
                        let values = x.Values |> List.choose (fun v -> match v.Value with |String s when s.StartsWith("@") -> Some v |_ -> None)
                        match values with
                        | [] -> children
                        | x -> 
                            x |> List.map ((fun f -> f, f.Value.ToString()) >> (fun (l, v) -> if variables |> List.contains v then OK else Invalid [inv (ErrorCodes.UndefinedVariable v) l]))
                              |> List.fold (<&&>) children)
        let fCombine = (<&&>)
        node |> (foldNode2 fNode fCombine OK)
    
    

    let validateVariables : StructureValidator =
        fun os es ->
            let globalVars = os.GlobMatch("**/common/scripted_variables/*.txt") @ es.GlobMatch("**/common/scripted_variables/*.txt")
                            |> List.map getDefinedVariables
                            |> List.collect id
            es.All <&!&>
            // let x =  
            //     es.All  
            //     |> List.map
                    (fun node -> 
                        let defined = getDefinedVariables node
                        let errors = checkUsedVariables node (defined @ globalVars)
                        errors
                    )
            //x |> List.fold (<&&>) OK

    let categoryScopeList = [
        ModifierCategory.Army, [Scope.Army; Scope.Country]
        ModifierCategory.Country, [Scope.Country]
        ModifierCategory.Leader, [Scope.Leader; Scope.Country]
        ModifierCategory.Megastructure, [Scope.Megastructure; Scope.Country]
        ModifierCategory.Planet, [Scope.Planet; Scope.Country]
        ModifierCategory.PlanetClass, [Scope.Planet; Scope.Pop; Scope.Country]
        ModifierCategory.Pop, [Scope.Pop; Scope.Planet; Scope.Country]
        ModifierCategory.PopFaction, [Scope.PopFaction; Scope.Country]
        ModifierCategory.Science, [Scope.Ship; Scope.Country]
        ModifierCategory.Ship, [Scope.Ship; Scope.Starbase; Scope.Fleet; Scope.Country]
        ModifierCategory.ShipSize, [Scope.Ship; Scope.Starbase; Scope.Country]
        ModifierCategory.Starbase, [Scope.Starbase; Scope.Country]
        ModifierCategory.Tile, [Scope.Tile; Scope.Pop; Scope.Planet; Scope.Country]
    ]

    let inline checkCategoryInScope (modifier : string) (scope : Scope) (node : ^a) (cat : ModifierCategory) =
        match List.tryFind (fun (c, _) -> c = cat) categoryScopeList, scope with
        |None, _ -> OK
        |Some _, s when s = Scope.Any -> OK
        |Some (c, ss), s -> if List.contains s ss then OK else Invalid [inv (ErrorCodes.IncorrectStaticModifierScope modifier (s.ToString()) (ss |> List.map (fun f -> f.ToString()) |> String.concat ", ")) node]
        

    let inline valStaticModifier (modifiers : Modifier list) (scopes : ScopeContext) (modifier : string) (node) =
        let exists = modifiers |> List.tryFind (fun m -> m.tag = modifier && not m.core )
        match exists with
        |None -> Invalid [inv (ErrorCodes.UndefinedStaticModifier modifier) node]
        |Some m -> m.categories <&!&>  (checkCategoryInScope modifier scopes.CurrentScope node)


    let valTriggerLeafUsage (modifiers : Modifier list) (scopes : ScopeContext) (leaf : Leaf) =
        match leaf.Key with
        | "has_modifier" -> valStaticModifier modifiers scopes (leaf.Value.ToRawString()) leaf
        | _ -> OK

    let valEffectLeafUsage (modifiers : Modifier list) (scopes : ScopeContext) (leaf : Leaf) =
        match leaf.Key with
        | "remove_modifier" -> valStaticModifier modifiers scopes (leaf.Value.ToRawString()) leaf
        | _ -> OK

    let valEffectNodeUsage (modifiers : Modifier list) (scopes : ScopeContext) (node : Node) =
        match node.Key with
        | "add_modifier" -> valStaticModifier modifiers scopes (node.TagText "modifier") node
        | _ -> OK

    let eventScope (event : Event) =
        match event.Key with
        |"country_event" -> Scope.Country
        |"fleet_event" -> Scope.Fleet
        |"ship_event" -> Scope.Ship
        |"pop_faction_event" -> Scope.PopFaction
        |"pop_event" -> Scope.Pop
        |"planet_event" -> Scope.Planet
        |_ -> Scope.Army

    let inline handleUnknownTrigger (root : ^a) (key : string) =
        match STLProcess.ignoreKeys |> List.tryFind (fun k -> k == key) with
        |Some _ -> OK //Do better
        |None -> if key.StartsWith("@") then OK else Invalid [inv (ErrorCodes.UndefinedTrigger key) root]
    
    let inline handleUnknownEffect root (key : string) =
        match STLProcess.ignoreKeys |> List.tryFind (fun k -> k == key) with
        |Some _ -> OK //Do better
        |None -> if key.StartsWith("@") then OK else Invalid [inv (ErrorCodes.UndefinedEffect key) root]
    
    let valTriggerLeaf (triggers : Effect list) (modifiers : Modifier list) (scopes : ScopeContext) (leaf : Leaf) =
        match triggers |> List.tryFind (fun e -> e.Name == leaf.Key) with
        |Some (:? ScopedEffect as e) -> Invalid [inv (ErrorCodes.IncorrectScopeAsLeaf (e.Name) (leaf.Value.ToRawString())) leaf]
        |Some e ->
            if e.Scopes |> List.contains(scopes.CurrentScope) || scopes.CurrentScope = Scope.Any
            then valTriggerLeafUsage modifiers scopes leaf
            else Invalid [inv (ErrorCodes.IncorrectTriggerScope leaf.Key (scopes.CurrentScope.ToString()) (e.Scopes |> List.map (fun f -> f.ToString()) |> String.concat ", ")) leaf]
        |None -> handleUnknownTrigger leaf leaf.Key

    let valEffectLeaf (effects : Effect list) (modifiers : Modifier list) (scopes : ScopeContext) (leaf : Leaf) =
        match effects |> List.tryFind (fun e -> e.Name == leaf.Key) with
            |Some (:? ScopedEffect as e) -> Invalid [inv (ErrorCodes.IncorrectScopeAsLeaf (e.Name) (leaf.Value.ToRawString())) leaf]
            |Some e ->
                if e.Scopes |> List.contains(scopes.CurrentScope) || scopes.CurrentScope = Scope.Any
                then valEffectLeafUsage modifiers scopes leaf
                else Invalid [inv (ErrorCodes.IncorrectEffectScope leaf.Key (scopes.CurrentScope.ToString()) (e.Scopes |> List.map (fun f -> f.ToString()) |> String.concat ", ")) leaf]
            |None -> handleUnknownEffect leaf leaf.Key

    let rec valEventTrigger (root : Node) (triggers : Effect list) (effects : Effect list) (modifiers : Modifier list) (scopes : ScopeContext) (effect : Child) =
        match effect with
        |LeafC leaf -> valTriggerLeaf triggers modifiers scopes leaf
        |NodeC node ->
            match node.Key with
            |x when STLProcess.toTriggerBlockKeys |> List.exists (fun t -> t == x) ->
                valNodeTriggers root triggers effects modifiers scopes [] node
            |x when ["else"] |> List.exists (fun t -> t == x) ->
                valNodeTriggers node triggers effects modifiers scopes [] node

            // |x when STLProcess.isTargetKey x ->  
            //     valNodeTriggers root triggers effects Scope.Any node
            // |x when x.Contains("event_target:") ->

            //     OK //Handle later
            |x when x.Contains("parameter:") ->
                OK //Handle later
            |x ->
                match changeScope effects triggers x scopes with
                |NewScope (s, ignores) -> valNodeTriggers root triggers effects modifiers s ignores node
                |WrongScope ss -> Invalid [inv (ErrorCodes.IncorrectScopeScope x (scopes.CurrentScope.ToString()) (ss |> List.map (fun s -> s.ToString()) |> String.concat ", ")) node]
                |NotFound ->
                    match triggers |> List.tryFind (fun e -> e.Name == x ) with
                    |Some e ->
                        if e.Scopes |> List.contains(scopes.CurrentScope) || scopes.CurrentScope = Scope.Any
                        then OK
                        else Invalid [inv (ErrorCodes.IncorrectTriggerScope x (scopes.CurrentScope.ToString()) (e.Scopes |> List.map (fun f -> f.ToString()) |> String.concat ", ")) node]
                    // |Some (_, true) -> OK
                    // |Some (t, false) -> Invalid [inv S.Error node (sprintf "%s trigger used in incorrect scope. In %A but expected %s" x scopes.CurrentScope (t.Scopes |> List.map (fun f -> f.ToString()) |> String.concat ", "))]
                    |None -> handleUnknownTrigger node x
        |_ -> OK

    and valEventEffect (root : Node) (triggers : Effect list) (effects : Effect list) (modifiers : Modifier list) (scopes : ScopeContext) (effect : Child) =
        match effect with
        |LeafC leaf -> valEffectLeaf effects modifiers scopes leaf
        |NodeC node ->
            match node.Key with
            |x when STLProcess.toTriggerBlockKeys |> List.exists (fun t -> t == x) ->
                valNodeTriggers root triggers effects modifiers scopes [] node
            |x when ["else"] |> List.exists (fun t -> t == x) ->
                valNodeEffects node triggers effects modifiers scopes [] node
            // |x when STLProcess.isTargetKey x ->
            //     OK //Handle later
            // |x when x.Contains("event_target:") ->
            //     OK //Handle later
            |x when x.Contains("parameter:") ->
                OK //Handle later
            |x ->
                match changeScope effects triggers x scopes with
                |NewScope (s, ignores) -> valNodeEffects node triggers effects modifiers s ignores node
                |WrongScope ss -> Invalid [inv (ErrorCodes.IncorrectScopeScope x (scopes.CurrentScope.ToString()) (ss |> List.map (fun s -> s.ToString()) |> String.concat ", ")) node]
                |NotFound ->
                    match effects |> List.tryFind (fun e -> e.Name == x) with
                    |Some e -> 
                        if e.Scopes |> List.contains(scopes.CurrentScope) || scopes.CurrentScope = Scope.Any
                        then valEffectNodeUsage modifiers scopes node
                        else Invalid [inv (ErrorCodes.IncorrectEffectScope x (scopes.CurrentScope.ToString()) (e.Scopes  |> List.map (fun f -> f.ToString()) |> String.concat ", ")) node]
                    // |Some(_, true) -> OK
                    // |Some (t, false) -> Invalid [inv S.Error node (sprintf "%s effect used in incorrect scope. In %A but expected %s" x scopes.CurrentScope (t.Scopes  |> List.map (fun f -> f.ToString()) |> String.concat ", "))]
                    |None -> handleUnknownEffect node x
        |_ -> OK
    
    and valNodeTriggers (root : Node) (triggers : Effect list) (effects : Effect list) (modifiers : Modifier list) (scopes : ScopeContext) (ignores : string list) (node : Node) =
        // let scopedTriggers = triggers |> List.map (fun (e, _) -> e, scopes.CurrentScope = Scope.Any || e.Scopes |> List.exists (fun s -> s = scopes.CurrentScope))
        // let scopedEffects = effects |> List.map (fun (e, _) -> e,  scopes.CurrentScope = Scope.Any || e.Scopes |> List.exists (fun s -> s = scopes.CurrentScope)) 
        let filteredAll = 
            node.All
            |> List.filter (function |NodeC c -> not (List.exists (fun i -> i == c.Key) ignores) |LeafC c -> not (List.exists (fun i -> i == c.Key) ignores) |_ -> false)
        List.map (valEventTrigger root triggers effects modifiers scopes) filteredAll |> List.fold (<&&>) OK

    and valNodeEffects (root : Node) (triggers : Effect list) (effects : Effect list) (modifiers : Modifier list) (scopes : ScopeContext) (ignores : string list) (node : Node) =
        //let scopedTriggers = triggers |> List.map (fun (e, _) -> e, scopes.CurrentScope = Scope.Any || e.Scopes |> List.exists (fun s -> s = scopes.CurrentScope)) 
        //let scopedEffects = effects |> List.map (fun (e, _) -> e, scopes.CurrentScope = Scope.Any || e.Scopes |> List.exists (fun s -> s = scopes.CurrentScope)) 
        let filteredAll = 
            node.All
            |> List.filter (function |NodeC c -> not (List.exists (fun i -> i == c.Key) ignores) |LeafC c -> not (List.exists (fun i -> i == c.Key) ignores) |_ -> false)
        List.map (valEventEffect root triggers effects modifiers scopes) filteredAll |> List.fold (<&&>) OK
            

    let valOption (root : Node) (triggers : Effect list) (effects : Effect list) (modifiers : Modifier list) (scopes : ScopeContext) (node : Node) =
        let optionTriggers = ["trigger"; "allow"]
        let optionEffects = ["tooltip"; "hidden_effect"]
        let optionExcludes = ["name"; "custom_tooltip"; "response_text"; "is_dialog_only"; "sound"; "ai_chance"; "custom_gui"; "default_hide_option"]
        let filterFunction =
            function
            | NodeC n -> optionExcludes |> List.exists (fun f -> n.Key == f)
            | LeafC l -> optionExcludes |> List.exists (fun f -> l.Key == f)
            | _ -> false
        let leaves = node.Values |> List.filter (fun l -> not (optionExcludes |> List.exists (fun f -> l.Key == f)))
        let children = node.Children |> List.filter (fun c -> not (optionExcludes |> List.exists (fun f -> c.Key == f)))
        let lres = leaves <&!&> (valEffectLeaf effects modifiers scopes)
        let tres = children |> List.filter (fun c -> optionTriggers |> List.exists (fun f -> c.Key == f))
                    <&!&> (NodeC >> valEventTrigger root triggers effects modifiers scopes)
        let effectPos = children |> List.filter (fun c -> not (optionTriggers |> List.exists (fun f -> c.Key == f)))
        let eres = effectPos |> List.filter (fun c -> not (optionEffects |> List.exists (fun f -> c.Key == f)))
                    <&!&> (NodeC >> valEventEffect root triggers effects modifiers scopes)
        let esres = effectPos |> List.filter (fun c -> optionEffects |> List.exists (fun f -> c.Key == f))
                    <&!&> (valNodeEffects root triggers effects modifiers scopes [])
        lres <&&> tres <&&> eres <&&> esres
        //children |> List.map (valEventEffect node triggers effects scopes) |> List.fold (<&&>) OK
        
    
    let valEventTriggers  (triggers : (Effect) list) (effects : (Effect) list) (modifiers : Modifier list) (event : Event) =
        let eventScope = { Root = eventScope event; From = []; Scopes = [eventScope event]}
        //let eventScope = Seq.append [eventScope event] (Seq.initInfinite (fun _ -> Scope.Any))
        // let scopedTriggers = triggers |> List.map (fun e -> e, e.Scopes |> List.exists (fun s -> s = eventScope.CurrentScope)) 
        // let scopedEffects = effects |> List.map (fun e -> e, e.Scopes |> List.exists (fun s -> s = eventScope.CurrentScope)) 
        match event.Child "trigger" with
        |Some n -> 
            let v = List.map (valEventTrigger event triggers effects modifiers eventScope) n.All
            v |> List.fold (<&&>) OK
        |None -> OK
    /// Flawed, caqn't deal with DU        
    let rec copy source target =
        let properties (x:obj) = x.GetType().GetProperties()
        query {
            for s in properties source do
            join t in properties target on (s.Name = t.Name)
            where t.CanWrite
            select s }
        |> Seq.iter (fun s ->
            let value = s.GetValue(source,null)
            if value.GetType().FullName.StartsWith("System.") 
            then s.SetValue(target, value, null)            
            else copy value (s.GetValue(target,null))
        )

    let filterOptionToEffects (o : STLProcess.Option) =
        let optionTriggers = ["trigger"; "allow"; "exclusive_trigger"]
        let optionEffects = ["tooltip";]
        let optionExcludes = ["name"; "custom_tooltip"; "response_text"; "is_dialog_only"; "sound"; "ai_chance"; "custom_gui"; "default_hide_option"] @ optionTriggers @ optionEffects
        let newO = Option(o.Key, o.Position)
        copy o newO
        newO.All <- newO.All |> List.filter (function |LeafC l -> (not (List.contains l.Key optionExcludes)) | _ -> true)
        newO.All <- newO.All |> List.filter (function |NodeC l -> (not (List.contains l.Key optionExcludes)) | _ -> true)
        newO.Scope <- o.Scope
        newO :> Node

    let valEffectsNew (triggers : (Effect) list) (effects : (Effect) list) (modifiers : Modifier list) (effectBlock : Node) =
        let scope = { Root = effectBlock.Scope; From = []; Scopes = [effectBlock.Scope]}
        effectBlock.All <&!&> valEventEffect effectBlock triggers effects modifiers scope

    let valAllEffects (triggers : (Effect) list) (effects : (Effect) list) (modifiers : Modifier list) (es : EntitySet) =
        let fNode = (fun (x : Node) children ->
            match x with
            | (:? EffectBlock as x) -> valEffectsNew triggers effects modifiers x
            | _ -> OK
            <&&> children)
        let foNode = (fun (x : Node) children ->
            match x with
            | (:? Option as x) -> x |> filterOptionToEffects |> (fun o -> valEffectsNew triggers effects modifiers o)
            | _ -> OK
            <&&> children)
        let fseNode = (fun (x : Node) ->
            
            let scope = { Root = Scope.Any; From = []; Scopes = [Scope.Any]}
            x.Children <&!&> (fun f -> valEventEffect x triggers effects modifiers scope (NodeC f)))
        let fstNode = (fun (x : Node)  ->
            let scope = { Root = Scope.Any; From = []; Scopes = [Scope.Any]}
            x.Children <&!&> (fun f -> valEventTrigger x triggers effects modifiers scope (NodeC f)))

        let fCombine = (<&&>)
        // let opts = es.All |> List.collect (foldNode2 foNode (@) []) |> List.map filterOptionToEffects
        // es.All @ opts <&!&> foldNode2 fNode fCombine OK 
        (es.All <&!&> foldNode2 foNode fCombine OK)
        <&&>
        (es.All <&!&> foldNode2 fNode fCombine OK)
        <&&>
        (es.AllOfTypeChildren EntityType.ScriptedEffects <&!&> fseNode)
        <&&>
        (es.AllOfTypeChildren EntityType.ScriptedTriggers <&!&> fstNode)


    let valTriggersNew (triggers : (Effect) list) (effects : (Effect) list) (modifiers : Modifier list) (effectBlock : Node) =
        let scope = { Root = effectBlock.Scope; From = []; Scopes = [effectBlock.Scope]}
        effectBlock.All <&!&> valEventTrigger effectBlock triggers effects modifiers scope


    let valAllTriggers (triggers : (Effect) list) (effects : (Effect) list) (modifiers : Modifier list) (es : EntitySet) =
        let fNode = (fun (x : Node) children ->
            match x with
            | (:? TriggerBlock as x) when not x.InEffectBlock -> valTriggersNew triggers effects modifiers x
            | _ -> OK
            <&&> children)
        let fCombine = (<&&>)
        es.All <&!&> foldNode2 fNode fCombine OK 



    let valEventEffects (triggers : (Effect) list) (effects : (Effect) list) (modifiers : Modifier list) (event : Event) =
        let eventScope = { Root = eventScope event; From = []; Scopes = [eventScope event]}
        // let scopedTriggers = triggers |> List.map (fun e -> e, e.Scopes |> List.exists (fun s -> s = eventScope.CurrentScope)) 
        // let scopedEffects = effects |> List.map (fun e -> e, e.Scopes |> List.exists (fun s -> s = eventScope.CurrentScope)) 
        let desc = 
            event.Childs "desc" 
            <&!&> (fun n ->
                    match n.Child "trigger" with
                    |Some t -> valNodeTriggers event triggers effects modifiers eventScope [] t
                    |None -> OK)
        let imm = 
            match event.Child "immediate" with
            |Some n -> 
                let v = List.map (valEventEffect event triggers effects modifiers eventScope) n.All
                v |> List.fold (<&&>) OK
            |None -> OK
        let aft = 
            match event.Child "after" with
            |Some n -> 
                let v = List.map (valEventEffect event triggers effects modifiers eventScope) n.All
                v |> List.fold (<&&>) OK
            |None -> OK
        let opts = event.Childs "option" |> List.ofSeq |> List.map (fun o -> (valOption o triggers effects modifiers eventScope o))
        let opts2 = opts |> List.fold (<&&>) OK
        desc <&&> imm <&&> aft <&&> opts2

    /// Make sure an event either has a mean_time_to_happen or is stopped from checking all the time
    /// Not mandatory, but performance reasons, suggested by Caligula
    /// Check "mean_time_to_happen", "is_triggered_only", "fire_only_once" and "trigger = { always = no }".
    /// Create issue if none are true
    let valEventVals (event : Event) =
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
        match isMTTH || isTrig || isOnce || isAlwaysNo with
        | false -> Invalid [inv ErrorCodes.EventEveryTick event]
        | true -> OK

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

    let valTechnology : StructureValidator =
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

    let valButtonEffects : StructureValidator =
        fun os es ->
            let effects = (os.GlobMatchChildren("**/common/button_effects/*.txt"))
                            |> List.filter (fun e -> e :? Button_Effect)
                            |> List.map (fun e -> e.Key)
            let buttons = es.GlobMatchChildren("**/interface/*.gui") @ es.GlobMatchChildren("**/interface/**/*.gui")
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

    let valSprites : StructureValidator = 
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
    
    let valSpriteFiles : FileValidator =
        fun rm es ->
            let sprites = es.GlobMatchChildren("**/interface/*.gfx") @ es.GlobMatchChildren("**/interface/*/*.gfx")
                            |> List.filter (fun e -> e.Key = "spriteTypes")
                            |> List.collect (fun e -> e.Children)
            let filenames = rm.GetResources() |> List.choose (function |FileResource (f, _) -> Some f |EntityResource (f, _) -> Some f)
            let inner = 
                fun (x : Node) ->
                   Seq.append (x.Leafs "textureFile") (Seq.append (x.Leafs "texturefile") (x.Leafs "effectFile"))
                    <&!&> (fun l ->
                        let filename = l.Value.ToRawString().Replace("/","\\")
                        match filenames |> List.exists (fun f -> f.EndsWith(filename) || f.EndsWith(filename.Replace(".lua",".shader").Replace(".tga",".dds"))) with
                        | true -> OK
                        | false -> Invalid [inv (ErrorCodes.MissingFile (l.Value.ToRawString())) l])
            sprites <&!&> inner



    let findAllSetVariables (node : Node) =
        let keys = ["set_variable"; "change_variable"; "subtract_variable"; "multiply_variable"; "divide_variable"]
        let fNode = (fun (x : Node) children ->
                    match x.Children |> List.filter (fun n -> List.contains (n.Key) keys) with
                    | [] -> []
                    | x -> 
                        x |> List.map (fun v -> v.TagText "which")
                    @ children )
        let fCombine = (@)
        foldNode2 fNode fCombine [] node

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

    let getDefinedScriptVariables (es : EntitySet) =
        let fNode = (fun (x : Node) children ->
                    match x with
                    | (:? EffectBlock as x) -> x::children
                    | _ -> children)
        let ftNode = (fun (x : Node) children ->
                    match x with
                    | (:? TriggerBlock as x) -> x::children
                    | _ -> children)
        let foNode = (fun (x : Node) children ->
                    match x with
                    | (:? Option as x) -> x::children
                    | _ -> children)
        let fCombine = (@)
        let opts = es.All |> List.collect (foldNode2 foNode fCombine []) |> List.map filterOptionToEffects
        let effects = es.All |> List.collect (foldNode2 fNode fCombine []) |> List.map (fun f -> f :> Node)
        effects @ opts |> List.collect findAllSetVariables  

    let valVariables : StructureValidator =
        fun os es ->
            let fNode = (fun (x : Node) children ->
                        match x with
                        | (:? EffectBlock as x) -> x::children
                        | _ -> children)
            let ftNode = (fun (x : Node) children ->
                        match x with
                        | (:? TriggerBlock as x) -> x::children
                        | _ -> children)
            let foNode = (fun (x : Node) children ->
                        match x with
                        | (:? Option as x) -> x::children
                        | _ -> children)
            let fCombine = (@)
            let opts = os.All @ es.All |> List.collect (foldNode2 foNode fCombine []) |> List.map filterOptionToEffects
            let effects = os.All @ es.All |> List.collect (foldNode2 fNode fCombine []) |> List.map (fun f -> f :> Node)
            let triggers = es.All |> List.collect (foldNode2 ftNode fCombine []) |> List.map (fun f -> f :> Node)
            let defVars = effects @ opts |> List.collect findAllSetVariables
            triggers <&!&> (validateUsedVariables defVars)


    let valTest : StructureValidator = 
        fun os es ->
            let fNode = (fun (x : Node) children ->
                        match x with
                        | (:? EffectBlock as x) -> x::children
                        | _ -> children)
            let ftNode = (fun (x : Node) children ->
                        match x with
                        | (:? TriggerBlock as x) -> x::children
                        | _ -> children)
            let foNode = (fun (x : Node) children ->
                        match x with
                        | (:? Option as x) -> x::children
                        | _ -> children)
            let fCombine = (@)
            let opts = es.All |> List.collect (foldNode2 foNode fCombine []) |> List.map filterOptionToEffects
            let effects = es.All |> List.collect (foldNode2 fNode fCombine []) |> List.map (fun f -> f :> Node)
            let triggers = es.All |> List.collect (foldNode2 ftNode fCombine []) |> List.map (fun f -> f :> Node)
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
        |Some m -> m.categories <&!&> checkModifierInScope (leaf.Key) (scope) leaf
            // match m.categories |> List.contains (modifierCategory) with
            // |true -> OK
            // |false -> Invalid [inv (ErrorCodes.IncorrectModifierScope (leaf.Key) (modifierCategory.ToString()) (m.categories.ToString())) leaf]


    let valModifiers (modifiers : Modifier list) (node : ModifierBlock) =
        let filteredModifierKeys = ["description"; "key"]
        let filtered = node.Values |> List.filter (fun f -> not (filteredModifierKeys |> List.exists (fun k -> k == f.Key)))
        filtered <&!&> valModifier modifiers node.Scope
    let valAllModifiers (modifiers : (Modifier) list) (es : EntitySet) =
        let fNode = (fun (x : Node) children ->
            match x with
            | (:? ModifierBlock as x) -> valModifiers modifiers x
            | _ -> OK
            <&&> children)
        let fCombine = (<&&>)
        es.All <&!&> foldNode2 fNode fCombine OK 

    let addGeneratedModifiers (modifiers : Modifier list) (es : EntitySet) =
        let ships = es.GlobMatchChildren("**/common/ship_sizes/*.txt")
        let shipKeys = ships |> List.map (fun f -> f.Key)
        let shipModifierCreate =
            (fun k -> 
            [
                {tag = "shipsize_"+k+"_build_speed_mult"; categories = [ModifierCategory.Starbase]; core = true }
                {tag = "shipsize_"+k+"_build_cost_mult"; categories = [ModifierCategory.Starbase]; core = true }
                {tag = "shipsize_"+k+"_upkeep_mult"; categories = [ModifierCategory.Ship]; core = true }
                {tag = "shipsize_"+k+"_hull_mult"; categories = [ModifierCategory.Ship]; core = true }
                {tag = "shipsize_"+k+"_hull_add"; categories = [ModifierCategory.Ship]; core = true }
            ])
        let shipModifiers = shipKeys |> List.collect shipModifierCreate

        let stratres = es.GlobMatchChildren("**/common/strategic_resources/*.txt")
        let srKeys = stratres |> List.map (fun f -> f.Key)
        let srModifierCreate = 
            (fun k ->
            [
                {tag = "static_resource_"+k+"_add"; categories = [ModifierCategory.Country]; core = true }
                {tag = "static_planet_resource_"+k+"_add"; categories = [ModifierCategory.Planet]; core = true }
                {tag = "tile_resource_"+k+"_mult"; categories = [ModifierCategory.Tile]; core = true }
                {tag = "country_resource_"+k+"_mult"; categories = [ModifierCategory.Country]; core = true }
                {tag = "country_federation_member_resource_"+k+"_mult"; categories = [ModifierCategory.Country]; core = true }
                {tag = "country_federation_member_resource_"+k+"_max_mult"; categories = [ModifierCategory.Country]; core = true }
                {tag = "country_subjects_resource_"+k+"_mult"; categories = [ModifierCategory.Country]; core = true }
                {tag = "country_subjects_resource_"+k+"_max_mult"; categories = [ModifierCategory.Country]; core = true }
                {tag = "country_strategic_resources_resource_"+k+"_mult"; categories = [ModifierCategory.Country]; core = true }
                {tag = "country_strategic_resources_resource_"+k+"_max_mult"; categories = [ModifierCategory.Country]; core = true }
                {tag = "country_planet_classes_resource_"+k+"_mult"; categories = [ModifierCategory.Country]; core = true }
                {tag = "country_planet_classes_resource_"+k+"_max_mult"; categories = [ModifierCategory.Country]; core = true }
                {tag = "tile_building_resource_"+k+"_add"; categories = [ModifierCategory.Tile]; core = true }
                {tag = "tile_resource_"+k+"_add"; categories = [ModifierCategory.Tile]; core = true }
                {tag = "planet_resource_"+k+"_add"; categories = [ModifierCategory.Planet]; core = true }
                {tag = "country_resource_"+k+"_add"; categories = [ModifierCategory.Country]; core = true }
                {tag = "max_"+k; categories = [ModifierCategory.Country]; core = true }
            ])
        let srModifiers = srKeys |> List.collect srModifierCreate
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
        let countryTypeKeys = es.GlobMatchChildren("**/common/country_types/*.txt") |> List.map (fun f -> f.Key)
        let countryTypeModifiers = countryTypeKeys |> List.map (fun k -> {tag = "damage_vs_country_type_"+k+"_mult"; categories = [ModifierCategory.Ship]; core = true})
        let speciesKeys = es.GlobMatchChildren("**/common/species_archetypes/*.txt")
                            |> List.filter (fun s -> not (s.Has "inherit_traits_from"))
                            |> List.map (fun s -> s.Key)
        let speciesModifiers = speciesKeys |> List.map (fun k -> {tag = k+"_species_trait_points_add"; categories = [ModifierCategory.Country]; core = true})                    
        shipModifiers @ srModifiers @ pcModifiers @ buildingModifiers @ countryTypeModifiers @ speciesModifiers @ modifiers



    let filterWeightBlockToTriggers (wmb : STLProcess.WeightModifierBlock) =
        let excludes = ["factor"; "add"; "weight"]
        let newWmb = WeightModifierBlock(wmb.Key, wmb.Position)
        copy wmb newWmb
        newWmb.All <- newWmb.All |> List.filter (function |LeafC l -> (not (List.contains l.Key excludes)) | _ -> true)
        newWmb.All <- newWmb.All |> List.filter (function |NodeC l -> (not (List.contains l.Key excludes)) | _ -> true)
        newWmb.Scope <- wmb.Scope
        // eprintfn "%A" (wmb.Values |> List.map (fun  c -> c.Key))
        // eprintfn "%A" (newWmb.Values |> List.map (fun  c -> c.Key))
        newWmb :> Node

    let validateModifierBlocks (triggers : (Effect) list) (effects : (Effect) list) (modifiers : Modifier list) (es : EntitySet) =
        let foNode = (fun (x : Node) children ->
            match x with
            | (:? WeightModifierBlock as x) -> x |> filterWeightBlockToTriggers |> (fun o -> valTriggersNew triggers effects modifiers o)
            | _ -> OK
            <&&> children)
        let fCombine = (<&&>)
        es.All <&!&> foldNode2 foNode fCombine OK