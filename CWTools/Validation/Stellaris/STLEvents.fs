namespace CWTools.Validation.Stellaris

open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Common.STLConstants
open CWTools.Utilities.Utils
open System
open QuickGraph
open CWTools.Common

module STLEventValidation =
    type 'a Node = 'a * 'a list
    type 'a Edge = 'a * 'a

    type 'a Graph = 'a list * 'a Edge list

    type 'a AdjacencyGraph = 'a Node list

    type Vertex<'a> = { string: 'a }

    let connectedComponents (g: 'a Graph) =
        match g with
        | _, [] -> []
        | _, es ->
            let edges =
                List.map ((fun x -> ({ string = fst x }, { string = snd x })) >> Edge<Vertex<'a>>) es

            let undirGraph = edges.ToUndirectedGraph()

            let algo =
                QuickGraph.Algorithms.ConnectedComponents.ConnectedComponentsAlgorithm(undirGraph)

            algo.Compute()

            let res =
                algo.Components
                |> Seq.groupBy (fun kv -> kv.Value)
                |> Seq.map (fun (_, vertices) -> vertices |> Seq.map (fun kv -> kv.Key) |> List.ofSeq)
                |> List.ofSeq

            res |> List.map (fun vs -> vs |> List.map (fun v -> v.string))
    // let nodes = g |> List.map fst |> Set.ofList
    // let start = g |> List.head |> fst
    // let rec loop acc g start nodes =
    //     let dfst = depthFirstOrder g start |> Set.ofList
    //     let nodes' = Set.difference nodes dfst
    //     if Set.isEmpty nodes' then
    //         g::acc
    //     else
    //         // once we have the dfst set we can remove those nodes from the graph and
    //         // add them to the solution and continue with the remaining nodes
    //         let (cg,g') = g |> List.fold(fun (xs,ys) v -> if Set.contains (fst v) dfst then (v::xs,ys) else (xs,v::ys)) ([],[])
    //         let start' = List.head g' |> fst
    //         loop (cg::acc) g' start' nodes'
    // loop [] g start nodes

    let findAllReferencedEvents (projects: (Node * string) list) (event: Node) =
        let eventEffectKeys =
            [ "ship_event"
              "pop_event"
              "fleet_event"
              "pop_faction_event"
              "country_event"
              "planet_event" ]

        let fNode =
            (fun (x: Node) children ->
                match x.Key with
                | k when eventEffectKeys |> List.exists (fun f -> f == k) -> x.TagText "id" :: children
                | _ -> children)

        let fpNode =
            (fun (x: Node) children ->
                match x.Key with
                | "enable_special_project" -> [ x.TagText "name" ]
                | _ -> children)

        let fCombine = (@)
        let directCalls = event.Children |> List.collect (foldNode7 fNode) // |> List.fold (@) []
        let referencedProjects = event.Children |> List.collect (foldNode7 fpNode)
        //log "%s projs %A" (event.ID) referencedProjects
        let projectKeys =
            [ "on_success"
              "on_fail"
              "on_start"
              "on_progress_25"
              "on_progress_50"
              "on_progress_75" ]

        let getProjectTarget (n: Node) =
            n.Children
            |> List.filter (fun c -> List.contains c.Key projectKeys)
            |> List.collect (foldNode7 fNode)

        let projectTargets =
            projects
            |> List.filter (fun (_, pk) -> List.contains pk referencedProjects)
            |> List.collect (fun (p, _) -> getProjectTarget p)
        //log "%s proj targets %A" (event.ID) projectTargets
        directCalls @ projectTargets

    let findAllUsedEventTargets (event: Node) =
        let fNode =
            (fun (x: Node) children ->
                let targetFromString (k: string) = k.Substring(13).Split('.').[0]

                let inner children (leaf: Leaf) =
                    let value = leaf.Value.ToRawString()

                    if value.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase) then
                        (value |> targetFromString) :: children
                    else
                        children

                let innerValues = x.Values |> List.fold inner children

                match x.Key with
                | k when k.StartsWith("event_target:", StringComparison.OrdinalIgnoreCase) ->
                    (targetFromString k) :: innerValues //((x.Values |> List.choose inner) @ children)
                | _ -> innerValues
            // ((x.Values |> List.choose inner) @ children)

            )
        //let fCombine = (@)
        event |> (foldNode7 fNode) |> Set.ofList

    let findAllSavedEventTargets (event: Node) =
        let fNode =
            (fun (x: Node) children ->
                let inner (leaf: Leaf) =
                    if leaf.Key == "save_event_target_as" then
                        Some(leaf.Value.ToRawString())
                    else
                        None

                (x.Values |> List.choose inner) @ children)

        let fCombine = (@)
        event |> (foldNode2 fNode fCombine []) |> Set.ofList

    let findAllExistsEventTargets (event: Node) =
        let fNode =
            (fun (x: Node) children ->
                let inner (leaf: Leaf) =
                    if leaf.Key == "exists" && leaf.Value.ToRawString().StartsWith("event_target:") then
                        Some(leaf.Value.ToRawString().Substring(13).Split('.').[0])
                    else
                        None

                (x.Values |> List.choose inner) @ children)

        let fCombine = (@)
        event |> (foldNode2 fNode fCombine []) |> Set.ofList

    let findAllSavedGlobalEventTargets (event: Node) =
        let fNode =
            (fun (x: Node) children ->
                let inner (leaf: Leaf) =
                    if leaf.Key == "save_global_event_target_as" then
                        Some(leaf.Value.ToRawString())
                    else
                        None

                (x.Values |> List.choose inner) @ children)

        let fCombine = (@)
        event |> (foldNode2 fNode fCombine []) |> Set.ofList

    let addScriptedEffectTargets
        (effects: ScriptedEffect list)
        (((eid, e), s, u, r, x): (string * Node) * Set<string> * Set<string> * string list * Set<string>)
        =
        let fNode =
            (fun (x: Node) (s, u) ->
                let inner (leaf: Leaf) =
                    effects
                    |> List.tryFind (fun e -> leaf.KeyId.normal = e.Name.normal)
                    |> Option.map (fun e -> e.SavedEventTargets, e.UsedEventTargets)

                x.Values
                |> List.choose inner
                |> List.fold (fun (s, u) (s2, u2) -> s @ s2, u @ u2) (s, u))

        let fCombine = (fun (s, u) (s2, u2) -> s @ s2, u @ u2)
        let s2, u2 = foldNode2 fNode fCombine (s |> Set.toList, u |> Set.toList) e
        ((eid, e), s2 |> Set.ofList, u2 |> Set.ofList, r, x)

    let addSystemInitializer
        (sinits: Node list)
        (((eid, e), s, u, r, x): (string * Node) * Set<string> * Set<string> * string list * Set<string>)
        =
        let fNode =
            (fun (x: Node) (s, u) ->
                match x.Key with
                | "spawn_system" ->
                    let initializer = x.TagText "initializer"

                    match sinits |> List.tryFind (fun f -> f.Key == initializer) with
                    | None -> s, u
                    | Some sys ->
                        (findAllSavedEventTargets sys |> Set.toList) @ s,
                        (findAllUsedEventTargets sys |> Set.toList) @ u
                | _ -> s, u)

        let fCombine = (fun (s, u) (s2, u2) -> s @ s2, u @ u2)
        let s2, u2 = foldNode2 fNode fCombine (s |> Set.toList, u |> Set.toList) e
        ((eid, e), s2 |> Set.ofList, u2 |> Set.ofList, r, x)

    let checkEventChain
        (effects: ScriptedEffect list)
        (sinits: Node list)
        (projects: (Node * string) list)
        (globals: Set<string>)
        (events: Node list)
        =
        let mutable current =
            events
            |> List.map (fun e ->
                ((e.TagText "id", e),
                 findAllSavedEventTargets e,
                 findAllUsedEventTargets e,
                 findAllReferencedEvents projects e,
                 findAllExistsEventTargets e))
            //|> List.map (fun (e, s, u, r, x) -> log "%s %A %A %A %A" e.ID s u r x; (e, s, u, r, x))
            //|> (fun f -> log "%A" f; f)
            |> List.map (addScriptedEffectTargets effects)
            |> List.map (addSystemInitializer sinits)
            //|> List.map (fun (e, s, u, r, x) -> log "%s %A %A %A %A" e.ID s u r x; (e, s, u, r, x))
            |> List.map (fun (e, s, u, r, x) -> e, s, s, Set.difference u s, r, x, x)

        let getRequiredTargets (ids: string list) =
            if List.isEmpty ids then
                Set.empty
            else
                let ret =
                    ids
                    |> List.choose (fun x ->
                        current
                        |> List.tryPick (fun ((e2id, e2), _, _, u2, _, _, _) ->
                            if e2id = x then Some(Set.toList u2) else None))
                    |> List.collect id
                    |> Set.ofList
                // log "%A" ret
                ret

        let getExistsTargets currentx (ids: string list) =
            let inter =
                ids
                |> List.choose (fun x ->
                    current
                    |> List.tryPick (fun ((e2id, e2), _, _, u2, _, _, x2) -> if e2id = x then Some(u2, x2) else None))

            let all =
                inter |> List.fold (fun xs (u, x) -> Set.union (Set.union u x) xs) currentx

            let ret =
                inter
                |> List.fold
                    (fun xs (u, x) ->
                        xs
                        |> Set.toList
                        |> List.fold
                            (fun nxs nx ->
                                if Set.contains nx u && not (Set.contains nx x) then
                                    nxs
                                else
                                    nx :: nxs)
                            []
                        |> Set.ofList)
                    all
            // log "%A" ret
            ret

        let update (e, os, s, u, r, ox, x) =
            e, os, s, Set.difference (Set.union u (getRequiredTargets r)) os, r, ox, Set.union x (getExistsTargets x r)

        let mutable i = 0

        let step es =
            //log "%A" current
            i <- i + 1
            let before = current
            current <- es |> List.map update
            current = before || i > 100

        while not (step current) do
            ()
        //current |> List.iter (fun (e, s, u, r) -> log "event %s has %A and needs %A" (e.ID) s u)

        // let getSourceSetTargets (ids : string list) =
        //     let inner = (fun (x : string) -> current |> List.pick (fun (e2, s2, _, _) -> if e2.ID = x then Some (s2) else None))
        //     match ids with
        //     |[] -> Set.empty
        //     | xs -> xs |> List.map inner |> List.reduce (Set.intersect)
        let getSourceSetTargets (id: string) =
            let inner =
                (fun (x: string) ->
                    current
                    |> List.choose (fun ((e2id, e2), _, s2, _, r2, _, _) ->
                        if List.contains x r2 && e2id <> x then Some s2 else None))

            inner id

        let getSourceExistsTargets (id: string) =
            let inner =
                (fun (x: string) ->
                    current
                    |> List.choose (fun ((e2id, e2), _, _, _, r2, _, x2) ->
                        if List.contains x r2 && e2id <> x then Some x2 else None))

            inner id

        let downOptimistic ((eid, e): string * Node, os, s, u, r, ox, x) =
            (eid, e),
            os,
            Set.union
                os
                (getSourceSetTargets eid
                 |> (fun f ->
                     if List.isEmpty f then
                         Set.empty
                     else
                         List.reduce Set.union f)),
            u,
            r,
            ox,
            //x
            Set.union
                x
                (getSourceExistsTargets eid
                 |> (fun f ->
                     if List.isEmpty f then
                         Set.empty
                     else
                         List.reduce Set.union f))

        let mutable i = 0

        let step es =
            //log "%A" current
            i <- i + 1
            let before = current
            current <- es |> List.map downOptimistic
            current = before || i > 100

        while not (step current) do
            ()

        let downPessimistic ((eid, e): string * Node, os, s, u, r, ox, x) =
            (eid, e),
            os,
            Set.union
                os
                (getSourceSetTargets eid
                 |> (fun f ->
                     if List.isEmpty f then
                         Set.empty
                     else
                         List.reduce Set.intersect f)),
            u,
            r,
            ox,
            //x
            Set.union
                x
                (getSourceExistsTargets eid
                 |> (fun f ->
                     if List.isEmpty f then
                         Set.empty
                     else
                         List.reduce Set.intersect f))

        let mutable i = 0

        let step es =
            //log "%A" i
            i <- i + 1
            let before = current
            current <- es |> List.map downPessimistic
            current = before || i > 100

        while not (step current) do
            ()
        //current |> List.iter (fun (e, s, u, r) -> log "event %s has %A and needs %A" (e.ID) s u)
        // current |> List.iter (fun (e, s, u, r) -> log "event %s is missing %A" (e.ID) (Set.difference u s))
        // current |> List.iter (fun (e, os, s, u, r, ox, x) -> log "%s %A %A %A %A" e.ID s u r x;)

        let missing =
            current
            |> List.filter (fun (e, os, s, u, r, ox, x) ->
                not (Set.difference (Set.difference u (Set.union s x)) globals |> Set.isEmpty))

        let maybeMissing =
            current
            |> List.filter (fun (e, os, s, u, r, ox, x) ->
                not (Set.difference (Set.difference u s) globals |> Set.isEmpty)
                && (Set.difference (Set.difference u (Set.union s x)) globals |> Set.isEmpty))

        let createError ((eid, e): string * Node, os, s, u, _, _, x) =
            let needed =
                Set.difference (Set.difference u (Set.union s x)) globals
                |> Set.toList
                |> String.concat ", "

            Invalid(Guid.NewGuid(), [ inv (ErrorCodes.UnsavedEventTarget eid needed) e ])

        let createWarning ((eid, e): string * Node, os, s, u, _, _, _) =
            let needed =
                Set.difference (Set.difference u s) globals |> Set.toList |> String.concat ", "

            Invalid(Guid.NewGuid(), [ inv (ErrorCodes.MaybeUnsavedEventTarget eid needed) e ])

        missing <&!&> createError <&&> (maybeMissing <&!&> createWarning)



    let getEventChains: LookupValidator<_> =
        fun lu os es ->
            let reffects = lu.effects
            let events = es.GlobMatchChildren("**/events/*.txt")
            let eids = events |> List.map (fun e -> e.TagText "id", e) |> Map.ofList

            let projects =
                os.GlobMatchChildren("**/common/special_projects/*.txt")
                @ es.GlobMatchChildren("**/common/special_projects/*.txt")

            let projectsWithTags = projects |> List.map (fun p -> p, p.TagText("key"))

            let chains =
                events
                |> List.collect (fun event ->
                    findAllReferencedEvents projectsWithTags event
                    |> List.map (fun f -> event.TagText "id", f))
                |> List.filter (fun (_, f) -> Map.containsKey f eids)
                //|> (fun f -> log "%A" f; f)
                |> List.collect (fun (s, t) -> [ s, t; t, s ])
                //|> (fun f -> log "%A" f; f)
                //|> (fun es -> (graph2AdjacencyGraph (events |> List.map (fun f -> f.ID), es)))
                |> (fun es -> (events |> List.map (fun f -> f.TagText "id"), es))
                |> connectedComponents
                //|> (fun f -> log "%A" f; f)
                |> List.map (fun set -> set |> List.map (fun event -> eids.[event]))

            if chains |> List.isEmpty then
                OK
            else
                let seffects =
                    reffects
                    |> List.choose (function
                        | :? ScriptedEffect as e -> Some e
                        | _ -> None)

                let effects = os.AllEffects @ es.AllEffects

                let globalScriptedEffects =
                    seffects |> List.collect (fun se -> se.GlobalEventTargets) |> Set.ofList

                let globals =
                    Set.union
                        globalScriptedEffects
                        (effects
                         |> List.map findAllSavedGlobalEventTargets
                         |> List.fold Set.union Set.empty)

                let sinits =
                    os.GlobMatchChildren("**\\common\\solar_system_initializers\\**\\*.txt")
                    @ es.GlobMatchChildren("**\\common\\solar_system_initializers\\**\\*.txt")
                //log "%s" (globals |> Set.toList |> String.concat ", ")
                let filteredChains =
                    chains |> List.choose (fun c -> if c.Length > 500 then None else Some c)

                filteredChains <&!!&> checkEventChain seffects sinits projectsWithTags globals


    let valEventCalls: STLStructureValidator =
        fun os es ->
            let events = (os.AllOfType EntityType.Events @ es.AllOfType EntityType.Events)
            let eventIds = events |> List.collect (fun (e, d) -> d.Force().Eventids)
            let effects = es.AllEffects

            let eventEffectKeys =
                [ "ship_event"
                  "pop_event"
                  "fleet_event"
                  "pop_faction_event"
                  "country_event"
                  "planet_event" ]

            let fNode =
                (fun (x: Node) children ->
                    match x.Key with
                    | k when eventEffectKeys |> List.exists (fun f -> f == k) ->
                        x.Leafs "id"
                        <&!&> (fun l ->
                            if eventIds |> List.contains (l.Value.ToRawString()) then
                                OK
                            else
                                Invalid(Guid.NewGuid(), [ inv (ErrorCodes.UndefinedEvent(l.Value.ToRawString())) l ]))
                    | _ -> OK
                    <&&> children)

            let fCombine = (<&&>)
            effects <&!&> (foldNode2 fNode fCombine OK)
