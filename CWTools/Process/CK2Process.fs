namespace CWTools.Process
open CWTools.Parser
open CWTools.Localisation
open CWTools.Process.ProcessCore
open CWTools.Process
open System.Text

module CK2Process =
    type Option(key, pos) = 
        inherit Node(key, pos)
        member this.Name = this.Tag "name" |> (function | Some (String s) -> s | Some (QString s) -> s | _ -> "")
        member this.CustomTooltip = this.TagText "custom_tooltip"
    type Event(key, pos) =
        inherit Node(key, pos)
        member this.ID = this.TagText "id"
        member this.Desc = this.TagText "desc"
        member this.Hidden = this.Tag "hide_window" |> (function | Some (Bool b) -> b | _ -> false)

    type EventRoot(key, pos) =
        inherit Node("events", Position.Empty)
        member __.Events : Event list = base.All |> List.choose (function |NodeC n -> Some n |_ -> None) |> List.choose (function | :? Event as e -> Some e |_ -> None)
        member this.Namespace = this.Tag "namespace" |> (function |Some (String s) -> s | _ -> "")

    type ArtifactFile(key, pos) =
        inherit Node("artifacts", Position.Empty)
        member this.Slots = this.Child "slots" |> (function |Some c -> c.ToRaw | _ -> [])
        member __.Weapons = base.All |> List.choose (function |NodeC n when n.Key <> "slots" -> Some n |_ -> None)

    // let maps =
    //     [
    //         fst3 >> ((=) "option"), processNodeSimple<Option>, "option", id;
    //         (fun (s, _, _) -> s.EndsWith("event")), processNodeSimple<Event>, "event", id;
    //     ]

    let maps =
        function
        |("option", _, _) -> processNodeSimple<Option>, "option", id;
        |(s, _, _) when s.EndsWith("event") -> processNodeSimple<Event>, "event", id;
        |_ -> processNodeSimple<Node>, "", id;

    let ck2Process = BaseProcess(maps)
    let processCK2Node = ck2Process.ProcessNode<Node>()
    
    let processEventFile (ev : EventFile) = 
        let (EventFile e) = ev
        (ck2Process.ProcessNode<EventRoot>()) "" Position.Empty e :?> EventRoot

    let processArtifact = (ck2Process.ProcessNode<ArtifactFile>()) "" Position.Empty >> (fun n -> n :?> ArtifactFile) 


    let getTriggeredEvents (event:Event) =
        let fNode = (fun (x:Node) children ->
                        match x.Tag "id" with
                        | Some v -> v.ToString()::children
                        | None -> children)
        let fCombine = (@)
        (event.ID, event.Children |> List.collect (foldNode2 fNode fCombine []))

    
    let getTriggeredEventsAll (root:EventRoot) =
        List.map getTriggeredEvents root.Events

    let getIDs (node:Node) =
        let fNode = (fun (x:Node) children ->
                        match x.Tag "id" with
                        | Some v -> v.ToString()::children
                        | None -> children)
        let fCombine = (@)
        foldNode2 fNode fCombine [] node

    let getAllLocalisationKeys (localisation : ILocalisationAPI) (node:Node) =
        let getDesc x = 
            match localisation.Values.ContainsKey(x) with
            | true -> localisation.Values.[x]
            | false -> x
        let fNode = (fun (x:Node) keys ->
                        match x with
                        | :? Event as e -> e.Desc::keys
                        | :? Option as o -> o.Name::keys
                        | _ -> keys)
        let fCombine = (@)
        let keys = foldNode2 fNode fCombine [] node
        keys |> List.map (fun k -> k, getDesc k)

    let addLocalisedDesc (localisation : ILocalisationAPI) (node:Node) =
        let getDesc x = localisation.GetDesc x
          //  match localisation.values.ContainsKey(x) with
          //  | true -> localisation.values.[x]
         //   | false -> x
        let fNode = (fun (x:Node) _ -> 
                        match x with
                        | :? Event as e when e.Tag "desc" |> Option.isSome 
                            -> e.SetTag "desc" (LeafC (Leaf (KeyValueItem(Key("desc"), Value.String (getDesc e.Desc)))))
                        | :? Event as e when e.Child "desc" |> Option.isSome
                            -> e.Child "desc" |> 
                                function 
                                |Some n -> n.SetTag "text" (LeafC (Leaf (KeyValueItem(Key("text"), Value.String (getDesc (n.TagText "text"))))))
                                |None -> ()
                        | :? Option as o -> o.SetTag "name" (LeafC (Leaf (KeyValueItem(Key("name"), Value.String (getDesc o.Name)))))
                        | _ -> ()
                        )
        let fCombine = (fun _ _ -> ())
        foldNode2 fNode fCombine () node
    
    let addLocalisedDescAll (root:EventRoot) (localisation : ILocalisationAPI) =
        root.Events |> List.iter (addLocalisedDesc localisation)
        root
    let getOption (localisation : ILocalisationAPI) (option:Option) =
        let fNode = (fun (x:Node) ((d, t),i) ->
                        match x with
                        | :? Option as o -> 
                            let name =
                                match o.Name with
                                |"" -> 
                                    match o.Child  "name" with |Some n -> n.TagText "text" |None -> ""
                                |a -> a
                            let custom =
                                match o.CustomTooltip with
                                |"" -> 
                                    match o.Child  "custom_toolip" with |Some n -> n.TagText "text" |None -> ""
                                |a -> a
                            match name, custom with
                            |"", "" -> ((d,t),i)
                            |"", vt -> ((d,localisation.GetDesc(vt)), i)
                            |vd, "" -> ((localisation.GetDesc(vd),t), i)
                            |vd, vt -> ((localisation.GetDesc(vd),localisation.GetDesc(vt)), i)
                        | n ->
                            match n.Tag "id" with
                            |Some v -> ((d,t), (v.ToString()) :: i)
                            |_ -> ((d,t), i)
                             )
        let fCombine x c =
            match x, c with
            |(("",""),[]), c -> c
            |(("",""),l), (a,b) -> (a, l@b)
            |(d,l), (_,b) -> (d,l@b)
        foldNode2 fNode fCombine (("",""),[]) option

    let getOptions (localisation : ILocalisationAPI) (event : Event) =
        event.Children |> List.choose (function | :? Option as o -> Some o |_ -> None)
                       |> List.map (getOption localisation)

    let getEventsOptions (localisation : ILocalisationAPI) (root : EventRoot) =
        root.Events |> List.map (fun e -> (e.ID, getOptions localisation e))

    let getEventComments (root : EventRoot) =
        let findComment t s (a : Child) =
            match (s, a) with
            | ((b, c), _) when b -> (b, c)
            | ((_, _), CommentC nc) -> (false, nc)
            | ((_, c), NodeC (:? Event as n)) when n.ID = t -> (true, c)
            | ((_, _), _) -> (false, "")
        root.Events |> List.map (fun e -> e.ID, root.All |> List.fold (findComment e.ID) (false, "") |> snd)

    let getImmediate (event : Event)=
        event.Children |> List.filter (fun c -> c.Key = "immediate")
                       |> List.map getIDs
    
    let getAllImmediates (root : EventRoot) =
        root.Events |> List.map (fun e -> (e.ID, getImmediate e))

    type eventView = { ID :string; Desc:string; Hidden:bool; Key:string}

    let getEventViews (root : EventRoot) =
        let getView (e : Event) =
            //let etype = e.Tag "type" |> (function | Some (String s) -> s | _ -> "")
            let desc =
                match e.Desc with
                |"" -> 
                    match e.Child "desc" with
                    |Some n -> n.TagText "text"
                    |None -> ""
                |a -> a
            {ID = e.ID; Desc = desc; Hidden = e.Hidden; Key = e.Key}
        root.Events |> List.map getView