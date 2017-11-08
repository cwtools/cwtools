namespace CK2_Events.Application

open CK2_Events.Application.CKParser
module Process =
    type Option() =
        member val Name = "" with get, set      
    type Event() =
        member val ID = "" with get, set
        member val Desc = "" with get, set
        member val AllTags : KeyValueItem list = List.empty with get, set
        member val Comments : string list = List.empty with get, set
        member this.Tag x = this.AllTags |> List.pick (function |KeyValueItem(ID(y), v) when x=y -> Some v |_ -> None)

    type Root() =
        member val Namespace = "" with get, set
        member val Events : Event list = List.empty with get, set
        member val Comments : string list = List.empty with get, set
        member val AllTags : KeyValueItem list = List.empty with get, set
    let addTag (event : Event) tag =
        event.AllTags <- tag::event.AllTags
        match tag with
            | KeyValueItem(ID("desc"), String(v)) -> event.Desc <- v 
            | KeyValueItem(ID("id"), String(v)) -> event.ID <- v
            | _ -> ()
    
    let processEvent sl =
        let tags = sl |> List.choose (function |KeyValue kv -> Some kv |_ -> None)
        let comments = sl |> List.choose (function |Comment c -> Some c |_ -> None)
        let event = Event()
        List.iter (fun t -> addTag event t) tags
        event.Comments <- comments 
        event


    let processRoot (root : Root) statement =
        //root.AllTags <- statement::root.AllTags
        match statement with
            | KeyValue(KeyValueItem(_ , Block(sl))) -> root.Events <- (processEvent sl)::root.Events
            | KeyValue(KeyValueItem(ID("namespace"), String(v))) -> root.Namespace <- v
            | KeyValue(kv) -> root.AllTags <- kv::root.AllTags
            | Comment(c) -> root.Comments <- c::root.Comments
            | _ -> ()


    let processEventFile (ev : Statement list) =
        let root = Root()
        ev |> List.iter (processRoot root)
        root
        // ev
        // |> List.filter (function |Comment _ -> false |_ -> true)
        // |> List.map processRoot
