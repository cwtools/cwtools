namespace CK2_Events.Application

open CK2_Events.Application.CKParser
module Process =
    type Event(id : string) =
        member val ID = id with get, set
        member val Desc = "" with get, set
        member val AllTags : KeyValueItem list = List.empty with get, set
        member val Comments : string list = List.empty with get, set

    
    let addTag (event : Event) tag =
        event.AllTags <- tag::event.AllTags
        let (KeyValueItem(key, value)) = tag
        match key with
            | KeyValueItem(ID("desc"), String(v)) -> event.Desc = v 
            | KeyValuet
    
    let processEvent sl =
        let tags = sl |> List.choose (function |KeyValue kv -> Some kv |_ -> None)
        let id = tags |> List.pick (function |KeyValueItem(ID("id"), String(v)) -> Some v |_ -> None )
        //let id = tags |> List.filter (fun (KeyValueItem(id, _)) -> id.ToString() = "id" )
        let event = Event(id)
        List.iter (fun t -> addTag event t) tags
        event


    let processRoot = function
        | KeyValue(KeyValueItem(id , Block(sl))) -> sl
        | Comment(c) -> c


    let processEventFile ev =
        ev
        |> List.filter (function |Comment _ -> false |_ -> true)
        |> List.map processRoot
