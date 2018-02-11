namespace CWTools.Games
open CWTools.Process
open FSharp.Collections.ParallelSeq
open FParsec
open System.IO
open CWTools.Parser

type ResourceInput =
    {
        scope : string
        filepath : string
        filetext : string
        validate : bool
    }


type PassFileResult = {
    statements : Statement list
    parseTime : int64
}
type FailFileResult = {
    error : string
    position : FParsec.Position
    parseTime : int64
}
type FileResult =
    |Pass of result : PassFileResult
    |Fail of result : FailFileResult
    //|Embedded of file : string * statements : Statement list

type Resource =
    {
        scope : string
        filepath : string
        result : FileResult
        validate : bool
    }

type Entity =
    {
        filepath : string
        entity : Node
        validate : bool
    }

type UpdateFile = ResourceInput -> Entity list
type UpdateFiles = ResourceInput list -> Entity list
type GetResources = unit -> Resource list
type ValidatableFiles = unit -> Resource list
type AllEntities = unit -> Entity list
type ValidatableEntities = unit -> Entity list

type IResourceAPI =
    abstract UpdateFile : UpdateFile
    abstract UpdateFiles : UpdateFiles
    abstract GetResources : GetResources
    abstract ValidatableFiles : ValidatableFiles
    abstract AllEntities : AllEntities
    abstract ValidatableEntities : ValidatableEntities

type ResourceManager () =

    let mutable fileMap : Map<string, Resource> = Map.empty
    let mutable entitiesMap : Map<string, Entity> = Map.empty
    let duration f = 
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()
        let returnValue = f()
        (returnValue  , timer.ElapsedMilliseconds) 

    let matchResult (scope : string, file : string, validate : bool, (parseResult, time)) = 
        match parseResult with
        | Success(parsed, _, _) -> { scope = scope; filepath = file; validate = validate; result = Pass({statements = parsed; parseTime = time}) }
        | Failure(msg, pe, _) -> { scope = scope; filepath = file; validate = validate; result = Fail({error = msg; position = pe.Position; parseTime = time})}


    let parseResults (files : ResourceInput list) = 
            files |> PSeq.map ((fun f -> f.scope, f.filepath, f.validate, (fun (t, t2) -> duration (fun () -> CKParser.parseString t t2)) (f.filepath, f.filetext)) >> matchResult)
                     |> PSeq.toList

    let parseEntities (validfiles : Resource list) =
        validfiles |> List.filter (fun v -> v.result |> function |Pass(_) -> true |_ -> false)
                   |> List.map ((fun f -> (f.filepath, f.validate, let (Pass s) = f.result in s.statements))
                        >> (fun (f, v, parsed) -> { filepath = f; entity = (STLProcess.shipProcess.ProcessNode<Node>() "root" (Position.File(f)) parsed); validate = v}))

    let updateFiles files =
        let fileres = files |> parseResults
        fileMap <- fileres |> List.map (fun fr -> fr.filepath, fr) |> Map.ofList
        let newEntities = fileres |> parseEntities
        entitiesMap <- newEntities |> List.fold (fun x s -> x.Add(s.filepath, s)) entitiesMap
        newEntities

    let getResources() = fileMap |> Map.toList |> List.map snd
    let validatableFiles() = fileMap |> Map.toList |> List.map snd |> List.filter (fun f -> f.validate)
    let allEntities() = entitiesMap |> Map.toList |> List.map snd
    let validatableEntities() = entitiesMap |> Map.toList |> List.map snd |> List.filter (fun e -> e.validate)
        
    member __.Api = {
        new IResourceAPI with
            member __.UpdateFiles = updateFiles
            member __.UpdateFile = (fun f -> updateFiles([f]))
            member __.GetResources = getResources
            member __.ValidatableFiles = validatableFiles
            member __.AllEntities = allEntities
            member __.ValidatableEntities = validatableEntities
        }