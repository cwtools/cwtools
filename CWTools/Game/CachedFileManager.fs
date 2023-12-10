namespace CWTools.Games
// open CWTools.Process
// open FSharp.Collections.ParallelSeq
// open FParsec
// open System.IO
// open CWTools.Parser
// open DotNet.Globbing
// open CWTools.Common.STLConstants
// open CWTools.Parser.Types
// open CWTools.Utilities.Position
// open CWTools.Utilities.Utils

// type UpdateFile<'T> = ResourceInput -> (Resource * struct (Entity * Lazy<'T>)) list
// type UpdateFiles<'T> = ResourceInput list -> (Resource *  struct (Entity * Lazy<'T>)) list
// type GetResources = unit -> Resource list
// type ValidatableFiles = unit -> EntityResource list
// type AllEntities<'T> = unit -> struct (Entity * Lazy<'T>) list
// type ValidatableEntities<'T> = unit -> struct (Entity * Lazy<'T>) list

// type IResourceAPI<'T> =
//     abstract UpdateFile : UpdateFile<'T>
//     abstract UpdateFiles : UpdateFiles<'T>
//     abstract GetResources : GetResources
//     abstract ValidatableFiles : ValidatableFiles
//     abstract AllEntities : AllEntities<'T>
//     abstract ValidatableEntities : ValidatableEntities<'T>
//     abstract ForceRecompute : unit -> unit

// // type ResourceManager<'T> (computedDataFunction : (Entity -> 'T)) =
