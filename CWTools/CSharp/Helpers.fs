namespace CWTools.CSharp
open CWTools.Rules.RulesHelpers
open CWTools.Games.Stellaris.STLGameFunctions
open CWTools.Common
open CWTools.Rules
open System.IO
open CWTools.Utilities.Utils
open CWTools.Games
open CWTools.Process
open System.Collections.Generic

type Helpers =
    static member LoadAndInitializeFromConfigFiles (configs : (string * string) seq) (game : Game) =
        RulesLoader.loadAndInitializeFromConfigFiles (configs |> List.ofSeq) game |> ignore
        let rules, types, enums, complexenums, values =
            configs
                |> List.ofSeq
                |> List.filter (fun (fn, ft) -> Path.GetExtension fn == ".cwt" )
                |> CWTools.Rules.RulesParser.parseConfigs (scopeManager.ParseScope()) (scopeManager.AllScopes) (scopeManager.AnyScope)
        rules, types, enums, complexenums, values

    static member GetTypesInFile (filepath : string) (node : Node) (types : TypeDefinition list) =
       let entity = {
            filepath = filepath
            logicalpath = filepath
            entity = node
            validate = true
            entityType = CWTools.Common.STLConstants.EntityType.Other
            overwrite = Overwrite.No
        }
       let typeInfoMap = getTypesFromDefinitions None types [entity]
       typeInfoMap |> Map.filter (fun _ v -> not (List.isEmpty v))
                   |> Map.map (fun _ v -> v :> IReadOnlyCollection<_>)
                   :> IReadOnlyDictionary<_,_>

                   