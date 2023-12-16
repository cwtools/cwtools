module ProcessTests

open Expecto
open FParsec
open CWTools.Common
open CWTools.Process
open CWTools.Parser
open CWTools.Parser.Types
open CWTools.Rules
// open CWTools.Rules.RulesParser
open CWTools.Games
open CWTools.Process.ProcessCore
open System.IO
open System.Reflection
open CWTools.Parser.DocsParser
open CWTools.Parser.SetupLogParser
open CWTools.Common.STLConstants
open System
open CWTools.Process.STLProcess
open CWTools.Utilities.Position
open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Utilities.Utils
open CWTools.Utilities
open CWTools.Games.Files
open CWTools.Games.Stellaris
open CWTools.Games.Stellaris.STLLookup
open CWTools.Validation

open CWTools.Validation.Stellaris
open CWTools.Process.Scopes.STL
open CWTools.Process.Scopes
open CWTools.Process.Scopes.Scopes
open CWTools.Common.NewScope
open CWTools.Rules.RulesWrapper


let emptyStellarisSettings (rootDirectory) = {
    rootDirectories = [WD { name = "test"; path = rootDirectory;}]
    modFilter = None
    validation = {
        validateVanilla = false
        experimental = true
        langs = [STL STLLang.English]
    }
    rules = None
    embedded = FromConfig ([], [])
    scriptFolders = None
    excludeGlobPatterns = None
    maxFileSize = None
}
let emptyEmbeddedSettings = {
        triggers = []
        effects = []
        modifiers = []
        embeddedFiles = []
        cachedResourceData = []
        localisationCommands = Legacy ([], [], [])
        eventTargetLinks = []
        cachedRuleMetadata = None
        featureSettings = CWTools.Parser.UtilityParser.FeatureSettings.Default
}
let emptyDataTypes = { DataTypeParser.JominiLocDataTypes.promotes = Map.empty; DataTypeParser.JominiLocDataTypes.confidentFunctions = Map.empty; DataTypeParser.JominiLocDataTypes.functions = Map.empty; DataTypeParser.JominiLocDataTypes.dataTypes = Map.empty; DataTypeParser.JominiLocDataTypes.dataTypeNames = Set.empty }

let specificField = RulesParser.specificField
let optionalMany = RulesParser.optionalMany
let optionalSingle = RulesParser.optionalSingle
let requiredSingle = RulesParser.requiredSingle
let defaultFloat = RulesParser.defaultFloat
let defaultInt = RulesParser.defaultInt
let parseConfig = RulesParser.parseConfig
let dynamicSettings (_) =
    {
            CWTools.Process.Localisation.LegacyLocDynamicsSettings.scriptedLocCommands = []
            CWTools.Process.Localisation.LegacyLocDynamicsSettings.eventTargets = []
            CWTools.Process.Localisation.LegacyLocDynamicsSettings.setVariables = []
    }

let processLocalisation = (CWTools.Games.Helpers.createLocalisationFunctions CWTools.Process.Localisation.STL.locStaticSettings dynamicSettings ([], [], ([], []))  (STLLookup())) |> fst
let validateLocalisation = (CWTools.Games.Helpers.createLocalisationFunctions CWTools.Process.Localisation.STL.locStaticSettings dynamicSettings ([], [], ([], []))  (STLLookup())) |> snd

let createStarbase() =
    let owner = NewRule (LeafRule(specificField "owner", ScopeField [(scopeManager.AnyScope)]), requiredSingle)
    let size = NewRule (LeafRule(specificField "size", ValueField(ValueType.Enum "size")), requiredSingle)
    let moduleR = NewRule (LeafRule(specificField "module", ValueField(ValueType.Enum "module")), optionalMany)
    let building = NewRule (LeafRule(specificField "building", ValueField(ValueType.Enum "building")), optionalMany)
    let effect = NewRule (NodeRule(specificField "effect", [(LeafRule (AliasField "effect", AliasField "effect")), optionalMany]), { optionalSingle with replaceScopes = Some { froms = None; root = Some (scopeManager.ParseScope() "country"); this = Some (scopeManager.ParseScope() "country"); prevs = None }})
    let rule = NewRule (NodeRule(specificField "create_starbase", [owner; size; moduleR; building; effect]), optionalMany)
    rule
let createStarbaseAlias = AliasRule ("effect", createStarbase())
let createStarbaseEnums =
    [("size", ("size", ["medium"; "large"]));
     ("module", ("module", ["trafficControl"]));
     ("building", ("building", ["crew"]))]
    |> Map.ofList
let createStarbaseTypeDef =
    {
        name = "create_starbase"
        nameField = None
        pathOptions = {
            paths = ["events"]
            pathStrict = false
            pathFile = None
            pathExtension = None
        }
        conditions = None
        subtypes = []
        typeKeyFilter = None
        skipRootKey = []
        warningOnly = false
        type_per_file = false
        localisation = []
        modifiers = []
        startsWith = None
        unique = false
        graphRelatedTypes = []
        keyPrefix = None
        shouldBeReferenced = false
    }
// # strategic_resource: strategic resource, deprecated, strategic resource used by the building.
// # allow: trigger to check for allowing construction of building.
// # prerequisites: Tech requirements for building.
// # empire_unique: boolean, can only build one if set to true.
// # cost: resource table, cost of building.
// # is_orbital: boolean, can only be built in orbital station.
// # modifier: modifier, deprecated, applies a modifier to planet; use planet_modifier instead.
// # planet_modifier, country_modifier, army_modifier: applies modifier to planet/country/armies
// # triggered_planet_modifier = { key (optional), potential (scope: planet), modifier }: applies conditional modifier to planet
// # base_buildtime: int, number of days for construction.
// # requires_pop, boolean, building will require a pop for production.
// # required_resources, resource table, required resources for production.
// # produced_resources, resource table, produced resources in production.
// # upgrades, buildings list, buildings this building can upgrade into.
// # is_listed, boolean, toggles if this building is shown in the non-upgrade buildable list.
// # planet_unique, toggles if one can build multiple of this type on a single planet.
// # ai_weight, weight for AI, default is set to one, weight set to 0 means that AI will never build it
// # is_colony: trigger to check if the building is a colony shelter for country (scope: country, from: planet). default: "always = no"
// # active: trigger to check if a building can be active with a given pop worker (scope: pop) if you add a trigger here, you should also add the requirements in the description
// # show_tech_unlock_if: trigger to show this building only conditionally in the technology screen. scope: country. default: { always = yes }
// # planet_modifier_with_pop_trigger = { key (optional), potential (scope: pop), modifier }: applies modifier to pops on planet that satisfy condition in trigger

let building =
    let inner =
        [
            NewRule (LeafRule(specificField "allow", ScalarField ScalarValue), requiredSingle)
            NewRule (LeafRule(specificField "empire_unique", ValueField ValueType.Bool), optionalSingle)
        ]
    NewRule(NodeRule(specificField "building", inner), optionalMany)

// formation_priority = @corvette_formation_priority
// max_speed = @speed_very_fast
// acceleration = 0.35
// rotation_speed = 0.1
// collision_radius = @corvette_collision_radius
// max_hitpoints = 300
// modifier = {
// 	ship_evasion_add = 60
// }
// size_multiplier = 1
// fleet_slot_size = 1
// section_slots = { "mid" = { locator = "part1" } }
// num_target_locators = 2
// is_space_station = no
// icon_frame = 2
// base_buildtime = 60
// can_have_federation_design = yes
// enable_default_design = yes	#if yes, countries will have an auto-generated design at start

// default_behavior = swarm

// prerequisites = { "tech_corvettes" }

// combat_disengage_chance = 1.75

// has_mineral_upkeep = yes
// class = shipclass_military
// construction_type = starbase_shipyard
// required_component_set = "power_core"
// required_component_set = "ftl_components"
// required_component_set = "thruster_components"
// required_component_set = "sensor_components"
// required_component_set = "combat_computers"
let shipsize =
    let inner =
        [
            NewRule(LeafRule(specificField "formation_priority", defaultInt), optionalSingle);
            NewRule(LeafRule(specificField "max_speed", defaultFloat), requiredSingle);
            NewRule(LeafRule(specificField "acceleration", defaultFloat), requiredSingle);
            NewRule(LeafRule(specificField "rotation_speed", defaultFloat), requiredSingle);
            NewRule(LeafRule(specificField "collision_radius", defaultFloat), optionalSingle);
            NewRule(LeafRule(specificField "max_hitpoints", defaultInt), requiredSingle);
            NewRule(NodeRule(specificField "modifier", []), optionalSingle);
            NewRule(LeafRule(specificField "size_multiplier", defaultInt), requiredSingle);
            NewRule(LeafRule(specificField "fleet_slot_size", defaultInt), requiredSingle);
            NewRule(NodeRule(specificField "section_slots", []), optionalSingle);
            NewRule(LeafRule(specificField "num_target_locators", defaultInt), requiredSingle);
            NewRule(LeafRule(specificField "is_space_station", ValueField ValueType.Bool), requiredSingle);
            NewRule(LeafRule(specificField "icon_frame", defaultInt), requiredSingle);
            NewRule(LeafRule(specificField "base_buildtime", defaultInt), requiredSingle);
            NewRule(LeafRule(specificField "can_have_federation_design", ValueField ValueType.Bool), requiredSingle);
            NewRule(LeafRule(specificField "enable_default_design", ValueField ValueType.Bool), requiredSingle);
            NewRule(LeafRule(specificField "default_behavior", TypeField (TypeType.Simple "ship_behavior")), requiredSingle);
            NewRule(NodeRule(specificField "prerequisites", []), optionalSingle);
            NewRule(LeafRule(specificField "combat_disengage_chance", defaultFloat), optionalSingle);
            NewRule(LeafRule(specificField "has_mineral_upkeep", ValueField ValueType.Bool), requiredSingle);
            NewRule(LeafRule(specificField "class", ScalarField ScalarValue), requiredSingle);
            NewRule(LeafRule(specificField "construction_type", ScalarField ScalarValue), requiredSingle);
            NewRule(LeafRule(specificField "required_component_set", ScalarField ScalarValue), requiredSingle);
        ]
    NewRule(NodeRule(specificField "ship_size", inner), optionalMany)

let shipBehaviorType =
    {
        name = "ship_behavior";
        nameField = Some "name";
        pathOptions = {
            paths = ["common/ship_behaviors"];
            pathStrict = false
            pathFile = None
            pathExtension = None
        }
        conditions = None;
        subtypes = [];
        typeKeyFilter = None
        skipRootKey = []
        warningOnly = false
        type_per_file = false
        localisation = []
        modifiers = []
        startsWith = None
        unique = false
        shouldBeReferenced = false
        graphRelatedTypes = []
        keyPrefix = None
    }
let shipSizeType =
    {
        name = "ship_size";
        pathOptions = {
            paths = ["common/ship_sizes"];
            pathStrict = false
            pathFile = None
            pathExtension = None
        }
        nameField = None;
        conditions = None;
        subtypes = [];
        typeKeyFilter = None
        skipRootKey = []
        warningOnly = false
        type_per_file = false
        localisation = []
        modifiers = []
        startsWith = None
        unique = false
        shouldBeReferenced = false
        graphRelatedTypes = []
        keyPrefix = None
    }
//  type[ship_behavior] = {
//      path = "game/common/ship_behaviors"
//      name_field = "name"
//  }
//  type[leader_trait] = {
//      path = "game/common/traits"
//      conditions = {
//          leader_trait = yes
//      }
//  }
//  type[species_trait] = {
//      path = "game/common/traits"
//  }


let effectMap = EffectMap()

[<Tests>]
let testc =
    testList "config parse" [
        testCase "simple parse" <| fun () ->
            let config = "create_starbase = {\n\
                          ## cardinality = 1..1\n\
                          owner = scalar\n\
                          ## cardinality = 1..1\n\
                          size = scalar\n\
                          ## cardinality = 0..100\n\
                          module = scalar\n\
                          ## cardinality = 0..100\n\
                          building = scalar\n\
                          ## cardinality = 0..1\n\
                          effect = effect\n\
                          }"
            let rules, types, enums, _, _ = parseConfig (scopeManager.ParseScope()) (scopeManager.AllScopes) (scopeManager.ParseScope() "Any") (scopeManager.ScopeGroups) "" config
            let Typerules = rules |> List.choose (function |TypeRule (_, rs) -> Some (rs) |_ -> None)
            let input =    "create_starbase = {\n\
                            owner = this \n\
                            owner = this \n\
                            size = large \n\
                            }"
            match CKParser.parseString input "test" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode() "root" (range.Zero) r)
                let apply = RuleValidationService(RulesWrapper(rules), [], Map.empty, Map.empty, Map.empty, [], Set.empty, effectMap, effectMap, (scopeManager.ParseScope() "Any"), changeScope, defaultContext, STL STLLang.Default, processLocalisation, validateLocalisation)
                let errors = apply.ApplyNodeRule(Typerules, node)
                match errors with
                | OK -> ()
                | Invalid (_, es) -> Expect.equal (es.Length) 1 (sprintf "Following lines are not expected to have an error %A" es )
            |Failure(e, _, _) -> Expect.isTrue false e

    ]

let leftScope() = RootRule.AliasRule("effect", (NodeRule((ScopeField [(scopeManager.ParseScope() "Any")]), [LeafRule ((AliasField "effect"), (AliasField "Effect")), optionalMany]), optionalMany))
let eopEffect() = RootRule.AliasRule("effect", (NodeRule((SpecificField(SpecificValue (StringResource.stringManager.InternIdentifierToken "every_owned_planet"))), [LeafRule ((AliasField "effect"), (AliasField "Effect")), optionalMany]), {optionalMany with pushScope = Some (scopeManager.ParseScope() "Planet")} ))
let logEffect() = RootRule.AliasRule("effect", (LeafRule((NewField.SpecificField(SpecificValue (StringResource.stringManager.InternIdentifierToken "log"))), (ValueField (ValueType.Bool))), {optionalMany with pushScope = Some (scopeManager.ParseScope() "Planet")} ))

[<Tests>]
let testsv =
    testList "config validate" [
        testCase "create_starbase" <| fun () ->
            let input =    "create_starbase = {\n\
                            owner = root \n\
                            size = large \n\
                            module = trafficControl \n\
                            }"
            match CKParser.parseString input "test" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode() "root" (range.Zero) r)
                let enums = [("size", ("size", ["medium"; "large"])); ("module", ("module",["trafficControl"]))] |> Map.ofList |> Map.toSeq |> Seq.map (fun (k, (d, s)) -> k, (d, createStringSet s)) |> Map.ofSeq
                let rules = RuleValidationService(RulesWrapper [TypeRule ("create_starbase", createStarbase())], [], Map.empty, enums, Map.empty, [], Set.empty, effectMap, effectMap, (scopeManager.ParseScope() "Any"), changeScope, defaultContext, STL STLLang.Default, processLocalisation, validateLocalisation)
                let errors = rules.ApplyNodeRule([createStarbase()], node)
                match errors with
                | OK -> ()
                | Invalid (_ , es)-> Expect.isEmpty es (sprintf "should be empty: %A" es)
            |Failure(e, _, _) -> Expect.isTrue false e
        testCase "create_starbase fail" <| fun () ->
            let input =    "create_starbase = {\n\
                            owner = root \n\
                            size = fake \n\
                            module = faker \n\
                            unknown = test
                            }"
            match CKParser.parseString input "test" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode() "root" (range.Zero) r)
                let enums = createStarbaseEnums |> Map.toSeq |> Seq.map (fun (k, (d ,s)) -> k, (d, createStringSet s)) |> Map.ofSeq
                let rules = RuleValidationService(RulesWrapper [TypeRule ("create_starbase", createStarbase())], [], Map.empty, enums, Map.empty, [], Set.empty, effectMap, effectMap, (scopeManager.ParseScope() "Any"), changeScope, defaultContext, STL STLLang.Default, processLocalisation, validateLocalisation)
                let errors = rules.ApplyNodeRule([createStarbase()], node)
                match errors with
                | OK -> ()
                | Invalid (_ , es)-> Expect.equal (es.Length) 3 (sprintf "Following lines are not expected to have an error %A" es )
            |Failure(e, _, _) -> Expect.isTrue false e
        testCase "create_starbase min count" <| fun () ->
            let input =    "create_starbase = {\n\
                            }"
            match CKParser.parseString input "test" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode() "root" (range.Zero) r)
                let enums = [("size", ["medium"; "large"])] |> Map.ofList |> Map.toSeq |> Seq.map (fun (k, s) -> k, createStringSet s) |> Map.ofSeq
                let rules = RuleValidationService(RulesWrapper [TypeRule ("create_starbase", createStarbase())], [], Map.empty,Map.empty, enums, [], Set.empty, effectMap, effectMap, (scopeManager.ParseScope() "Any"), changeScope, defaultContext, STL STLLang.Default, processLocalisation, validateLocalisation)
                let errors = rules.ApplyNodeRule([createStarbase()], node)
                match errors with
                | OK -> ()
                | Invalid (_ , es)-> Expect.equal 2 (es.Length) (sprintf "Following lines are not expected to have an error %A" es )
            |Failure(e, _, _) -> Expect.isTrue false e
        testCase "create_starbase max count" <| fun () ->
            let input =    "create_starbase = {\n\
                            owner = this \n\
                            owner = this \n\
                            size = large \n\
                            }"
            match CKParser.parseString input "test" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode() "root" (range.Zero) r)
                let enums = [("size", ("size", ["medium"; "large"]))] |> Map.ofList |> Map.toSeq |> Seq.map (fun (k, (d, s)) -> k, (d, createStringSet s)) |> Map.ofSeq
                let rules = RuleValidationService(RulesWrapper [TypeRule ("create_starbase", createStarbase())], [], Map.empty, enums, Map.empty, [], Set.empty, effectMap, effectMap, (scopeManager.ParseScope() "Any"), changeScope, defaultContext, STL STLLang.Default, processLocalisation, validateLocalisation)
                let errors = rules.ApplyNodeRule([createStarbase()], node)
                match errors with
                | OK -> ()
                | Invalid (_ , es)-> Expect.equal (es.Length) 1 (sprintf "Following lines are not expected to have an error %A" es )
            |Failure(e, _, _) -> Expect.isTrue false e
        testCase "create_starbase effect in effect" <| fun () ->
            let input =    "create_starbase = {\n\
                            owner = this \n\
                            size = large \n\
                            effect = {\n\
                            create_starbase = {\
                            owner = this \n size = large\n\
                            }\
                            }\
                            }"
            match CKParser.parseString input "test" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode() "root" (range.Zero) r)
                let enums = [("size", ("size", ["medium"; "large"]))] |> Map.ofList |> Map.toSeq |> Seq.map (fun (k, (d, s)) -> k, (d, createStringSet s)) |> Map.ofSeq
                let rules = RuleValidationService(RulesWrapper [TypeRule ("create_starbase", createStarbase()); createStarbaseAlias], [], Map.empty, enums, Map.empty, [], Set.empty, effectMap, effectMap, (scopeManager.ParseScope() "Any"), changeScope, defaultContext, STL STLLang.Default, processLocalisation, validateLocalisation)
                let errors = rules.ApplyNodeRule([createStarbase()], node)
                match errors with
                | OK -> ()
                | Invalid (_ , es)-> Expect.equal (es.Length) 0 (sprintf "Following lines are not expected to have an error %A" es )
            |Failure(e, _, _) -> Expect.isTrue false e
        testCase "test rhs completion" <| fun () ->
            let input =    "create_starbase = {\n\
                            owner = this \n\
                            size = large \n\
                            }"
            // let resource = makeEntityResourceInput filepath filetext
            // match resourceManager.ManualProcessResource resource, infoService with
            // |Some e, Some info ->

            match CKParser.parseString input "test.txt" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode() "root" (range.Zero) r)
                let entity = { filepath = "events/test.txt"; logicalpath = "events/test.txt"; rawEntity = node; entity = node; validate = true; entityType = EntityType.Events; overwrite = Overwrite.No}
                let enums = [("size", ("size", ["medium"; "large"]))] |> Map.ofList |> Map.toSeq |> Seq.map (fun (k, (d, s)) -> k, (d, createStringSet s)) |> Map.ofSeq

                let comp = CompletionService(RulesWrapper [TypeRule ("create_starbase", createStarbase())], [createStarbaseTypeDef], Map.empty, enums, Map.empty, [], Set.empty, effectMap, effectMap, [], changeScope, defaultContext, (scopeManager.ParseScope() "Any"), [], STL STLLang.Default, emptyDataTypes, processLocalisation, validateLocalisation)
                let pos = mkPos 3 8
                let suggestions = comp.Complete(pos, entity, None) |> Seq.map (function |CompletionResponse.Simple (c, _, _) -> c |Snippet (l, _, _, _, _) -> l) |> Seq.sort
                let expected = ["medium"; "large"] |> Seq.sort
                Expect.sequenceEqual suggestions expected "Completion should match"
            |Failure(e, _, _) -> Expect.isTrue false e
        testCase "test lhs completion" <| fun () ->
            let input =    "create_starbase = {\n\
                            owner = this \n\
                            size \n\
                            }"
            match CKParser.parseString input "test.txt" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode() "root" (range.Zero) r)
                let entity = { filepath = "events/test.txt"; logicalpath = "events/test.txt"; rawEntity = node; entity = node; validate = true; entityType = EntityType.Events; overwrite = Overwrite.No}
                let comp = CompletionService(RulesWrapper [TypeRule ("create_starbase", createStarbase())], [createStarbaseTypeDef], Map.empty, Map.empty, Map.empty, [], Set.empty, effectMap, effectMap, [], changeScope, defaultContext, (scopeManager.ParseScope() "Any"), [], STL STLLang.Default, emptyDataTypes, processLocalisation, validateLocalisation)
                let pos = mkPos 3 3
                let suggestions = comp.Complete(pos, entity, None) |> Seq.map (function |Simple (c, _, _) -> c |Snippet (l, _, _, _, _) -> l) |> Seq.sort
                let expected = ["size"; "owner"; "building"; "effect"; "module"] |> Seq.sort
                Expect.sequenceEqual suggestions expected "Completion should match"
            |Failure(e, _, _) -> Expect.isTrue false e

        testCase "test root completion" <| fun () ->
            let input =    "\n\
                            #test\n\
                            \n\
                            create_starbase = {\n\
                            owner = this \n\
                            }\n"
            let pos = mkPos 1 0
            let split = input.Split('\n')
            let filetext = split |> Array.mapi (fun i s -> if i = (pos.Line - 1) then log (sprintf "%s" s); let s = s.Insert(pos.Column, magicCharString) in log(sprintf "%s" s); s else s) |> String.concat "\n"
            match CKParser.parseString filetext "test.txt" with
            |Success(r, _, _) ->
                let node = (STLProcess.shipProcess.ProcessNode() "root" (range.Zero) r)
                let entity = { filepath = "events/test.txt"; logicalpath = "events/test.txt"; rawEntity = node ; entity = node; validate = true; entityType = EntityType.Events; overwrite = Overwrite.No}
                let comp = CompletionService(RulesWrapper [TypeRule ("create_starbase", createStarbase())], [createStarbaseTypeDef], Map.empty, Map.empty, Map.empty, [], Set.empty, effectMap, effectMap, [], changeScope, defaultContext, (scopeManager.ParseScope() "Any"), [], STL STLLang.Default, emptyDataTypes, processLocalisation, validateLocalisation)
                let suggestions = comp.Complete(pos, entity, None) |> Seq.map (function |Simple (c, _, _) -> c |Snippet (l, _, _, _, _) -> l) |> Seq.sort
                let expected = ["create_starbase"; "create_starbase"] |> Seq.sort
                Expect.sequenceEqual suggestions expected "Completion should match"
            |Failure(e, _, _) -> Expect.isTrue false e

        testCase "test test ship_behavior" <| fun () ->
            let input =    "ship_size = {\n\
                            default_behavior = s \n\
                            }"
            let behaviours = "ship_behavior = {\n\
                              name = \"default\"\n\
                              }\n\
                              ship_behavior = {\n\
                              name = \"swarm\"\n\
                              }"
            match CKParser.parseString input "common/ship_sizes/test.txt", CKParser.parseString behaviours "common/ship_behaviors/test.txt" with
            |Success(r, _, _), Success(b, _, _) ->
                let bnode = (STLProcess.shipProcess.ProcessNode() "root" (mkZeroFile "common/ship_behaviors/test.txt") b)
                let be = { entity = bnode; rawEntity = bnode; filepath = "/test/stellaris/common/ship_behaviors/test.txt"; logicalpath = "common/ship_behaviors/test.txt"; validate = false; entityType = EntityType.ShipBehaviors; overwrite = Overwrite.No}
                let ruleapplicator = RuleValidationService(RulesWrapper [TypeRule ("create_starbase", createStarbase())], [], Map.empty, Map.empty, Map.empty, [], Set.empty, effectMap, effectMap, (scopeManager.ParseScope() "Any"), changeScope, defaultContext, STL STLLang.Default, processLocalisation, validateLocalisation)
                let typeinfo = CWTools.Rules.RulesHelpers.getTypesFromDefinitions (Some ruleapplicator) [shipBehaviorType; shipSizeType] [be] |> Map.toSeq |> Seq.map (fun (k, s) -> k, createStringSet (s |> List.map _.id)) |> Map.ofSeq
                eprintfn "%A" typeinfo
                let node = (STLProcess.shipProcess.ProcessNode() "root" (mkZeroFile "common/ship_sizes/test.txt") r)
                let entity = { filepath = "common/ship_sizes/test.txt"; logicalpath = "common/ship_sizes/test.txt"; rawEntity = node; entity = node; validate = true; entityType = EntityType.Events; overwrite = Overwrite.No}
                let pos = mkPos 2 20
                let comp = CompletionService(RulesWrapper [TypeRule ("ship_size", shipsize)], [shipBehaviorType; shipSizeType], typeinfo, Map.empty, Map.empty, [], Set.empty, effectMap, effectMap, [], changeScope, defaultContext, (scopeManager.ParseScope() "Any"), [], STL STLLang.Default, emptyDataTypes, processLocalisation, validateLocalisation)
                let res = comp.Complete(pos, entity, None)
                eprintfn "res4 %A" res
                let suggestions = res |> Seq.map (function |Simple (c, _, _) -> c |Snippet (l, _, _, _, _) -> l) |> Seq.sort
                let expected = ["default"; "swarm"] |> Seq.sort
                Expect.sequenceEqual suggestions expected "Completion should match"

        testCase "test scope at pos simple nodes" <| fun () ->
            let input = "create_starbase = {\n\
                         effect = {\n\
                         every_owned_planet = { \n\
                         }\n\
                         }\n\
                         }"
            match CKParser.parseString input "test.txt" with
            |Success(r, _, _) ->
                UtilityParser.initializeScopes None (Some defaultScopeInputs)
                let node = (STLProcess.shipProcess.ProcessNode() "root" (range.Zero) r)
                let entity = { filepath = "events/test.txt"; logicalpath = "events/test.txt"; rawEntity = node; entity = node; validate = true; entityType = EntityType.Events; overwrite = Overwrite.No}
                let rules = RuleValidationService(RulesWrapper [TypeRule ("create_starbase", createStarbase()); eopEffect()], [createStarbaseTypeDef], Map.empty, Map.empty, Map.empty, [], Set.empty, effectMap, effectMap,(scopeManager.ParseScope() "Any"), changeScope, defaultContext, STL STLLang.Default, processLocalisation, validateLocalisation)
                let infoService = InfoService(RulesWrapper [TypeRule ("create_starbase", createStarbase()); eopEffect()], [createStarbaseTypeDef], Map.empty, Map.empty, Map.empty, [], Set.empty, effectMap, effectMap, rules, changeScope, defaultContext, (scopeManager.ParseScope() "Any"),  STL STLLang.Default, processLocalisation, validateLocalisation)
                // let comp = CompletionService([TypeRule ("create_starbase", RulesParser.createStarbase())], [RulesParser.createStarbaseTypeDef], Map.empty, Map.empty, [], Set.empty, [], [])
                let pos = mkPos 3 23
                let suggestions = infoService.GetInfo (pos, entity)
                match suggestions with
                |None -> Expect.isTrue false "info failed"
                |Some (context, _) ->
                    let scopes = context.Scopes
                    let expected = [(scopeManager.ParseScope() "Planet"); (scopeManager.ParseScope() "Country");]
                    Expect.sequenceEqual scopes expected "Scopes should match"
            |Failure(e, _, _) -> Expect.isTrue false e
        testCase "test scope at pos prev" <| fun () ->
            let input = "create_starbase = {\n\
                         effect = {\n\
                         every_owned_planet = {\n\
                         prev = { \n\
                         }\n\
                         }\n\
                         }\n\
                         }"
            match CKParser.parseString input "test.txt" with
            |Success(r, _, _) ->
                UtilityParser.initializeScopes None (Some defaultScopeInputs)
                let node = (STLProcess.shipProcess.ProcessNode() "root" (range.Zero) r)
                let entity = { filepath = "events/test.txt"; logicalpath = "events/test.txt"; rawEntity = node; entity = node; validate = true; entityType = EntityType.Events; overwrite = Overwrite.No}
                let rules = RuleValidationService(RulesWrapper [TypeRule ("create_starbase", createStarbase()); eopEffect(); leftScope()], [createStarbaseTypeDef], Map.empty, Map.empty, Map.empty, [], Set.empty, effectMap, effectMap, (scopeManager.ParseScope() "Any"), changeScope, defaultContext, STL STLLang.Default, processLocalisation, validateLocalisation)
                let infoService = InfoService(RulesWrapper [TypeRule ("create_starbase", createStarbase()); eopEffect(); leftScope()], [createStarbaseTypeDef], Map.empty, Map.empty, Map.empty, [], Set.empty, effectMap, effectMap, rules, changeScope, defaultContext, (scopeManager.ParseScope() "Any"),  STL STLLang.Default, processLocalisation, validateLocalisation)
                // let comp = CompletionService([TypeRule ("create_starbase", RulesParser.createStarbase())], [RulesParser.createStarbaseTypeDef], Map.empty, Map.empty, [], Set.empty, [], [])
                let pos = mkPos 4 9
                let suggestions = infoService.GetInfo (pos, entity)
                match suggestions with
                |None -> Expect.isTrue false "info failed"
                |Some (context, _) ->
                    let scopes = context.Scopes
                    let expected = [(scopeManager.ParseScope() "Country"); (scopeManager.ParseScope() "Planet"); (scopeManager.ParseScope() "Country");]
                    Expect.sequenceEqual scopes expected "Scopes should match"
            |Failure(e, _, _) -> Expect.isTrue false e
        testCase "test scope at pos leaf" <| fun () ->
            let input = "create_starbase = {\n\
                         effect = {\n\
                         every_owned_planet = {\n\
                         log = yes \n\
                         }\n\
                         }\n\
                         }"
            match CKParser.parseString input "test.txt" with
            |Success(r, _, _) ->
                UtilityParser.initializeScopes None (Some defaultScopeInputs)
                let node = (STLProcess.shipProcess.ProcessNode() "root" (range.Zero) r)
                let entity = { filepath = "events/test.txt"; logicalpath = "events/test.txt"; rawEntity = node; entity = node; validate = true; entityType = EntityType.Events; overwrite = Overwrite.No}
                let rules = RuleValidationService(RulesWrapper [TypeRule ("create_starbase", createStarbase()); eopEffect(); leftScope(); logEffect()], [createStarbaseTypeDef], Map.empty, Map.empty, Map.empty, [], Set.empty, effectMap, effectMap , (scopeManager.ParseScope() "Any"), changeScope, defaultContext, STL STLLang.Default, processLocalisation, validateLocalisation)
                let infoService = InfoService(RulesWrapper [TypeRule ("create_starbase", createStarbase()); eopEffect(); leftScope(); logEffect()], [createStarbaseTypeDef], Map.empty, Map.empty, Map.empty, [], Set.empty, effectMap, effectMap, rules, changeScope, defaultContext, (scopeManager.ParseScope() "Any"),  STL STLLang.Default, processLocalisation, validateLocalisation)
                // let comp = CompletionService([TypeRule ("create_starbase", RulesParser.createStarbase())], [RulesParser.createStarbaseTypeDef], Map.empty, Map.empty, [], Set.empty, [], [])
                let pos = mkPos 4 2
                let suggestions = infoService.GetInfo (pos, entity)
                match suggestions with
                |None -> Expect.isTrue false "info failed"
                |Some (context, _) ->
                    let scopes = context.Scopes
                    let expected = [(scopeManager.ParseScope() "Planet"); (scopeManager.ParseScope() "Country");]
                    Expect.sequenceEqual scopes expected "Scopes should match"
                let pos = mkPos 4 8
                let suggestions = infoService.GetInfo (pos, entity)
                match suggestions with
                |None -> Expect.isTrue false "info failed"
                |Some (context, _) ->
                    let scopes = context.Scopes
                    let expected = [(scopeManager.ParseScope() "Planet"); (scopeManager.ParseScope() "Country");]
                    Expect.sequenceEqual scopes expected "Scopes should match"

            |Failure(e, _, _) -> Expect.isTrue false e

    ]


[<Tests>]
let testsConfig =
    testList "full config" [
        testCase "basic" <| fun () ->
            let configtext = ["./testfiles/configtests/config/test.cwt", File.ReadAllText "./testfiles/configtests/config/test.cwt"]
            let configtext = ("./testfiles/validationtests/trigger_docs.log", File.ReadAllText "./testfiles/validationtests/trigger_docs.log")::configtext
            let configtext = ("./testfiles/validationtests/setup.log", File.ReadAllText "./testfiles/validationtests/setup.log")::configtext

            let folder = "./testfiles/configtests/completiontests"
            // let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.4.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs (scopeManager.ParseScopes) p)
            // let modifiers = SetupLogParser.parseLogsFile "./testfiles/validationtests/setup.log" |> (function |Success(p, _, _) -> SetupLogParser.processLogs p)
            let settings = emptyStellarisSettings folder
            let settings = { settings with embedded = FromConfig ([], []);
                                            rules = Some { ruleFiles = configtext; validateRules = true; debugRulesOnly = false; debugMode = false}}
            let stl = STLGame(settings) :> IGame<STLComputedData>
            //let stl = STLGame(folder, Files(scopeManager.ParseScope() "All"), "", triggers, effects, modifiers, [], [configtext], [STL STLLang.English], false, true, true)

            let input =    "ship_size = {\n\
                            default_behavior =  \n\
                            }"
            let pos = mkPos 2 20
            // let suggestions = stl.Complete pos "common/ship_sizes/test.txt" input
            let suggestions = stl.Complete pos "common/ship_sizes/test.txt" input
            //eprintfn "%A" suggestions
            let suggestions = suggestions |> Seq.map (function |Simple (c, _, _) -> c |Snippet (l, _, _, _, _) -> l) |> Seq.sort
            let expected = ["default"; "swarm"] |> Seq.sort
            Expect.sequenceEqual suggestions expected "Completion should match"

        testCase "basic with config load" <| fun () ->
            let configtext = ["./testfiles/configtests/config/test.cwt", File.ReadAllText "./testfiles/configtests/config/test.cwt"]
            let configtext = ("./testfiles/validationtests/trigger_docs.log", File.ReadAllText "./testfiles/validationtests/trigger_docs.log")::configtext
            let configtext = ("./testfiles/validationtests/setup.log", File.ReadAllText "./testfiles/validationtests/setup.log")::configtext
            let folder = "./testfiles/configtests/completiontests"
            // let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.4.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs (scopeManager.ParseScopes) p)
            // let modifiers = SetupLogParser.parseLogsFile "./testfiles/validationtests/setup.log" |> (function |Success(p, _, _) -> SetupLogParser.processLogs p)
            let settings = emptyStellarisSettings folder
            let settings = { settings with embedded = FromConfig ([], []);
                                            rules = Some { ruleFiles = configtext; validateRules = true; debugRulesOnly = false; debugMode = false}}
            let stl = STLGame(settings) :> IGame<STLComputedData>

            let input =    "ship_size = {\n\
                            default_behavior = s \n\
                            }"
            let pos = mkPos 2 20
            let pos2 = mkPos 2 5
            let suggestions = stl.Complete pos2 "common/ship_sizes/test.txt" input |> Seq.map (function |Simple (c, _, _) -> c |Snippet (l, _, _, _, _) -> l) |> Seq.sort
            let suggestions = stl.Complete pos "common/ship_sizes/test.txt" input |> Seq.map (function |Simple (c, _, _) -> c |Snippet (l, _, _, _, _) -> l) |> Seq.sort
            let expected = ["default"; "swarm"] |> Seq.sort
            Expect.sequenceEqual suggestions expected "Completion should match"

        testCase "shipsize prerequisits" <| fun () ->
            let configtext = ["./testfiles/configtests/config/test.cwt", File.ReadAllText "./testfiles/configtests/config/test.cwt"]
            let configtext = ("./testfiles/validationtests/trigger_docs.log", File.ReadAllText "./testfiles/validationtests/trigger_docs.log")::configtext
            let configtext = ("./testfiles/validationtests/setup.log", File.ReadAllText "./testfiles/validationtests/setup.log")::configtext
            let folder = "./testfiles/configtests/completiontests"
            // let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.4.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs (scopeManager.ParseScopes) p)
            // let modifiers = SetupLogParser.parseLogsFile "./testfiles/validationtests/setup.log" |> (function |Success(p, _, _) -> SetupLogParser.processLogs p)
            let settings = emptyStellarisSettings folder
            let settings = { settings with embedded = FromConfig ([], []);
                                            rules = Some { ruleFiles = configtext; validateRules = true; debugRulesOnly = false; debugMode = false}}
            let stl = STLGame(settings) :> IGame<STLComputedData>

            let input =    "ship_size = {\n\
                            prerequisites = {\n\
                            \n\
                            }\n\
                            }"
            let pos = mkPos 3 0
            let suggestions = stl.Complete pos "common/ship_sizes/test.txt" input |> Seq.map (function |Simple (c, _, _) -> c |Snippet (l, _, _, _, _) -> l) |> Seq.sort
            let expected = ["tech_one"; "tech_two"] |> Seq.sort
            Expect.sequenceEqual suggestions expected "Completion should match"

        testCase "shipsize enum" <| fun () ->
            let configtext = ["./testfiles/configtests/config/test.cwt", File.ReadAllText "./testfiles/configtests/config/test.cwt"]
            let configtext = ("./testfiles/validationtests/trigger_docs.log", File.ReadAllText "./testfiles/validationtests/trigger_docs.log")::configtext
            let configtext = ("./testfiles/validationtests/setup.log", File.ReadAllText "./testfiles/validationtests/setup.log")::configtext
            let folder = "./testfiles/configtests/completiontests"
            // let triggers, effects = parseDocsFile "./testfiles/validationtests/trigger_docs_2.0.4.txt" |> (function |Success(p, _, _) -> DocsParser.processDocs (scopeManager.ParseScopes) p)
            // let modifiers = SetupLogParser.parseLogsFile "./testfiles/validationtests/setup.log" |> (function |Success(p, _, _) -> SetupLogParser.processLogs p)
            let settings = emptyStellarisSettings folder
            let settings = { settings with embedded = FromConfig ([], []);
                                            rules = Some { ruleFiles = configtext; validateRules = true; debugRulesOnly = false; debugMode = false}}
            let stl = STLGame(settings) :> IGame<STLComputedData>

            let input =    "ship_size = {\n\
                            class = \n\
                            }"
            let pos = mkPos 2 8
            let suggestions = stl.Complete pos "common/ship_sizes/test.txt" input |> Seq.map (function |Simple (c, _, _) -> c |Snippet (l, _, _, _, _) -> l) |> Seq.sort
            let expected = ["shipclass_military"; "shipclass_transport"; "shipclass_military_station"; "shipclass_starbase"] |> Seq.sort
            Expect.sequenceEqual suggestions expected "Completion should match"
    ]
//     
// [<Tests>]
// let miscTests =
//     testList "miscTests" [
//         ftestCase "concurrentParse" <| fun () ->
//             let filepath = "./testfiles/configtests/config/test.cwt"
//             let filetext = File.ReadAllText filepath
//             let inner i =
//                 printfn "%A" i
//                 match CKParser.parseString filetext filepath with
//                 | Success (statements, _, _) ->
//                     simpleProcess.ProcessNode() filepath (mkZeroFile filepath) statements |> ignore
//                 | _ -> ()
//             Array.create 10000 0 |> Array.Parallel.iteri (fun i _ -> inner i)
//
//     ]