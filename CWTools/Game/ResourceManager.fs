namespace CWTools.Games

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.Linq
open System.Threading
open CWTools.Process
open FSharp.Collections.ParallelSeq
open FParsec
open System.IO
open CWTools.Parser
open DotNet.Globbing
open CWTools.Common.STLConstants
open CWTools.Utilities.Position
open CWTools.Utilities.Utils
open CWTools.Utilities
open System.Threading.Tasks
open CWTools.Utilities.StringResource

// Fuzzy = prefix/suffix
type ReferenceType =
    | TypeDef = 0uy
    | Link = 1uy
    | TypeDefFuzzy = 2uy

type ReferenceDetails =
    { name: StringTokens
      originalValue: StringTokens
      position: range
      isOutgoing: bool
      referenceLabel: string option
      referenceType: ReferenceType
      // Associated type, e.g. with modifiers, the main type is the building, but the associated type is modifier_type
      associatedType: string option }

type ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks, savedEventTargets) =
    member val Cache: Map<string, obj list> = Map.empty with get, set
    member val WithRulesData: bool = withRulesData with get, set
    member val Referencedtypes: Map<string, ReferenceDetails list> option = referencedtypes with get, set
    member val Definedvariables: Map<string, ResizeArray<string * range>> option = definedvariable with get, set

    member val SavedEventTargets: ResizeArray<string * range * CWTools.Common.NewScope.Scope> option =
        savedEventTargets with get, set

    member val EffectBlocks: Node list option = effectBlocks with get, set
    member val TriggerBlocks: Node list option = triggersBlocks with get, set

type EU4ComputedData
    (
        referencedtypes,
        definedvariable,
        scriptedeffectparams,
        withRulesData,
        effectBlocks,
        triggersBlocks,
        savedEventTargets
    ) =
    inherit
        ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks, savedEventTargets)

    member _.ScriptedEffectParams: string list option = scriptedeffectparams

type HOI4ComputedData = ComputedData
type CK2ComputedData = ComputedData
type VIC2ComputedData = ComputedData

type ScriptedEffectComputedData
    (
        referencedtypes,
        definedvariable,
        scriptedeffectparams,
        withRulesData,
        effectBlocks,
        triggersBlocks,
        savedEventTargets
    ) =
    inherit
        ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks, savedEventTargets)

    member _.ScriptedEffectParams: string list option = scriptedeffectparams

type STLComputedData = ScriptedEffectComputedData
type JominiComputedData = ScriptedEffectComputedData

type IRComputedData = JominiComputedData
type CK3ComputedData = JominiComputedData
type VIC3ComputedData = JominiComputedData
type EU5ComputedData = JominiComputedData
//     inherit ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks)
// type CK2ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks) =
//     inherit ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks)
// type IRComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks) =
//     inherit ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks)


type PassFileResult = { parseTime: float }

type FailFileResult =
    { error: string
      position: FParsec.Position
      parseTime: float }

type FileResult =
    | Pass of result: PassFileResult
    | Fail of result: FailFileResult
//|Embedded of file : string * statements : Statement list

type Overwrite =
    | No = 1uy
    | Overwrote = 2uy
    | Overwritten = 3uy

type EntityResource =
    { scope: string
      filepath: string
      logicalpath: string
      result: FileResult
      overwrite: Overwrite
      validate: bool }

type FileResource =
    { scope: string
      filepath: string
      logicalpath: string }

type FileWithContentResource =
    { scope: string
      filetext: string
      filepath: string
      extension: string
      logicalpath: string
      overwrite: Overwrite
      validate: bool }

type Resource =
    | EntityResource of string * EntityResource
    | FileResource of string * FileResource
    | FileWithContentResource of string * FileWithContentResource

type EntityResourceInput =
    { scope: string
      filepath: string
      logicalpath: string
      filetext: string
      validate: bool }

type FileResourceInput =
    { scope: string
      filepath: string
      logicalpath: string }

type FileWithContentResourceInput =
    { scope: string
      filetext: string
      filepath: string
      logicalpath: string
      validate: bool }

type Entity =
    { filepath: string
      logicalpath: string
      rawEntity: Node
      entity: Node
      validate: bool
      entityType: EntityType
      overwrite: Overwrite }

    override x.ToString() =
        sprintf "%s %s %b" x.filepath x.logicalpath x.validate

type RawEntity = Entity

type CachedResourceData =
    { resources: (Resource * Entity) list
      files: (string * string) list
      fileIndexTable: FileIndexTable
      stringResourceManager: StringResourceManager }

type ResourceInput =
    | EntityResourceInput of EntityResourceInput
    | FileResourceInput of FileResourceInput
    | CachedResourceInput of Resource * Entity
    | FileWithContentResourceInput of FileWithContentResourceInput




type UpdateFile<'T> = ResourceInput -> Resource * struct (Entity * Lazy<'T>) option
type UpdateFiles<'T> = ResourceInput list -> (Resource * struct (Entity * Lazy<'T>) option) list
type GetResources = unit -> Resource list
type ValidatableFiles = unit -> EntityResource list
type AllEntities<'T> = unit -> struct (Entity * Lazy<'T>) list
type ValidatableEntities<'T> = unit -> struct (Entity * Lazy<'T>) list
type FileNames = unit -> string array

type IResourceAPI<'T when 'T :> ComputedData> =
    abstract UpdateFiles: UpdateFiles<'T>
    abstract UpdateFile: UpdateFile<'T>
    abstract GetResources: GetResources
    abstract ValidatableFiles: ValidatableFiles
    abstract AllEntities: AllEntities<'T>
    abstract ValidatableEntities: ValidatableEntities<'T>
    abstract ForceRecompute: unit -> unit
    abstract ForceRulesDataGenerate: unit -> unit
    abstract GetFileNames: FileNames

[<Sealed>]
type ResourceManager<'T when 'T :> ComputedData>
    (
        computedDataFunction: Entity -> 'T,
        computedDataUpdateFunction: Entity -> 'T -> unit,
        encoding,
        fallbackencoding,
        enableInlineScripts
    ) =
    static do GlobOptions.Default.Evaluation.CaseInsensitive <- true
    static let globCache = ConcurrentDictionary<string, Glob>()

    let globCheckFilepath (pattern: string) (path: string) =
        match globCache.TryGetValue(pattern) with
        | true, glob -> glob.IsMatch(path)
        | false, _ ->
            let glob = Glob.Parse(pattern)
            globCache.TryAdd(pattern, glob) |> ignore
            glob.IsMatch(path)

    let filepathToEntityType =
        function
        | x when globCheckFilepath "**/common/*.txt" x -> EntityType.Other
        | x when globCheckFilepath "**/common/agendas/*.txt" x -> EntityType.Agenda
        | x when globCheckFilepath "**/common/ambient_objects/*.txt" x -> EntityType.AmbientObjects
        | x when globCheckFilepath "**/common/anomalies/*.txt" x -> EntityType.Anomalies
        | x when globCheckFilepath "**/common/armies/*.txt" x -> EntityType.Armies
        | x when globCheckFilepath "**/common/army_attachments/*.txt" x -> EntityType.ArmyAttachments
        | x when globCheckFilepath "**/common/ascension_perks/*.txt" x -> EntityType.AscensionPerks
        | x when globCheckFilepath "**/common/attitudes/*.txt" x -> EntityType.Attitudes
        | x when globCheckFilepath "**/common/bombardment_stances/*.txt" x -> EntityType.BombardmentStances
        | x when globCheckFilepath "**/common/buildable_pops/*.txt" x -> EntityType.BuildablePops
        | x when globCheckFilepath "**/common/building_tags/*.txt" x -> EntityType.BuildingTags
        | x when globCheckFilepath "**/common/buildings/*.txt" x -> EntityType.Buildings
        | x when globCheckFilepath "**/common/button_effects/*.txt" x -> EntityType.ButtonEffects
        | x when globCheckFilepath "**/common/bypass/*.txt" x -> EntityType.Bypass
        | x when globCheckFilepath "**/common/casus_belli/*.txt" x -> EntityType.CasusBelli
        | x when globCheckFilepath "**/common/colors/*.txt" x -> EntityType.Colors
        | x when globCheckFilepath "**/common/component_flags/*.txt" x -> EntityType.ComponentFlags
        | x when globCheckFilepath "**/common/component_sets/*.txt" x -> EntityType.ComponentSets
        | x when globCheckFilepath "**/common/component_tags/*.txt" x -> EntityType.ComponentTags
        | x when globCheckFilepath "**/common/component_templates/*.txt" x -> EntityType.ComponentTemplates
        | x when globCheckFilepath "**/common/country_customization/*.txt" x -> EntityType.CountryCustomization
        | x when globCheckFilepath "**/common/country_types/*.txt" x -> EntityType.CountryTypes
        | x when globCheckFilepath "**/common/decisions/*.txt" x -> EntityType.Decisions
        | x when globCheckFilepath "**/common/deposits/*.txt" x -> EntityType.Deposits
        | x when globCheckFilepath "**/common/diplo_phrases/*.txt" x -> EntityType.DiploPhrases
        | x when globCheckFilepath "**/common/diplomatic_actions/*.txt" x -> EntityType.DiplomaticActions
        | x when globCheckFilepath "**/common/edicts/*.txt" x -> EntityType.Edicts
        | x when globCheckFilepath "**/common/ethics/*.txt" x -> EntityType.Ethics
        | x when globCheckFilepath "**/common/event_chains/*.txt" x -> EntityType.EventChains
        | x when globCheckFilepath "**/common/fallen_empires/*.txt" x -> EntityType.FallenEmpires
        | x when globCheckFilepath "**/common/game_rules/*.txt" x -> EntityType.GameRules
        | x when globCheckFilepath "**/common/global_ship_designs/*.txt" x -> EntityType.GlobalShipDesigns
        | x when globCheckFilepath "**/common/governments/*.txt" x -> EntityType.Governments
        | x when globCheckFilepath "**/common/governments/authorities/*.txt" x -> EntityType.Authorities
        | x when globCheckFilepath "**/common/governments/civics/*.txt" x -> EntityType.Civics
        | x when globCheckFilepath "**/common/graphical_culture/*.txt" x -> EntityType.GraphicalCulture
        | x when globCheckFilepath "**/common/mandates/*.txt" x -> EntityType.Mandates
        | x when globCheckFilepath "**/common/map_modes/*.txt" x -> EntityType.MapModes
        | x when globCheckFilepath "**/common/megastructures/*.txt" x -> EntityType.Megastructures
        | x when globCheckFilepath "**/common/name_lists/*.txt" x -> EntityType.NameLists
        | x when globCheckFilepath "**/common/notification_modifiers/*.txt" x -> EntityType.NotificationModifiers
        | x when globCheckFilepath "**/common/observation_station_missions/*.txt" x ->
            EntityType.ObservationStationMissions
        | x when globCheckFilepath "**/common/on_actions/*.txt" x -> EntityType.OnActions
        | x when globCheckFilepath "**/common/opinion_modifiers/*.txt" x -> EntityType.OpinionModifiers
        | x when globCheckFilepath "**/common/personalities/*.txt" x -> EntityType.Personalities
        | x when globCheckFilepath "**/common/planet_classes/*.txt" x -> EntityType.PlanetClasses
        | x when globCheckFilepath "**/common/planet_modifiers/*.txt" x -> EntityType.PlanetModifiers
        | x when globCheckFilepath "**/common/policies/*.txt" x -> EntityType.Policies
        | x when globCheckFilepath "**/common/pop_faction_types/*.txt" x -> EntityType.PopFactionTypes
        | x when globCheckFilepath "**/common/precursor_civilizations/*.txt" x -> EntityType.PrecursorCivilizations
        | x when globCheckFilepath "**/common/scripted_effects/*.txt" x -> EntityType.ScriptedEffects
        | x when globCheckFilepath "**/common/scripted_loc/*.txt" x -> EntityType.ScriptedLoc
        | x when globCheckFilepath "**/common/scripted_triggers/*.txt" x -> EntityType.ScriptedTriggers
        | x when globCheckFilepath "**/common/scripted_variables/*.txt" x -> EntityType.ScriptedVariables
        | x when globCheckFilepath "**/common/section_templates/*.txt" x -> EntityType.SectionTemplates
        | x when globCheckFilepath "**/common/sector_types/*.txt" x -> EntityType.SectorTypes
        | x when globCheckFilepath "**/common/ship_behaviors/*.txt" x -> EntityType.ShipBehaviors
        | x when globCheckFilepath "**/common/ship_sizes/*.txt" x -> EntityType.ShipSizes
        | x when globCheckFilepath "**/common/solar_system_initializers/**/*.txt" x ->
            EntityType.SolarSystemInitializers
        | x when globCheckFilepath "**\\common\\solar_system_initializers\\**\\*.txt" x ->
            EntityType.SolarSystemInitializers
        | x when globCheckFilepath "**/common/solar_system_initializers/*.txt" x -> EntityType.SolarSystemInitializers
        | x when globCheckFilepath "**/common/special_projects/*.txt" x -> EntityType.SpecialProjects
        | x when globCheckFilepath "**/common/species_archetypes/*.txt" x -> EntityType.SpeciesArchetypes
        | x when globCheckFilepath "**/common/species_classes/*.txt" x -> EntityType.SpeciesClasses
        | x when globCheckFilepath "**/common/species_names/*.txt" x -> EntityType.SpeciesNames
        | x when globCheckFilepath "**/common/species_rights/*.txt" x -> EntityType.SpeciesRights
        | x when globCheckFilepath "**/common/star_classes/*.txt" x -> EntityType.StarClasses
        | x when globCheckFilepath "**/common/starbase_buildings/*.txt" x -> EntityType.StarbaseBuilding
        | x when globCheckFilepath "**/common/starbase_levels/*.txt" x -> EntityType.StarbaseLevels
        | x when globCheckFilepath "**/common/starbase_modules/*.txt" x -> EntityType.StarbaseModules
        | x when globCheckFilepath "**/common/starbase_types/*.txt" x -> EntityType.StarbaseTypes
        | x when globCheckFilepath "**/common/spaceport_modules/*.txt" x -> EntityType.SpaceportModules
        | x when globCheckFilepath "**/common/start_screen_messages/*.txt" x -> EntityType.StartScreenMessages
        | x when globCheckFilepath "**/common/static_modifiers/*.txt" x -> EntityType.StaticModifiers
        | x when globCheckFilepath "**/common/strategic_resources/*.txt" x -> EntityType.StrategicResources
        | x when globCheckFilepath "**/common/subjects/*.txt" x -> EntityType.Subjects
        | x when globCheckFilepath "**/common/system_types/*.txt" x -> EntityType.SystemTypes
        | x when globCheckFilepath "**/common/technology/*.txt" x -> EntityType.Technology
        | x when globCheckFilepath "**/common/terraform/*.txt" x -> EntityType.Terraform
        | x when globCheckFilepath "**/common/tile_blockers/*.txt" x -> EntityType.TileBlockers
        | x when globCheckFilepath "**/common/tradition_categories/*.txt" x -> EntityType.TraditionCategories
        | x when globCheckFilepath "**/common/traditions/*.txt" x -> EntityType.Traditions
        | x when globCheckFilepath "**/common/traits/*.txt" x -> EntityType.Traits
        | x when globCheckFilepath "**/common/triggered_modifiers/*.txt" x -> EntityType.TriggeredModifiers
        | x when globCheckFilepath "**/common/war_demand_counters/*.txt" x -> EntityType.WarDemandCounters
        | x when globCheckFilepath "**/common/war_demand_types/*.txt" x -> EntityType.WarDemandTypes
        | x when globCheckFilepath "**/common/war_goals/*.txt" x -> EntityType.WarGoals
        | x when globCheckFilepath "**/events/*.txt" x -> EntityType.Events
        | x when globCheckFilepath "**/events/**/*.txt" x -> EntityType.Events
        | x when globCheckFilepath "**\\events\\**\\*.txt" x -> EntityType.Events
        | x when globCheckFilepath "**/map/galaxy/*.txt" x -> EntityType.MapGalaxy
        | x when globCheckFilepath "**/map/setup_scenarios/*.txt" x -> EntityType.MapSetupScenarios
        | x when globCheckFilepath "**/prescripted_countries/*.txt" x -> EntityType.PrescriptedCountries
        | x when globCheckFilepath "**/interface/**/*.gfx" x -> EntityType.Interface
        | x when globCheckFilepath "**\\interface\\**\\*.gfx" x -> EntityType.Interface
        | x when globCheckFilepath "**/interface/**/*.gui" x -> EntityType.Interface
        | x when globCheckFilepath "**\\interface\\**\\*.gui" x -> EntityType.Interface
        | x when globCheckFilepath "**/interface/*.gfx" x -> EntityType.Interface
        | x when globCheckFilepath "**/interface/*.gui" x -> EntityType.Interface
        | x when globCheckFilepath "**/gfx/**/*.gfx" x -> EntityType.GfxGfx
        | x when globCheckFilepath "**\\gfx\\**\\*.gfx" x -> EntityType.GfxGfx
        | x when globCheckFilepath "**/gfx/*.gfx" x -> EntityType.GfxGfx
        | x when globCheckFilepath "**/gfx/**/*.asset" x -> EntityType.GfxAsset
        | x when globCheckFilepath "**\\gfx\\**\\*.asset" x -> EntityType.GfxAsset
        | x when globCheckFilepath "**/gfx/*.asset" x -> EntityType.GfxAsset
        | _ -> EntityType.Other

    let fileMap = Dictionary<string, Resource>(52361)

    let entitiesMap = Dictionary<string, struct (Entity * Lazy<'T>)>(5306)

    let duration f =
        let timestamp = Stopwatch.GetTimestamp()
        let returnValue = f ()
        (returnValue, Stopwatch.GetElapsedTime(timestamp).TotalMilliseconds)

    let matchResult (scope: string, file: string, logicalpath: string, validate: bool, (parseResult, time)) =
        match parseResult with
        | Success(parsed, _, _) ->
            EntityResource(
                file,
                { scope = scope
                  filepath = file
                  logicalpath = logicalpath
                  validate = validate
                  result = Pass({ parseTime = time })
                  overwrite = Overwrite.No }
            ),
            parsed
        | Failure(msg, pe, _) ->
            EntityResource(
                file,
                { scope = scope
                  filepath = file
                  logicalpath = logicalpath
                  validate = validate
                  result =
                    Fail(
                        { error = msg
                          position = pe.Position
                          parseTime = time }
                    )
                  overwrite = Overwrite.No }
            ),
            []

    let shipProcess = STLProcess.shipProcess.ProcessNode

    let parseEntity ((file, statements): Resource * Statement list) =
        file,
        match file with
        | EntityResource(_,
                         { result = Pass _
                           filepath = f
                           validate = v
                           logicalpath = l }) ->
            let entityType = filepathToEntityType f
            let filename = Path.GetFileNameWithoutExtension f
            let entity = (shipProcess entityType filename (mkZeroFile f) statements)

            Some
                { filepath = f
                  logicalpath = l
                  entity = entity
                  rawEntity = entity
                  validate = v
                  entityType = entityType
                  overwrite = Overwrite.No }
        | _ -> None

    let changeEncoding (filestring: string) (source: System.Text.Encoding) (target: System.Text.Encoding) =
        let conv = System.Text.Encoding.Convert(source, target, source.GetBytes(filestring))
        target.GetString(conv)

    let parseFileInner (filetext: string) (filename: string) =
        let res = CKParser.parseString filetext (System.String.Intern(filename))

        match res with
        | Success _ -> res
        | Failure _ ->
            let res2 =
                CKParser.parseString
                    (changeEncoding filetext encoding fallbackencoding)
                    (System.String.Intern(filename))

            match res2 with
            | Success _ -> res2
            | Failure _ -> res

    let parseFileThenEntity (file: ResourceInput) =
        match file with
        | CachedResourceInput(r, e) -> r, Some e
        | EntityResourceInput e ->
            // log "%A %A" e.logicalpath e.filepath
            e
            |> ((fun f ->
                    f.scope,
                    f.filepath,
                    f.logicalpath,
                    f.validate,
                    (fun (t, t2) -> duration (fun () -> parseFileInner t2 (System.String.Intern(t)))) (
                        f.filepath,
                        f.filetext
                    ))
                >> matchResult)
            |> parseEntity
        | FileResourceInput f ->
            (FileResource(
                f.filepath,
                { scope = f.scope
                  filepath = f.filepath
                  logicalpath = f.logicalpath }
             ),
             [])
            |> parseEntity
        | FileWithContentResourceInput f ->
            (FileWithContentResource(
                f.filepath,
                { scope = f.scope
                  filepath = f.filepath
                  logicalpath = f.logicalpath
                  filetext = f.filetext
                  overwrite = Overwrite.No
                  extension = Path.GetExtension(f.filepath)
                  validate = f.validate }
             ),
             [])
            |> parseEntity


    let saveResults (resource, entity) =
        seq {
            match resource with
            | EntityResource(f, _) -> fileMap[f] <- resource
            | FileResource(f, _) -> fileMap[f] <- resource
            | FileWithContentResource(f, _) -> fileMap[f] <- resource

            match entity with
            | Some e ->
                let lazyi =
                    new System.Lazy<_>((fun () -> computedDataFunction e), LazyThreadSafetyMode.PublicationOnly)

                let item = struct (e, lazyi)
                // log "e %A %A %A" e.filepath e.logicalpath e.overwrite
                entitiesMap[e.filepath] <- item
                yield resource, Some item
            | None -> yield resource, None
        }

    let updateOverwrite () =
        let fileList = fileMap.Values.ToArray()

        let entities =
            fileList
            |> Array.choose (function
                | EntityResource(s, e) -> Some((s, e))
                | _ -> None)

        let processGroup (key, es: (string * EntityResource) array) =
            match es with
            | [| s, e |] -> [| s, { e with overwrite = Overwrite.No } |]
            | es ->
                let sorted =
                    es
                    |> Array.sortByDescending (fun (s, e) ->
                        if e.scope = "embedded" then ""
                        else if e.scope = "" then "ZZZZZZZZ"
                        else e.scope)

                let firstStr, entity = sorted[0]

                sorted[0] <-
                    (firstStr,
                     { entity with
                         overwrite = Overwrite.Overwrote })

                for i in 1 .. sorted.Length - 1 do
                    let s, entity = sorted[i]

                    sorted[i] <-
                        (s,
                         { entity with
                             overwrite = Overwrite.Overwritten })

                sorted

        let res =
            entities
            |> Array.groupBy (fun (_, e) -> e.logicalpath)
            |> Array.collect processGroup

        res |> Array.iter (fun (s, e) -> fileMap[s] <- EntityResource(s, e))

        let entityMap (s, e: EntityResource) =
            let found, result = entitiesMap.TryGetValue(s)

            if found then
                let struct (olde, oldl) = result

                entitiesMap[s] <-
                    struct ({ olde with
                                Entity.overwrite = e.overwrite },
                            oldl)

        res |> Array.iter entityMap

        let filesWithContent =
            fileList
            |> Array.choose (function
                | FileWithContentResource(s, e) -> Some((s, e))
                | _ -> None)

        let processGroup (key, es: (string * FileWithContentResource) array) =
            match es with
            | [| s, e |] -> [| s, { e with overwrite = Overwrite.No } |]
            | es ->
                let sorted =
                    es
                    |> Array.sortByDescending (fun (s, e) ->
                        if e.scope = "embedded" then ""
                        else if e.scope = "" then "ZZZZZZZZ"
                        else e.scope)

                let firstStr, entity = sorted[0]

                sorted[0] <-
                    (firstStr,
                     { entity with
                         overwrite = Overwrite.Overwrote })

                for i in 1 .. sorted.Length - 1 do
                    let s, entity = sorted[i]

                    sorted[i] <-
                        (s,
                         { entity with
                             overwrite = Overwrite.Overwritten })

                sorted

        let res =
            filesWithContent
            |> Array.groupBy (fun (s, e) -> e.logicalpath)
            |> Array.collect processGroup

        res |> Array.iter (fun (s, e) -> fileMap[s] <- FileWithContentResource(s, e))

    // log "print all"
    // entitiesMap |> Map.toList |> List.map fst |> List.sortBy id |> List.iter (log "%s")
    let updateInlineScripts (news: (Resource * struct (Entity * Lazy<'T>) option) list) =
        let inlinePath = "common/inline_scripts/"
        let inlinePathLength = inlinePath.Length

        let inlineScriptsMap =
            entitiesMap.Values
            |> Seq.filter (fun struct (e, _) -> e.overwrite <> Overwrite.Overwritten)
            |> Seq.map structFst
            |> Seq.filter (fun e -> e.logicalpath.StartsWith inlinePath)
            |> Seq.map (fun e -> ((e.logicalpath.Substring inlinePathLength).Replace(".txt", ""), e.rawEntity))
            |> Map.ofSeq

        let keyId = StringResource.stringManager.InternIdentifierToken "inline_script"

        let folder (node: Node) (acc: Child list) =
            if node.KeyId.lower = keyId.lower then
                NodeC node :: acc
            else
                node.LeafsById keyId.lower |> Seq.fold (fun state x -> LeafC x :: state) acc

        let updateEntity (e: Entity) =
            let allScriptRefs = ProcessCore.foldNode7 folder e.entity
            // allScriptRefs |> List.iter (function |LeafC l -> log $"a {l.ValueText} {l.Position}" |_ -> ())
            let nodeScriptRefs =
                allScriptRefs
                |> List.choose (function
                    | NodeC n -> Some n
                    | _ -> None)

            let leafScriptRefs =
                allScriptRefs
                |> List.choose (function
                    | LeafC l -> Some l
                    | _ -> None)

            let rec replaceCataFun (node: Node) =
                let childFun (child: Child) =
                    let addTrivia originalPosition =
                        function
                        | NodeC n -> n.Trivia <- Some { originalSource = Some originalPosition }
                        | LeafC l -> l.Trivia <- Some { originalSource = Some originalPosition }
                        | LeafValueC lv -> lv.Trivia <- Some { originalSource = Some originalPosition }
                        | ValueClauseC vc -> vc.Trivia <- Some { originalSource = Some originalPosition }
                        | CommentC _ -> ()

                    match child with
                    | NodeC n ->
                        let stringReplace (isParams: (string * string) seq) (key: string) =
                            isParams
                            |> Seq.fold (fun (key: string) (par, value) -> key.Replace(par, value)) key

                        let rec foldOverNode stringReplacer (node: Node) =
                            node.Key <- stringReplacer node.Key

                            let stringTokenReplace (token: StringTokens) =
                                let res = stringReplacer (token.GetString())
                                StringResource.stringManager.InternIdentifierToken res

                            node.Leaves
                            |> Seq.iter (fun (l: Leaf) ->
                                l.Key <- stringReplacer l.Key

                                l.Value
                                |> (function
                                | Value.String s -> l.Value <- String(stringTokenReplace s)
                                | Value.QString s -> l.Value <- QString(stringTokenReplace s)
                                | _ -> ()))

                            node.LeafValues
                            |> Seq.iter (fun (l: LeafValue) ->
                                l.Value
                                |> (function
                                | Value.String s -> l.Value <- String(stringTokenReplace s)
                                | Value.QString s -> l.Value <- QString(stringTokenReplace s)
                                | _ -> ()))

                            node.Nodes |> Seq.iter (foldOverNode stringReplacer)


                        if
                            nodeScriptRefs
                            |> List.exists (fun s -> s.Position.Equals(n.Position) && s.KeyId.lower = n.KeyId.lower)
                        then
                            let scriptName = (n.TagText "script")

                            let values =
                                n.Leaves
                                |> Seq.map (fun l -> "$" + l.Key + "$", l.ValueText)
                                |> Seq.where (fun (k, v) -> k.Length > 0 && v.Length > 0)

                            match inlineScriptsMap |> Map.tryFind scriptName with
                            | Some scriptNode ->
                                let newScriptNode = STLProcess.cloneNode scriptNode
                                foldOverNode (stringReplace values) newScriptNode

                                newScriptNode.AllArray
                                |> Seq.map (fun x ->
                                    addTrivia n.Position x
                                    x)
                            // |Some scriptNode -> [CommentC (n.Position, $"Dummy comment to replace empty inline_script {scriptName}")]
                            | _ ->
                                [ LeafValueC(
                                      LeafValue(
                                          Value.String(
                                              stringManager.InternIdentifierToken $"Missing inline_script {scriptName}"
                                          ),
                                          n.Position
                                      )
                                  ) ]
                        else
                            replaceCataFun n
                            [ NodeC n ]
                    | LeafC l ->
                        // log $"b {l.ValueText} {l.Position}"
                        if
                            leafScriptRefs
                            |> List.exists (fun s ->
                                s.Position.Equals(l.Position)
                                && s.KeyId = l.KeyId
                                && s.ValueId.lower = l.ValueId.lower)
                        then
                            let scriptName = l.ValueText

                            match inlineScriptsMap |> Map.tryFind scriptName with
                            | Some scriptNode ->
                                let newScriptNode = STLProcess.cloneNode scriptNode

                                newScriptNode.AllArray
                                |> Seq.map (fun x ->
                                    addTrivia l.Position x
                                    x)
                            // |Some scriptNode -> [CommentC (l.Position, $"Dummy comment to replace empty inline_script {scriptName}")]
                            | _ ->
                                [ LeafValueC(
                                      LeafValue(
                                          Value.String(
                                              stringManager.InternIdentifierToken $"Missing inline_script {scriptName}"
                                          ),
                                          l.Position
                                      )
                                  ) ]
                        else
                            [ LeafC l ]
                    | x -> [ x ]

                node.AllArray <- node.AllArray |> Seq.collect childFun |> Array.ofSeq

            if List.isEmpty allScriptRefs then
                None
            else
                let newNode = STLProcess.cloneNode e.entity
                replaceCataFun newNode
                Some newNode

        news
        |> Seq.map (function
            | resource, Some struct (oldE, oldLazy) ->
                let maxIter = 5

                let rec updateInner entity i =
                    match i, updateEntity entity with
                    | i, Some newEntity when i = maxIter -> Some newEntity
                    | i, Some newEntity -> updateInner { entity with entity = newEntity } (i + 1)
                    | 0, None -> None
                    | _, None -> Some entity.entity

                let updatedE = updateInner oldE 0

                match updatedE with
                | Some newNode ->
                    let newE = { oldE with entity = newNode }

                    let lazyi =
                        System.Lazy<_>((fun () -> computedDataFunction newE), LazyThreadSafetyMode.PublicationOnly)

                    let item = struct (newE, lazyi)
                    entitiesMap[newE.filepath] <- item
                    resource, Some item
                | None -> resource, Some struct (oldE, oldLazy)
            | resource, None -> resource, None)

    let forceRulesData () =
        entitiesMap.Values
        |> PSeq.iter (fun (struct (e, l)) -> computedDataUpdateFunction e (l.Force()))

    let forceEagerData () =
        entitiesMap.Values.ToArray()
        |> (fun array ->
            Array.randomShuffleInPlace array
            array)
        |> PSeq.iter (fun (struct (_, l)) -> (l.Force() |> ignore))

    let forceRecompute () =
        for pair in entitiesMap do
            let struct (entity, _) = pair.Value

            let lazyi =
                System.Lazy<_>((fun () -> computedDataFunction entity), LazyThreadSafetyMode.PublicationOnly)

            entitiesMap[pair.Key] <- struct (entity, lazyi)

        let _ = Task.Run(fun () -> forceEagerData ())
        ()

    let updateFiles files =
        let news =
            files
            |> PSeq.ofList
            |> PSeq.map parseFileThenEntity
            |> Seq.collect saveResults
            |> Seq.toList

        updateOverwrite ()
        let mutable res = news

        if enableInlineScripts then
            res <- updateInlineScripts news |> Seq.toList

        let _ = Task.Run(fun () -> forceEagerData ())
        res

    let updateFile file =
        let res = updateFiles [ file ]

        if res.Length > 1 then
            log (sprintf "File %A returned multiple resources" file)
        else
            ()

        res.[0]

    let getResources () = fileMap.Values |> List.ofSeq

    let validatableFiles () =
        fileMap.Values
        |> List.ofSeq
        |> List.choose (function
            | EntityResource(_, e) -> Some e
            | _ -> None)
        |> List.filter (fun f -> f.validate)

    let allEntities () =
        entitiesMap.Values
        |> Seq.filter (fun struct (e, _) -> e.overwrite <> Overwrite.Overwritten)
        |> Seq.toList

    let validatableEntities () =
        entitiesMap.Values
        |> Seq.filter (fun struct (e, _) -> e.overwrite <> Overwrite.Overwritten && e.validate)
        |> Seq.toList

    let getFileNames () : string array =
        fileMap.Values
        |> Seq.map (function
            | EntityResource(_, r) -> r.logicalpath
            | FileResource(_, r) -> r.logicalpath
            | FileWithContentResource(_, r) -> r.logicalpath)
        |> Seq.toArray

    member _.ManualProcessResource = parseFileThenEntity >> snd

    member _.ManualProcess (filename: string) (filetext: string) =
        let parsed = CKParser.parseString filetext filename

        match parsed with
        | Failure _ -> None
        | Success(s, _, _) ->
            let filenamenopath = Path.GetFileNameWithoutExtension filename
            Some(shipProcess EntityType.Other filenamenopath (mkZeroFile filename) s)

    member _.Api =
        { new IResourceAPI<'T> with
            member _.UpdateFiles = updateFiles
            member _.UpdateFile = updateFile
            member _.GetResources = getResources
            member _.ValidatableFiles = validatableFiles
            member _.AllEntities = allEntities
            member _.ValidatableEntities = validatableEntities
            member _.ForceRecompute() = forceRecompute ()
            member _.ForceRulesDataGenerate() = forceRulesData ()
            member _.GetFileNames = getFileNames }
