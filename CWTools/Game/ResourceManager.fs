namespace CWTools.Games
open CWTools.Process
open FSharp.Collections.ParallelSeq
open FParsec
open System.IO
open CWTools.Parser
open DotNet.Globbing
open CWTools.Common.STLConstants



type EntityResourceInput =
    {
        scope : string
        filepath : string
        filetext : string
        validate : bool
    }
type FileResourceInput =
    {
        scope : string
        filepath : string
    }
type ResourceInput =
    |EntityResourceInput of EntityResourceInput
    |FileResourceInput of FileResourceInput


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

type EntityResource =
    {
        scope : string
        filepath : string
        result : FileResult
        validate : bool
    }

type FileResource =
    {
        scope : string
        filepath : string   
    }

type Resource =
    |EntityResource of string * EntityResource
    |FileResource of string * FileResource

type Entity =
    {
        filepath : string
        entity : Node
        validate : bool
        entityType : EntityType
    }

type UpdateFile = ResourceInput -> Entity list
type UpdateFiles = ResourceInput list -> Entity list
type GetResources = unit -> Resource list
type ValidatableFiles = unit -> EntityResource list
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
    let memoize keyFunction memFunction =
        let dict = new System.Collections.Generic.Dictionary<_,_>()
        fun n ->
            match dict.TryGetValue(keyFunction(n)) with
            | (true, v) -> v
            | _ ->
                let temp = memFunction(n)
                dict.Add(keyFunction(n), temp)
                temp
    let globCheckFilepathI (pattern : string) =
        let glob = Glob.Parse(pattern)
        (fun (p : string) -> glob.IsMatch(p))

    let globCheckFilepath pattern = (memoize id globCheckFilepathI) pattern

    let filepathToEntityType =
        function
        |x when globCheckFilepath "**/common/agendas/*.txt" x -> EntityType.Agenda
        |x when globCheckFilepath "**/common/ambient_objects/*.txt" x -> EntityType.AmbientObjects
        |x when globCheckFilepath "**/common/anomalies/*.txt" x -> EntityType.Anomalies
        |x when globCheckFilepath "**/common/armies/*.txt" x -> EntityType.Armies
        |x when globCheckFilepath "**/common/army_attachments/*.txt" x -> EntityType.ArmyAttachments
        |x when globCheckFilepath "**/common/ascension_perks/*.txt" x -> EntityType.AscensionPerks
        |x when globCheckFilepath "**/common/attitudes/*.txt" x -> EntityType.Attitudes
        |x when globCheckFilepath "**/common/bombardment_stances/*.txt" x -> EntityType.BombardmentStances
        |x when globCheckFilepath "**/common/buildable_pops/*.txt" x -> EntityType.BuildablePops
        |x when globCheckFilepath "**/common/building_tags/*.txt" x -> EntityType.BuildingTags
        |x when globCheckFilepath "**/common/buildings/*.txt" x -> EntityType.Buildings
        |x when globCheckFilepath "**/common/button_effects/*.txt" x -> EntityType.ButtonEffects
        |x when globCheckFilepath "**/common/bypass/*.txt" x -> EntityType.Bypass
        |x when globCheckFilepath "**/common/casus_belli/*.txt" x -> EntityType.CasusBelli
        |x when globCheckFilepath "**/common/colors/*.txt" x -> EntityType.Colors
        |x when globCheckFilepath "**/common/component_flags/*.txt" x -> EntityType.ComponentFlags
        |x when globCheckFilepath "**/common/component_sets/*.txt" x -> EntityType.ComponentSets
        |x when globCheckFilepath "**/common/component_tags/*.txt" x -> EntityType.ComponentTags
        |x when globCheckFilepath "**/common/component_templates/*.txt" x -> EntityType.ComponentTemplates
        |x when globCheckFilepath "**/common/country_customization/*.txt" x -> EntityType.CountryCustomization
        |x when globCheckFilepath "**/common/country_types/*.txt" x -> EntityType.CountryTypes
        |x when globCheckFilepath "**/common/deposits/*.txt" x -> EntityType.Deposits
        |x when globCheckFilepath "**/common/diplo_phrases/*.txt" x -> EntityType.DiploPhrases
        |x when globCheckFilepath "**/common/diplomatic_actions/*.txt" x -> EntityType.DiplomaticActions
        |x when globCheckFilepath "**/common/edicts/*.txt" x -> EntityType.Edicts
        |x when globCheckFilepath "**/common/ethics/*.txt" x -> EntityType.Ethics
        |x when globCheckFilepath "**/common/event_chains/*.txt" x -> EntityType.EventChains
        |x when globCheckFilepath "**/common/fallen_empires/*.txt" x -> EntityType.FallenEmpires
        |x when globCheckFilepath "**/common/game_rules/*.txt" x -> EntityType.GameRules
        |x when globCheckFilepath "**/common/global_ship_designs/*.txt" x -> EntityType.GlobalShipDesigns
        |x when globCheckFilepath "**/common/governments/*.txt" x -> EntityType.Governments
        |x when globCheckFilepath "**/common/governments/authorities/*.txt" x -> EntityType.Authorities
        |x when globCheckFilepath "**/common/governments/civics/*.txt" x -> EntityType.Civics
        |x when globCheckFilepath "**/common/graphical_culture/*.txt" x -> EntityType.GraphicalCulture
        |x when globCheckFilepath "**/common/mandates/*.txt" x -> EntityType.Mandates
        |x when globCheckFilepath "**/common/map_modes/*.txt" x -> EntityType.MapModes
        |x when globCheckFilepath "**/common/megastructures/*.txt" x -> EntityType.Megastructures
        |x when globCheckFilepath "**/common/name_lists/*.txt" x -> EntityType.NameLists
        |x when globCheckFilepath "**/common/notification_modifiers/*.txt" x -> EntityType.NotificationModifiers
        |x when globCheckFilepath "**/common/observation_station_missions/*.txt" x -> EntityType.ObservationStationMissions
        |x when globCheckFilepath "**/common/on_actions/*.txt" x -> EntityType.OnActions
        |x when globCheckFilepath "**/common/opinion_modifiers/*.txt" x -> EntityType.OpinionModifiers
        |x when globCheckFilepath "**/common/personalities/*.txt" x -> EntityType.Personalities
        |x when globCheckFilepath "**/common/planet_classes/*.txt" x -> EntityType.PlanetClasses
        |x when globCheckFilepath "**/common/planet_modifiers/*.txt" x -> EntityType.PlanetModifiers
        |x when globCheckFilepath "**/common/policies/*.txt" x -> EntityType.Policies
        |x when globCheckFilepath "**/common/pop_faction_types/*.txt" x -> EntityType.PopFactionTypes
        |x when globCheckFilepath "**/common/precursor_civilizations/*.txt" x -> EntityType.PrecursorCivilizations
        |x when globCheckFilepath "**/common/scripted_effects/*.txt" x -> EntityType.ScriptedEffects
        |x when globCheckFilepath "**/common/scripted_loc/*.txt" x -> EntityType.ScriptedLoc
        |x when globCheckFilepath "**/common/scripted_triggers/*.txt" x -> EntityType.ScriptedTriggers
        |x when globCheckFilepath "**/common/scripted_variables/*.txt" x -> EntityType.ScriptedVariables
        |x when globCheckFilepath "**/common/section_templates/*.txt" x -> EntityType.SectionTemplates
        |x when globCheckFilepath "**/common/sector_types/*.txt" x -> EntityType.SectorTypes
        |x when globCheckFilepath "**/common/ship_behaviors/*.txt" x -> EntityType.ShipBehaviors
        |x when globCheckFilepath "**/common/ship_sizes/*.txt" x -> EntityType.ShipSizes
        |x when globCheckFilepath "**\\common\\solar_system_initializers\\**\\*.txt" x -> EntityType.SolarSystemInitializers
        |x when globCheckFilepath "**/common/solar_system_initializers/*.txt" x -> EntityType.SolarSystemInitializers
        |x when globCheckFilepath "**/common/special_projects/*.txt" x -> EntityType.SpecialProjects
        |x when globCheckFilepath "**/common/species_archetypes/*.txt" x -> EntityType.SpeciesArchetypes
        |x when globCheckFilepath "**/common/species_classes/*.txt" x -> EntityType.SpeciesClasses
        |x when globCheckFilepath "**/common/species_names/*.txt" x -> EntityType.SpeciesNames
        |x when globCheckFilepath "**/common/species_rights/*.txt" x -> EntityType.SpeciesRights
        |x when globCheckFilepath "**/common/star_classes/*.txt" x -> EntityType.StarClasses
        |x when globCheckFilepath "**/common/starbase_building/*.txt" x -> EntityType.StarbaseBuilding
        |x when globCheckFilepath "**/common/starbase_levels/*.txt" x -> EntityType.StarbaseLevels
        |x when globCheckFilepath "**/common/starbase_modules/*.txt" x -> EntityType.StarbaseModules
        |x when globCheckFilepath "**/common/starbase_types/*.txt" x -> EntityType.StarbaseTypes
        |x when globCheckFilepath "**/common/spaceport_modules/*.txt" x -> EntityType.SpaceportModules
        |x when globCheckFilepath "**/common/start_screen_messages/*.txt" x -> EntityType.StartScreenMessages
        |x when globCheckFilepath "**/common/static_modifiers/*.txt" x -> EntityType.StaticModifiers
        |x when globCheckFilepath "**/common/strategic_resources/*.txt" x -> EntityType.StrategicResources
        |x when globCheckFilepath "**/common/subjects/*.txt" x -> EntityType.Subjects
        |x when globCheckFilepath "**/common/system_types/*.txt" x -> EntityType.SystemTypes
        |x when globCheckFilepath "**/common/technology/*.txt" x -> EntityType.Technology
        |x when globCheckFilepath "**/common/terraform/*.txt" x -> EntityType.Terraform
        |x when globCheckFilepath "**/common/tile_blockers/*.txt" x -> EntityType.TileBlockers
        |x when globCheckFilepath "**/common/tradition_categories/*.txt" x -> EntityType.TraditionCategories
        |x when globCheckFilepath "**/common/traditions/*.txt" x -> EntityType.Traditions
        |x when globCheckFilepath "**/common/traits/*.txt" x -> EntityType.Traits
        |x when globCheckFilepath "**/common/triggered_modifiers/*.txt" x -> EntityType.TriggeredModifiers
        |x when globCheckFilepath "**/common/war_demand_counters/*.txt" x -> EntityType.WarDemandCounters
        |x when globCheckFilepath "**/common/war_demand_types/*.txt" x -> EntityType.WarDemandTypes
        |x when globCheckFilepath "**/common/war_goals/*.txt" x -> EntityType.WarGoals
        |x when globCheckFilepath "**/events/*.txt" x -> EntityType.Events
        |x when globCheckFilepath "**/map/galaxy/*.txt" x -> EntityType.MapGalaxy
        |x when globCheckFilepath "**/map/setup_scenarios/*.txt" x -> EntityType.MapSetupScenarios
        |x when globCheckFilepath "**/prescripted_countries/*.txt" x -> EntityType.PrescriptedCountries
        |x when globCheckFilepath "**/interface/*.txt" x -> EntityType.Interface
        |x when globCheckFilepath "**/gfx/*.txt" x -> EntityType.Gfx
        |_ -> EntityType.Other

    let mutable fileMap : Map<string, Resource> = Map.empty
    let mutable entitiesMap : Map<string, Entity> = Map.empty
    let duration f = 
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()
        let returnValue = f()
        (returnValue  , timer.ElapsedMilliseconds) 

    let matchResult (scope : string, file : string, validate : bool, (parseResult, time)) = 
        match parseResult with
        | Success(parsed, _, _) -> EntityResource (file, { scope = scope; filepath = file; validate = validate; result = Pass({statements = parsed; parseTime = time}) })
        | Failure(msg, pe, _) -> EntityResource (file, { scope = scope; filepath = file; validate = validate; result = Fail({error = msg; position = pe.Position; parseTime = time})})

    let parseFile (file : ResourceInput) = 
         match file with
                    |EntityResourceInput e -> e |> ((fun f -> f.scope, f.filepath, f.validate, (fun (t, t2) -> duration (fun () -> CKParser.parseString t2 t)) (f.filepath, f.filetext)) >> matchResult)
                    |FileResourceInput f -> FileResource (f.filepath, { scope = f.scope; filepath = f.filepath })
        

    let parseEntity (file : Resource) =
        file,
                match file with
                |EntityResource (_, {result = Pass(s); filepath = f; validate = v}) ->
                    let entityType = filepathToEntityType f
                    Some { filepath = f; entity = (STLProcess.shipProcess.ProcessNode<Node>(entityType) "root" (Position.File(f)) s.statements); validate = v; entityType = entityType}
                |_ -> None        

    let saveResults (resource, entity) =
        seq {
            fileMap <- 
                match resource with
                |EntityResource (f, _) -> fileMap.Add(f, resource) 
                |FileResource (f, _) -> fileMap.Add(f, resource)
            match entity with
            |Some e -> entitiesMap <- entitiesMap.Add(e.filepath, e); yield e
            |None -> ()
        }

    let updateFiles files =
        files |> PSeq.ofList |> PSeq.map (parseFile >> parseEntity) |> Seq.collect saveResults |> Seq.toList

    let getResources() = fileMap |> Map.toList |> List.map snd
    let validatableFiles() = fileMap |> Map.toList |> List.map snd |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None) |> List.filter (fun f -> f.validate)
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