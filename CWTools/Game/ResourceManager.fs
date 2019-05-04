namespace CWTools.Games
open CWTools.Process
open FSharp.Collections.ParallelSeq
open FParsec
open System.IO
open CWTools.Parser
open DotNet.Globbing
open CWTools.Common.STLConstants
open CWTools.Parser.Types
open CWTools.Utilities.Position
open CWTools.Utilities.Utils
open CWTools.Utilities

type ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks) =
    member val Cache : Map<string, obj list> = Map.empty with get, set
    member val WithRulesData : bool = withRulesData with get,set
    member val Referencedtypes : Map<string, (string  * range) list> option = referencedtypes with get, set
    member val Definedvariables : Map<string, (string * range) list> option = definedvariable with get, set
    member val EffectBlocks : Node list option = effectBlocks with get, set
    member val TriggerBlocks : Node list option = triggersBlocks with get, set

type EU4ComputedData(referencedtypes, definedvariable, scriptedeffectparams, withRulesData, effectBlocks, triggersBlocks) =
    inherit ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks)
    member __.ScriptedEffectParams : string list option = scriptedeffectparams
type HOI4ComputedData = ComputedData
type CK2ComputedData = ComputedData
type IRComputedData = ComputedData
type VIC2ComputedData = ComputedData
//     inherit ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks)
// type CK2ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks) =
//     inherit ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks)
// type IRComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks) =
//     inherit ComputedData(referencedtypes, definedvariable, withRulesData, effectBlocks, triggersBlocks)


type PassFileResult = {
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

type Overwrite =
    |No
    |Overwrote
    |Overwritten
type EntityResource =
    {
        scope : string
        filepath : string
        logicalpath : string
        result : FileResult
        overwrite : Overwrite
        validate : bool
    }

type FileResource =
    {
        scope : string
        filepath : string
        logicalpath : string
    }

type FileWithContentResource =
    {
        scope : string
        filetext : string
        filepath : string
        extension : string
        logicalpath : string
        overwrite : Overwrite
        validate : bool
    }

type Resource =
    |EntityResource of string * EntityResource
    |FileResource of string * FileResource
    |FileWithContentResource of string * FileWithContentResource

type EntityResourceInput =
    {
        scope : string
        filepath : string
        logicalpath : string
        filetext : string
        validate : bool
    }
type FileResourceInput =
    {
        scope : string
        filepath : string
        logicalpath : string
    }

type FileWithContentResourceInput =
    {
        scope : string
        filetext : string
        filepath : string
        logicalpath : string
        validate : bool
    }
// [<Struct>]
type Entity =
    {
        filepath : string
        logicalpath : string
        entity : Node
        validate : bool
        entityType : EntityType
        overwrite : Overwrite
    }
    override x.ToString() = sprintf "%s %s %b" x.filepath x.logicalpath x.validate

type CachedResourceData = {
    resources : (Resource * Entity) list
    files : (string * string) list
    fileIndexTable : FileIndexTable
    stringResourceManager : StringResourceManager
}
type ResourceInput =
    |EntityResourceInput of EntityResourceInput
    |FileResourceInput of FileResourceInput
    |CachedResourceInput of Resource * Entity
    |FileWithContentResourceInput of FileWithContentResourceInput




type UpdateFile<'T> = ResourceInput -> (Resource * struct (Entity * Lazy<'T>) option)
type UpdateFiles<'T> = ResourceInput list -> (Resource *  struct (Entity * Lazy<'T>) option) list
type GetResources = unit -> Resource list
type ValidatableFiles = unit -> EntityResource list
type AllEntities<'T> = unit -> struct (Entity * Lazy<'T>) list
type ValidatableEntities<'T> = unit -> struct (Entity * Lazy<'T>) list
type GetFileNames = unit -> string list

type IResourceAPI<'T when 'T :> ComputedData > =
    abstract UpdateFiles : UpdateFiles<'T>
    abstract UpdateFile : UpdateFile<'T>
    abstract GetResources : GetResources
    abstract ValidatableFiles : ValidatableFiles
    abstract AllEntities : AllEntities<'T>
    abstract ValidatableEntities : ValidatableEntities<'T>
    abstract ForceRecompute : unit -> unit
    abstract ForceRulesDataGenerate : unit -> unit
    abstract GetFileNames : GetFileNames

type ResourceManager<'T when 'T :> ComputedData> (computedDataFunction : (Entity -> 'T), computedDataUpdateFunction : (Entity -> 'T -> unit), encoding, fallbackencoding) =
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
        let options = new GlobOptions();
        options.Evaluation.CaseInsensitive <- true;
        let glob = Glob.Parse(pattern, options)
        (fun (p : string) -> glob.IsMatch(p))

    let globCheckFilepath pattern = (memoize id globCheckFilepathI) pattern

    let filepathToEntityType =
        function
        |x when globCheckFilepath "**/common/*.txt" x -> EntityType.Other
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
        |x when globCheckFilepath "**/common/decisions/*.txt" x -> EntityType.Decisions
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
        |x when globCheckFilepath "**/common/solar_system_initializers/**/*.txt" x -> EntityType.SolarSystemInitializers
        |x when globCheckFilepath "**\\common\\solar_system_initializers\\**\\*.txt" x -> EntityType.SolarSystemInitializers
        |x when globCheckFilepath "**/common/solar_system_initializers/*.txt" x -> EntityType.SolarSystemInitializers
        |x when globCheckFilepath "**/common/special_projects/*.txt" x -> EntityType.SpecialProjects
        |x when globCheckFilepath "**/common/species_archetypes/*.txt" x -> EntityType.SpeciesArchetypes
        |x when globCheckFilepath "**/common/species_classes/*.txt" x -> EntityType.SpeciesClasses
        |x when globCheckFilepath "**/common/species_names/*.txt" x -> EntityType.SpeciesNames
        |x when globCheckFilepath "**/common/species_rights/*.txt" x -> EntityType.SpeciesRights
        |x when globCheckFilepath "**/common/star_classes/*.txt" x -> EntityType.StarClasses
        |x when globCheckFilepath "**/common/starbase_buildings/*.txt" x -> EntityType.StarbaseBuilding
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
        |x when globCheckFilepath "**/events/**/*.txt" x -> EntityType.Events
        |x when globCheckFilepath "**\\events\\**\\*.txt" x -> EntityType.Events
        |x when globCheckFilepath "**/map/galaxy/*.txt" x -> EntityType.MapGalaxy
        |x when globCheckFilepath "**/map/setup_scenarios/*.txt" x -> EntityType.MapSetupScenarios
        |x when globCheckFilepath "**/prescripted_countries/*.txt" x -> EntityType.PrescriptedCountries
        |x when globCheckFilepath "**/interface/**/*.gfx" x -> EntityType.Interface
        |x when globCheckFilepath "**\\interface\\**\\*.gfx" x -> EntityType.Interface
        |x when globCheckFilepath "**/interface/**/*.gui" x -> EntityType.Interface
        |x when globCheckFilepath "**\\interface\\**\\*.gui" x -> EntityType.Interface
        |x when globCheckFilepath "**/interface/*.gfx" x -> EntityType.Interface
        |x when globCheckFilepath "**/interface/*.gui" x -> EntityType.Interface
        |x when globCheckFilepath "**/gfx/**/*.gfx" x -> EntityType.GfxGfx
        |x when globCheckFilepath "**\\gfx\\**\\*.gfx" x -> EntityType.GfxGfx
        |x when globCheckFilepath "**/gfx/*.gfx" x -> EntityType.GfxGfx
        |x when globCheckFilepath "**/gfx/**/*.asset" x -> EntityType.GfxAsset
        |x when globCheckFilepath "**\\gfx\\**\\*.asset" x -> EntityType.GfxAsset
        |x when globCheckFilepath "**/gfx/*.asset" x -> EntityType.GfxAsset
        |_ -> EntityType.Other

    let mutable fileMap : Map<string, Resource> = Map.empty
    let mutable entitiesMap : Map<string, struct (Entity * Lazy<'T>)> = Map.empty
    let duration f =
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()
        let returnValue = f()
        (returnValue  , timer.ElapsedMilliseconds)

    let matchResult (scope : string, file : string, logicalpath : string, validate : bool, (parseResult, time)) =
        match parseResult with
        | Success(parsed, _, _) -> EntityResource (file, { scope = scope; filepath = file; logicalpath = logicalpath; validate = validate; result = Pass({parseTime = time}); overwrite = No }), parsed
        | Failure(msg, pe, _) -> EntityResource (file, { scope = scope; filepath = file; logicalpath = logicalpath; validate = validate; result = Fail({error = msg; position = pe.Position; parseTime = time}); overwrite = No }), []

    // let parseFile (file : ResourceInput) =
    //      match file with
    //                 |CachedResourceInput (r, s) -> r, s
    //                 |EntityResourceInput e ->
    //                     // log "%A %A" e.logicalpath e.filepath
    //                     e |> ((fun f -> f.scope, f.filepath, f.logicalpath, f.validate, (fun (t, t2) -> duration (fun () -> CKParser.parseString t2 (System.String.Intern(t)))) (f.filepath, f.filetext)) >> matchResult)
    //                 |FileResourceInput f -> FileResource (f.filepath, { scope = f.scope; filepath = f.filepath; logicalpath = f.logicalpath }), []

    let shipProcess = STLProcess.shipProcess.ProcessNode
    let parseEntity ((file, statements) : Resource * Statement list) =
        file,
                match file with
                |EntityResource (_, {result = Pass(s); filepath = f; validate = v; logicalpath = l}) ->
                    let entityType = filepathToEntityType f
                    let filename = Path.GetFileNameWithoutExtension f
                    Some { filepath = f; logicalpath = l; entity = (shipProcess entityType filename (mkZeroFile f) (statements)); validate = v; entityType = entityType; overwrite = No}
                |_ -> None

    let changeEncoding (filestring : string) (source : System.Text.Encoding) (target : System.Text.Encoding) =
        let conv = System.Text.Encoding.Convert(source, target, source.GetBytes(filestring))
        target.GetString(conv)

    let parseFileInner (filetext : string) (filename : string) =
        let res = CKParser.parseString filetext (System.String.Intern(filename))
        match res with
        | Success(_,_,_) -> res
        | Failure(_,_,_) ->
            let res2 = CKParser.parseString (changeEncoding filetext encoding fallbackencoding) (System.String.Intern(filename))
            match res2 with
            |Success(_,_,_) -> res2
            |Failure(_,_,_) -> res
    let parseFileThenEntity (file : ResourceInput) =
        match file with
            |CachedResourceInput (r, e) -> r, Some e
            |EntityResourceInput e ->
                // log "%A %A" e.logicalpath e.filepath
                e |> ((fun f -> f.scope, f.filepath, f.logicalpath, f.validate, (fun (t, t2) -> duration (fun () -> parseFileInner t2 (System.String.Intern(t)))) (f.filepath, f.filetext)) >> matchResult)
                    |> parseEntity
            |FileResourceInput f ->
                (FileResource (f.filepath, { scope = f.scope; filepath = f.filepath; logicalpath = f.logicalpath }), [])
                 |> parseEntity
            |FileWithContentResourceInput f ->
                (FileWithContentResource (f.filepath, { scope = f.scope; filepath = f.filepath; logicalpath = f.logicalpath; filetext = f.filetext; overwrite = No; extension = Path.GetExtension(f.filepath); validate = f.validate}), [])
                 |> parseEntity


    let saveResults (resource, entity) =
        seq {
            fileMap <-
                match resource with
                |EntityResource (f, _) -> fileMap.Add(f, resource)
                |FileResource (f, _) -> fileMap.Add(f, resource)
                |FileWithContentResource (f, _) -> fileMap.Add(f, resource)
            match entity with
            |Some e ->
                let item = struct(e, lazy (computedDataFunction e))
                // log "e %A %A %A" e.filepath e.logicalpath e.overwrite
                entitiesMap <- entitiesMap.Add(e.filepath, item)
                yield resource, Some item
            |None -> yield resource, None
        }

    let updateOverwrite() =
        let filelist = fileMap |> Map.toList |> List.map snd
        let entities = filelist |> List.choose (function |EntityResource (s, e) -> Some ((s, e)) |_ -> None)
        let processGroup (key, (es : (string * EntityResource) list)) =
            match es with
            | [s,e] -> [s, {e with overwrite = No}]
            | es ->
                let sorted = es |> List.sortByDescending (fun (s, e) -> if e.scope = "embedded" then "" else if e.scope = "" then "ZZZZZZZZ" else e.scope)
                let first = sorted |> List.head |> (fun (s, h) -> s, {h with overwrite = Overwrite.Overwrote})
                // log "overwrote: %s" (first |> fst)
                let rest = sorted |> List.skip 1 |> List.map (fun (s, r) -> s, {r with overwrite = Overwrite.Overwritten})
                // rest |> List.iter (fun (s, _) -> log "overwritten: %s" s)
                first::rest
        let res = entities |> List.groupBy (fun (s, e) -> e.logicalpath)
                           |> List.collect processGroup
        fileMap <- res |> List.fold (fun fm (s, e) -> fm.Add(s, EntityResource (s, e))) fileMap
        let entityMap em (s, (e : EntityResource)) =
            match Map.tryFind s em with
            |None -> em
            |Some struct (olde, oldl) ->
                 em.Add(s, struct ({olde with Entity.overwrite = e.overwrite}, oldl))
        entitiesMap <- res |> List.fold entityMap entitiesMap
        let filesWithContent = filelist |> List.choose (function |FileWithContentResource (s, e) -> Some ((s, e)) |_ -> None)
        let processGroup (key, (es : (string * FileWithContentResource) list)) =
            match es with
            | [s,e] -> [s, {e with overwrite = No}]
            | es ->
                let sorted = es |> List.sortByDescending (fun (s, e) -> if e.scope = "embedded" then "" else if e.scope = "" then "ZZZZZZZZ" else e.scope)
                let first = sorted |> List.head |> (fun (s, h) -> s, {h with overwrite = Overwrite.Overwrote})
                // log "overwrote: %s" (first |> fst)
                let rest = sorted |> List.skip 1 |> List.map (fun (s, r) -> s, {r with overwrite = Overwrite.Overwritten})
                // rest |> List.iter (fun (s, _) -> log "overwritten: %s" s)
                first::rest
        let res = filesWithContent |> List.groupBy (fun (s, e) -> e.logicalpath)
                           |> List.collect processGroup
        fileMap <- res |> List.fold (fun fm (s, e) -> fm.Add(s, FileWithContentResource (s, e))) fileMap

        // log "print all"
        // entitiesMap |> Map.toList |> List.map fst |> List.sortBy id |> List.iter (log "%s")
    let forceRulesData() =
        entitiesMap |> Map.toSeq |> PSeq.iter (fun (_,(struct (e, l))) -> computedDataUpdateFunction e (l.Force()))
    let forceRecompute() =
        entitiesMap <- entitiesMap |> Map.map (fun _ (struct (e, _)) -> struct (e, lazy (computedDataFunction e)))

    let updateFiles files =
        let news = files |> PSeq.ofList |> PSeq.map (parseFileThenEntity) |> Seq.collect saveResults |> Seq.toList
        updateOverwrite()
        news
    let updateFile file =
        let res = updateFiles [file]
        if res.Length > 1 then log (sprintf "File %A returned multiple resources" (file)) else ()
        res.[0]
    let getResources() = fileMap |> Map.toList |> List.map snd
    let validatableFiles() = fileMap |> Map.toList |> List.map snd |> List.choose (function |EntityResource (_, e) -> Some e |_ -> None) |> List.filter (fun f -> f.validate)
    let allEntities() = entitiesMap |> Map.toList |> List.map snd |> List.filter (fun struct (e, _) -> e.overwrite <> Overwritten)
    let validatableEntities() = entitiesMap |> Map.toList |> List.map snd  |> List.filter (fun struct (e, _) -> e.overwrite <> Overwritten) |> List.filter (fun struct (e, _) -> e.validate)
    let getFileNames() = fileMap |> Map.toList |> List.map snd
                                 |> List.map (function |EntityResource(_, r) -> r.logicalpath |FileResource(_, r) -> r.logicalpath |FileWithContentResource(_,r) -> r.logicalpath)
    member __.ManualProcessResource = parseFileThenEntity >> snd
    member __.ManualProcess (filename : string) (filetext : string) =
        let parsed = CKParser.parseString filetext filename
        match parsed with
        |Failure(_) -> None
        |Success(s,_,_) ->
            let filenamenopath = Path.GetFileNameWithoutExtension filename
            Some (shipProcess EntityType.Other filenamenopath (mkZeroFile filename) s)

    member __.Api = {
        new IResourceAPI<'T> with
            member __.UpdateFiles = updateFiles
            member __.UpdateFile = updateFile
            member __.GetResources = getResources
            member __.ValidatableFiles = validatableFiles
            member __.AllEntities = allEntities
            member __.ValidatableEntities = validatableEntities
            member __.ForceRecompute() = forceRecompute()
            member __.ForceRulesDataGenerate() = forceRulesData()
            member __.GetFileNames = getFileNames
        }