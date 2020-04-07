namespace CWTools.Common

open System
open System.ComponentModel.Design
open System
open CWTools.Utilities.Utils
open NewScope

module STLConstants =
    let defaultScopes = [
        "Country", ["country"]
        "Leader", ["leader"]
        "System", ["galacticobject"; "system"; "galactic_object"]
        "Planet", ["planet"]
        "Ship", ["ship"]
        "Fleet",["fleet"]
        "Pop",["pop"]
        "Ambient Object",["ambientobject"; "ambient_object"]
        "Army",["army"]
        "Tile",["tile"]
        "Species",["species"]
        "Pop Faction",["popfaction"; "pop_faction"]
        "Sector",["sector"]
        "Federation",["alliance"; "federation"; "Alliance"]
        "War",["war"]
        "Megastructure",["megastructure"]
        "Design",["design"]
        "Starbase",["starbase"]
        "Star",["star"]
        "Deposit",["deposit"]
        "Archaeological Site",["archaeologicalsite"; "archaeological_site"]
    ]
    let defaultScopeInputs =
        defaultScopes |> List.map (fun (n, s) -> { NewScope.ScopeInput.name = n; NewScope.ScopeInput.aliases = s; NewScope.ScopeInput.isSubscopeOf = []})

    type RawStaticModifier =
        {
            num : int
            tag : string
            name : string
        }
    type RawModifier =
        {
            tag : string
            category : int
        }

    let defaultModifiers = [
        "Pop", Some 2, ["pop"; "planet"; "galacticobject"; "country"]
        "Science", Some 64, ["ship"; "country"]
        "Country", Some 256, ["country"]
        "Army", Some 512, ["army"; "planet"; "country"]
        "Leader", Some 1024, ["leader"; "country"]
        "Planet", Some 2048, ["planet"; "galacticobject"; "country"]
        "PopFaction", Some 8192, ["popfaction"; "country"]
        "ShipSize", Some 16496, ["ship"; "starbase"; "country"]
        "Ship", Some 16508, ["ship"; "starbase";"fleet";"country"]
        "Tile", Some 32768, ["tile";"pop";"planet";"country"]
        "Megastructure", Some 65536, ["megastructure";"country"]
        "PlanetClass", Some 131072, ["planet"; "pop"; "country"]
        "Starbase", Some 262144, ["starbase";"country"]
        "Resource", Some 524288, ["country";"galacticobject";"planet";"pop";"starbase";"leader";"ship"]
        "Federation", Some 4194304, ["federation"]
    ]

    let defaultModifiersInputs() =
        defaultModifiers |> List.map (fun (n, intID, ss) ->
            {
                NewScope.ModifierCategoryInput.name = n;
                NewScope.ModifierCategoryInput.internalID = intID;
                NewScope.ModifierCategoryInput.scopes = ss |> List.map (scopeManager.ParseScope())
                })

    type EntityType =
    |Agenda = 1
    |AmbientObjects = 2
    |Anomalies = 3
    |Armies = 4
    |ArmyAttachments = 5
    |AscensionPerks = 6
    |Attitudes = 7
    |BombardmentStances = 8
    |BuildablePops = 9
    |BuildingTags = 10
    |Buildings = 11
    |ButtonEffects = 12
    |Bypass = 13
    |CasusBelli = 14
    |Colors = 15
    |ComponentFlags = 16
    |ComponentSets = 17
    |ComponentTags = 18
    |ComponentTemplates = 19
    |CountryCustomization = 20
    |CountryTypes = 21
    |Deposits = 22
    |DiploPhrases = 23
    |DiplomaticActions =24
    |Edicts = 25
    |Ethics = 26
    |EventChains = 27
    |FallenEmpires = 28
    |GameRules = 29
    |GlobalShipDesigns = 30
    |Governments = 31
    |Authorities = 90
    |Civics = 32
    |GraphicalCulture = 33
    |Mandates = 34
    |MapModes = 35
    |Megastructures = 36
    |NameLists = 37
    |NotificationModifiers = 38
    |ObservationStationMissions = 39
    |OnActions = 40
    |OpinionModifiers = 41
    |Personalities = 42
    |PlanetClasses =43
    |PlanetModifiers = 44
    |Policies = 45
    |PopFactionTypes = 46
    |PrecursorCivilizations = 47
    |ScriptedEffects = 48
    |ScriptedLoc = 49
    |ScriptedTriggers = 50
    |ScriptedVariables = 51
    |SectionTemplates = 52
    |SectorTypes = 53
    |ShipBehaviors = 54
    |ShipSizes = 55
    |SolarSystemInitializers = 56
    |SpecialProjects = 57
    |SpeciesArchetypes = 58
    |SpeciesClasses = 59
    |SpeciesNames = 60
    |SpeciesRights = 61
    |StarClasses = 62
    |StarbaseBuilding = 63
    |StarbaseLevels = 64
    |StarbaseModules = 65
    |StarbaseTypes = 66
    |SpaceportModules = 67
    |StartScreenMessages =68
    |StaticModifiers = 69
    |StrategicResources = 70
    |Subjects = 71
    |SystemTypes = 72
    |Technology = 73
    |Terraform = 74
    |TileBlockers = 75
    |TraditionCategories =76
    |Traditions = 77
    |Traits = 78
    |TriggeredModifiers = 79
    |WarDemandCounters = 80
    |WarDemandTypes = 81
    |WarGoals = 82
    |Events = 83
    |MapGalaxy = 84
    |MapSetupScenarios = 85
    |PrescriptedCountries = 86
    |Interface = 87
    |GfxGfx = 88
    |Other = 89
    |GfxAsset = 90
    |Decisions = 91

    let scriptFolders = [
        "common";
        "common/deposits";
        "events";
        "map/galaxy";
        "map/setup_scenarios";
        "prescripted_countries";
        "interface";
        "gfx";
        "music";
        "sound";
        "fonts";
        "flags";
        "localisation";
        "localisation_synced"
        ]
