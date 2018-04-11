namespace CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser.Types
open CWTools.Process.STLScopes
open CWTools.Common
open CWTools.Common.STLConstants
open DotNet.Globbing
open CWTools.Validation.STLValidation
open System.Xml.Linq
open System.Threading
open CWTools.Localisation
open FParsec
open System
open CWTools.Utilities.Utils

module STLLocalisationString =
    type LocKeySet = Microsoft.FSharp.Collections.Tagged.Set<string, STLStringComparer>

    type LocElement =
    | Ref of string
    | Command of string
    | Chars of string
    let valueChars = many1Satisfy ( isNoneOf ['$'; '['] )

    let dollarChars = many1Satisfy ( isNoneOf ['$'; '|'] )
    let dollarColour = pchar '|' .>> dollarChars
    let commandChars = many1Satisfy (isNoneOf [']'])
    let ref = between (skipChar '$') (skipChar '$') (dollarChars .>> optional dollarColour |>> Ref)
    let command = between (skipChar '[') (skipChar ']') (commandChars |>> Command)
    let locStringParser = many (valueChars |>> Chars <|> ref <|> command) .>> eof

    let parseLocString fileString filename = runParserOnString locStringParser () filename fileString

    let hardcodedLocalisation = 
        [
            "playername"
        ]
    let checkRef (lang : Lang) (keys : LocKeySet) (entry : Entry) (r : string) = 
        match keys.Contains r with
        | true -> OK
        | false ->
            match r |> seq |> Seq.exists (fun c -> Char.IsLower(c)) && not (List.contains r hardcodedLocalisation) with
            | true -> Invalid [invManual (ErrorCodes.UndefinedLocReference entry.key r (lang :> obj)) (Position.Conv entry.position) entry.key None ]
            | false -> OK
    
    let commands = 
        [
            "GetAdj";
            "GetAllianceName";
            "GetFleetName";
            "GetHomeWorldName";
            "GetLeaderName";
            "GetName";
            "GetNamePlural";
            "GetPlanetMoon";
            "GetPopFactionName";
            "GetRandomSpeciesSound";
            "GetRegnalName";
            "GetRulerName";
            "GetRulerTitle";
            "GetSpeciesAdj";
            "GetSpeciesClass";
            "GetSpeciesClassPlural";
            "GetSpeciesMouthName";
            "GetSpeciesName";
            "GetSpeciesNameCompliment";
            "GetSpeciesNameInsult";
            "GetSpeciesNamePlural";
            "GetSpeciesNamePluralCompliment";
            "GetSpeciesNamePluralInsult";
            "GetSpeciesOrganName";
            "GetSpeciesSpawnNamePlural";
            "GetStarName";
            "GetHerHim";
            "GetSheHe";
            "GetSheHeCap";
            "GetHerHis";
            "GetHerHisCap";
            "LastKilledCountryName";
            "GetSpeciesSpawnName"; //Vanilla usage
            "GetHeirTitle"; //Vanilla usage
            "GetHeirName"; //Vanilla usage
            "GetAdjective"; //Vanilla usage
            "GetSpeciesClassName"; //Vanilla usage
            "GetHimHer"; //Vanilla usage
            "GetNumPlayerFleetsGoneMIA"; //Vanilla usage
            "GetNumPlayerWormholeStationsDestroyed"; //Vanilla usage;
            "StarName";
            "GetTitle"; // STNH
            "GetLeaderTitle"; // STNH
            "GetHisHer"; // STNH
            "GetMidGameDate"; // STNH
            "GetPersonalityName"; // STNH
            "GetSpeciesPlural"; // STNH
            "GetHisHerCap"; // STNH
        ]
    let checkCommand (entry : Entry) (commands : string list) (eventtargets : string list) (setvariables : string list) (command : string) =
        match localisationCommandContext commands eventtargets setvariables entry command with
        | ContextResult.Found _ -> OK
        | LocNotFound s -> Invalid [invManual (ErrorCodes.InvalidLocCommand entry.key s) (Position.Conv entry.position) entry.key None ]

    let validateLocalisation (effects : Effect list) (scriptedLoc : string list) (setvariables : string list) (os : STLEntitySet) (api : (Lang * Map<string, Entry>)) (keys : (Lang * LocKeySet) list) =
        let lang = api |> fst
        let keys = keys |> List.filter (fun (l, _) -> l = lang) |> List.map snd |> List.fold (fun a b -> LocKeySet.Union (a, b)) (LocKeySet.Empty(STLStringComparer()))
        let all = api |> snd
        let eventtargetsnormal = (os.AllWithData |> List.collect (fun (_, d) -> d.Force().savedeventtargets))
        let eventtargetsglobal = effects |> List.choose (function | :? ScriptedEffect as e -> Some e |_ -> None) |> List.collect (fun e -> e.GlobalEventTargets @ e.SavedEventTargets)
        let eventtargets = eventtargetsnormal @ eventtargetsglobal
        let extractResult =
            function
            |Success (v, _, _) -> v
            |Failure _ -> []
                
        let parsed = all |> Map.map (fun k v -> v, parseLocString v.desc "" |> extractResult)
        parsed |> Map.toList <&!&> (fun (k, (e, v)) -> v |> List.choose (function |Ref s -> Some s |_ -> None) <&!&> checkRef lang keys e )
        <&&>
        (parsed |> Map.toList <&!&> (fun (k, (e, v)) -> v |> List.choose (function |Command s -> Some s |_ -> None) <&!&> checkCommand e (scriptedLoc @ commands) eventtargets setvariables ))