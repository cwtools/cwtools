namespace CWTools.Validation.VIC2
open CWTools.Validation
open CWTools.Validation.LocalisationString
open CWTools.Process
open CWTools.Process.Scopes.VIC2
open CWTools.Common
open CWTools.Common.VIC2Constants
open CWTools.Utilities.Utils
open CWTools.Process.Scopes
open CWTools.Common.NewScope
open CWTools.Process.Scopes.Scopes
open CWTools.Process.Localisation
open CWTools.Process.Localisation.VIC2
module VIC2LocalisationString =

    let hardcodedLocalisation =
        [
            "playername";
            "prov"
        ]


    let commands =
        [
            "GetAreaName";
            "GetAdjective";
            "GetAdm";
            "GetCapitalName";
            "GetDip";
            "GetFlagshipName";
            "GetGroupName";
            "GetHerHim";
            "GetHerHimCap";
            "GetHerHis";
            "GetHerHisCap";
            "GetHerselfHimself";
            "GetHerselfHimselfCap";
            "GetSheHe";
            "GetSheHeCap";
            "GetMil";
            "GetName";
            "GetTitle";
            "GetTradeGoodsName";
            "GetWomanMan";
            "GetYear";
            "GovernmentName";
            "GetXEDip";
            "GetErEreDip";
            "GetDateText";
            "GetDate";
            "GetYear";
            "GetMonth";
            "GetReligiousSchool";
            "GetCultureName";
            "GetCultureGroupName";
            "GetReligionName";
            "GetReligionGroupName";
        ]
    let locCommands() = commands |> List.map (fun c -> c, scopeManager.AllScopes), []

    let validateProcessedLocalisation : ((Lang * LocKeySet) list -> (Lang * Map<string,LocEntry>) list -> ValidationResult) = validateProcessedLocalisationBase hardcodedLocalisation