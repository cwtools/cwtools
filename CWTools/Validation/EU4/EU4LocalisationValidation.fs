namespace CWTools.Validation.EU4
open CWTools.Validation.ValidationCore
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser.Types
open CWTools.Process.STLScopes
open CWTools.Common
open CWTools.Common.STLConstants
open DotNet.Globbing
open System.Xml.Linq
open System.Threading
open CWTools.Utilities.Utils
open CWTools.Validation.Stellaris.STLLocalisationValidation
open CWTools.Games

module EU4LocalisationValidation =
    type LocalisationValidator = EntitySet<EU4ComputedData> -> (Lang * Set<string>) list -> EntitySet<EU4ComputedData> -> ValidationResult

    let valOpinionModifierLocs : LocalisationValidator =
        fun _ keys es ->
            let opinion_modifier = es.GlobMatchChildren("**/common/opinion_modifiers/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""]
            opinion_modifier <&!&> inner

    let valStaticModifierLocs : LocalisationValidator =
        fun _ keys es ->
            let static_modifier = es.GlobMatchChildren("**/common/static_modifiers/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""]
            static_modifier <&!&> inner

    let valTimedModifierLocs : LocalisationValidator =
        fun _ keys es ->
            let timed_modifier = es.GlobMatchChildren("**/common/timed_modifiers/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""]
            timed_modifier <&!&> inner

    let valEventModifierLocs : LocalisationValidator =
        fun _ keys es ->
            let event_modifier = es.GlobMatchChildren("**/common/event_modifiers/*.txt")
            let inner1 = checkLocNodeKeyAdvs keys "" [""]
            let inner2 = checkLocNodeKeyAdvs keys "desc_" [""]
            event_modifier <&!&> inner1
            <&&>
            (event_modifier <&!&> inner2)

    let valTriggeredModifierLocs : LocalisationValidator =
        fun _ keys es ->
            let triggered_modifier = es.GlobMatchChildren("**/common/triggered_modifiers/*.txt")
            let inner1 = checkLocNodeKeyAdvs keys "" [""]
            let inner2 = checkLocNodeKeyAdvs keys "desc_" [""]
            triggered_modifier <&!&> inner1
            <&&>
            (triggered_modifier <&!&> inner2)

    let valProvinceTriggeredModifierLocs : LocalisationValidator =
        fun _ keys es ->
            let province_triggered_modifier = es.GlobMatchChildren("**/common/province_triggered_modifiers/*.txt")
            let inner1 = checkLocNodeKeyAdvs keys "" [""]
            let inner2 = checkLocNodeKeyAdvs keys "desc_" [""]
            province_triggered_modifier <&!&> inner1
            <&&>
            (province_triggered_modifier <&!&> inner2)


    let valUnitTypeLocs : LocalisationValidator =
        fun _ keys es ->
            let unit_type = es.GlobMatchChildren("**/common/units/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""; "DESC"]
            unit_type <&!&> inner

    let valAdvisorTypeLocs : LocalisationValidator =
        fun _ keys es ->
            let advisor_type = es.GlobMatchChildren("**/common/advisortypes/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""; "_desc"]
            advisor_type <&!&> inner

    let valTradeGoodLocs : LocalisationValidator =
        fun _ keys es ->
            let trade_good = es.GlobMatchChildren("**/common/tradegoods/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""; "DESC"]
            trade_good <&!&> inner

    let valTradeCompanyLocs : LocalisationValidator =
        fun _ keys es ->
            let trade_company = es.GlobMatchChildren("**/common/trade_companies/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""]
            trade_company <&!&> inner

    let valTradeCompanyInvestmentLocs : LocalisationValidator =
        fun _ keys es ->
            let trade_company_investment = es.GlobMatchChildren("**/common/tradecompany_investments/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""; "_desc"]
            trade_company_investment <&!&> inner

    let valTradeNodeLocs : LocalisationValidator =
        fun _ keys es ->
            let trade_node = es.GlobMatchChildren("**/common/tradenodes/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""]
            trade_node <&!&> inner

    let valTradingPolicyLocs : LocalisationValidator =
        fun _ keys es ->
            let trading_policy = es.GlobMatchChildren("**/common/trading_policies/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""; "_desc"]
            trading_policy <&!&> inner


    let valTradeCenterLocs : LocalisationValidator =
        fun _ keys es ->
            let trade_center = es.GlobMatchChildren("**/common/centers_of_trade/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""; "_desc"]
            trade_center <&!&> inner

    let valCasusBelliLocs : LocalisationValidator =
        fun _ keys es ->
            let casus_belli = es.GlobMatchChildren("**/common/casus_belli/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""; "_desc"]
            casus_belli <&!&> inner

    let valWarGoalLocs : LocalisationValidator =
        fun _ keys es ->
            let war_goal = es.GlobMatchChildren("**/common/wargoal_types/*.txt")
            let inner = checkLocNodeKeyAdvs keys "" [""; "_desc"]
            war_goal <&!&> inner
