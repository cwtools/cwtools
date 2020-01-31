namespace CWTools.Validation.CK2
open CWTools.Validation
open CWTools.Validation.LocalisationString
open CWTools.Process
open CWTools.Process.Scopes.CK2
open CWTools.Common
open CWTools.Common.CK2Constants

open CWTools.Utilities.Utils
open CWTools.Process.Scopes
open CWTools.Common.NewScope
open CWTools.Process.Scopes.Scopes
open CWTools.Process.Localisation
open CWTools.Process.Localisation.CK2

module CK2LocalisationString =

    let hardcodedLocalisation =
        [
            "playername";
            "prov"
        ]

    let validateProcessedLocalisation : ((Lang * LocKeySet) list -> (Lang * Map<string,LocEntry>) list -> ValidationResult) = validateProcessedLocalisationBase hardcodedLocalisation
    let processLocalisation = fun commands variableCommands dynamicSettings -> processLocalisationBase (localisationCommandValidator commands variableCommands dynamicSettings) defaultContext
    let validateLocalisationCommand = fun commands variableCommands dynamicSettings -> validateLocalisationCommandsBase (localisationCommandValidator commands variableCommands dynamicSettings)
