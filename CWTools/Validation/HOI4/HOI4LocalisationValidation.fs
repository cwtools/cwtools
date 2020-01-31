namespace CWTools.Validation.HOI4
open CWTools.Validation
open CWTools.Validation.LocalisationString
open CWTools.Process
open CWTools.Process.Scopes.HOI4
open CWTools.Common
open CWTools.Common.HOI4Constants
open CWTools.Utilities.Utils
open CWTools.Process.Scopes
open CWTools.Process.Scopes.Scopes
open CWTools.Common.NewScope
open CWTools.Process.Localisation
open CWTools.Process.Localisation.HOI4

module HOI4LocalisationString =

    let hardcodedLocalisation =
        [
            "playername";
            "prov"
        ]

    let validateProcessedLocalisation : ((Lang * LocKeySet) list -> (Lang * Map<string,LocEntry>) list -> ValidationResult) = validateProcessedLocalisationBase hardcodedLocalisation