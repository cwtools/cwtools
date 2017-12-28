namespace CK2Events.Application.Localisation
open System.Collections.Generic

module LocalisationDomain =
    type GetDesc = string -> string
    type GetKeys = string list
    type Values = IDictionary<string, string>
    type Results = IDictionary<string, (bool * int * string)>

    type LocalisationAPI =
        {
            results : Results
            values : Values
            getKeys : GetKeys
            getDesc : GetDesc
        }