namespace CWTools.Localisation
open System.Collections.Generic
open CWTools.Common

[<Struct>]
type Entry = {
    key : string
    value : char option
    desc : string
    position : FParsec.Position
}

type GetDesc = string -> string
type GetKeys = string list
type Values = IDictionary<string, string>
type Results = IDictionary<string, (bool * int * string)>

type ILocalisationAPI =
    abstract Results : IDictionary<string, (bool * int * string)>
    abstract ValueMap : Map<string, Entry>
    abstract Values : IDictionary<string, string>
    abstract GetKeys : string list
    abstract GetDesc : string -> string
    abstract GetLang : Lang


type LocalisationSettings = 
    {
        folder : string
    }