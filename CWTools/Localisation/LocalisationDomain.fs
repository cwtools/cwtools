namespace CWTools.Localisation

open System.Collections.Generic
open CWTools.Common
open CWTools.Utilities.Position
open FParsec

type Entry =
    { key: string
      value: char option
      desc: string
      position: range
      errorRange: range option }

type GetDesc = string -> string
type GetKeys = string list
type Values = IDictionary<string, string>
type Results = IDictionary<string, bool * int * string * Position option>

type ILocalisationAPI =
    abstract Results: Results
    abstract ValueMap: Map<string, Entry>
    abstract Values: IDictionary<string, string>
    abstract GetKeys: string list
    abstract GetDesc: string -> string
    abstract GetLang: Lang

type ILocalisationAPICreator =
    abstract Api: Lang -> ILocalisationAPI


type LocalisationSettings<'L> =
    { folder: string
      gameName: string
      keyToLanguage: string -> 'L option
      gameToLang: 'L -> Lang }
