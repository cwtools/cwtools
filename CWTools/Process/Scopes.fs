namespace CWTools.Process

open NodaTime.TimeZones
open System
open CWTools.Localisation
open CWTools.Utilities.Position
open CWTools.Common
module Scopes =
    open CWTools.Utilities.Utils
    open Microsoft.FSharp.Collections.Tagged
    type EffectMap<'S when 'S : comparison> = Map<string, Effect<'S>, InsensitiveStringComparer>
    type UsageScopeContext<'S> = 'S list
    type ContextResult<'S> =
    | Found of string * ('S list)
    | LocNotFound of string
    //| Failed
    [<Struct>]
    type LocEntry<'S> = {
        key : string
        value : char option
        desc : string
        position : range
        scopes : ContextResult<'S> list
        refs : string list
    }

    type IScopeContext<'S when 'S : comparison> =
        abstract CurrentScope : 'S
        abstract PopScope : 'S list
        abstract GetFrom : int -> 'S
        abstract Root : 'S
        abstract From :'S list
        abstract Scopes :'S list
    // type RuleContext<'T when 'T : comparison> =
    //     {
    //         subtypes : string list
    //         scopes : IScopeContext<'T>
    //         warningOnly : bool
    //     }