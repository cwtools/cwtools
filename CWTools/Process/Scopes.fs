namespace CWTools.Process

open NodaTime.TimeZones
open System
open CWTools.Localisation
open CWTools.Utilities.Position
open CWTools.Common
open Microsoft.FSharp.Reflection
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

    // type IScopeContext<'S when 'S : comparison> =
    //     abstract CurrentScope : 'S
    //     abstract PopScope : 'S list
    //     abstract GetFrom : int -> 'S
    //     abstract Root : 'S
    //     abstract From :'S list
    //     abstract Scopes :'S list
    // type RuleContext<'T when 'T : comparison> =
    //     {
    //         subtypes : string list
    //         scopes : IScopeContext<'T>
    //         warningOnly : bool
    //     }

    type OutputScopeContext<'T> =
        {
            Root : 'T
            From : 'T list
            Scopes : 'T list
        }
        member inline this.CurrentScope = match this.Scopes with |[] -> None | x::_ -> Some x
        member inline this.PopScope : 'T list = match this.Scopes with |[] -> [] |_::xs -> xs
        member inline this.GetFrom i =
            if this.From.Length >= i then Some (this.From.Item (i - 1)) else None

    type ScopeContext< 'T when 'T :> IScope<'T> > =
        {
            Root : 'T
            From : 'T list
            Scopes : 'T list
        }
        member this.CurrentScope = match this.Scopes with |[] -> this.Root.AnyScope | x::_ -> x
        member this.PopScope : 'T list = match this.Scopes with |[] -> [] |_::xs -> xs
        member this.GetFrom i =
             if this.From.Length >= i then (this.From.Item (i - 1)) else this.Root.AnyScope




    type ScopeResult<'T when 'T :> IScope<'T>> =
        | NewScope of newScope : ScopeContext<'T> * ignoreKeys : string list
        | WrongScope of command : string * scope : 'T * expected : 'T list
        | NotFound
    // interface IScopeContext<^T> with
    //     member this.CurrentScope = this.CurrentScope
    //     member this.PopScope = this.PopScope
    //     member this.GetFrom i = this.GetFrom i
    //     member this.Root = this.Root
    //     member this.From = this.From
    //     member this.Scopes = this.Scopes
