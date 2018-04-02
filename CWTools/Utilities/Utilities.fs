namespace CWTools.Utilities
open System
open System.Collections.Generic

module Utils =

    
    let inline (==) (x: string) (y: string) = x.Equals(y, StringComparison.OrdinalIgnoreCase)

    type STLStringComparer() = 
      interface IComparer<string> with
        member __.Compare(a, b) =  String.Compare(a, b, StringComparison.OrdinalIgnoreCase)