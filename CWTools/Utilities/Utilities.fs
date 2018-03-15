namespace CWTools.Utilities
open System

module Utils =

    
    let inline (==) (x: string) (y: string) = x.Equals(y, StringComparison.OrdinalIgnoreCase)
