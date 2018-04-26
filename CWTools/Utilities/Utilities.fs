namespace CWTools.Utilities
open System
open System.Collections.Generic

module Utils =

    
    let inline (==) (x: string) (y: string) = x.Equals(y, StringComparison.OrdinalIgnoreCase)

    type STLStringComparer() = 
      interface IComparer<string> with
        member __.Compare(a, b) =  String.Compare(a, b, StringComparison.OrdinalIgnoreCase)

    let memoize keyFunction memFunction =
        let dict = new System.Collections.Generic.Dictionary<_,_>()
        fun n ->
            match dict.TryGetValue(keyFunction(n)) with
            | (true, v) -> v
            | _ ->
                let temp = memFunction(n)
                dict.Add(keyFunction(n), temp)
                temp

    let duration f s = 
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let returnValue = f()
        eprintfn "Elapsed Time: %i %s" timer.ElapsedMilliseconds s
        returnValue


    type LogLevel =
    |Silent
    |Verbose

    let mutable loglevel = Silent

    let log format =
        match loglevel with
        |Silent -> ignore
        |Verbose -> Printf.eprintfn format
    
    let logVerbose format = log format

