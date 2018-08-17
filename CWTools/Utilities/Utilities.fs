namespace CWTools.Utilities
open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler.Range
open System.Globalization

module Utils =


    let inline (==) (x: string) (y: string) = x.Equals(y, StringComparison.OrdinalIgnoreCase)

    type InsensitiveStringComparer() =
      interface IComparer<string> with
        member __.Compare(a, b) =  String.Compare(a, b, StringComparison.OrdinalIgnoreCase)

    type LocKeySet = Microsoft.FSharp.Collections.Tagged.Set<string, InsensitiveStringComparer>

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

    let mkZeroFile file = mkRange file (mkPos 0 0) (mkPos 10000 0)

module TryParser =
    // convenient, functional TryParse wrappers returning option<'a>
    let tryParseWith tryParseFunc = tryParseFunc >> function
        | true, v    -> Some v
        | false, _   -> None

    let parseDate   = tryParseWith System.DateTime.TryParse
    let parseInt    = tryParseWith System.Int32.TryParse
    let parseSingle = tryParseWith System.Single.TryParse
    let parseDouble = tryParseWith (fun s -> (System.Double.TryParse(s, (Globalization.NumberStyles.Float ||| Globalization.NumberStyles.AllowThousands), CultureInfo.InvariantCulture)))
    // etc.

    // active patterns for try-parsing strings
    let (|Date|_|)   = parseDate
    let (|Int|_|)    = parseInt
    let (|Single|_|) = parseSingle
    let (|Double|_|) = parseDouble