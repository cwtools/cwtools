namespace CWTools.Utilities
open System
open System.Collections.Generic
open CWTools.Utilities.Position
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
        //log "Elapsed Time: %i %s" timer.ElapsedMilliseconds s
        returnValue


    type LogLevel =
    |Silent
    |Normal
    |Verbose


    let mutable loglevel = Silent

    let logInner level message =
        match loglevel, level with
        |Silent, _ -> ()
        |Normal, Normal -> Printf.eprintfn "%s: %s" (System.DateTime.Now.ToString("HH:mm:ss")) message
        |Verbose, _ -> Printf.eprintfn "%s: %s" (System.DateTime.Now.ToString("HH:mm:ss")) message
        |_, _ -> ()
        // |Verbose -> logWith logger format

    let logVerbose message = logInner Verbose message
    let logNormal message = logInner Normal message
    let log = logVerbose

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

type StringToken = int

[<Sealed>]
type StringResourceManager() =
    let strings = new System.Collections.Generic.Dictionary<string, StringToken>(1024)
    let ints = new System.Collections.Generic.Dictionary<StringToken, string>(1024)
    let mutable i = 0
    let monitor = new Object()
    member x.InternIdentifierToken(s) =
        let mutable res = Unchecked.defaultof<_>
        let ok = strings.TryGetValue(s, &res)
        if ok then res  else
        lock monitor (fun () ->
            i <- i + 1
            let res = i
            strings.[s] <- res;
            ints.[res] <- s;
            res
        )
    member x.GetStringForID(id) =
        ints.[id]

module StringResource =
    let mutable stringManager = StringResourceManager()
