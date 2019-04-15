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
    type StringSet = Microsoft.FSharp.Collections.Tagged.Set<string, InsensitiveStringComparer>

module TryParser =
    // convenient, functional TryParse wrappers returning option<'a>
    let tryParseWith tryParseFunc = tryParseFunc >> function
        | true, v    -> Some v
        | false, _   -> None

    let parseDate   = tryParseWith System.DateTime.TryParse
    let parseInt    = tryParseWith System.Int32.TryParse
    let parseIntWithDecimal    =  tryParseWith (fun s -> System.Int32.TryParse(s, Globalization.NumberStyles.AllowDecimalPoint ||| Globalization.NumberStyles.Integer, CultureInfo.InvariantCulture))
    let parseSingle = tryParseWith System.Single.TryParse
    let parseDouble = tryParseWith (fun s -> (System.Double.TryParse(s, (Globalization.NumberStyles.Float ||| Globalization.NumberStyles.AllowThousands), CultureInfo.InvariantCulture)))
    // etc.

    // active patterns for try-parsing strings
    let (|Date|_|)   = parseDate
    let (|Int|_|)    = parseInt
    let (|Single|_|) = parseSingle
    let (|Double|_|) = parseDouble


type StringToken = int
type StringLowerToken = int
type StringTokens =
    struct
        val lower: StringLowerToken
        val normal: StringToken
        new(lower, normal) = { lower = lower; normal = normal }
    end

[<Sealed>]
type StringResourceManager() =
    let strings = new System.Collections.Generic.Dictionary<string, StringTokens>(1024)
    let ints = new System.Collections.Generic.Dictionary<StringToken, string>(1024)
    let mutable i = 0
    let monitor = new Object()
    member x.InternIdentifierToken(s) =
        let mutable res = Unchecked.defaultof<_>
        let ok = strings.TryGetValue(s, &res)
        if ok then res else
        lock monitor (fun () ->
            let ls = s.ToLower()
            let lok = strings.TryGetValue(ls, &res)
            if lok
            then
                let stringID = i
                i <- i + 1
                let resn = new StringTokens(res.lower, stringID)
                strings.[s] <- resn;
                ints.[stringID] <- s
                resn
            else
                let stringID = i
                let lowID = i + 1
                i <- i + 2
                let res = new StringTokens(lowID, stringID)
                let resl = new StringTokens(lowID, lowID)
                strings.[s] <- res;
                strings.[ls] <- resl;
                ints.[lowID] <- ls;
                ints.[stringID] <- s;
                res
        )
    member x.GetStringForIDs(id : StringTokens) =
        ints.[id.normal]
    member x.GetLowerStringForIDs(id : StringTokens) =
        ints.[id.lower]
    member x.GetStringForID(id : StringToken) =
        ints.[id]

module StringResource =
    let mutable stringManager = StringResourceManager()
