namespace CWTools.Utilities
open System
open System.Collections.Generic
open CWTools.Utilities.Position
open System.Globalization
open System.IO

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



    type LogLevel =
    |Silent
    |Normal
    |Verbose


    /// For the default logger only
    let mutable loglevel = Silent

    let logInner level message =
        match loglevel, level with
        |Silent, _ -> ()
        |Normal, Normal -> Printf.eprintfn "%s: %s" (System.DateTime.Now.ToString("HH:mm:ss")) message
        |Verbose, _ -> Printf.eprintfn "%s: %s" (System.DateTime.Now.ToString("HH:mm:ss")) message
        |_, _ -> ()
        // |Verbose -> logWith logger format

    let private defaultLogVerbose message = logInner Verbose message
    let private defaultLogNormal message = logInner Normal message
    let private defaultLogAll message = Printf.eprintfn "%s: %s" (System.DateTime.Now.ToString("HH:mm:ss")) message

    let mutable logDiag = defaultLogVerbose
    let mutable logInfo = defaultLogNormal
    let mutable logWarning = defaultLogNormal
    let mutable logError = defaultLogAll

    let log m = logInfo m

    let duration f s =
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let returnValue = f()
        //log (sprintf "Elapsed Time: %i %s" timer.ElapsedMilliseconds s)
        returnValue

    let mkZeroFile file = mkRange file (mkPos 0 0) (mkPos 10000 0)
    type StringSet = Microsoft.FSharp.Collections.Tagged.Set<string, InsensitiveStringComparer>
    let repeatN f n x =
        let mutable x = x
        for i = 1 to n do
            x <- f x
        x

    let getAllFoldersUnion dirs =
        let rec getAllFolders depth dirs =
            if Seq.isEmpty dirs || depth > 20 then Seq.empty else
                seq { yield! dirs |> Seq.collect Directory.EnumerateDirectories
                      yield! dirs |> Seq.collect Directory.EnumerateDirectories |> getAllFolders (depth + 1) }
        seq {
            yield! dirs
            yield! getAllFolders 0 dirs
        }
        
    let structSnd struct(_, x) = x

    [<Literal>]
    let magicChar = '\u1E00'
    [<Literal>]
    let magicCharString = "\u1E00"
    let quoteCharArray = [|'"'|]
module TryParser =
    // convenient, functional TryParse wrappers returning option<'a>
    let tryParseWith tryParseFunc = tryParseFunc >> function
        | true, v    -> Some v
        | false, _   -> None

    let parseDate : string -> _   = tryParseWith System.DateTime.TryParse
    let parseInt : string -> _    = tryParseWith System.Int32.TryParse
    let parseIntWithDecimal : string -> _    =  tryParseWith (fun s -> System.Int32.TryParse(s, Globalization.NumberStyles.AllowDecimalPoint ||| Globalization.NumberStyles.Integer, CultureInfo.InvariantCulture))
    let parseSingle : string -> _ = tryParseWith System.Single.TryParse
    let parseDouble : string -> _ = tryParseWith (fun s -> (System.Double.TryParse(s, (Globalization.NumberStyles.Float ||| Globalization.NumberStyles.AllowThousands), CultureInfo.InvariantCulture)))
    let parseDecimal : string -> _ = tryParseWith (fun s -> (System.Decimal.TryParse(s, (NumberStyles.Float ||| NumberStyles.AllowThousands), CultureInfo.InvariantCulture)))
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
        /// We throw away the quotes when we intern, but we do need to keep that info, but don't want to have multiple tokens with/without quotes
        val quoted : bool
        new(lower, normal, quoted) = { lower = lower; normal = normal; quoted = quoted }
    end
type StringMetadata =
    struct
        val startsWithAmp : bool
        val containsDoubleDollar : bool
        val containsQuestionMark : bool
        val containsHat : bool
        val startsWithSquareBracket : bool
        val containsPipe : bool
        new(startsWithAmp, containsDoubleDollar, containsQuestionMark, containsHat, startsWithSquareBracket, containsPipe) =
            {
                startsWithAmp = startsWithAmp;
                containsDoubleDollar = containsDoubleDollar
                containsQuestionMark = containsQuestionMark
                containsHat = containsHat
                startsWithSquareBracket = startsWithSquareBracket
                containsPipe = containsPipe
            }
    end
[<Sealed>]
type StringResourceManager() =
    let strings = new System.Collections.Concurrent.ConcurrentDictionary<string, StringTokens>()
    let ints = new System.Collections.Concurrent.ConcurrentDictionary<StringToken, string>()
    let metadata = new System.Collections.Concurrent.ConcurrentDictionary<StringToken, StringMetadata>()
    let mutable i = 0
    // let mutable j = 0
    let monitor = Object()
    member x.InternIdentifierToken(s) =
        // j <- j + 1
        // eprintfn "%A" j
        let mutable res = Unchecked.defaultof<_>
        let ok = strings.TryGetValue(s, &res)
        if ok then res else
        lock monitor (fun () ->
            let ls = s.ToLower().Trim('"')
            let quoted = s.StartsWith "\"" && s.EndsWith "\""
            let lok = strings.TryGetValue(ls, &res)
            if lok
            then
                let stringID = i
                i <- i + 1
                let resn = StringTokens(res.lower, stringID, quoted)
                strings.[s] <- resn;
                ints.[stringID] <- s
                metadata.[stringID] <- metadata.[res.lower]
                resn
            else
                let stringID = i
                let lowID = i + 1
                i <- i + 2
                // eprintfn "%A" i
                let res = StringTokens(lowID, stringID, quoted)
                let resl = StringTokens(lowID, lowID, false)
                strings.[s] <- res;
                strings.[ls] <- resl;
                ints.[lowID] <- ls;
                ints.[stringID] <- s;
                let startsWithAmp, containsQuestionMark, containsHat, containsDoubleDollar, startsWithSquareBracket, containsPipe =
                    if ls.Length > 0
                    then
                        let startsWithAmp = ls.[0] = '@'
                        let containsQuestionMark = ls.IndexOf('?') >= 0
                        let containsHat = ls.IndexOf('^') >= 0
                        let first = ls.IndexOf('$')
                        let last = ls.LastIndexOf('$')
                        let containsDoubleDollar = first >= 0 && first <> last
                        let startsWithSquareBracket = ls.[0] = '[' || ls.[0] = ']'
                        let containsPipe = ls.IndexOf('|') >= 0
                        // let quoted =
                        startsWithAmp, containsQuestionMark, containsHat, containsDoubleDollar, startsWithSquareBracket, containsPipe
                    else
                        false, false, false, false, false, false
                metadata.[lowID] <- StringMetadata(startsWithAmp, containsDoubleDollar, containsQuestionMark, containsHat, startsWithSquareBracket, containsPipe)
                metadata.[stringID] <- StringMetadata(startsWithAmp, containsDoubleDollar, containsQuestionMark, containsHat, startsWithSquareBracket, containsPipe)
                res
        )
    member x.GetStringForIDs(id : StringTokens) =
        ints.[id.normal]
    member x.GetLowerStringForIDs(id : StringTokens) =
        ints.[id.lower]
    member x.GetStringForID(id : StringToken) =
        ints.[id]
    member x.GetMetadataForID(id : StringToken) =
        metadata.[id]

module StringResource =
    let mutable stringManager = StringResourceManager()
