namespace CWTools.Parser

    
open FParsec
open System.IO
open CWTools.Parser.CKParser
open CWTools.Common
open CWTools.Common.STLConstants



module SetupLogParser =

 
    //let idChar = letter <|> anyOf ['_']
    let isvaluechar = fun c -> CKParser.isvaluechar c || c = '?'
    let str s = pstring s .>> ws <?> ("string " + s)
    let header = skipCharsTillString "Initializing Database: CStaticModifierDatabase" true 2000000 .>> ws <?> "header"
    let pre = skipCharsTillString "Static Modifier #" true 100
    let num = pre >>. pint64 .>> ws |>> int
    let tag = skipString "tag = " >>. many1Satisfy isvaluechar .>> ws
    let name = str "name = " >>. restOfLine true //manyCharsTill valuechar newline .>> ws

    let staticModifier = pipe3 num tag name (fun i t n -> {num = i; tag = t; name = n})

    let modifierHeader = skipCharsTillString "Printing Modifier Definitions" true 2000000 .>> ws <?> "modifier header"

    let mtag = skipCharsTillString "Tag: " true 500 >>. many1CharsTill (satisfy isvaluechar) (pchar ',') .>> ws
    let cat = skipString "Categories: " >>. pint64 |>> int
    let modifier = pipe2 mtag cat (fun t c -> {tag = t; category = c} )

    let footer = many1Chars anyChar

    let logFile = ws >>. header >>. many1 (attempt staticModifier) .>> modifierHeader .>>. many1 (attempt modifier) .>> footer .>> eof

    
    let toDocEffect effectType (x : RawEffect) = DocEffect(x, effectType)

    let parseLogsFile filepath = runParserOnFile logFile () filepath (System.Text.Encoding.GetEncoding(1252))
    let parseLogsStream file = runParserOnStream logFile () "logFile" file (System.Text.Encoding.GetEncoding(1252))
    let processLogs ((s: RawStaticModifier list), (m : RawModifier list)) =
        m |> List.map createModifier
