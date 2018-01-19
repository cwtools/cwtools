namespace CWTools.Parser

    
open FParsec
open System.IO
open CWTools.Parser.CKParser
open CWTools.Common
open CWTools.Common.STLConstants



module DocsParser =

    let idChar = letter <|> anyOf ['_']
    let valuechar = CKParser.valuechar <|> pchar '?'
    let header = skipCharsTillString "DOCUMENTATION ==" true 2000 .>> ws <?> "header"
    let name = (many1Chars idChar) .>> ws .>> pchar '-' .>>. restOfLine false .>> ws <?> "name"
    let usage = charsTillString "Supported Scopes:" true 2000 .>> ws <?> "usage"
    let scope = many1Chars valuechar .>> many spaces1 <?> "scope"
    let scopes = many1Till scope (skipString "Supported Targets:" .>> ws)
    let target = many1Chars valuechar .>> many (skipChar ' ') <?> "target"
    let targets = many1Till target newline .>> ws
    let doc =  pipe4 name usage  scopes  targets (fun (n, d) u s t  -> {name = n; desc = d; usage = u; scopes = s; targets = t}) <?> "doc"
    let footer = skipString "=================" .>> ws
    let docFile = ws >>. header >>. many doc .>> footer

    let twoDocs = docFile .>>. docFile

    let toDocEffect effectType (x : RawEffect) = DocEffect(x, effectType)
    let processDocs (t, e) = t |> List.map (toDocEffect EffectType.Trigger), e |> List.map (toDocEffect EffectType.Effect)

    let parseDocsFile filepath = runParserOnFile twoDocs () filepath (System.Text.Encoding.GetEncoding(1252))
    
    let parseDocsStream file = runParserOnStream twoDocs () "docFile" file (System.Text.Encoding.GetEncoding(1252))