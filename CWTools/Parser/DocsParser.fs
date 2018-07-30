namespace CWTools.Parser

    
open FParsec
open System.IO
open CWTools.Common
open CWTools.Common.STLConstants



module DocsParser =

    let private idChar = letter <|> anyOf ['_']
    let private isvaluechar = SharedParsers.isvaluechar
    let private header = skipCharsTillString "DOCUMENTATION ==" true 2000 .>> SharedParsers.ws <?> "header"
    let private name = (many1Chars idChar) .>> SharedParsers.ws .>> pchar '-' .>>. restOfLine false .>> SharedParsers.ws <?> "name"
    let private usage = charsTillString "Supported Scopes:" true 2000 .>> SharedParsers.ws <?> "usage"
    let private scope = many1Satisfy isvaluechar .>> many spaces1 <?> "scope"
    let private scopes = many1Till scope (skipString "Supported Targets:" .>> SharedParsers.ws)
    let private target = many1Satisfy isvaluechar .>> many (skipChar ' ') <?> "target"
    let private targets = many1Till target newline .>> SharedParsers.ws
    let private doc =  pipe4 name usage  scopes  targets (fun (n, d) u s t  -> {name = n; desc = d; usage = u; scopes = s; targets = t}) <?> "doc"
    let private footer = skipString "=================" .>> SharedParsers.ws
    let private docFile = SharedParsers.ws >>. header >>. many doc .>> footer

    let private twoDocs = docFile .>>. docFile

    let toDocEffect effectType (x : RawEffect) = DocEffect(x, effectType)
    let processDocs (t, e) = t |> List.map (toDocEffect EffectType.Trigger), e |> List.map (toDocEffect EffectType.Effect)

    let parseDocsFile filepath = runParserOnFile twoDocs () filepath (System.Text.Encoding.GetEncoding(1252))
    
    let parseDocsStream file = runParserOnStream twoDocs () "docFile" file (System.Text.Encoding.GetEncoding(1252))