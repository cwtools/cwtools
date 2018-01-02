namespace CWTools.Parser

    
open FParsec
open System.IO
open CWTools.Parser.CKParser

type Effect =
    {
        name : string
        desc : string
        usage : string
        scopes : string list
        targets : string list
    }

module DocsParser =


    let idChar = letter <|> anyOf ['_']
    let valuechar = CKParser.valuechar <|> pchar '?'
    let header = skipCharsTillString "== TRIGGER DOCUMENTATION ==" true 1000 .>> ws <?> "header"
    let name = (many1Chars idChar) .>> ws .>> pchar '-' .>>. restOfLine false .>> ws <?> "name"
    let usage = charsTillString "Supported Scopes:" true 1000 .>> ws <?> "usage"
    let scope = many1Chars valuechar .>> many spaces1 <?> "scope"
    let scopes = many1Till scope (skipString "Supported Targets:" .>> ws)
    let target = many1Chars valuechar .>> many (skipChar ' ') <?> "target"
    let targets = many1Till target newline .>> ws
    let doc =  pipe4 name usage  scopes  targets (fun (n, d) u s t  -> {name = n; desc = d; usage = u; scopes = s; targets = t}) <?> "doc"
    let docFile = ws >>. header >>. many doc

    let parseDocs filepath = runParserOnFile docFile () filepath (System.Text.Encoding.GetEncoding(1252))
