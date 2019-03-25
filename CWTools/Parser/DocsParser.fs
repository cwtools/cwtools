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
    let private usage = charsTillString "Supported scopes:" true 2000 .>> SharedParsers.ws <?> "usage"
    let private usageC = charsTillString "Supported Scopes:" true 2000 .>> SharedParsers.ws <?> "usage"
    let private scope = many1Satisfy ((fun c -> isvaluechar c || c = '?' || c = '(' || c = ')')) .>> many spaces1 <?> "scope"
    let private scopes = manyTill scope (skipString "Supported targets:" .>> skipManySatisfy (fun c -> c = ' ')) <?> "scopes"
    let private scopesC = manyTill scope (skipString "Supported Targets:" .>> skipManySatisfy (fun c -> c = ' ')) <?> "scopes"
    let private target = many1Satisfy isvaluechar .>> many (skipChar ' ') <?> "target"
    let private targets = manyTill target newline .>> SharedParsers.ws <?> "targets"
    let private doc =  pipe4 name (attempt usage <|> usageC)  (attempt scopes <|> scopesC)  targets (fun (n, d) u s t  -> {name = n; desc = d; traits = None; usage = u; scopes = s; targets = t}) <?> "doc"
    let private footer = skipString "=================" .>> SharedParsers.ws
    let private docFile = SharedParsers.ws >>. header >>. many doc //.>> footer

    let private twoDocs = docFile .>>. docFile

    let toDocEffect<'a when 'a : comparison> effectType (parseScopes) (x : RawEffect)  = DocEffect<'a>(x, effectType, parseScopes)
    let processDocs parseScopes (t, e) = t |> List.map (toDocEffect EffectType.Trigger parseScopes), e |> List.map (toDocEffect EffectType.Effect parseScopes)

    let parseDocsFile filepath = runParserOnFile twoDocs () filepath (System.Text.Encoding.GetEncoding(1252))
    let parseDocsFilesRes filepath = parseDocsFile filepath |> (function |Success(p, _, _) -> p |_ -> [], [])
    let parseDocsStream file = runParserOnStream twoDocs () "docFile" file (System.Text.Encoding.GetEncoding(1252))

module JominiParser =
    let private idChar = letter <|> digit <|> anyOf ['_']
    let private isvaluechar = SharedParsers.isvaluechar
    let private header = skipCharsTillString "Event Target Documentation:" true 2000 .>> SharedParsers.ws <?> "header"
    let private spacer = skipString "--------------------" .>> SharedParsers.ws
    let private name = (many1Chars idChar) .>> SharedParsers.ws .>> pchar '-' .>>. restOfLine false .>> SharedParsers.ws <?> "name"
    let private reqData = pstring "Requires Data: yes" .>> SharedParsers.ws <?> "requires data"
    let private wildCard = pstring "Wild Card: yes" .>> SharedParsers.ws <?> "wildcard"
    let private inscopes = pstring "Input Scopes: " >>. sepBy (manyChars (noneOf ("," + "\r\n"))) (pstring ",") .>> newline .>> SharedParsers.ws
    let private outscopes = pstring "Output Scopes: " >>. sepBy (manyChars (noneOf ("," + "\r\n"))) (pstring ",") .>> newline .>> SharedParsers.ws
    let private link = pipe5 name (opt reqData) (opt wildCard) (opt inscopes) (opt outscopes) (fun (n, d) r w i o -> n,d,r,w,i,o)
    let private footer = pstring "Event Targets Saved from Code:" .>> many1Chars anyChar .>> eof
    let private linkFile = SharedParsers.ws >>. header >>. spacer >>. many (attempt (link .>> spacer)) .>> footer

    let parseLinksFile filepath = runParserOnFile linkFile () filepath (System.Text.Encoding.GetEncoding(1252))
    let parseLinksFilesRes filepath = parseLinksFile filepath |> (function |Success(p, _, _) -> p |_ -> [])

    let private triggerheader = skipCharsTillString "Trigger Documentation:" true 2000 .>> SharedParsers.ws <?> "header"
    let private effectheader = skipCharsTillString "Effect Documentation:" true 2000 .>> SharedParsers.ws <?> "header"
    let private supportedscopes = pstring "Supported Scopes: " >>. sepBy (manyChars (noneOf ("," + "\r\n"))) (pstring ",") .>> newline .>> SharedParsers.ws <?> "scopes"
    let private supportedtargets = pstring "Supported Targets: " >>. sepBy (manyChars (noneOf ("," + "\r\n"))) (pstring ",") .>> newline .>> SharedParsers.ws <?> "targets"
    let private traits = pstring "Traits: " >>. restOfLine false .>> SharedParsers.ws <?> "traits"
    let private tname = (many1Chars idChar) .>> SharedParsers.ws .>> pchar '-' .>> SharedParsers.ws <?> "name"
    let private endOfDesc = newline >>. (pstring "Supported Scopes:" <|> pstring "Supported Targets:" <|> pstring "Traits:")
    let private desc = (many1CharsTill anyChar (followedBy endOfDesc)) .>> SharedParsers.ws <?> "desc"
    let private trigger = pipe5 tname desc (opt traits) (opt supportedscopes) (opt supportedtargets) (fun n d tr s t -> {name = n; desc = d; traits = tr; usage = ""; scopes = s |> Option.defaultValue []; targets = t |> Option.defaultValue []})

    let private triggerFile = SharedParsers.ws >>. triggerheader >>. many1 (attempt (spacer >>. trigger)) .>> eof
    let private effectFile = SharedParsers.ws >>. effectheader >>. many1 (attempt (spacer >>. trigger)) .>> eof

    let parseTriggerFile filepath = runParserOnFile triggerFile () filepath (System.Text.Encoding.GetEncoding(1252))
    let parseTriggerFilesRes filepath = parseTriggerFile filepath |> (function |Success(p, _, _) -> p |_ -> [])
    let parseTriggerStream file = runParserOnStream triggerFile () "triggerFile" file (System.Text.Encoding.GetEncoding(1252))

    let parseEffectFile filepath = runParserOnFile effectFile () filepath (System.Text.Encoding.GetEncoding(1252))
    let parseEffectFilesRes filepath = parseEffectFile filepath |> (function |Success(p, _, _) -> p |_ -> [])
    let parseEffectStream file = runParserOnStream effectFile () "effectFile" file (System.Text.Encoding.GetEncoding(1252))

    let toDocEffect<'a when 'a : comparison> effectType (parseScopes) (x : RawEffect)  = DocEffect<'a>(x, effectType, parseScopes)

    let processEffects parseScopes e = e |> List.map (toDocEffect EffectType.Effect parseScopes)
    let processTriggers parseScopes (t : RawEffect list) =
        t |> List.map (fun t -> if t.traits.IsSome && t.traits.Value.Contains "<, <=, =, !=, >, >=" then toDocEffect EffectType.ValueTrigger parseScopes t else toDocEffect EffectType.Trigger parseScopes t)
