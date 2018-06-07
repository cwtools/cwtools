namespace CWTools.Localisation
open CWTools.Common
open Microsoft.FSharp.Compiler.Range

module YAMLLocalisationParser =
    open FParsec

    // type Entry = {
    //     key : string
    //     value : char option
    //     desc : string
    //     position : Position
    // }

    type LocFile = {
        key : string
        entries : Entry list
    }
    
    //let key = charsTillString ":" true 1000 .>> spaces <?> "key"
    let key = many1Satisfy ( (=) ':' >> not ) .>> pchar ':' .>> spaces <?> "key"
    //let descInner = (charsTillString "§")
    //let stringThenEscaped = pipe2 (manyCharsTill (noneOf ['"']) (pchar '§')) (manyCharsTill anyChar (pstring "§!")) (+) <?> "escaped"
    //let desc = pipe2 (pchar '"') (many ((attempt stringThenEscaped) <|> manyCharsTill anyChar (pchar '"')) |>> List.reduce (+)) (fun a b -> string a + b)
    //let desc = between (pchar '"') (pchar '"') (charsTillString "\"" false 10000) .>> spaces <?> "desc"
    //let desc = pipe3 (pchar '"' |>> string) (many (attempt stringThenEscaped) |>> List.fold (+) "")  (manyCharsTill (noneOf ['§']) (pchar '"')) (fun a b c -> string a + b + c) <?> "string"
    let desc = restOfLine true .>> spaces <?> "desc"
    let value = digit .>> spaces <?> "version"
    let getRange (start: FParsec.Position) (endp : FParsec.Position) = mkRange start.StreamName (mkPos (int start.Line) (int start.Column)) (mkPos (int endp.Line) (int endp.Column))

    let entry = pipe5 (getPosition) (key) (opt value) (desc .>> spaces) (getPosition) (fun s k v d e -> {key = k; value = v; desc = d; position = getRange s e}) <?> "entry"
    let comment = pstring "#" >>. restOfLine true .>> spaces <?> "comment"
    let file = spaces >>. many (attempt comment) >>. pipe2 (key) (many ((attempt comment |>> (fun _ -> None)) <|> (entry |>> Some)) .>> eof) (fun k es -> {key = k; entries = List.choose id es}) <?> "file"

    let parseLocFile filepath = runParserOnFile file () filepath System.Text.Encoding.UTF8
    let parseLocText text name = runParserOnString file () name text
