namespace CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser.Types
open CWTools.Process.STLScopes
open CWTools.Common
open CWTools.Common.STLConstants
open DotNet.Globbing
open CWTools.Validation.STLValidation
open System.Xml.Linq
open System.Threading
open CWTools.Localisation
open FParsec

module STLLocalisationString =
    type LocElement =
    | Ref of string
    | Chars of string
    let valueChars = many1Satisfy ( (<>) '$' )
    let ref = between (skipChar '$') (skipChar '$') (valueChars |>> Ref)
    let locStringParser = many (valueChars |>> Chars <|> ref) .>> eof

    let parseLocString fileString filename = runParserOnString locStringParser () filename fileString

    let validateLocalisation (api : (Lang * Map<string, Entry>)) =
        let lang = api |> fst
        let all = api |> snd
        let extractResult =
            function
            |Success (v, _, _) -> v
            |Failure _ -> []
        let checkRef (entry : Entry) (r : string) =
            match all.ContainsKey r with
            | true -> OK
            | false ->
                Invalid [invManual (ErrorCodes.UndefinedLocReference entry.key r (lang :> obj)) (Position.Conv entry.position) entry.key None ]
                
        let parsed = all |> Map.map (fun k v -> v, parseLocString v.desc "" |> extractResult)
        parsed |> Map.toList <&!&> (fun (k,(e, v)) -> v |> List.choose (function |Ref s -> Some s |_ -> None) <&!&> checkRef e )