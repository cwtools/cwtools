module HtmlTemplates

open Giraffe.GiraffeViewEngine
open CWTools.Rules
open CWTools.Utilities.StringResource

let fieldToText (field : NewField) =
    match field with
    | SpecificField (SpecificValue value) -> stringManager.GetStringForID value.normal
    | _ -> ""

let ruleTemplate ((rule, options) : NewRule) =
    let lhs = rule |> (function |NodeRule (left, _) -> Some left |LeafRule (left, _) -> Some left |LeafValueRule left -> Some left |ValueClauseRule _ -> None |SubtypeRule _ -> None)
    lhs |> Option.map fieldToText
        |> Option.map (fun t -> tr [] [td [] [str t]; td [] [str (options.description |> Option.defaultValue "")]])
let typeBlock ((typeName : string), ((rule, options): NewRule)) =
    let _, rules = rule |> (function |NodeRule (l, r) -> l, r)
    div [] [
        h2 [ _class "title" ] [ str typeName]
        table [ _class "table"] (rules |> List.choose ruleTemplate) ]
let rootRules (rootRules : RootRule list) =
    html [] [
        meta [ _name "viewport"; _content "width=device-width, initial-scale=1"]
        head [] [
            rawText "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.5/css/bulma.css\" integrity=\"sha256-ujE/ZUB6CMZmyJSgQjXGCF4sRRneOimQplBVLu8OU5w=\" crossorigin=\"anonymous\" />"]
        body []
            [
                section [ _class "section"]
                 [
                    div [ _class "container"] ((rootRules) |> List.choose (function |TypeRule (name, rule) -> Some (typeBlock (name, rule)) |_ -> None))
                ]
            ]
    ] |> renderHtmlDocument