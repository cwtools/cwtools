module HtmlTemplates

open Giraffe.GiraffeViewEngine
open CWTools.Rules
open CWTools.Utilities.StringResource

let createTableOfContents (types : TypeDefinition list) =
    let groupedByTopFolder = types |> List.groupBy (fun td -> td.pathOptions.paths |> List.tryHead |> Option.map (fun s -> s.Split('/').[0]) |> Option.defaultValue "")
    let createLink typeName = li [] [a [ _href ("#" + typeName) ] [ str typeName ]]
    let createForGroup (topFolder, (innerTypes : TypeDefinition list)) =
        li [] [
            str topFolder
            ul [] (innerTypes |> List.sortBy (fun td -> td.name) |> List.map (fun td -> createLink td.name))
        ]
    let links = groupedByTopFolder |> List.map createForGroup
    ul [_class "content"] links

let valueTypeField enums (vt : ValueType) =
    match vt with
    |(ValueType.Bool) -> "yes/no"
    |(ValueType.Date) -> "date"
    |(ValueType.Int (RulesParser.intFieldDefaultMinimum, RulesParser.intFieldDefaultMaximum)) -> "Integer"
    |(ValueType.Int (RulesParser.intFieldDefaultMinimum, max)) -> sprintf "Integer below %i" max
    |(ValueType.Int (min, RulesParser.intFieldDefaultMaximum)) -> sprintf "Integer above %i" min
    |(ValueType.Int (min, max)) -> sprintf "Integer between %i and %i" min max
    |(ValueType.Float (min, max)) when min = RulesParser.floatFieldDefaultMinimum && max = RulesParser.floatFieldDefaultMaximum -> "Float"
    |(ValueType.Float (min, max)) when min = RulesParser.floatFieldDefaultMinimum -> sprintf "Float below %s" (max.ToString())
    |(ValueType.Float (min, max)) when max = RulesParser.floatFieldDefaultMaximum -> sprintf "Float above %s" (min.ToString())
    |(ValueType.Float (min, max)) -> sprintf "Float between %s and %s" (min.ToString()) (max.ToString())
    |(ValueType.Percent) -> "percentage"
    |(ValueType.Enum enumName) ->
        let enumDef = enums |> List.tryFind (fun e -> e.key = enumName)
        enumDef |> Option.map (fun ed -> (ed.values |> String.concat ", "))
                |> Option.defaultValue ""
    | ValueType.CK2DNA -> "ck2DNA"
    | ValueType.CK2DNAProperty -> "ck2DNAproperty"
    | ValueType.IRFamilyName -> "IRFamilyName"

let createTypeFieldLink (typeType : TypeType) =
    match typeType with
    | TypeType.Simple s when s.Contains "." ->
        let splitTypes = s.Split('.', 2)
        let firstPart = splitTypes.[0]
        let link = a [ _href ("#"+firstPart)] [str ("<"+firstPart+">")]
        span [] [link; (str (" of subtype "+(splitTypes.[1])))]
    | TypeType.Simple s ->  a [ _href ("#"+s)] [str ("<"+s+">")]
    | _ -> str ""

let fieldToText enums (field : NewField) =
    match field with
    | SpecificField (SpecificValue value) -> str (stringManager.GetStringForID value.normal)
    | TypeField tt -> createTypeFieldLink tt
    | ValueField (vt) -> str (valueTypeField enums vt)
    | LocalisationField true -> str "Synchronised localisation key"
    | LocalisationField false -> str "Localisation key"
    | FilepathField (prefix, extension) ->
        match prefix, extension with
        | None, None -> str "Filepath"
        | Some prefix, None -> str (sprintf "Filepath in folder \"%s\"" prefix)
        | None, Some extension -> str (sprintf "Filepath with extension %s" extension)
        | Some prefix, Some extension -> str (sprintf "Filepath in folder \"%s\" with extension %s" prefix extension)
    | IconField icon -> str (sprintf "%s icon" icon)
    | ScalarField (ScalarValue) -> str "Scalar"
    | ScopeField (scope) -> str (sprintf "Scope object in %O scope" scope)
    // TODO more detail
    | VariableField (isInt, (min, max)) ->
        match isInt with
        | true -> str "Integer or integer variable"
        | false -> str "Float or float variable"
    | VariableGetField v -> str (sprintf "A \"%s\" value" v)
    // TODO clearer
    | VariableSetField v -> str (sprintf "Scalar, a \"%s\" value" v)
    | ValueScopeField _
    | AliasValueKeysField _ ->
        // TODO better for these
        str ""
    | ValueScopeMarkerField _
    | TypeMarkerField _
    | SubtypeField _
    | SingleAliasField _
    | MarkerField _
    | AliasField _ ->
        str ""
    // | _ -> str ""

let rhsFieldToText (enums : EnumDefinition list) (field : NewField) =
    match field with
    | SpecificField (SpecificValue value) -> str (stringManager.GetStringForID value.normal)
    | TypeField tt -> createTypeFieldLink tt
    | ValueField (vt) -> str (valueTypeField enums vt)
    | LocalisationField true -> str "Synchronised localisation key"
    | LocalisationField false -> str "Localisation key"
    | FilepathField (prefix, extension) ->
        match prefix, extension with
        | None, None -> str "Filepath"
        | Some prefix, None -> str (sprintf "Filepath in folder \"%s\"" prefix)
        | None, Some extension -> str (sprintf "Filepath with extension %s" extension)
        | Some prefix, Some extension -> str (sprintf "Filepath in folder \"%s\" with extension %s" prefix extension)
    | IconField icon -> str (sprintf "%s icon" icon)
    | ScalarField (ScalarValue) -> str "Scalar"
    | ScopeField (scope) -> str (sprintf "Scope object in %O scope" scope)
    // TODO more detail
    | VariableField (isInt, (min, max)) ->
        match isInt with
        | true -> str "Integer or integer variable"
        | false -> str "Float or float variable"
    | VariableGetField v -> str (sprintf "A \"%s\" value" v)
    // TODO clearer
    | VariableSetField v -> str (sprintf "Scalar, a \"%s\" value" v)
    | ValueScopeField _
    | AliasValueKeysField _ ->
        // TODO better for these
        str ""
    | ValueScopeMarkerField _
    | TypeMarkerField _
    | SubtypeField _
    | SingleAliasField _
    | MarkerField _ ->
        str ""
    | AliasField x -> str (x + " fields")

let replaceScopesToText (replaceScopes : ReplaceScopes) =
    let rootText = replaceScopes.root |> Option.map (fun r -> sprintf "ROOT: %s" (r.ToString()))
    let fromText =
        replaceScopes.froms
        |> Option.map (fun froms -> froms |> List.mapi (fun i s -> (String.replicate (i+1) "FROM") + ": " + (s.ToString())) |> String.concat ", ")
    match rootText, fromText with
    | Some rt, Some "" -> rt
    | Some rt, Some ft -> rt + ", " + ft
    | Some rt, None -> rt
    | None, Some "" -> ""
    | None, Some ft -> ft
    | None, None -> ""

let getReqCount (options : Options) =
    match options.min, options.max with
    | 0, 1 -> "Optional"
    | 0, RulesParser.cardinalityDefaultMaximum -> "Optional, many"
    | 0, x -> sprintf "Optional, up to %i" x
    | 1, 1 -> "Required"
    | 1, RulesParser.cardinalityDefaultMaximum -> "Required, many"
    | 1, x -> sprintf "Required, up to %i" x
    | x, y -> sprintf "Min %i, up to %i" x y

let rec ruleTemplate (enums : EnumDefinition list) (maxDepth : int) (indent : int) ((rule, options) : NewRule) =
    let lhs = rule |> (function |NodeRule (left, _) -> Some left |LeafRule (left, _) -> Some left |LeafValueRule left -> Some left |ValueClauseRule _ -> None |SubtypeRule _ -> None)
    let colspan = (maxDepth - indent).ToString()
    let reqCount = td [] [str (getReqCount options)]
    match rule with
    | LeafRule (left, right) ->
        let lhs = td [ _colspan colspan] [(fieldToText enums left)]
        let rhs = td [] [(rhsFieldToText enums right)]
        let desc = td [] [str (options.description |> Option.defaultValue "")]
        [(tr [] [lhs; desc; reqCount; rhs])]
    | NodeRule (left, [LeafRule(AliasField x, _), innerOptions]) ->
        let lhs = td [_colspan colspan] [(fieldToText enums left)]
        let desc = td [] [str (options.description |> Option.defaultValue "")]
        let scopes = options.replaceScopes |> Option.map replaceScopesToText
        let rhsText = if scopes.IsSome then (x + " block, with scopes " + scopes.Value) else (x + " block")
        let rhs = td [] [str rhsText]
        [(tr [] [lhs; desc; reqCount; rhs])]
    | NodeRule (left, inner) ->
        let desc = td [_colspan colspan] [str (options.description |> Option.defaultValue "")]
        let rhs = td [] [strong [] [str "block, containing:"]]
        let inners = (inner |> List.collect (ruleTemplate enums maxDepth (indent + 1)))
        let lhs = td [ _rowspan ((inners.Length + 1).ToString()); ] [ strong [] [(fieldToText enums left)]]
        ((tr [] ([lhs; desc; reqCount; rhs]))::inners)

    | LeafValueRule (left) ->
        let lhs = td [_colspan colspan] [(fieldToText enums left)]
        let desc = td [] [str (options.description |> Option.defaultValue "")]
        [(tr [] [lhs; desc; reqCount; td [] [str ""]])]
    | SubtypeRule(_,_, inner) -> inner |> List.collect (ruleTemplate enums maxDepth indent)
    // | _ -> []
    // lhs |> Option.map fieldToText
    //     |> Option.map (fun t -> tr [] [td [] [str t]; td [] [str (options.description |> Option.defaultValue "")];])

let rec getTypeBlockDepth (depth : int) ((rule, options): NewRule) =
    match rule with
    | SubtypeRule (_, _, inner) ->
        inner |> List.map (getTypeBlockDepth (depth)) |> List.max
    | NodeRule (_, []) ->
        depth
    | NodeRule (_, inner) ->
        inner |> List.map (getTypeBlockDepth (depth + 1)) |> List.max
    | _ -> depth

let typeBlock (enums : EnumDefinition list) ((typeDef : TypeDefinition), ((rule, options): NewRule)) =
    let _, rules = rule |> (function |NodeRule (l, r) -> l, r)
    let typeBlockDepth = getTypeBlockDepth 0 (rule, options)
    let typeName = typeDef.name
    let tableHeader = tr [] [th [ _colspan (typeBlockDepth.ToString())] [str "field"]; th [] [str "description"]; th [] [str "required"] ;th [] [str "rhs"]]
    let description =
        options.description |> Option.map ((sprintf "Description %s") >> str)
        |> Option.map (fun s -> div [] [s])
    div [] [
        h2 [ _class "title"; _id typeName] [ str typeName]
        div [] (typeDef.pathOptions.paths |> List.map ((sprintf "path: %s") >> str))
        div [] ([description] |> List.choose id)
        table [ _class "table is-striped is-fullwidth"] (tableHeader::(rules |> List.collect (ruleTemplate enums typeBlockDepth 0))) ]

let rootRules (rootRules : RootRule list) (enums : EnumDefinition list) (types : TypeDefinition list) =
    let typeBlocks = ((rootRules)
            |> List.choose (function |TypeRule (name, rule) -> Some (name, rule) |_ -> None)
            |> List.sortBy fst
            |> List.choose (function (name, rule) ->
                                        let typeDef = types |> List.tryFind (fun td -> td.name = name)
                                        typeDef |> Option.map (fun td -> typeBlock enums ((td), (rule)))))
    let tableOfContents = createTableOfContents types
    html [] [
        meta [ _name "viewport"; _content "width=device-width, initial-scale=1"]
        head [] [
            rawText "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.5/css/bulma.css\" integrity=\"sha256-ujE/ZUB6CMZmyJSgQjXGCF4sRRneOimQplBVLu8OU5w=\" crossorigin=\"anonymous\" />"]
        body []
            [
                section [ _class "section"]
                 [
                    div [ _class "container"] (tableOfContents::typeBlocks)
                ]
            ]
    ] |> renderHtmlDocument