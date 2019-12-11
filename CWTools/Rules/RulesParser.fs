namespace CWTools.Rules


open FParsec
open CWTools.Utilities.Position
open CWTools.Utilities
open CWTools.Parser.Types
open CWTools.Common
open CWTools.Common.STLConstants
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Utilities.Utils
open System
open CWTools.Parser
open CWTools.Common.NewScope
open CWTools.Utilities.StringResource


type ReplaceScopes = {
    root : Scope option
    this : Scope option
    froms : Scope list option
    prevs : Scope list option
}
type Options = {
    min : int
    max : int
    strictMin : bool
    leafvalue : bool
    description : string option
    pushScope : Scope option
    replaceScopes : ReplaceScopes option
    severity : Severity option
    requiredScopes : Scope list
    comparison : bool
    referenceDetails : (bool * string) option
    keyRequiredQuotes : bool
    valueRequiredQuotes : bool
}

type PathOptions = {
    paths : string list
    pathStrict : bool
    pathFile : string option
    pathExtension : string option
}

[<Struct>]
type ValueType =
| Enum of enumc : string
| Float of minmax: (decimal*decimal)
| Bool
| Int of minmaxi: (int*int)
| Percent
| Date
| DateTime
| CK2DNA
| CK2DNAProperty
| IRFamilyName
| STLNameFormat of variable : string
    override x.ToString() =
        match x with
        // | Scalar -> "Scalar"
        | Enum enumc -> sprintf "Enum %s" enumc
        // | Specific valuec -> sprintf "Specific %s" (StringResource.stringManager.GetStringForIDs valuec)
        | Float (min, max) -> sprintf "Float with min %f and max %f" min max
        | Bool -> "Bool"
        | Int (min, max) -> sprintf "Int with min %i and max %i" min max
        | Percent -> "Percent"
        | Date -> "Date"
        | DateTime -> "DateTime"
        | CK2DNA -> "CK2DNA"
        | CK2DNAProperty -> "CK2DNAProperty"
        | IRFamilyName -> "IRFamilyName"
        | STLNameFormat x -> sprintf "STLNameFormat %s" x

[<Struct>]
type SpecificValue = |SpecificValue of valuec : StringTokens
[<Struct>]
type ScalarValue = |ScalarValue

type TypeType =
| Simple of name: string
| Complex of prefix : string * name : string * suffix : string

type Marker =
| ColourField
| IRCountryTag

type TypeLocalisation = {
    name : string
    prefix : string
    suffix: string
    required : bool
    optional : bool
    explicitField : string option
    replaceScopes : ReplaceScopes option
    primary : bool
}

type SkipRootKey = |SpecificKey of string |AnyKey |MultipleKeys of string list * bool
type SubTypeDefinition = {
    name : string
    displayName : string option
    abbreviation : string option
    rules : NewRule list
    typeKeyField : string option
    startsWith : string option
    pushScope : Scope option
    localisation : TypeLocalisation list
}
and TypeDefinition = {
    name : string
    nameField : string option
    pathOptions : PathOptions
    conditions : Node option
    subtypes : SubTypeDefinition list
    typeKeyFilter : (string list * bool) option
    skipRootKey : SkipRootKey list
    startsWith : string option
    type_per_file : bool
    warningOnly : bool
    unique : bool
    localisation : TypeLocalisation list
    graphRelatedTypes : string list
}

and NewField =
| ValueField of ValueType
| SpecificField of SpecificValue
| ScalarField of ScalarValue
| TypeField of TypeType
/// This is only used internally to match type definitions
| TypeMarkerField of dummyKey : StringLowerToken * typedef : TypeDefinition
| ScopeField of Scope
| LocalisationField of synced : bool * isInline : bool
| FilepathField of prefix : string option * extension : string option
| IconField of string
/// The keys of an alias rule
| AliasValueKeysField of string
| AliasField of string
| SingleAliasField of string
| SingleAliasClauseField of string * string
| SubtypeField of string * bool * NewRule list
| VariableSetField of string
| VariableGetField of string
| VariableField of isInt : bool * minmax : (decimal * decimal)
| ValueScopeMarkerField of isInt : bool * minmax : (decimal * decimal)
| ValueScopeField of isInt : bool * minmax : (decimal * decimal)
| MarkerField of Marker
| JominiGuiField
| IgnoreMarkerField
| IgnoreField of field : NewField
    override x.ToString() =
        match x with
        | ValueField vt -> sprintf "Field of %O" vt
        | ScalarField sv -> "Field of any value"
        | SpecificField (SpecificValue sv) -> sprintf "Field of %s" (stringManager.GetStringForID sv.normal)
        | _ -> sprintf "Field of %A" x
and RuleType =
|NodeRule of left : NewField * rules : NewRule list
|LeafRule of left : NewField * right : NewField
|LeafValueRule of right : NewField
|ValueClauseRule of rules : NewRule list
|SubtypeRule of string * bool * NewRule list
    override x.ToString() =
        match x with
        | NodeRule (l, r) -> sprintf "NodeRule with Left (%O) and inner (%O)" l r
        | LeafRule (l, r) -> sprintf "LeafRule with Left (%O) and right (%O)" l r
        | LeafValueRule (r) -> sprintf "LeafValueRule (%O)" r
        | ValueClauseRule (rs) -> sprintf "ValueClauseRule with inner (%O)" rs
        | SubtypeRule (n, p, r) -> sprintf "SubtypeRule %s with inner (%O)" n r
and NewRule = RuleType * Options

type RootRule =
| AliasRule of string * NewRule
| SingleAliasRule of string * NewRule
| TypeRule of string * NewRule
    override x.ToString() =
        match x with
        | AliasRule (n, r) -> sprintf "Alias definition %s (%O)" n r
        | SingleAliasRule (n, r) -> sprintf "Single alias definition %s (%O)" n r
        | TypeRule (n, r) -> sprintf "Type rule %s (%O)" n r
// type EffectRule = Rule // Add scopes

type EnumDefinition = {
        key : string
        description : string
        values : string list
    }
type ComplexEnumDef = {
    name : string
    description : string
    pathOptions : PathOptions
    nameTree : Node
    start_from_root : bool
}

[<RequireQualifiedAccess>]
module RulesParser =
    let specificField x = SpecificField(SpecificValue (StringResource.stringManager.InternIdentifierToken x))
    let private parseSeverity =
        function
        |"error" -> Severity.Error
        |"warning" -> Severity.Warning
        |"info" -> Severity.Information
        |"information" -> Severity.Information
        |"hint" -> Severity.Hint
        |s -> failwithf "Invalid severity %s" s
    let defaultOptions = { min = 0; max = 1000; strictMin = true; leafvalue = false; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = []; comparison = false; referenceDetails = None; keyRequiredQuotes = false; valueRequiredQuotes = false }
    let requiredSingle : Options = { defaultOptions with min = 1; max = 1 }
    let requiredMany = { defaultOptions with min = 1; max = 100 }
    let optionalSingle : Options = { defaultOptions with min = 0; max = 1 }
    let optionalMany : Options = { defaultOptions with min = 0; max = 100 }

    [<Literal>]
    let intFieldDefaultMinimum = Int32.MinValue
    [<Literal>]
    let intFieldDefaultMaximum = Int32.MaxValue
    let floatFieldDefaultMinimum = -1E+12M
    let floatFieldDefaultMaximum = 1E+12M
    [<Literal>]
    let cardinalityDefaultMaximum = 10000
    let defaultFloat = ValueField (ValueType.Float (floatFieldDefaultMinimum, floatFieldDefaultMaximum))
    let defaultInt = ValueField (ValueType.Int (intFieldDefaultMinimum, intFieldDefaultMaximum))
    let private getNodeComments (clause : IClause) =
        let findComments (t : range) s (a : Child) =
                match (s, a) with
                | ((b, c), _) when b -> (b, c)
                | ((_, c), CommentC (_, nc)) when nc.StartsWith("#") -> (false, nc::c)
                | ((_, c), CommentC (_, nc)) -> (false, c)
                | ((_, c), NodeC n) when n.Position.Code = t.Code -> (true, c)
                | ((_, c), LeafC v) when v.Position.Code = t.Code -> (true, c)
                | ((_, c), LeafValueC v) when v.Position.Code = t.Code -> (true, c)
                | ((_, c), ValueClauseC vc) when vc.Position.Code = t.Code-> (true, c)
                | _ -> (false, [])
                // | ((_, c), LeafValueC lv) when lv.Position = t -> (true, c)
                // | ((_, _), _) -> (false, [])
        //let fNode = (fun (node:Node) (children) ->
        let one = clause.Leaves |> Seq.map (fun e -> LeafC e, clause.AllArray |> Array.fold (findComments e.Position) (false, []) |> snd) |> List.ofSeq
        //log "%s %A" node.Key (node.All |> List.rev)
        //log "%A" one
        let two = clause.Nodes |> Seq.map (fun e -> NodeC e, clause.AllArray |> Array.fold (findComments e.Position) (false, []) |> snd |> (fun l -> (l))) |> List.ofSeq
        let three = clause.LeafValues |> Seq.toList |> List.map (fun e -> LeafValueC e, clause.AllArray |> Array.fold (findComments e.Position) (false, []) |> snd)
        let four = clause.ValueClauses |> Seq.toList |> List.map (fun e -> ValueClauseC e, clause.AllArray |> Array.fold (findComments e.Position) (false, []) |> snd)
        let new2 = one @ two @ three @ four
        new2

    let getSettingFromString (full : string) (key : string) =
        let setting = full.Substring(key.Length)
        if not (setting.StartsWith "[" && setting.EndsWith "]") then None else
            Some (setting.Substring(1, setting.Length - 2))

    let getFloatSettingFromString (full : string) =
        match getSettingFromString full "float" with
        |Some s ->
            let split = s.Split([|".."|], 2, StringSplitOptions.None)
            let parseDecimal (s : string) =
                match s, Decimal.TryParse s with
                | "inf", _ -> Some (decimal floatFieldDefaultMaximum)
                | "-inf", _ -> Some (decimal floatFieldDefaultMinimum)
                | _, (true, num) -> Some (num)
                | _, (false, _) -> None
            if split.Length < 2 then None else
                match (parseDecimal split.[0]), (parseDecimal split.[1]) with
                | Some min, Some max -> Some (min, max)
                | _ -> None
        |None -> None


    let getIntSettingFromString (full : string) =
        match getSettingFromString full "int" with
        |Some s ->
            let split = s.Split([|".."|], 2, StringSplitOptions.None)
            let parseInt (s : string) =
                match s, Int32.TryParse s with
                | "inf", _ -> Some intFieldDefaultMaximum
                | "-inf", _ -> Some intFieldDefaultMinimum
                | _, (true, num) -> Some num
                | _, (false, _) -> None
            if split.Length < 2 then None else
                match (parseInt split.[0]), (parseInt split.[1]) with
                | Some min, Some max -> Some (min, max)
                | _ -> None
        |None -> None

    let getAliasSettingsFromString (full : string) =
        match getSettingFromString full "alias" with
        |Some s ->
            let split = s.Split([|":"|], 2, StringSplitOptions.None)
            if split.Length < 2 then None else Some (split.[0], split.[1])
        |None -> None
    let getSingleAliasSettingsFromString (full : string) =
        match getSettingFromString full "single_alias" with
        |Some s ->
            let split = s.Split([|":"|], 2, StringSplitOptions.None)
            if split.Length < 2 then None else Some (split.[0], split.[1])
        |None -> None

    let getPathOptions (node : Node) =
        let path = (node.TagsText "path") |> List.ofSeq |> List.map (fun s -> s.Replace("game/","").Replace("game\\",""))
        let pathStrict = node.TagText "path_strict" == "yes"
        let pathFile = if node.Has "path_file" then Some (node.TagText "path_file") else None
        let pathExtension = if node.Has "path_extension" then Some (node.TagText "path_extension") else None
        {
            paths = path
            pathStrict = pathStrict
            pathFile = pathFile
            pathExtension = pathExtension
        }

    let inline private replaceScopes parseScope (comments : string list) =
        match comments |> List.tryFind (fun s -> s.Contains("replace_scope")) with
        | Some s ->
            let s = s.Trim('#')
            let parsed = CKParser.parseString s "config"
            match parsed with
            | Failure(_) -> None
            | Success(s,_,_) ->
                let n = (STLProcess.shipProcess.ProcessNode EntityType.Other "root" (mkZeroFile "config") s)
                match n.Child "replace_scope" with
                | Some c ->
                    let this = if c.Has "this" then c.TagText "this" |> parseScope |> Some else None
                    let root = if c.Has "root" then c.TagText "root" |> parseScope |> Some else None
                    let from = if c.Has "from" then c.TagText "from" |> parseScope |> Some else None
                    let fromfrom = if c.Has "fromfrom" then c.TagText "fromfrom" |> parseScope |> Some else None
                    let fromfromfrom = if c.Has "fromfromfrom" then c.TagText "fromfromfrom" |> parseScope |> Some else None
                    let fromfromfromfrom = if c.Has "fromfromfromfrom" then c.TagText "fromfromfromfrom" |> parseScope |> Some else None
                    let froms = [from;fromfrom;fromfromfrom;fromfromfromfrom] |> List.choose id
                    let prev = if c.Has "prev" then c.TagText "prev" |> parseScope |> Some else None
                    let prevprev = if c.Has "prevprev" then c.TagText "prevprev" |> parseScope |> Some else None
                    let prevprevprev = if c.Has "prevprevprev" then c.TagText "prevprevprev" |> parseScope |> Some else None
                    let prevprevprevprev = if c.Has "prevprevprevprev" then c.TagText "prevprevprevprev" |> parseScope |> Some else None
                    let prevs = [prev;prevprev;prevprevprev;prevprevprevprev] |> List.choose id
                    Some { root = root; this = this; froms = Some froms; prevs = Some prevs }
                | None -> None
        | None -> None


    let getOptionsFromComments (parseScope) (allScopes) (anyScope) (operator : Operator) (keyRequiredQuotes : bool) (valueRequiredQuotes : bool) (comments : string list) =
        let min, max, strictmin =
            match comments |> List.tryFind (fun s -> s.Contains("cardinality")) with
            | Some c ->
                let nums = c.Substring(c.IndexOf "=" + 1).Trim().Split([|".."|], 2, StringSplitOptions.None)
                try
                    let minText, strictMin =
                        if nums.[0].StartsWith "~" then nums.[0].Substring(1), false else nums.[0], true
                    match minText, nums.[1] with
                    | min, "inf" -> (int min), cardinalityDefaultMaximum, strictMin
                    | min, max -> (int min), (int max), strictMin
                with
                | _ -> 1, 1, true
            | None -> 1, 1, true
        let description =
            match comments |> List.tryFind (fun s -> s.StartsWith "##") with
            | Some d -> Some (d.Trim('#'))
            | None -> None
        let pushScope =
            match comments |> List.tryFind (fun s -> s.Contains("push_scope")) with
            | Some s -> s.Substring(s.IndexOf "=" + 1).Trim() |> parseScope |> Some
            | None -> None
        let reqScope =
            match comments |> List.tryFind (fun s -> s.StartsWith("# scope =")) with
            | Some s ->
                let rhs = s.Substring(s.IndexOf "=" + 1).Trim()
                match rhs.StartsWith("{") && rhs.EndsWith("}") with
                | true -> rhs.Trim('{','}') |> (fun s -> s.Split([|' '|])) |> Array.filter (fun s -> s <> "") |> Array.map parseScope |> List.ofArray
                | false -> let scope = rhs |> parseScope in if scope = anyScope then allScopes else [scope]
            | None -> []
        let severity =
            match comments |> List.tryFind (fun s -> s.Contains("severity")) with
            | Some s -> s.Substring(s.IndexOf "=" + 1).Trim() |> parseSeverity |> Some
            | None -> None
        let referenceDetails =
            match comments |> List.tryFind (fun s -> s.Contains("outgoingReferenceLabel")) with
            | Some s -> s.Substring(s.IndexOf "=" + 1).Trim() |> (fun s -> true, s) |> Some
            | None ->
                match comments |> List.tryFind (fun s -> s.Contains("incomingReferenceLabel")) with
                | Some s -> s.Substring(s.IndexOf "=" + 1).Trim()|> (fun s -> false, s) |> Some
                | None -> None
        let comparison = operator = Operator.EqualEqual
        { min = min; max = max; strictMin = strictmin; leafvalue = false; description = description; pushScope = pushScope; replaceScopes = replaceScopes parseScope comments; severity = severity; requiredScopes = reqScope; comparison = comparison; referenceDetails = referenceDetails; keyRequiredQuotes = keyRequiredQuotes; valueRequiredQuotes = valueRequiredQuotes }

    let processKey parseScope anyScope =
        function
        | "scalar" -> ScalarField ScalarValue
        | "bool" -> ValueField ValueType.Bool
        | "percentage_field" -> ValueField ValueType.Percent
        | "localisation" -> LocalisationField (false, false)
        | "localisation_synced" -> LocalisationField (true, false)
        | "localisation_inline" -> LocalisationField (false, true)
        | "filepath" -> FilepathField (None, None)
        | x when x.StartsWith "filepath[" ->
            match getSettingFromString x "filepath" with
            | Some (setting) ->
                match setting.Contains "," with
                | true ->
                    match setting.Split([|','|], 2) with
                    | [|folder; extension|] ->
                        FilepathField (Some folder, Some extension)
                    | _ -> FilepathField (Some setting, None)
                | false ->
                    FilepathField (Some setting, None)
            | None -> FilepathField (None, None)
        | "date_field" -> ValueField Date
        | "datetime_field" -> ValueField DateTime
        | x when x.StartsWith "<" && x.EndsWith ">" ->
            TypeField (TypeType.Simple (x.Trim([|'<'; '>'|])))
        | x when x.Contains "<" && x.Contains ">" ->
            let x = x.Trim('"')
            let prefixI = x.IndexOf "<"
            let suffixI = x.IndexOf ">"
            TypeField (TypeType.Complex (x.Substring(0,prefixI), x.Substring(prefixI + 1, suffixI - prefixI - 1), x.Substring(suffixI + 1)))
        | "int" -> defaultInt
        | x when x.StartsWith "int[" ->
            match getIntSettingFromString x with
            | Some (min, max) -> ValueField (ValueType.Int (min, max))
            | None -> (defaultInt)
        | "float" -> defaultFloat
        | x when x.StartsWith "float[" ->
            match getFloatSettingFromString x with
            | Some (min, max) -> ValueField (ValueType.Float (min, max))
            | None -> (defaultFloat)
        | x when x.StartsWith "enum[" ->
            match getSettingFromString x "enum" with
            | Some (name) -> ValueField (ValueType.Enum name)
            | None -> ValueField (ValueType.Enum "")
        | x when x.StartsWith "icon[" ->
            match getSettingFromString x "icon" with
            | Some (folder) -> IconField folder
            | None -> ScalarField (ScalarValue)
        | x when x.StartsWith "alias_match_left[" ->
            match getSettingFromString x "alias_match_left" with
            | Some alias -> AliasField alias
            | None -> ScalarField (ScalarValue)
        | x when x.StartsWith "alias_name[" ->
            match getSettingFromString x "alias_name" with
            | Some alias -> AliasField alias
            | None -> ScalarField (ScalarValue)
        | "scope_field" -> ScopeField (anyScope)
        | "variable_field" -> VariableField (false, (floatFieldDefaultMinimum, floatFieldDefaultMaximum))
        | x when x.StartsWith "variable_field[" ->
            match getFloatSettingFromString x with
            | Some (min, max) -> VariableField (false,(min, max))
            | None -> VariableField (false,(floatFieldDefaultMinimum, floatFieldDefaultMaximum))
        | "int_variable_field" -> VariableField (true, (decimal intFieldDefaultMinimum, decimal intFieldDefaultMaximum))
        | x when x.StartsWith "int_variable_field[" ->
            match getIntSettingFromString x with
            | Some (min, max) -> VariableField (true,(decimal min,decimal max))
            | None -> VariableField (true,(decimal intFieldDefaultMinimum, decimal intFieldDefaultMaximum))
        | "value_field" -> ValueScopeMarkerField (false, (floatFieldDefaultMinimum, floatFieldDefaultMaximum))
        | x when x.StartsWith "value_field[" ->
            match getFloatSettingFromString x with
            | Some (min, max) -> ValueScopeMarkerField (false,(min, max))
            | None -> ValueScopeMarkerField (false,(floatFieldDefaultMinimum, floatFieldDefaultMaximum))
        | "int_value_field" -> ValueScopeMarkerField (true, (decimal intFieldDefaultMinimum, decimal intFieldDefaultMaximum))
        | x when x.StartsWith "int_value_field[" ->
            match getIntSettingFromString x with
            | Some (min, max) -> ValueScopeMarkerField (true,(decimal min,decimal max))
            | None -> ValueScopeMarkerField (true,(decimal intFieldDefaultMinimum, decimal intFieldDefaultMaximum))
        | x when x.StartsWith "value_set[" ->
            match getSettingFromString x "value_set" with
            | Some variable ->
                VariableSetField variable
            | None -> ScalarField (ScalarValue)
        | x when x.StartsWith "value[" ->
            match getSettingFromString x "value" with
            | Some variable ->
                VariableGetField variable
            | None -> ScalarField (ScalarValue)
        | x when x.StartsWith "scope[" ->
            match getSettingFromString x "scope" with
            | Some target ->
                ScopeField (parseScope target)
            | None -> ScalarField (ScalarValue)
        | x when x.StartsWith "event_target" ->
            match getSettingFromString x "event_target" with
            | Some target ->
                ScopeField (parseScope target)
            | None -> ScalarField (ScalarValue)
        | x when x.StartsWith "single_alias_right" ->
            match getSettingFromString x "single_alias_right" with
            | Some alias ->
                SingleAliasField alias
            | None -> ScalarField (ScalarValue)
        | x when x.StartsWith "alias_keys_field" ->
            match getSettingFromString x "alias_keys_field" with
            | Some aliasKey ->
                AliasValueKeysField aliasKey
            | None -> ScalarField (ScalarValue)
        | x when x.StartsWith "stellaris_name_format" ->
            match getSettingFromString x "stellaris_name_format" with
            | Some aliasKey ->
                ValueField (STLNameFormat aliasKey)
            | None -> ScalarField (ScalarValue)
        | "portrait_dna_field" -> ValueField CK2DNA
        | "portrait_properties_field" -> ValueField CK2DNAProperty
        | "colour_field" -> MarkerField Marker.ColourField
        | "ir_country_tag_field" -> MarkerField Marker.IRCountryTag
        | "ir_family_name_field" -> ValueField IRFamilyName
        | "ignore_field" -> IgnoreMarkerField
        | x ->
            // eprintfn "ps %s" x
            SpecificField (SpecificValue (StringResource.stringManager.InternIdentifierToken(x.Trim([|'\"'|]))))



    let configNode (processChildConfig) (parseScope) (allScopes) (anyScope) (node : Node) (comments : string list) (key : string) =
        let children = getNodeComments node
        let options = getOptionsFromComments parseScope allScopes anyScope (Operator.Equals) (node.KeyId.quoted) false comments
        let innerRules = children |> List.choose (processChildConfig parseScope allScopes anyScope)
        let rule =
            match key with
            |x when x.StartsWith "subtype[" ->
                match getSettingFromString x "subtype" with
                |Some st when st.StartsWith "!" -> SubtypeRule (st.Substring(1), false, (innerRules))
                |Some st -> SubtypeRule (st, true, (innerRules))
                |None -> failwith (sprintf "Invalid subtype string %s" x)
            |_ when node.KeyPrefixId.IsSome && node.ValuePrefixId.IsSome -> NodeRule(JominiGuiField, innerRules)
            |x -> NodeRule(processKey parseScope anyScope (x.Trim('"')), innerRules)
            // |"int" -> NodeRule(ValueField(ValueType.Int(Int32.MinValue, Int32.MaxValue)), innerRules)
            // |"float" -> NodeRule(ValueField(ValueType.Float(Double.MinValue, Double.MaxValue)), innerRules)
            // |"scalar" -> NodeRule(ValueField(ValueType.Scalar), innerRules)
            // |"filepath" -> NodeRule(FilepathField, innerRules)
            // |"scope" -> NodeRule(ScopeField(Scope.Any), innerRules)
            // |x when x.StartsWith "enum[" ->
            //     match getSettingFromString x "enum" with
            //     |Some e -> NodeRule(ValueField(ValueType.Enum e), innerRules)
            //     |None -> failwith (sprintf "Invalid enum string %s" x)
            // |x when x.StartsWith "<" && x.EndsWith ">" ->
            //     NodeRule(TypeField(x.Trim([|'<'; '>'|])), innerRules)
            // |x -> NodeRule(ValueField(ValueType.Specific x), innerRules)
        NewRule(rule, options)

    let configValueClause processChildConfig (parseScope) (allScopes) (anyScope) (valueclause : ValueClause) (comments : string list) =
        let children = getNodeComments valueclause
        let options = getOptionsFromComments parseScope allScopes anyScope (Operator.Equals) false false comments
        let innerRules = children |> List.choose (processChildConfig parseScope allScopes anyScope)
        let rule = ValueClauseRule innerRules
        NewRule(rule, options)




    let rgbRule = LeafValueRule (ValueField (ValueType.Int (0, 255))), { min = 3; max = 4; strictMin = true; leafvalue = true; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = []; comparison = false; referenceDetails = None; keyRequiredQuotes = false; valueRequiredQuotes = false }
    let hsvRule = LeafValueRule (ValueField (ValueType.Float (0.0M, 2.0M))), { min = 3; max = 4; strictMin = true; leafvalue = true; description = None; pushScope = None; replaceScopes = None; severity = None; requiredScopes = []; comparison = false; referenceDetails = None; keyRequiredQuotes = false; valueRequiredQuotes = false }

    let configLeaf processChildConfig (parseScope) (allScopes) (anyScope) (leaf : Leaf) (comments : string list) (key : string) =
        let leftfield = processKey parseScope anyScope (key.Trim('"'))
        let options = getOptionsFromComments parseScope allScopes anyScope (leaf.Operator) (leaf.KeyId.quoted) (leaf.ValueId.quoted) comments
        let rightkey = leaf.Value.ToString()
        match key, rightkey with
        |_, x when x.StartsWith("colour[") ->
            let colourRules =
                match getSettingFromString x "colour" with
                |Some "rgb" -> [rgbRule]
                |Some "hsv" -> [hsvRule]
                |_ -> [rgbRule; hsvRule]
            NewRule(NodeRule(leftfield, colourRules), options)
        |l, r when l.StartsWith "clause_single_alias" && r.StartsWith "single_alias_right" ->
            match getSettingFromString l "clause_single_alias", getSettingFromString r "single_alias_right" with
            |Some ls, Some rs ->
                let leftfield = LeafValueRule(SingleAliasClauseField (ls, rs))
                NewRule(leftfield, options)
            |_ ->
                let rightfield = processKey parseScope anyScope (rightkey.Trim('"'))
                let leafRule = LeafRule(leftfield, rightfield)
                NewRule(leafRule, options)
        |_, x ->
            let rightfield = processKey parseScope anyScope (rightkey.Trim('"'))
            let leafRule = LeafRule(leftfield, rightfield)
            NewRule(leafRule, options)

    let configLeafValue processChildConfig (parseScope) allScopes (anyScope) (leafvalue : LeafValue) (comments : string list) =
        let field = processKey parseScope anyScope (leafvalue.Value.ToRawString())
            // match leafvalue.Value.ToRawString() with
            // |x when x.StartsWith "<" && x.EndsWith ">" ->
            //     TypeField (x.Trim([|'<'; '>'|]))
            // |x -> ValueField (ValueType.Enum x)
        let options = { getOptionsFromComments parseScope allScopes anyScope (Operator.Equals) false leafvalue.ValueId.quoted comments with leafvalue = true }
        NewRule(LeafValueRule(field), options)

    let configRootLeaf processChildConfig (parseScope) allScopes (anyScope) (leaf : Leaf) (comments : string list) =
        match leaf.Key with
        |x when x.StartsWith "alias[" ->
            match getAliasSettingsFromString x with
            |Some (a, rn) ->
                let innerRule = configLeaf processChildConfig parseScope allScopes anyScope leaf comments rn
                AliasRule (a, innerRule)
            |None ->
                let rule = configLeaf processChildConfig parseScope allScopes anyScope leaf comments leaf.Key
                TypeRule (x, rule)
        |x when x.StartsWith "single_alias[" ->
            match getSettingFromString x "single_alias" with
            |Some (a) ->
                let innerRule = configLeaf processChildConfig parseScope allScopes anyScope leaf comments x
                SingleAliasRule (a, innerRule)
            |None ->
                let rule = configLeaf processChildConfig parseScope allScopes anyScope leaf comments leaf.Key
                TypeRule (x, rule)
        |x ->
            let rule = configLeaf processChildConfig parseScope allScopes anyScope leaf comments leaf.Key
            TypeRule (x, rule)

    let configRootNode processChildConfig (parseScope) allScopes (anyScope) (node : Node) (comments : string list) =
        let children = getNodeComments node
        let options = getOptionsFromComments parseScope allScopes anyScope (Operator.Equals) false false comments
        let innerRules = children |> List.choose (processChildConfig parseScope allScopes anyScope)
        match node.Key with
        |x when x.StartsWith "alias[" ->
            match getAliasSettingsFromString x with
            |Some (a, rn) ->
                let innerRule = configNode processChildConfig parseScope allScopes anyScope node comments rn
                // log "%s %A" a innerRule
                AliasRule (a, innerRule)
            |None ->
                TypeRule (x, NewRule(NodeRule(SpecificField(SpecificValue (StringResource.stringManager.InternIdentifierToken x)), innerRules), options))
        |x when x.StartsWith "single_alias[" ->
            match getSettingFromString x "single_alias" with
            |Some (a) ->
                let innerRule = configNode processChildConfig parseScope allScopes anyScope node comments x
                SingleAliasRule (a, innerRule)
            |None ->
                TypeRule (x, NewRule(NodeRule(SpecificField(SpecificValue (StringResource.stringManager.InternIdentifierToken x)), innerRules), options))
        |x ->
            TypeRule (x, NewRule(NodeRule(SpecificField(SpecificValue (StringResource.stringManager.InternIdentifierToken x)), innerRules), options))

    let rec processChildConfig (parseScope) allScopes (anyScope) ((child, comments) : Child * string list)  =
        match child with
        |NodeC n -> Some (configNode processChildConfig parseScope allScopes anyScope n comments (n.Key))
        |ValueClauseC vc -> Some (configValueClause processChildConfig parseScope allScopes anyScope vc comments)
        |LeafC l -> Some (configLeaf processChildConfig parseScope allScopes anyScope l comments (l.Key))
        |LeafValueC lv -> Some (configLeafValue processChildConfig parseScope allScopes anyScope lv comments)
        |_ -> None

    let processChildConfigRoot (parseScope) (allScopes) (anyScope) ((child, comments) : Child * string list) =
        match child with
        |NodeC n when n.Key == "types" -> None
        |NodeC n -> Some (configRootNode processChildConfig parseScope allScopes anyScope n comments)
        |LeafC l -> Some (configRootLeaf processChildConfig parseScope allScopes anyScope l comments)
        // |LeafValueC lv -> Some (configLeafValue lv comments)
        |_ -> None

    // Types

    let processType (parseScope) (allScopes) (anyScope) (node : Node) (comments : string list) =
        let parseLocalisation ((child : Child), comments : string list) =
            match child with
            |LeafC loc ->
                let required = comments |> List.exists (fun s -> s.Contains "required")
                let optional = comments |> List.exists (fun s -> s.Contains "optional")
                let primary = comments |> List.exists (fun s -> s.Contains "primary")
                let key = loc.Key
                let value = loc.Value.ToRawString()
                match value.IndexOf "$" with
                | -1 ->
                    Some { name = key; prefix = ""; suffix = ""; required = required; optional = optional; replaceScopes = replaceScopes parseScope comments; explicitField = Some value; primary = primary }
                | dollarIndex ->
                    let prefix = value.Substring(0, dollarIndex)
                    let suffix = value.Substring(dollarIndex + 1)
                    Some { name = key; prefix = prefix; suffix = suffix; required = required; optional = optional; replaceScopes = replaceScopes parseScope comments; explicitField = None; primary = primary }
            |_ -> None
        let parseSubTypeLocalisation (subtype : Node) =
            match subtype.Key.StartsWith("subtype[") with
            |true ->
                match getSettingFromString subtype.Key "subtype" with
                |Some st ->
                    let res = getNodeComments subtype |> List.choose parseLocalisation
                    Some (st, res)
                |_ -> None
            |_ -> None
        let parseSubType ((child : Child), comments : string list) =
            match child with
            |NodeC subtype when subtype.Key.StartsWith "subtype" ->
                let typekeyfilter =
                    match comments |> List.tryFind (fun s -> s.Contains "type_key_filter") with
                    |Some c -> Some (c.Substring(c.IndexOf "=" + 1).Trim())
                    |None -> None
                let displayName =
                    match comments |> List.tryFind (fun s -> s.Contains "display_name") with
                    |Some c -> Some (c.Substring(c.IndexOf "=" + 1).Trim())
                    |None -> None
                let abbreviation =
                    match comments |> List.tryFind (fun s -> s.Contains "abbreviation") with
                    |Some c -> Some (c.Substring(c.IndexOf "=" + 1).Trim())
                    |None -> None
                let pushScope =
                    match comments |> List.tryFind (fun s -> s.Contains("push_scope")) with
                    |Some s -> s.Substring(s.IndexOf "=" + 1).Trim() |> parseScope |> Some
                    |None -> None
                let startsWith =
                    match comments |> List.tryFind (fun s -> s.Contains "starts_with") with
                    |Some c -> Some (c.Substring(c.IndexOf "=" + 1).Trim())
                    |None -> None
                let rules = (getNodeComments subtype |> List.choose (processChildConfig parseScope allScopes anyScope))
                match getSettingFromString (subtype.Key) "subtype" with
                |Some key -> Some { name = key; rules = rules; typeKeyField = typekeyfilter; pushScope = pushScope; localisation = []; startsWith = startsWith; displayName = displayName; abbreviation = abbreviation }
                |None -> None
            |_ -> None
        let getSkipRootKey (node : Node) =
            let createSkipRoot (s : string) = if s == "any" then SkipRootKey.AnyKey else SkipRootKey.SpecificKey s
            let skipRootKeyLeaves = node.Leafs "skip_root_key" |> List.ofSeq
            match skipRootKeyLeaves with
            | [x] when x.ValueText = "any" -> [SkipRootKey.AnyKey]
            | [x] -> [SkipRootKey.SpecificKey x.ValueText]
            | x::xs ->
                let shouldMatch = x.Operator = Operator.Equals
                [SkipRootKey.MultipleKeys ( (x::xs) |> List.map (fun y -> y.ValueText), shouldMatch)]
            | [] -> node.Child "skip_root_key" |> Option.map (fun c -> c.LeafValues |> Seq.map (fun lv -> createSkipRoot (lv.Value.ToRawString())))
                                                    |> Option.defaultValue Seq.empty
                                                    |> Seq.toList

            // match node.Has "skip_root_key", node.TagText "skip_root_key" with
            // |_, "any" -> [SkipRootKey.AnyKey]
            // |true, "" -> node.Child "skip_root_key" |> Option.map (fun c -> c.LeafValues |> Seq.map (fun lv -> createSkipRoot (lv.Value.ToRawString())))
            //                                         |> Option.defaultValue Seq.empty
            //                                         |> Seq.toList
            // |true, x -> [SkipRootKey.SpecificKey x]
            // |false, _ -> []
        let validTypeKeys = [|"name_field"; "type_per_file"; "skip_root_key"; "path"; "path_strict"; "path_file"; "starts_with"; "severity"; "unique"; |]
        let checkTypeChildren (child : Child) =
            match child with
            | LeafC leaf ->
                if Array.contains leaf.Key validTypeKeys
                then ()
                else log (sprintf "Unexpected leaf %s found in type definition at %A" leaf.Key leaf.Position)
            | NodeC node ->
                match node.Key with
                | "localisation" -> ()
                | x when x.StartsWith "subtype" -> ()
                | x -> log (sprintf "Unexpected node %s found in type definition at %A" x node.Position)
            | LeafValueC leafvalue -> log (sprintf "Unexpected leafvalue %s found in type definition at %A" leafvalue.Key leafvalue.Position)
            | ValueClauseC vc -> log (sprintf "Unexpected valueclause found in type definition at %A" vc.Position)
            | CommentC _ -> ()
        match node.Key with
        |x when x.StartsWith("type") ->
            node.All |> List.iter checkTypeChildren
            let typename = getSettingFromString node.Key "type"
            let namefield = if node.Has "name_field" then Some (node.TagText "name_field") else None
            let type_per_file = node.TagText "type_per_file" == "yes"
            let pathOptions = getPathOptions node
            let startsWith = if node.Has "starts_with" then Some (node.TagText "starts_with") else None
            let skiprootkey = getSkipRootKey node
            let subtypes = getNodeComments node |> List.choose parseSubType
            let warningOnly = node.TagText "severity" == "warning"
            let unique = node.TagText "unique" == "yes"
            let localisation = node.Child "localisation" |> Option.map (fun l -> getNodeComments l |> List.choose parseLocalisation) |> Option.defaultValue []
            let subtypelocalisations = node.Child "localisation" |> Option.map (fun l -> l.Children |> List.choose parseSubTypeLocalisation) |> Option.defaultValue []
            let subtypes = subtypes |> List.map (fun st -> let loc = subtypelocalisations |> List.filter (fun (stl, _) -> stl = st.name) |> List.collect snd in {st with localisation = loc})
            let typekeyfilter =
                match comments |> List.tryFind (fun s -> s.Contains "type_key_filter") with
                |Some c ->
                    //log "c %A" c
                    let valid = c.Contains "=" || c.Contains "<>"
                    if valid
                    then
                        let negative = c.Contains "<>"
                        let rhs =
                            if negative
                            then c.Substring(c.IndexOf "<>" + 2).Trim()
                            else c.Substring(c.IndexOf "=" + 1).Trim()
                        let values =
                            match rhs.StartsWith("{") && rhs.EndsWith("}") with
                            |true -> rhs.Trim('{','}') |> (fun s -> s.Split([|' '|])) |> List.ofArray
                            |false -> [rhs]
                        Some (values, negative)
                    else None
                |None -> None
            let graphData =
                match comments |> List.tryFind (fun s -> s.Contains "graph_related_types") with
                |Some c ->
                    let valid = c.Contains "="
                    if valid
                    then
                        let rhs = c.Substring(c.IndexOf "=" + 1).Trim()
                        let values =
                            match rhs.StartsWith("{") && rhs.EndsWith("}") with
                            |true -> rhs.Trim('{','}') |> (fun s -> s.Split([|' '|])) |> List.ofArray
                            |false -> [rhs]
                        values
                    else []
                |None -> []
            match typename with
            |Some tn ->
                Some {
                    name = tn;
                    nameField = namefield;
                    type_per_file = type_per_file;
                    pathOptions = pathOptions;
                    conditions = None;
                    subtypes = subtypes;
                    typeKeyFilter = typekeyfilter;
                    skipRootKey = skiprootkey;
                    warningOnly = warningOnly;
                    localisation = localisation;
                    startsWith = startsWith;
                    unique = unique;
                    graphRelatedTypes = graphData
                    }
            |None -> None
        |_ -> None



    let processChildType (parseScope) allScopes (anyScope) ((child, comments) : Child * string list) =
        match child with
        | NodeC n when n.Key == "types" ->
            let inner ((child2, comments2) : Child * string list) =
                match child2 with
                |NodeC n2 -> (processType parseScope allScopes anyScope n2 comments2)
                |_ -> None
            Some (getNodeComments n |> List.choose inner)
        |_ -> None

    let processEnum (node : Node) (comments : string list) =
        match node.Key with
        | x when x.StartsWith("enum") ->
            let enumname = getSettingFromString node.Key "enum"
            let values = node.LeafValues |> List.ofSeq |> List.map (fun lv -> lv.Value.ToString().Trim([|'\"'|]))
            match enumname with
            | Some en ->
                let description =
                    match comments |> List.tryFind (fun s -> s.StartsWith "##") with
                    | Some d -> (d.Trim('#'))
                    | None -> en
                Some ({key = en; values = values; description = description})
            | None -> None
        | _ -> None

    let processChildEnum ((child, comments) : Child * string list) =
        match child with
        | NodeC n when n.Key == "enums" ->
            let inner ((child2, comments2) : Child * string list) =
                match child2 with
                | NodeC n2 -> (processEnum n2 comments2)
                | _ -> None
            Some (getNodeComments n |> List.choose inner)
        | _ -> None

    let processComplexEnum (node : Node) (comments : string list) =
        match node.Key with
        | x when x.StartsWith("complex_enum") ->
            let enumname = getSettingFromString node.Key "complex_enum"
            let pathOptions = getPathOptions node
            let nametree = node.Child "name"
            let start_from_root = node.TagText "start_from_root" == "yes"
            match (enumname, nametree) with
            | Some en, Some nt ->
                let description =
                    match comments |> List.tryFind (fun s -> s.StartsWith "##") with
                    | Some d -> (d.Trim('#'))
                    | None -> en
                Some {name = en; pathOptions = pathOptions; nameTree = nt; start_from_root = start_from_root; description = description}
            | _ -> None
        | _ -> None

    let processComplexChildEnum ((child, comments) : Child * string list) =
        match child with
        |NodeC n when n.Key == "enums" ->
            let inner ((child2, comments2) : Child * string list) =
                match child2 with
                |NodeC n2 -> (processComplexEnum n2 comments2)
                |_ -> None
            Some (getNodeComments n |> List.choose inner)
        |_ -> None


    let processValue (node : Node) (comments : string list) =
        match node.Key with
        |x when x.StartsWith("value") ->
            let enumname = getSettingFromString node.Key "value"
            let values = node.LeafValues |> List.ofSeq |> List.map (fun lv -> lv.Value.ToString().Trim([|'\"'|]))
            match enumname with
            |Some en -> Some (en, values)
            |None -> None
        |_ -> None

    let processChildValue ((child, comments) : Child * string list) =
        match child with
        |NodeC n when n.Key == "values" ->
            let inner ((child2, comments2) : Child * string list) =
                match child2 with
                |NodeC n2 -> (processValue n2 comments2)
                |_ -> None
            Some (getNodeComments n |> List.choose inner)
        |_ -> None



    let replaceSingleAliases (rules : RootRule list) =
        let mutable singlealiases = rules |> List.choose (function |SingleAliasRule (name, inner) -> Some (SingleAliasRule (name, inner)) |_ -> None) //|> Map.ofList
        let singlealiasesmap() = singlealiases |> List.choose (function |SingleAliasRule (name, inner) -> Some (name, inner) |_ -> None) |> Map.ofList

        let rec cataRule rule : NewRule =
            match rule with
            | (NodeRule (l, r), o) -> (NodeRule (l, r |> List.map cataRule), o)
            | (ValueClauseRule (r), o) -> (ValueClauseRule (r |> List.map cataRule), o)
            | (SubtypeRule (a, b, i), o) -> (SubtypeRule(a, b, (i |> List.map cataRule)), o)
            | (LeafRule (l, SingleAliasField name), o) ->
                match singlealiasesmap() |> Map.tryFind name with
                | Some (LeafRule (al, ar), ao) ->
                    // log (sprintf "Replaced single alias leaf %A %s with leaf %A" (l |> function |ValueField (Specific x) -> StringResource.stringManager.GetStringForIDs x |_ -> "") name (al |> function |ValueField (Specific x) -> StringResource.stringManager.GetStringForIDs x |_ -> ""))
                    LeafRule (l, ar), o
                | Some (NodeRule (al, ar), ao) ->
                    // log (sprintf "Replaced single alias leaf %A %s with node %A" (l |> function |ValueField (Specific x) -> StringResource.stringManager.GetStringForIDs x |_ -> "") name (al |> function |ValueField (Specific x) -> StringResource.stringManager.GetStringForIDs x |_ -> ""))
                    NodeRule (l, ar), o
                | x ->
                    log (sprintf "Failed to find defined single alias %s when replacing single alias leaf %A. Found %A" name (l |> function |SpecificField (SpecificValue x) -> StringResource.stringManager.GetStringForIDs x |_ -> "") x)
                    rule
            /// TODO: Add clause key validation
            | (LeafValueRule (SingleAliasClauseField (clauseKey, name)), o) ->
                match singlealiasesmap() |> Map.tryFind name with
                | Some (NodeRule (al, ar), ao) ->
                    ValueClauseRule(ar), o
                | x ->
                    log (sprintf "Failed to find defined single alias %s when replacing single alias clause. Found %A" name x)
                    rule

            | _ -> rule
        let singlealiasesmapper =
            function
            | SingleAliasRule (name, rule) -> SingleAliasRule(name, cataRule rule)
            | x -> x
        let mutable final = singlealiases
        let mutable i = 0
        let mutable first = true
        let ff() =
            i <- i + 1
            let before = final
            final <- final |> List.map singlealiasesmapper
            singlealiases <- final
            first <- false
            before = final || i > 10
        while (not (ff())) do ()

        let rulesMapper =
            function
            | TypeRule (name, rule) -> TypeRule (name, cataRule rule)
            | AliasRule (name, rule) -> AliasRule (name, cataRule rule)
            | SingleAliasRule (name, rule) -> SingleAliasRule(name, cataRule rule)
        rules |> List.map rulesMapper


    let replaceColourField (rules : RootRule list) =

        let rec cataRule rule : NewRule list =
            match rule with
            | LeafRule (l, MarkerField (ColourField)), o  ->
                [
                    NodeRule((l), [LeafValueRule(ValueField(ValueType.Float(-256.0M, 256.0M))), { defaultOptions with min = 3; max = 3 } ]), o
                ]
            | LeafRule (l, MarkerField (IRCountryTag)), o  ->
                [
                    LeafRule(l, ValueField(ValueType.Enum "country_tags")), o
                    LeafRule(l, VariableGetField "dynamic_country_tag"), o
                ]
            | LeafRule (MarkerField (IRCountryTag), r), o  ->
                [
                    LeafRule(ValueField(ValueType.Enum "country_tags"), r), o
                    LeafRule(VariableGetField "dynamic_country_tag", r), o
                ]
            | NodeRule (MarkerField (IRCountryTag), r), o ->
                [
                    NodeRule(ValueField(ValueType.Enum "country_tags"), r |> List.collect cataRule), o
                    NodeRule(VariableGetField "dynamic_country_tag", r |> List.collect cataRule), o
                ]
            | NodeRule (l, r), o ->
                [NodeRule(l, r |> List.collect cataRule), o]
            | ValueClauseRule (r), o -> [ValueClauseRule (r |> List.collect cataRule), o]
            | (SubtypeRule (a, b, i), o) -> [(SubtypeRule(a, b, (i |> List.collect cataRule)), o)]
            | _ -> [rule]
        let rulesMapper =
            function
            | TypeRule (name, rule) -> cataRule rule |> List.map (fun x -> TypeRule (name, x))
            | AliasRule (name, rule) -> cataRule rule |> List.map (fun x ->  AliasRule (name, x))
            | SingleAliasRule (name, rule) -> cataRule rule |> List.map (fun x ->  SingleAliasRule(name, x))
        rules |> List.collect rulesMapper

    let replaceValueMarkerFields (rules : RootRule list) =
        let rec cataRule rule : NewRule list =
            match rule with
            | LeafRule (ValueScopeMarkerField (i,m), ValueScopeMarkerField (i2,m2)), o when not o.comparison ->
                [
                    LeafRule(ValueScopeField(i, m), ValueScopeField(i2, m2)), o
                    LeafRule(ValueScopeField(i, m), SingleAliasField("formula")), o
                    LeafRule(ValueScopeField(i, m), SingleAliasField("range")), o
                ]
            | LeafRule (ValueScopeMarkerField (i,m), ValueScopeMarkerField (i2,m2)), o when o.comparison ->
                [
                    LeafRule(ValueScopeField(i, m), ValueScopeField(i2, m2)), o
                ]
            | LeafRule (l, ValueScopeMarkerField (i2,m2)), o when not o.comparison ->
                [
                    LeafRule(l, ValueScopeField(i2, m2)), o
                    LeafRule(l, SingleAliasField("formula")), o
                    LeafRule(l, SingleAliasField("range")), o
                ]
            | LeafRule (l, ValueScopeMarkerField (i2,m2)), o when o.comparison ->
                [
                    LeafRule(l, ValueScopeField(i2, m2)), o
                ]
            | LeafRule (ValueScopeMarkerField (i,m), r), o ->
                [LeafRule(ValueScopeField(i, m), r), o]
            | NodeRule (ValueScopeMarkerField (i,m), r), o ->
                [NodeRule(ValueScopeField(i, m), r |> List.collect cataRule), o]
            | NodeRule (l, r), o ->
                [NodeRule(l, r |> List.collect cataRule), o]
            | ValueClauseRule (r), o -> [ValueClauseRule (r |> List.collect cataRule), o]
            | (SubtypeRule (a, b, i), o) -> [(SubtypeRule(a, b, (i |> List.collect cataRule)), o)]
            | _ -> [rule]
        let rulesMapper =
            function
            | TypeRule (name, rule) -> cataRule rule |> List.map (fun x -> TypeRule (name, x))
            | AliasRule (name, rule) -> cataRule rule |> List.map (fun x ->  AliasRule (name, x))
            | SingleAliasRule (name, rule) -> cataRule rule |> List.map (fun x ->  SingleAliasRule(name, x))
        rules |> List.collect rulesMapper

    let replaceIgnoreMarkerFields (rules : RootRule list) =
        let rec cataRule rule : NewRule list =
            match rule with
            | LeafRule (field, IgnoreMarkerField), o ->
               [ NodeRule (IgnoreField field, []), o ]
            | NodeRule (l, r), o ->
                [NodeRule(l, r |> List.collect cataRule), o]
            | ValueClauseRule (r), o -> [ValueClauseRule (r |> List.collect cataRule), o]
            | (SubtypeRule (a, b, i), o) -> [(SubtypeRule(a, b, (i |> List.collect cataRule)), o)]
            | _ -> [rule]
        let rulesMapper =
            function
            | TypeRule (name, rule) -> cataRule rule |> List.map (fun x -> TypeRule (name, x))
            | AliasRule (name, rule) -> cataRule rule |> List.map (fun x ->  AliasRule (name, x))
            | SingleAliasRule (name, rule) -> cataRule rule |> List.map (fun x ->  SingleAliasRule(name, x))
        rules |> List.collect rulesMapper

    let processConfig (parseScope) (allScopes) (anyScope) (node : Node) =
        let nodes = getNodeComments node
        let rules = nodes |> List.choose (processChildConfigRoot parseScope allScopes anyScope)
        let types = nodes |> List.choose (processChildType parseScope allScopes anyScope) |> List.collect id
        let enums = nodes |> List.choose processChildEnum |> List.collect id
        let complexenums = nodes |> List.choose processComplexChildEnum |> List.collect id
        let values = nodes |> List.choose processChildValue |> List.collect id
        rules, types, enums, complexenums, values

    let parseConfig (parseScope) (allScopes) (anyScope) filename fileString =
        //log "parse"
        let parsed = CKParser.parseString fileString filename
        match parsed with
        |Failure(e, _, _) -> log (sprintf "config file %s failed with %s" filename e); ([], [], [], [], [])
        |Success(s,_,_) ->
            //log "parsed %A" s
            let root = simpleProcess.ProcessNode() "root" (mkZeroFile filename) (s)
            //log "processConfig"
            processConfig parseScope allScopes anyScope root
    let parseConfigs (parseScope) (allScopes) (anyScope) (files : (string * string) list)  =
        let rules, types, enums, complexenums, values =
            files |> List.map (fun (filename, fileString) -> parseConfig parseScope allScopes anyScope filename fileString)
              |> List.fold (fun (rs, ts, es, ces, vs) (r, t, e, ce, v) -> r@rs, t@ts, e@es, ce@ces, v@vs) ([], [], [], [], [])
        let rules = rules |> replaceValueMarkerFields |> replaceSingleAliases |> replaceColourField |> replaceIgnoreMarkerFields
        // File.AppendAllText ("test.test", sprintf "%O" rules)
        rules, types, enums, complexenums, values


