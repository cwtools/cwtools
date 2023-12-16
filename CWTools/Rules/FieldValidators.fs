namespace CWTools.Rules

open CWTools.Common
open CWTools.Process.Scopes
open CWTools.Utilities.Utils2
open Microsoft.FSharp.Collections.Tagged
open CWTools.Utilities
open CWTools.Utilities.Utils
open CWTools.Utilities.StringResource
open System.IO
open CWTools.Process.Localisation

type RuleContext =
    { subtypes: string list
      scopes: ScopeContext
      warningOnly: bool }

type CheckFieldParams =
    { varMap: Collections.Map<string, StringSet>
      enumsMap: Collections.Map<string, string * StringSet>
      typesMap: Collections.Map<string, StringSet>
      linkMap: EffectMap 
      wildcardLinks: ScopedEffect list
      valueTriggerMap: EffectMap
      varSet: StringSet
      localisation: (Lang * Collections.Set<string>) list
      defaultLocalisation: Collections.Set<string>
      files: Collections.Set<string>
      changeScope: ChangeScope
      anyScope: Scope
      defaultLang: Lang
      aliasKeyList: Collections.Map<string, Collections.Set<StringToken>>
      processLocalisation:
          Lang * Collections.Map<string, CWTools.Localisation.Entry> -> Lang * Collections.Map<string, LocEntry>
      validateLocalisation: LocEntry -> ScopeContext -> CWTools.Validation.ValidationResult }

[<RequireQualifiedAccess>]
module internal FieldValidators =

    open System
    open CWTools.Process
    open CWTools.Validation
    open CWTools.Validation.ValidationCore
    open CWTools.Rules



    let checkPathDir (pathOptions: PathOptions) (pathDir: string) (file: string) =
        match pathOptions.pathStrict with
        | true -> pathOptions.paths |> List.exists (fun tp -> pathDir == tp.Replace("\\", "/"))
        | false ->
            pathOptions.paths
            |> List.exists (fun tp -> pathDir.StartsWith(tp.Replace("\\", "/"), StringComparison.OrdinalIgnoreCase))
        && match pathOptions.pathFile with
           | Some f -> file == f
           | None -> true
        && match pathOptions.pathExtension with
           | Some ext -> Path.GetExtension file == ext
           | None -> true

    let getValidValues =
        function
        | ValueType.Bool -> Some [ "yes"; "no" ]
        | ValueType.Enum es -> Some [ es ]
        | _ -> None

    let checkFileExists (files: Collections.Set<string>) (leaf: Leaf) =
        let file =
            leaf.ValueText
                .Trim('"')
                .Replace("\\", "/")
                .Replace(".lua", ".shader")
                .Replace(".tga", ".dds")

        if files.Contains file then
            OK
        else
            Invalid(Guid.NewGuid(), [ inv (ErrorCodes.MissingFile file) leaf ])

    let checkIconExists (files: Collections.Set<string>) (folder: string) (leaf: Leaf) =
        let value = folder + "/" + leaf.ValueText + ".dds"

        if files.Contains value then
            OK
        else
            Invalid(Guid.NewGuid(), [ inv (ErrorCodes.MissingFile value) leaf ])



    // type ScopeContext = IScopeContext<Scope>

    // type RuleContext  = RuleContext<Scope>
    let firstCharEqualsAmp (key: StringToken) =
        (stringManager.GetMetadataForID key).startsWithAmp

    let getStringMetadata (key: StringToken) = (stringManager.GetMetadataForID key)
    // let firstCharEqualsAmp (s : string) = s.Length > 0 && (s.[0] = '@')// || s.[0] = '$')
    let quoteArray = [| '\"' |]
    let ampArray = [| '@' |]
    let trimQuote (s: string) = s.Trim(quoteArray)
    let getLowerKey (ids: StringTokens) = stringManager.GetLowerStringForIDs(ids)
    let getOriginalKey (ids: StringTokens) = stringManager.GetStringForIDs ids

    let checkValidValue
        (varMap: Collections.Map<_, StringSet>)
        (enumsMap: Collections.Map<_, string * StringSet>)
        (keys: (Lang * Collections.Set<string>) list)
        (severity: Severity)
        (vt: CWTools.Rules.ValueType)
        (ids: StringTokens)
        leafornode
        errors
        =
        let key = getLowerKey ids

        if firstCharEqualsAmp ids.lower then
            errors
        else
            match vt with
            // | ValueType.Scalar ->
            //     errors
            | ValueType.Bool ->
                if key == "yes" || key == "no" then
                    errors
                else
                    inv
                        (ErrorCodes.ConfigRulesUnexpectedValue $"Expecting yes or no, got %s{key}" severity)
                        leafornode
                    <&&&> errors
            | ValueType.Int(min, max) ->
                match TryParser.parseIntWithDecimal key with
                | Some i ->
                    if i <= max && i >= min then
                        errors
                    else
                        inv
                            (ErrorCodes.ConfigRulesUnexpectedValue
                                $"Expecting a value between %i{min} and %i{max}"
                                severity)
                            leafornode
                        <&&&> errors
                | None ->
                    match enumsMap.TryFind "static_values" with
                    | Some(_, es) ->
                        if es.ContainsKey(trimQuote key) then
                            errors
                        else
                            inv
                                (ErrorCodes.ConfigRulesUnexpectedValue
                                    $"Expecting an integer, got %s{key}"
                                    severity)
                                leafornode
                            <&&&> errors
                    | None ->
                        inv
                            (ErrorCodes.ConfigRulesUnexpectedValue $"Expecting an integer, got %s{key}" severity)
                            leafornode
                        <&&&> errors
            | ValueType.Float(min, max) ->
                match TryParser.parseDecimal key with
                | Some f ->
                    if f <= max && f >= min then
                        errors
                    else
                        inv
                            (ErrorCodes.ConfigRulesUnexpectedValue
                                $"Expecting a value between %f{min} and %f{max}"
                                severity)
                            leafornode
                        <&&&> errors
                | None ->
                    match enumsMap.TryFind "static_values" with
                    | Some(_, es) ->
                        if es.ContainsKey(trimQuote key) then
                            errors
                        else
                            inv
                                (ErrorCodes.ConfigRulesUnexpectedValue
                                    $"Expecting a float, got %s{key}"
                                    severity)
                                leafornode
                            <&&&> errors
                    | None ->
                        inv
                            (ErrorCodes.ConfigRulesUnexpectedValue $"Expecting a float, got %s{key}" severity)
                            leafornode
                        <&&&> errors
            | ValueType.Enum e ->
                match enumsMap.TryFind e with
                | Some(desc, es) ->
                    if es.ContainsKey(trimQuote key) then
                        errors
                    else
                        inv
                            (ErrorCodes.ConfigRulesUnexpectedValue
                                $"Expecting a \"%s{desc}\" value, e.g. %A{es.StringValues |> Seq.head}"
                                severity)
                            leafornode
                        <&&&> errors
                | None ->
                    inv
                        (ErrorCodes.RulesError
                            $"Configuration error: there are no defined values for the enum %s{e}"
                            severity)
                        leafornode
                    <&&&> errors
            // | ValueType.Specific s ->
            //     // if trimQuote key == s then OK else Invalid (Guid.NewGuid(), [inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting value %s" s) severity) leafornode])
            //     if id = s.lower then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expecting value %s" (StringResource.stringManager.GetStringForID(s.normal))) severity) leafornode <&&&> errors
            | ValueType.Percent ->
                if key.EndsWith("%") then
                    errors
                else
                    inv
                        (ErrorCodes.ConfigRulesUnexpectedValue $"Expecting an percentage, got %s{key}" severity)
                        leafornode
                    <&&&> errors
            | ValueType.Date ->
                let parts = key.Split([| '.' |])

                let ok =
                    (parts.Length = 3)
                    && parts.[0].Length <= 4
                    && Int32.TryParse(parts.[0]) |> fst
                    && Int32.TryParse(parts.[1]) |> fst
                    && Int32.Parse(parts.[1]) <= 12
                    && Int32.TryParse(parts.[2]) |> fst
                    && Int32.Parse(parts.[2]) <= 31

                if ok then
                    errors
                else
                    inv
                        (ErrorCodes.ConfigRulesUnexpectedValue $"Expecting a date, got %s{key}" severity)
                        leafornode
                    <&&&> errors
            | ValueType.DateTime ->
                let parts = key.Split([| '.' |])

                let ok =
                    match parts.Length with
                    | 3 ->
                        (parts.Length = 3)
                        && parts.[0].Length <= 4
                        && Int32.TryParse(parts.[0]) |> fst
                        && Int32.TryParse(parts.[1]) |> fst
                        && Int32.Parse(parts.[1]) <= 12
                        && Int32.TryParse(parts.[2]) |> fst
                        && Int32.Parse(parts.[2]) <= 31
                    | 4 ->
                        (parts.Length = 4)
                        && parts.[0].Length <= 4
                        && Int32.TryParse(parts.[0]) |> fst
                        && Int32.TryParse(parts.[1]) |> fst
                        && Int32.Parse(parts.[1]) <= 12
                        && Int32.TryParse(parts.[2]) |> fst
                        && Int32.Parse(parts.[2]) <= 31
                        && Int32.TryParse(parts.[3]) |> fst
                        && Int32.Parse(parts.[3]) <= 24
                    | _ -> false

                if ok then
                    errors
                else
                    inv
                        (ErrorCodes.ConfigRulesUnexpectedValue $"Expecting a date, got %s{key}" severity)
                        leafornode
                    <&&&> errors
            | ValueType.CK2DNA ->
                if key.Length = 11 && key |> Seq.forall Char.IsLetter then
                    errors
                else
                    inv
                        (ErrorCodes.ConfigRulesUnexpectedValue $"Expecting a dna value, got %s{key}" severity)
                        leafornode
                    <&&&> errors
            | ValueType.CK2DNAProperty ->
                if key.Length <= 39 && key |> Seq.forall (fun c -> Char.IsLetter c || c = '0') then
                    errors
                else
                    inv
                        (ErrorCodes.ConfigRulesUnexpectedValue
                            $"Expecting a portrait properties value, got %s{key}"
                            severity)
                        leafornode
                    <&&&> errors
            | ValueType.IRFamilyName ->
                let parts = key.Split([| '.' |])

                if (parts.Length <> 4) then
                    inv
                        (ErrorCodes.ConfigRulesUnexpectedValue
                            $"Expecting a family names value, got %s{key}"
                            severity)
                        leafornode
                    <&&&> errors
                else
                    (CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeN
                        keys
                        ids
                        parts.[0]
                        leafornode
                        errors)
                    |> (CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeN keys ids parts.[1] leafornode)
                    |> (CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeN keys ids parts.[2] leafornode)
                    |> (CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeN keys ids parts.[3] leafornode)
            | ValueType.STLNameFormat var ->
                match varMap.TryFind var with
                | Some vars ->
                    let refs =
                        System.Text.RegularExpressions.Regex.Matches(key, @"<([^>]*)>")
                        |> Seq.cast<System.Text.RegularExpressions.Match>

                    let refs =
                        refs
                        |> Seq.map (fun m -> m.Groups.[1])
                        |> Seq.cast<Text.RegularExpressions.Capture>
                        |> Seq.map (fun c -> c.Value)

                    let res = refs |> Seq.exists (vars.ContainsKey >> not)

                    if res then
                        inv
                            (ErrorCodes.CustomError $"Expecting a defined parts list of %s{var}" Severity.Error)
                            leafornode
                        <&&&> errors
                    else
                        OK
                | None -> errors


    let checkValidValueNE
        (varMap: Collections.Map<_, StringSet>)
        (enumsMap: Collections.Map<_, string * StringSet>)
        (keys: (Lang * Collections.Set<string>) list)
        (severity: Severity)
        (vt: CWTools.Rules.ValueType)
        (ids: StringTokens)
        =
        // if key |> firstCharEqualsAmp then true else
        let key = getLowerKey ids

        (match vt with
         // | ValueType.Scalar ->
         //     true
         | ValueType.Bool -> key == "yes" || key == "no"
         | ValueType.Int(min, max) ->
             match TryParser.parseIntWithDecimal key with
             | Some i -> i <= max && i >= min
             | None ->
                 match enumsMap.TryFind "static_values" with
                 | Some(_, es) -> es.ContainsKey(trimQuote key)
                 | None -> false
         | ValueType.Float(min, max) ->
             match TryParser.parseDecimal key with
             | Some f -> f <= max && f >= min
             | None ->
                 match enumsMap.TryFind "static_values" with
                 | Some(_, es) -> es.ContainsKey(trimQuote key)
                 | None -> false
         | ValueType.Enum e ->
             match enumsMap.TryFind e with
             | Some(_, es) -> es.ContainsKey(trimQuote key)
             | None -> false
         // | ValueType.Specific s ->
         //     // if trimQuote key == s then true else false
         //     id = s.lower
         | ValueType.Percent -> key.EndsWith("%")
         | ValueType.Date ->
             let parts = key.Split([| '.' |])

             (parts.Length = 3)
             && parts.[0].Length <= 4
             && Int32.TryParse(parts.[0]) |> fst
             && Int32.TryParse(parts.[1]) |> fst
             && Int32.Parse(parts.[1]) <= 12
             && Int32.TryParse(parts.[2]) |> fst
             && Int32.Parse(parts.[2]) <= 31
         | ValueType.DateTime ->
             let parts = key.Split([| '.' |])

             match parts.Length with
             | 3 ->
                 (parts.Length = 3)
                 && parts.[0].Length <= 4
                 && Int32.TryParse(parts.[0]) |> fst
                 && Int32.TryParse(parts.[1]) |> fst
                 && Int32.Parse(parts.[1]) <= 12
                 && Int32.TryParse(parts.[2]) |> fst
                 && Int32.Parse(parts.[2]) <= 31
             | 4 ->
                 (parts.Length = 4)
                 && parts.[0].Length <= 4
                 && Int32.TryParse(parts.[0]) |> fst
                 && Int32.TryParse(parts.[1]) |> fst
                 && Int32.Parse(parts.[1]) <= 12
                 && Int32.TryParse(parts.[2]) |> fst
                 && Int32.Parse(parts.[2]) <= 31
                 && Int32.TryParse(parts.[3]) |> fst
                 && Int32.Parse(parts.[3]) <= 24
             | _ -> false

         | ValueType.CK2DNA -> key.Length = 11 && key |> Seq.forall Char.IsLetter
         | ValueType.CK2DNAProperty -> key.Length <= 39 && key |> Seq.forall (fun c -> Char.IsLetter c || c = '0')
         | ValueType.IRFamilyName ->
             let parts = key.Split([| '.' |])

             (parts.Length = 4)
             && CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeNE keys ids parts.[0]
             && CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeNE keys ids parts.[1]
             && CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeNE keys ids parts.[2]
             && CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeNE keys ids parts.[3]
         | ValueType.STLNameFormat var ->
             match varMap.TryFind var with
             | Some vars ->
                 let refs =
                     System.Text.RegularExpressions.Regex.Matches(key, @"<([^>]*)>")
                     |> Seq.cast<System.Text.RegularExpressions.Match>

                 let res =
                     refs
                     |> Seq.map (fun m -> m.Groups.[1])
                     |> Seq.cast<Text.RegularExpressions.Capture>
                     |> Seq.map (fun c -> c.Value)
                     |> Seq.exists (vars.ContainsKey >> not)

                 res |> not
             | None -> false

        )
        || firstCharEqualsAmp ids.lower

    let checkLocalisationField
        (processLocalisation:
            Lang * Collections.Map<string, CWTools.Localisation.Entry> -> Lang * Collections.Map<string, LocEntry>)
        (validateLocalisation: LocEntry -> ScopeContext -> CWTools.Validation.ValidationResult)
        scopeContext
        (keys: (Lang * Collections.Set<string>) list)
        (defaultKeys: Collections.Set<string>)
        defaultLang
        (synced: bool)
        (isInline: bool)
        (ids: StringTokens)
        (leafornode: IKeyPos)
        errors
        =
        let key = trimQuote (getOriginalKey ids)

        match synced, isInline with
        | true, false ->
            // let defaultKeys = keys |> List.choose (fun (l, ks) -> if l = defaultLang then Some ks else None) |> List.tryHead |> Option.defaultValue Set.empty
            //let key = leaf.Value |> (function |QString s -> s |s -> s.ToString())
            CWTools.Validation.LocalisationValidation.checkLocNameN leafornode defaultKeys defaultLang ids key errors
        | false, true ->
            CWTools.Validation.LocalisationValidation.checkLocKeysInlineLeafOrNodeN keys ids key leafornode errors
        | false, false ->
            if key.Contains("[") then
                let entry =
                    { CWTools.Localisation.Entry.key = "inline"
                      CWTools.Localisation.Entry.value = None
                      CWTools.Localisation.Entry.desc = key
                      CWTools.Localisation.Entry.position = leafornode.Position }

                let proc =
                    processLocalisation (defaultLang, Collections.Map.ofList [ "inline", entry ])
                    |> snd
                    |> Map.toList
                    |> List.head
                    |> snd

                validateLocalisation proc scopeContext
            else
                CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeN keys ids key leafornode errors
        | _ -> errors

    let checkLocalisationFieldNE
        (processLocalisation:
            Lang * Collections.Map<string, CWTools.Localisation.Entry> -> Lang * Collections.Map<string, LocEntry>)
        (validateLocalisation: LocEntry -> ScopeContext -> CWTools.Validation.ValidationResult)
        (keys: (Lang * Collections.Set<string>) list)
        (defaultKeys: Collections.Set<string>)
        defaultLang
        (synced: bool)
        (isInline: bool)
        (ids: StringTokens)
        =
        let key = trimQuote (getOriginalKey ids)

        match synced, isInline with
        | true, false ->
            // let defaultKeys = keys |> List.choose (fun (l, ks) -> if l = defaultLang then Some ks else None) |> List.tryHead |> Option.defaultValue Set.empty
            //let key = leaf.Value |> (function |QString s -> s |s -> s.ToString())
            CWTools.Validation.LocalisationValidation.checkLocNameNE defaultKeys defaultLang ids key
        | false, true -> CWTools.Validation.LocalisationValidation.checkLocKeysInlineLeafOrNodeNE keys ids key
        | false, false -> CWTools.Validation.LocalisationValidation.checkLocKeysLeafOrNodeNE keys ids key
        | _ -> false

    let memoize keyFunction memFunction =
        let dict = new System.Collections.Generic.Dictionary<_, _>()

        fun n ->
            match dict.TryGetValue(keyFunction (n)) with
            | true, v -> v
            | _ ->
                let temp = memFunction (n)
                dict.Add(keyFunction (n), temp)
                temp
    // let memoizedComplexTypes (typetype : TypeType) (values : StringSet)
    let checkTypeField
        (typesMap: Collections.Map<_, StringSet>)
        severity
        (typetype: TypeType)
        (ids: StringTokens)
        leafornode
        errors
        =
        let isComplex, fieldType =
            match typetype with
            | TypeType.Simple t -> false, t
            | Complex(_, t, _) -> true, t

        let typeKeyMap v =
            match typetype with
            | TypeType.Simple t -> v
            | Complex(p, _, s) -> p + v + s

        let key = getLowerKey ids

        match typesMap.TryFind fieldType with
        | Some values ->
            let value = trimQuote key

            if firstCharEqualsAmp ids.lower then
                errors
            else
                // let values = if isComplex then values.ToList() |> List.map typeKeyMap |> (fun ts -> StringSet.Create(InsensitiveStringComparer(), ts)) else values
                // let values = if isComplex then values.ToList() |> List.map typeKeyMap |> (fun ts -> StringSet.Create(InsensitiveStringComparer(), ts)) else values
                let found =
                    let newvalue =
                        match typetype with
                        | TypeType.Simple t -> Some value
                        | Complex(p, _, s) ->
                            // match value.IndexOf(p, StringComparison.OrdinalIgnoreCase), value.LastIndexOf(s, StringComparison.OrdinalIgnoreCase) with
                            // | -1, -1 ->
                            //     None
                            // | fi, -1 ->
                            //     Some (value.Substring(p.Length))
                            // | -1, si ->
                            //     Some (value.Substring(0, si))
                            // | fi, si ->
                            //     Some (value.Substring(p.Length, (si - p.Length)))
                            match
                                value.StartsWith(p, StringComparison.OrdinalIgnoreCase),
                                value.EndsWith(s, StringComparison.OrdinalIgnoreCase),
                                (value.Length - p.Length - s.Length)
                            with
                            | _, false, _ -> None
                            | false, _, _ -> None
                            | _, _, n when n <= 0 -> None
                            | true, true, n -> Some(value.Substring(p.Length, n))
                    // eprintfn "ct %s %A %A" value newvalue typetype
                    newvalue |> Option.map values.ContainsKey |> Option.defaultValue false

                if found then
                    errors
                else
                    inv
                        (ErrorCodes.ConfigRulesUnexpectedValue $"Expected value of type %s{fieldType}" severity)
                        leafornode
                    <&&&> errors

        //let values = values typeKeyMap values
        // if values.Contains value then errors else inv (ErrorCodes.ConfigRulesUnexpectedValue (sprintf "Expected value of type %s" fieldType) severity) leafornode <&&&> errors
        | None ->
            inv (ErrorCodes.CustomError $"Unknown type referenced %s{fieldType}" Severity.Error) leafornode
            <&&&> errors

    let checkTypeFieldNE (typesMap: Collections.Map<_, StringSet>) severity (typetype: TypeType) (ids: StringTokens) =
        let isComplex, fieldType =
            match typetype with
            | TypeType.Simple t -> false, t
            | Complex(_, t, _) -> true, t

        let typeKeyMap v =
            match typetype with
            | TypeType.Simple t -> v
            | Complex(p, _, s) -> p + v + s

        let key = getLowerKey ids

        match typesMap.TryFind fieldType with
        | Some values ->
            let value = trimQuote key

            if firstCharEqualsAmp ids.lower then
                true
            else
                let value =
                    match typetype with
                    | TypeType.Simple t -> Some value
                    | Complex(p, _, s) ->
                        // match value.IndexOf(p, StringComparison.OrdinalIgnoreCase), value.LastIndexOf(s, StringComparison.OrdinalIgnoreCase) with
                        // | -1, -1 ->
                        //     None
                        // | fi, -1 ->
                        //     Some (value.Substring(p.Length))
                        // | -1, si ->
                        //     Some (value.Substring(0, si))
                        // | fi, si ->
                        //     Some (value.Substring(p.Length, (si - p.Length)))
                        match
                            value.StartsWith(p, StringComparison.OrdinalIgnoreCase),
                            value.EndsWith(s, StringComparison.OrdinalIgnoreCase),
                            (value.Length - p.Length - s.Length)
                        with
                        | _, false, _ -> None
                        | false, _, _ -> None
                        | _, _, n when n <= 0 -> None
                        | true, true, n -> Some(value.Substring(p.Length, n))

                value |> Option.map values.ContainsKey |> Option.defaultValue false
        // let values = if isComplex then values.ToList() |> List.map typeKeyMap |> (fun ts -> StringSet.Create(InsensitiveStringComparer(), ts)) else values
        // match isComplex with
        // | true ->
        //     values.ToList() |> List.map typeKeyMap |> List.exists ((==) value)
        // | false ->
        //     values.Contains value
        // let values = if isComplex then values.ToList() |> List.map typeKeyMap |> (fun ts -> StringSet.Create(InsensitiveStringComparer(), ts)) else values

        //let values = values typeKeyMap values
        | None -> false

    let checkVariableGetField
        (varMap: Collections.Map<_, StringSet>)
        severity
        (varName: string)
        (ids: StringTokens)
        leafornode
        errors
        =
        let key = getLowerKey ids

        match varMap.TryFind varName with
        | Some values ->
            let value = trimQuote key

            if firstCharEqualsAmp ids.lower then
                errors
            else if values.ContainsKey value then
                errors
            else if value.Contains("@") && values.ContainsKey(value.Split([| '@' |]).[0]) then
                errors
            else if
                (let result = values.FindPredecessor(value) in result <> null)
            then
                errors
            else
                inv
                    (ErrorCodes.ConfigRulesUnexpectedValue
                        $"Expected defined value of %s{varName}, got %s{value}"
                        (min Severity.Warning severity))
                    leafornode
                <&&&> errors
        | None ->
            inv
                (ErrorCodes.ConfigRulesUnexpectedValue
                    $"Expected defined value of %s{varName}, got %s{key}"
                    (min Severity.Warning severity))
                leafornode
            <&&&> errors

    let checkVariableGetFieldNE (varMap: Collections.Map<_, StringSet>) severity (varName: string) (ids: StringTokens) =
        let key = getLowerKey ids

        match varMap.TryFind varName with
        | Some values ->
            let value = trimQuote key

            if firstCharEqualsAmp ids.lower then
                true
            else
                values.ContainsKey value
                || (value.Contains("@") && values.ContainsKey(value.Split([| '@' |]).[0]))
                || (values.FindSuccessor(value) <> null)
        | None -> false
        // var:asd
    // var:asdasd
    
    // var -> var.StartsWith (var:asd)
    // var:asd -> var:asdasd.StartsWith(var:asd)

    let checkFilepathField
        (files: Collections.Set<string>)
        (ids: StringTokens)
        (prefix: string option)
        (extension: string option)
        leafornode
        errors
        =
        let key = getOriginalKey ids
        let file = (trimQuote key).Replace("\\", "/")
        let file2 = file.Replace(".lua", ".shader").Replace(".tga", ".dds")
        let file = if extension.IsSome then file + extension.Value else file

        match prefix with
        | Some pre ->
            if
                files.Contains file
                || files.Contains(pre + file)
                || files.Contains file2
                || files.Contains(pre + file2)
            then
                errors
            else
                inv (ErrorCodes.MissingFile file) leafornode <&&&> errors
        | None ->
            if files.Contains file || files.Contains file2 then
                errors
            else
                inv (ErrorCodes.MissingFile file) leafornode <&&&> errors

    let checkFilepathFieldNE
        (files: Collections.Set<string>)
        (ids: StringTokens)
        (prefix: string option)
        (extension: string option)
        =
        let key = getOriginalKey ids
        let file = (trimQuote key).Replace("\\", "/")
        let file2 = file.Replace(".lua", ".shader").Replace(".tga", ".dds")
        let file = if extension.IsSome then file + extension.Value else file

        match prefix with
        | Some pre ->
            files.Contains file
            || files.Contains(pre + file)
            || files.Contains file2
            || files.Contains(pre + file2)
        | None -> files.Contains file || files.Contains file2

    let checkIconField (files: Collections.Set<string>) (folder: string) (ids: StringTokens) leafornode errors =
        let key = trimQuote (getOriginalKey ids)
        let value = folder + "/" + key + ".dds"

        if files.Contains value then
            errors
        else
            inv (ErrorCodes.MissingFile value) leafornode <&&&> errors

    let checkIconFieldNE (files: Collections.Set<string>) (folder: string) (ids: StringTokens) =
        let key = trimQuote (getOriginalKey ids)
        let value = folder + "/" + key + ".dds"
        files.Contains value

    let private checkAnyScopesMatch anyScope (scopes: Scope list) (currentScope: Scope) =
        (currentScope = anyScope)
        || (List.exists (fun s -> currentScope.IsOfScope(s) || s = anyScope) scopes)

    let checkScopeField
        (linkMap: EffectMap)
        (valueTriggerMap: EffectMap)
        (wildcardLinks: ScopedEffect list)
        varSet
        changeScope
        anyScope
        (ctx: RuleContext)
        s
        (ids: StringTokens)
        leafornode
        errors
        =
        // let key = key.Trim([|'"';' '|])
        let key = getOriginalKey ids
        let key = key.Trim([| '"' |])
        let scope = ctx.scopes

        match changeScope false true linkMap valueTriggerMap wildcardLinks varSet key scope with
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = ( ^a : (static member AnyScope : ^a) ()) || current = ( ^a : (static member AnyScope : ^a) ()) then OK else Invalid (Guid.NewGuid(), [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode])
        | ScopeResult.NewScope({ Scopes = current :: _ }, _, _) ->
            if checkAnyScopesMatch anyScope s current then
                errors
            else
                inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString()) key) leafornode
                <&&&> errors
        | NotFound ->
            inv (ErrorCodes.ConfigRulesInvalidTarget (s.ToString()) key) leafornode
            <&&&> errors
        | ScopeResult.WrongScope(command, prevscope, expected, _) ->
            inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) $"{expected}") leafornode
            <&&&> errors
        | VarFound -> errors
        | VarNotFound s -> inv (ErrorCodes.ConfigRulesUnsetVariable s) leafornode <&&&> errors
        | ValueFound _ ->
            inv (ErrorCodes.CustomError "This is a value, but should be a scope" Severity.Error) leafornode
            <&&&> errors
        | _ -> errors

    let checkScopeFieldNE
        (linkMap: EffectMap)
        (valueTriggerMap: EffectMap)
        (wildcardLinks: ScopedEffect list)
        varSet
        changeScope
        anyScope
        (ctx: RuleContext)
        s
        (ids: StringTokens)
        =
        // log "scope %s %A"key ctx
        let key = getOriginalKey ids
        let key = key.Trim([| '"' |])
        let scope = ctx.scopes

        match changeScope true true linkMap valueTriggerMap wildcardLinks varSet key scope with
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = ( ^a : (static member AnyScope : ^a) ()) || current = ( ^a : (static member AnyScope : ^a) ()) then OK else Invalid (Guid.NewGuid(), [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode])
        | ScopeResult.NewScope({ Scopes = current :: _ }, _, _) -> checkAnyScopesMatch anyScope s current
        | NotFound -> false
        | ScopeResult.WrongScope(command, prevscope, expected, _) -> true
        | VarNotFound s -> false
        | ValueFound _ -> false
        | _ -> true

    let checkVariableField
        (linkMap: EffectMap)
        (valueTriggerMap: EffectMap)
        (wildcardLinks: ScopedEffect list)
        varSet
        changeScope
        anyScope
        (ctx: RuleContext)
        isInt
        is32Bit
        min
        max
        (ids: StringTokens)
        leafornode
        errors
        =
        let scope = ctx.scopes
        let key = getOriginalKey ids
        let metadata = getStringMetadata ids.lower

        if metadata.startsWithAmp then
            errors
        else
            let key =
                match metadata.containsQuestionMark, metadata.containsHat with
                | true, _ -> key.Split('?').[0]
                | false, true -> key.Split('^').[0]
                | _ -> key

            match
                TryParser.parseDecimal key,
                TryParser.parseInt key,
                changeScope false true linkMap valueTriggerMap wildcardLinks varSet key scope
            with
            | _, Some i, _ when isInt && min <= decimal i && max >= decimal i -> errors
            | Some f, _, _ when (not isInt) && min <= f && max >= f && ((not is32Bit) || (f = Math.Round(f, 3))) ->
                errors
            | Some f, _, _ when min <= f && max >= f ->
                inv ErrorCodes.ConfigRulesVariableTooSmall leafornode <&&&> errors
            | Some f, _, _ when isInt -> inv ErrorCodes.ConfigRulesVariableIntOnly leafornode <&&&> errors
            | _, _, VarFound -> errors
            | _, _, VarNotFound s -> inv (ErrorCodes.ConfigRulesUnsetVariable s) leafornode <&&&> errors
            //TODO: Better error messages for scope instead of variable
            // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then OK else Invalid (Guid.NewGuid(), [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode])
            | _, _, ScopeResult.WrongScope(command, prevscope, expected, refHint) ->
                Invalid(
                    Guid.NewGuid(),
                    [ inv
                          (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) $"%A{expected}")
                          leafornode ]
                )
            | _, _, NotFound -> inv ErrorCodes.ConfigRulesExpectedVariableValue leafornode <&&&> errors
            //        |_, _, WrongScope (command, prevscope, expected) ->
            | _ ->
                inv (ErrorCodes.CustomError "Expecting a variable, but got a scope" Severity.Error) leafornode
                <&&&> errors

    let checkVariableFieldNE
        (linkMap: EffectMap)
        (valueTriggerMap: EffectMap)
        (wildcardLinks: ScopedEffect list)
        varSet
        changeScope
        anyScope
        (ctx: RuleContext)
        isInt
        is32Bit
        min
        max
        (ids: StringTokens)
        =
        let scope = ctx.scopes
        let metadata = getStringMetadata ids.lower
        let key = getOriginalKey ids

        if metadata.startsWithAmp then
            true
        else
            let key =
                match metadata.containsQuestionMark, metadata.containsHat with
                | true, _ -> key.Split('?').[0]
                | false, true -> key.Split('^').[0]
                | _ -> key

            match
                TryParser.parseDecimal key,
                TryParser.parseInt key,
                changeScope false true linkMap valueTriggerMap wildcardLinks varSet key scope
            with
            | _, Some i, _ -> isInt && min <= decimal i && max >= decimal i
            | Some f, _, _ -> min <= f && max >= f && ((not is32Bit) || (f = Math.Round(f, 3)))
            | _, _, VarFound -> true
            | _, _, VarNotFound s -> false
            // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then OK else Invalid (Guid.NewGuid(), [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode])
            // |NotFound _ -> Invalid (Guid.NewGuid(), [inv (ErrorCodes.ConfigRulesInvalidTarget (s.ToString())) leafornode])
            // |WrongScope (command, prevscope, expected) -> Invalid (Guid.NewGuid(), [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) leafornode])
            | _ -> false

    let checkValueScopeField
        (enumsMap: Collections.Map<_, string * StringSet>)
        (linkMap: EffectMap)
        (valueTriggerMap: EffectMap)
        (wildcardLinks: ScopedEffect list)
        varSet
        changeScope
        anyScope
        (ctx: RuleContext)
        isInt
        min
        max
        (ids: StringTokens)
        leafornode
        errors
        =
        let scope = ctx.scopes
        // let res = changeScope false true linkMap valueTriggerMap varSet key scope
        let metadata = getStringMetadata ids.lower
        let key = getOriginalKey ids

        let key =
            match metadata.containsPipe with
            | true -> key.Split('|').[0]
            | _ -> key

        match
            firstCharEqualsAmp ids.lower,
            TryParser.parseDecimal key,
            TryParser.parseInt key,
            changeScope false true linkMap valueTriggerMap wildcardLinks varSet key scope
        with
        | true, _, _, _ -> errors
        | _, _, Some i, _ when isInt && min <= decimal i && max >= decimal i -> errors
        | _, Some f, _, _ when min <= f && max >= f -> errors
        | _, _, _, VarFound -> errors
        | _, _, _, VarNotFound s -> inv (ErrorCodes.ConfigRulesUnsetVariable s) leafornode <&&&> errors
        | _, _, _, ValueFound _ -> errors
        //TODO: Better error messages for scope instead of variable
        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then OK else Invalid (Guid.NewGuid(), [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode])
        // |WrongScope (command, prevscope, expected) -> Invalid (Guid.NewGuid(), [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) leafornode])
        | _, _, _, ScopeResult.WrongScope(command, prevscope, expected, refHint) ->
            inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) $"%A{expected}") leafornode
            <&&&> errors
        | _, _, _, NotFound ->
            match enumsMap.TryFind "static_values" with
            | Some(_, es) ->
                if es.ContainsKey(trimQuote key) then
                    errors
                else
                    inv ErrorCodes.ConfigRulesExpectedVariableValue leafornode <&&&> errors
            | None -> inv ErrorCodes.ConfigRulesExpectedVariableValue leafornode <&&&> errors
        | _ ->
            inv (ErrorCodes.CustomError "Expecting a variable, but got a scope" Severity.Error) leafornode
            <&&&> errors

    let checkValueScopeFieldNE
        (enumsMap: Collections.Map<_, string * StringSet>)
        (linkMap: EffectMap)
        (valueTriggerMap: EffectMap)
        (wildcardLinks: ScopedEffect list)
        varSet
        changeScope
        anyScope
        (ctx: RuleContext)
        isInt
        min
        max
        (ids: StringTokens)
        =
        let scope = ctx.scopes
        let key = getOriginalKey ids

        match
            firstCharEqualsAmp ids.lower,
            TryParser.parseDecimal key,
            TryParser.parseInt key,
            changeScope false true linkMap valueTriggerMap wildcardLinks varSet key scope
        with
        | true, _, _, _ -> true
        | _, _, Some i, _ -> isInt && min <= decimal i && max >= decimal i
        | _, Some f, _, _ -> min <= f && max >= f
        | _, _, _, VarFound -> true
        | _, _, _, VarNotFound s -> false
        | _, _, _, ValueFound _ -> true
        | _, _, _, NotFound ->
            match enumsMap.TryFind "static_values" with
            | Some(_, es) -> es.ContainsKey(trimQuote key)
            | None -> false

        // |NewScope ({Scopes = current::_} ,_) -> if current = s || s = anyScope || current = anyScope then OK else Invalid (Guid.NewGuid(), [inv (ErrorCodes.ConfigRulesTargetWrongScope (current.ToString()) (s.ToString())) leafornode])
        // |NotFound _ -> Invalid (Guid.NewGuid(), [inv (ErrorCodes.ConfigRulesInvalidTarget (s.ToString())) leafornode])
        // |WrongScope (command, prevscope, expected) -> Invalid (Guid.NewGuid(), [inv (ErrorCodes.ConfigRulesErrorInTarget command (prevscope.ToString()) (sprintf "%A" expected) ) leafornode])
        | _ -> false

    let checkAliasValueKeysField
        (aliasKeyList: Collections.Map<string, Collections.Set<StringToken>>)
        aliasKey
        (ids: StringTokens)
        severity
        leafornode
        errors
        =
        let key = getOriginalKey ids

        match aliasKeyList |> Map.tryFind aliasKey with
        | Some values ->
            if values |> Set.contains ids.lower then
                errors
            else
                inv (ErrorCodes.ConfigRulesUnexpectedAliasKeyValue aliasKey key severity) leafornode
                <&&&> errors
        | None ->
            inv (ErrorCodes.ConfigRulesUnexpectedAliasKeyValue aliasKey key severity) leafornode
            <&&&> errors

    let checkAliasValueKeysFieldNE
        (aliasKeyList: Collections.Map<string, Collections.Set<StringToken>>)
        aliasKey
        (ids: StringTokens)
        =
        let key = getOriginalKey ids

        match aliasKeyList |> Map.tryFind aliasKey with
        | Some values -> values |> Set.contains ids.lower
        | None -> true

    let rec checkField
        (p: CheckFieldParams)
        (severity: Severity)
        (ctx: RuleContext)
        (field: NewField)
        (keyIDs: StringTokens)
        (leafornode: IKeyPos)
        errors
        =
        let metadataForId = (stringManager.GetMetadataForID keyIDs.lower)

        if metadataForId.containsDoubleDollar || metadataForId.startsWithSquareBracket then
            errors
        else
            match field with
            | ValueField vt -> checkValidValue p.varMap p.enumsMap p.localisation severity vt keyIDs leafornode errors
            | TypeField t -> checkTypeField p.typesMap severity t keyIDs leafornode errors
            | ScopeField s ->
                checkScopeField
                    p.linkMap
                    p.valueTriggerMap
                    p.wildcardLinks
                    p.varSet
                    p.changeScope
                    p.anyScope
                    ctx
                    s
                    keyIDs
                    leafornode
                    errors
            | LocalisationField(synced, isInline) ->
                checkLocalisationField
                    p.processLocalisation
                    p.validateLocalisation
                    ctx.scopes
                    p.localisation
                    p.defaultLocalisation
                    p.defaultLang
                    synced
                    isInline
                    keyIDs
                    leafornode
                    errors
            | FilepathField(prefix, extension) -> checkFilepathField p.files keyIDs prefix extension leafornode errors
            | IconField folder -> checkIconField p.files folder keyIDs leafornode errors
            | VariableSetField v -> errors
            | VariableGetField v -> checkVariableGetField p.varMap severity v keyIDs leafornode errors
            | VariableField(isInt, is32Bit, (min, max)) ->
                checkVariableField
                    p.linkMap
                    p.valueTriggerMap
                    p.wildcardLinks
                    p.varSet
                    p.changeScope
                    p.anyScope
                    ctx
                    isInt
                    is32Bit
                    min
                    max
                    keyIDs
                    leafornode
                    errors
            | ValueScopeField(isInt, (min, max)) ->
                checkValueScopeField
                    p.enumsMap
                    p.linkMap
                    p.valueTriggerMap
                    p.wildcardLinks
                    p.varSet
                    p.changeScope
                    p.anyScope
                    ctx
                    isInt
                    min
                    max
                    keyIDs
                    leafornode
                    errors
            | ScalarField _ -> errors
            | SpecificField(SpecificValue v) ->
                if keyIDs.lower = v.lower then
                    errors
                else
                    inv
                        (ErrorCodes.ConfigRulesUnexpectedValue
                            $"Expecting value %s{StringResource.stringManager.GetStringForID(v.normal)}"
                            severity)
                        leafornode
                    <&&&> errors
            | AliasField _
            | MarkerField _
            | SingleAliasField _
            | SubtypeField _
            | TypeMarkerField _
            | IgnoreMarkerField
            | ValueScopeMarkerField _ ->
                inv (ErrorCodes.CustomError $"Unexpected rule type {field}" Severity.Error) leafornode
                <&&&> errors
            | AliasValueKeysField aliasKey ->
                checkAliasValueKeysField p.aliasKeyList aliasKey keyIDs severity leafornode errors
            | IgnoreField field -> checkField p severity ctx field keyIDs leafornode errors

    let rec checkFieldNE
        (p: CheckFieldParams)
        (severity: Severity)
        (ctx: RuleContext)
        (field: NewField)
        (keyIDs: StringTokens)
        =
        let metadataForId = (stringManager.GetMetadataForID keyIDs.lower)

        if metadataForId.containsDoubleDollar || metadataForId.startsWithSquareBracket then
            true
        else
            match field with
            | ValueField vt -> checkValidValueNE p.varMap p.enumsMap p.localisation severity vt keyIDs
            | TypeField t -> checkTypeFieldNE p.typesMap severity t keyIDs
            | ScopeField s ->
                checkScopeFieldNE
                    p.linkMap
                    p.valueTriggerMap
                    p.wildcardLinks
                    p.varSet
                    p.changeScope
                    p.anyScope
                    ctx
                    s
                    keyIDs
            | LocalisationField(synced, isInline) -> true
            | FilepathField(prefix, extension) -> checkFilepathFieldNE p.files keyIDs prefix extension
            | IconField folder -> checkIconFieldNE p.files folder keyIDs
            | VariableSetField v -> true
            | VariableGetField v -> checkVariableGetFieldNE p.varMap severity v keyIDs
            | VariableField(isInt, is32Bit, (min, max)) ->
                checkVariableFieldNE
                    p.linkMap
                    p.valueTriggerMap
                    p.wildcardLinks
                    p.varSet
                    p.changeScope
                    p.anyScope
                    ctx
                    isInt
                    is32Bit
                    min
                    max
                    keyIDs
            | ValueScopeField(isInt, (min, max)) ->
                checkValueScopeFieldNE
                    p.enumsMap
                    p.linkMap
                    p.valueTriggerMap
                    p.wildcardLinks
                    p.varSet
                    p.changeScope
                    p.anyScope
                    ctx
                    isInt
                    min
                    max
                    keyIDs
            | TypeMarkerField(dummy, _) -> dummy = keyIDs.lower
            | ScalarField _ -> true
            | SpecificField(SpecificValue v) -> v.lower = keyIDs.lower
            | AliasField _
            | MarkerField _
            | SingleAliasField _
            | SubtypeField _
            | TypeMarkerField _
            | IgnoreMarkerField
            | ValueScopeMarkerField _ -> false
            | AliasValueKeysField aliasKey -> checkAliasValueKeysFieldNE p.aliasKeyList aliasKey keyIDs
            | IgnoreField field -> checkFieldNE p severity ctx field keyIDs

    let checkLeftField (p: CheckFieldParams) (severity: Severity) (ctx: RuleContext) (field: NewField) keyIDs =
        checkFieldNE p severity ctx field keyIDs

    let checkFieldByKey (p: CheckFieldParams) (severity: Severity) (ctx: RuleContext) (field: NewField) keyIDs =
        checkLeftField p severity ctx field keyIDs

    let inline validateTypeLocalisation
        (typedefs: TypeDefinition list)
        (invertedTypeMap: Collections.Map<string, string list>)
        localisation
        (typeKey: string)
        (key: string)
        leafornode
        =
        let typenames = typeKey.Split('.')
        let typename = typenames.[0]

        let actualSubtypes =
            match invertedTypeMap |> Map.tryFind key with
            | Some keytypes ->
                keytypes
                |> List.filter (fun kt -> kt.StartsWith(typename + ".", StringComparison.OrdinalIgnoreCase))
                |> List.map (fun kt -> kt.Split('.').[1])
            | None -> []

        match typedefs |> List.tryFind (fun t -> t.name == typename) with
        | None -> OK
        | Some typedef ->
            let inner =
                (fun (l: TypeLocalisation) ->
                    let lockey = l.prefix + key + l.suffix

                    if l.optional || l.explicitField.IsSome then
                        OK
                    else
                        CWTools.Validation.Stellaris.STLLocalisationValidation.checkLocKeysLeafOrNode
                            localisation
                            lockey
                            leafornode)

            let subtype =
                let subtypes =
                    (if typenames.Length > 1 then
                         typenames.[1] :: actualSubtypes
                     else
                         actualSubtypes)
                    |> List.distinct

                let inner2 (nextSt: string) =
                    match typedef.subtypes |> List.tryFind (fun st -> st.name == nextSt) with
                    | None -> OK
                    | Some st -> st.localisation <&!&> inner

                subtypes <&!&> inner2

            typedef.localisation <&!&> inner <&&> subtype

    let typekeyfilter (td: TypeDefinition) (n: string) (keyPrefix: string option) =
        match td.typeKeyFilter with
        | Some(values, negate) -> (values |> List.exists ((==) n)) <> negate
        | None -> true
        && match td.startsWith with
           | Some prefix -> n.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)
           | None -> true
        && match td.keyPrefix, keyPrefix with
           | Some prefix, Some keyPrefix -> prefix == keyPrefix
           | None, None -> true
           | _ -> false
