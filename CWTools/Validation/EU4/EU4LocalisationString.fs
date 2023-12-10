namespace CWTools.Validation.EU4

open CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Common
open System
open System.IO
open CWTools.Utilities.Position

module EU4LocalisationString =

    let checkFileEncoding (file: string) =
        use fs = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.ReadWrite) in
        let bits = Array.zeroCreate 3
        fs.Read(bits, 0, 3) |> ignore
        // UTF8 byte order mark is: 0xEF,0xBB,0xBF
        if (bits.[0] = byte 0xEF && bits.[1] = byte 0xBB && bits.[2] = byte 0xBF) then
            OK
        else
            let pos = rangeN file 0
            Invalid(Guid.NewGuid(), [ invManual ErrorCodes.WrongEncoding pos "" None ])

    let checkLocFileName (file: string) =
        let filename = Path.GetFileNameWithoutExtension file

        if filename = "languages.yml" then
            OK
        else
            let fileHeader =
                File.ReadLines(file)
                |> Seq.tryFind (fun l -> l.Trim().StartsWith("#") |> not && l.Trim().Length > 0)
                |> Option.map (fun h -> h.Trim().Replace(":", ""))
            // log "lcfn %s %A" filename fileHeader
            let keyToLanguage =
                function
                | (x: string) when x.IndexOf("l_english", StringComparison.OrdinalIgnoreCase) >= 0 ->
                    Some STLLang.English
                | x when x.IndexOf("l_french", StringComparison.OrdinalIgnoreCase) >= 0 -> Some STLLang.French
                | x when x.IndexOf("l_spanish", StringComparison.OrdinalIgnoreCase) >= 0 -> Some STLLang.Spanish
                | x when x.IndexOf("l_german", StringComparison.OrdinalIgnoreCase) >= 0 -> Some STLLang.German
                | x when x.IndexOf("l_russian", StringComparison.OrdinalIgnoreCase) >= 0 -> Some STLLang.Russian
                | x when x.IndexOf("l_polish", StringComparison.OrdinalIgnoreCase) >= 0 -> Some STLLang.Polish
                | x when x.IndexOf("l_braz_por", StringComparison.OrdinalIgnoreCase) >= 0 -> Some STLLang.Braz_Por
                | x when x.IndexOf("l_simp_chinese", StringComparison.OrdinalIgnoreCase) >= 0 -> Some STLLang.Chinese
                | x when x.IndexOf("l_japanese", StringComparison.OrdinalIgnoreCase) >= 0 -> Some STLLang.Japanese
                | x when x.IndexOf("l_korean", StringComparison.OrdinalIgnoreCase) >= 0 -> Some STLLang.Korean
                | _ -> None

            let keyAtEnd =
                function
                | (x: string) when x.EndsWith("l_english", StringComparison.OrdinalIgnoreCase) -> true
                | x when x.EndsWith("l_french", StringComparison.OrdinalIgnoreCase) -> true
                | x when x.EndsWith("l_spanish", StringComparison.OrdinalIgnoreCase) -> true
                | x when x.EndsWith("l_german", StringComparison.OrdinalIgnoreCase) -> true
                | x when x.EndsWith("l_russian", StringComparison.OrdinalIgnoreCase) -> true
                | x when x.EndsWith("l_polish", StringComparison.OrdinalIgnoreCase) -> true
                | x when x.EndsWith("l_braz_por", StringComparison.OrdinalIgnoreCase) -> true
                | x when x.EndsWith("l_default", StringComparison.OrdinalIgnoreCase) -> true
                | x when x.EndsWith("l_simp_chinese", StringComparison.OrdinalIgnoreCase) -> true
                | x when x.EndsWith("l_japanese", StringComparison.OrdinalIgnoreCase) -> true
                | x when x.EndsWith("l_korean", StringComparison.OrdinalIgnoreCase) -> true
                | _ -> false

            match keyToLanguage filename, Option.bind keyToLanguage fileHeader with
            | _, Some STLLang.Default -> OK
            | _, None ->
                Invalid(Guid.NewGuid(), [ invManual ErrorCodes.MissingLocFileLangHeader (rangeN file 1) "" None ])
            | None, _ -> Invalid(Guid.NewGuid(), [ invManual ErrorCodes.MissingLocFileLang (rangeN file 1) "" None ])
            // Removed this as only convention
            // |Some l1, Some l2 when not (keyAtEnd filename) -> Invalid (Guid.NewGuid(), [invManual ErrorCodes.LocFileLangWrongPlace (rangeN file 1) "" None ])
            | Some l1, Some l2 when l1 = l2 -> OK
            | Some l1, Some l2 ->
                Invalid(Guid.NewGuid(), [ invManual (ErrorCodes.LocFileLangMismatch l1 l2) (rangeN file 1) "" None ])



    let validateLocalisationFiles (locFiles: string list) =
        locFiles <&!&> (checkLocFileName <&> checkFileEncoding)
