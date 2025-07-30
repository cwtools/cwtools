open System
open System.IO

let countLines path wildcard recurse =

    let lineCount file =

        let isEmpty (line: string) = line.Trim() = ""
        let isComment (line: string) = line.Trim().StartsWith("//")

        let isCode (line: string) =
            not (isEmpty line) && not (isComment line)

        File.ReadAllLines file |> Seq.filter (fun line -> isCode line) |> Seq.length

    Directory.EnumerateFiles(
        path,
        wildcard,
        if recurse then
            SearchOption.AllDirectories
        else
            SearchOption.TopDirectoryOnly
    )
    |> Seq.map (fun file -> lineCount file)
    |> Seq.sum

// Example
let test1 = countLines @"./" "*.fs" true
