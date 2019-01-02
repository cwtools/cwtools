#r "System.Core.dll"

open System

open System.IO
open System.Security.Cryptography

let rec allFilesUnder baseFolder =
    seq {
        yield! Directory.GetFiles(baseFolder)
        for subDir in Directory.GetDirectories(baseFolder) do
            yield! allFilesUnder subDir
        }

let MD5 filename =
    use md5 = MD5.Create()
    use stream = File.OpenRead(filename)
    let hash = md5.ComputeHash(stream)
    BitConverter.ToString(hash).Replace("-", "").ToLowerInvariant()

let outFile = new StreamWriter("out.csv")

// allFilesUnder @"C:\Users\Thomas\Documents\Paradox Interactive\Stellaris\mod\newhorizons3\gfx"
allFilesUnder @"F:\Git\New-Horizons-Development\gfx"
|> Seq.map (fun fn -> fn, MD5 fn, FileInfo(fn).Length)
|> Seq.iter (fun (fn, hash, fs) -> outFile.WriteLine(sprintf "%A, %A, %A" fn hash fs ))
outFile.Flush()