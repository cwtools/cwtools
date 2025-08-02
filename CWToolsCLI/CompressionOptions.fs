module CWToolsCLI.CompressionOptions

open System.IO
open ICSharpCode.SharpZipLib.BZip2

/// Compression algorithm options
type Compression =
    | NoCompression
    | Bz2

/// Convert compression type to file extension
let getExtension =
    function
    | NoCompression -> ""
    | Bz2 -> ".bz2"

/// Compress data using the specified compression algorithm
let compressData (compression: Compression) (data: byte[]) : byte[] =
    match compression with
    | NoCompression -> data
    | Bz2 ->
        use input = new MemoryStream(data)
        use output = new MemoryStream()
        BZip2.Compress(input, output, true, 9)
        output.ToArray()

/// Decompress data using the specified compression algorithm
let decompressData (compression: Compression) (data: byte[]) : byte[] =
    match compression with
    | NoCompression -> data
    | Bz2 ->
        use input = new MemoryStream(data)
        use output = new MemoryStream()
        BZip2.Decompress(input, output, false)
        output.ToArray()

/// Write compressed data to file
let compressAndWriteToFile (compression: Compression) (data: byte[]) (filename: string) : unit =
    let compressedData = compressData compression data

    let fullFilename =
        match compression with
        | NoCompression -> filename
        | Bz2 -> filename + getExtension compression

    File.WriteAllBytes(fullFilename, compressedData)

/// Read and decompress data from file
let readAndDecompressFromFile (filename: string) : byte[] =
    let data = File.ReadAllBytes(filename)

    let compression =
        match Path.GetExtension(filename).ToLowerInvariant() with
        | ".bz2" -> Bz2
        | _ -> NoCompression

    decompressData compression data

/// Detect compression type from file extension
let detectCompressionFromPath (path: string) : Compression =
    match Path.GetExtension(path).ToLowerInvariant() with
    | ".bz2" -> Bz2
    | _ -> NoCompression
