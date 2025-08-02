open System.Text

#r @"C:\Users\Thomas\.nuget\packages\dotnet.glob\2.0.3\lib\netstandard1.1\DotNet.Glob.dll"
#r @"C:\Users\Thomas\.nuget\packages\fparsec\1.0.4-rc3\lib\netstandard1.6\FParsec.dll"
#r @"C:\Users\Thomas\.nuget\packages\fparsec\1.0.4-rc3\lib\netstandard1.6\FParsecCS.dll"
#r @"C:\Users\Thomas\.nuget\packages\fsharp.collections.parallelseq\1.1.2\lib\netstandard2.0\FSharp.Collections.ParallelSeq.dll"
#r @"C:\Users\Thomas\.nuget\packages\fsharp.data\3.0.0-beta\lib\netstandard2.0\FSharp.Data.dll"
#r @"C:\Users\Thomas\.nuget\packages\fsharpx.collections\1.17.0\lib\net40\FSharpx.Collections.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\Microsoft.Win32.Primitives.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.AppContext.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Collections.Concurrent.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Collections.NonGeneric.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Collections.Specialized.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Collections.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ComponentModel.Composition.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ComponentModel.EventBasedAsync.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ComponentModel.Primitives.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ComponentModel.TypeConverter.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ComponentModel.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Console.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Core.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Data.Common.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Data.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.Contracts.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.Debug.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.FileVersionInfo.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.Process.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.StackTrace.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.TextWriterTraceListener.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.Tools.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.TraceSource.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.Tracing.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Drawing.Primitives.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Drawing.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Dynamic.Runtime.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Globalization.Calendars.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Globalization.Extensions.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Globalization.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.Compression.FileSystem.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.Compression.ZipFile.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.Compression.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.FileSystem.DriveInfo.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.FileSystem.Primitives.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.FileSystem.Watcher.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.FileSystem.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.IsolatedStorage.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.MemoryMappedFiles.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.Pipes.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.UnmanagedMemoryStream.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Linq.Expressions.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Linq.Parallel.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Linq.Queryable.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Linq.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.Http.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.NameResolution.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.NetworkInformation.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.Ping.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.Primitives.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.Requests.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.Security.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.Sockets.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.WebHeaderCollection.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.WebSockets.Client.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.WebSockets.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Numerics.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ObjectModel.dll"
#r @"C:\Users\Thomas\.nuget\packages\system.reflection.emit.ilgeneration\4.3.0\ref\netstandard1.0\System.Reflection.Emit.ILGeneration.dll"
#r @"C:\Users\Thomas\.nuget\packages\system.reflection.emit.lightweight\4.3.0\ref\netstandard1.0\System.Reflection.Emit.Lightweight.dll"
#r @"C:\Users\Thomas\.nuget\packages\system.reflection.emit\4.3.0\ref\netstandard1.1\System.Reflection.Emit.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Reflection.Extensions.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Reflection.Primitives.dll"
#r @"C:\Users\Thomas\.nuget\packages\system.reflection.typeextensions\4.5.0\ref\netstandard2.0\System.Reflection.TypeExtensions.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Reflection.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Resources.Reader.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Resources.ResourceManager.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Resources.Writer.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.CompilerServices.VisualC.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Extensions.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Handles.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.InteropServices.RuntimeInformation.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.InteropServices.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Numerics.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Serialization.Formatters.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Serialization.Json.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Serialization.Primitives.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Serialization.Xml.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Serialization.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.Claims.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.Cryptography.Algorithms.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.Cryptography.Csp.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.Cryptography.Encoding.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.Cryptography.Primitives.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.Cryptography.X509Certificates.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.Principal.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.SecureString.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ServiceModel.Web.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Text.Encoding.Extensions.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Text.Encoding.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Text.RegularExpressions.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Threading.Overlapped.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Threading.Tasks.Parallel.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Threading.Tasks.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Threading.Thread.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Threading.ThreadPool.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Threading.Timer.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Threading.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Transactions.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ValueTuple.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Web.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Windows.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.Linq.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.ReaderWriter.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.Serialization.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.XDocument.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.XPath.XDocument.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.XPath.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.XmlDocument.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.XmlSerializer.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.dll"
#r @"C:\Users\Thomas\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\netstandard.dll"
#r @"C:\Users\Thomas\git\cwtools/CWTools/bin/Debug/netstandard2.0/CWTools.dll"

open System

open System.IO
open System.Security.Cryptography

open CWTools.Parser
open FParsec

Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)

// let triggers, effects = DocsParser.parseDocsFilesRes @"C:\Users\Thomas\git\cwtools\CWToolsTests\testfiles\validationtests\trigger_docs_2.1.0.txt"
let links =
    JominiParser.parseLinksFilesRes @"C:\Users\Thomas\git\cwtools/Scripts/event_targets.log"

// let tinner =
//             """{
// 	alias_name[effect] = alias_match_left[effect]
// }
// """
// let tout =
//     triggers |> List.map (fun t ->
//                         let scopes = t.scopes |> List.map (fun s -> s.ToString()) |> String.concat " "
//                         let scopes = if scopes = "" then "" else "## scopes = { " + scopes + " }\n"
//                         let any = t.name.StartsWith("any_")
//                         let rhs = if any then tinner else "replace_me"
//                         sprintf "###%s\n%salias[trigger:%s] = %s\n\r" t.desc scopes t.name rhs)
//                 |> String.concat("")

// let einner =
//             """{
//     ## cardinality = 0..1
// 	limit = {
// 		alias_name[trigger] = alias_match_left[trigger]
// 	}
// 	alias_name[effect] = alias_match_left[effect]
// }
// """
// let eout =
//     effects |> List.map (fun t ->
//                         let scopes = t.scopes |> List.map (fun s -> s.ToString()) |> String.concat " "
//                         let scopes = if scopes = "" then "" else "## scopes = { " + scopes + " }\n"
//                         let every = t.name.StartsWith("every_") || t.name.StartsWith("random_")
//                         let rhs = if every then einner else "replace_me"
//                         sprintf "###%s\n%salias[effect:%s] = %s\n\r" t.desc scopes t.name rhs)
//                 |> String.concat("")

// File.WriteAllText("triggers.cwt", tout)
// File.WriteAllText("effects.cwt", eout)

let createLink (n, (d: string), r, w, i, (o: string list option), g) =
    match r with
    | Some _ ->
        let input =
            match i with
            | None -> None
            | Some [ x ] -> Some("input_scopes = " + x)
            | Some xs ->
                Some(
                    "input_scopes = { "
                    + (String.concat " " (xs |> List.map (fun x -> x.Trim().Replace(" ", "_"))))
                    + " }"
                )

        let output =
            match o with
            | None -> None
            | Some oi -> Some("output_scope = " + (oi |> List.head))

        let start = Some(n + " = {\n\t" + "desc = \"" + d + "\"")
        let fromData = g |> Option.map (fun _ -> "from_data = yes")
        let dataSource = g |> Option.map (fun _ -> ("data_source = <" + n + ">"))

        let prefix =
            match g with
            | None -> None
            | Some _ -> Some(sprintf "prefix = %s:" n)

        ([ start; fromData; dataSource; prefix; input; output ]
         |> List.choose id
         |> String.concat ("\n\t"))
        + "\n}"
    | None ->
        let input =
            match i with
            | None -> None
            | Some [ x ] -> Some("input_scopes = " + x)
            | Some xs ->
                Some(
                    "input_scopes = { "
                    + (String.concat " " (xs |> List.map (fun x -> x.Trim().Replace(" ", "_"))))
                    + " }"
                )

        let output =
            match o with
            | None -> None
            | Some oi -> Some("output_scope = " + (oi |> List.head))

        let start = Some(n + " = {\n\t" + "desc = \"" + d + "\"")
        let fromData = g |> Option.map (fun _ -> "from_data = yes")
        let dataSource = g |> Option.map (fun _ -> ("data_source = <" + n + ">"))
        // let prefix =
        //     match g with
        //     | None -> None
        //     | Some _ -> Some (sprintf "prefix = %s:" n)
        ([ start; fromData; dataSource; input; output ]
         |> List.choose id
         |> String.concat ("\n\t"))
        + "\n}"

let linksText = links |> List.map createLink
File.WriteAllLines("links.cwt", linksText)
