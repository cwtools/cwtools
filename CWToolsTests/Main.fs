module CWToolsTests

open Expecto
open System.Text

[<EntryPoint>]
let main argv =
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    Tests.runTestsInAssembly defaultConfig argv
