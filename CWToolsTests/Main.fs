module CWToolsTests

open Expecto
open System.Text
open System.Threading
open System.Globalization
open Expecto.Logging
open Expecto.Logging.Message

let logger = Log.create "MyTests"
CWTools.Utilities.Utils.logDiag <- logger.logSimple << (event LogLevel.Verbose)
CWTools.Utilities.Utils.logInfo <- logger.logSimple << (event LogLevel.Info)
CWTools.Utilities.Utils.logWarning <- logger.logSimple << (event LogLevel.Warn)
CWTools.Utilities.Utils.logError <- logger.logSimple << (event LogLevel.Error)


[<EntryPoint>]
let main argv =
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

    CultureInfo.DefaultThreadCurrentCulture <- CultureInfo("ru-RU")
    CultureInfo.DefaultThreadCurrentUICulture <- CultureInfo("ru-RU")
    Thread.CurrentThread.CurrentCulture <- CultureInfo("ru-RU")
    Thread.CurrentThread.CurrentUICulture <- CultureInfo("ru-RU")
    CWTools.Utilities.Utils.loglevel <- CWTools.Utilities.Utils.LogLevel.Verbose

    Tests.runTestsInAssemblyWithCLIArgs [ Sequenced; CLIArguments.Summary ] argv
