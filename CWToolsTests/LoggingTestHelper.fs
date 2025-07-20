module LogCaptureTest

open Expecto
open Expecto.Logging
open System.Text

/// Runs a test with log capture, only showing logs if the test fails
let testWithCapturedLogs name testFunc =
    testCase name <| fun () ->
        // Create a StringBuilder to capture logs
        let logCapture = StringBuilder()
        
        // Original loggers
        let originalDiag = CWTools.Utilities.Utils.logDiag
        let originalInfo = CWTools.Utilities.Utils.logInfo
        let originalWarning = CWTools.Utilities.Utils.logWarning
        let originalError = CWTools.Utilities.Utils.logError
        
        // Create a test-specific logger
        let testLogger = Expecto.Logging.Log.create (sprintf "Test: %s" name)
        
        // Override the logging functions to capture output
        let captureLog level msg =
            // Call original logger
            testLogger.logSimple (Message.event level msg)
            // Also capture to our StringBuilder
            logCapture.AppendLine(sprintf "[%A] %s" level msg) |> ignore
            
        // Replace the global loggers with our capturing versions
        CWTools.Utilities.Utils.logDiag <- captureLog Logging.LogLevel.Verbose
        CWTools.Utilities.Utils.logInfo <- captureLog Logging.LogLevel.Info
        CWTools.Utilities.Utils.logWarning <- captureLog Logging.LogLevel.Warn
        CWTools.Utilities.Utils.logError <- captureLog Logging.LogLevel.Error
        
        try
            try
                // Run the test
                testFunc()
                // If we get here, test passed - discard logs
            with ex ->
                // Test failed - print the captured logs
                if logCapture.Length > 0 then
                    printfn "===== Logs for failed test: %s =====" name
                    printfn "%s" (logCapture.ToString())
                    printfn "===== End logs for: %s =====" name
                
                // Re-throw the exception
                reraise()
        finally
            // Restore the original loggers
            CWTools.Utilities.Utils.logDiag <- originalDiag
            CWTools.Utilities.Utils.logInfo <- originalInfo
            CWTools.Utilities.Utils.logWarning <- originalWarning
            CWTools.Utilities.Utils.logError <- originalError