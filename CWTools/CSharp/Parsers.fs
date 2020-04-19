namespace CWTools.CSharp
open CWTools.Parser
open CWTools.Process.STLProcess
open CWTools.Utilities.Utils

[<NoEquality; NoComparison>]
type ParserError = {
    Filename : string
    Line : int64
    Column : int64
    ErrorMessage : string
}
// type ParserError(filename : string, line : int64, column : int64, error : string) =
//     member val FileName = filename with get
//     member val Line = line with get
//     member val Column = column with get
//     member val ErrorMessage = error with get

type Parsers =
    static member ParseScriptFile(filename: string, filetext : string) = CKParser.parseString filetext filename
    static member ProcessStatements(filename: string, filepath : string, statements : Statement list) =
        simpleProcess.ProcessNode() filename (mkZeroFile filepath) statements