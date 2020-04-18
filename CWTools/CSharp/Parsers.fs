namespace CWTools.CSharp
open CWTools.Parser

type Parsers =
    static member ParseScriptFile(filename: string, filetext : string) = CKParser.parseString filetext filename
