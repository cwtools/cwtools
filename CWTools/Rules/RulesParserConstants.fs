namespace CWTools.Rules

open System

module RulesParserConstants =
    [<Literal>]
    let IntFieldDefaultMinimum = Int32.MinValue

    [<Literal>]
    let IntFieldDefaultMaximum = Int32.MaxValue

    [<Literal>]
    let CardinalityDefaultMaximum = 10000

    [<Literal>]
    let floatFieldDefaultMinimum = -1E+12M

    [<Literal>]
    let floatFieldDefaultMaximum = 1E+12M
