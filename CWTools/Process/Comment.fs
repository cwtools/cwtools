namespace CWTools.Process

open CWTools.Utilities.Position

type Comment =
    {
        Position: range
        Comment: string
    }