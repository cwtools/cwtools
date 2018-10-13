namespace CWTools.Common

type Game = |CK2 = 0 |HOI4 = 1 |EU4 = 2 |STL = 3
type CK2Lang = |English = 0 |French = 1 |German = 2 |Spanish = 3 |Russian = 4
type STLLang = |English = 0 |French = 1 |German = 2 |Spanish = 3 |Russian = 4 |Polish = 5 |Braz_Por = 6 |Default = 7
type HOI4Lang = |English = 0 |French = 1 |German = 2 |Spanish = 3 |Russian = 4 |Polish = 5 |Braz_Por = 6
type EU4Lang = |English = 0 |French = 1 |German = 2 |Spanish = 3
type Lang =
    |CK2 of CK2Lang
    |STL of STLLang
    |HOI4 of HOI4Lang
    |EU4 of EU4Lang
    override x.ToString() = x |> function |CK2 c -> c.ToString() | STL s -> s.ToString() |HOI4 s -> s.ToString() |EU4 s -> s.ToString()

type RawEffect =
    {
        name : string
        desc : string
        usage : string
        scopes : string list
        targets : string list
    }

type Severity =
| Error = 1
| Warning = 2
| Information = 3
| Hint = 4

type ComputedData() =
    member val Cache : Map<string, obj list> = Map.empty with get, set

type EU4ComputedData() =
    inherit ComputedData()
