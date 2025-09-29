namespace Shared

type PathOptions =
    { paths: string array
      pathStrict: bool
      pathFile: string option
      pathExtension: string option }

type ModInfo = { name: string; path: string }

type DirectoryType =
    | Vanilla
    | MultipleMod of ModInfo array
    | Mod
    | Unknown

type ExpandedWorkspaceDirectory =
    { path: string
      name: string
      dirType: DirectoryType
      normalisedPath: string }
