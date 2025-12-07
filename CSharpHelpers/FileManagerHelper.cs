using Cysharp.Text;
using Shared;

namespace CSharpHelpers;

public static class FileManagerHelper
{
    // need thread-safe
    public static string ConvertPathToLogicalPath(
        ReadOnlySpan<char> path,
        IEnumerable<ExpandedWorkspaceDirectory> expandedWorkspaceDirectories,
        IEnumerable<string> scriptFolders
    )
    {
        // must be false
        using var sb = ZString.CreateStringBuilder(false);
        sb.Append(path);
        sb.Replace('\\', '/');

        var pathSpan = sb.AsSpan();
        foreach (var expandedWorkspaceDirectory in expandedWorkspaceDirectories)
        {
            int index = pathSpan.IndexOf(
                expandedWorkspaceDirectory.normalisedPath.AsSpan(),
                StringComparison.Ordinal
            );

            if (index >= 0)
            {
                sb.Remove(0, index + expandedWorkspaceDirectory.normalisedPath.Length);
                pathSpan = sb.AsSpan();
                break;
            }
        }

        if (pathSpan.StartsWith("gfx/", StringComparison.Ordinal))
        {
            return sb.ToString();
        }

        var matches = new List<int>();

        using var pathBuilder = ZString.CreateStringBuilder(false);
        foreach (string folder in scriptFolders)
        {
            int index = FindFolderIndexInPath(pathSpan, folder, pathBuilder);
            if (index < 0)
            {
                continue;
            }

            matches.Add(++index);
        }

        var result = sb.ToString();
        if (matches.Count == 0)
        {
            return result;
        }

        int minIndex = matches.Min();
        return result[minIndex..];
    }

    private static int FindFolderIndexInPath(
        ReadOnlySpan<char> pathSpan,
        string folder,
        Utf16ValueStringBuilder builder
    )
    {
        builder.Clear();

        builder.Append('/');
        builder.Append(folder);
        builder.Append('/');
        builder.Replace('\\', '/');
        return pathSpan.IndexOf(builder.AsSpan(), StringComparison.Ordinal);
    }
}
