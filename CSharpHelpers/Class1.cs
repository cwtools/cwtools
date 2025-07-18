using Shared;

namespace CSharpHelpers;

public static class FieldValidators
{
    public static bool CheckPathDir(PathOptions pathOptions, string fullPath)
    {
        bool exists = false;
        var span = fullPath.AsSpan();
        var directory = new Span<char>(new char[span.Length]);
        Path.GetDirectoryName(span).Replace(directory, '\\', '/');

        if (pathOptions.pathStrict)
        {
            foreach (string path in pathOptions.paths)
            {
                if (
                    path.Replace('\\', '/')
                        .AsSpan()
                        .Equals(directory, StringComparison.OrdinalIgnoreCase)
                )
                {
                    exists = true;
                    break;
                }
            }
        }
        else
        {
            foreach (string path in pathOptions.paths)
            {
                if (
                    MemoryExtensions.StartsWith(
                        directory,
                        path.Replace('\\', '/'),
                        StringComparison.OrdinalIgnoreCase
                    )
                )
                {
                    exists = true;
                    break;
                }
            }
        }

        bool isValidFileName;
        if (pathOptions.pathFile is null)
        {
            isValidFileName = true;
        }
        else
        {
            var fileName = Path.GetFileName(span);
            isValidFileName = fileName.Equals(
                pathOptions.pathFile.Value,
                StringComparison.OrdinalIgnoreCase
            );
        }

        bool isValidExtension;
        if (pathOptions.pathExtension is null)
        {
            isValidExtension = true;
        }
        else
        {
            var extension = Path.GetExtension(span);
            isValidExtension = extension.Equals(
                pathOptions.pathExtension.Value,
                StringComparison.OrdinalIgnoreCase
            );
        }

        return exists && isValidFileName && isValidExtension;
    }
}
