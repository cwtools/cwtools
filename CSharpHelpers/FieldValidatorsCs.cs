using Shared;

namespace CSharpHelpers;

public static class FieldValidatorsCs
{
    public static bool CheckPathDir(PathOptions pathOptions, string fullPath)
    {
        // to prevent stack overflow, chosen a relatively conservative array size.
        const int maxStackSize = 256;

        var span = fullPath.AsSpan();
        var dirSpan = Path.GetDirectoryName(span);
        Span<char> directory =
            dirSpan.Length > maxStackSize
                ? new char[dirSpan.Length]
                : stackalloc char[dirSpan.Length];

        // TODO: use Vector check?
        dirSpan.Replace(directory, '\\', '/');

        return CheckPathDir(pathOptions, directory, Path.GetFileName(span));
    }

    public static bool CheckPathDir(
        PathOptions pathOptions,
        ReadOnlySpan<char> directory,
        ReadOnlySpan<char> fileName
    )
    {
        bool exists = false;
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
                    directory.StartsWith(
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
            var extension = Path.GetExtension(fileName);
            isValidExtension = extension.Equals(
                pathOptions.pathExtension.Value,
                StringComparison.OrdinalIgnoreCase
            );
        }

        return exists && isValidFileName && isValidExtension;
    }

    public static ReadOnlySpan<Range> Split(string key, Span<Range> destination, char separator)
    {
        int length = key.AsSpan().Split(destination, separator);
        return destination[..length];
    }
}
