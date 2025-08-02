using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;
using Shared;

namespace CSharpHelpers;

public static partial class FieldValidatorsHelper
{
    [GeneratedRegex("<([^>]*)>")]
    public static partial Regex StlNameFormatRegex();

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

    public static bool IsValidDate(string date)
    {
        var span = date.AsSpan();
        Span<Range> ranges = stackalloc Range[8];
        var parts = Split(span, ranges, '.');

        if (parts.Length != 3)
        {
            return false;
        }

        return IsValidDate(span, parts);
    }

    public static bool IsValidDateTime(string dateTime)
    {
        var span = dateTime.AsSpan();
        Span<Range> ranges = stackalloc Range[8];
        var parts = Split(span, ranges, '.');
        if (parts.Length == 3)
        {
            return IsValidDate(span, parts);
        }

        if (parts.Length == 4)
        {
            if (
                !int.TryParse(span[parts[0]], out int year)
                || !byte.TryParse(span[parts[1]], out byte month)
                || !byte.TryParse(span[parts[2]], out byte day)
                || !byte.TryParse(span[parts[3]], out byte hour)
            )
            {
                return false;
            }

            return year is >= 0 and <= 9999
                && month is >= 1 and <= 12
                && day is >= 1 and <= 31
                && hour <= 24;
        }

        return false;
    }

    private static bool IsValidDate(ReadOnlySpan<char> date, ReadOnlySpan<Range> parts)
    {
        Debug.Assert(parts.Length == 3);

        if (
            !int.TryParse(date[parts[0]], out int year)
            || !byte.TryParse(date[parts[1]], out byte month)
            || !byte.TryParse(date[parts[2]], out byte day)
        )
        {
            return false;
        }

        return year is >= 0 and <= 9999 && month is >= 1 and <= 12 && day is >= 1 and <= 31;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static ReadOnlySpan<Range> Split(
        ReadOnlySpan<char> str,
        Span<Range> destination,
        char separator
    )
    {
        int length = str.Split(destination, separator);
        return destination[..length];
    }
}
