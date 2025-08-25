using System.Collections.Frozen;
using System.Diagnostics;
using Microsoft.FSharp.Core;

namespace CSharpHelpers;

public static class Extensions
{
    public static FSharpOption<TValue> TryFind<TKey, TValue>(
        this FrozenDictionary<TKey, TValue> dictionary,
        TKey key
    )
        where TKey : notnull
    {
        if (dictionary.TryGetValue(key, out var value))
        {
            return FSharpOption<TValue>.Some(value);
        }

        return FSharpOption<TValue>.None;
    }

    public static FSharpValueOption<TValue> TryFindV<TKey, TValue>(
        this FrozenDictionary<TKey, TValue> dictionary,
        TKey key
    )
        where TKey : notnull
    {
        if (dictionary.TryGetValue(key, out var value))
        {
            return FSharpValueOption<TValue>.Some(value);
        }

        return FSharpValueOption<TValue>.None;
    }

    public static ReadOnlySpan<char> Split(this ReadOnlySpan<char> span, char separator, int index)
    {
        var enumerator = span.Split(separator);
        int currentIndex = 0;
        while (enumerator.MoveNext())
        {
            if (currentIndex == index)
            {
                return span[enumerator.Current];
            }

            currentIndex++;
        }

        throw new ArgumentOutOfRangeException(
            nameof(index),
            "Index is out of range of the enumerator."
        );
    }

    public static ReadOnlySpan<char> SplitWithCount(this ReadOnlySpan<char> span, char separator, int index, int count)
    {
        Debug.Assert(count > 0);
        Debug.Assert(count > index);

        Span<Range> ranges = count > 64 ? new Range[count] : stackalloc Range[count];
        int total = span.Split(ranges, separator);
        if (total > index)
        {
            return span[ranges[index]];
        }

        throw new ArgumentOutOfRangeException(
            nameof(index),
            "Index is out of range of the enumerator."
        );
    }

    public static ReadOnlySpan<char> SplitFirst(this ReadOnlySpan<char> span, char separator)
    {
        var enumerator = span.Split(separator);
        if (enumerator.MoveNext())
        {
            return span[enumerator.Current];
        }

        throw new ArgumentOutOfRangeException(nameof(span), "enumerator is empty.");
    }

    public static Range Last(this MemoryExtensions.SpanSplitEnumerator<char> enumerator)
    {
        if (!enumerator.MoveNext())
        {
            throw new ArgumentOutOfRangeException(nameof(enumerator));
        }

        var range = enumerator.Current;
        while (enumerator.MoveNext())
        {
            range = enumerator.Current;
        }

        return range;
    }
}