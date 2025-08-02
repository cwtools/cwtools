using System.Collections.Frozen;
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
}
