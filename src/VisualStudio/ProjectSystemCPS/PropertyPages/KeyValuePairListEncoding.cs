//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Collections.Generic;
using System.Linq;

namespace XSharp.ProjectSystem.PropertyPages
{
    /// <summary>
    /// The value format of the property editors' name/value lists (MultiStringSelector, NameValueList):
    /// "name1=value1,name2=value2", with '/' escaping '/', ',' and '='.
    /// </summary>
    /// <remarks>
    /// The same format as KeyValuePairListEncoding of the managed project system, which is internal to it.
    /// </remarks>
    internal static class KeyValuePairListEncoding
    {
        public static IEnumerable<(string Name, string Value)> Parse(string input)
        {
            if (string.IsNullOrWhiteSpace(input))
                yield break;
            foreach (var entry in ReadEntries(input))
            {
                var (encodedName, encodedValue) = SplitEntry(entry);
                var name = Decode(encodedName);
                if (!string.IsNullOrEmpty(name))
                    yield return (name, Decode(encodedValue));
            }
        }

        public static string Format(IEnumerable<(string Name, string Value)> pairs) =>
            string.Join(",", pairs.Select(pair => Encode(pair.Name) + "=" + Encode(pair.Value)));

        private static string Encode(string value) => value.Replace("/", "//").Replace(",", "/,").Replace("=", "/=");

        private static string Decode(string value) => value.Replace("/=", "=").Replace("/,", ",").Replace("//", "/");

        private static IEnumerable<string> ReadEntries(string text)
        {
            bool escaped = false;
            int start = 0;
            for (int i = 0; i < text.Length; i++)
            {
                if (text[i] == ',' && !escaped)
                {
                    yield return text.Substring(start, i - start);
                    start = i + 1;
                    escaped = false;
                }
                else
                {
                    escaped = text[i] == '/' && !escaped;
                }
            }
            yield return text.Substring(start);
        }

        private static (string Name, string Value) SplitEntry(string entry)
        {
            bool escaped = false;
            for (int i = 0; i < entry.Length; i++)
            {
                if (entry[i] == '=' && !escaped)
                    return (entry.Substring(0, i), entry.Substring(i + 1));
                escaped = entry[i] == '/' && !escaped;
            }
            return (string.Empty, string.Empty);
        }
    }
}
