//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Collections.Immutable;
namespace XSharpModel
{
    public static class ListExtensions
    {
        public static void AddUnique(this List<String> list, String item)
        {
            // Check if it already exist
            if (!list.Contains(item,StringComparer.OrdinalIgnoreCase))
                list.Add(item);
        }

        public static IReadOnlyList<string> Expanded( this IEnumerable<String> source)
        {
            var result = new List<String>();
            result.AddRange(source);
            foreach (string s in source)
            {
                string part = s;
                while (part.Contains("."))
                {
                    part = part.Substring(0, part.LastIndexOf("."));
                    if (!result.Contains(part))
                    {
                        result.Add(part);
                    }
                }

            }
            return result.ToImmutableList();
        }
    }

    public static class DictionaryExtensions
    {
        public static TValue AddUnique<TKey, TValue>(this Dictionary<TKey, TValue> dict, TKey key, TValue value)
        {
            // Check if it already exist
            if (dict != null && key != null)
            {
                if (!dict.ContainsKey(key))
                {
                    dict.Add(key, value);
                    return value;
                }
                //
                return dict[key];
            }
            return default(TValue);
        }
    }
    public static class TypeExtensions
    {
        static IDictionary<String, String> lookupTable;
        static TypeExtensions()
        {
            lookupTable = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            lookupTable.Add("System.Boolean", "LOGIC");
            lookupTable.Add("System.Byte", "BYTE");
            lookupTable.Add("System.String", "STRING");
            lookupTable.Add("System.Char", "CHAR");
            lookupTable.Add("System.Double", "REAL8");
            lookupTable.Add("System.Int16", "SHORT");
            lookupTable.Add("System.Int32", "INT");
            lookupTable.Add("System.Int64", "INT64");
            lookupTable.Add("System.Object", "OBJECT");
            lookupTable.Add("System.Single", "REAL4");
            lookupTable.Add("System.UInt16", "WORD");
            lookupTable.Add("System.UInt32", "DWORD");
            lookupTable.Add("System.UInt64", "UINT64");
            lookupTable.Add("System.Void", "VOID");
            lookupTable.Add("Vulcan._CodeBlock", "CODEBLOCK");
            lookupTable.Add("Vulcan.__Array", "UINT64");
            lookupTable.Add("Vulcan.__Psz", "PSZ");
            lookupTable.Add("Vulcan.__Symbol", "SYMBOL");
            lookupTable.Add("Vulcan.__Usual", "USUAL");
            lookupTable.Add("Vulcan.__VODate", "DATE");
            lookupTable.Add("Vulcan.__VOFloat", "FLOAT");
            lookupTable.Add("Vulcan.__WinBool", "LOGIC");
            lookupTable = lookupTable.ToImmutableDictionary();
        }

        public static string GetXSharpTypeName(this System.Type type)
        {
            string name = type.FullName;
            if (name == null)
                name = type.Name;
            string suffix = "";
            if (name.EndsWith("[]"))
            {
                name = name.Substring(0, name.Length - 2);
                suffix = "[]";
            }
            if (name.EndsWith("&"))
            {
                name = name.Substring(0, name.Length - 1);
                suffix = "";
            }
            if (lookupTable.ContainsKey(name))
                name = lookupTable[name];
            return name+suffix;
        }

        public static string GetSystemTypeName(this String typename)
        {
            switch (typename.ToLower())
            {
                case "array":
                    return "Vulcan.__Array";
                case "date":
                    return "Vulcan.__VODate";
                case "float":
                    return "Vulcan.__VOFloat";
                case "psz":
                    return "Vulcan.__Psz";
                case "symbol":
                    return "Vulcan.__Symbol";
                case "usual":
                    return "Vulcan.__Usual";
            }
            return typename;
        }

    }
    public class Support
    {
        public static void Debug(string msg, params object[] o)
        {
#if DEBUG
            if (System.Diagnostics.Debugger.IsAttached)
                System.Diagnostics.Debug.WriteLine(String.Format("XModel: " + msg, o));
#endif
        }
    }
}
