//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.Text;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Tagging;
using System;
using System.Collections.Generic;

namespace XSharpModel
{
    public static class ListExtensions
    {
        public static void AddUnique<T>(this List<T> list, T item)
        {
            // Check if it already exist
            if (!list.Contains(item))
                list.Add(item);
        }
    }

    public static class DictionaryExtensions
    {
        public static TValue AddUnique<TKey, TValue>(this Dictionary<TKey, TValue> dict, TKey key, TValue value)
        {
            // Check if it already exist
            if (!dict.ContainsKey(key))
                dict.Add(key, value);
            //
            return value;
        }
    }
}
