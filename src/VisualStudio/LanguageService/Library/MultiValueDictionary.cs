//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/*****************************************************************************
* Based on IronStudio/IronPythonTools/IronPythonTools/Navigation
****************************************************************************/


using System.Collections.Generic;
using System.Collections.Concurrent;

namespace XSharp.LanguageService
{
    // In Dictionary we have one key for one value
    // Here we have one key for multiple values
    public class MultiValueDictionary<TKey, TValue> : ConcurrentDictionary<TKey, HashSet<TValue>>
    {
        // Add an element, associated with a key
        public void Add(TKey key, TValue value)
        {
            //
            // Try to get an existing HashSet
            if (!this.TryGetValue(key, out var values))
            {
                // No ? Create a new group
                values = new HashSet<TValue>();
                // and add it with the key
                base.TryAdd(key, values);
            }
            values.Add(value);
        }

        
        // Check if the value already exist with that ket
        public bool ContainsValue(TKey key, TValue value)
        {
            bool result = false;

            // Try to get an existing HashSet
            if (TryGetValue(key, out var values))
            {
                // Ok, check if that value already exist
                result = values.Contains(value);
            }
            return result;
        }


        public void Remove(TKey key, TValue value)
        {
            // Try to get an existing HashSet
            if (this.TryGetValue(key, out var values))
            {
                // Ok, check try to remove that value if present 
                values.Remove(value);
                // Empty HashSet ?
                if (values.Count <= 0)
                {
                    // Remove the HashSet and the Key
                    this.TryRemove(key, out var _);
                }
            }
        }

    }

}
