/*****************************************************************************
 *
 * Copyright(c) Microsoft Corporation.
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A
* copy of the license can be found in the License.html file at the root of this distribution.If
* you cannot locate the Apache License, Version 2.0, please send an email to
* ironpy@microsoft.com.By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Apache License, Version 2.0.
 *
 * You must not remove this notice, or any other, from this software.
*
****************************************************************************/
/*****************************************************************************
* XSharp.BV
* Based on IronStudio/IronPythonTools/IronPythonTools/Navigation
*
****************************************************************************/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Collections;
using System.Runtime.Serialization;

namespace XSharp.Project
{
    // In Dictionary we have one key for one value
    // Here we have one key for multiple values
    public class MultiValueDictionary<TKey, TValue> : Dictionary<TKey, HashSet<TValue>>
    {

        // Add an element, associated with a key
        public void Add(TKey key, TValue value)
        {
            //
            HashSet<TValue> values = null;
            // Try to get an existing HashSet
            if (!this.TryGetValue(key, out values))
            {
                // No ? Create a new group
                values = new HashSet<TValue>();
                // and add it with the key
                base.Add(key, values);
            }
            values.Add(value);
        }

        
        // Check if the value already exist with that ket
        public bool ContainsValue(TKey key, TValue value)
        {
            bool result = false;
            HashSet<TValue> values = null;
            // Try to get an existing HashSet
            if (this.TryGetValue(key, out values))
            {
                // Ok, check if that value already exist
                result = values.Contains(value);
            }
            return result;
        }


        public void Remove(TKey key, TValue value)
        {
            HashSet<TValue> values = null;
            // Try to get an existing HashSet
            if (this.TryGetValue(key, out values))
            {
                // Ok, check try to remove that value if present 
                values.Remove(value);
                // Empty HashSet ?
                if (values.Count <= 0)
                {
                    // Remove the HashSet and the Key
                    this.Remove(key);
                }
            }
        }

    }

}