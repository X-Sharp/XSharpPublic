//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.ObjectModel
USING System.Collections.Generic
USING System.Collections.Concurrent
BEGIN NAMESPACE XSharp.Settings

    /// <summary>
    /// Special class that has check inside Add to prevent duplicate key exceptions
    /// </summary>
    /// <typeparam name="TKey"></typeparam>
    /// <typeparam name="TValue"></typeparam>
    CLASS XDictionary<TKey, TValue> INHERIT ConcurrentDictionary<TKey,TValue>

        CONSTRUCTOR()
            SUPER()
        CONSTRUCTOR (comparer as IEqualityComparer<TKey>)
            SUPER(comparer)
        /// <summary>
        /// Add item to collection. If an item with the key already exists then nothing is added
        /// </summary>
        /// <param name="key"></param>
        /// <param name="value"></param>
        METHOD Add( key as TKey, @@value as TValue) AS VOID
            IF !SELF:ContainsKey(key)
                SUPER:TryAdd(key, @@value)
            ENDIF
        METHOD Append(oDict as IDictionary<TKey, TValue>) AS VOID
            FOREACH var item in oDict
                SELF:Add(item:Key, item:Value)
            NEXT
        METHOD Remove(key as TKey) AS VOID
            SUPER:TryRemove(key, out var _)

        METHOD ToReadOnlyDictionary() AS IDictionary<TKey, TValue>
            RETURN ReadOnlyDictionary<TKey, TValue>{SELF}

    END CLASS
END NAMESPACE
