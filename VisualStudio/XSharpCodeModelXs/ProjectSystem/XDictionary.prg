//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
BEGIN NAMESPACE XSharpModel

    /// <summary>
    /// Special class that has check inside Add to prevent duplicate key exceptions
    /// </summary>
    /// <typeparam name="TKey"></typeparam>
    /// <typeparam name="TValue"></typeparam>
    CLASS XDictionary<TKey, TValue> INHERIT Dictionary<TKey,TValue>

        CONSTRUCTOR()
            SUPER()
        CONSTRUCTOR (comparer as IEqualityComparer<TKey>)
            SUPER(comparer)
        /// <summary>
        /// Add item to collection. If an item with the key already exists then nothing is added
        /// </summary>
        /// <param name="key"></param>
        /// <param name="value"></param>
        NEW METHOD Add( key as TKey, @@value as TValue) AS VOID
            IF !SELF:ContainsKey(key)
                SUPER:Add(key, @@value)
            ENDIF


    END CLASS
END NAMESPACE
