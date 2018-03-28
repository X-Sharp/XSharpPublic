//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System
using System.Collections.Generic
BEGIN NAMESPACE XSharpModel

    STATIC CLASS DictionaryExtensions
        STATIC METHOD AddUnique<TKey, TValue>( SELF dict AS Dictionary<TKey, TValue>, key AS TKey, value AS TValue) AS TValue 
            IF dict != null .AND. key != null
                IF ! dict:ContainsKey(key)
                    dict:Add(key, value)
                    RETURN value
                ENDIF
                RETURN dict:Item[key]
            ENDIF
            RETURN DEFAULT (TValue)

    END CLASS

END NAMESPACE 

