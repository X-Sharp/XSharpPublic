//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics

/// <summary>This collection is used to store additional properties for fields and servers
/// such as captions, descriptions etc.<summary>
[DebuggerDisplay("Properties: {Count}")];
CLASS XSharp.RDD.PropertyCollection INHERIT Dictionary<DatabasePropertyType, OBJECT>
    CONSTRUCTOR()
        SUPER()

    NEW METHOD Add(key as DatabasePropertyType, val as OBJECT) AS VOID
        // Duplicate keys simply replace the existing value
        SELF[key] := val
        RETURN

    METHOD GetValue<T> (key as DatabasePropertyType) AS T
        IF SELF:ContainsKey(key)
            return (T) SELF[key]
        ENDIF
        RETURN default(T)
END CLASS
