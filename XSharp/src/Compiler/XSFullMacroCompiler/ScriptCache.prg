//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Collections.Concurrent
USING System.Text
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.CodeAnalysis.Scripting
USING LanguageService.CodeAnalysis.XSharp.Scripting
USING XSharp

/// <summary>
/// The SriptCache class.
/// </summary>
INTERNAL CLASS ScriptCache INHERIT ConcurrentDictionary<STRING,Script>
    PRIVATE _keys AS ConcurrentQueue<STRING>
    PRIVATE _maxItems AS INT
 
PUBLIC CONSTRUCTOR(maxItems AS INT)
    _keys := ConcurrentQueue<STRING>{}
    _maxItems := maxItems
    RETURN

NEW PUBLIC METHOD TryAdd(key as STRING, value AS Script) AS LOGIC
    VAR res := SUPER:TryAdd(key, value)
    IF res
        _keys:Enqueue(key)
    END
    DO WHILE _keys:Count > _maxItems
        LOCAL oldKey := NULL AS STRING
        IF _keys:TryDequeue(OUT oldKey)
            SUPER:TryRemove(oldKey, OUT VAR _)
        ENDIF
    END
    RETURN res

END CLASS
