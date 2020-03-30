//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Please note that the VFP Docs speak about "Statement Handles" for connections and also 
// about "statement handles" for statements. These are returned for example by SqlPrepare
// In this code we speak about "connection handles" and "statement handles".



USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.Data
USING System.Data.Common

INTERNAL ENUM XSharp.VFP.HandleType
    MEMBER Connection
    MEMBER Statement
END ENUM

INTERNAL CLASS XSharp.VFP.HandleCacheElement
    INTERNAL Type       AS HandleType
    INTERNAL Number     AS LONG
    INTERNAL @@Value    AS OBJECT
END CLASS

INTERNAL STATIC CLASS XSharp.VFP.SQLSupport
    STATIC INTERNAL Cache AS Dictionary <LONG, HandleCacheElement>
    STATIC INTERNAL UniqueId AS LONG
    STATIC INTERNAL Factory AS ISqlFactory
    STATIC CONSTRUCTOR
        Cache    := Dictionary<LONG, HandleCacheElement>{}
        UniqueId := 0
        Factory  := XSharp.Data.Functions.GetSqlFactory()
        
    STATIC METHOD FindElementOfType(nId AS LONG, nType AS HandleType) AS OBJECT
        IF Cache:ContainsKey(nId)
            VAR element := Cache[nId]
            IF element:Type == nType
                RETURN element:Value
            ENDIF
        ENDIF
        RETURN NULL
    
    STATIC METHOD FindConnection(nId AS LONG) AS XSharp.VFP.SQLConnection
        VAR result := FindElementOfType(nId, HandleType.Connection)
        RETURN (XSharp.VFP.SQLConnection) result
        
    STATIC METHOD FindStatement(nId AS LONG) AS OBJECT
        VAR result := FindElementOfType(nId, HandleType.Statement)
        RETURN result
        
    STATIC METHOD AddObject(oObject AS OBJECT, nType AS HandleType) AS LONG
        VAR element := HandleCacheElement{}
        element:Number := ++UniqueId
        element:Type   := nType
        element:Value  := oObject
        Cache:Add(element:Number, element)
        RETURN element:Number
        
    STATIC METHOD AddConnection(oConnection AS XSharp.VFP.SQLConnection) AS LONG
        RETURN AddObject(oConnection, HandleType.Connection)

    STATIC METHOD AddStatement(oConnection AS OBJECT) AS LONG
        RETURN AddObject(oConnection, HandleType.Statement)

    STATIC METHOD RemoveObject(nId AS LONG) AS LOGIC
        IF Cache:ContainsKey(nId)
            VAR element := Cache[nId]
            Cache:Remove(nId)
            IF element:Value IS IDisposable VAR disp
                disp:Dispose()
            ENDIF
            RETURN TRUE
        ENDIF
        RETURN FALSE


   



END CLASS

