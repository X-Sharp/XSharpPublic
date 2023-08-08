//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD.SqlRDD

/// <summary>
/// The SqlRddEvent class.
/// </summary>
CLASS SqlRddEventArgs
    PROPERTY Reason as SqlRDDEventReason AUTO
    PROPERTY Value  as Object AUTO
    PROPERTY Table  as String Auto
    Constructor( nReason as SqlRDDEventReason, cTable as string, oValue as Object)
        SELF:Reason := nReason
        SELF:Value  := oValue
        SELF:Table  := cTable
        RETURN
    PROPERTY StringValue as STRING
        GET
            IF SELF:Value IS String var strValue
                RETURN strValue
            ENDIF
            RETURN NULL
        END GET
    END PROPERTY
    PROPERTY IntValue as LONG
        GET
            IF SELF:Value IS Long var intValue
                RETURN intValue
            ENDIF
            RETURN 0
        END GET
    END PROPERTY
    PROPERTY ListValue as List<String>
        GET
            IF SELF:Value IS List<String> var listValue
                RETURN listValue
            ENDIF
            RETURN NULL
        END GET
    END PROPERTY

END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD.SupportClasses
