//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="VFPRuntimeDocs.xml" path="Runtimefunctions/addproperty/*" />
FUNCTION AddProperty (oObjectName AS OBJECT, cPropertyName AS STRING, eNewValue := NIL AS USUAL) AS LOGIC
    if (object) oObjectName is XSharp.IDynamicProperties2 var oDynamic
        oDynamic:_AddProperty(cPropertyName, eNewValue,1,"")
    else
        Send(oObjectName,#AddProperty,cPropertyName, eNewValue)
    endi
    return true


/// <include file="VFPRuntimeDocs.xml" path="Runtimefunctions/removeproperty/*" />
function RemoveProperty( oObjectName as object, cPropertyName as string) as logic
    if (object) oObjectName is XSharp.IDynamicProperties2 var oDynamic
        oDynamic:_RemoveProperty(cPropertyName)
    ELSE
        Send(oObjectName,#RemoveProperty,cPropertyName)
    ENDIF
    RETURN TRUE

