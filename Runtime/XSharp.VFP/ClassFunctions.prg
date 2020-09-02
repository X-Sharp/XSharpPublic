//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="VFPRuntimeDocs.xml" path="Runtimefunctions/addproperty/*" />
FUNCTION AddProperty (oObjectName AS OBJECT, cPropertyName AS STRING, eNewValue := NIL AS USUAL) AS LOGIC
    if (object) oObjectName IS XSharp.VFP.Empty VAR oEmpty
        oEmpty:__AddProperty(cPropertyName, eNewValue)
    ELSE
        Send(oObjectName,#AddProperty,cPropertyName, eNewValue)
    ENDIF
    RETURN TRUE


/// <include file="VFPRuntimeDocs.xml" path="Runtimefunctions/removeproperty/*" />
FUNCTION RemoveProperty( oObjectName AS OBJECT, cPropertyName AS STRING) AS LOGIC
    if (object) oObjectName IS XSharp.VFP.Empty VAR oEmpty
        oEmpty:__RemoveProperty(cPropertyName)
    ELSE
        Send(oObjectName,#RemoveProperty,cPropertyName)
    ENDIF
    RETURN TRUE

