//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="VFPRuntimeDocs.xml" path="Runtimefunctions/addproperty/*" />
FUNCTION AddProperty (oObjectName, cPropertyName, eNewValue )
    IF IsObject(oObjectName) .and. IsString(cPropertyName)
        Send(oObjectName,#AddProperty,cPropertyName, eNewValue)
    ENDIF
    RETURN FALSE


/// <include file="VFPRuntimeDocs.xml" path="Runtimefunctions/removeproperty/*" />
FUNCTION RemoveProperty( oObjectName, cPropertyName ) AS LOGIC
    IF IsObject(oObjectName) .and. IsString(cPropertyName)
        Send(oObjectName,#RemoveProperty,cPropertyName)
    ENDIF
    RETURN FALSE

