//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/iscolor/*" />
FUNCTION IsColor( ) AS LOGIC
    RETURN TRUE

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ismouse/*" />
FUNCTION IsMouse( ) AS LOGIC
    RETURN TRUE

/// <include file="VFPDocs.xml" path="Runtimefunctions/nvl/*" />
FUNCTION NVL( eExpression1, eExpression2) AS USUAL CLIPPER
    IF IsNil(eExpression1)
        RETURN eExpression2
    ENDIF
    RETURN eExpression1


/// <include file="VFPDocs.xml" path="Runtimefunctions/evl/*" />
FUNCTION EVL( eExpression1 AS USUAL, eExpression2  AS USUAL) AS USUAL
    IF ! Empty(eExpression1)
        RETURN eExpression1
    ENDIF
    RETURN eExpression2
