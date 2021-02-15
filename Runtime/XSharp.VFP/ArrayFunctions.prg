//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


FUNCTION FoxArrayCreate(nRows as DWORD, nCols := 1 as DWORD) AS __FoxArray
    RETURN __FoxArray{nRows , nCols}


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/alen/*" />
FUNCTION ALen(a as __FoxArray, nArrayAttribute as LONG) AS DWORD
    SWITCH nArrayAttribute
    CASE 1
        RETURN (DWORD) a:Rows
    CASE 2
        IF a:MultiDimensional
            RETURN (DWORD) a:Columns
        ELSE
            RETURN 0
        ENDIF
    OTHERWISE      
        RETURN (DWORD) a:Count
    END SWITCH


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/alen/*" />
FUNCTION ALen(a as __FoxArray) AS DWORD
    RETURN ALen(a, 0)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/aelement/*" />
FUNCTION AElement(ArrayName as __FoxArray, nRowSubscript as DWORD, nColumnSubscript := 1 as DWORD) AS USUAL
    IF ArrayName:MultiDimensional
        RETURN ArrayName[nRowSubscript, nColumnSubscript]
    ELSEIF nColumnSubscript == 1
        RETURN ArrayName[nRowSubscript]
    ELSE
        THROW ArrayName:__GetDimensionError(2)
    ENDIF

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/adel/*" />
FUNCTION ADel(ArrayName as __FoxArray, nElementNumber as DWORD, nDeleteType := 2 as LONG) AS DWORD
    IF ! ArrayName:MultiDimensional
        ArrayName:Delete((LONG) nElementNumber)
    ELSE
        IF nDeleteType == 2
            ArrayName:DeleteColumn( (LONG) nElementNumber)
        ELSE
            ArrayName:DeleteRow((LONG) nElementNumber)
        ENDIF
    ENDIF
    RETURN 1


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/asubscript/*" />
FUNCTION ASubScript(ArrayName as __FoxArray, nElementNumber AS DWORD, nSubscript := 1 as DWORD) AS DWORD
    IF !ArrayName:MultiDimensional
        IF nSubscript == 1 .and. nElementNumber <= (DWORD) ArrayName:Count
            RETURN nElementNumber
        ENDIF
    ELSE
       IF nElementNumber <= ArrayName:Rows * ArrayName:Columns
            // assume an array of 5 * 3
            // element 3 should be on row 1, column 3
            // element 4 should be on row 2, column 1
            // max element = 15
           IF nSubscript == 1
                // calculate the row
                RETURN (DWORD) ArrayName:GetRow((LONG) nElementNumber)
           ELSE
                // calculate the column
                RETURN (DWORD) ArrayName:GetColumn((LONG) nElementNumber)
           ENDIF
        ENDIF
    ENDIF
    RETURN 0

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ains/*" />
FUNCTION AIns(ArrayName as __FoxArray, nElementNumber as DWORD, nInsertType := 1 as DWORD) AS DWORD
    IF !ArrayName:MultiDimensional
        IF nInsertType == 1 .and. nElementNumber <= (DWORD) ArrayName:Count
            ArrayName:Insert((LONG) nElementNumber)
        ENDIF
    ELSE
        IF nInsertType == 2
            ArrayName:InsertColumn( (LONG) nElementNumber)
        ELSE
            ArrayName:InsertRow((LONG) nElementNumber)
        ENDIF
    ENDIF
    RETURN 0
    
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/asize/*" />
FUNCTION ASize(ArrayName as __FoxArray, nSize as DWORD) AS __FoxArray
    ArrayName:Resize((LONG) nSize)
    RETURN ArrayName


FUNCTION __FoxRedim(a as __FoxArray, nRows as INT, nCols := 1 as INT) AS __FoxArray
    a:ReDim(nRows, nCols)
    RETURN a
