//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharp.Internal


FUNCTION __FoxFillArray(uArray AS USUAL, uValue AS USUAL) AS USUAL
    IF IsArray(uArray)
        LOCAL oldArray := uArray AS ARRAY
        IF oldArray IS __FoxArray VAR foxArray
            foxArray:__Fill(uValue)
        ENDIF
    ENDIF
    RETURN uArray

FUNCTION __FoxRedim(uCurrent AS USUAL, nRows AS DWORD, nCols := 1 AS DWORD) AS __FoxArray
    LOCAL result := NULL AS __FoxArray
    IF IsArray(uCurrent)
        LOCAL oldArray := uCurrent AS ARRAY
        IF oldArray IS __FoxArray VAR foxArray
            result := foxArray:ReDim(nRows, nCols)
        ENDIF
    ENDIF
    IF result == NULL
        result := __FoxArray{nRows, nCols}
    ENDIF
    RETURN result


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/alen/*" />
FUNCTION ALen(a AS __FoxArray, nArrayAttribute AS LONG) AS DWORD
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
FUNCTION ALen(a AS __FoxArray) AS DWORD
    RETURN ALen(a, 0)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/aelement/*" />
FUNCTION AElement(ArrayName AS __FoxArray, nRowSubscript AS DWORD, nColumnSubscript := 1 AS DWORD) AS USUAL
    IF ArrayName:MultiDimensional
        RETURN ArrayName[nRowSubscript, nColumnSubscript]
    ELSEIF nColumnSubscript == 1
        RETURN ArrayName[nRowSubscript]
    ELSE
        THROW ArrayName:__GetDimensionError(2)
    ENDIF

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/adel/*" />
FUNCTION ADel(ArrayName AS __FoxArray, nElementNumber AS DWORD, nDeleteType := 2 AS LONG) AS DWORD
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
FUNCTION ASubScript(ArrayName AS __FoxArray, nElementNumber AS DWORD, nSubscript := 1 AS DWORD) AS DWORD
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
FUNCTION AIns(ArrayName AS __FoxArray, nElementNumber AS DWORD, nInsertType := 1 AS DWORD) AS DWORD
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
FUNCTION ASize(ArrayName AS __FoxArray, nSize AS DWORD) AS __FoxArray
    ArrayName:Resize((LONG) nSize)
    RETURN ArrayName


