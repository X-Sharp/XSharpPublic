//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// functions used by the compiler. These functions allow the assignment to an array to be interpreted as an array fill.


USING XSharp.Internal

PROCEDURE RegisterFoxMemVarSupport AS VOID INIT3
    VAR x := XSharp.MemVar{"InitTheClass",0}
    x:Value := 42 // make sure the class constructor gets called
    XSharp.MemVar.Put := __FoxMemVarPut
    RETURN

INTERNAL FUNCTION __FoxMemVarPut(cName AS STRING, uValue AS USUAL) AS USUAL
    VAR current :=  XSharp.MemVar.GetSafe(cName)
    IF current IS __FoxArray .AND. ! uValue IS __FoxArray
        RETURN __FoxFillArray(current, uValue )
    ELSE
        RETURN XSharp.MemVar._Put(cName, uValue)
    ENDIF


[NeedsAccessToLocals(TRUE)];
FUNCTION __FoxAssign(uLHS AS USUAL, uValue AS USUAL) AS USUAL
    IF uLHS IS __FoxArray .AND. ! uValue IS __FoxArray
        RETURN __FoxFillArray(uLHS, uValue )
    ELSE
        RETURN uValue
    ENDIF




FUNCTION __FoxFillArray(uArray AS USUAL, uValue AS USUAL) AS USUAL
    IF IsArray(uArray) .AND. ! IsArray(uValue)
        LOCAL oldArray := uArray AS ARRAY
        IF oldArray IS __FoxArray VAR foxArray
            foxArray:__Fill(uValue)
            RETURN foxArray
        ENDIF
    ENDIF
    RETURN uArray

FUNCTION __FoxRedim(uCurrent AS USUAL, nRows AS DWORD, nCols := 0 AS DWORD) AS __FoxArray
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
