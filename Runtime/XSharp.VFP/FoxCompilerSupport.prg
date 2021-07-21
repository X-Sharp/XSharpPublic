//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// functions used by the compiler. These functions allow the assignment to an array to be interpreted as an array fill.


USING XSharp.Internal


[NeedsAccessToLocals(TRUE)];
FUNCTION __FoxMemVarPut(cName AS STRING, uValue AS USUAL) AS USUAL
    VAR current :=  __MemVarGetSafe(cName)
    IF current IS __FoxArray .AND. ! uValue IS __FoxArray
        RETURN __FoxFillArray(current, uValue )
    ELSE
        RETURN XSharp.MemVar.Put(cName, uValue)
    ENDIF


[NeedsAccessToLocals(TRUE)];
FUNCTION __FoxAssign(uLHS AS USUAL, uValue AS USUAL) AS USUAL
    IF uLHS IS __FoxArray .AND. ! uValue IS __FoxArray
        RETURN __FoxFillArray(uLHS, uValue )
    ELSE
        RETURN uValue
    ENDIF



[NeedsAccessToLocals(TRUE)];
FUNCTION __FoxVarPut(cName AS STRING, uValue AS USUAL) AS USUAL
    IF FieldPos(cName) > 0
        RETURN __FieldSet(cName, uValue)
    ENDIF
    RETURN __FoxMemVarPut(cName, uValue)



FUNCTION __FoxFieldSetWa( area AS USUAL, fieldName AS STRING, uValue AS USUAL ) AS USUAL
    IF IsNil(area)
        RETURN __FieldSet(fieldName,uValue)
    ENDIF
    IF IsString(area) .AND. ((STRING) area):ToUpper() == "M"
        RETURN __FoxMemVarPut(fieldName,uValue)
    ENDIF
    IF IsSymbol(area) .AND. ((STRING) area):ToUpper() == "M"
        RETURN __FoxMemVarPut(fieldName,uValue)
    ENDIF
    LOCAL curArea := RuntimeState.CurrentWorkarea AS DWORD
    LOCAL newArea := _Select( area ) AS DWORD
    IF newArea > 0
        RuntimeState.CurrentWorkarea := newArea
        TRY
            __FieldSet( fieldName, uValue )
        FINALLY
            RuntimeState.CurrentWorkarea := curArea
        END TRY
    ELSE
        THROW Error.VoDbError( EG_ARG, EDB_BADALIAS, __FUNCTION__, nameof(area),1, <OBJECT>{area}  )
    ENDIF
    // Note: must return the same value passed in, to allow chained assignment expressions
    RETURN uValue


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
