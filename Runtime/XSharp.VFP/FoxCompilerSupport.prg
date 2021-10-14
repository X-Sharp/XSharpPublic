//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// functions used by the compiler.
// These functions allow the assignment to an array to be interpreted as an array fill.
// This also contains the push / pop code to allow access to WITH variables outside of the
// function where they are declared.


USING XSharp.Internal
USING System.Collections.Generic

PROCEDURE RegisterFoxMemVarSupport AS VOID INIT3
    VAR x := XSharp.MemVar{"InitTheClass",0}
    x:Value := 42 // make sure the class constructor gets called
    XSharp.MemVar.Put := __FoxMemVarPut
    RETURN

INTERNAL FUNCTION __FoxMemVarPut(cName AS STRING, uValue AS USUAL) AS USUAL
    VAR current :=  XSharp.MemVar.GetSafe(cName)
    IF XSharp.RuntimeState.CompilerOptionFox2
        IF current IS __FoxArray .AND. ! uValue IS __FoxArray
            RETURN __FoxFillArray(current, uValue )
        ENDIF
    ENDIF
    RETURN XSharp.MemVar._Put(cName, uValue)


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


FUNCTION __FoxArrayAccess(cName AS STRING, uValue AS USUAL, nIndex1 AS USUAL, nIndex2 AS USUAL) AS USUAL
    IF uValue IS  __FoxArray VAR fa
        RETURN fa[nIndex1, nIndex2]
    ENDIF
    IF _HasClipFunc(cName)
        RETURN _CallClipFunc(cName, {nIndex1, nIndex2})
    ENDIF
    IF XSharp.MemVar.TryGet(cName, OUT VAR _)
        THROW Error{__VfpStr(VFPErrors.VARIABLE_NOT_ARRAY, cName)}
    ELSE
        THROW Error{__VfpStr(VFPErrors.VARIABLE_DOES_NOT_EXIST, cName)}
    ENDIF



FUNCTION __FoxArrayAccess(cName AS STRING, uValue AS USUAL, nIndex1 AS USUAL) AS USUAL
    IF uValue IS  __FoxArray VAR fa
        RETURN fa[nIndex1]
    ENDIF
    IF _HasClipFunc(cName)
        RETURN _CallClipFunc(cName, {nIndex1})
    ENDIF
    IF XSharp.MemVar.TryGet(cName, OUT VAR _)
        THROW Error{__VfpStr(VFPErrors.VARIABLE_NOT_ARRAY, cName)}
    ELSE
        THROW Error{__VfpStr(VFPErrors.VARIABLE_DOES_NOT_EXIST, cName)}
    ENDIF

INTERNAL FUNCTION __GetFoxWithStack() AS Stack<OBJECT>
    LOCAL stack as Stack<OBJECT>
    stack := XSharp.RuntimeState.GetValue<Stack<Object>>(Set.WithStack)
    if stack == NULL
        stack := Stack<OBJECT>{}
        XSharp.RuntimeState.SetValue(Set.WithStack, stack)
    ENDIF
    return stack

FUNCTION __FoxPushWithBlock(oVar as OBJECT) AS VOID
    LOCAL stack := __GetFoxWithStack() as Stack<OBJECT>
    stack:Push(oVar)
    RETURN
FUNCTION __FoxPopWithBlock() AS OBJECT
    LOCAL stack := __GetFoxWithStack() as Stack<OBJECT>
    if stack:Count > 0
        return stack:Pop()
    ENDIF
    THROW Error{__VfpStr(VFPErrors.WITH_STACK_EMPTY)}
FUNCTION __FoxGetWithExpression() AS OBJECT
    LOCAL stack := __GetFoxWithStack() as Stack<OBJECT>
    if stack:Count > 0
        return stack:Peek()
    ENDIF
    THROW Error{__VfpStr(VFPErrors.WITH_STACK_EMPTY)}
