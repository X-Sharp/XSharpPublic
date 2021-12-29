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
USING XSharp.RDD
USING XSharp.RDD.Support
USING System.Collections.Generic

PROCEDURE RegisterFoxMemVarSupport AS VOID INIT3
    // make sure the class constructor gets called
    VAR x := XSharp.MemVar{"InitTheClass",42}
    x:Value := 43   // assign a value to suppress the warning about an unused var
    XSharp.MemVar.Put := __FoxMemVarPut
    XSharp.RuntimeState.AutoLock    := __FoxAutoLock
    XSharp.RuntimeState.AutoUnLock  := __FoxAutoUnLock
RETURN


// Automatically lock a record in the FoxPro dialect
FUNCTION __FoxAutoLock() AS VOID
    LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
    IF oRdd:Shared
        VAR locked := oRdd:RecInfo(DBRI_LOCKED, NULL, NULL)
        IF locked IS LOGIC var lIsLocked .and. ! lIsLocked
            VAR lockInfo    := DbLockInfo{}
            lockInfo:Method := DbLockInfo.LockMethod.Exclusive
            lockInfo:RecId  := oRdd:RecNo
            oRdd:Lock(lockInfo)
            IF ! lockInfo:Result
                NOP
            ENDIF
        ENDIF
    ENDIF
    RETURN

// Automatically unlock a record in the FoxPro dialect
FUNCTION __FoxAutoUnLock() AS VOID
    LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
    IF oRdd:Shared
        oRdd:UnLock(oRdd:RecNo)
    ENDIF
    RETURN


INTERNAL FUNCTION __FoxMemVarPut(cName AS STRING, uValue AS USUAL) AS USUAL
    VAR current :=  XSharp.MemVar.GetSafe(cName)
    IF XSharp.RuntimeState.CompilerOptionFox2
        IF current IS __FoxArray .AND. ! uValue IS __FoxArray
            RETURN __FoxFillArray(current, uValue )
        ENDIF
    ENDIF
    RETURN XSharp.MemVar._Put(cName, uValue)


FUNCTION __FoxAssign(uLHS AS USUAL, uValue AS USUAL) AS USUAL
    IF uLHS IS __FoxArray VAR aFoxArray
        IF uValue IS __FoxArray
            RETURN uValue
        ELSEIF uValue IS __Array VAR aValue
            aFoxArray:__AssignFrom(aValue)
            RETURN aFoxArray
        ELSE
           RETURN __FoxFillArray(uLHS, uValue )
        ENDIF
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
    IF uValue IS  __FoxArray VAR fa .and. IsNumeric(nIndex1) .and. IsNumeric(nIndex2)
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
    IF uValue IS  __FoxArray VAR fa .and. IsNumeric(nIndex1)
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
    var error := Error{__VfpStr(VFPErrors.WITH_STACK_EMPTY)}
    error:Gencode := EG_NULLVAR
    error:FuncSym := ProcName(1)
    error:StackTrace := ErrorStack(1)
    THROW error

FUNCTION __FoxGetWithExpression() AS OBJECT
    LOCAL stack := __GetFoxWithStack() as Stack<OBJECT>
    if stack:Count > 0
        return stack:Peek()
    ENDIF
    var error := Error{__VfpStr(VFPErrors.WITH_STACK_EMPTY)}
    error:Gencode := EG_NULLVAR
    error:FuncSym := ProcName(1)
    error:StackTrace := ErrorStack(1)
    THROW error
