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
USING System.Reflection
/// <exclude />
PROCEDURE RegisterFoxMemVarSupport AS VOID INIT3
    oVFPErrorMessage := __VfpStr
    XSharp.RuntimeState.DialectChanged += DialectChanged
    InitFoxState()
RETURN


INTERNAL FUNCTION InitFoxState() AS VOID
    // make sure the class constructor gets called
    #pragma warnings(219, off) // variable is assigned but not used
    VAR x := XSharp.MemVar{"InitTheClass",42}
    #pragma warnings(219, default)

    XSharp.MemVar.Put := __FoxMemVarPut
    XSharp.RuntimeState.AutoLock    := __FoxAutoLock
    XSharp.RuntimeState.AutoUnLock  := __FoxAutoUnLock

    XSharp.__Array.FoxArrayHelpers.ADel         := XSharp.VFP.Functions.ADel
    XSharp.__Array.FoxArrayHelpers.ALen         := XSharp.VFP.Functions.FoxALen
    XSharp.__Array.FoxArrayHelpers.AIns         := XSharp.VFP.Functions.FoxAIns
    XSharp.__Array.FoxArrayHelpers.ShowArray    := XSharp.VFP.Functions.ShowFoxArray
RETURN

INTERNAL FUNCTION ClearFoxState as VOID
    XSharp.MemVar.Initialize()
    XSharp.RuntimeState.AutoLock    := NULL
    XSharp.RuntimeState.AutoUnLock  := NULL
    XSharp.__Array.FoxArrayHelpers.Reset()



RETURN


/// <include file="XSharp.VFP.Docs.xml" path="doc/DialectChanged/*" />
INTERNAL FUNCTION DialectChanged(oldDialect as XSharpDialect, newDialect as XSharpDialect) AS VOID
IF oldDialect != newDialect
    if newDialect == XSharpDialect.FoxPro
        InitFoxState()
    else
        ClearFoxState()
    endif
ENDIF



// Automatically lock a record in the FoxPro dialect
/// <exclude />
FUNCTION __FoxAutoLock() AS VOID
    LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
    IF oRdd != null .and. oRdd:Shared
        VAR locked := oRdd:RecInfo(DBRI_LOCKED, NULL, NULL)
        IF locked IS LOGIC var lIsLocked .and. ! lIsLocked
            VAR lockInfo    := DbLockInfo{}
            lockInfo:Method := DbLockInfo.LockMethod.Exclusive
            lockInfo:RecId  := oRdd:RecNo
            oRdd:Lock(REF lockInfo)
            IF ! lockInfo:Result
                NOP
            ENDIF
        ENDIF
    ENDIF
    RETURN

// Automatically unlock a record in the FoxPro dialect
/// <exclude />
FUNCTION __FoxAutoUnLock() AS VOID
    LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
    IF oRdd != null .and. oRdd:Shared
        oRdd:UnLock(oRdd:RecNo)
    ENDIF
    RETURN


/// <include file="XSharp.VFP.Docs.xml" path="doc/__FoxMemVarPut/*" />
INTERNAL FUNCTION __FoxMemVarPut(cName AS STRING, uValue AS USUAL) AS USUAL
    VAR current :=  XSharp.MemVar.GetSafe(cName)
    IF XSharp.RuntimeState.CompilerOptionFox2
        IF current IS __FoxArray .AND. uValue IS NOT __FoxArray
            RETURN __FoxFillArray(current, uValue )
        ENDIF
    ENDIF
    RETURN XSharp.MemVar._Put(cName, uValue)

/// <include file="XSharp.VFP.Docs.xml" path="doc/__FoxAssign/*" />
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
/// <include file="XSharp.VFP.Docs.xml" path="doc/__FoxFillArray/*" />
FUNCTION __FoxFillArray(uArray AS USUAL, uValue AS USUAL) AS USUAL
    IF IsArray(uArray) .AND. ! IsArray(uValue)
        LOCAL oldArray := uArray AS ARRAY
        IF oldArray IS __FoxArray VAR foxArray
            foxArray:__Fill(uValue)
            RETURN foxArray
        ENDIF
    ENDIF
    RETURN uArray

/// <include file="XSharp.VFP.Docs.xml" path="doc/__FoxRedim/*" />
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

/// <include file="XSharp.VFP.Docs.xml" path="doc/__FoxArrayAccess/*" />
FUNCTION __FoxArrayAccess(cName AS STRING, uValue AS USUAL, nIndex1 AS USUAL, nIndex2 AS USUAL) AS USUAL
    IF uValue IS  __FoxArray VAR fa .and. IsNumeric(nIndex1) .and. IsNumeric(nIndex2) .and. fa:MultiDimensional
        RETURN fa[nIndex1, nIndex2]
    ENDIF
    IF _HasClipFunc(cName)
        RETURN _CallClipFunc(cName, {nIndex1, nIndex2})
    ENDIF
    IF XSharp.MemVar.TryGet(cName, OUT VAR _)
        THROW Error{__VfpStr(VFPErrors.VFP_VARIABLE_NOT_ARRAY, cName)}
    ELSE
        THROW Error{__VfpStr(VFPErrors.VFP_VARIABLE_DOES_NOT_EXIST, cName)}
    ENDIF

/// <include file="XSharp.VFP.Docs.xml" path="doc/__FoxArrayAccess_2/*" />
FUNCTION __FoxArrayAccess(cName AS STRING, uValue AS USUAL, nIndex1 AS USUAL) AS USUAL
    IF uValue IS  __FoxArray VAR fa .and. IsNumeric(nIndex1)
        RETURN fa[nIndex1]
    ENDIF
    IF _HasClipFunc(cName)
        RETURN _CallClipFunc(cName, {nIndex1})
    ENDIF
    IF XSharp.MemVar.TryGet(cName, OUT VAR _)
        THROW Error{__VfpStr(VFPErrors.VFP_VARIABLE_NOT_ARRAY, cName)}
    ELSE
        THROW Error{__VfpStr(VFPErrors.VFP_VARIABLE_DOES_NOT_EXIST, cName)}
    ENDIF


/// <include file="XSharp.VFP.Docs.xml" path="doc/__GetFoxWithStack/*" />
INTERNAL FUNCTION __GetFoxWithStack() AS Stack<OBJECT>
    LOCAL stack as Stack<OBJECT>
    stack := XSharp.RuntimeState.GetValue<Stack<Object>>(Set.WithStack)
    if stack == NULL
        stack := Stack<OBJECT>{}
        XSharp.RuntimeState.SetValue(Set.WithStack, stack)
    ENDIF
    return stack

/// <include file="XSharp.VFP.Docs.xml" path="doc/__FoxPushWithBlock/*" />
FUNCTION __FoxPushWithBlock(oVar as OBJECT) AS VOID
    LOCAL stack := __GetFoxWithStack() as Stack<OBJECT>
    stack:Push(oVar)
    RETURN

/// <include file="XSharp.VFP.Docs.xml" path="doc/__FoxPopWithBlock/*" />
FUNCTION __FoxPopWithBlock() AS OBJECT
    LOCAL stack := __GetFoxWithStack() as Stack<OBJECT>
    if stack:Count > 0
        return stack:Pop()
    ENDIF
    var error := Error{__VfpStr(VFPErrors.VFP_WITH_STACK_EMPTY)}
    error:Gencode := EG_NULLVAR
    error:FuncSym := ProcName(1)
    error:SetStackTrace(ErrorStack(1))
    THROW error
/// <include file="XSharp.VFP.Docs.xml" path="doc/__FoxGetWithExpression/*" />
FUNCTION __FoxGetWithExpression() AS OBJECT
    LOCAL stack := __GetFoxWithStack() AS Stack<OBJECT>
    IF stack:Count > 0
        RETURN stack:Peek()
    ENDIF
    VAR error := Error{__VfpStr(VFPErrors.VFP_WITH_STACK_EMPTY)}
    error:Gencode := EG_NULLVAR
    error:FuncSym := ProcName(1)
    error:SetStackTrace(ErrorStack(1))
    THROW error


/// <include file="XSharp.VFP.Docs.xml" path="doc/__IVarGetOrSend/*" />
function __IVarGetOrSend(oObj as usual, cName as string, nIndex as long) as usual
    var members := __CheckParams(oObj, cName)
    // this returns an array with at least one member (there may be more when the method is overloaded)
    var mem := members[1]
    if mem is MethodInfo
        return __InternalSend(oObj, cName, nIndex)
    endif
    var aVar := IVarGet(oObj, cName)
    return aVar[nIndex]

/// <include file="XSharp.VFP.Docs.xml" path="doc/__IVarGetOrSend_2/*" />
function __IVarGetOrSend(oObj as usual, cName as string, nIndex1 as long, nIndex2 as long) as usual
    var members := __CheckParams(oObj, cName)
    // this returns an array with at least one member (there may be more when the method is overloaded)
    var mem := members[1]
    if mem is MethodInfo
        return __InternalSend(oObj, cName, nIndex1, nIndex2)
    endif
    var aVar := IVarGet(oObj, cName)
    return aVar[nIndex1, nIndex2]

/// <include file="XSharp.VFP.Docs.xml" path="doc/__CheckParams/*" />
static function __CheckParams(oObj as object, cName as string) as MemberInfo[]
 if oObj == null
        Throw ArgumentException{"Parameter should not be null", nameof(oObj)}
    endif
    if String.IsNullOrEmpty(cName)
        Throw ArgumentException{"Parameter should not be null or empty", nameof(cName)}
    endif
    local oType as System.Type
    oType := oObj:GetType()
    var members := oType:GetMember(cName, BindingFlags.Instance+BindingFlags.IgnoreCase+BindingFlags.Public)
    if members:Length == 0
        Throw Exception{i"Class '{oType:Name}' does not have a member with the name '{cName}'"}
    endif
    return members


FUNCTION __VarGetOrCreateFoxArray(cName as STRING) as USUAL
    local uResult := XSharp.MemVar.GetSafe(cName) as usual
    if IsNil(uResult)
        var oMemVar := XSharp.MemVar.Find(cName)
        if oMemVar == NULL
            uResult := __FoxArray{}
            MemVarPut(cName, uResult)
        ENDIF
    endif
    RETURN uResult

