//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING XSharp.RDD.Support
USING XSharp.Internal

/// <summary>Creates an object from a class definition or an Automation-enabled application.</summary>
/// <param name="cClassName">Specifies the class or OLE object from which the new object is created.</param>
/// <param name="_args">These optional parameters are used to pass values to the Init event procedure for the class.
/// The Init event is executed when you issue CREATEOBJECT( ) and allows you to initialize the object.</param>
/// <returns>The object that was created</returns>
/// <seealso cref='CreateInstance' >CreateInstance</seealso>
FUNCTION CreateObject(cClassName, _args ) AS OBJECT CLIPPER
    // The pseudo function _ARGS() returns the Clipper arguments array
    RETURN CreateInstance(_ARGS())

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/createobjectex/*" />
FUNCTION CreateObjectEx(cClsIdOrcProgId, cComputerName , cIID ) AS OBJECT CLIPPER
    // The pseudo function _ARGS() returns the Clipper arguments array
    RETURN CreateInstance(_ARGS())


INTERNAL PROCEDURE RddInit() AS VOID _INIT3
    // Make sure that the VFP dialect has the DBFVFP driver as default RDD
    RddSetDefault("DBFVFP")
    RuntimeState.SetValue(Set.CollateFox,"MACHINE")
    RuntimeState.SetValue(Set.MemoWidth, 50)
    RuntimeState.SetValue(Set.Near, FALSE)
    RuntimeState.SetValue(Set.SqlAnsi, FALSE)
    RuntimeState.SetValue(Set.FoxLock, TRUE)
    RuntimeState.AutoOrder := 0
    RuntimeState.Eof :=  TRUE
    RuntimeState.MemoBlockSize := 64
RETURN



Function SetFoxCollation(cCollation as STRING) AS STRING
    local cOld := RuntimeState.GetValue<STRING>(Set.CollateFox) AS STRING
    local aAllowed as STRING[]
    LOCAL lOk := FALSE as LOGIC
    LOCAL cValue := cCollation as STRING
    aAllowed := System.Enum.GetNames(typeof(XSharp.FoxCollations))
    cValue := cValue:Trim():ToUpper()
    FOREACH VAR cEnum in aAllowed
        IF String.Compare(cValue, cEnum, StringComparison.OrdinalIgnoreCase) == 0
            lOk := TRUE
            EXIT
        ENDIF
    NEXT
    IF lOk
        RuntimeState.SetValue(Set.CollateFox,cValue)
    ELSE
        local oError as Error
        oError := Error.ArgumentError(__FUNCTION__, nameof(cCollation), 1, {cCollation})
        oError:Description := __VfpStr(VFPErrors.COLLATION_NOT_FOUND, cCollation)
        oError:Throw()
    ENDIF
    RETURN cOld


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/icase/*" />

function ICase(lCondition, eResult, lCondition2, eResult2, eOtherwiseResult) as usual CLIPPER
    LOCAL nCount := PCount() AS LONG
    // loop through the actual parameters. The odd parameters should be logic
    // the even parameters are return values for their siblings.
    for var nI := 1 to nCount-1 step 2
        local cond := _GetFParam(nI) as logic
        if cond
            return _GetFParam(nI+1)
        endif
    next
    // no conditions are met, if the # of parameters is odd then return the last value
    if nCount % 2 == 1
        return _GetFParam(nCount)
    endif
    // the # of parameters is even. When >= 2 then get the type of parameter 2 and return an empty value
    if PCount() >= 2
        var type := UsualType(_GetFParam(2))
        return EmptyUsual(type)
    ENDIF
    // when they call this function with < 2 parameters then we have no idea what return type they expect...
    return NIL


FUNCTION __VfpStr( resid AS DWORD , args PARAMS OBJECT[]) AS STRING
    // Strings are stored in a Managed resource with a name
    // the name matches the enum names
    // convert the id to the enum and get its name
    LOCAL strId  AS STRING
    LOCAL strMessage AS STRING
    strId := Enum.GetName( TYPEOF(VFPErrors) , resid)
    IF !String.IsNullOrEmpty(strId)
        strMessage := XSharp.Messages.GetString( strId )
        IF String.IsNullOrEmpty( strMessage )
            strMessage := ": canot load string resource '" + strId + "'"
        ELSEIF args != null .and. args:Length > 0
            strMessage := String.Format(strMessage, args)
        ENDIF
    ELSE
        strMessage := "Cannot find string for error number "+resid:ToString()
    ENDIF
    RETURN strMessage




/// <include file="VFPDocs.xml" path="Runtimefunctions/vartype/*" />
FUNCTION VarType( eExpression AS USUAL) AS STRING
    RETURN VarType(eExpression, FALSE)


/// <include file="VFPDocs.xml" path="Runtimefunctions/vartype/*" />
FUNCTION VarType( eExpression AS USUAL, lNullDataType AS LOGIC) AS STRING
    IF IsNil(eExpression)
        IF ! lNullDataType
            RETURN "L"
        ELSE
            RETURN "U"
        ENDIF
    ENDIF
    local result as string
    if eExpression IS __FoxArray VAR aFoxArray
        if ALen(aFoxArray) > 0
            eExpression := aFoxArray[1]
            IF IsNil(eExpression)
                result := "L"
            ELSE
                result := ValType(eExpression)
            ENDIF
        else
            result := "U"
        endif
    else

        result := ValType(eExpression)
    endif
    RETURN result


