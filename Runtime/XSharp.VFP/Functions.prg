//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <summary>Creates an object from a class definition or an Automation-enabled application.</summary>
/// <param name="cClassName">Specifies the class or OLE object from which the new object is created.</param>
/// <param name="_args">These optional parameters are used to pass values to the Init event procedure for the class.
/// The Init event is executed when you issue CREATEOBJECT( ) and allows you to initialize the object.</param>
/// <returns>The object that was created</returns>
/// <seealso cref='M:XSharp.RT.Functions.CreateInstance(XSharp.__Usual,XSharp.__Usual)' >CreateInstance</seealso>

FUNCTION CreateObject(cClassName, _args ) AS OBJECT CLIPPER
    // The pseudo function _ARGS() returns the Clipper arguments array
    RETURN CreateInstance(_ARGS())


PROCEDURE RddInit() AS VOID _INIT3
    // Make sure that the VFP dialect has the DBFVFP driver as default RDD
    RddSetDefault("DBFVFP")
    RuntimeState.SetValue(Set.FOXCOLLATE,"")
    RuntimeState.SetValue(Set.MEMOWIDTH, 50)
    RuntimeState.SetValue(Set.NEAR, FALSE)
    RuntimeState.SetValue(Set.SQLANSI, FALSE)
    RuntimeState.SetValue(Set.FOXLOCK, TRUE)
    RETURN 



Function SetFoxCollation(cCollation as STRING) AS STRING
local cOld := RuntimeState.GetValue<STRING>(Set.FOXCOLLATE) AS STRING
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
    RuntimeState.SetValue(Set.FOXCOLLATE,cValue)
ELSE
    local oError as Error
    oError := Error.ArgumentError(__FUNCTION__, nameof(cCollation), 1, {cCollation})
    oError:Description := "Collating sequence '"+cCollation+"' is not found"
    oError:Throw()
ENDIF
RETURN cOld


function ICase(lCond1, uExp1, lCond2, uExp2) as usual
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
