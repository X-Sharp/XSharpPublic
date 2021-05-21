//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Reflection

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/sysobject/*" />
FUNCTION SysObject(oSys AS OBJECT) AS OBJECT
    // Note that the VO docs says that it returns oSys and not the previous sysobject when oSys is specified !
   XSharp.RuntimeState.SetValue(Set.SysObject, oSys)
   RETURN oSys

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/sysobject/*" />
FUNCTION SysObject() AS OBJECT
   RETURN XSharp.RuntimeState.GetValue<OBJECT>(Set.SysObject)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/_run/*" />
FUNCTION _Run (cProgram AS STRING) AS DWORD PASCAL
    RETURN Win32.WinExec(cProgram, Win32.SW_SHOWNORMAL)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/oclone/*" />
FUNCTION OClone(o AS OBJECT) AS OBJECT
    LOCAL oMethod AS MethodInfo
    LOCAL oNewObject := NULL AS OBJECT
    IF o != NULL
        oMethod := o:GetType():GetMethod("MemberwiseClone" , BindingFlags.NonPublic + BindingFlags.Instance)
        oNewObject := oMethod:Invoke(o , OBJECT[]{0})
    END IF
RETURN oNewObject

	

