//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/sysobject/*" />
FUNCTION SysObject(oSys) AS OBJECT CLIPPER
   LOCAL oValue AS OBJECT
   IF PCount() == 1
      XSharp.RuntimeState.SetValue(Set.SysObject, oSys)
      oValue := oSys
   ELSE
      oValue := XSharp.RuntimeState.GetValue<OBJECT>(Set.SysObject)
   ENDIF
   RETURN oValue



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/_run/*" />
FUNCTION _Run (cProgram AS STRING) AS DWORD PASCAL
    RETURN Win32.WinExec(cProgram, Win32.SW_SHOWNORMAL)
