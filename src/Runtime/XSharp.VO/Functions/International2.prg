//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Text

/// <include file="XSharp.VO.Docs.xml" path="doc/LoadResString/*" />
FUNCTION LoadResString(cDef ,id ,xModule ) AS STRING CLIPPER
   LOCAL cRet  := "" AS STRING
   LOCAL uiID  := 0	 AS DWORD
   LOCAL hInst AS IntPtr
   LOCAL sb    AS StringBuilder

   IF IsString( cDef )
      cRet := cDef
   ENDIF

   IF PCount() < 2
      RETURN cRet
   ENDIF

   IF IsNumeric( id )
      uiID := id
      IF uiID == 0
         RETURN cRet
      ENDIF
   ELSE
      RETURN cRet
   ENDIF

   IF PCount() > 2
      IF IsPtr( xModule )
         hInst := (IntPtr) xModule
      ELSEIF IsString( xModule )
         hInst := GetModuleHandleW( xModule )
      ELSE
         hInst := IntPtr.Zero
      ENDIF
   ELSE
      hInst := GetModuleHandleW( NULL )
   ENDIF

   IF hInst != IntPtr.Zero
      sb := StringBuilder{ 1024 }
      IF LoadStringW( hInst, uiID, sb, sb:Capacity + 1 ) > 0
         cRet := sb:ToString()
      ENDIF
   ENDIF

   RETURN cRet

INTERNAL _DLL FUNCTION LoadStringW( hInst AS IntPtr, uID AS DWORD, lpBuffer AS StringBuilder, nBufferSize AS INT ) AS INT PASCAL:User32.LoadStringW
INTERNAL _DLL FUNCTION GetModuleHandleW( name AS STRING ) AS IntPtr PASCAL:Kernel32.GetModuleHandleW

/// <include file="XSharp.VO.Docs.xml" path="doc/MBSubstr/*" />
FUNCTION MBSubstr(c AS USUAL,iStart AS USUAL,wLen AS USUAL) AS STRING
	RETURN SubStr(c, iStart, wLen)
