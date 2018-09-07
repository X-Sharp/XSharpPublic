//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System.Runtime.InteropServices
INTERNAL STATIC CLASS Win32
	[DllImport("kernel32.dll", SetLastError := FALSE, EntryPoint := "GetOEMCP")];
	EXTERN STATIC METHOD GetDosCodePage() AS LONG

	[DllImport("kernel32.dll", SetLastError := FALSE, EntryPoint := "GetACP")];
	EXTERN STATIC METHOD GetWinCodePage() AS LONG
	
	
	[DllImport("kernel32.dll", EntryPoint := "CompareStringA", SetLastError := TRUE)];
	EXTERN STATIC METHOD  CompareStringAnsi(LCID AS INT, cmpFlags AS DWORD , bString1 AS BYTE[], chCount1 AS INT , bString2 AS BYTE[] , chCount2 AS INT) AS INT PASCAL 
	[DllImport("kernel32.dll", EntryPoint := "CompareStringA", CharSet := CharSet.Ansi, SetLastError := TRUE)];
	EXTERN STATIC METHOD  CompareStringAnsi2(LCID AS INT, cmpFlags AS DWORD , bString1 AS STRING, chCount1 AS INT , bString2 AS STRING , chCount2 AS INT) AS INT PASCAL 

	PUBLIC CONST SORT_STRINGSORT := 0x00001000  AS INT
    PUBLIC CONST CSTR_LESS_THAN := 1 AS INT
    PUBLIC CONST CSTR_EQUAL := 2  AS INT
    PUBLIC CONST CSTR_GREATER_THAN := 3  AS INT

END CLASS
