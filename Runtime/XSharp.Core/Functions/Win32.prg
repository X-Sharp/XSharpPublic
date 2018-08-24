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
	
	
	[DllImport("kernel32.dll", EntryPoint := "CompareStringA")];
	EXTERN STATIC METHOD  CompareStringAnsi(LCID AS INT, cmpFlags AS DWORD , bString1 AS BYTE[], chCount1 AS INT , bString2 AS BYTE[] , chCount2 AS INT) AS INT

	PUBLIC CONST SORT_STRINGSORT := 0x00001000  AS INT
END CLASS
