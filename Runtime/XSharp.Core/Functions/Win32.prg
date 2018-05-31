//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System.Runtime.InteropServices
CLASS Win32
	[DllImport("kernel32.dll", SetLastError := FALSE, EntryPoint := "GetOEMCP")];
	EXTERN STATIC METHOD GetDosCodePage() AS LONG

	[DllImport("kernel32.dll", SetLastError := FALSE, EntryPoint := "GetACP")];
	EXTERN STATIC METHOD GetWinCodePage() AS LONG
	
	
	[DllImport("kernel32.dll", EntryPoint := "CompareStringA", CharSet:=CharSet.Ansi)];
	EXTERN STATIC METHOD  CompareStringAnsi(LCID AS INT, cmpFlags AS DWORD , bString1 AS STRING, chCount1 AS INT , bString2 AS STRING , chCount2 AS INT) AS INT

	public const SORT_STRINGSORT := 0x00001000  as int
END CLASS