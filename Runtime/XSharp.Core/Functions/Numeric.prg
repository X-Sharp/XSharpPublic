//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/absint/*" /> 
FUNCTION AbsInt(liValue AS LONGINT) AS LONG
	RETURN Math.Abs(liValue)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/abslong/*" /> 
FUNCTION AbsLong(liValue AS LONGINT) AS LONG
	RETURN Math.Abs(liValue)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/absreal4/*" /> 
FUNCTION AbsReal4(r4Value AS REAL4) AS REAL4
	RETURN Math.Abs(r4Value)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/absreal8/*" />
FUNCTION AbsReal8(r8Value AS REAL8) AS REAL8
	RETURN Math.Abs(r8Value)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/absshort/*" />
FUNCTION AbsShort(siValue AS SHORT) AS LONG
	RETURN Math.Abs(siValue)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mod/*" />
FUNCTION Mod(nDividend AS REAL8, nDivisor AS REAL8) AS REAL8
	RETURN nDividend % nDivisor

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mod/*" />
FUNCTION Mod(nDividend AS INT64, nDivisor AS INT64) AS INT64
	RETURN nDividend % nDivisor

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mod/*" />
FUNCTION Mod(nDividend AS LONG, nDivisor AS LONG) AS LONG
	RETURN nDividend % nDivisor

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/swapbyte/*" />
FUNCTION SwapByte(bSwap AS BYTE) AS BYTE
	RETURN (BYTE)  ((bSwap << 4) | (bSwap >> 4))

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/swapdword/*" />
FUNCTION SwapDWord(dwSwap AS DWORD) AS DWORD
	LOCAL dw1, dw2 AS DWORD
	dw1 := (dwSwap & 0x0000ffff) << 16
	dw2 := (dwSwap >> 16) & 0x0000ffff
	dwSwap := dw1 | dw2
RETURN	dwSwap

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/swapint/*" />
FUNCTION SwapInt(liSwap AS LONG) AS LONG
	RETURN SwapLong(liSwap) 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/swaplong/*" />
FUNCTION SwapLong(liSwap AS LONG) AS LONG
	LOCAL li1, li2 AS LONG
	li1 := (liSwap & 0x0000ffff) << 16
	li2 := (liSwap >> 16) & 0x0000ffff
	liSwap := li1 | li2
	RETURN liSwap


/// <summary>
/// Exchange the right and left halves of a Int64
/// </summary>
/// <param name="i64"></param>
/// <returns>
/// </returns>
FUNCTION SwapInt64( i64 AS INT64 ) AS INT64
   RETURN (INT64)  ( i64 << 32 ) | ( i64 >> 32 ) 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/swapshort/*" />
FUNCTION SwapShort(siSwap AS SHORT) AS SHORT
RETURN	 (SHORT) (((siSwap & 0x00ff) << 8) | ((siSwap >> 8) & 0x00ff))

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/swapword/*" />
FUNCTION SwapWord(wSwap AS WORD) AS WORD
RETURN (WORD) (((wSwap & 0x00ff) << 8) | ((wSwap >> 8) & 0x00ff))


FUNCTION SwapQWord( qw AS UINT64 ) AS UINT64
   RETURN (UINT64)  ( qw << 32 ) | ( qw >> 32 ) 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/makedword/*" />
FUNCTION MakeDWord( wLow AS WORD, wHigh AS WORD ) AS DWORD
	RETURN DWORD( ( DWORD(wHigh) << 16 ) | wLow )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/makelong/*" />
FUNCTION MakeLong( wLow AS WORD, wHigh AS WORD ) AS INT
	RETURN INT( ( DWORD(wHigh) << 16 ) | wLow )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/makeword/*" />
FUNCTION MakeWord( bLow AS BYTE, bHigh AS BYTE ) AS WORD
	RETURN WORD( ( WORD(bHigh) << 8 ) | bLow )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rgb/*" />
FUNCTION RGB(bRed AS BYTE,bGreen AS BYTE,bBlue AS BYTE) AS DWORD
	RETURN (DWORD(bRed) << 16) + (DWORD(bGreen) << 8) + DWORD(bBlue)

