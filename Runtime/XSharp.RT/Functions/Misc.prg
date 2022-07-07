//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Runtime.CompilerServices
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/between/*" />
/// <seealso cref='O:XSharp.Core.Functions.Between'>Between</seealso>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Between(uValue AS USUAL,uMin AS USUAL,uMax AS USUAL) AS LOGIC
	RETURN uValue >=uMin .AND.  uValue<=uMax

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/betweentyped/*" />
/// <seealso cref='O:XSharp.Core.Functions.Between'>Between</seealso>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Between(dValue AS DATE, dMin AS DATE, dMax AS DATE) AS LOGIC
    RETURN ( dValue >= dMin .AND. dValue <= dMax )


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/betweentyped/*" />
/// <seealso cref='O:XSharp.Core.Functions.Between'>Between</seealso>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Between(nValue AS CURRENCY, nMin AS CURRENCY, nMax AS CURRENCY) AS LOGIC
    RETURN ( nValue >= nMin .AND. nValue <= nMax )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/betweentyped/*" />
/// <seealso cref='O:XSharp.Core.Functions.Between'>Between</seealso>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Between(nValue AS FLOAT, nMin AS FLOAT, nMax AS FLOAT) AS LOGIC
    RETURN ( nValue >= nMin .AND. nValue <= nMax )



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/inlist/*" />
FUNCTION InList(u AS USUAL, uValueList PARAMS USUAL[]) AS LOGIC
	RETURN _InListWorker(u, uValueList, FALSE)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/inlistexact/*" />
FUNCTION InListExact(u AS USUAL, uValueList PARAMS USUAL[]) AS LOGIC
	RETURN _InListWorker(u, uValueList, TRUE)


INTERNAL FUNCTION _InListWorker( u AS USUAL, args AS CONST USUAL[], lExact AS LOGIC) AS LOGIC
	LOCAL i, nLen AS INT
	nLen := args:Length
	IF lExact
		FOR i := 1 TO nLen
			IF args[i] == u
				RETURN TRUE
			ENDIF
		NEXT
	ELSE
		FOR i := 1 TO nLen
			IF u = args[i]
				RETURN TRUE
			ENDIF
		NEXT
	ENDIF
	RETURN FALSE




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
FUNCTION Max(uValue1 AS USUAL,uValue2 AS USUAL) AS USUAL

	IF uValue1:IsNumeric .AND. uValue2:IsNumeric

		IF uValue1:IsFloat .OR. uValue2:IsFloat
			RETURN (USUAL) Math.Max( (REAL8) uValue1, (REAL8) uValue2)

		ELSEIF uValue1:IsDecimal .OR. uValue2:IsDecimal .OR. uValue1:IsCurrency  .OR. uValue2:IsCurrency
			RETURN (USUAL) Math.Max( (Decimal) uValue1, (Decimal) uValue2)

		ELSEIF uValue1:IsInt64 .OR. uValue2:IsInt64
			RETURN (USUAL) Math.Max( (INT64) uValue1, (INT64) uValue2)
		ENDIF
		RETURN (USUAL) Math.Max( (LONG) uValue1, (LONG) uValue2)

	ELSEIF uValue1:IsDate .AND. uValue2:IsDate
		RETURN IIF ((DATE) uValue1 > (DATE) uValue2, uValue1, uValue2)

	ELSEIF uValue1:IsDateTime .AND. uValue2:IsDateTime
		RETURN IIF ((DateTime) uValue1 > (DateTime) uValue2, uValue1, uValue2)

	ELSEIF (uValue1:IsDateTime .OR. uValue1:IsDate) .AND. (uValue2:IsDateTime .OR. uValue2:IsDate)
		RETURN IIF ((Date) uValue1 > (Date) uValue2, uValue1, uValue2)

	ELSEIF uValue1:IsString .AND. uValue2:IsString
		RETURN IIF ((STRING) uValue1 > (STRING) uValue2, uValue1, uValue2)

	ELSEIF uValue1:IsSymbol .AND. uValue2:IsSymbol
		RETURN IIF ((SYMBOL) uValue1 > (SYMBOL) uValue2, uValue1, uValue2)

	ELSE
        THROW Error.ArgumentError( __FUNCTION__, NAMEOF(uValue2) , "Incompatible types")
	ENDIF




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/min/*" />
FUNCTION Min(uValue1 AS USUAL,uValue2 AS USUAL) AS USUAL
	IF uValue1:IsNumeric .AND. uValue2:IsNumeric

		IF uValue1:IsFloat .OR. uValue2:IsFloat

			RETURN (USUAL) Math.Min((REAL8) uValue1, (REAL8) uValue2)

		ELSEIF uValue1:IsDecimal .OR. uValue2:IsDecimal .OR. uValue1:IsCurrency .OR. uValue2:IsCurrency
			RETURN (USUAL) Math.Min( (Decimal) uValue1, (Decimal) uValue2)

		ELSEIF uValue1:IsInt64 .OR. uValue2:IsInt64
			RETURN (USUAL) Math.Min( (INT64) uValue1, (INT64) uValue2)
		ENDIF
		RETURN (USUAL) Math.Min( (LONG) uValue1, (LONG) uValue2)

	ELSEIF uValue1:IsDate .AND. uValue2:IsDate
		RETURN IIF ((DATE) uValue1 <(DATE) uValue2, uValue1, uValue2)

    ELSEIF uValue1:IsDateTime .AND. uValue2:IsDateTime
		RETURN IIF ((DateTime) uValue1 <(DateTime) uValue2, uValue1, uValue2)

    ELSEIF (uValue1:IsDateTime .OR. uValue1:IsDate) .AND. (uValue2:IsDateTime .OR. uValue2:IsDate)

		RETURN IIF ((Date) uValue1 <(Date) uValue2, uValue1, uValue2)

	ELSEIF uValue1:IsString .AND. uValue2:IsString
		RETURN IIF ((STRING) uValue1 < (STRING) uValue2, uValue1, uValue2)

	ELSEIF uValue1:IsSymbol .AND. uValue2:IsSymbol
		RETURN IIF ((SYMBOL) uValue1 < (SYMBOL) uValue2, uValue1, uValue2)
	ELSE
        THROW Error.ArgumentError( __FUNCTION__, NAMEOF(uValue2) , "Incompatible types")
	ENDIF



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/palettergb/*" />
FUNCTION PaletteRGB(bR AS USUAL,bG AS USUAL,bB AS BYTE) AS INT
	RETURN (INT) RGB(bR, bG, bB)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/tone/*" />
FUNCTION Tone(wFrequency AS DWORD,wDuration AS DWORD) AS USUAL
	Console.Beep( (INT)wFrequency, (INT)wDuration * 1000 / 18 )
RETURN	 NIL


// moved from wingdi in win32apilibrary

///  <exclude/>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION GetRValue(rgb AS DWORD) AS BYTE
	RETURN BYTE(_CAST,rgb)

///  <exclude/>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION GetGValue(rgb AS DWORD) AS BYTE
	LOCAL val AS WORD
	val := WORD(_CAST,rgb)>>8
	RETURN BYTE(_CAST,val)

///  <exclude/>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION GetBValue(rgb AS DWORD) AS BYTE
	LOCAL val AS DWORD
	val := rgb>>16
	RETURN BYTE(_CAST,val)


// Macros from wingdi.h
#define _CMYK(c,m,y,k)       ((DWORD)((((BYTE)(k)|((WORD)((BYTE)(y))<<8))|(((DWORD)(BYTE)(m))<<16))|(((DWORD)(BYTE)(c))<<24)))
#define _MAKEROP4(fore,back) (DWORD)((((back) << 8) & 0xFF000000) | (fore))
#define _GetKValue(cmyk)      ((BYTE)(cmyk))
#define _GetYValue(cmyk)      ((BYTE)((cmyk)>> 8))
#define _GetMValue(cmyk)      ((BYTE)((cmyk)>>16))
#define _GetCValue(cmyk)      ((BYTE)((cmyk)>>24))

///  <exclude/>
FUNCTION CMYK(c AS DWORD, m AS DWORD, y AS DWORD, k AS DWORD)  AS DWORD
	RETURN _CMYK(c,m,y,k)


///  <exclude/>
FUNCTION MAKEROP4(fore AS WORD, back AS WORD) AS DWORD
	return _MAKEROP4(fore,back)


///  <exclude/>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION GetKValue(cmyk AS DWORD) AS BYTE
	RETURN _GetKValue(cmyk)



///  <exclude/>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION GetCValue(cmyk AS DWORD) AS BYTE STRICT
	RETURN _GetCValue(cmyk)

///  <exclude/>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION GetMValue(cmyk AS DWORD) AS BYTE
	RETURN _GetMValue(cmyk)

///  <exclude/>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION GetYValue(cmyk AS DWORD) AS BYTE
	RETURN _GetYValue(cmyk)

