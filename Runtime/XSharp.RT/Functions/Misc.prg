//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Runtime.CompilerServices
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/between/*" />
/// <seealso cref='O:XSharp.Core.Functions.Between'>Between</seealso>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Between(uValue AS USUAL, uMin AS USUAL, uMax AS USUAL) AS LOGIC
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
	RETURN _InListWorker(u, uValueList, {x,y => x = y})

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/inlistexact/*" />
FUNCTION InListExact(u AS USUAL, uValueList PARAMS USUAL[]) AS LOGIC
	RETURN _InListWorker(u, uValueList, {x,y => x == y})


INTERNAL FUNCTION _InListWorker( u AS USUAL, args AS CONST USUAL[], compare as Func<USUAL, USUAL, LOGIC>) AS LOGIC
	LOCAL i, nLen AS INT
	nLen := args:Length
	FOR i := 1 TO nLen
		IF compare(args[i], u)
			RETURN TRUE
		ENDIF
	NEXT
	RETURN FALSE


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Max(uValue1 AS REAL8, uValue2 AS REAL8) AS REAL8
    RETURN IIF (uValue1 > uValue2, uValue1, uValue2)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Max(uValue1 AS LONG, uValue2 AS LONG) AS LONG
    RETURN IIF (uValue1 > uValue2, uValue1, uValue2)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Max(uValue1 AS INT64, uValue2 AS INT64) AS INT64
    RETURN IIF (uValue1 > uValue2, uValue1, uValue2)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Max(uValue1 AS DECIMAL, uValue2 AS DECIMAL) AS DECIMAL
    RETURN IIF (uValue1 > uValue2, uValue1, uValue2)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Max(uValue1 AS DWORD, uValue2 AS DWORD) AS DWORD
    RETURN IIF (uValue1 > uValue2, uValue1, uValue2)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Max(uValue1 AS FLOAT, uValue2 AS FLOAT) AS FLOAT
    RETURN IIF (uValue1 > uValue2, uValue1, uValue2)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Max(uValue1 AS CURRENCY, uValue2 AS CURRENCY) AS CURRENCY
    RETURN IIF (uValue1 > uValue2, uValue1, uValue2)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Max(uValue1 AS SYMBOL, uValue2 AS SYMBOL) AS SYMBOL
    RETURN IIF (uValue1 > uValue2, uValue1, uValue2)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Max(uValue1 AS STRING, uValue2 AS STRING) AS STRING
    RETURN IIF(XSharp.RuntimeState.StringCompare(uValue1, uValue2) >= 0, uValue1, uValue2)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
FUNCTION Max(uValue1 AS USUAL, uValue2 AS USUAL) AS USUAL

	IF uValue1:IsNumeric .AND. uValue2:IsNumeric

		IF uValue1:IsFloat .OR. uValue2:IsFloat
			RETURN Max( (REAL8) uValue1, (REAL8) uValue2)

		ELSEIF uValue1:IsDecimal .OR. uValue2:IsDecimal
			RETURN Max( (Decimal) uValue1, (Decimal) uValue2)
        ELSEIF uValue1:IsCurrency  .OR. uValue2:IsCurrency
            RETURN Max( (Currency) uValue1, (Currency) uValue2)
		ELSEIF uValue1:IsInt64 .OR. uValue2:IsInt64
			RETURN Max( (INT64) uValue1, (INT64) uValue2)
		ENDIF
		RETURN Max( (LONG) uValue1, (LONG) uValue2)

	ELSEIF uValue1:IsDate .AND. uValue2:IsDate
		RETURN Max(uValue1:_dateValue, uValue2:_dateValue)

	ELSEIF uValue1:IsDateTime .AND. uValue2:IsDateTime
		RETURN Max(uValue1:_dateTimeValue, uValue2:_dateTimeValue)

	ELSEIF (uValue1:IsDateTime .OR. uValue1:IsDate) .AND. (uValue2:IsDateTime .OR. uValue2:IsDate)
		RETURN Max((Date) uValue1 ,(Date) uValue2)

	ELSEIF uValue1:IsString .AND. uValue2:IsString
		RETURN Max (uValue1:_stringValue , uValue2:_stringValue)

	ELSEIF uValue1:IsSymbol .AND. uValue2:IsSymbol
		RETURN Max(uValue1:_symValue, uValue2:_symValue)

	ELSEIF uValue1:IsBinary .AND. uValue2:IsBinary
		RETURN iif( (BINARY) uValue1 > (BINARY) uValue2, uValue1, uValue2)

	ELSE
        THROW Error.ArgumentError( __FUNCTION__, NAMEOF(uValue2) , "Incompatible types")
	ENDIF




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Min(uValue1 AS REAL8, uValue2 AS REAL8) AS REAL8
    RETURN IIF (uValue1 < uValue2, uValue1, uValue2)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Min(uValue1 AS LONG, uValue2 AS LONG) AS LONG
    RETURN IIF (uValue1 < uValue2, uValue1, uValue2)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Min(uValue1 AS INT64, uValue2 AS INT64) AS INT64
    RETURN IIF (uValue1 < uValue2, uValue1, uValue2)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Min(uValue1 AS DECIMAL, uValue2 AS DECIMAL) AS DECIMAL
    RETURN IIF (uValue1 < uValue2, uValue1, uValue2)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Min(uValue1 AS DWORD, uValue2 AS DWORD) AS DWORD
    RETURN IIF (uValue1 < uValue2, uValue1, uValue2)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Min(uValue1 AS FLOAT, uValue2 AS FLOAT) AS FLOAT
    RETURN IIF (uValue1 < uValue2, uValue1, uValue2)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Min(uValue1 AS CURRENCY, uValue2 AS CURRENCY) AS CURRENCY
    RETURN IIF (uValue1 < uValue2, uValue1, uValue2)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Min(uValue1 AS SYMBOL, uValue2 AS SYMBOL) AS SYMBOL
    RETURN IIF (uValue1 < uValue2, uValue1, uValue2)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Min(uValue1 AS STRING, uValue2 AS STRING) AS STRING
    RETURN IIF(XSharp.RuntimeState.StringCompare(uValue1, uValue2) <= 0, uValue1, uValue2)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/min/*" />
FUNCTION Min(uValue1 AS USUAL, uValue2 AS USUAL) AS USUAL
	IF uValue1:IsNumeric .AND. uValue2:IsNumeric

		IF uValue1:IsFloat .OR. uValue2:IsFloat

			RETURN Min((REAL8) uValue1, (REAL8) uValue2)

		ELSEIF uValue1:IsDecimal .OR. uValue2:IsDecimal
			RETURN Min( (Decimal) uValue1, (Decimal) uValue2)

        ELSEIF uValue1:IsCurrency  .OR. uValue2:IsCurrency
            RETURN Min( (Currency) uValue1, (Currency) uValue2)

		ELSEIF uValue1:IsInt64 .OR. uValue2:IsInt64
			RETURN Min( (INT64) uValue1, (INT64) uValue2)
		ENDIF
		RETURN Min( (LONG) uValue1, (LONG) uValue2)

	ELSEIF uValue1:IsDate .AND. uValue2:IsDate
		RETURN Min(uValue1:_dateValue, uValue2:_dateValue)

	ELSEIF uValue1:IsDateTime .AND. uValue2:IsDateTime
		RETURN Min(uValue1:_dateTimeValue, uValue2:_dateTimeValue)

	ELSEIF (uValue1:IsDateTime .OR. uValue1:IsDate) .AND. (uValue2:IsDateTime .OR. uValue2:IsDate)
		RETURN Min((Date) uValue1 ,(Date) uValue2)

	ELSEIF uValue1:IsString .AND. uValue2:IsString
		RETURN Min (uValue1:_stringValue , uValue2:_stringValue)

	ELSEIF uValue1:IsSymbol .AND. uValue2:IsSymbol
		RETURN Min(uValue1:_symValue, uValue2:_symValue)

	ELSEIF uValue1:IsBinary .AND. uValue2:IsBinary
		RETURN iif( (BINARY) uValue1 < (BINARY) uValue2, uValue1, uValue2)

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

