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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/inlist/*" />
/// <remarks>InList() determines whether an expression is found in a series of expressions.
/// The first expression is compared to each of the other expressions until either a match is
/// found or the entire list has been traversed.
/// For exact matching, use InListExact(). </remarks>
/// <seealso cref='M:XSharp.RT.Functions.InListExact(XSharp.__Usual,XSharp.__Usual[])'>InListExact</seealso>
FUNCTION InList(uValue AS USUAL, uValueList PARAMS USUAL[]) AS LOGIC
    RETURN _InListWorker(uValue, uValueList, {x,y => x = y})

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/inlist/*" />
/// <remarks>InListExact() determines whether an expression is found in a series of expressions.
/// The first expression is compared to each of the other expressions until either a match is
/// found or the entire list has been traversed.
/// InListExact() is the same as InList() except that <c>==</c> is used for matching instead of <c>=</c>.
/// There is no difference between InList() and InListExact() for non string datatypes. </remarks>
/// <seealso cref='O:XSharp.RT.Functions.InList'>InList</seealso>
/// <example>
/// This example compares InList() and InListExact() when the left operand in a comparison is longer than the right operand of a comparison:
/// <code language="X#">
/// ? InList("longer", "long")&#0009;&#0009;// TRUE
/// ? InList("long  ", "long")&#0009;&#0009;// TRUE
/// ? InListExact("longer", "long")&#0009;// FALSE
/// ? InListExact("long  ", "long")&#0009;// FALSE
/// </code>
/// This example compares InList() and InListExact() when the right operand in a comparison is longer than the left operand of a comparison:
/// <code language="X#">
/// ? InList("long", "longer")&#0009;&#0009;// FALSE
/// ? InList("long", "long  ")&#0009;&#0009;// FALSE
/// ? InListExact("long", "longer")&#0009;// FALSE
/// ? InListExact("long", "long  ")&#0009;// FALSE
/// </code>
/// </example>
/// <seealso cref='M:XSharp.RT.Functions.InList(XSharp.__Usual,XSharp.__Usual[])'>InList</seealso>
FUNCTION InListExact(uValue AS USUAL, uValueList PARAMS USUAL[]) AS LOGIC
    RETURN _InListWorker(uValue, uValueList, {x,y => x == y})


INTERNAL FUNCTION _InListWorker( u IN USUAL, args AS CONST USUAL[], compare as Func<USUAL, USUAL, LOGIC>) AS LOGIC
    LOCAL i, nLen AS INT
    TRY
    nLen := args:Length
    FOR i := 1 TO nLen
        IF compare(u, args[i]) // make sure these are in the right order so that a shorter string search key does not match a longer string value in the list
            RETURN TRUE
        ENDIF
    NEXT
    CATCH e as Exception
        ? e:ToString()
    END TRY
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
        var d1 := (DateTime) uValue1
        var d2 := (DateTime) uValue2
        RETURN IIF (d1 > d2, d1, d2)

    ELSEIF uValue1:IsString .AND. uValue2:IsString
        RETURN IIF ((STRING) uValue1 > (STRING) uValue2, uValue1, uValue2)

    ELSEIF uValue1:IsSymbol .AND. uValue2:IsSymbol
        RETURN IIF ((SYMBOL) uValue1 > (SYMBOL) uValue2, uValue1, uValue2)

   ELSEIF uValue1:IsBinary.AND. uValue2:IsBinary
        RETURN IIF ((BINARY) uValue1 > (BINARY) uValue2, uValue1, uValue2)

    ELSE
        var type1 := uValue1:ValType
        var type2 := uValue2:ValType
        THROW Error.ArgumentError( __FUNCTION__, NAMEOF(uValue2) , i"Incompatible types ({type1}, {type2})")
    ENDIF

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/min/*" />
FUNCTION Min(uValue1 AS USUAL, uValue2 AS USUAL) AS USUAL
    IF uValue1:IsNumeric .AND. uValue2:IsNumeric

        IF uValue1:IsFloat .OR. uValue2:IsFloat
            RETURN (USUAL) Math.Min( (REAL8) uValue1, (REAL8) uValue2)

        ELSEIF uValue1:IsDecimal .OR. uValue2:IsDecimal .OR. uValue1:IsCurrency  .OR. uValue2:IsCurrency
            RETURN (USUAL) Math.Min( (Decimal) uValue1, (Decimal) uValue2)

        ELSEIF uValue1:IsInt64 .OR. uValue2:IsInt64
            RETURN (USUAL) Math.Min( (INT64) uValue1, (INT64) uValue2)
        ENDIF
        RETURN (USUAL) Math.Min( (LONG) uValue1, (LONG) uValue2)

    ELSEIF uValue1:IsDate .AND. uValue2:IsDate
        RETURN IIF ((DATE) uValue1 < (DATE) uValue2, uValue1, uValue2)

    ELSEIF uValue1:IsDateTime .AND. uValue2:IsDateTime
        RETURN IIF ((DateTime) uValue1 < (DateTime) uValue2, uValue1, uValue2)

    ELSEIF (uValue1:IsDateTime .OR. uValue1:IsDate) .AND. (uValue2:IsDateTime .OR. uValue2:IsDate)
        var d1 := (DateTime) uValue1
        var d2 := (DateTime) uValue2
        RETURN IIF ( d1 < d2, d1, d2)

    ELSEIF uValue1:IsString .AND. uValue2:IsString
        RETURN IIF ((STRING) uValue1 < (STRING) uValue2, uValue1, uValue2)

    ELSEIF uValue1:IsSymbol .AND. uValue2:IsSymbol
        RETURN IIF ((SYMBOL) uValue1 < (SYMBOL) uValue2, uValue1, uValue2)

    ELSEIF uValue1:IsBinary.AND. uValue2:IsBinary
        RETURN IIF ((BINARY) uValue1 < (BINARY) uValue2, uValue1, uValue2)

        ELSE
            var type1 := uValue1:ValType
            var type2 := uValue2:ValType
        THROW Error.ArgumentError( __FUNCTION__, NAMEOF(uValue2) , i"Incompatible types ({type1}, {type2})")
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


