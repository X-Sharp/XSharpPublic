//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Text
USING System.Globalization
USING System.Collections.Generic

INTERNAL STATIC CLASS ConversionHelpers
    STATIC INTERNAL usCulture AS CultureInfo
    STATIC CONSTRUCTOR
        usCulture := CultureInfo{"en-US"}
END CLASS

/// <summary>
/// Convert a string containing a 32-bit unsigned integer to a double word.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
/// <seealso cref='M:XSharp.Core.Functions.W2Bin(System.UInt16)' >W2Bin</seealso>
/// <seealso cref='M:XSharp.Core.Functions.DW2Bin(System.UInt32)' >DW2Bin</seealso>
FUNCTION Bin2DW(c AS STRING) AS DWORD
    LOCAL dwResult := 0 AS DWORD
    IF c!= NULL .AND. c:Length >= 4
        LOCAL aBytes AS BYTE[]
        aBytes := BYTE[]{4}
        aBytes[0] := (BYTE) _AND(c:Chars[0], 0xFF)
        aBytes[1] := (BYTE) _AND(c:Chars[1], 0xFF)
        aBytes[2] := (BYTE) _AND(c:Chars[2], 0xFF)
        aBytes[3] := (BYTE) _AND(c:Chars[3], 0xFF)
        dwResult := BitConverter.ToUInt32(aBytes, 0)
    ENDIF
    RETURN dwResult

/// <summary>
/// Convert a string containing a 16-bit signed integer to a short integer.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
/// <seealso cref='M:XSharp.Core.Functions.I2Bin(System.Int16)'>I2Bin</seealso>
/// <seealso cref='M:XSharp.Core.Functions.L2Bin(System.Int32)' >L2Bin</seealso>
FUNCTION Bin2I(c AS STRING) AS SHORT
    LOCAL siResult := 0 AS SHORT
    IF c!= NULL .AND. c:Length >= 2
        LOCAL aBytes AS BYTE[]
        aBytes := BYTE[]{2}
        aBytes[0] := (BYTE) _AND(c:Chars[0], 0xFF)
        aBytes[1] := (BYTE) _AND(c:Chars[1], 0xFF)
        siResult := BitConverter.ToInt16(aBytes, 0)
    ENDIF
    RETURN siResult

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/bin2l/*" />
/// <seealso cref='M:XSharp.Core.Functions.I2Bin(System.Int16)'>I2Bin</seealso>
/// <seealso cref='M:XSharp.Core.Functions.L2Bin(System.Int32)' >L2Bin</seealso>
FUNCTION Bin2L(cSignedInt AS STRING) AS LONG
    LOCAL liResult := 0 AS LONG
    IF cSignedInt!= NULL .AND. cSignedInt:Length >= 4
        LOCAL aBytes AS BYTE[]
        aBytes := BYTE[]{4}
        aBytes[0] := (BYTE) _AND(cSignedInt:Chars[0], 0xFF)
        aBytes[1] := (BYTE) _AND(cSignedInt:Chars[1], 0xFF)
        aBytes[2] := (BYTE) _AND(cSignedInt:Chars[2], 0xFF)
        aBytes[3] := (BYTE) _AND(cSignedInt:Chars[3], 0xFF)
        liResult := BitConverter.ToInt32(aBytes, 0)
    ENDIF
    RETURN liResult


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/bin2logic/*" />
/// <seealso cref='M:XSharp.Core.Functions.LTOC(System.Boolean)' >LTOC</seealso>
/// <seealso cref='M:XSharp.Core.Functions.Logic2Bin(System.Boolean)' >Logic2Bin</seealso>
FUNCTION Bin2Logic(pszLogical AS STRING) AS LOGIC
    RETURN pszLogical != NULL .AND. pszLogical[0] != 0

/// <summary>
/// Convert a intptr to a string
/// </summary>
/// <include file="CoreComments.xml" path="Comments/PtrBin/*" />
/// <seealso cref='M:XSharp.Core.Functions.Bin2Ptr(System.String)' >Bin2Ptr</seealso>
FUNCTION Ptr2Bin(p AS IntPtr) AS STRING
    IF IntPtr.Size == 4
        RETURN L2Bin( p:ToInt32())
    ELSE
        THROW NotSupportedException{}
    ENDIF

/// <summary>
/// </summary>
/// <param name="cPointer"></param>
/// <include file="CoreComments.xml" path="Comments/PtrBin/*" />
/// <seealso cref='M:XSharp.Core.Functions.Ptr2Bin(System.IntPtr)' >Ptr2Bin</seealso>
FUNCTION Bin2Ptr(cPointer AS STRING) AS IntPtr
    IF IntPtr.Size == 4
        RETURN (IntPtr) Bin2L(cPointer)
    ELSE
        THROW NotSupportedException{}
    ENDIF

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/bin2real4/*" />
/// <seealso cref='M:XSharp.Core.Functions.Real42Bin(System.Single)' >Real42Bin</seealso>
/// <seealso cref='M:XSharp.Core.Functions.Real82Bin(System.Double)' >Real82Bin</seealso>

FUNCTION Bin2Real4(cFloat AS STRING) AS REAL4
    LOCAL r4Result := 0 AS REAL4
    IF cFloat!= NULL .AND. cFloat:Length >= 4
        LOCAL aBytes AS BYTE[]
        aBytes := BYTE[]{4}
        aBytes[0] := (BYTE) _AND(cFloat:Chars[0], 0xFF)
        aBytes[1] := (BYTE) _AND(cFloat:Chars[1], 0xFF)
        aBytes[2] := (BYTE) _AND(cFloat:Chars[2], 0xFF)
        aBytes[3] := (BYTE) _AND(cFloat:Chars[3], 0xFF)
        r4Result := BitConverter.ToSingle(aBytes, 0)
    ENDIF
    RETURN r4Result


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/bin2real8/*" />
/// <seealso cref='M:XSharp.Core.Functions.Real42Bin(System.Single)' >Real42Bin</seealso>
/// <seealso cref='M:XSharp.Core.Functions.Real82Bin(System.Double)' >Real82Bin</seealso>
FUNCTION Bin2Real8(cFloat AS STRING) AS REAL8
    LOCAL r8Result := 0 AS REAL8
    IF cFloat!= NULL .AND. cFloat:Length >= 8
        LOCAL aBytes AS BYTE[]
        aBytes := BYTE[]{8}
        aBytes[0] := (BYTE) _AND(cFloat:Chars[0], 0xFF)
        aBytes[1] := (BYTE) _AND(cFloat:Chars[1], 0xFF)
        aBytes[2] := (BYTE) _AND(cFloat:Chars[2], 0xFF)
        aBytes[3] := (BYTE) _AND(cFloat:Chars[3], 0xFF)
        aBytes[4] := (BYTE) _AND(cFloat:Chars[4], 0xFF)
        aBytes[5] := (BYTE) _AND(cFloat:Chars[5], 0xFF)
        aBytes[6] := (BYTE) _AND(cFloat:Chars[6], 0xFF)
        aBytes[7] := (BYTE) _AND(cFloat:Chars[7], 0xFF)
        r8Result := BitConverter.ToDouble(aBytes, 0)
    ENDIF
    RETURN r8Result

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/bin2w/*" />
/// <seealso cref='M:XSharp.Core.Functions.W2Bin(System.UInt16)' >W2Bin</seealso>
/// <seealso cref='M:XSharp.Core.Functions.DW2Bin(System.UInt32)' >DW2Bin</seealso>
FUNCTION Bin2W(cUnsignedInt AS STRING) AS WORD
    LOCAL wResult := 0 AS WORD
    IF cUnsignedInt!= NULL .AND. cUnsignedInt:Length >= 2
        LOCAL aBytes AS BYTE[]
        aBytes := BYTE[]{2}
        aBytes[0] := (BYTE) _AND(cUnsignedInt:Chars[0], 0xFF)
        aBytes[1] := (BYTE) _AND(cUnsignedInt:Chars[1], 0xFF)
        wResult := BitConverter.ToUInt16(aBytes, 0)
    ENDIF
    RETURN wResult



/// <summary>
/// Convert a string value to a logic.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
/// <seealso cref='M:XSharp.Core.Functions.LTOC(System.Boolean)' >LTOC</seealso>
/// <seealso cref='M:XSharp.Core.Functions.Logic2Bin(System.Boolean)' >Logic2Bin</seealso>

FUNCTION CTOL(c AS STRING) AS LOGIC
    IF c != NULL 
        IF c[0] == 'T' .OR. c[0] == 't' .OR. c[0] == 'Y' .OR. c[0] =='y'
            RETURN TRUE	
        ENDIF
    ENDIF
    RETURN FALSE

/// <summary>Convert a string value to a hexadecimal string.</summary>
/// <param name="cSource">String to convert</param>
/// <param name="lAddSpace">When TRUE then the inidividual characters are separated with a space in the result string</param>
/// <returns>A string with the hex representation of the value</returns>
/// <example>
///  ? _C2Hex("abcdef", TRUE)                // 61 62 63 64 65 66<br/>
///  ? _C2Hex("abcdef", FALSE)               // 616263646566<br/>
/// </example>
/// <seealso cref='M:XSharp.RT.Functions.AsHexString(XSharp.__Usual)' >AsHexString</seealso>
/// <seealso cref='M:XSharp.Core.Functions.C2Hex(System.String)' >C2Hex</seealso>
FUNCTION _C2Hex(cSource AS STRING, lAddSpace as LOGIC) AS STRING
    LOCAL sb AS StringBuilder
    sb := StringBuilder{cSource:Length*2}
    FOREACH c AS CHAR IN cSource
        IF sb:Length > 0 .and. lAddSpace
            sb:Append(' ')
        ENDIF
        var s := String.Format("{0:X2}",(INT) c)
        sb:Append(s)
    NEXT
    RETURN sb:ToString()

/// <summary>Convert a string value to a hexadecimal string.</summary>
/// <param name="cSource">String to convert</param>
/// <returns>A string with the hex representation of the value</returns>
/// <example>
///  ? C2Hex("abcdef")               // 616263646566<br/>
/// </example>
/// <seealso cref='M:XSharp.RT.Functions.AsHexString(XSharp.__Usual)' >AsHexString</seealso>
/// <seealso cref='M:XSharp.Core.Functions._C2Hex(System.String,System.Boolean)' >_C2Hex</seealso>
FUNCTION C2Hex(cSource AS STRING) AS STRING
    RETURN _C2Hex(cSource, FALSE)

// helper function to convert bytes to string

INTERNAL FUNCTION _bytes2String(byteArray AS BYTE[]) AS STRING
    LOCAL sb AS StringBuilder
    sb := StringBuilder{}
    FOREACH VAR b IN byteArray
        sb:Append( (CHAR) b)
    NEXT
    RETURN sb:ToString()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dw2bin/*" />
/// <seealso cref='M:XSharp.Core.Functions.Bin2W(System.String)' >Bin2W</seealso>
/// <seealso cref='M:XSharp.Core.Functions.Bin2DW(System.String)' >Bin2DW</seealso>
FUNCTION DW2Bin(dwValue AS DWORD) AS STRING
    LOCAL byteArray := BitConverter.GetBytes( dwValue ) AS BYTE[]
    RETURN _bytes2String(byteArray)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/hibyte/*" />
FUNCTION HiByte(wValue AS WORD) AS BYTE
    LOCAL upper := Convert.ToByte(wValue >> 8) AS BYTE
    RETURN (BYTE) upper   

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/hiword/*" />
FUNCTION HiWord(dwValue AS DWORD) AS WORD
    LOCAL upper := Convert.ToUInt16(dwValue >> 16) AS WORD
    RETURN (WORD) upper


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/i2bin/*" />
/// <seealso cref='M:XSharp.Core.Functions.Bin2I(System.String)' >Bin2I</seealso>
FUNCTION I2Bin(siValue AS SHORT) AS STRING
    LOCAL byteArray := BitConverter.GetBytes( siValue ) AS BYTE[]
    RETURN _bytes2String(byteArray)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/l2bin/*" />
/// <seealso cref='M:XSharp.Core.Functions.Bin2L(System.String)' >Bin2L</seealso>
FUNCTION L2Bin(liValue AS LONG) AS STRING
    LOCAL byteArray := BitConverter.GetBytes( liValue ) AS BYTE[]
    RETURN _bytes2String(byteArray)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/lobyte/*" />
FUNCTION LoByte(wValue AS WORD) AS BYTE
    RETURN (BYTE) (wValue & 0x00FF)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/loword/*" />
FUNCTION LoWord(dwValue AS DWORD) AS WORD
    RETURN (WORD) (dwValue & 0xFFFF) 


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/logic2bin/*" />
/// <seealso cref='M:XSharp.Core.Functions.Bin2Logic(System.String)' >Bin2Logic</seealso>
/// <seealso cref='M:XSharp.Core.Functions.CTOL(System.String)' >CTOL</seealso>
FUNCTION Logic2Bin(lValue AS LOGIC) AS STRING
    IF lValue
        RETURN e"\x0001"
    ELSE
        RETURN e"\x0000"
    ENDIF


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ltoc/*" />
/// <seealso cref='M:XSharp.Core.Functions.Bin2Logic(System.String)' >Bin2Logic</seealso>
/// <seealso cref='M:XSharp.Core.Functions.CTOL(System.String)' >CTOL</seealso>
FUNCTION LTOC(lValue AS LOGIC) AS STRING
    IF lValue
        RETURN "T"
    ELSE
        RETURN "F"
    ENDIF




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/real42bin/*" />
/// <seealso cref='M:XSharp.Core.Functions.Bin2Real8(System.String)' >Bin2Real8</seealso>
/// <seealso cref='M:XSharp.Core.Functions.Bin2Real4(System.String)' >Bin2Real4</seealso>
FUNCTION Real42Bin(r4Value AS REAL4) AS STRING
    LOCAL byteArray := BitConverter.GetBytes( r4Value ) AS BYTE[]
    RETURN _bytes2String(byteArray)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/real82bin/*" />
/// <seealso cref='M:XSharp.Core.Functions.Bin2Real8(System.String)' >Bin2Real8</seealso>
/// <seealso cref='M:XSharp.Core.Functions.Bin2Real4(System.String)' >Bin2Real4</seealso>
FUNCTION Real82Bin(r8Value AS REAL8) AS STRING
    LOCAL byteArray := BitConverter.GetBytes( r8Value ) AS BYTE[]
    RETURN _bytes2String(byteArray)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/w2bin/*" />
/// <seealso cref='M:XSharp.Core.Functions.Bin2W(System.String)' >Bin2W</seealso>
/// <seealso cref='M:XSharp.Core.Functions.Bin2DW(System.String)' >Bin2DW</seealso>
FUNCTION W2Bin(wValue AS WORD) AS STRING
    LOCAL byteArray := BitConverter.GetBytes( wValue ) AS BYTE[]
    RETURN _bytes2String(byteArray)

/// <summary>
/// Convert a string containing a numeric value to a numeric data type.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION _Val(cNumber AS STRING) AS OBJECT
    IF String.IsNullOrEmpty(cNumber)
      RETURN 0
    ENDIF
    cNumber := cNumber:Trim():ToUpper()
    // find non numeric characters in cNumber and trim the field to that length
    VAR pos := 0
    VAR done := FALSE
    VAR hex  := FALSE
    VAR hasdec := FALSE
    VAR hasexp := FALSE
    VAR cDec := (CHAR) RuntimeState.DecimalSep
    
    IF cDec != '.'
        cNumber := cNumber:Replace('.', cDec) // VO behavior...
        cNumber := cNumber:Replace(cDec, '.')
    ENDIF
    FOREACH VAR c IN cNumber
        SWITCH c
        CASE '0'
        CASE '1'
        CASE '2'
        CASE '3'
        CASE '4'
        CASE '5'
        CASE '6'
        CASE '7'
        CASE '8'
        CASE '9'
        CASE '-'
        CASE '+'
            NOP
        CASE '.'
        CASE ','
            IF c == ',' .AND. cDec != ',' // Don't ask, VO...
                done := TRUE
            ELSEIF hasdec
                done := TRUE
            ELSE
                hasdec := TRUE
            ENDIF
        CASE 'A'
        CASE 'B'
        CASE 'C'
        CASE 'D'
        CASE 'F'
            IF !hex
                done := TRUE
            ENDIF
        CASE 'E'
            // exponentional notation only allowed if decimal separator was there
            IF hasdec
                hasexp := TRUE
            ELSE
                IF !hex
                    done := TRUE
                ENDIF
            ENDIF
        CASE 'L'	// LONG result
        CASE 'U'	// DWORD result
            done := TRUE
        CASE 'X'
            IF pos == 1
                hex := TRUE
            ELSE
                done := TRUE
            ENDIF
        OTHERWISE
            done := TRUE
        END SWITCH
        IF done
            EXIT
        ENDIF
        pos += 1
    NEXT
    IF pos < cNumber:Length
        cNumber := cNumber:SubString(0, pos)
    ENDIF
    IF cNumber:IndexOfAny(<CHAR> {'.'}) > -1
        LOCAL r8Result := 0 AS REAL8
        IF cDec != '.'
            cNumber := cNumber:Replace(cDec, '.')
        ENDIF
        VAR style := NumberStyles.Number
        IF hasexp
            style |= NumberStyles.AllowExponent
        ENDIF
        IF System.Double.TryParse(cNumber, style, ConversionHelpers.usCulture, OUT r8Result)
            RETURN r8Result
        ENDIF
    ELSE
        LOCAL iResult := 0 AS INT64
        LOCAL style AS NumberStyles
        IF hex
            cNumber := cNumber:Substring(2)
            style := NumberStyles.HexNumber
        ELSE
            style := NumberStyles.Integer
        ENDIF
        IF System.Int64.TryParse(cNumber, style, ConversionHelpers.usCulture, OUT iResult)
            IF iResult < Int32.MaxValue .AND. iResult > int32.MinValue
                RETURN (INT) iResult
            ENDIF
            RETURN iResult
        ENDIF
    ENDIF
    RETURN 0








