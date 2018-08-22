//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Text
/// <summary>
/// Convert a string containing a 32-bit unsigned integer to a double word.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
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

/// <summary>
/// Convert a string containing a 32-bit signed integer to a long integer.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2L(c AS STRING) AS LONG
	LOCAL liResult := 0 AS LONG
	IF c!= NULL .AND. c:Length >= 4
		LOCAL aBytes AS BYTE[]
		aBytes := BYTE[]{4}
		aBytes[0] := (BYTE) _AND(c:Chars[0], 0xFF)
		aBytes[1] := (BYTE) _AND(c:Chars[1], 0xFF)
		aBytes[2] := (BYTE) _AND(c:Chars[2], 0xFF)
		aBytes[3] := (BYTE) _AND(c:Chars[3], 0xFF)
		liResult := BitConverter.ToInt32(aBytes, 0)
	ENDIF
	RETURN liResult


/// <summary>
/// Convert a string containing an 8-bit logical into a logical value.
/// </summary>
/// <param name="pszC"></param>
/// <returns>
/// </returns>
FUNCTION Bin2Logic(c AS STRING) AS LOGIC
	RETURN c != NULL .AND. c[0] != 0

FUNCTION Ptr2Bin(p AS IntPtr) AS STRING
	RETURN L2Bin( p:ToInt32())

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2Ptr(c AS STRING) AS IntPtr
RETURN (IntPtr) Bin2L(c)

/// <summary>
/// Convert a string containing a 32-bit Floating point number to a Real4 value.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2Real4(c AS STRING) AS REAL4
	LOCAL r4Result := 0 AS REAL4
	IF c!= NULL .AND. c:Length >= 4
		LOCAL aBytes AS BYTE[]
		aBytes := BYTE[]{4}
		aBytes[0] := (BYTE) _AND(c:Chars[0], 0xFF)
		aBytes[1] := (BYTE) _AND(c:Chars[1], 0xFF)
		aBytes[2] := (BYTE) _AND(c:Chars[2], 0xFF)
		aBytes[3] := (BYTE) _AND(c:Chars[3], 0xFF)
		r4Result := BitConverter.ToSingle(aBytes, 0)
	ENDIF
	RETURN r4Result


/// <summary>
/// Convert a string containing a 32-bit Floating point number to a Real8 value.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2REAL8(c AS STRING) AS REAL8
	LOCAL r8Result := 0 AS REAL8
	IF c!= NULL .AND. c:Length >= 8
		LOCAL aBytes AS BYTE[]
		aBytes := BYTE[]{8}
		aBytes[0] := (BYTE) _AND(c:Chars[0], 0xFF)
		aBytes[1] := (BYTE) _AND(c:Chars[1], 0xFF)
		aBytes[2] := (BYTE) _AND(c:Chars[2], 0xFF)
		aBytes[3] := (BYTE) _AND(c:Chars[3], 0xFF)
		aBytes[4] := (BYTE) _AND(c:Chars[4], 0xFF)
		aBytes[5] := (BYTE) _AND(c:Chars[5], 0xFF)
		aBytes[6] := (BYTE) _AND(c:Chars[6], 0xFF)
		aBytes[7] := (BYTE) _AND(c:Chars[7], 0xFF)
		r8Result := BitConverter.ToDouble(aBytes, 0)
	ENDIF
	RETURN r8Result

/// <summary>
/// Convert a string containing a 16-bit unsigned integer to a word.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2W(c AS STRING) AS WORD
	LOCAL wResult := 0 AS WORD
	IF c!= NULL .AND. c:Length >= 2
		LOCAL aBytes AS BYTE[]
		aBytes := BYTE[]{2}
		aBytes[0] := (BYTE) _AND(c:Chars[0], 0xFF)
		aBytes[1] := (BYTE) _AND(c:Chars[1], 0xFF)
		wResult := BitConverter.ToUInt16(aBytes, 0)
	ENDIF
	RETURN wResult



/// <summary>
/// Convert a string value to a logic.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION CTOL(c AS STRING) AS LOGIC
	IF c != NULL 
		IF c[0] == 'T' .OR. c[0] == 't' .OR. c[0] == 'Y' .OR. c[0] =='y'
			RETURN TRUE	
		ENDIF
	ENDIF
	RETURN FALSE


FUNCTION _GetHexChar(c AS CHAR, c1 OUT CHAR, c2 OUT CHAR) AS STRING
	LOCAL s AS STRING
	s := String.Format("{0:X2}",(INT) c)
	c1 := s[0]
	c2 := s[1]
	RETURN s

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION C2Hex(cSource AS STRING) AS STRING
	LOCAL sb AS StringBuilder
	sb := StringBuilder{cSource:Length*2}
	FOREACH c AS CHAR IN cSource
		LOCAL c1, c2 AS CHAR
		_GetHexChar(c, OUT c1, OUT c2)
		sb:Append(c1)
		sb:Append(c2)
	NEXT
	RETURN sb:ToString()

// helper function to convert bytes to string

INTERNAL FUNCTION _bytes2String(byteArray AS BYTE[]) AS STRING
	LOCAL sb AS StringBuilder
	sb := StringBuilder{}
	FOREACH VAR b IN byteArray
		sb:Append( (CHAR) b)
	NEXT
	RETURN sb:ToString()

/// <summary>
/// Convert a double word to a string containing a 32-bit unsigned integer.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION DW2Bin(n AS DWORD) AS STRING
	LOCAL byteArray := BitConverter.GetBytes( n ) AS BYTE[]
	RETURN _bytes2String(byteArray)

/// <summary>
/// Return the high-order (leftmost) byte in a number.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
FUNCTION HiByte(dw AS WORD) AS BYTE
	LOCAL upper := Convert.ToByte(dw >> 8) AS BYTE
	RETURN (BYTE) upper   

/// <summary>
/// Return the high-order (leftmost) word in a number.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
FUNCTION HiWord(dw AS DWORD) AS WORD
	LOCAL upper := Convert.ToUInt16(dw >> 16) AS WORD
	RETURN (WORD) upper


/// <summary>
/// Convert a short integer to a string containing a 16-bit signed integer.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION I2Bin(n AS SHORT) AS STRING
	LOCAL byteArray := BitConverter.GetBytes( n ) AS BYTE[]
	RETURN _bytes2String(byteArray)



/// <summary>
/// Convert a long integer to a string containing a 32-bit signed integer.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION L2Bin(n AS LONG) AS STRING
	LOCAL byteArray := BitConverter.GetBytes( n ) AS BYTE[]
	RETURN _bytes2String(byteArray)


/// <summary>
/// Return the low-order (rightmost) byte in a number.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
FUNCTION LoByte(dw AS WORD) AS BYTE
	RETURN (BYTE) (dw & 0x00FF)

/// <summary>
/// Return the low-order (rightmost) word in a number.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
FUNCTION LoWord(dw AS DWORD) AS WORD
	RETURN (WORD) (dw & 0xFFFF) 


/// <summary>
/// Convert a logical value to a string containing an 8-bit logical value.
/// </summary>
/// <param name="l"></param>
/// <returns>
/// </returns>
FUNCTION Logic2Bin(l AS LOGIC) AS STRING
	IF l
		RETURN e"\x0001"
	ELSE
		RETURN e"\x0000"
	ENDIF


/// <summary>
/// Convert a logical value to a string.
/// </summary>
/// <param name="l"></param>
/// <returns>
/// </returns>
FUNCTION LTOC(l AS LOGIC) AS STRING
	IF l
		RETURN "T"
	ELSE
		RETURN "F"
	ENDIF




/// <summary>
/// Convert a Real4 value to a string containing a 32-bit Floating point number.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION Real42Bin(n AS REAL4) AS STRING
	LOCAL byteArray := BitConverter.GetBytes( n ) AS BYTE[]
	RETURN _bytes2String(byteArray)

/// <summary>
/// Convert a Real8 value to a string containing an 8-byte Floating point number.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION Real82Bin(n AS REAL8) AS STRING
	LOCAL byteArray := BitConverter.GetBytes( n ) AS BYTE[]
	RETURN _bytes2String(byteArray)




/// <summary>
/// Convert a word to a string containing a 16-bit unsigned integer.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION W2Bin(n AS WORD) AS STRING
	LOCAL byteArray := BitConverter.GetBytes( n ) AS BYTE[]
	RETURN _bytes2String(byteArray)
