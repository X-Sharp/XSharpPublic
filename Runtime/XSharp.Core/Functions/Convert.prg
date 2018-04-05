//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Text
/// <summary>
/// Convert a string containing a 32-bit unsigned integer to a double word.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function Bin2DW(c as string) as dword
	local dwResult := 0 as dword
	if c!= null .and. c:Length >= 4
		local aBytes as byte[]
		aBytes := byte[]{4}
		aBytes[1] := (byte) _AND(c:Chars[0], 0xFF)
		aBytes[2] := (byte) _AND(c:Chars[1], 0xFF)
		aBytes[3] := (byte) _AND(c:Chars[2], 0xFF)
		aBytes[4] := (byte) _AND(c:Chars[3], 0xFF)
		dwResult := BitConverter.ToUInt32(aBytes, 0)
	endif
	return dwResult

/// <summary>
/// Convert a string containing a 16-bit signed integer to a short integer.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function Bin2I(c as string) as short
	local siResult := 0 as short
	if c!= null .and. c:Length >= 2
		local aBytes as byte[]
		aBytes := byte[]{2}
		aBytes[1] := (byte) _AND(c:Chars[0], 0xFF)
		aBytes[2] := (byte) _AND(c:Chars[1], 0xFF)
		siResult := BitConverter.ToInt16(aBytes, 0)
	endif
	return siResult

/// <summary>
/// Convert a string containing a 32-bit signed integer to a long integer.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function Bin2L(c as string) as long
	local liResult := 0 as long
	if c!= null .and. c:Length >= 4
		local aBytes as byte[]
		aBytes := byte[]{4}
		aBytes[1] := (byte) _AND(c:Chars[0], 0xFF)
		aBytes[2] := (byte) _AND(c:Chars[1], 0xFF)
		aBytes[3] := (byte) _AND(c:Chars[2], 0xFF)
		aBytes[4] := (byte) _AND(c:Chars[3], 0xFF)
		liResult := BitConverter.ToInt32(aBytes, 0)
	endif
	return liResult


/// <summary>
/// Convert a string containing an 8-bit logical into a logical value.
/// </summary>
/// <param name="pszC"></param>
/// <returns>
/// </returns>
function Bin2Logic(c as string) as logic
	return c != null .and. c[0] != 0

FUNCTION Ptr2Bin(p AS IntPtr) AS STRING
	RETURN L2Bin( p:ToInt32())

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function Bin2Ptr(c as string) as IntPtr
return (IntPtr) Bin2L(c)

/// <summary>
/// Convert a string containing a 32-bit Floating point number to a Real4 value.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function Bin2Real4(c as string) as real4
	local r4Result := 0 as real4
	if c!= null .and. c:Length >= 4
		local aBytes as byte[]
		aBytes := byte[]{4}
		aBytes[1] := (byte) _AND(c:Chars[0], 0xFF)
		aBytes[2] := (byte) _AND(c:Chars[1], 0xFF)
		aBytes[3] := (byte) _AND(c:Chars[2], 0xFF)
		aBytes[4] := (byte) _AND(c:Chars[3], 0xFF)
		r4Result := BitConverter.ToSingle(aBytes, 0)
	endif
	return r4Result


/// <summary>
/// Convert a string containing a 32-bit Floating point number to a Real8 value.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function Bin2REAL8(c as string) as real8
	local r8Result := 0 as real8
	if c!= null .and. c:Length >= 8
		local aBytes as byte[]
		aBytes := byte[]{8}
		aBytes[1] := (byte) _AND(c:Chars[0], 0xFF)
		aBytes[2] := (byte) _AND(c:Chars[1], 0xFF)
		aBytes[3] := (byte) _AND(c:Chars[2], 0xFF)
		aBytes[4] := (byte) _AND(c:Chars[3], 0xFF)
		aBytes[5] := (byte) _AND(c:Chars[4], 0xFF)
		aBytes[6] := (byte) _AND(c:Chars[5], 0xFF)
		aBytes[7] := (byte) _AND(c:Chars[6], 0xFF)
		aBytes[8] := (byte) _AND(c:Chars[7], 0xFF)
		r8Result := BitConverter.ToDouble(aBytes, 0)
	endif
	return r8Result

/// <summary>
/// Convert a string containing a 16-bit unsigned integer to a word.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function Bin2W(c as string) as word
	local wResult := 0 as word
	if c!= null .and. c:Length >= 2
		local aBytes as byte[]
		aBytes := byte[]{2}
		aBytes[1] := (byte) _AND(c:Chars[0], 0xFF)
		aBytes[2] := (byte) _AND(c:Chars[1], 0xFF)
		wResult := BitConverter.ToUInt16(aBytes, 0)
	endif
	return wResult



/// <summary>
/// Convert a string value to a logic.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function CTOL(c as string) as logic
	if c != null 
		if c[0] == 'T' .or. c[0] == 't' .or. c[0] == 'Y' .or. c[0] =='y'
			return true	
		endif
	endif
	return false


function _GetHexChar(c as char, c1 out char, c2 out char) as string
	local s as string
	s := String.Format("{0:X2}",(int) c)
	c1 := s[0]
	c2 := s[1]
	return s

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function C2Hex(cSource as string) as string
	local sb as StringBuilder
	sb := StringBuilder{cSource:Length*2}
	foreach c as char in cSource
		local c1, c2 as char
		_GetHexChar(c, out c1, out c2)
		sb:Append(c1)
		sb:Append(c2)
	next
	return sb:ToString()

// helper function to convert bytes to string

function _bytes2String(byteArray as byte[]) as string
	local sb as StringBuilder
	sb := StringBuilder{}
	foreach var b in byteArray
		sb:Append( (char) b)
	next
	return sb:ToString()

/// <summary>
/// Convert a double word to a string containing a 32-bit unsigned integer.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
function DW2Bin(n as dword) as string
	local byteArray := BitConverter.GetBytes( n ) as byte[]
	return _bytes2String(byteArray)

/// <summary>
/// Return the high-order (leftmost) byte in a number.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
function HiByte(dw as word) as byte
	local upper := Convert.ToByte(dw >> 8) as byte
	return (byte) upper   

/// <summary>
/// Return the high-order (leftmost) word in a number.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
function HiWord(dw as dword) as word
	local upper := Convert.ToUInt16(dw >> 16) as word
	return (word) upper


/// <summary>
/// Convert a short integer to a string containing a 16-bit signed integer.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
function I2Bin(n as short) as string
	local byteArray := BitConverter.GetBytes( n ) as byte[]
	return _bytes2String(byteArray)



/// <summary>
/// Convert a long integer to a string containing a 32-bit signed integer.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
function L2Bin(n as long) as string
	local byteArray := BitConverter.GetBytes( n ) as byte[]
	return _bytes2String(byteArray)


/// <summary>
/// Return the low-order (rightmost) byte in a number.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
function LoByte(dw as word) as byte
	return (byte) (dw & 0x00FF)

/// <summary>
/// Return the low-order (rightmost) word in a number.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
function LoWord(dw as dword) as word
	return (word) (dw & 0xFFFF) 


/// <summary>
/// Convert a logical value to a string containing an 8-bit logical value.
/// </summary>
/// <param name="l"></param>
/// <returns>
/// </returns>
function Logic2Bin(l as logic) as string
	if l
		return e"\x0001"
	else
		return e"\x0000"
	endif


/// <summary>
/// Convert a logical value to a string.
/// </summary>
/// <param name="l"></param>
/// <returns>
/// </returns>
function LTOC(l as logic) as string
	if l
		return "T"
	else
		return "F"
	endif




/// <summary>
/// Convert a Real4 value to a string containing a 32-bit Floating point number.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
function Real42Bin(n as real4) as string
	local byteArray := BitConverter.GetBytes( n ) as byte[]
	return _bytes2String(byteArray)

/// <summary>
/// Convert a Real8 value to a string containing an 8-byte Floating point number.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
function Real82Bin(n as real8) as string
	local byteArray := BitConverter.GetBytes( n ) as byte[]
	return _bytes2String(byteArray)




/// <summary>
/// Convert a word to a string containing a 16-bit unsigned integer.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
function W2Bin(n as word) as string
	local byteArray := BitConverter.GetBytes( n ) as byte[]
	return _bytes2String(byteArray)