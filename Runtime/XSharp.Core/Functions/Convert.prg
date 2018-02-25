//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>
/// Convert a string containing a 32-bit unsigned integer to a double word.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2DW(c AS STRING) AS DWORD
	/// THROW NotImplementedException{}
	RETURN 0   

/// <summary>
/// Convert a string containing a 16-bit signed integer to a short integer.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2I(c AS STRING) AS SHORT
	/// THROW NotImplementedException{}
	RETURN 0

/// <summary>
/// Convert a string containing a 32-bit signed integer to a long integer.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2L(c AS STRING) AS LONG
	/// THROW NotImplementedException{}
	RETURN 0   

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
UNSAFE FUNCTION Bin2Ptr(c AS STRING) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero

/// <summary>
/// Convert a string containing a 32-bit __VOFloating point number to a Real4 value.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2Real4(c AS STRING) AS REAL4
	/// THROW NotImplementedException{}
	RETURN 0   


/// <summary>
/// Convert a string containing a 32-bit __VOFloating point number to a Real8 value.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2REAL8(c AS STRING) AS REAL8
	/// THROW NotImplementedException{}
	RETURN 0   

/// <summary>
/// Convert a string containing a 16-bit unsigned integer to a word.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2W(c AS STRING) AS WORD
	/// THROW NotImplementedException{}
	RETURN 0   


/// <summary>
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION C2Hex(c AS STRING) AS STRING
	/// THROW NotImplementedException{}
	RETURN String.Empty   

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
	LOCAL upper := Convert.ToByte(dw >> 16) AS WORD
	RETURN (WORD) upper


/// <summary>
/// Convert a short integer to a string containing a 16-bit signed integer.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION I2Bin(n AS SHORT) AS STRING
	LOCAL byteArray := BitConverter.GetBytes( n ) AS BYTE[]
	RETURN System.Text.Encoding.ASCII:GetString(byteArray)  



/// <summary>
/// Convert a long integer to a string containing a 32-bit signed integer.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION L2Bin(n AS LONG) AS STRING
	LOCAL byteArray := BitConverter.GetBytes( n ) AS BYTE[]
	RETURN System.Text.Encoding.ASCII:GetString(byteArray)     



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
/// Convert a Real4 value to a string containing a 32-bit __VOFloating point number.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION Real42Bin(n AS REAL4) AS STRING
	LOCAL byteArray := BitConverter.GetBytes( n ) AS BYTE[]
	RETURN System.Text.Encoding.ASCII:GetString(byteArray)        

/// <summary>
/// Convert a Real8 value to a string containing an 8-byte __VOFloating point number.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION Real82Bin(n AS REAL8) AS STRING
	LOCAL byteArray := BitConverter.GetBytes( n ) AS BYTE[]
	RETURN System.Text.Encoding.ASCII:GetString(byteArray)   




	/// <summary>
/// Convert a word to a string containing a 16-bit unsigned integer.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION W2Bin(n AS WORD) AS STRING
	LOCAL byteArray := BitConverter.GetBytes( n ) AS BYTE[]
	RETURN System.Text.Encoding.ASCII:GetString(byteArray)    