//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>
/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
/// </summary>
/// <param name="i"></param>
/// <returns>
/// </returns>
FUNCTION AbsInt(i AS LONGINT) AS LONG
	RETURN Math.Abs(i)

/// <summary>
/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
/// </summary>
/// <param name="li"></param>
/// <returns>
/// </returns>
FUNCTION AbsLong(li AS LONGINT) AS LONG
	RETURN Math.Abs(li)

/// <summary>
/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
/// </summary>
/// <param name="r4"></param>
/// <returns>
/// </returns>
FUNCTION AbsReal4(r4 AS REAL4) AS REAL4
	RETURN Math.Abs(r4)

/// <summary>
/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
/// </summary>
/// <param name="r8"></param>
/// <returns>
/// </returns>
FUNCTION AbsReal8(r8 AS REAL8) AS REAL8
	RETURN Math.Abs(r8)

/// <summary>
/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
/// </summary>
/// <param name="si"></param>
/// <returns>
/// </returns>
FUNCTION AbsShort(si AS SHORT) AS LONG
	RETURN Math.Abs(si)


/// <summary>
/// Return an uninitialized string of a specified size.
/// </summary>
/// <param name="dwSize"></param>
/// <returns>
/// </returns>
FUNCTION Buffer(dwSize AS DWORD) AS STRING
	/// THROW NotImplementedException{}
	RETURN String.Empty   



/// <summary>
/// Return a description string for a DOS error number.
/// </summary>
/// <param name="nDosErr"></param>
/// <returns>
/// </returns>
FUNCTION DosErrString(nDosErr AS DWORD) AS STRING
	/// THROW NotImplementedException{}
	RETURN String.Empty   

/// <summary>
/// Convert a double word to a string containing a 32-bit unsigned integer.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION DW2Bin(n AS DWORD) AS STRING
	LOCAL byteArray := BitConverter.GetBytes( n ) AS BYTE[]
	RETURN System.Text.Encoding.ASCII:GetString(byteArray)

 

/// <summary>
/// Return an error message associated with a system-generated error code.
/// </summary>
/// <param name="nGenCode"></param>
/// <returns>
/// </returns>
FUNCTION ErrString(nGenCode AS DWORD) AS STRING
	/// THROW NotImplementedException{}
	RETURN String.Empty   



/// <summary>
/// Display file attributes as a string.
/// </summary>
/// <param name="nAttrib"></param>
/// <returns>
/// </returns>
FUNCTION FAttr2String(nAttrib AS DWORD) AS STRING
	/// THROW NotImplementedException{}
	RETURN String.Empty   


/// <summary>
/// </summary>
/// <param name="dwInst"></param>
/// <returns>
/// </returns>
FUNCTION NationInit(dwInst AS DWORD) AS INT
	/// THROW NotImplementedException{}
	RETURN 0   

/// <summary>
/// Exchange the right and left halves of a byte.
/// </summary>
/// <param name="b">The byte whose nibbles should be swaped.</param>
/// <returns>
/// New value with the nibbles swapped.
/// </returns>
FUNCTION SwapByte(b AS BYTE) AS WORD
	RETURN ((b & 0x0f) << 4) | ((b >> 4) & 0x0f)

/// <summary>
/// Exchange the right and left halves of a double word.
/// </summary>
/// <param name="li"></param>
/// <returns>
/// </returns>
FUNCTION SwapDWord(li AS DWORD) AS DWORD
RETURN	 (((li & 0x0000ffff) << 16) | ((li >> 16) & 0x0000ffff))   

/// <summary>
/// Exchange the right and left halves of an integer.
/// </summary>
/// <param name="li"></param>
/// <returns>
/// </returns>
FUNCTION SwapInt(li AS LONG) AS LONG
	RETURN SwapLong(li) 

/// <summary>
/// Exchange the right and left halves of a long integer.
/// </summary>
/// <param name="li"></param>
/// <returns>
/// </returns>
FUNCTION SwapLong(li AS LONG) AS LONG
RETURN	 ((LONG)((li & 0x0000ffff) << 16) | ((li >> 16) & 0x0000ffff))

/// <summary>
/// Exchange the right and left halves of a short integer.
/// </summary>
/// <param name="si"></param>
/// <returns>
/// </returns>
FUNCTION SwapShort(si AS SHORT) AS SHORT
RETURN	 ((short)((si & 0x00ff) << 8) | ((si >> 8) & 0x00ff))

/// <summary>
/// Exchange the right and left halves of a word.
/// </summary>
/// <param name="w"></param>
/// <returns>
/// </returns>
FUNCTION SwapWord(w AS WORD) AS WORD
RETURN ((WORD)((w & 0x00ff) << 8) | ((w >> 8) & 0x00ff))


/// <summary>
/// </summary>
/// <param name="dwType"></param>
/// <returns>
/// </returns>
FUNCTION TypeString(dwType AS DWORD) AS STRING
	/// THROW NotImplementedException{}
	RETURN String.Empty   



 
