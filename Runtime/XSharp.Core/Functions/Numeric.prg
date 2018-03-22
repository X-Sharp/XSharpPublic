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




 
