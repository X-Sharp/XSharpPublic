//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp
/// <summary>
/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION AbsFloat(f AS Float) AS Float
RETURN Float{Math.Abs(f:Value)}



/// <summary>
/// Convert a string containing a 80-bit Floating point number to a Float value.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2F(c AS STRING) AS Float
	/// THROW NotImplementedException{}
RETURN 0   


/// <summary>
/// Convert a Float to a string containing an 80-bit Floating point number.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION F2Bin(f AS Float) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty

/// <summary>
/// Calculate the factorial of a number.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION Fact(n AS DWORD) AS Float
	local result := 1 as double
	if  n > 0
		local i as dword
		for i := 1 upto n
			result := result * i
		next
	endif
RETURN Float{result}


/// <summary>
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
function FClone(o as float) as Float
	// no need to clone. Value type
	RETURN o



/// <summary>
/// Set the display format for a Floating point numeric.
/// </summary>
/// <param name="f"></param>
/// <param name="nLen"></param>
/// <param name="nDec"></param>
/// <returns>
/// </returns>
FUNCTION FloatFormat(f AS Float,nLen AS INT,nDec AS INT) AS Float
	if nDec < 0
		nDec := f:Decimals
	endif
	if nLen < 0
		// determine length by creating new float and converting it to string
		// not very efficient but this is what VO does
		local nDigits as int
		nDigits := f:Digits
		if nDigits < 0
			nDigits := RuntimeState.Digits
		endif
		local fTemp as float
		fTemp := float{f:value, nDigits, nDec}
		var cTemp := Ntrim(fTemp)
		nLen := cTemp:Length
	elseif nDec != 0 .and. nLen != 0 .and. nLen < nDec +2
		nLen := nDec + 2
	endif
	RETURN Float{f:Value, nLen, nDec}  

/// <summary>
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION FloatNext(f AS Float) AS Float
	/// THROW NotImplementedException{}
RETURN 0   


/// <summary>
/// </summary>
/// <param name="uValue"></param>
/// <param name="dwLen"></param>
/// <param name="dwDec"></param>
/// <returns>
/// </returns>
UNSAFE FUNCTION Float2Str(uValue AS Usual,dwLen AS DWORD,dwDec AS DWORD) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty
		


/// <summary>
/// Return the fractional portion of a number.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION Frac(f AS Float) AS Float
	/// THROW NotImplementedException{}
RETURN 0   


/// <summary>
/// </summary>
/// <param name="xd"></param>
/// <param name="wDec"></param>
/// <returns>
/// </returns>
FUNCTION MyDalFloatVal(xd AS REAL8,wDec AS WORD) AS Float
	/// THROW NotImplementedException{}
RETURN 0   


	/// <summary>
	/// Return and optionally change the setting that determines the point at which 2 floating point numbers would be considered equal even though they are different.
	/// </summary>
	/// <param name="fDelta"></param>
	/// <returns>
	/// </returns>
FUNCTION SetFloatDelta(fDelta AS Real8) AS Real8
	var result := RuntimeState.FloatDelta
	RuntimeState.FloatDelta := fDelta
	return result

FUNCTION SetFloatDelta() AS Real8
	return RuntimeState.FloatDelta






