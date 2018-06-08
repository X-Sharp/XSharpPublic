//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>
/// Return the absolute value of a strongly typed numeric expression, regardless of its sign.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION AbsFloat(f AS Float) AS Float
	RETURN Float{Math.Abs(f:Value)}


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
			nDigits := (short) RuntimeState.Digits
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
/// Return the fractional portion of a number.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION Frac(f AS Float) AS Float
	return f - Integer(f)



/// <summary>
/// </summary>
/// <param name="xd"></param>
/// <param name="wDec"></param>
/// <returns>
/// </returns>
FUNCTION MyDalFloatVal(xd AS REAL8,wDec AS WORD) AS Float
	RETURN Float{xd, wDec}


/// <summary>
/// Return and change the setting that determines the point at which 2 floating point numbers would be considered equal even though they are different.
/// </summary>
/// <param name="fDelta"></param>
/// <returns>
/// </returns>
FUNCTION SetFloatDelta(fDelta AS Real8) AS Real8
	var result := RuntimeState.FloatDelta
	RuntimeState.FloatDelta := fDelta
	return result

/// <summary>
/// Return the setting that determines the point at which 2 floating point numbers would be considered equal even though they are different.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetFloatDelta() AS Real8
	return RuntimeState.FloatDelta






