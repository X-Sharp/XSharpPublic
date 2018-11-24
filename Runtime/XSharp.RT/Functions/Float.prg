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
FUNCTION AbsFloat(f AS FLOAT) AS FLOAT
	RETURN FLOAT{Math.Abs(f:Value)}


/// <summary>
/// Calculate the factorial of a number.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION Fact(n AS DWORD) AS FLOAT
	LOCAL result := 1 AS double
	IF  n > 0
		LOCAL i AS DWORD
		FOR i := 1 UPTO n
			result := result * i
		NEXT
	ENDIF
RETURN FLOAT{result}


/// <summary>
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
FUNCTION FClone(o AS FLOAT) AS FLOAT
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
FUNCTION FloatFormat(f AS FLOAT,nLen AS INT,nDec AS INT) AS FLOAT
	IF nDec < 0
		nDec := f:Decimals
	ENDIF
	IF nLen < 0
		// determine length by creating new float and converting it to string
		// not very efficient but this is what VO does
		LOCAL nDigits AS INT
		nDigits := f:Digits
		IF nDigits < 0 
			nDigits := (SHORT) RuntimeState.Digits
		ENDIF
		LOCAL fTemp AS FLOAT
		fTemp := FLOAT{f:value, nDigits, nDec}
		VAR cTemp := Ntrim(fTemp)
		nLen := cTemp:Length
	ELSEIF nDec != 0 .AND. nLen != 0 .AND. nLen < nDec +2
		nLen := nDec + 2
	ENDIF
	RETURN FLOAT{f:Value, nLen, nDec}  


/// <summary>
/// Return the fractional portion of a number.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION Frac(f AS FLOAT) AS FLOAT
	RETURN f - Integer(f)



/// <summary>
/// </summary>
/// <param name="xd"></param>
/// <param name="wDec"></param>
/// <returns>
/// </returns>
FUNCTION MyDalFloatVal(xd AS REAL8,wDec AS WORD) AS FLOAT
	RETURN FLOAT{xd, wDec}


/// <summary>
/// Return and change the setting that determines the point at which 2 floating point numbers would be considered equal even though they are different.
/// </summary>
/// <param name="fDelta"></param>
/// <returns>
/// </returns>
FUNCTION SetFloatDelta(fDelta AS REAL8) AS REAL8
	VAR result := RuntimeState.FloatDelta
	RuntimeState.FloatDelta := fDelta
	RETURN result

/// <summary>
/// Return the setting that determines the point at which 2 floating point numbers would be considered equal even though they are different.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetFloatDelta() AS REAL8
	RETURN RuntimeState.FloatDelta






