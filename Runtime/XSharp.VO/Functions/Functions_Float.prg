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
FUNCTION Abs__VOFloat(f AS __VOFloat) AS __VOFloat
RETURN __VOFloat{Math.Abs(f:Value)}



/// <summary>
/// Convert a string containing a 80-bit __VOFloating point number to a __VOFloat value.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2F(c AS STRING) AS __VOFloat
	/// THROW NotImplementedException{}
RETURN 0   


/// <summary>
/// Convert a __VOFloat to a string containing an 80-bit __VOFloating point number.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION F2Bin(f AS __VOFloat) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty

/// <summary>
/// Calculate the factorial of a number.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION Fact(n AS DWORD) AS __VOFloat
	local result := 1 as double
	if  n > 0
		local i as dword
		for i := 1 upto n
			result := result * i
		next
	endif
RETURN __VOFloat{result}


/// <summary>
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
FUNCTION FClone(o AS __VOFloat) AS __VOFloat
	RETURN __VoFloat{o:Value, o:Digits, o:Decimals}



/// <summary>
/// Set the display format for a __VOFloating point numeric.
/// </summary>
/// <param name="f"></param>
/// <param name="nLen"></param>
/// <param name="nDec"></param>
/// <returns>
/// </returns>
FUNCTION FloatFormat(f AS __VOFloat,nLen AS INT,nDec AS INT) AS __VOFloat
	/// THROW NotImplementedException{}
	RETURN __VoFloat{f:Value, nLen, nDec}  

/// <summary>
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION FloatNext(f AS __VOFloat) AS __VOFloat
	/// THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// Return the fractional portion of a number.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION Frac(f AS __VOFloat) AS __VOFloat
	/// THROW NotImplementedException{}
RETURN 0   


/// <summary>
/// </summary>
/// <param name="xd"></param>
/// <param name="wDec"></param>
/// <returns>
/// </returns>
FUNCTION MyDalFloatVal(xd AS REAL8,wDec AS WORD) AS __VOFloat
	/// THROW NotImplementedException{}
RETURN 0   


	/// <summary>
	/// Return and optionally change the setting that determines the point at which 2 floating point numbers would be considered equal even though they are different.
	/// </summary>
	/// <param name="fDelta"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetFloatDelta(fDelta AS OBJECT) AS __VoFLoat
		/// THROW NotImplementedException{}
	RETURN 0   




/// <summary>
/// Convert a numeric expression to a string.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION Str1(f AS __VOFloat) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

/// <summary>
/// Convert a numeric expression to a string of a specified length.
/// </summary>
/// <param name="f"></param>
/// <param name="dwLen"></param>
/// <returns>
/// </returns>
FUNCTION Str2(f AS __VOFloat,dwLen AS DWORD) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

/// <summary>
/// Convert a numeric expression to a string of specific length and decimal places.
/// </summary>
/// <param name="f"></param>
/// <param name="dwLen"></param>
/// <param name="dwDec"></param>
/// <returns>
/// </returns>
FUNCTION Str3(f AS __VOFloat,dwLen AS DWORD,dwDec AS DWORD) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   



/// <summary>
/// </summary>
/// <param name="c"></param>
/// <param name="dwRadix"></param>
/// <returns>
/// </returns>
FUNCTION StrToFloat(c AS STRING,dwRadix AS DWORD) AS __VOFloat
	/// THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <param name="dwRadix"></param>
/// <returns>
/// </returns>
FUNCTION StrToLong(c AS STRING,dwRadix AS DWORD) AS __VOFloat
	/// THROW NotImplementedException{}
RETURN 0   


