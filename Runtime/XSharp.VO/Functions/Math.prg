//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <summary>
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION Float2Long(f AS USUAL) AS LONG
	LOCAL flValue AS FLOAT
	flValue := (FLOAT) f
	RETURN (LONG) flValue
	
	/// <summary>
	/// Return the absolute value of a numeric expression, regardless of its sign.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
FUNCTION Abs(n AS USUAL) AS USUAL
	LOCAL uRet AS USUAL
	IF n:IsInt64
		uRet := Math.Abs( (INT64) n )
	ELSEIF n:IsDecimal
		uRet := Math.Abs( (Decimal) n)
	ELSEIF n:isInteger
		uRet := Math.Abs( (INT) n )
	ELSEIF n:IsFLoat
		uRet := AbsFloat( n )
	ELSE
		THROW  Error.ArgumentError( __ENTITY__, NAMEOF(n), "Argument not numeric",1)
	ENDIF
	RETURN uRet
	
	/// <summary>
	/// Calculate the arc cotangent of a number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
FUNCTION ACot(n AS USUAL) AS FLOAT
	LOCAL f AS FLOAT
	LOCAL r8 AS REAL8
	f := n
	r8 := f
	RETURN 2 * Math.ATan(1) + Math.ATan(r8)
	
	
	
	/// <summary>
	/// Calculate the arc tangent of a number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
FUNCTION ATan(n AS USUAL) AS FLOAT
	LOCAL f AS FLOAT
	LOCAL r8 AS REAL8
	f := n
	r8 := f
	RETURN Math.ATan(r8)
	
	
	/// <summary>
	/// Round a number up to the next highest integer.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
FUNCTION Ceil(n AS USUAL) AS USUAL
	LOCAL result AS USUAL
	IF n:IsInteger
		result := n
	ELSEIF n:IsDecimal
		LOCAL d AS decimal
		d := (decimal) n
		d := Math.Ceiling(d)
		IF d <= int32.MaxValue .AND. d >= int32.MinValue
			result := COnvert.ToInt32(d)
		ELSEIF d <= int64.MaxValue .AND. d >= int64.MinValue
			result := COnvert.ToInt64(d)
		ELSE
			result := d
		ENDIF
		
	ELSEIF n:IsFloat
		LOCAL r8 AS REAL8
		r8 := (FLOAT) n
		r8 := Math.Ceiling(r8)
		IF r8 <= int32.MaxValue .AND. r8 >= int32.MinValue
			result := COnvert.ToInt32(r8)
		ELSEIF r8 <= int64.MaxValue .AND. r8 >= int64.MinValue
			result := COnvert.ToInt64(r8)
		ELSE
			result := r8
		ENDIF
	ELSE
		THROW  Error.ArgumentError( __ENTITY__, NAMEOF(n), "Argument not numeric",1)
	ENDIF
	RETURN result
	
	/// <summary>
	/// Calculate the cosine of a number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
FUNCTION Cos(n AS USUAL) AS FLOAT
	LOCAL f := n AS FLOAT
	LOCAL r8 := (REAL8) f AS REAL8
	RETURN Math.Cos(r8)
	
	/// <summary>
	/// Calculate the cotangent of a value.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
FUNCTION Cot(n AS USUAL) AS FLOAT
	LOCAL f := n AS FLOAT
	LOCAL r8 := (REAL8) f AS REAL8
	RETURN 2 * ATan( 1 ) + Math.ATan( r8 )
	
	
	
	/// <summary>
	/// Calculate the numeric value of a natural logarithm.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
FUNCTION EXP(n AS USUAL) AS FLOAT
	LOCAL f := n AS FLOAT
	LOCAL r8 := (REAL8) f AS REAL8
	RETURN Math.Exp(r8)
	
	
	/// <summary>
	/// Round a number down to the next lowest integer.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
FUNCTION Floor(n AS USUAL) AS USUAL
	LOCAL result AS USUAL
	IF n:IsInteger
		result := n
	ELSEIF n:IsDecimal
		LOCAL d AS decimal
		d := (decimal) n
		d := Math.Floor(d)
		IF d <= int32.MaxValue .AND. d >= int32.MinValue
			result := COnvert.ToInt32(d)
		ELSEIF d <= int64.MaxValue .AND. d >= int64.MinValue
			result := COnvert.ToInt64(d)
		ELSE
			result := d
		ENDIF
		
	ELSEIF n:IsFloat
		LOCAL r8 AS REAL8
		r8 := (FLOAT) n
		r8 := Math.Floor(r8)
		IF r8 <= int32.MaxValue .AND. r8 >= int32.MinValue
			result := COnvert.ToInt32(r8)
		ELSEIF r8 <= int64.MaxValue .AND. r8 >= int64.MinValue
			result := COnvert.ToInt64(r8)
		ELSE
			result := r8
		ENDIF
	ELSE
		THROW  Error.ArgumentError( __ENTITY__, NAMEOF(n), "Argument not numeric",1)
	ENDIF
	RETURN result
	
	
	/// <summary>
	/// Truncate or 
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
FUNCTION Integer(n AS USUAL) AS USUAL
	LOCAL result AS USUAL
	IF n:IsInteger
		result := n
	ELSEIF n:IsFloat .OR. n:IsDecimal
		IF n > 0.0
			result := Floor(n)
		ELSE
			result := Ceil(n)
		ENDIF
	ELSE
		THROW Error.ArgumentError( __ENTITY__, NAMEOF(n), "Argument not numeric",1)
	ENDIF
	RETURN result
	
	/// <summary>
	/// Calculate the natural logarithm of a numeric value.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
FUNCTION LOG(n AS USUAL) AS FLOAT
	LOCAL f := n AS FLOAT
	LOCAL r8 := (REAL8) f AS REAL8
	RETURN Math.Log(r8)
	
	/// <summary>
	/// Calculate the common logarithm of a numeric value.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
FUNCTION Log10(n AS USUAL) AS FLOAT
	LOCAL f := n AS FLOAT
	LOCAL r8 := (REAL8) f AS REAL8
	RETURN Math.Log10(r8) 
	
	
	
	/// <summary>
	/// Raise the first argument to the numeric power specified by the second argument.
	/// </summary>
	/// <param name="x"></param>
	/// <param name="y"></param>
	/// <returns>
	/// </returns>
FUNCTION POW(x AS USUAL,y AS USUAL) AS FLOAT
	LOCAL r8x := (REAL8) (FLOAT) x AS REAL8
	LOCAL r8y := (REAL8) (FLOAT) y AS REAL8
	RETURN Math.Pow(x, y)
	
	
INTERNAL GLOBAL _randomGenerator AS Random   
/// <summary>
/// Return a random number between 0 and 1.
/// </summary>
/// <param name="nSeed"></param>
/// <returns>
/// </returns>
FUNCTION Rand(nSeed AS USUAL) AS FLOAT
	IF !nSeed:IsNumeric .OR. nSeed <= 0
		nSeed := (INT) DateTime.Now:Ticks
	ENDIF
	_randomGenerator := Random{ nSeed }
	RETURN _randomGenerator:NextDouble()
	
FUNCTION Rand() AS FLOAT
	IF _randomGenerator == NULL
		_randomGenerator := Random{ 100001 }
	ENDIF
	RETURN _randomGenerator:NextDouble() 
	
	/// <summary>
	/// Round a number to a specified number of digits.
	/// </summary>
	/// <param name="n"></param>
	/// <param name="iDec"></param>
	/// <returns>
	/// </returns>
	
	#define MAX_DECIMALS 15
FUNCTION Round(n AS USUAL,iDec AS INT) AS USUAL
	LOCAL ret    AS USUAL
	LOCAL IsLong   AS LOGIC
	LOCAL IsInt64  AS LOGIC
	LOCAL r8     AS REAL8
	
	IF ! n:IsNumeric
		THROW Error.ArgumentError( __ENTITY__, NAMEOF(n), "Argument is not numeric")
	ENDIF
	
	// For Integers we could round the numbers 
	// Round(12345, -1) -> 12350
	IsInt64 := n:IsInt64
	IsLong  := n:IsLong
	r8  := n
	
	IF iDec > 0
			// Round after decimal point
			IF iDec > MAX_DECIMALS
				iDec := MAX_DECIMALS
			ENDIF
			
		r8 := Math.Round( r8, iDec, MidpointRounding.AwayFromZero ) 
	ELSE   
		// Round before decimal point
		iDec := -iDec   
		
		IF iDec > MAX_DECIMALS
			iDec := MAX_DECIMALS
		ENDIF
		
		r8 := r8 / ( 10 ^ iDec )
		r8 := Math.Round( r8, 0, MidpointRounding.AwayFromZero )
		r8 := r8 * ( 10 ^ iDec )
		
		IF r8 < 0
			isLong	:= r8 >= Int32.MinValue 
			isInt64 := r8 >= Int64.MinValue 
		ELSE
			isLong  := r8 <= Int32.MaxValue
			isInt64 := r8 <= Int64.MaxValue 
		ENDIF
		iDec := 0   
	ENDIF
	
	IF isLong .OR. IsInt64
		iDec := 0
	ENDIF
	
	IF iDec == 0 
		IF IsLong
			ret := (INT) r8
		ELSEIF IsInt64
			ret := (INT64) r8
		ELSE
			ret := FLOAT{r8, 0}
		ENDIF
	ELSE
		ret := FLOAT{r8, iDec}
	ENDIF
	
	RETURN ret   
	
	
	/// <summary>
	/// Calculate the sine of a number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
FUNCTION Sin(n AS USUAL) AS FLOAT
	LOCAL f := n AS FLOAT
	LOCAL r8 := (REAL8) f AS REAL8
	RETURN Math.Sin(r8)
	
	/// <summary>
	/// Return the square root of a positive number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
FUNCTION SQRT(n AS USUAL) AS FLOAT
	LOCAL  f := n AS FLOAT
	LOCAL r8 := (REAL8) f AS REAL8
	RETURN Math.SQrt(r8)
	
	/// <summary>
	/// Calculate the tangent of a number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
FUNCTION Tan(n AS USUAL) AS FLOAT
LOCAL  f := n AS FLOAT
LOCAL r8 := (REAL8) f AS REAL8
RETURN Math.Tan(r8)  
