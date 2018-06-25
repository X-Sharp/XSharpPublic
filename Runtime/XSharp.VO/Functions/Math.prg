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
function Float2Long(f as usual) as long
	local flValue as float
	flValue := (float) f
	return (long) flValue
	
	/// <summary>
	/// Return the absolute value of a numeric expression, regardless of its sign.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function Abs(n as usual) as usual
	local uRet as usual
	if n:IsInt64
		uRet := Math.Abs( (int64) n )
	elseif n:IsDecimal
		uRet := Math.Abs( (Decimal) n)
	elseif n:isInteger
		uRet := Math.Abs( (int) n )
	elseif n:IsFLoat
		uRet := AbsFloat( n )
	else
		throw  Error.ArgumentError( __ENTITY__, nameof(n), "Argument not numeric",1)
	endif
	return uRet
	
	/// <summary>
	/// Calculate the arc cotangent of a number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function ACot(n as usual) as float
	local f as float
	local r8 as real8
	f := n
	r8 := f
	return 2 * Math.ATan(1) + Math.ATan(r8)
	
	
	
	/// <summary>
	/// Calculate the arc tangent of a number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function ATan(n as usual) as float
	local f as float
	local r8 as real8
	f := n
	r8 := f
	return Math.ATan(r8)
	
	
	/// <summary>
	/// Round a number up to the next highest integer.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function Ceil(n as usual) as usual
	local result as usual
	if n:IsInteger
		result := n
	elseif n:IsDecimal
		local d as decimal
		d := (decimal) n
		d := Math.Ceiling(d)
		if d <= int32.MaxValue .and. d >= int32.MinValue
			result := COnvert.ToInt32(d)
		elseif d <= int64.MaxValue .and. d >= int64.MinValue
			result := COnvert.ToInt64(d)
		else
			result := d
		endif
		
	elseif n:IsFloat
		local r8 as real8
		r8 := (float) n
		r8 := Math.Ceiling(r8)
		if r8 <= int32.MaxValue .and. r8 >= int32.MinValue
			result := COnvert.ToInt32(r8)
		elseif r8 <= int64.MaxValue .and. r8 >= int64.MinValue
			result := COnvert.ToInt64(r8)
		else
			result := r8
		endif
	else
		throw  Error.ArgumentError( __ENTITY__, nameof(n), "Argument not numeric",1)
	endif
	return result
	
	/// <summary>
	/// Calculate the cosine of a number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function Cos(n as usual) as float
	local f := n as float
	local r8 := (real8) f as real8
	return Math.Cos(r8)
	
	/// <summary>
	/// Calculate the cotangent of a value.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function Cot(n as usual) as float
	local f := n as float
	local r8 := (real8) f as real8
	return 2 * ATan( 1 ) + Math.ATan( r8 )
	
	
	
	/// <summary>
	/// Calculate the numeric value of a natural logarithm.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function EXP(n as usual) as float
	local f := n as float
	local r8 := (real8) f as real8
	return Math.Exp(r8)
	
	
	/// <summary>
	/// Round a number down to the next lowest integer.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function Floor(n as usual) as usual
	local result as usual
	if n:IsInteger
		result := n
	elseif n:IsDecimal
		local d as decimal
		d := (decimal) n
		d := Math.Floor(d)
		if d <= int32.MaxValue .and. d >= int32.MinValue
			result := COnvert.ToInt32(d)
		elseif d <= int64.MaxValue .and. d >= int64.MinValue
			result := COnvert.ToInt64(d)
		else
			result := d
		endif
		
	elseif n:IsFloat
		local r8 as real8
		r8 := (float) n
		r8 := Math.Floor(r8)
		if r8 <= int32.MaxValue .and. r8 >= int32.MinValue
			result := COnvert.ToInt32(r8)
		elseif r8 <= int64.MaxValue .and. r8 >= int64.MinValue
			result := COnvert.ToInt64(r8)
		else
			result := r8
		endif
	else
		throw  Error.ArgumentError( __ENTITY__, nameof(n), "Argument not numeric",1)
	endif
	return result
	
	
	/// <summary>
	/// Truncate or 
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function Integer(n as usual) as usual
	local result as usual
	if n:IsInteger
		result := n
	elseif n:IsFloat .or. n:IsDecimal
		if n > 0.0
			result := Floor(n)
		else
			result := Ceil(n)
		endif
	else
		throw Error.ArgumentError( __ENTITY__, nameof(n), "Argument not numeric",1)
	endif
	return result
	
	/// <summary>
	/// Calculate the natural logarithm of a numeric value.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function LOG(n as usual) as float
	local f := n as float
	local r8 := (real8) f as real8
	return Math.Log(r8)
	
	/// <summary>
	/// Calculate the common logarithm of a numeric value.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function Log10(n as usual) as float
	local f := n as float
	local r8 := (real8) f as real8
	return Math.Log10(r8) 
	
	
	
	/// <summary>
	/// Raise the first argument to the numeric power specified by the second argument.
	/// </summary>
	/// <param name="x"></param>
	/// <param name="y"></param>
	/// <returns>
	/// </returns>
function POW(x as usual,y as usual) as float
	local r8x := (real8) (float) x as real8
	local r8y := (real8) (float) y as real8
	return Math.Pow(x, y)
	
	
internal global _randomGenerator as Random   
/// <summary>
/// Return a random number between 0 and 1.
/// </summary>
/// <param name="nSeed"></param>
/// <returns>
/// </returns>
function Rand(nSeed as usual) as float
	if !nSeed:IsNumeric .or. nSeed <= 0
		nSeed := (int) DateTime.Now:Ticks
	endif
	_randomGenerator := Random{ nSeed }
	return _randomGenerator:NextDouble()
	
function Rand() as float
	if _randomGenerator == null
		_randomGenerator := Random{ 100001 }
	endif
	return _randomGenerator:NextDouble() 
	
	/// <summary>
	/// Round a number to a specified number of digits.
	/// </summary>
	/// <param name="n"></param>
	/// <param name="iDec"></param>
	/// <returns>
	/// </returns>
	
	#define MAX_DECIMALS 15
function Round(n as usual,iDec as int) as usual
	local ret    as usual
	local IsLong   as logic
	local IsInt64  as logic
	local r8     as real8
	
	if ! n:IsNumeric
		throw Error.ArgumentError( __ENTITY__, nameof(n), "Argument is not numeric")
	endif
	
	// For Integers we could round the numbers 
	// Round(12345, -1) -> 12350
	IsInt64 := n:IsInt64
	IsLong  := n:IsLong
	r8  := n
	
	if iDec > 0
			// Round after decimal point
			if iDec > MAX_DECIMALS
				iDec := MAX_DECIMALS
			endif
			
		r8 := Math.Round( r8, iDec, MidpointRounding.AwayFromZero ) 
	else   
		// Round before decimal point
		iDec := -iDec   
		
		if iDec > MAX_DECIMALS
			iDec := MAX_DECIMALS
		endif
		
		r8 := r8 / ( 10 ^ iDec )
		r8 := Math.Round( r8, 0, MidpointRounding.AwayFromZero )
		r8 := r8 * ( 10 ^ iDec )
		
		if r8 < 0
			isLong	:= r8 >= Int32.MinValue 
			isInt64 := r8 >= Int64.MinValue 
		else
			isLong  := r8 <= Int32.MaxValue
			isInt64 := r8 <= Int64.MaxValue 
		endif
		iDec := 0   
	endif
	
	if isLong .or. IsInt64
		iDec := 0
	endif
	
	if iDec == 0 
		if IsLong
			ret := (int) r8
		elseif IsInt64
			ret := (int64) r8
		else
			ret := float{r8, 0}
		endif
	else
		ret := float{r8, iDec}
	endif
	
	return ret   
	
	
	/// <summary>
	/// Calculate the sine of a number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function Sin(n as usual) as float
	local f := n as float
	local r8 := (real8) f as real8
	return Math.Sin(r8)
	
	/// <summary>
	/// Return the square root of a positive number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function SQRT(n as usual) as float
	local  f := n as float
	local r8 := (real8) f as real8
	return Math.SQrt(r8)
	
	/// <summary>
	/// Calculate the tangent of a number.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function Tan(n as usual) as float
local  f := n as float
local r8 := (real8) f as real8
return Math.Tan(r8)  
