//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp










/// <summary>
/// Determine if a value is between two other values.
/// </summary>
/// <param name="x">Value which should be compared.</param>
/// <param name="y">Lower value to compare against.</param>
/// <param name="z">Upper value to compare against.</param>
/// <returns>
/// True if x is >= y and <= z otherwise false.
/// </returns>
function Between(val as Usual,min as Usual,max as Usual) as logic
	return val >=min .and.  val<=max


/// <summary>
/// Return the number of arguments that a code block is expecting.
/// </summary>
/// <param name="uCodeBlock"></param>
/// <returns>
/// </returns>
function CParamCount(uCodeBlock as Usual) as dword
	/// THROW NotImplementedException{}
	return 0   




















/// <summary>
/// Indicate whether the first expression in a series is repeated later in the series.
/// </summary>
/// <param name="u"></param>
/// <returns>
/// </returns>
function InList(u as usual, args params usual[]) as logic
	return _InListWorker(u, args, FALSE)
/// <summary>
/// Indicate whether the first expression in a series is repeated in the exact same form later in the series.
/// </summary>
/// <param name="u"></param>
/// <returns>
/// </returns>
function InListExact(u as usual, args params usual[]) as logic
	return _InListWorker(u, args, TRUE)


internal function _InListWorker( u as usual, args as const usual[], lExact as logic)
	local i, nLen as int
	nLen := args:Length
	if lExact
		for i := 1 to nLen
			if args[i] == u
				return true
			endif
		next
	else
		for i := 1 to nLen
			if args[i]  = u
				return true
			endif
		next
	endif
	return false














/// <summary>
/// Return the larger of 2 values.
/// </summary>
/// <param name="u1"></param>
/// <param name="u2"></param>
/// <returns>
/// </returns>
function Max(u1 as Usual,u2 as Usual) as Usual

	if u1:IsNumeric .and. u2:IsNumeric

		if u1:IsFloat .or. u2:IsFloat
			return (USUAL) Math.Max( (Real8) u1, (Real8) u2)

		elseif u1:IsDecimal .or. u2:IsDecimal
			return (usual) Math.Max( (Decimal) u1, (Decimal) u2)

		elseif u1:IsInt64 .or. u2:IsInt64
			return (USUAL) Math.Max( (Int64) u1, (Int64) u2)
		endif
		return (USUAL) Math.Max( (Long) u1, (Long) u2)

	elseif u1:IsDate .and. u2:IsDate
		return iif ((date) u1 > (date) u2, u1, u2)

	elseif u1:IsString .and. u2:IsString
		return iif ((string) u1 > (string) u2, u1, u2)

	else
        throw Error.ArgumentError( nameof(u2) , "Incompatible types")
	endif
	return u1



/// <summary>
/// Return the smaller of 2 values.
/// </summary>
/// <param name="u1"></param>
/// <param name="u2"></param>
/// <returns>
/// </returns>
function Min(u1 as Usual,u2 as Usual) as Usual
	if u1:IsNumeric .and. u2:IsNumeric

		if u1:IsFloat .or. u2:IsFloat
			
			return (USUAL) Math.Min((Real8) u1, (Real8) u2)
		
		elseif u1:IsDecimal .or. u2:IsDecimal
			return (usual) Math.Min( (Decimal) u1, (Decimal) u2)
		
		elseif u1:IsInt64 .or. u2:IsInt64
			return (USUAL) Math.Min( (Int64) u1, (Int64) u2)
		endif
		return (USUAL) Math.Min( (Long) u1, (Long) u2)
	
	elseif u1:IsDate .and. u2:IsDate
		return iif ((date) u1 <(date) u2, u1, u2)
	
	elseif u1:IsString .and. u2:IsString
		return iif ((string) u1 <(string) u2, u1, u2)
	else
        throw Error.ArgumentError( nameof(u2) , "Incompatible types")
	endif
	return u1



/// <summary>
/// Get a particular color from a user-defined palette.
/// </summary>
/// <param name="bR"></param>
/// <param name="bG"></param>
/// <param name="bB"></param>
/// <returns>
/// </returns>
function PaletteRGB(bR as Usual,bG as Usual,bB as byte) as int
	/// THROW NotImplementedException{}
	return 0   

/// <summary>
/// Display a system modal dialog box to pause the current application.
/// </summary>
/// <returns>
/// </returns>
function Pause() as dword
	/// THROW NotImplementedException{}
	return 0   

/// <summary>
/// Duplicate a polymorphic variable.
/// </summary>
/// <param name="x"></param>
/// <returns>
/// </returns>
function PClone(x as Usual) as Usual
	/// THROW NotImplementedException{}
	return NIL   

/// <summary>
/// Return the position of the last argument in the list of arguments passed when a procedure or function is invoked.
/// </summary>
/// <returns>
/// </returns>
function PCount() as dword
	/// THROW NotImplementedException{}
	return 0   






/// <summary>
/// Get a particular Windows color.
/// </summary>
/// <param name="bR"></param>
/// <param name="bG"></param>
/// <param name="bB"></param>
/// <returns>
/// </returns>
function RGB(bR as Usual,bG as Usual,bB as byte) as int
	/// THROW NotImplementedException{}
	return 0   





/// <summary>
/// Install a system-wide object that receives all messages being sent to other data types.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
function SysObject(o as Usual) as object
	/// THROW NotImplementedException{}
	return null_object   


/// <summary>
/// Sound a speaker tone for a specified frequency and duration.
/// </summary>
/// <param name="dwFreq"></param>
/// <param name="dwDur"></param>
/// <returns>
/// </returns>
function Tone(dwFreq as dword,dwDur as dword) as Usual
	Console.Beep( (INT)dwFreq, (INT)dwDur * 1000 / 18 )
return	 NIL   
