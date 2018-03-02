//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp




/// <summary>
/// </summary>
/// <param name="x"></param>
/// <param name="nType"></param>
/// <returns>
/// </returns>
function Any2Usual(x as __Usual,nType as dword) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function ArgCount() as dword
	/// THROW NotImplementedException{}
	return 0   







/// <summary>
/// Determine if a value is between two other values.
/// </summary>
/// <param name="x">Value which should be compared.</param>
/// <param name="y">Lower value to compare against.</param>
/// <param name="z">Upper value to compare against.</param>
/// <returns>
/// True if x is >= y and <= z otherwise false.
/// </returns>
function Between(x as __Usual,y as __Usual,z as __Usual) as logic
	return ((x>=y) && (x<=z))



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function CompString() as int
	/// THROW NotImplementedException{}
	return 0   



/// <summary>
/// Return the number of arguments that a code block is expecting.
/// </summary>
/// <param name="uCodeBlock"></param>
/// <returns>
/// </returns>
function CParamCount(uCodeBlock as __Usual) as dword
	/// THROW NotImplementedException{}
	return 0   







/// <summary>
/// Assign a default value to a __Usual._NIL argument.
/// </summary>
/// <param name="xRef"></param>
/// <param name="x"></param>
/// <returns>
/// </returns>
function Default(xRef as __Usual,x as __Usual) as void
	/// THROW NotImplementedException{}
	return  







/// <summary>
/// Determine if the result of an expression is empty.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function Empty(uVal as __Usual) as logic
	/// THROW NotImplementedException{}
	return false   








/// <summary>
/// Indicate whether the first expression in a series is repeated later in the series.
/// </summary>
/// <param name="u"></param>
/// <returns>
/// </returns>
function InList(u as __Usual) as logic
	/// THROW NotImplementedException{}
	return false   

/// <summary>
/// Indicate whether the first expression in a series is repeated in the exact same form later in the series.
/// </summary>
/// <param name="u"></param>
/// <returns>
/// </returns>
function InListExact(u as __Usual) as logic
	/// THROW NotImplementedException{}
	return false   
















/// <summary>
/// Return the larger of 2 values.
/// </summary>
/// <param name="u1"></param>
/// <param name="u2"></param>
/// <returns>
/// </returns>
function Max(u1 as __Usual,u2 as __Usual) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   





/// <summary>
/// Return the smaller of 2 values.
/// </summary>
/// <param name="u1"></param>
/// <param name="u2"></param>
/// <returns>
/// </returns>
function Min(u1 as __Usual,u2 as __Usual) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   



/// <summary>
/// Get a particular color from a user-defined palette.
/// </summary>
/// <param name="bR"></param>
/// <param name="bG"></param>
/// <param name="bB"></param>
/// <returns>
/// </returns>
function PaletteRGB(bR as __Usual,bG as __Usual,bB as byte) as int
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
function PClone(x as __Usual) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   

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
function RGB(bR as __Usual,bG as __Usual,bB as byte) as int
	/// THROW NotImplementedException{}
	return 0   





/// <summary>
/// Install a system-wide object that receives all messages being sent to other data types.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
function SysObject(o as __Usual) as object
	/// THROW NotImplementedException{}
	return null_object   







/// <summary>
/// Access contents of an address, whether it is passed by reference or not.
/// </summary>
/// <param name="u"></param>
/// <returns>
/// </returns>
function UsualVal(u as __Usual) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   

/// <summary>
/// Determine the data type of an expression.
/// </summary>
/// <param name="x"></param>
/// <returns>
/// </returns>
function ValType(x as __Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   





/// <summary>
/// Sound a speaker tone for a specified frequency and duration.
/// </summary>
/// <param name="dwFreq"></param>
/// <param name="dwDur"></param>
/// <returns>
/// </returns>
function Tone(dwFreq as dword,dwDur as dword) as __Usual
	System.Media.SystemSounds.Beep:Play()
return	 __Usual._NIL   
