//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp
/// <summary>
/// Return the empty value of a specified data type.
/// </summary>
/// <param name="dwType"></param>
/// <returns>
/// </returns>
function EmptyUsual(dwType as dword) as __Usual
	/// THROW NotImplementedException{}
	return	 __Usual._NIL   


/// <summary>
/// Convert a string containing a numeric value to a numeric data type.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function Val(c as string) as __Usual
	/// THROW NotImplementedException{}
	return	 __Usual._NIL   





/// <summary>
/// Determine if a value is an __Array.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsArray(uVal as __Usual) as logic
	/// THROW NotImplementedException{}
	return false   




/// <summary>
/// Determine if a value is a code block.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsCodeBlock(uVal as __Usual) as logic
	/// THROW NotImplementedException{}
	return false   

/// <summary>
/// Determine if a value is a __VODate.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsDate(uVal as __Usual) as logic
	/// THROW NotImplementedException{}
	return false   

/// <summary>
/// Determine if a value is a __VOFloat.
/// </summary>
/// <param name="uVal"></param>
/// <returns> 
/// </returns>
function IsFloat(uVal as __Usual) as logic
	/// THROW NotImplementedException{}
	return false   

/// <summary>
/// Determine if a value is a logical.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsLogic(uVal as __Usual) as logic
	/// THROW NotImplementedException{}
	return false  

/// <summary>
/// Determine if a value is a LONGINT.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsLong(uVal as __Usual) as logic
	/// THROW NotImplementedException{}
	return false   

/// <summary>
/// Determine if a value is __Usual._NIL.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsNil(uVal as __Usual) as logic
	/// THROW NotImplementedException{}
	return false   

/// <summary>
/// Determine if a value is a numeric.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsNumeric(uVal as __Usual) as logic
	/// THROW NotImplementedException{}
	return false   

/// <summary>
/// Determine if a value is an object.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsObject(uVal as __Usual) as logic
	/// THROW NotImplementedException{}
	return false   


/// <summary>
/// Determine if a value is a pointer.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsPtr(uVal as __Usual) as logic
	/// THROW NotImplementedException{}
	return false   

/// <summary>
/// Determine if a value is a string.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsString(uVal as __Usual) as logic
	/// THROW NotImplementedException{}
	return false

/// <summary>
/// Determine if a value is a __Symbol.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsSymbol(uVal as __Usual) as logic
	/// THROW NotImplementedException{}
	return false   




/// <summary>
/// Return the length of a string or an __Array.
/// </summary>
/// <param name="u"></param>
/// <returns>
/// </returns>
function Len(u as __Usual) as dword
	/// THROW NotImplementedException{}
	return 0   




/// <summary>
/// </summary>
/// <param name="dwType"></param>
/// <returns>
/// </returns>
FUNCTION TypeString(dwType AS DWORD) AS STRING
	/// THROW NotImplementedException{}
	RETURN String.Empty   
