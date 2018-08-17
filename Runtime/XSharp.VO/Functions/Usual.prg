//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//



/// <summary>
/// Determine if the result of an expression is empty.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function Empty(uVal as usual) as logic
	return uVal:IsEmpty


/// <summary>
/// Return the empty value of a specified data type.
/// </summary>
/// <param name="dwType"></param>
/// <returns>
/// </returns>
function EmptyUsual(dwType as dword) as __Usual
	local result as usual
	switch dwType
	case array
		result := __Usual{null_array}
	case byte; case word; case dword; case shortint;  case long; case int64
		result := __Usual{0}
	case float; case real4; case real8; case (dword) __UsualType.Decimal
		result := __Usual{0.0}
	case string
		result := __Usual{null_string}
	case date
		result := __Usual{(date) 0}
	case (dword) __UsualType.DateTime
		result := __Usual{DateTime.MinValue}
	case logic
		result := __Usual{false}
	case ptr
		result := __Usual{null_ptr}
	case psz
		result := __Usual{null_psz}
	case symbol
		result := __Usual{null_symbol}
	case usual
		result := nil
	case codeblock
		result := __Usual{null_codeblock}
	otherwise
		throw Error.ArgumentError(__entity__, NAMEOF(dwType) , "Unknown type parameter")
	end switch
	return result

/// <summary>
/// Determine if a value is an Array.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsArray(uVal as __Usual) as logic
	return uVal:IsArray

/// <summary>
/// Determine if a value is passed by reference
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsByRef(uVal as usual) as logic
	return uVal:IsByRef


/// <summary>
/// Determine if a value is a code block.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsCodeBlock(uVal as usual) as logic
	return uVal:IsCodeBlock

/// <summary>
/// Determine if a value is a Date.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsDate(uVal as usual) as logic
	return uVal:IsDate


/// <summary>
/// Determine if a value is a DateTime.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsDateTime(uVal as usual) as logic
	return uVal:IsDateTime

/// <summary>
/// Determine if a value is a DateTime.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsDecimal(uVal as usual) as logic
	return uVal:IsDecimal


/// <summary>
/// Determine if a value is a Float.
/// </summary>
/// <param name="uVal"></param>
/// <returns> 
/// </returns>
function IsFloat(uVal as usual) as logic
	return uVal:IsFloat

/// <summary>
/// Determine if a value is a logical.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsLogic(uVal as usual) as logic
	return uVal:IsLogic

/// <summary>
/// Determine if a value is a LONGINT.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsLong(uVal as usual) as logic
	return uVal:IsLong

/// <summary>
/// Determine if a value is __Usual._NIL.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsNil(uVal as usual) as logic
	return uVal:IsNil

/// <summary>
/// Determine if a value is a numeric.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsNumeric(uVal as usual) as logic
	return uVal:IsNumeric

/// <summary>
/// Determine if a value is an object.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsObject(uVal as usual) as logic
	return uVal:IsObject


/// <summary>
/// Determine if a value is a pointer.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsPtr(uVal as usual) as logic
	return uVal:IsPtr

/// <summary>
/// Determine if a value is a string.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsString(uVal as usual) as logic
	return uVal:IsString

/// <summary>
/// Determine if a value is a Symbol.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
function IsSymbol(uVal as usual) as logic
	return uVal:IsSymbol




/// <summary>
/// Return the length of a string or an Array.
/// </summary>
/// <param name="u"></param>
/// <returns>
/// </returns>
function Len(u as usual) as dword
	if u:IsArray
		return (dword) ((array) u):Length
	elseif u:IsString
		return (dword) ((string) u):Length
	else
		throw Error.DataTypeError(__entity__, u, 1, u)
	endif



  


/// <summary>
/// Assign a default value to a NIL argument.
/// </summary>
/// <param name="uVar"></param>
/// <param name="uDefaultValue"></param>
/// <returns>
/// </returns>
function DEFAULT(uVar ref usual, uDefaultValue as usual) as void
	if uVar:IsNil
		uVar := uDefaultValue
	endif
	return  


/// <summary>
/// Make sure a variable is a numeric.
/// </summary>
/// <param name="refu"></param>
/// <returns> 
/// </returns>
function EnforceNumeric(u ref usual) as void
	if u:IsNil
		u := 0
	elseif ! u:IsNumeric
		throw Error.DataTypeError(__entity__, u, 1, u)
	endif
	return  

/// <summary>
/// Make sure a variable is of a certain type.
/// </summary>
/// <param name="refu"></param>
/// <param name="nType"></param>
/// <returns>
/// </returns>
function EnforceType(u ref usual, dwType as dword) as void
	if u:IsNil
		u := EmptyUsual(dwType)
	elseif UsualType(u) != dwType
		throw Error.DataTypeError(__entity__, u, 1, u, dwType)
	endif
	return  
