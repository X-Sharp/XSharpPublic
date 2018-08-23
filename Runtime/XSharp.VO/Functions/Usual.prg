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
FUNCTION Empty(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsEmpty


/// <summary>
/// Return the empty value of a specified data type.
/// </summary>
/// <param name="dwType"></param>
/// <returns>
/// </returns>
FUNCTION EmptyUsual(dwType AS DWORD) AS __Usual
	LOCAL result AS USUAL
	SWITCH dwType
	CASE ARRAY
		result := __Usual{NULL_ARRAY}
	CASE BYTE; CASE WORD; CASE DWORD; CASE SHORTINT;  CASE LONG; CASE INT64
		result := __Usual{0}
	CASE FLOAT; CASE REAL4; CASE REAL8; CASE (DWORD) __UsualType.Decimal
		result := __Usual{0.0}
	CASE STRING
		result := __Usual{NULL_STRING}
	CASE DATE
		result := __Usual{(DATE) 0}
	CASE (DWORD) __UsualType.DateTime
		result := __Usual{DateTime.MinValue}
	CASE LOGIC
		result := __Usual{FALSE}
	CASE PTR
		result := __Usual{NULL_PTR}
	CASE PSZ
		result := __Usual{NULL_PSZ}
	CASE SYMBOL
		result := __Usual{NULL_SYMBOL}
	CASE USUAL
		result := NIL
	CASE CODEBLOCK
		result := __Usual{NULL_CODEBLOCK}
	OTHERWISE
		THROW Error.ArgumentError(__ENTITY__, NAMEOF(dwType) , "Unknown type parameter")
	END SWITCH
	RETURN result

/// <summary>
/// Determine if a value is an Array.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
FUNCTION IsArray(uVal AS __Usual) AS LOGIC
	RETURN uVal:IsArray

/// <summary>
/// Determine if a value is passed by reference
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
FUNCTION IsByRef(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsByRef


/// <summary>
/// Determine if a value is a code block.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
FUNCTION IsCodeBlock(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsCodeBlock

/// <summary>
/// Determine if a value is a Date.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
FUNCTION IsDate(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsDate


/// <summary>
/// Determine if a value is a DateTime.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
FUNCTION IsDateTime(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsDateTime

/// <summary>
/// Determine if a value is a DateTime.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
FUNCTION IsDecimal(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsDecimal


/// <summary>
/// Determine if a value is a Float.
/// </summary>
/// <param name="uVal"></param>
/// <returns> 
/// </returns>
FUNCTION IsFloat(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsFloat

/// <summary>
/// Determine if a value is a logical.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
FUNCTION IsLogic(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsLogic

/// <summary>
/// Determine if a value is a LONGINT.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
FUNCTION IsLong(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsLong

/// <summary>
/// Determine if a value is __Usual._NIL.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
FUNCTION IsNil(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsNil

/// <summary>
/// Determine if a value is a numeric.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
FUNCTION IsNumeric(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsNumeric

/// <summary>
/// Determine if a value is an object.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
FUNCTION IsObject(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsObject


/// <summary>
/// Determine if a value is a pointer.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
FUNCTION IsPtr(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsPtr

/// <summary>
/// Determine if a value is a string.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
FUNCTION IsString(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsString

/// <summary>
/// Determine if a value is a Symbol.
/// </summary>
/// <param name="uVal"></param>
/// <returns>
/// </returns>
FUNCTION IsSymbol(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsSymbol




/// <summary>
/// Return the length of a string or an Array.
/// </summary>
/// <param name="u"></param>
/// <returns>
/// </returns>
FUNCTION Len(u AS USUAL) AS DWORD
	IF u:IsArray
		RETURN (DWORD) ((ARRAY) u):Length
	ELSEIF u:IsString
		RETURN (DWORD) ((STRING) u):Length
	ELSE
		THROW Error.DataTypeError(__ENTITY__, u, 1, u)
	ENDIF



  


/// <summary>
/// Assign a default value to a NIL argument.
/// </summary>
/// <param name="uVar"></param>
/// <param name="uDefaultValue"></param>
/// <returns>
/// </returns>
FUNCTION DEFAULT(uVar REF USUAL, uDefaultValue AS USUAL) AS VOID
	IF uVar:IsNil
		uVar := uDefaultValue
	ENDIF
	RETURN  


/// <summary>
/// Make sure a variable is a numeric.
/// </summary>
/// <param name="refu"></param>
/// <returns> 
/// </returns>
FUNCTION EnforceNumeric(u REF USUAL) AS VOID
	IF u:IsNil
		u := 0
	ELSEIF ! u:IsNumeric
		THROW Error.DataTypeError(__ENTITY__, u, 1, u)
	ENDIF
	RETURN  

/// <summary>
/// Make sure a variable is of a certain type.
/// </summary>
/// <param name="refu"></param>
/// <param name="nType"></param>
/// <returns>
/// </returns>
FUNCTION EnforceType(u REF USUAL, dwType AS DWORD) AS VOID
	IF u:IsNil
		u := EmptyUsual(dwType)
	ELSEIF UsualType(u) != dwType
		THROW Error.DataTypeError(__ENTITY__, u, 1, u, dwType)
	ENDIF
	RETURN  
