//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/empty/*" />
FUNCTION Empty(uValue AS USUAL) AS LOGIC
	RETURN uValue:IsEmpty


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptyusual/*" />
FUNCTION EmptyUsual(kType AS DWORD) AS USUAL
	LOCAL result AS USUAL
	SWITCH kType
	CASE ARRAY
		result := USUAL{NULL_ARRAY}
	CASE BYTE; CASE WORD; CASE DWORD; CASE SHORTINT;  CASE LONG; CASE INT64
		result := USUAL{0}
	CASE FLOAT; CASE REAL4; CASE REAL8; CASE (DWORD) __UsualType.Decimal
		result := USUAL{0.0}
	CASE STRING
		result := USUAL{""}
	CASE DATE
		result := USUAL{(DATE) 0}
	CASE (DWORD) __UsualType.DateTime
		result := USUAL{DateTime.MinValue}
	CASE LOGIC
		result := USUAL{FALSE}
	CASE PTR
		result := USUAL{NULL_PTR}
	CASE PSZ
		result := USUAL{NULL_PSZ}
	CASE SYMBOL
		result := USUAL{NULL_SYMBOL}
	CASE USUAL
		result := NIL
	CASE CODEBLOCK
		result := USUAL{NULL_CODEBLOCK}
	OTHERWISE
		THROW Error.ArgumentError(__ENTITY__, NAMEOF(kType) , "Unknown type parameter")
	END SWITCH
	RETURN result

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isarray/*" />
FUNCTION IsArray(uValue AS USUAL) AS LOGIC
	RETURN uValue:IsArray

/// <summary>
/// Determine if a value is passed by reference
/// </summary>
/// <param name="uVal">The value to examine.</param>
/// <returns>
/// </returns>
FUNCTION IsByRef(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsByRef


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/iscodeblock/*" />
FUNCTION IsCodeBlock(uValue AS USUAL) AS LOGIC
	RETURN uValue:IsCodeBlock

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isdate/*" />
FUNCTION IsDate(uValue AS USUAL) AS LOGIC
	RETURN uValue:IsDate .OR. uValue:IsDateTime


/// <summary>
/// Determine if a value is a DateTime.
/// </summary>
/// <param name="uVal">The value to examine.</param>
/// <returns>TRUE if the value is a DATETIME data type; otherwise, FALSE. </returns>
FUNCTION IsDateTime(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsDateTime

/// <summary>
/// Determine if a value is a Decimal.
/// </summary>
/// <param name="uVal">The value to examine.</param>
/// <returns>TRUE if the value is a DECIMAL data type; otherwise, FALSE. </returns>
FUNCTION IsDecimal(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsDecimal

/// <summary>
/// Determine if a value is a Decimal or a Float
/// </summary>
/// <param name="uVal">The value to examine.</param>
/// <returns>TRUE if the value is a DECIMAL or FLOAT data type; otherwise, FALSE. </returns>
FUNCTION IsFractional(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsFloat .OR. uVal:IsDecimal

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isfloat/*" />
FUNCTION IsFloat(uValue AS USUAL) AS LOGIC
	RETURN uValue:IsFloat

/// <summary>
/// Determine if a value is a INT64.
/// </summary>
/// <param name="uVal">The value to examine.</param>
/// <returns>TRUE if the value is a INT64 data type; otherwise, FALSE. </returns>
FUNCTION IsInt64(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsInt64

/// <summary>
/// Determine if a value is an integer (LONG or INT64).
/// </summary>
/// <param name="uVal">The value to examine.</param>
/// <returns>TRUE if the value is a LONG or INT64 data type; otherwise, FALSE. </returns>
FUNCTION IsInteger(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsInteger

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/islogic/*" />
FUNCTION IsLogic(uValue AS USUAL) AS LOGIC
	RETURN uValue:IsLogic

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/islong/*" />
FUNCTION IsLong(uValue AS USUAL) AS LOGIC
	RETURN uValue:IsLong


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isnil/*" />
FUNCTION IsNil(uValue AS USUAL) AS LOGIC
	RETURN uValue:IsNil

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isnumeric/*" />
/// <returns>TRUE if the value is a LONG, FLOAT, IN64 or DECIMAL data type; otherwise, FALSE. </returns>
FUNCTION IsNumeric(uValue AS USUAL) AS LOGIC
	RETURN uValue:IsNumeric

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isobject/*" />
FUNCTION IsObject(uValue AS USUAL) AS LOGIC
	RETURN uValue:IsObject


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isptr/*" />
FUNCTION IsPtr(uValue AS USUAL) AS LOGIC
	RETURN uValue:IsPtr

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isstring/*" />
FUNCTION IsString(uValue AS USUAL) AS LOGIC
	RETURN uValue:IsString

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/issymbol/*" />
FUNCTION IsSymbol(uValue AS USUAL) AS LOGIC
	RETURN uValue:IsSymbol


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/len/*" />
FUNCTION Len(uValue AS USUAL) AS DWORD
	IF uValue:IsArray
		RETURN (DWORD) ((ARRAY) uValue):Length
	ELSEIF uValue:IsString
		RETURN (DWORD) ((STRING) uValue):Length
	ELSE
		THROW Error.DataTypeError(__ENTITY__, nameof(uValue), 1, uValue)
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
		THROW Error.DataTypeError(__ENTITY__, nameof(u), 1, u)
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
        VAR cMessage := "Expected type: " + ((__UsualType) dwType):ToString()+" actual type "+ ((__UsualType) UsualType(u)):ToString()
		THROW Error.DataTypeError(ProcName(1), nameof(u), 1, u, cMessage)
	ENDIF
	RETURN  
