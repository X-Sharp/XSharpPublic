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
	SWITCH (__UsualType) kType
	CASE __UsualType.Array
		result := USUAL{NULL_ARRAY}
	CASE __UsualType.Byte
    CASE __UsualType.Word
    CASE __UsualType.DWord
    CASE __UsualType.ShortInt
    CASE __UsualType.Long
    CASE __UsualType.Int64
    CASE __UsualType.UInt64
		result := USUAL{0}
    CASE __UsualType.Float
    CASE __UsualType.Real4
    CASE __UsualType.Real8
		result := USUAL{0.0}
    CASE __UsualType.Decimal
    CASE __UsualType.Currency
		result := USUAL{0m}
	CASE __UsualType.String
		result := USUAL{""}
	CASE __UsualType.Date
		result := USUAL{(DATE) 0}
	CASE __UsualType.DateTime
		result := USUAL{DateTime.MinValue}
	CASE __UsualType.Logic
		result := USUAL{FALSE}
	CASE __UsualType.Ptr
		result := USUAL{NULL_PTR}
	CASE __UsualType.Psz
		result := USUAL{NULL_PSZ}
	CASE __UsualType.Symbol
		result := USUAL{NULL_SYMBOL}
	CASE __UsualType.Usual
	CASE __UsualType.Void
		result := NIL
	CASE __UsualType.Codeblock
		result := USUAL{NULL_CODEBLOCK}
    CASE __UsualType.Object
	    result := USUAL{NULL_OBJECT}
    CASE __UsualType.Binary
       result := USUAL{BINARY{""}}
    CASE __UsualType.Fixed
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

/// <summary>
/// Determine if a value is a Binary.
/// </summary>
/// <param name="uVal">The value to examine.</param>
/// <returns>TRUE if the value is a Binary data type; otherwise, FALSE. </returns>
FUNCTION IsBinary(uValue AS USUAL) AS LOGIC
	RETURN uValue:IsBinary

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/iscodeblock/*" />
FUNCTION IsCodeBlock(uValue AS USUAL) AS LOGIC
	RETURN uValue:IsCodeblock

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
/// <returns>TRUE if the value is a Decimal data type; otherwise, FALSE. </returns>
FUNCTION IsDecimal(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsDecimal


/// <summary>
/// Determine if a value is a Currency.
/// </summary>
/// <param name="uVal">The value to examine.</param>
/// <returns>TRUE if the value is a Currency data type; otherwise, FALSE. </returns>
FUNCTION IsCurrency(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsCurrency

/// <summary>
/// Determine if a value is a Decimal or a Float
/// </summary>
/// <param name="uVal">The value to examine.</param>
/// <returns>TRUE if the value is a Decimal, Currency or Float data type; otherwise, FALSE. </returns>
FUNCTION IsFractional(uVal AS USUAL) AS LOGIC
	RETURN uVal:IsFractional

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


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/default/*" />
FUNCTION @@Default(uVar REF USUAL, uDefault AS USUAL) AS VOID
	IF uVar:IsNil
		uVar := uDefault
	ENDIF
	RETURN  



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/enforcenumeric/*" />
FUNCTION EnforceNumeric(u REF USUAL) AS VOID
	IF u:IsNil
		u := 0
	ELSEIF ! u:IsNumeric
		THROW Error.DataTypeError(__ENTITY__, nameof(u), 1, u)
	ENDIF
	RETURN  

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/enforcetype/*" />
FUNCTION EnforceType(u REF USUAL, dwType AS DWORD) AS VOID
	IF u:_usualType != dwType
		u := EmptyUsual(dwType)
	ENDIF
	RETURN


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/enforcetype/*" />
FUNCTION EnforceType(u AS USUAL, dwType AS DWORD) AS VOID
	IF u:_usualType != dwType
        VAR cMessage := "Expected type: " + ((__UsualType) dwType):ToString()+" actual type "+ ((__UsualType) UsualType(u)):ToString()
		THROW Error.DataTypeError(ProcName(1), nameof(u), 1, u, cMessage)
	ENDIF
	RETURN  

