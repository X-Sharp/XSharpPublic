//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Runtime.CompilerServices
USING System.Collections.Generic

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/empty/*" />
FUNCTION Empty(uValue AS USUAL) AS LOGIC
    RETURN uValue:IsEmpty

#region Empty Overloads
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For String values the 'emptyness' is determined by calling String.IsNullOrWhiteSpace()</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty(uValue AS STRING) AS LOGIC
    RETURN String.IsNullOrWhiteSpace(uValue)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For DATE values the 'emptyness' is determined by comparing to NULL_DATE</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty(uValue AS Date) AS LOGIC
    RETURN uValue:IsEmpty

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For ARRAY values the 'emptyness' is determined by comparing to NULL_ARRAY and by checking if the length is 0</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty(uValue AS ARRAY) AS LOGIC
    RETURN (uValue?:Length ?? 0) == 0

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For ARRAY OF values the 'emptyness' is determined by comparing to NULL_ARRAY and by checking if the length is 0</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty<T>(uValue AS XSharp.__ArrayBase<T>) AS LOGIC
    RETURN (uValue?:Length ?? 0) == 0

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For T[] values the 'emptyness' is determined by comparing to NULL and by checking if the length is 0</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty<T>(uValue AS T[]) AS LOGIC
    RETURN (uValue?:Length ?? 0) == 0

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For ICollection&lt;T&gt; values the 'emptyness' is determined by comparing to NULL and by checking if the Count is 0</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty<T>(uValue AS ICollection<T>) AS LOGIC
    RETURN (uValue?:Count ?? 0) == 0

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For Object values the 'emptyness' is determined by Comparing to NULL_OBJECT and comparign to DBNull.Value</remarks>
FUNCTION Empty(oObject AS OBJECT) AS LOGIC
    IF oObject == NULL_OBJECT .OR. Convert.IsDBNull(oObject)
        RETURN TRUE
    ENDIF

    VAR oType := oObject:GetType()
    IF oType:IsValueType
        SWITCH System.Type.GetTypeCode(oType)
        CASE TypeCode.Byte
            RETURN ((BYTE)oObject == 0)
        CASE TypeCode.SByte
            RETURN ((System.SByte)oObject == 0)
        CASE TypeCode.UInt16
            RETURN ((WORD)oObject == 0)
        CASE TypeCode.UInt32
            RETURN ((DWORD)oObject == 0)
        CASE TypeCode.UInt64
            RETURN ((UINT64)oObject == 0)
        CASE TypeCode.Int16
            RETURN ((SHORT)oObject == 0)
        CASE TypeCode.Int32
            RETURN ((INT)oObject == 0)
        CASE TypeCode.Int64
            RETURN ((INT64)oObject == 0)
        CASE TypeCode.Decimal
            RETURN ((System.Decimal)oObject == 0)
        CASE TypeCode.Double
            RETURN ((REAL8)oObject == 0)
        CASE TypeCode.Single
            RETURN ((REAL4)oObject == 0)
        CASE TypeCode.String
            RETURN Empty((STRING)oObject)
        CASE TypeCode.DateTime
            RETURN Empty((DateTime)oObject)
        CASE TypeCode.Boolean
            RETURN Empty((LOGIC)oObject)
        CASE TypeCode.Object WHEN oObject IS IDate VAR oDat
            RETURN oDat:IsEmpty
        CASE TypeCode.Object WHEN oObject IS IFloat VAR oFl
            RETURN Empty(oFl:Value)
        CASE TypeCode.Object WHEN oObject IS SYMBOL VAR oSym
            RETURN Empty(oSym)
        CASE TypeCode.Object WHEN oObject IS PSZ VAR oPsz
            RETURN Empty(oPsz)
        CASE TypeCode.Object WHEN oObject IS System.IntPtr VAR oPtr
            RETURN Empty(oPtr)
        CASE TypeCode.Object WHEN oObject IS Currency VAR oCur
            RETURN Empty(oCur)
        CASE TypeCode.Object WHEN oObject IS __WinBool VAR oWb
            RETURN Empty((LOGIC) oWb)
        CASE TypeCode.Object WHEN oObject IS __WinDate VAR oWd
            RETURN Empty((DATE) oWd)
        END SWITCH
    ELSE
        LOCAL u as USUAL
        u := oObject
        RETURN Empty(u)
    ENDIF

RETURN FALSE

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For T values the 'emptyness' is determined by calling the Equals() operator and comparing to a DEFAULT(T)</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty<T>(uValue AS T) AS LOGIC WHERE T IS STRUCT
    RETURN uValue:Equals(DEFAULT(T))

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For INT values the 'emptyness' is determined by comparing to 0</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty(uValue AS INT) AS LOGIC
    RETURN uValue == 0

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For DWORD values the 'emptyness' is determined by comparing to 0</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty(uValue AS DWORD) AS LOGIC
    RETURN uValue == 0

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For IFloat values the 'emptyness' is determined by comparing the value 0</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty(uValue AS Float) AS LOGIC
    RETURN uValue:@@Value = 0.0


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For Currency values the 'emptyness' is determined by comparing the value 0</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty(uValue AS Currency) AS LOGIC
    RETURN uValue:@@Value = 0.0m


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For LOGIC values the 'emptyness' is determined by comparing to FALSE</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty(uValue AS LOGIC) AS LOGIC
    RETURN uValue == FALSE

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For SYMBOL values the 'emptyness' is determined by comparing to NULL_SYMBOL</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty(uValue AS SYMBOL) AS LOGIC
    RETURN uValue == NULL_SYMBOL

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For PSZ values the 'emptyness' is determined by comparing to NULL_PSZ and by checking the length with 0</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty(uValue AS PSZ) AS LOGIC
    RETURN uValue == NULL_PSZ .OR. uValue:Length == 0

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For PTR values the 'emptyness' is determined by comparing to NULL_PTR</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty(uValue AS PTR) AS LOGIC
    RETURN (uValue == NULL_PTR)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptytyped/*" />
/// <remarks>For DateTime values the 'emptyness' is determined by comparing to DEFAULT(DateTime)</remarks>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION Empty(uValue AS DateTime) AS LOGIC
    RETURN uValue == DEFAULT(DateTime)
#endregion



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
        THROW Error.ArgumentError(__FUNCTION__, NAMEOF(kType) , "Unknown type parameter")
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
        THROW Error.DataTypeError(__FUNCTION__, nameof(uValue), 1, uValue)
    ENDIF


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/default/*" />
FUNCTION @@Default(uVar REF USUAL, uDefault AS USUAL) AS VOID
    IF uVar:Type == __UsualType.Void
        uVar := uDefault
    ENDIF
    RETURN



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/enforcenumeric/*" />
FUNCTION EnforceNumeric(u REF USUAL) AS VOID
    IF u:IsNil
        u := 0
    ELSEIF ! u:IsNumeric
        THROW Error.DataTypeError(__FUNCTION__, nameof(u), 1, u)
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

