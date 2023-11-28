//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections
USING System.Collections.Generic
USING System.Collections.Concurrent
USING System.Diagnostics
USING System.Runtime.CompilerServices
USING System.Runtime.Serialization


#include "attributes.xh"

BEGIN NAMESPACE XSharp
/// <summary>Internal type that implements the XBase Compatible SYMBOL type.<br/>
/// This type has many operators and implicit converters that normally are never directly called from user code.<br/>
/// There are also some operators that handle implicit conversions between Symbols and Strings in your code.
/// </summary>
/// <include file="RTComments.xml" path="Comments/Symbol/*" />
//[DebuggerTypeProxy(TYPEOF(SymbolDebugView))];
[DebuggerDisplay("{ToDebugString(),nq}",Type := "SYMBOL")];
[Serializable];
PUBLIC STRUCTURE __Symbol ;
        IMPLEMENTS IEqualityComparer<__Symbol>, ;
        IEquatable<__Symbol>,;
        IComparable<__Symbol>, ;
        IComparable, ;
        ICloneable , ;
        IConvertible,;
        ISerializable

#region fields
    [NOSHOW] PRIVATE INITONLY _index		AS DWORD
    // This dictionary is only used when symbols are retrievd with SysGetAtomName
    // We are caching these because we do not know when they can be freed.
    [NOSHOW] PRIVATE STATIC _PszDict		AS ConcurrentDictionary<DWORD, PSZ>
#endregion

#region constrúctors
    /// <exclude />
    STATIC CONSTRUCTOR
        SymbolTable.Initialize()

    /// <include file="RTComments.xml" path="Comments/Constructor/*" />
    [NODEBUG] [INLINE];
    CONSTRUCTOR(sValue AS STRING)
        SELF(sValue, TRUE)

    /// <include file="RTComments.xml" path="Comments/Constructor/*" />
    [NODEBUG] [INLINE];
    CONSTRUCTOR(sValue AS STRING,  upperCase AS LOGIC)
        IF sValue != NULL
            IF (upperCase)
                sValue := sValue:ToUpperInvariant()
            ENDIF
            _index := SymbolTable.Add(sValue)
        ELSE
            _index := 0
        ENDIF
        RETURN

    [NODEBUG] [INLINE];
    PRIVATE CONSTRUCTOR (dwValue AS DWORD)
        SELF:_index := dwValue

#endregion
    INTERNAL STATIC METHOD Find(sValue AS STRING ) AS __Symbol
        IF SymbolTable.TryGetValue(sValue, out var index)
            RETURN __Symbol{index}
        ENDIF
        RETURN __Symbol{0}

    [NOSHOW] INTERNAL PROPERTY _value AS STRING
    GET
        RETURN SymbolTable.GetString(SELF:_index)
    END GET
    END PROPERTY
    [NOSHOW] INTERNAL STATIC PROPERTY PszDict AS ConcurrentDictionary<DWORD, PSZ>
    GET
        IF _PszDict == NULL
            _PszDict := ConcurrentDictionary<DWORD, PSZ>{}
        ENDIF
        RETURN _PszDict
    END GET
    END PROPERTY

#region methods
    /// <inheritdoc />
    OVERRIDE METHOD Equals(obj AS OBJECT) AS LOGIC
        LOCAL rhs AS SYMBOL
        IF (obj == NULL)
            RETURN FALSE
        ENDIF
        rhs := (SYMBOL) obj
        RETURN SELF:Equals(rhs)

    /// <inheritdoc />
    OVERRIDE METHOD GetHashCode() AS LONG
        RETURN (INT) SELF:_index

    /// <inheritdoc />
    METHOD GetHashCode(s AS __Symbol) AS LONG
        RETURN (INT) s:_index

    /// <inheritdoc />
    OVERRIDE METHOD ToString() AS STRING
        RETURN _value

    /// <exclude />
    METHOD SysGetAtomName() AS PSZ
        IF PszDict:TryGetValue(_index, OUT VAR ret)
            RETURN ret
        ENDIF
        LOCAL pszAtom AS PSZ
        pszAtom := __Psz.CreatePsz(_value)
        PszDict:TryAdd(_index, pszAtom)
        RETURN pszAtom

#endregion

#region Equality
    /// <inheritdoc />
    METHOD Equals(symOther AS SYMBOL) AS LOGIC
        RETURN SELF:_index == symOther:_index

    /// <inheritdoc />
    METHOD Equals(x AS SYMBOL, y AS SYMBOL) AS LOGIC
        RETURN x:_index == y:_index

    /// <exclude />
    METHOD Equals(s AS STRING) AS LOGIC
        RETURN SELF:_value == s

#endregion
#region Operators

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR ==(lhs AS SYMBOL, rhs AS SYMBOL) AS LOGIC
        RETURN lhs:_index == rhs:_index

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR !=(a AS SYMBOL, b AS SYMBOL) AS LOGIC
        RETURN a:_index != b:_index

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR ==(lhs AS SYMBOL, rhs AS STRING) AS LOGIC
        RETURN lhs:_value == rhs

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR !=(lhs AS SYMBOL, rhs AS STRING) AS LOGIC
        RETURN lhs:_value != rhs

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR ==(lhs AS STRING, rhs AS SYMBOL) AS LOGIC
        RETURN lhs == rhs:_value

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR !=(lhs AS STRING, rhs AS SYMBOL) AS LOGIC
        RETURN lhs != rhs:_value

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR ==(lhs AS SYMBOL, rhs AS DWORD) AS LOGIC
        RETURN lhs:_index == rhs

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR !=(lhs AS SYMBOL, rhs AS DWORD) AS LOGIC
        RETURN lhs:_index != rhs

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR ==(lhs AS DWORD, rhs AS SYMBOL) AS LOGIC
        RETURN lhs == rhs:_index

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR !=(lhs AS DWORD, rhs AS SYMBOL) AS LOGIC
        RETURN lhs != rhs:_index

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR EXPLICIT(dwValue AS DWORD) AS SYMBOL
        IF dwValue <= SymbolTable.Count
            RETURN SYMBOL{dwValue}
        ENDIF
        RETURN SYMBOL{0}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR EXPLICIT(symValue AS SYMBOL) AS DWORD
        RETURN symValue:_index

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR IMPLICIT(sValue AS STRING) AS SYMBOL
        RETURN SYMBOL{sValue, TRUE}

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR IMPLICIT(symValue AS SYMBOL) AS STRING
        RETURN symValue:_value

        // relative comparisons
        // compare symbols or symbols and strings
    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR >(lhs AS SYMBOL, rhs AS SYMBOL) AS LOGIC
        RETURN __StringCompare(lhs:_value, rhs:_value) > 0

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR >(lhs AS SYMBOL, rhs AS STRING) AS LOGIC
        RETURN __StringCompare(lhs:_value, rhs) > 0

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR >(lhs AS STRING, rhs AS SYMBOL) AS LOGIC
        RETURN __StringCompare(lhs, rhs:_value) > 0

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR <(lhs AS SYMBOL, rhs AS SYMBOL) AS LOGIC
        RETURN __StringCompare(lhs:_value, rhs:_value) < 0

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR <(lhs AS SYMBOL, rhs AS STRING) AS LOGIC
        RETURN __StringCompare(lhs:_value, rhs) < 0

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR <(lhs AS STRING, rhs AS SYMBOL) AS LOGIC
        RETURN __StringCompare(lhs, rhs:_value) < 0

        // or Equals
    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR >=(lhs AS SYMBOL, rhs AS SYMBOL) AS LOGIC
        RETURN __StringCompare(lhs:_value, rhs:_value) >= 0

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR >=(lhs AS SYMBOL, rhs AS STRING) AS LOGIC
        RETURN __StringCompare(lhs:_value, rhs) >= 0

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR >=(lhs AS STRING, rhs AS SYMBOL) AS LOGIC
        RETURN __StringCompare(lhs, rhs:_value) >= 0

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR <=(lhs AS SYMBOL, rhs AS SYMBOL) AS LOGIC
        RETURN __StringCompare(lhs:_value, rhs:_value) <= 0

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR <=(lhs AS SYMBOL, rhs AS STRING) AS LOGIC
        RETURN __StringCompare(lhs:_value, rhs) <= 0

    /// <include file="RTComments.xml" path="Comments/Operator/*" />
    [NODEBUG]  [INLINE];
    STATIC OPERATOR <=(lhs AS STRING, rhs AS SYMBOL) AS LOGIC
        RETURN __StringCompare(lhs, rhs:_value) <= 0

#endregion


#region INTERNAL types
    INTERNAL STATIC  CLASS SymbolTable
#region fields
        STATIC PRIVATE LookupTable AS ConcurrentDictionary<STRING,DWORD>
        STATIC PRIVATE Strings		AS List<STRING>
        STATIC PRIVATE sync AS OBJECT
#endregion

#region constructors
        STATIC METHOD Initialize() AS VOID
            sync		:= OBJECT{}
            LookupTable := ConcurrentDictionary<STRING,DWORD>{}
            Strings := List<STRING>{}
            LookupTable:TryAdd("", 0)
            Strings:Add("")
#endregion

#region methods
        INTERNAL STATIC METHOD Add(strValue AS STRING) AS DWORD
            LOCAL index := 0 AS DWORD
            BEGIN LOCK sync
                IF (LookupTable:TryGetValue(strValue, OUT index))
                    NOP
                ELSE
                    index := (DWORD) LookupTable:Count
                    LookupTable:TryAdd(strValue, index)
                    Strings:Add(strValue)
                ENDIF
            END LOCK
            RETURN index

        INTERNAL STATIC METHOD GetString(index AS DWORD) AS STRING
            BEGIN LOCK sync
                IF (INT) index < Strings:Count
                    RETURN Strings[(INT) index]
                ENDIF
            END LOCK
            RETURN ""

        INTERNAL STATIC PROPERTY Count AS LONG
        GET
            RETURN LookupTable:Count
        END GET
        END PROPERTY

        INTERNAL STATIC METHOD TryGetValue(s AS STRING, index OUT DWORD) AS LOGIC
            BEGIN LOCK sync
                RETURN LookupTable:TryGetValue(s, OUT index)
            END LOCK
        END METHOD

#endregion

    END CLASS
#endregion


    /// <exclude/>
    METHOD CompareTo(o AS OBJECT) AS LONG
        RETURN SELF:CompareTo((SYMBOL)o)


    /// <exclude/>
    METHOD CompareTo(rhs AS SYMBOL) AS LONG
        RETURN __StringCompare(SELF:_value, rhs:_value)

#region IConvertible Methods

    /// <inheritdoc />
    METHOD IConvertible.GetTypeCode() AS TypeCode
        RETURN TypeCode.Object

    /// <inheritdoc />
    METHOD IConvertible.ToBoolean( provider AS IFormatProvider ) AS LOGIC
        RETURN ((IConvertible)_value):ToBoolean( provider )

    /// <inheritdoc />
    METHOD IConvertible.ToByte( provider AS IFormatProvider ) AS BYTE
        RETURN ((IConvertible)_value):ToByte( provider )

    /// <inheritdoc />
    METHOD IConvertible.ToChar( provider AS IFormatProvider ) AS CHAR
        RETURN ((IConvertible)_value):ToChar( provider )

    /// <inheritdoc />
    METHOD IConvertible.ToDateTime( provider AS IFormatProvider ) AS DateTime
        RETURN ((IConvertible)_value):ToDateTime( provider )

    /// <inheritdoc />
    METHOD IConvertible.ToDecimal( provider AS IFormatProvider ) AS Decimal
        RETURN ((IConvertible)_value):ToDecimal( provider )

    /// <inheritdoc />
    METHOD IConvertible.ToDouble( provider AS IFormatProvider ) AS Double
        RETURN ((IConvertible)_value):ToDouble( provider )

    /// <inheritdoc />
    METHOD IConvertible.ToInt16( provider AS IFormatProvider ) AS Int16
        RETURN ((IConvertible)_value):ToInt16( provider )

    /// <inheritdoc />
    METHOD IConvertible.ToInt32( provider AS IFormatProvider ) AS Int32
        RETURN ((IConvertible)_value):ToInt32( provider )

    /// <inheritdoc />
    METHOD IConvertible.ToInt64( provider AS IFormatProvider ) AS INT64
        RETURN ((IConvertible)_value):ToInt64( provider )

    /// <inheritdoc />
    METHOD IConvertible.ToSByte( provider AS IFormatProvider ) AS SByte
        RETURN ((IConvertible)_value):ToSByte( provider )

    /// <inheritdoc />
    METHOD IConvertible.ToSingle( provider AS IFormatProvider ) AS Single
        RETURN ((IConvertible)_value):ToSingle( provider )

    /// <inheritdoc />
    METHOD IConvertible.ToString( provider AS IFormatProvider ) AS STRING
        RETURN ((IConvertible)_value):ToString( provider )

    /// <inheritdoc />
    METHOD IConvertible.ToType( conversionType AS Type, provider AS IFormatProvider ) AS OBJECT
        IF conversionType == TYPEOF(__Symbol)
            RETURN SELF
        ELSEIF conversionType == TYPEOF(System.String)
            RETURN _value
        ENDIF
        RETURN ((IConvertible)_value):ToType(conversionType, provider)

    /// <inheritdoc />
    METHOD IConvertible.ToUInt16( provider AS IFormatProvider ) AS UInt16
        RETURN ((IConvertible)_value):ToUInt16( provider )

    /// <inheritdoc />
    METHOD IConvertible.ToUInt32( provider AS IFormatProvider ) AS UInt32
        RETURN ((IConvertible)_value):ToUInt32( provider )

    /// <inheritdoc />
    METHOD IConvertible.ToUInt64( provider AS IFormatProvider ) AS UINT64
        RETURN ((IConvertible)_value):ToUInt64( provider )

#endregion
#region IClonable
    /// <exclude />
    METHOD Clone() AS OBJECT
        RETURN SYMBOL{SELF:_index}
#endregion

#region IEnumerable
#region ISerializable
    /// <inheritdoc/>
    PUBLIC METHOD GetObjectData(info AS SerializationInfo, context AS StreamingContext) AS VOID
        IF info == NULL
            THROW System.ArgumentException{"info"}
        ENDIF
        info:AddValue("Value", SymbolTable.GetString(SELF:_index))
        RETURN

    /// <include file="RTComments.xml" path="Comments/SerializeConstructor/*" />
    CONSTRUCTOR (info AS SerializationInfo, context AS StreamingContext)
        IF info == NULL
            THROW System.ArgumentException{"info"}
        ENDIF
        _index := SymbolTable.Add(info:GetString("Value"))
#endregion

        // Vulcan.Symbol
        //METHOD GetEnumerator() as IEnumerator
        //return SymbolTable.strings.GetEnumerator()
    METHOD ToDebugString() AS STRING
        RETURN SELF:_value

#endregion
END STRUCTURE
//        INTERNAL CLASS SymbolDebugView
//        PRIVATE _svalue AS SYMBOL
//            CONSTRUCTOR( s AS SYMBOL)
//        _svalue := s
//
//        [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)] ;
//        PUBLIC PROPERTY @@Value AS OBJECT GET _svalue:_value
//    END CLASS

END NAMESPACE
