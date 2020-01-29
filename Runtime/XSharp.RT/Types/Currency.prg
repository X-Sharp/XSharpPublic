//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Runtime.InteropServices
USING System.Runtime.CompilerServices
USING System.Diagnostics

BEGIN NAMESPACE XSharp
    // use explicit layout so we can compact the size into 12 bytes
    // Type is Immutable, so no settable properties
    /// <summary>Internal type that implements the VO Compatible FLOAT type.
    /// This type has many operators and implicit converters that normally are never directly called from user code.
    /// </summary>
    /// <seealso cref="T:XSharp.IFloat"/>
    /// <seealso cref="T:XSharp.RDD.DbFloat"/>
    PUBLIC STRUCTURE __Currency IMPLEMENTS IConvertible,; 
        IFormattable, ;
        IComparable<__Currency>, ;
        IEquatable<__Currency>, ;
        IComparable
    
        PRIVATE INITONLY _value AS System.Decimal
        
        #region constructors
        /// <include file="RTComments.xml" path="Comments/Constructor/*" />
        /// <param name="r8">Real8 value to convert to a FLOAT</param>
        [DebuggerStepThroughAttribute] [MethodImpl(MethodImplOptions.AggressiveInlining)];        
        CONSTRUCTOR (r8 AS REAL8)
            SELF:_value    := Math.Round((Decimal)r8,4)
            
        /// <include file="RTComments.xml" path="Comments/Constructor/*" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)];        
        CONSTRUCTOR (d AS System.Decimal)
            SELF:_value    := Math.Round(d,4)            
        /// <include file="RTComments.xml" path="Comments/Constructor/*" />
        [DebuggerStepThroughAttribute] [MethodImpl(MethodImplOptions.AggressiveInlining)];        
        CONSTRUCTOR (f AS IFloat)
            SELF:_value		:= Math.Round((Decimal)f:Value,4)

        #endregion
        #region Properties
        /// <summary>REAL8 (System.Double) value</summary>
        PROPERTY @@Value    AS System.Decimal	GET _value			
        #endregion
        
        #region Equality Operators
        /// <inheritdoc />
        OVERRIDE METHOD Equals(rhs AS OBJECT  ) AS LOGIC
            LOCAL result AS LOGIC
            IF rhs != NULL .AND. rhs IS __Currency
                result := SELF:Equals( (__Currency) rhs)
            ELSE
                result := FALSE
            ENDIF
            RETURN result
            
        /// <inheritdoc />
        METHOD Equals(rhs AS __Currency ) AS LOGIC
            RETURN SELF:Value == rhs:Value
            
            /// <inheritdoc />
        OVERRIDE METHOD GetHashCode() AS INT
            RETURN SELF:_value:GetHashCode()
            
            /// <exclude />	
        METHOD GetTypeCode() AS TypeCode
            RETURN TypeCode.Decimal
            
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR ==(lhs AS __Currency, rhs AS __Currency) AS LOGIC
            RETURN lhs:Equals(rhs)
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR !=(lhs AS __Currency, rhs AS __Currency) AS LOGIC
            RETURN ! lhs:Equals(rhs)
            #endregion
            
        #region Comparison Operators
        /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR >(lhs AS __Currency, rhs AS __Currency) AS LOGIC
            RETURN lhs:_value > rhs:_value
            
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR <(lhs AS __Currency, rhs AS __Currency) AS LOGIC
            RETURN lhs:_value < rhs:_value
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR >=(lhs AS __Currency, rhs AS __Currency) AS LOGIC
            RETURN lhs:_value >= rhs:_value
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR <=(lhs AS __Currency, rhs AS __Currency) AS LOGIC
            RETURN lhs:_value <= rhs:_value
            
            #endregion
            
        #region Implicit Converters
        /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(b AS BYTE) AS __Currency
            RETURN __Currency{(Decimal) b}
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(sb AS SByte) AS __Currency
            RETURN __Currency{(Decimal) sb}
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(si AS SHORT) AS __Currency
            RETURN __Currency{(Decimal)si}
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(w AS WORD) AS __Currency
            RETURN __Currency{(Decimal)w}
            /// <include file="RTComments.xml" path="Comments/Converter/*" />
            
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(i AS INT) AS __Currency
            RETURN __Currency{(Decimal)i}
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(dw AS DWORD) AS __Currency
            RETURN __Currency{(Decimal)dw}
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(i64 AS INT64) AS __Currency
            RETURN __Currency{(Decimal)i64}
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(ui64 AS UINT64) AS __Currency
            RETURN __Currency{(Decimal)ui64}
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(r4 AS REAL4) AS __Currency
            RETURN __Currency{(REAL8)r4}
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(r8 AS REAL8) AS __Currency
            RETURN __Currency{r8}
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(val AS System.Decimal) AS __Currency
            RETURN __Currency{val}
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(c  AS __Currency) AS REAL8
            RETURN (REAL8) c:_value
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(c  AS __Currency) AS REAL4
            RETURN (REAL4) c:_value
            

        /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(c  AS __Currency) AS FLOAT
            RETURN FLOAT{ (REAL8) c:_value, -1, 4}

            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(c  AS __Currency) AS System.Decimal
            RETURN c:_value

        #endregion
        #region Explicit Converters
        /// <include file="RTComments.xml" path="Comments/Converter/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR EXPLICIT(c  AS __Currency) AS BYTE
            IF RuntimeState.CompilerOptionVO11
                RETURN Convert.ToByte(c:_value)
            ENDIF
            RETURN (BYTE) c:_value
            /// <include file="RTComments.xml" path="Comments/Converter/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR EXPLICIT(c AS __Currency) AS SByte
            RETURN (SByte) c:_value
            /// <include file="RTComments.xml" path="Comments/Converter/*" />

        [DebuggerStepThroughAttribute];
        STATIC OPERATOR EXPLICIT(c  AS __Currency) AS SHORT
            IF RuntimeState.CompilerOptionVO11
                RETURN Convert.ToInt16(c:_value)
            ENDIF
            RETURN (SHORT)c:_value
            
            /// <include file="RTComments.xml" path="Comments/Converter/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR EXPLICIT(c  AS __Currency) AS WORD
            RETURN (WORD) c:_value
            /// <include file="RTComments.xml" path="Comments/Converter/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR EXPLICIT(c  AS __Currency) AS LONG
            IF RuntimeState.CompilerOptionVO11
                RETURN Convert.ToInt32(c:_value)
            ENDIF
            RETURN (LONG) c:_value
            /// <include file="RTComments.xml" path="Comments/Converter/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR EXPLICIT(c  AS __Currency) AS DWORD
            RETURN (DWORD) c:_value
            /// <include file="RTComments.xml" path="Comments/Converter/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR EXPLICIT(c  AS __Currency) AS INT64
            IF RuntimeState.CompilerOptionVO11
                RETURN Convert.ToInt64(c:_value)
            ENDIF
            RETURN (INT64) c:_value
            
            /// <include file="RTComments.xml" path="Comments/Converter/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR EXPLICIT(c  AS __Currency) AS UINT64
            RETURN (UINT64) c:_value
            
            #endregion
            
        #region Numeric Operators
        /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR +(c  AS __Currency) AS __Currency
            RETURN c
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR -(c  AS __Currency) AS __Currency
            RETURN -c
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR+(lhs AS __Currency, rhs AS __Currency) AS __Currency
            RETURN lhs:Add(rhs)
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR+(lhs AS __Currency, rhs AS USUAL) AS __Currency
            RETURN lhs:Add(rhs)
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR+(lhs AS USUAL, rhs AS __Currency) AS __Currency
            RETURN rhs:Add(lhs)
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR-(lhs AS __Currency, rhs AS __Currency) AS __Currency
            RETURN lhs:Subtract(rhs)
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR-(lhs AS __Currency, rhs AS USUAL) AS __Currency
            RETURN lhs:Subtract(rhs)
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR-(lhs AS USUAL, rhs AS __Currency) AS __Currency
            // set decimals for LHS to 0, so max decmals is decimals right
            RETURN __Currency{lhs}:Subtract(rhs)		
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR*(lhs AS __Currency, rhs AS __Currency) AS __Currency
            RETURN __Currency{ lhs:_value * rhs:_value}
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR/(lhs AS __Currency, rhs AS __Currency) AS __Currency
            RETURN __Currency{ lhs:_value / rhs:_value}
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR%(lhs AS __Currency, rhs AS __Currency) AS __Currency
            RETURN __Currency{ lhs:_value % rhs:_value}
            
            #endregion
        #region Unary Operators
        /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR ++ (c  AS __Currency) AS __Currency
            RETURN __Currency{c:_value+1}
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR -- (c  AS __Currency) AS __Currency
            RETURN __Currency{c:_value-1}
            #endregion
            
        #region Explicit casts. Used inside Transform
        /// <exclude />	
        METHOD CastToInt() AS INT
            RETURN (INT)(SELF:_value)
            
            /// <exclude />	
        METHOD CastToInt64() AS INT64
            RETURN (INT64)(SELF:_value)
            
            #endregion
        #region Add and Subtract
        /// <exclude />	
        METHOD Add(rhs AS __Currency) AS __Currency
            RETURN __Currency{ SELF:_value + rhs:_value}
            
            /// <exclude />	
        METHOD Add(rhs AS USUAL) AS __Currency
            LOCAL result AS __Currency
            IF rhs:IsFloat
                result := SELF:Add ( (__Currency) rhs)
            ELSEIF rhs:IsDecimal .OR. rhs:IsCurrency
                result := SELF:Add ( (System.Decimal) rhs)
            ELSEIF  rhs:IsLong
                result := __Currency{ SELF:_value + (LONG) rhs}
            ELSE
                THROW Error.ArgumentError(__ENTITY__,Nameof(rhs), "Argument is not numeric")
            ENDIF
            RETURN result
            
            
            /// <exclude />	
        METHOD Subtract(rhs AS __Currency) AS __Currency
            RETURN __Currency{ SELF:_value - rhs:_value}
            
            /// <exclude />	
        METHOD Subtract(rhs AS USUAL) AS __Currency
            LOCAL result AS __Currency
            IF rhs:IsFloat
                result := SELF:Subtract( (__Currency) rhs)
            ELSEIF rhs:IsDecimal .OR. rhs:IsCurrency
                result := SELF:Subtract( (System.Decimal) rhs)
            ELSEIF  rhs:IsLong
                result := __Currency{ SELF:_value - (LONG) rhs}			
            ELSE
                THROW Error.ArgumentError(__ENTITY__,Nameof(rhs), "Argument is not numeric")
            ENDIF
            RETURN result
            
            
            #endregion
            
        #region IConvertable
        /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToBoolean(provider AS System.IFormatProvider) AS LOGIC
            RETURN _value ==  0.0m
            
            /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToByte(provider AS System.IFormatProvider) AS BYTE
            RETURN ((IConvertible) _value):ToByte(provider)
            
            /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToChar(provider AS System.IFormatProvider) AS CHAR
            RETURN ((IConvertible) _value):ToChar(provider)
            
            /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToDateTime(provider AS System.IFormatProvider) AS System.DateTime
            RETURN ((IConvertible) _value):ToDateTime(provider)
            
            /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToDecimal(provider AS System.IFormatProvider) AS Decimal
            RETURN ((IConvertible) _value):ToDecimal(provider)
            
            /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToDouble(provider AS System.IFormatProvider) AS REAL8
            RETURN ((IConvertible) _value):ToDouble(provider)
            
            /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToInt16(provider AS System.IFormatProvider) AS SHORT
            RETURN ((IConvertible) _value):ToInt16(provider)
            
            /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToInt32(provider AS System.IFormatProvider) AS LONG
            RETURN ((IConvertible) _value):ToInt32(provider)
            
            /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToInt64(provider AS System.IFormatProvider) AS INT64
            RETURN ((IConvertible) _value):ToInt64(provider)
            
            /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToSByte(provider AS System.IFormatProvider) AS SByte
            RETURN ((IConvertible) _value):ToSByte(provider)
            
            /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToSingle(provider AS System.IFormatProvider) AS REAL4
            RETURN ((IConvertible) _value):ToSingle(provider)
            
            /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToType(conversionType AS System.Type, provider AS System.IFormatProvider) AS OBJECT
            RETURN ((IConvertible) _value):ToType(conversionType, provider)
            
            /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToUInt16(provider AS System.IFormatProvider) AS WORD
            RETURN ((IConvertible) _value):ToUInt16(provider)
            
            /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToUInt32(provider AS System.IFormatProvider) AS DWORD
            RETURN ((IConvertible) _value):ToUInt32(provider)
            
            /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToUInt64(provider AS System.IFormatProvider) AS UINT64
            RETURN ((IConvertible) _value):ToUInt64(provider)
            
            /// <inheritdoc />
        PUBLIC METHOD IConvertible.ToString(provider AS System.IFormatProvider) AS STRING
            RETURN ((IConvertible) _value):ToString(provider)
            #endregion
        #region IFormattable
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD ToString() AS STRING
            RETURN ToString("0.0000")
            
        /// <inheritdoc cref="M:System.Double.ToString(System.String)"/>
        PUBLIC METHOD ToString(sFormat AS STRING) AS STRING
            RETURN _value:ToString(sFormat)
            
            /// <inheritdoc />
        PUBLIC METHOD ToString(format AS STRING, provider AS System.IFormatProvider) AS STRING
            RETURN ((IFormattable) _value):ToString(format, provider)
            #endregion
        #region IComparable
        /// <inheritdoc />
        PUBLIC METHOD CompareTo(rhs AS __Currency) AS INT
            RETURN _Value:CompareTo( rhs:_Value)
            
            /// <inheritdoc />
        PUBLIC METHOD CompareTo(rhs AS OBJECT) AS INT
            RETURN SELF:CompareTo( (__Currency) rhs)
            #endregion
            
    END STRUCTURE
    
END NAMESPACE
