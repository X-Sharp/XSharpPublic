//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Text
USING System.Runtime.InteropServices
USING System.Runtime.CompilerServices
USING System.Diagnostics
#pragma options ("az", on)
BEGIN NAMESPACE XSharp
    // use explicit layout so we can compact the size into 12 bytes
    // Type is Immutable, so has no settable properties
    /// <summary>Internal type that implements the FoxPro Compatible BINARY type.<br/>
    /// This type has many operators and implicit converters that normally are never directly called from user code.
    /// The data in this type is stored as an array of Bytes<br/>
    /// Conversions from and to String are supported and they use the current active windows codepage.
    /// </summary>
    PUBLIC STRUCTURE __Binary IMPLEMENTS IFormattable, ;
        IComparable<__Binary>, ;
        IEquatable<__Binary>, ;
        IComparable
    
        [DebuggerBrowsable(DebuggerBrowsableState.Never)];
        PRIVATE INITONLY _value AS BYTE[]

        #region constructors
        /// <include file="RTComments.xml" path="Comments/Constructor/*" />
        /// <param name="b">Byte[] value that has the bytes that define the binary</param>
        [DebuggerStepThroughAttribute] [MethodImpl(MethodImplOptions.AggressiveInlining)];        
        CONSTRUCTOR (b as Byte[])
            IF b == NULL
                THROW NullError()
            ENDIF
            SELF:_value    := b

        PRIVATE STATIC METHOD NullError() as Error
            VAR err			 := Error{ArgumentException{}}
            err:Gencode		 := Gencode.EG_ARG
            err:ArgNum		 := 1
            err:FuncSym		 := "Binary.ctor"
            err:Description  := "Argument cannot be null"
            err:Args         := <OBJECT> {NULL}
            return err

        PRIVATE CONSTRUCTOR( lhs as byte[], rhs as byte[]) 
            var len := lhs:Length + rhs:Length
            var result := byte[]{len}
            System.Array.Copy(lhs, result, lhs:Length)
            System.Array.Copy(rhs, 0, result,lhs:Length, rhs:Length)
            _value := result
            
            
        #endregion
        #region Properties
        /// <summary>Binary value as array of Bytes</summary>
        PROPERTY @@Value    AS Byte[]	GET _value
        PROPERTY Length     AS LONG GET iif(_value == NULL, 0, _value:Length)
        #endregion
        
        #region Equality Operators
        /// <inheritdoc />
        OVERRIDE METHOD Equals(rhs AS OBJECT  ) AS LOGIC
            LOCAL result AS LOGIC
            IF rhs != NULL .AND. rhs IS __Binary
                result := SELF:Equals( (__Binary) rhs)
            ELSE
                result := FALSE
            ENDIF
            RETURN result
            
        /// <inheritdoc />
        METHOD Equals(rhs AS __Binary ) AS LOGIC
            IF SELF:Length != rhs:Length
                RETURN FALSE
            ENDIF
            FOR VAR i := 0 to SELF:Length-1
                IF SELF:Value[i] != rhs:Value[i]
                    RETURN FALSE
                ENDIF
            NEXT
            RETURN TRUE
            
            /// <inheritdoc />
        OVERRIDE METHOD GetHashCode() AS INT
            RETURN SELF:_value:GetHashCode()
            
            /// <exclude />	
        METHOD GetTypeCode() AS TypeCode
            RETURN TypeCode.Object
            
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR ==(lhs AS __Binary, rhs AS __Binary) AS LOGIC
            RETURN lhs:Equals(rhs)
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR !=(lhs AS __Binary, rhs AS __Binary) AS LOGIC
            RETURN ! lhs:Equals(rhs)
            #endregion
            
        #region Comparison Operators
        /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR >(lhs AS __Binary, rhs AS __Binary) AS LOGIC
            var len := Math.Min(lhs:Length,rhs:Length)
            var res := RuntimeState.StringCompare(lhs:Value, rhs:Value, len)
            if res > 0
                return TRUE
            elseif res < 0
                return FALSE
            endif
            RETURN lhs:Length > rhs:Length
            
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR <(lhs AS __Binary, rhs AS __Binary) AS LOGIC
            var len := Math.Min(lhs:Length,rhs:Length)
            var res := RuntimeState.StringCompare(lhs:Value, rhs:Value, len)
            if res < 0
                return TRUE
            elseif res > 0
                return FALSE
            endif
            RETURN lhs:Length < rhs:Length
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR >=(lhs AS __Binary, rhs AS __Binary) AS LOGIC
            var len := Math.Min(lhs:Length,rhs:Length)
            var res := RuntimeState.StringCompare(lhs:Value, rhs:Value, len)
            if res > 0
                return TRUE
            elseif res < 0
                return FALSE
            endif
            RETURN lhs:Length >= rhs:Length
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR <=(lhs AS __Binary, rhs AS __Binary) AS LOGIC
            var len := Math.Min(lhs:Length,rhs:Length)
            var res := RuntimeState.StringCompare(lhs:Value, rhs:Value, len)
            if res < 0
                return TRUE
            elseif res > 0
                return FALSE
            endif
            RETURN lhs:Length <= rhs:Length
            
            #endregion
            
        #region Implicit Converters
        /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(b AS BYTE[]) AS __Binary
            RETURN __Binary{b}
            
            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(b AS __Binary) AS BYTE[]
            RETURN b:Value
            

        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(bytes AS __Binary) AS STRING
            RETURN RuntimeState.WinEncoding:GetString(bytes:Value)

        [DebuggerStepThroughAttribute];
        STATIC OPERATOR IMPLICIT(s AS STRING) AS __Binary
            RETURN __Binary{ RuntimeState.WinEncoding:GetBytes(s) }

            #endregion
            
        #region Numeric Operators


            /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR+(lhs AS __Binary, rhs AS __Binary) AS __Binary
            return __Binary{lhs:Value, rhs:Value}
            
        /// <include file="RTComments.xml" path="Comments/Operator/*" />
        OPERATOR+(lhs AS __Binary, rhs AS STRING) AS __Binary
            return __Binary{lhs:Value, RuntimeState.WinEncoding:GetBytes(rhs)}

        OPERATOR+(lhs AS STRING, rhs AS __Binary) AS STRING
            var sb := StringBuilder{}
            sb:Append(lhs)
            sb:Append(RuntimeState.WinEncoding:GetString(rhs))
            RETURN sb:ToString()

        #endregion
        #region Unary Operators
            
            #endregion
        #region Add and Subtract
        /// <exclude />	
        METHOD Add(rhs AS __Binary) AS __Binary
            RETURN SELF + rhs
            
        /// <inheritdoc />
        PUBLIC METHOD CompareTo(rhs AS __Binary) AS INT
            var len := Math.Min(SELF:Length,rhs:Length)
            for var i := 0 to len-1
                if SELF:Value[i] > rhs:Value[i]
                    return 1
                ELSEIF SELF:Value[i] < rhs:Value[i]
                    return -1
                ENDIF
            next
            if SELF:Length > rhs:Length
                RETURN 1
            ELSEIF SELF:Length < rhs:Length
                return -1
            ENDIF
            RETURN 0
            /// <inheritdoc />
        PUBLIC METHOD CompareTo(rhs AS OBJECT) AS INT
            RETURN SELF:CompareTo( (__Binary) rhs)
            #endregion

        #region IFormattable
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD ToString() AS STRING
            RETURN ToString("")
            
        /// <inheritdoc cref="M:System.Double.ToString(System.String)"/>
        PUBLIC METHOD ToString(sFormat AS STRING) AS STRING
            if sFormat == "G"
                return RuntimeState:WinEncoding:GetString(self:Value)
            endif
            var sb := StringBuilder{}
            sb:Append("0h")
            foreach var b in self:Value
                sb:Append(b:ToString("X"))
            next
            return sb:ToString()
            /// <inheritdoc />
        PUBLIC METHOD ToString(format AS STRING, provider AS System.IFormatProvider) AS STRING
            RETURN ToString(format)
            #endregion

    END STRUCTURE
    
END NAMESPACE
