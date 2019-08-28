//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Runtime.InteropServices
USING System.Diagnostics
USING System.Text
USING System.Reflection
BEGIN NAMESPACE XSharp
	
	/// <summary>Internal type that implements the VO Compatible PSZ type.<br/>
	/// This type has many operators and implicit converters that normally are never directly called from user code.
	/// </summary>
    /// <include file="RTComments.xml" path="Comments/PSZ/*" />
	[DebuggerDisplay( "{DebuggerString(),nq}", Type := "PSZ" ) ] ;
	STRUCTURE __Psz IMPLEMENTS  IEquatable<__Psz>
		PRIVATE _value AS BYTE PTR
		/// <exclude />	
		STATIC PROPERTY _NULL_PSZ AS __Psz GET (__Psz) IntPtr.zero
		PRIVATE STATIC _pszList AS List< IntPtr>
		INTERNAL STATIC METHOD RegisterPsz(pszToRegister AS PSZ) AS VOID
			IF _pszList == NULL
				_pszList := List<IntPtr>{}
				AppDomain.CurrentDomain:ProcessExit += System.EventHandler{ NULL, @__FreePSZs() }
			ENDIF
			IF !_pszList:Contains(pszToRegister:Address)
				_pszList:add(pszToRegister:Address)
			ENDIF
			RETURN
		
		INTERNAL STATIC METHOD CreatePsz( cString AS STRING) AS PSZ
				RETURN PSZ{cString}
		
		PRIVATE STATIC METHOD __FreePSZs(o AS OBJECT, args AS EventArgs ) AS VOID
			FOREACH VAR pszToFree IN _pszList
				TRY
					MemFree(pszToFree)
				CATCH
					NOP
				END TRY
			NEXT
			_pszList := NULL
		
		/// <include file="RTComments.xml" path="Comments/Constructor/*" />
		CONSTRUCTOR (s AS STRING)
			// this constructor has a memory leak
			// there is no garbage collection for structures
			// to free the memory we need to call MemFree on the pointer
			_value := String2Mem(s)
			RegisterPsz(_value)
			RETURN
		
		/// <include file="RTComments.xml" path="Comments/Constructor/*" />
		CONSTRUCTOR (p AS IntPtr)
			_value := p
		
		/// <exclude/>
        OVERRIDE METHOD ToString() AS STRING
            IF IsValid
                RETURN Mem2String( _value, Length )
            ENDIF
            RETURN "<Invalid PSZ>("+ IntPtr{_value}:ToString()+")"
	
		/// <exclude />	
		METHOD DebuggerString() AS STRING
			RETURN IIF( _value == NULL_PTR, "NULL_PSZ", e"\""+ tostring() +  e"\"" )
		
		/// <exclude />	
		METHOD Equals( p AS PSZ ) AS LOGIC
			
			LOCAL ret := FALSE AS LOGIC
			IF _value == p:_value
				ret := TRUE
			ELSEIF _value != NULL .AND. p:_value != NULL
				ret := __StringCompare( ToString(), p:ToString() ) == 0
			ENDIF
			RETURN ret   
		
		INTERNAL METHOD LessThan( p AS PSZ ) AS LOGIC
			// todo: should this follow nation rules ?
			LOCAL ret := FALSE AS LOGIC
			IF _value == p:_value
				ret := FALSE
			ELSEIF _value != NULL .AND. p:_value != NULL
				ret := __StringCompare( ToString(), p:ToString() ) < 0
			ENDIF
			RETURN ret       

		INTERNAL METHOD GreaterThan( p AS PSZ ) AS LOGIC
			LOCAL ret := FALSE AS LOGIC
			// todo: should this follow nation rules ?
			IF _value == p:_value
				ret := FALSE
			ELSEIF _value != NULL .AND. p:_value != NULL
				ret := __StringCompare( ToString(), p:ToString() ) > 0
			ENDIF
			RETURN ret     
		
		
			/// <exclude/>
         OVERRIDE METHOD Equals( o AS OBJECT ) AS LOGIC
			LOCAL ret := FALSE AS LOGIC
			
			IF o IS PSZ
				ret := SELF:Equals( (PSZ) o )
			ENDIF
			
		RETURN ret
		
		/// <inheritdoc />
		OVERRIDE METHOD GetHashCode() AS INT
			RETURN (INT) _value
		
		/// <exclude />	
		METHOD Free() AS VOID
			IF _value != NULL_PTR
				MemFree( _value )
				_value := NULL_PTR
			ENDIF
			RETURN

        PRIVATE PROPERTY IsValid AS LOGIC
            GET
                TRY
                    Marshal.ReadByte(_value)
                    RETURN TRUE
                CATCH
                    RETURN FALSE
                END TRY
            END GET
        END PROPERTY
		/// <exclude />
		PROPERTY Length AS DWORD
			GET
				LOCAL len AS DWORD
				len := 0
                IF !IsValid
                    RETURN len
                ENDIF
				IF _value != NULL_PTR
					DO WHILE _value[len+1] != 0
						len++
					ENDDO
				ENDIF
				RETURN len 
			END GET
		END PROPERTY
		/// <exclude />
		PROPERTY IsEmpty AS LOGIC
			GET
				LOCAL empty := TRUE AS LOGIC
                IF IsValid
				    LOCAL b AS BYTE
				    LOCAL x := 1 AS INT
				    IF _value != NULL_PTR
					    b := _value[x]
					    DO WHILE b != 0 .AND. empty
						    SWITCH b
							CASE 32
							CASE 13
							CASE 10
							CASE 9
								NOP
							OTHERWISE
								empty := FALSE
						    END SWITCH
						    x += 1
						    b := _value[x]
					    ENDDO
				    ENDIF
                ENDIF
				RETURN empty
				
				
			END GET
		END PROPERTY
		/// <exclude />
		PROPERTY IsNull AS LOGIC GET _value == NULL
		/// <exclude />
		PROPERTY Address AS IntPtr GET _value
		/// <exclude />
		PROPERTY SELF[index AS INT] AS BYTE
			GET
                IF !IsValid
                    RETURN 0
                ENDIF
				RETURN _value[index + __ARRAYBASE__]
			END GET
			SET
                IF IsValid
				    _value[index + __ARRAYBASE__] := VALUE
                ENDIF
			END SET
		END PROPERTY
		
		#region OPERATOR methods
			// binary
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR +( lhs AS PSZ, rhs AS PSZ ) AS PSZ
				RETURN PSZ{ lhs:ToString() + rhs:ToString() }
			
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR +( lhs AS PSZ, rhs AS STRING ) AS PSZ
				RETURN PSZ{ lhs:ToString() + rhs }
			
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR +( lhs AS STRING, rhs AS PSZ ) AS STRING
				RETURN lhs + rhs:ToString()
			
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR -( lhs AS PSZ, rhs AS PSZ ) AS PSZ
				LOCAL l   := lhs:ToString() AS STRING
				LOCAL r   := rhs:ToString() AS STRING
				RETURN PSZ{ String.Concat( l:TrimEnd(), r:TrimEnd() ):PadRight( l:Length + r:Length ) }
			
			
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR -( lhs AS PSZ, rhs AS STRING ) AS PSZ
				LOCAL l   := lhs:ToString() AS STRING
				RETURN PSZ{ String.Concat( l:TrimEnd(), rhs:TrimEnd() ):PadRight( l:Length + rhs:Length ) }
			
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR -( lhs AS STRING, rhs AS PSZ ) AS STRING
				LOCAL r   := rhs:ToString() AS STRING
				RETURN String.Concat( lhs:TrimEnd(), r:TrimEnd() ):PadRight( lhs:Length + r:Length )
			
			// Comparison Operators
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR ==( lhs AS PSZ, rhs AS PSZ ) AS LOGIC
				RETURN lhs:Equals( rhs )
			
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR !=( lhs AS PSZ, rhs AS PSZ ) AS LOGIC
				RETURN ! lhs:Equals( rhs )
			
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR <( lhs AS PSZ, rhs AS PSZ ) AS LOGIC
				RETURN lhs:LessThan( rhs )
			
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR <=( lhs AS PSZ, rhs AS PSZ ) AS LOGIC
				RETURN ! lhs:GreaterThan( rhs )
			
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR >( lhs AS PSZ, rhs AS PSZ ) AS LOGIC
				RETURN lhs:GreaterThan( rhs )
			
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR >=( lhs AS PSZ, rhs AS PSZ ) AS LOGIC
				RETURN ! lhs:LessThan( rhs )
			
			// Conversion Operators - To PSZ...  
			
			// PTR -> PSZ
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR IMPLICIT( p AS PTR ) AS PSZ
				RETURN PSZ{ (IntPtr) p }
			
			// BYTE PTR -> PSZ
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR IMPLICIT( p AS BYTE PTR ) AS PSZ
				RETURN PSZ{ (IntPtr) p }
			
			// SByte PTR -> PSZ
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR IMPLICIT( p AS SByte PTR ) AS PSZ
				RETURN PSZ{ (IntPtr) p }
			
			// IntPtr -> PSZ
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR IMPLICIT( p AS IntPtr ) AS PSZ
				RETURN PSZ{ p }
			
			// INT -> PSZ
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR IMPLICIT( i AS INT ) AS PSZ
				RETURN PSZ{ IntPtr{ i } }
			
			// DWORD -> PSZ
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR IMPLICIT( d AS DWORD ) AS PSZ
				RETURN PSZ{ IntPtr{ (INT64) d} } 
			
			///////////////////////////////////////////////////////////////////////////
			// Conversion Operators - From PSZ...  
			
			// PSZ -> PTR
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR IMPLICIT( p AS PSZ ) AS PTR
				RETURN p:_value
			
			// PSZ -> BYTE PTR
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR IMPLICIT( p AS PSZ ) AS BYTE PTR
				RETURN p:_value
			
			// PSZ -> SByte PTR
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR IMPLICIT( p AS PSZ ) AS SByte PTR
				RETURN (SByte PTR) p:_value
			
			// PSZ -> IntPtr
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR IMPLICIT( p AS PSZ ) AS IntPtr
				RETURN p:_value
			
			// PSZ -> STRING
			OPERATOR IMPLICIT( p AS PSZ ) AS STRING
				RETURN p:ToString()
			
			// PSZ -> INT
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR IMPLICIT( p AS PSZ ) AS INT
				RETURN (INT) p:_value
			
			// PSZ -> INT64
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR IMPLICIT( p AS PSZ ) AS INT64
				RETURN (INT64) p:_value
			
			// PSZ -> DWORD
			/// <include file="RTComments.xml" path="Comments/Operator/*" />
			OPERATOR IMPLICIT( p AS PSZ ) AS DWORD
				RETURN (DWORD) p:_value			
		#endregion
		
		
	END STRUCTURE
	
END NAMESPACE




// This function is handled by the compiler. The runtime function should never be called
/// <exclude />
FUNCTION Cast2Psz(cSource AS STRING) AS PSZ
	THROW NotImplementedException{}

// This function is handled by the compiler. The runtime function should never be called
/// <exclude />
FUNCTION String2Psz(cSource AS STRING) AS PSZ
	THROW NotImplementedException{}

