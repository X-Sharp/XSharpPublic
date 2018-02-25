//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Generic
using System.Runtime.InteropServices
using System.Diagnostics
using System.Text
using System.Reflection
begin namespace XSharp
	
	
	[DebuggerDisplay( "{DebuggerString(),nq}", Type := "PSZ" ), DefaultMember( "Item" ) ] ;
	unsafe structure __Psz
		private _value as byte ptr
		static property _NULL_PSZ as __Psz Get (__Psz) IntPtr.zero
		
		
		constructor (s as string)
			// this constructor has a memory leak
			// there is no garbage collection for structures
			// to free the memory we need to call MemFree on the pointer
			_value := String2Mem(s)
			return
		
		constructor (p as IntPtr)
			_value := p
		
		override method ToString() as string
			return Mem2String( _value, Length ) 
		
		method DebuggerString() as string
			return iif( _value == null_ptr, "NULL_PSZ", e"\""+ tostring() +  e"\"" )
		

		method equals( p as __Psz ) as logic
			
			local ret := FALSE as logic
			if _value == p:_value
				ret := true
			elseif _value != null && p:_value != null
				ret := String.CompareOrdinal( ToString(), p:ToString() ) == 0
			endif
			return ret   
		
		method LessThan( p as __Psz ) as logic
			// todo: should this follow nation rules ?
			local ret := FALSE as logic
			if _value == p:_value
				ret := false
			elseif _value != null && p:_value != null
				ret := String.CompareOrdinal( ToString(), p:ToString() ) < 0
			endif
			return ret       
		
		method GreaterThan( p as __Psz ) as logic
			local ret := FALSE as logic
			// todo: should this follow nation rules ?
			if _value == p:_value
				ret := false
			elseif _value != null && p:_value != null
				ret := String.CompareOrdinal( ToString(), p:ToString() ) > 0
			endif
			return ret     
		
		
		override method Equals( o as object ) as logic
			local ret := FALSE as logic
			
			if o is __Psz
				ret := Equals( (__Psz) o )
			endif
			
			return ret
		
		override method GetHashCode() as int
			return (int) _value
		
		method Free() as void
			if _value != null_ptr
				MemFree( _value )
				_value := null_ptr
			endif
			return
		
		property Length as dword
			get
				local len as dword
				len := 0
				if _value != null_ptr
					do while _value[len+1] != 0
						len++
					enddo
				endif
				return len 
			end get
		end property
		
		property IsEmpty as logic
			get
				local empty := true as logic
				local b as byte
				local x := 1 as int
				if _value != null_ptr
					b := _value[x]
					do while b != 0 .and. empty
						switch b
							case 32
							case 13
							case 10
							case 9
						nop
							otherwise
						empty := false
						end switch
						x += 1
						b := _value[x]
					enddo
				endif
				return empty
				
				
			end get
		end property
		property IsNull as logic get _value == null
		property Address as IntPtr get _value
		property Item[offset as dword] as byte
			get
				return _value[offset]
			end get
			set
				_value[offset] := value
			end set
		end property
		
		
		
		#region operator methods
			// binary
			operator +( lhs as __Psz, rhs as __Psz ) as __Psz
				return __Psz{ lhs:ToString() + rhs:ToString() }
			
			operator +( lhs as __Psz, rhs as string ) as __Psz
				return __Psz{ lhs:ToString() + rhs }
			
			operator +( lhs as string, rhs as __Psz ) as string
				return lhs + rhs:ToString()
			
			operator -( lhs as __Psz, rhs as __Psz ) as __Psz
				local l   := lhs:ToString() as string
				local r   := rhs:ToString() as string
				return __Psz{ String.Concat( l:TrimEnd(), r:TrimEnd() ):PadRight( l:Length + r:Length ) }
			
			
			operator -( lhs as __Psz, rhs as string ) as __Psz
				local l   := lhs:ToString() as string
				return __Psz{ String.Concat( l:TrimEnd(), rhs:TrimEnd() ):PadRight( l:Length + rhs:Length ) }
			
			operator -( lhs as string, rhs as __Psz ) as string
				local r   := rhs:ToString() as string
				return String.Concat( lhs:TrimEnd(), r:TrimEnd() ):PadRight( lhs:Length + r:Length )
			
			// Comparison Operators
			operator ==( lhs as __Psz, rhs as __Psz ) as logic
				return lhs:Equals( rhs )
			
			operator !=( lhs as __Psz, rhs as __Psz ) as logic
				return ! lhs:Equals( rhs )
			
			operator <( lhs as __Psz, rhs as __Psz ) as logic
				return lhs:LessThan( rhs )
			
			operator <=( lhs as __Psz, rhs as __Psz ) as logic
				return ! lhs:GreaterThan( rhs )
			
			operator >( lhs as __Psz, rhs as __Psz ) as logic
				return lhs:GreaterThan( rhs )
			
			operator >=( lhs as __Psz, rhs as __Psz ) as logic
				return ! lhs:LessThan( rhs )
			
			// Conversion Operators - To PSZ...  
			
			// PTR -> PSZ
			operator implicit( p as ptr ) as __Psz
				return __Psz{ (IntPtr) p }
			
			// BYTE PTR -> PSZ
			operator implicit( p as byte ptr ) as __Psz
				return __Psz{ (IntPtr) p }
			
			// SByte PTR -> PSZ
			operator implicit( p as SByte ptr ) as __Psz
				return __Psz{ (IntPtr) p }
			
			// IntPtr -> PSZ
			operator implicit( p as IntPtr ) as __Psz
				return __Psz{ p }
			
			// INT -> PSZ
			operator implicit( i as int ) as __Psz
				return __Psz{ IntPtr{ i } }
			
			// DWORD -> PSZ
			operator implicit( d as dword ) as __Psz
				return __Psz{ IntPtr{ (ptr) d } }
			
			///////////////////////////////////////////////////////////////////////////
			// Conversion Operators - From PSZ...  
			
			// PSZ -> PTR
			operator implicit( p as __Psz ) as ptr
				return p:_value
			
			// PSZ -> BYTE PTR
			operator implicit( p as __Psz ) as byte ptr
				return p:_value
			
			// PSZ -> SByte PTR
			operator implicit( p as __Psz ) as SByte ptr
				return (SByte ptr) p:_value
			
			// PSZ -> IntPtr
			operator implicit( p as __Psz ) as IntPtr
				return p:_value
			
			// PSZ -> STRING
			operator implicit( p as __Psz ) as string
				return p:ToString()
			
			// PSZ -> INT
			operator implicit( p as __Psz ) as int
				return (int) p:_value
			
			// PSZ -> INT64
			operator implicit( p as __Psz ) as int64
				return (int64) p:_value
			
			// PSZ -> DWORD
			operator implicit( p as __Psz ) as dword
				return (dword) p:_value			
		#endregion
		
		
	end structure
	
end namespace






function String2Mem(s as string) as IntPtr
	local result := 0 as IntPtr
	if s != null
		var encoding := Encoding.Default
		var bytes    := encoding:GetBytes(s)
		var len      := bytes:Length
		result	     := MemAlloc((DWORD) (len+1))
		Marshal.Copy(bytes,0,result, len)	
	endif
	return result 

unsafe function Mem2String(pString as IntPtr, nLen as dword) as string
	if pString == IntPtr.Zero .or. nLen == 0
		return String.Empty
	endif
	var encoding := Encoding.Default
	var numchars := encoding:GetCharCount( (byte ptr) pString, (int) nLen) 
	var buffer   := (char ptr) MemAlloc( (dword) (numchars * sizeof(char)) )
	numchars     := encoding:GetChars((byte ptr) pString, (int) nLen, buffer, numchars)
	var result   := String {buffer, 0, numchars}
	MemFree(buffer)
	return result