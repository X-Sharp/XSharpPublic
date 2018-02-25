//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System
using System.Collections
using System.Collections.Generic
using System.Diagnostics
// Todo: Implement System.IConvertible ?

begin namespace XSharp
	[DebuggerDisplay("{ToString(),nq}", Type := "SYMBOL")];
	public structure __Symbol ;
		implements IEqualityComparer<__Symbol>, ;
		IEquatable<__Symbol>,;
		ICloneable ;
		//IEnumerable ;
		//System.IConvertible,;
		
		#region fields
			private initonly _index		as dword
		#endregion
		
		#region constrúctors
			static constructor
				SymbolTable.Initialize()
			
			constructor(value as string)
				self(value, true)
			
			constructor(value as string,  upperCase as logic)
				
				if (upperCase)
					value := value:ToUpperInvariant()
				endif
				_index := SymbolTable.Add(value)
				return
			
			private constructor (value as dword)
				self:_index := value
			
		#endregion
		
		property _value as string get SymbolTable.GetString(_index)
		#region methods
			virtual method Equals(obj as object) as logic
				local other as __Symbol
				if (obj == null)
					return false
				endif
				other := (__Symbol)(obj)
				if (other == null)
					return false
				endif
				return self:Equals(other)
			
			virtual method GetHashCode() as long
				return self:_index:GetHashCode()
			
			method GetHashCode(obj as __Symbol) as long
				return obj:GetHashCode()
			
			method GetTypeCode() as TypeCode
				return TypeCode.String
			
			virtual method ToString() as string
				return _value
		#endregion
		
		#region Equality
			method Equals(other as __Symbol) as logic
				return self:_index == other:_index
			
			method Equals(x as __Symbol, y as __Symbol) as logic
				return x:_index == y:_index
			
			method Equals(s as string) as logic
				return self:_value == s
			
		#endregion
		#region Operators
			
			static operator ==(lhs as __Symbol, rhs as __Symbol) as logic
				return lhs:_index == rhs:_index
			
			static operator !=(a as __Symbol, b as __Symbol) as logic
				return a:_index != b:_index
			
			static operator ==(lhs as __Symbol, rhs as string) as logic
				return lhs:_value == rhs
			
			static operator !=(lhs as __Symbol, rhs as string) as logic
				return lhs:_value != rhs
			
			static operator ==(lhs as string, rhs as __Symbol) as logic
				return lhs == rhs:_value
			
			static operator !=(lhs as string, rhs as __Symbol) as logic
				return lhs != rhs:_value
			
			static operator ==(lhs as __Symbol, rhs as dword) as logic
				return lhs:_index == rhs
			
			static operator !=(lhs as __Symbol, rhs as dword) as logic
				return lhs:_index != rhs
			
			static operator ==(lhs as dword, rhs as __Symbol) as logic
				return lhs == rhs:_index
			
			static operator !=(lhs as dword, rhs as __Symbol) as logic
				return lhs != rhs:_index
			
			static operator explicit(value as dword) as __Symbol
				if value <= SymbolTable.Count
					return __Symbol{value}
				endif
				return __Symbol{0}
			
			static operator explicit(value as __Symbol) as dword
				return value:_index
			
			static operator implicit(value as string) as __Symbol
				return __Symbol{value, true}
			
			static operator implicit(value as __Symbol) as string
				return value._value
			
			// relative comparisons
			// compare symbols or symbols and strings
			static operator >(lhs as __Symbol, rhs as __Symbol) as logic
				return __StringCompare(lhs:_value, rhs:_value) > 0
			
			static operator >(lhs as __Symbol, rhs as string) as logic
				return __StringCompare(lhs:_value, rhs) > 0
			
			static operator >(lhs as string, rhs as __Symbol) as logic
				return __StringCompare(lhs, rhs:_value) > 0
			
			static operator <(lhs as __Symbol, rhs as __Symbol) as logic
				return __StringCompare(lhs:_value, rhs:_value) < 0
			
			static operator <(lhs as __Symbol, rhs as string) as logic
				return __StringCompare(lhs:_value, rhs) < 0
			
			static operator <(lhs as string, rhs as __Symbol) as logic
				return __StringCompare(lhs, rhs:_value) < 0
			
			// or Equals
			static operator >=(lhs as __Symbol, rhs as __Symbol) as logic
				return __StringCompare(lhs:_value, rhs:_value) >= 0
			
			static operator >=(lhs as __Symbol, rhs as string) as logic
				return __StringCompare(lhs:_value, rhs) >= 0
			
			static operator >=(lhs as string, rhs as __Symbol) as logic
				return __StringCompare(lhs, rhs:_value) >= 0
			
			static operator <=(lhs as __Symbol, rhs as __Symbol) as logic
				return __StringCompare(lhs:_value, rhs:_value) <= 0
			
			static operator <=(lhs as __Symbol, rhs as string) as logic
				return __StringCompare(lhs:_value, rhs) <= 0
			
			static operator <=(lhs as string, rhs as __Symbol) as logic
				return __StringCompare(lhs, rhs:_value) <= 0
			
		#endregion
		
		
		#region internal types
			internal static  class SymbolTable
				#region fields
					// Note that we are not using a ConcurrentDictionary since we want to keep the LookupTable and List
					// in sync. Therefore we handle our own locking in this class
					static internal LookupTable as Dictionary<string,dword>
					static internal Strings		as List<string>
					static private sync as object
				#endregion
				
				#region constructors
					static method Initialize() as void
						sync		:= object{}
						LookupTable := Dictionary<string,dword>{}
						Strings := List<string>{}
						LookupTable.Add("", 0)
						Strings.Add("")
				#endregion
				
				#region methods
					internal static method Add(strValue as string) as dword
						local index := 0 as dword
						begin lock sync
							if (LookupTable:ContainsKey(strValue))
								index := LookupTable[strValue]
							else
								index := (dword) LookupTable:Count
								LookupTable:Add(strValue, index)
								Strings.Add(strValue)
							endif
						end lock
						return index
					
					internal static method GetString(index as dword) as string
						if (int) index < Strings:Count
							return Strings[(int) index]
						endif
						return ""
					
					internal static property Count as long
						get
							return LookupTable:Count
						end get
					end property
					
				#endregion
				
			end class
		#endregion
		
		#region IClonable
			method Clone() as object
				return __Symbol{self:_index}
		#endregion
		
		#region IEnumerable
			// Vulcan.__Symbol
			//METHOD GetEnumerator() as IEnumerator
			//return SymbolTable.strings.GetEnumerator()
			
		#endregion
end	structure
end namespace