//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System
using System.Collections
using System.Collections.Generic
using System.Diagnostics
using static XSharp.Functions

// Todo: Implement System.IConvertible ?

begin namespace XSharp
	[DebuggerDisplay("{_debuggervalue,nq}",Type := "SYMBOL")];
    [DebuggerTypeProxy(typeof(SymbolDebugView))];
	public structure __Symbol ;
		implements IEqualityComparer<__Symbol>, ;
		IEquatable<__Symbol>,;
		ICloneable ;
		//IEnumerable ;
		//System.IConvertible,;
		
		#region fields
			private initonly _index		as dword
			private static _PszDict			as Dictionary<DWORD, PSZ>
			// next field is only used when someone requests a PSZ representation 
			// by calling SysGetAtomName
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
		internal static method Find(sValue as string ) as __symbol
			if SymbolTable:LookupTable:ContainsKey(sValue)
				var index := __Symbol.SymbolTable:LookupTable[sValue]
				return __Symbol{index}
			endif
			return __Symbol{0}


		internal property _value as STRING
		get
			return SymbolTable.GetString(self:_index)
		end get
		end property
		internal property _debuggervalue as STRING
		get
			if (_index == 0)
				return "NULL_SYMBOL"
			endif
			return "#"+_value
		end get
		end property
		internal static property PszDict as Dictionary<dword, Psz>
		get
			if _pszDict == null
				_pszDict := Dictionary<dword, Psz>{}
			endif
			return _PszDict
		end get
		end property
		
		#region methods
			virtual method Equals(obj as object) as logic 
				local rhs as symbol
				if (obj == null)
					return false
				endif
				rhs := (symbol)(obj)
				if (rhs == null)
					return false
				endif
				return self:Equals(rhs)
			
			virtual method GetHashCode() as long
				return self:_index:GetHashCode()
			
			method GetHashCode(obj as Symbol) as long
				return obj:GetHashCode()
			
			method GetTypeCode() as TypeCode
				return TypeCode.String
			
			virtual method ToString() as string
				return _value

			method SysGetAtomName() as Psz
				if PszDict:ContainsKey(_index)
					return PszDict[_index]
				endif
				local pszAtom as PSZ
				pszAtom := __Psz.CreatePsz(_value)
				PszDict:add(_index, pszAtom)
				return pszAtom

		#endregion
		
		#region Equality
			method Equals(symOther as symbol) as logic
				return self:_index == symOther:_index
			
			method Equals(x as symbol, y as symbol) as logic
				return x:_index == y:_index
			
			method Equals(s as string) as logic
				return self:_value == s
			
		#endregion
		#region Operators
			
			static operator ==(lhs as symbol, rhs as symbol) as logic
				return lhs:_index == rhs:_index
			
			static operator !=(a as symbol, b as symbol) as logic
				return a:_index != b:_index
			
			static operator ==(lhs as symbol, rhs as string) as logic
				return lhs:_value == rhs
			
			static operator !=(lhs as Symbol, rhs as string) as logic
				return lhs:_value != rhs
			
			static operator ==(lhs as string, rhs as Symbol) as logic
				return lhs == rhs:_value
			
			static operator !=(lhs as string, rhs as Symbol) as logic
				return lhs != rhs:_value
			
			static operator ==(lhs as Symbol, rhs as dword) as logic
				return lhs:_index == rhs
			
			static operator !=(lhs as Symbol, rhs as dword) as logic
				return lhs:_index != rhs
			
			static operator ==(lhs as dword, rhs as Symbol) as logic
				return lhs == rhs:_index
			
			static operator !=(lhs as dword, rhs as Symbol) as logic
				return lhs != rhs:_index
			
			static operator explicit(value as dword) as Symbol
				if value <= SymbolTable.Count
					return Symbol{value}
				endif
				return Symbol{0}
			
			static operator explicit(value as Symbol) as dword
				return value:_index
			
			static operator implicit(value as string) as Symbol
				return Symbol{value, true}
			
			static operator implicit(value as Symbol) as string
				return value:_value
			
			// relative comparisons
			// compare symbols or symbols and strings
			static operator >(lhs as Symbol, rhs as Symbol) as logic
				return __StringCompare(lhs:_value, rhs:_value) > 0
			
			static operator >(lhs as Symbol, rhs as string) as logic
				return __StringCompare(lhs:_value, rhs) > 0
			
			static operator >(lhs as string, rhs as Symbol) as logic
				return __StringCompare(lhs, rhs:_value) > 0
			
			static operator <(lhs as Symbol, rhs as Symbol) as logic
				return __StringCompare(lhs:_value, rhs:_value) < 0
			
			static operator <(lhs as Symbol, rhs as string) as logic
				return __StringCompare(lhs:_value, rhs) < 0
			
			static operator <(lhs as string, rhs as Symbol) as logic
				return __StringCompare(lhs, rhs:_value) < 0
			
			// or Equals
			static operator >=(lhs as Symbol, rhs as Symbol) as logic
				return __StringCompare(lhs:_value, rhs:_value) >= 0
			
			static operator >=(lhs as Symbol, rhs as string) as logic
				return __StringCompare(lhs:_value, rhs) >= 0
			
			static operator >=(lhs as string, rhs as Symbol) as logic
				return __StringCompare(lhs, rhs:_value) >= 0
			
			static operator <=(lhs as Symbol, rhs as Symbol) as logic
				return __StringCompare(lhs:_value, rhs:_value) <= 0
			
			static operator <=(lhs as Symbol, rhs as string) as logic
				return __StringCompare(lhs:_value, rhs) <= 0
			
			static operator <=(lhs as string, rhs as Symbol) as logic
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
						LookupTable:Add("", 0)
						Strings:Add("")
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
								Strings:Add(strValue)
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
				return Symbol{self:_index}
		#endregion
		
		#region IEnumerable
			// Vulcan.Symbol
			//METHOD GetEnumerator() as IEnumerator
			//return SymbolTable.strings.GetEnumerator()
			
		#endregion
	end structure
    internal class SymbolDebugView
        private _svalue as Symbol
		constructor( s as Symbol)
        _svalue := s
            
        [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)] ;
        public property Value as object get _svalue:_VALUE
            
    end class

end namespace