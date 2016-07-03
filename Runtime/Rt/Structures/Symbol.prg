//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
using Vulcan

//STRUCTURE Vulcan.__Symbol
	//PRIVATE Value as Long
	//CONSTRUCTOR(s as STRING)
		//Value := 1
	//OPERATOR IMPLICIT(s as STRING) AS __Symbol
		//RETURN __Symbol{s}
	//PROPERTY _stringValue as LONG GET Value
//END STRUCTURE
begin namespace Vulcan
	PUBLIC CLASS __Symbol IMPLEMENTS System.Collections.Generic.IEqualityComparer<__Symbol>, System.IEquatable<__Symbol>
		#region fields
		PRIVATE index		AS Long
		PRIVATE stringValue AS string
		#endregion

		#region constrúctors
		CONSTRUCTOR(value AS string,  upperCase := TRUE AS Logic)
			SUPER()
			if (string.IsNullOrEmpty(value)  )
				SELF:stringValue := string.Empty
			else
				SELF:stringValue:=  value
			endif
			IF (upperCase)
				SELF:stringValue := SELF:stringValue:ToUpperInvariant()
			ENDIF
			__SymbolDictionary.Add(SELF)
		return
		#endregion
		#region methods
		VIRTUAL METHOD Equals(obj AS Object) AS Logic
			LOCAL other AS __Symbol
			IF (obj == null)
				RETURN FALSE
			ENDIF
			other := (__Symbol)(obj)
			IF (other == null)
				RETURN FALSE
			ENDIF
		RETURN SELF:Equals(other)

		VIRTUAL METHOD Equals(other AS __Symbol) AS Logic
			IF (other == null)
				RETURN FALSE
			ENDIF
		RETURN SELF:stringValue:Equals(other:stringValue)

		VIRTUAL METHOD Equals(x AS __Symbol, y AS __Symbol) AS Logic
			IF ((x == null) .AND. (y == null))
				RETURN TRUE
			ENDIF
			IF (x == null)
				RETURN FALSE
			ENDIF
		RETURN x:Equals(y)

		VIRTUAL METHOD GetHashCode() AS Long
		RETURN SELF:index

		VIRTUAL METHOD GetHashCode(obj AS __Symbol) AS Long
		RETURN obj:index

		STATIC OPERATOR ==(a AS __Symbol, b AS __Symbol) AS Logic
			IF ((a == null) .OR. (b == null))
				RETURN Object.Equals(a, b)
			ENDIF
		RETURN (a:index == b:index)

		STATIC OPERATOR implicit(value AS string) AS __Symbol
		RETURN __Symbol{value, TRUE}

		STATIC OPERATOR implicit(new__Symbol AS __Symbol) AS string
		RETURN new__Symbol:stringValue

		STATIC OPERATOR !=(a AS __Symbol, b AS __Symbol) AS Logic
		RETURN ! (a == b)

		VIRTUAL METHOD ToString() AS string
			if ( string.IsNullOrEmpty(stringValue) ) 
				return string.Empty
			endif
		return self:stringValue
		#endregion
		#region internal types
		INTERNAL STATIC  CLASS __SymbolDictionary
			#region fields
			STATIC INITONLY PRIVATE __SymbolLookup AS System.Collections.Generic.Dictionary<string,Long>
			STATIC INITONLY PRIVATE __Symbols := System.Collections.Generic.List<__Symbol>{} AS System.Collections.Generic.List<__Symbol>
			STATIC PRIVATE sync AS Object
			STATIC INITONLY PRIVATE NON_EXISTING___Symbol := __Symbol{"", FALSE} AS __Symbol
			#endregion
			#region constructors
			STATIC  CONSTRUCTOR()
				LOCAL dictionary AS System.Collections.Generic.Dictionary<string,Long>
				dictionary := System.Collections.Generic.Dictionary<string,Long>{}
				Add(NON_EXISTING___Symbol)
				__SymbolDictionary.__SymbolLookup := dictionary
				__SymbolDictionary.sync := Object{}
			#endregion
			#region methods
			INTERNAL STATIC METHOD Add(new__Symbol AS __Symbol) AS void
				LOCAL syncObj AS Object
				IF (new__Symbol != __SymbolDictionary.NON_EXISTING___Symbol)
					syncObj := __SymbolDictionary.sync
					BEGIN LOCK syncObj
						IF (__SymbolDictionary.__SymbolLookup:ContainsKey(new__Symbol:stringValue))
							new__Symbol:index := __SymbolDictionary.__SymbolLookup[new__Symbol:stringValue]
						ELSE
							__SymbolDictionary.__Symbols:Add(new__Symbol)
							new__Symbol:index := __SymbolDictionary.__Symbols:Count
							__SymbolDictionary.__SymbolLookup:Add(new__Symbol:stringValue, new__Symbol:index)
						ENDIF
					END LOCK
				ENDIF
			return 

			INTERNAL STATIC METHOD Find(s AS string) AS __Symbol
				LOCAL num AS Long
				BEGIN LOCK __SymbolDictionary.sync
					IF (__SymbolDictionary.__SymbolLookup:ContainsKey(s))
						num := __SymbolDictionary.__SymbolLookup[s]
						RETURN __SymbolDictionary.__Symbols[num]
					ENDIF
				END LOCK
			RETURN __SymbolDictionary.NON_EXISTING___Symbol

			INTERNAL STATIC PROPERTY Count AS Long
				GET
					RETURN __Symbols:Count
				END GET
			END PROPERTY
			#endregion

		END CLASS
		#endregion

	END CLASS
end namespace