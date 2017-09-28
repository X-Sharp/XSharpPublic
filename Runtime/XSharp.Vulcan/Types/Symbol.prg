//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections
USING System.Collections.Generic

// Todo: Implement System.IConvertible ?

BEGIN NAMESPACE XSharp
PUBLIC STRUCTURE __Symbol ;
	IMPLEMENTS IEqualityComparer<__Symbol>, ;
	IEquatable<__Symbol>,;
	ICloneable ;
	//IEnumerable ;
	//System.IConvertible,;

    #region fields
    PRIVATE INITONLY _index		AS DWORD
	#endregion

    #region constrúctors
	STATIC CONSTRUCTOR
		SymbolTable.Initialize()

    CONSTRUCTOR(value AS string,  upperCase := TRUE AS Logic)
		IF (upperCase)
			value := value:ToUpperInvariant()
		endif
		_index := SymbolTable.Add(value)
	RETURN

	PRIVATE CONSTRUCTOR (value AS DWORD)
		SELF:_index := value

    #endregion

   PROPERTY _value as STRING GET SymbolTable.GetString(_index)
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

    VIRTUAL METHOD GetHashCode() AS Long
		RETURN SELF:_index:GetHashCode()

    METHOD GetHashCode(obj AS __Symbol) AS Long
		RETURN obj:GetHashCode()

    VIRTUAL METHOD ToString() AS string
		return _value
	#endregion

#region Equality
    METHOD Equals(other AS __Symbol) AS Logic
		RETURN SELF:_index == other:_index

     METHOD Equals(x AS __Symbol, y AS __Symbol) AS Logic
		RETURN x:_index == y:_index

    METHOD Equals(s AS String) AS Logic
		RETURN SELF:_value == s

#endregion
#region Operators

    STATIC OPERATOR ==(lhs AS __Symbol, rhs AS __Symbol) AS Logic
	    RETURN lhs:_index == rhs:_index

    STATIC OPERATOR !=(a AS __Symbol, b AS __Symbol) AS Logic
		RETURN a:_index != b:_index

    STATIC OPERATOR ==(lhs AS __Symbol, rhs AS STRING) AS LOGIC
		RETURN lhs:_value == rhs

    STATIC OPERATOR !=(lhs AS __Symbol, rhs AS STRING) AS LOGIC
		RETURN lhs:_value != rhs

    STATIC OPERATOR ==(lhs AS String, rhs AS __Symbol) AS Logic
	    RETURN lhs == rhs:_value

    STATIC OPERATOR !=(lhs AS String, rhs AS __Symbol) AS Logic
	    RETURN lhs != rhs:_value

    STATIC OPERATOR ==(lhs AS __Symbol, rhs AS DWORD) AS LOGIC
		RETURN lhs:_index == rhs

    STATIC OPERATOR !=(lhs AS __Symbol, rhs AS DWORD) AS LOGIC
		RETURN lhs:_index != rhs

    STATIC OPERATOR ==(lhs AS DWORD, rhs AS __Symbol) AS LOGIC
		RETURN lhs == rhs:_index

    STATIC OPERATOR !=(lhs AS DWORD, rhs AS __Symbol) AS LOGIC
		RETURN lhs != rhs:_index

    STATIC OPERATOR EXPLICIT(value AS DWORD) AS __Symbol
		IF value <= SymbolTable.Count
			RETURN __Symbol{value}
		ENDIF
		RETURN __Symbol{0}

    STATIC OPERATOR EXPLICIT(value AS __Symbol) AS DWORD
		RETURN value:_index

    STATIC OPERATOR IMPLICIT(value AS string) AS __Symbol
		RETURN __Symbol{value, TRUE}

    STATIC OPERATOR IMPLICIT(value AS __Symbol) AS string
		RETURN value._value

	// relative comparisons
	// compare symbols or symbols and strings
	STATIC OPERATOR >(lhs AS __Symbol, rhs AS __Symbol) as LOGIC
		RETURN __StringCompare(lhs:_value, rhs:_value) > 0

	STATIC OPERATOR >(lhs AS __Symbol, rhs AS String) as LOGIC
		RETURN __StringCompare(lhs:_value, rhs) > 0

	STATIC OPERATOR >(lhs AS STRING, rhs AS __Symbol) as LOGIC
		RETURN __StringCompare(lhs, rhs:_value) > 0

	STATIC OPERATOR <(lhs AS __Symbol, rhs AS __Symbol) as LOGIC
		return __StringCompare(lhs:_value, rhs:_value) < 0

	STATIC OPERATOR <(lhs AS __Symbol, rhs AS String) as LOGIC
		RETURN __StringCompare(lhs:_value, rhs) < 0

	STATIC OPERATOR <(lhs AS STRING, rhs AS __Symbol) as LOGIC
		RETURN __StringCompare(lhs, rhs:_value) < 0

	// or Equals
	STATIC OPERATOR >=(lhs AS __Symbol, rhs AS __Symbol) as LOGIC
		RETURN __StringCompare(lhs:_value, rhs:_value) >= 0

	STATIC OPERATOR >=(lhs AS __Symbol, rhs AS String) as LOGIC
		RETURN __StringCompare(lhs:_value, rhs) >= 0

	STATIC OPERATOR >=(lhs AS STRING, rhs AS __Symbol) as LOGIC
		RETURN __StringCompare(lhs, rhs:_value) >= 0

	STATIC OPERATOR <=(lhs AS __Symbol, rhs AS __Symbol) as LOGIC
		return __StringCompare(lhs:_value, rhs:_value) <= 0

	STATIC OPERATOR <=(lhs AS __Symbol, rhs AS String) as LOGIC
		RETURN __StringCompare(lhs:_value, rhs) <= 0

	STATIC OPERATOR <=(lhs AS STRING, rhs AS __Symbol) as LOGIC
		RETURN __StringCompare(lhs, rhs:_value) <= 0

#endregion


	#region internal types
    INTERNAL STATIC  CLASS SymbolTable
        #region fields
		// Note that we are not using a ConcurrentDictionary since we want to keep the LookupTable and List
		// in sync. Therefore we handle our own locking in this class
        STATIC INTERNAL LookupTable AS Dictionary<STRING,DWORD>
        STATIC INTERNAL Strings		as List<String>
        STATIC PRIVATE sync AS Object
		#endregion

        #region constructors
        STATIC METHOD Initialize() AS VOID
            sync		:= OBJECT{}
			LookupTable := Dictionary<STRING,DWORD>{}
         Strings := List<String>{}
			LookupTable.Add("", 0)
         Strings.Add("")
		#endregion

		#region methods
        INTERNAL STATIC METHOD Add(strValue AS STRING) AS DWORD
			local index := 0 as DWORD
			BEGIN LOCK sync
                IF (LookupTable:ContainsKey(strValue))
					index := LookupTable[strValue]
                ELSE
                    index := (DWORD) LookupTable:Count
                    LookupTable:Add(strValue, index)
                    Strings.Add(strValue)
                ENDIF
            END LOCK
		RETURN index

   INTERNAL STATIC METHOD GetString(index as DWORD) AS STRING
      if (INT) index < Strings:Count
         return Strings[(int) index]
      endif
      return ""
      
        INTERNAL STATIC PROPERTY Count AS Long
            GET
                RETURN LookupTable:Count
            END GET
        END PROPERTY

		#endregion

    END CLASS
	#endregion

	#region IClonable
	METHOD Clone() AS OBJECT
		RETURN __Symbol{SELF:_index}
	#endregion

	#region IEnumerable
	// Vulcan.__Symbol
	//METHOD GetEnumerator() as IEnumerator
		//return SymbolTable.strings.GetEnumerator()

	#endregion
END STRUCTURE
END NAMESPACE