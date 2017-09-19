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
	ICloneable,;
	IEnumerable ;
	//System.IConvertible,;

    #region fields
    PRIVATE INITONLY _index		AS DWORD
	#endregion

    #region constrúctors
    CONSTRUCTOR(value AS string,  upperCase := TRUE AS Logic)
		if (string.IsNullOrEmpty(value)  )
			self:_index := 0
		ELSE
			IF (upperCase)
				value := value:ToUpperInvariant()
			endif
			_index := SymbolTable.Add(value)
		endif
	RETURN

	PRIVATE CONSTRUCTOR (value AS DWORD)
		SELF:_index := value

    #endregion

	#region Properties
	PRIVATE PROPERTY StringValue AS STRING
	GET
		IF _index == 0
			RETURN String.Empty
		ENDIF
		return SymbolTable.getString(_index)
	END GET
	END PROPERTY
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

    VIRTUAL METHOD GetHashCode() AS Long
		RETURN SELF:_index:GetHashCode()

    METHOD GetHashCode(obj AS __Symbol) AS Long
		RETURN obj:GetHashCode()

    VIRTUAL METHOD ToString() AS string
		if _index == 0
			return string.Empty
		endif
		return self:stringValue
	#endregion

#region Equality
    METHOD Equals(other AS __Symbol) AS Logic
		RETURN SELF:_index == other:_index

     METHOD Equals(x AS __Symbol, y AS __Symbol) AS Logic
		RETURN x:_index == y:_index

    METHOD Equals(s AS String) AS Logic
		RETURN SELF:StringValue == s

#endregion
#region Operators

    STATIC OPERATOR ==(lhs AS __Symbol, rhs AS __Symbol) AS Logic
	    RETURN lhs:_index == rhs:_index

    STATIC OPERATOR !=(a AS __Symbol, b AS __Symbol) AS Logic
		RETURN a:_index != b:_index

    STATIC OPERATOR ==(lhs AS __Symbol, rhs AS STRING) AS LOGIC
		RETURN lhs:StringValue == rhs

    STATIC OPERATOR !=(lhs AS __Symbol, rhs AS STRING) AS LOGIC
		RETURN lhs:StringValue != rhs

    STATIC OPERATOR ==(lhs AS String, rhs AS __Symbol) AS Logic
	    RETURN lhs == rhs:StringValue 

    STATIC OPERATOR !=(lhs AS String, rhs AS __Symbol) AS Logic
	    RETURN lhs != rhs:StringValue 

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
		RETURN value:stringValue

	// relative comparisons
	// compare symbols or symbols and strings
	STATIC OPERATOR >(lhs AS __Symbol, rhs AS __Symbol) as LOGIC
		RETURN __StringCompare(lhs:StringValue, rhs:StringValue) > 0

	STATIC OPERATOR >(lhs AS __Symbol, rhs AS String) as LOGIC
		RETURN __StringCompare(lhs:StringValue, rhs) > 0

	STATIC OPERATOR >(lhs AS STRING, rhs AS __Symbol) as LOGIC
		RETURN __StringCompare(lhs, rhs:StringValue) > 0

	STATIC OPERATOR <(lhs AS __Symbol, rhs AS __Symbol) as LOGIC
		return __StringCompare(lhs:StringValue, rhs:StringValue) < 0

	STATIC OPERATOR <(lhs AS __Symbol, rhs AS String) as LOGIC
		RETURN __StringCompare(lhs:StringValue, rhs) < 0

	STATIC OPERATOR <(lhs AS STRING, rhs AS __Symbol) as LOGIC
		RETURN __StringCompare(lhs, rhs:StringValue) < 0

	// or Equals
	STATIC OPERATOR >=(lhs AS __Symbol, rhs AS __Symbol) as LOGIC
		RETURN __StringCompare(lhs:StringValue, rhs:StringValue) >= 0

	STATIC OPERATOR >=(lhs AS __Symbol, rhs AS String) as LOGIC
		RETURN __StringCompare(lhs:StringValue, rhs) >= 0

	STATIC OPERATOR >=(lhs AS STRING, rhs AS __Symbol) as LOGIC
		RETURN __StringCompare(lhs, rhs:StringValue) >= 0

	STATIC OPERATOR <=(lhs AS __Symbol, rhs AS __Symbol) as LOGIC
		return __StringCompare(lhs:StringValue, rhs:StringValue) <= 0

	STATIC OPERATOR <=(lhs AS __Symbol, rhs AS String) as LOGIC
		RETURN __StringCompare(lhs:StringValue, rhs) <= 0

	STATIC OPERATOR <=(lhs AS STRING, rhs AS __Symbol) as LOGIC
		RETURN __StringCompare(lhs, rhs:StringValue) <= 0

#endregion


	#region internal types
    INTERNAL STATIC  CLASS SymbolTable
        #region fields
		// Note that we are not using a ConcurrentDictionary since we want to keep the LookupTable and List
		// in sync. Therefore we handle our own locking in this class
        STATIC INITONLY INTERNAL LookupTable AS Dictionary<STRING,DWORD>
        STATIC INITONLY INTERNAL Strings AS List<String>
        STATIC INITONLY PRIVATE sync AS Object
        STATIC INITONLY INTERNAL EmptySymbol AS __Symbol
		#endregion

        #region constructors
        STATIC  CONSTRUCTOR()
			EmptySymbol := __Symbol{null, FALSE} 
            sync		:= OBJECT{}
			Strings     := List<String>{} 
            LookupTable := Dictionary<STRING,DWORD>{}
			// Add empty string to tables
			Strings.Add(String.Empty)
			LookupTable.Add(String.Empty, 0)
			LookupTable.Add(null, 0)
		#endregion

		#region methods
        INTERNAL STATIC METHOD Add(stringvalue AS STRING) AS DWORD
			local index := 0 as DWORD
            IF !String.IsNullOrEmpty(stringvalue)
				BEGIN LOCK sync
                    IF (LookupTable:ContainsKey(stringvalue))
						index := LookupTable[stringValue]
                    ELSE
                        strings:Add(stringvalue)
                        index := (DWORD) strings:Count
                        LookupTable:Add(stringValue, index)
                    ENDIF
                END LOCK
            ENDIF
		RETURN index

        INTERNAL STATIC METHOD Find(s AS string) AS __Symbol
            LOCAL index as DWORD
            BEGIN LOCK sync
                IF (LookupTable:ContainsKey(s))
                    index := LookupTable[s]
                    RETURN __Symbol{index}
                ENDIF
            END LOCK
			RETURN EmptySymbol

        INTERNAL STATIC PROPERTY Count AS Long
            GET
                RETURN strings:Count
            END GET
        END PROPERTY

		INTERNAL STATIC METHOD getString(index as DWORD) AS __Symbol
			IF (INT) index <= strings:Count
				return strings[(int) index]
			ENDIF
			RETURN String.Empty
		#endregion

    END CLASS
	#endregion

	#region IClonable
	METHOD Clone() AS OBJECT
		RETURN __Symbol{SELF:_index}
	#endregion

	#region IEnumerable
	// Vulcan.__Symbol
	METHOD GetEnumerator() as IEnumerator
		return SymbolTable.strings.GetEnumerator()

	#endregion
END STRUCTURE
END NAMESPACE