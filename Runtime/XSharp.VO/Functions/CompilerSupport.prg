//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
// functions used by the compiler

/// <summary>
/// Compare 2 strings. This function is used by the compiler for string comparisons
/// </summary>
/// <param name="strLHS">The first string .</param>
/// <param name="strRHS">The second string.</param>
/// <returns>
/// -1 strLHS precedes strRHS in the sort order. 
///  0 strLHS occurs in the same position as strRHS in the sort order. 
///  1 strLHS follows strRHS in the sort order. 
/// Note this this function respects SetCollation() and SetExact()
/// </returns>
FUNCTION __StringCompare(strLHS AS STRING, strRHS AS STRING) AS INT
	LOCAL ret AS INT
	IF Object.ReferenceEquals(strLHS, strRHS)
		ret := 0
	ELSEIF strLHS == NULL
		IF strRHS == NULL			// null and null are equal
			ret := 0
		ELSE
			ret := -1				// null precedes a string
		ENDIF
	ELSEIF strRHS == NULL			// a string comes after null
		ret := 1
	ELSE							// both not null
		// With Not Exact comparison we only compare the length of the RHS string
		// and we always use the unicode comparison because that is what vulcan does
		// This is done to make sure that >= and <= will also return TRUE when the LHS is longer than the RHS
		// The ordinal comparison can be done here because when both strings start with the same characters
		// then equality is guaranteed regardless of collations or other rules.
		// collations and other rules are really only relevant when both strings are different
		IF  !RuntimeState.Exact
			LOCAL lengthRHS AS INT
			lengthRHS := strRHS:Length
			IF lengthRHS == 0 .or. lengthRHS <= strLHS:Length  .and. String.Compare( strLHS, 0, strRHS, 0, lengthRHS , StringComparison.Ordinal ) == 0
				RETURN 0
			ENDIF
		ENDIF
		// either exact or RHS longer than LHS
		VAR mode := RuntimeState.CollationMode 
		SWITCH mode
		CASE CollationMode.Windows
			ret := StringCompareHelpers.CompareWindows(strLHS, strRHS) 
		CASE CollationMode.Clipper
			ret := StringCompareHelpers.CompareClipper(strLHS, strRHS)
		CASE CollationMode.Unicode
			ret := String.Compare(strLHS, strRHS)
		OTHERWISE
			ret := String.CompareOrdinal(strLHS, strRHS)
		END SWITCH
	ENDIF
	RETURN ret

	/// <summary>
/// Compare 2 strings. This function is used by the compiler for string comparisons
/// </summary>
/// <param name="strLHS">The first string .</param>
/// <param name="strRHS">The second string.</param>
/// <returns>
/// TRUE when the strings are equal, FALSE when they are not equal
/// This function respects SetExact()
/// </returns>
FUNCTION  __StringEquals(strLHS AS STRING, strRHS AS STRING) AS LOGIC
	LOCAL IsEqual:= FALSE AS LOGIC
	LOCAL lengthRHS AS INT
	IF Object.ReferenceEquals(strLHS, strRHS)
		IsEqual := TRUE
	ELSEIF RuntimeState.Exact
		IsEqual := String.Equals(strLHS , strRHS)
	ELSEIF strLHS != NULL .and. strRHS != NULL
		lengthRHS := strRHS:Length
		IF lengthRHS == 0
			IsEqual := TRUE        // With SetExact(FALSE) then "aaa" = "" returns TRUE
		ELSEIF lengthRHS <= strLHS:Length
			
			IsEqual := String.Compare( strLHS, 0, strRHS, 0, lengthRHS, StringComparison.Ordinal ) == 0
		ENDIF
	ELSEIF strLHS == NULL .and. strRHS == NULL
		IsEqual := TRUE
	ENDIF
	RETURN IsEqual

	/// <summary>
/// Compare 2 strings. This function is used by the compiler for string comparisons
/// </summary>
/// <param name="strLHS">The first string .</param>
/// <param name="strRHS">The second string.</param>
/// <returns>
/// TRUE when the strings are not equal, FALSE when they are equal
/// This function respects SetExact()
/// </returns>
FUNCTION  __StringNotEquals(strLHS AS STRING, strRHS AS STRING) AS LOGIC
	LOCAL notEquals := FALSE AS LOGIC
	LOCAL lengthRHS AS INT
	IF Object.ReferenceEquals(strLHS, strRHS)
		notEquals := FALSE
	ELSEIF RuntimeState.Exact
		notEquals := !String.Equals(strLHS , strRHS)
	ELSEIF strLHS != NULL .and. strRHS != NULL
		// shortcut: chec first char
		lengthRHS := strRHS:Length
		IF lengthRHS == 0
			notEquals := FALSE        // With SetExact(FALSE) then "aaa" = "" returns TRUE
		ELSEIF lengthRHS <= strLHS:Length
			notEquals := String.Compare( strLHS, 0, strRHS, 0, lengthRHS, StringComparison.Ordinal ) != 0
		ELSE
			notEquals := TRUE
		ENDIF
	ELSEIF strLHS == NULL .and. strRHS == NULL
		notEquals := FALSE
	ELSE
		notEquals := TRUE
	ENDIF
	RETURN notEquals

/// <summary>
/// Remove leading and trailing spaces from a string.
/// </summary>
/// <param name="c">The string to be trimmed.</param>
/// <returns>
/// The original string without leading and trailing spaces
/// </returns>
// _FIELD->Name
FUNCTION __FieldGet( fieldName AS STRING ) AS USUAL
	RETURN NIL

// CUSTOMER->NAME
FUNCTION __FieldGetWa( alias AS STRING, fieldName AS STRING ) AS USUAL
	RETURN NIL

// _FIELD->Name := "Foo"
FUNCTION __FieldSet( fieldName AS STRING, uValue AS USUAL ) AS USUAL
	RETURN uValue

// CUSTOMER->Name := "Foo"
FUNCTION __FieldSetWa( alias AS STRING, fieldName AS STRING, uValue AS USUAL ) AS USUAL
	RETURN uValue


// MEMVAR myName
// ? MyName
FUNCTION __MemVarGet(cName AS STRING) AS USUAL
	RETURN NIL

// MEMVAR myName
// MyName := "NewValue"
FUNCTION __MemVarPut(cName AS STRING, uValue AS USUAL) AS USUAL
	RETURN uValue


// ALIAS->(DoSomething())
// is translated to
// __pushWorkarea( alias ) ; DoSomething() ; __popWorkArea()

FUNCTION __pushWorkarea( alias AS USUAL ) AS VOID
	RETURN 

FUNCTION __popWorkarea() AS VOID
   RETURN
