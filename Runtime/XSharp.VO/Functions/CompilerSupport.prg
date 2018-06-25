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
function __StringCompare(strLHS as string, strRHS as string) as int
	local ret as int
	IF Object.ReferenceEquals(strLHS, strRHS)
		ret := 0
	elseif strLHS == null
		if strRHS == null			// null and null are equal
			ret := 0
		else
			ret := -1				// null precedes a string
		endif
	elseif strRHS == null			// a string comes after null
		ret := 1
	else							// both not null
		// With Not Exact comparison we only compare the length of the RHS string
		// and we always use the unicode comparison because that is what vulcan does
		// This is done to make sure that >= and <= will also return TRUE when the LHS is longer than the RHS
		// The ordinal comparison can be done here because when both strings start with the same characters
		// then equality is guaranteed regardless of collations or other rules.
		// collations and other rules are really only relevant when both strings are different
		if  !RuntimeState.Exact
			local lengthRHS as int
			lengthRHS := strRHS:Length
			if lengthRHS == 0 .or. lengthRHS <= strLHS:Length  .and. String.Compare( strLHS, 0, strRHS, 0, lengthRHS , StringComparison.Ordinal ) == 0
				RETURN 0
			endif
		endif
		// either exact or RHS longer than LHS
		var mode := RuntimeState.CollationMode 
		SWITCH mode
		case CollationMode.Windows
			ret := StringCompareHelpers.CompareWindows(strLHS, strRHS) 
		case CollationMode.Clipper
			ret := StringCompareHelpers.CompareClipper(strLHS, strRHS)
		case CollationMode.Unicode
			ret := String.Compare(strLHS, strRHS)
		OTHERWISE
			ret := String.CompareOrdinal(strLHS, strRHS)
		end switch
	endif
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
function  __StringEquals(strLHS as string, strRHS as string) as logic
	local IsEqual:= FALSE as logic
	local lengthRHS as int
	IF Object.ReferenceEquals(strLHS, strRHS)
		IsEqual := true
	elseif RuntimeState.Exact
		IsEqual := String.Equals(strLHS , strRHS)
	elseif strLHS != null .and. strRHS != null
		lengthRHS := strRHS:Length
		if lengthRHS == 0
			IsEqual := true        // With SetExact(FALSE) then "aaa" = "" returns TRUE
		ELSEIF lengthRHS <= strLHS:Length
			
			IsEqual := String.Compare( strLHS, 0, strRHS, 0, lengthRHS, StringComparison.Ordinal ) == 0
		endif
	elseif strLHS == null .and. strRHS == null
		IsEqual := true
	endif
	return IsEqual

	/// <summary>
/// Compare 2 strings. This function is used by the compiler for string comparisons
/// </summary>
/// <param name="strLHS">The first string .</param>
/// <param name="strRHS">The second string.</param>
/// <returns>
/// TRUE when the strings are not equal, FALSE when they are equal
/// This function respects SetExact()
/// </returns>
function  __StringNotEquals(strLHS as string, strRHS as string) as logic
	local notEquals := FALSE as logic
	local lengthRHS as int
	IF Object.ReferenceEquals(strLHS, strRHS)
		notEquals := false
	elseif RuntimeState.Exact
		notEquals := !String.Equals(strLHS , strRHS)
	ELSEIF strLHS != NULL .and. strRHS != NULL
		// shortcut: chec first char
		lengthRHS := strRHS:Length
		if lengthRHS == 0
			notEquals := false        // With SetExact(FALSE) then "aaa" = "" returns TRUE
		elseif lengthRHS <= strLHS:Length
			notEquals := String.Compare( strLHS, 0, strRHS, 0, lengthRHS, StringComparison.Ordinal ) != 0
		endif
	elseif strLHS == null .and. strRHS == null
		notEquals := false
	else
		notEquals := true
	endif
	return notEquals

/// <summary>
/// Remove leading and trailing spaces from a string.
/// </summary>
/// <param name="c">The string to be trimmed.</param>
/// <returns>
/// The original string without leading and trailing spaces
/// </returns>
// _FIELD->Name
FUNCTION __FieldGet( fieldName AS STRING ) AS USUAL
	return NIL

// CUSTOMER->NAME
FUNCTION __FieldGetWa( alias AS STRING, fieldName AS STRING ) AS USUAL
	return NIL

// _FIELD->Name := "Foo"
FUNCTION __FieldSet( fieldName AS STRING, uValue AS USUAL ) AS USUAL
	return uValue

// CUSTOMER->Name := "Foo"
FUNCTION __FieldSetWa( alias AS STRING, fieldName AS STRING, uValue AS USUAL ) AS USUAL
	return uValue


// MEMVAR myName
// ? MyName
function __MemVarGet(cName as string) as USUAL
	return NIL

// MEMVAR myName
// MyName := "NewValue"
function __MemVarPut(cName as string, uValue as usual) as usual
	return uValue


// ALIAS->(DoSomething())
// is translated to
// __pushWorkarea( alias ) ; DoSomething() ; __popWorkArea()

FUNCTION __pushWorkarea( alias AS USUAL ) AS VOID
	return 

FUNCTION __popWorkarea() AS VOID
   RETURN
