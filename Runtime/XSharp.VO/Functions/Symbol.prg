//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp
/// <summary>
/// </summary>
/// <param name="dwVS"></param>
/// <param name="dwStep"></param>
/// <returns>
/// </returns>
FUNCTION CreateAtomTable(dwVS AS DWORD,dwStep AS DWORD) AS DWORD
	/// THROW NotImplementedException{}
RETURN 0   



/// <summary>
/// </summary>
/// <param name="hf"></param>
/// <returns>
/// </returns>
FUNCTION ReadAtomTable(hf AS DWORD) AS DWORD
	/// THROW NotImplementedException{}
RETURN 0   


/// <summary>
/// Convert a string to a __Symbol.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION String2Atom(c AS STRING) AS __Symbol
	/// THROW NotImplementedException{}
RETURN NULL_SYMBOL  

/// <summary>
/// Convert a string to an uppercase __Symbol.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION String2Symbol(c AS STRING) AS __Symbol
	/// THROW NotImplementedException{}
return null_symbol   

	/// <summary>
	/// Concatenate two __Symbols.
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <returns>
	/// </returns>
	FUNCTION ConcatAtom(s1 AS __Symbol,s2 AS __Symbol) AS __Symbol
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// Concatenate three __Symbols.
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <param name="s3"></param>
	/// <returns>
	/// </returns>
	FUNCTION ConcatAtom3(s1 AS __Symbol,s2 AS __Symbol,s3 AS __Symbol) AS __Symbol
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <param name="s3"></param>
	/// <param name="s4"></param>
	/// <param name="s5"></param>
	/// <returns>
	/// </returns>
	FUNCTION ConcatAtom5(s1 AS __Symbol,s2 AS __Symbol,s3 AS __Symbol,s4 AS __Symbol,s5 AS __Symbol) AS __Symbol
		/// THROW NotImplementedException{}
	return null_symbol   


	/// <summary>
	/// Determine the number of __Symbols in the atom table.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION MaxAtom() AS DWORD
		/// THROW NotImplementedException{}
	return 0   


	/// <summary>
	/// Convert a __Symbol to a string.
	/// </summary>
	/// <param name="sym"></param>
	/// <returns>
	/// </returns>
	FUNCTION __Symbol2String(sym AS __Symbol) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysCompAtom(s1 AS __Symbol,s2 AS __Symbol) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   
