//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <summary>
/// Convert a string to a Symbol.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION String2Atom(c AS STRING) AS SYMBOL
	RETURN SYMBOL{c, FALSE}
	
	/// <summary>
	/// Convert a string to an uppercase Symbol.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
FUNCTION String2Symbol(c AS STRING) AS SYMBOL
	RETURN SYMBOL{c, TRUE}
	
	
	/// <summary>
	/// Convert a symbol to string
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
FUNCTION Symbol2String(s AS SYMBOL) AS STRING
	RETURN s:ToString()

	/// <summary>
	/// Concatenate two Symbols.
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <returns>
	/// </returns>
FUNCTION ConcatAtom(s1 AS SYMBOL,s2 AS SYMBOL) AS SYMBOL
	RETURN SYMBOL{ s1:ToString()+ s2:ToString()}
	
	/// <summary>
	/// Concatenate three Symbols.
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <param name="s3"></param>
	/// <returns>
	/// </returns>
FUNCTION ConcatAtom3(s1 AS SYMBOL,s2 AS SYMBOL,s3 AS SYMBOL) AS SYMBOL
	RETURN SYMBOL{ s1:ToString() +s2:ToString() +s3:ToString() }

	/// <summary>
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <param name="s3"></param>
	/// <param name="s4"></param>
	/// <returns>
	/// </returns>
FUNCTION ConcatAtom4(s1 AS SYMBOL,s2 AS SYMBOL,s3 AS SYMBOL,s4 AS SYMBOL) AS SYMBOL
	RETURN SYMBOL{ s1:ToString() +s2:ToString() +s3:ToString() +s4:ToString() }
	
	/// <summary>
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <param name="s3"></param>
	/// <param name="s4"></param>
	/// <param name="s5"></param>
	/// <returns>
	/// </returns>
FUNCTION ConcatAtom5(s1 AS SYMBOL,s2 AS SYMBOL,s3 AS SYMBOL,s4 AS SYMBOL,s5 AS SYMBOL) AS SYMBOL
	RETURN SYMBOL{ s1:ToString() +s2:ToString() +s3:ToString() +s4:ToString() +s5:ToString()}
	
	
	/// <summary>
	/// Determine the number of Symbols in the atom table.
	/// </summary>
	/// <returns>
	/// </returns>
FUNCTION MaxAtom() AS DWORD
	RETURN (DWORD) __Symbol.SymbolTable.Strings:Count
	
	
	
	/// <summary>
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <returns>
	/// </returns>
FUNCTION SysCompAtom(s1 AS SYMBOL,s2 AS SYMBOL) AS INT
	RETURN __StringCompare(s1:ToString(), s2:ToString())


	/// <summary>
	/// Convert a null-terminated string to a Symbol and add it to the atom table.
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
FUNCTION SysAddAtom(s AS STRING) AS SYMBOL
	RETURN SYMBOL { s, FALSE} 


	/// <summary>
	/// Convert a null-terminated string to a Symbol and add it to the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysAddAtom(p AS PSZ) AS SYMBOL
		RETURN SYMBOL { Psz2String(p), FALSE} 


	/// <summary>
	/// Convert a null-terminated string to an uppercase Symbol and add it to the atom table.
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysAddAtomUpperA(s AS STRING) AS SYMBOL
		RETURN SYMBOL { s, TRUE} 	
	
	/// <summary>
	/// Convert a null-terminated string to an uppercase Symbol and add it to the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysAddAtomUpperA(p AS PSZ) AS SYMBOL
		RETURN SYMBOL { Psz2String(p), TRUE} 	

	/// <summary>
	/// Determine whether a Symbol is in the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysFindAtom(s AS STRING) AS SYMBOL
		RETURN __Symbol.Find(s)
			
	/// <summary>
	/// Determine whether a Symbol is in the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysFindAtom(p AS PSZ) AS SYMBOL
		RETURN __Symbol.Find(Psz2String(p))
	
	/// <summary>
	/// Convert a Symbol to a null-terminated string.
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysGetAtomName(s AS SYMBOL) AS PSZ
		RETURN s:SysGetAtomName()