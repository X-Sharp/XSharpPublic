//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp


/// <summary>
/// Convert a string to a Symbol.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function String2Atom(c as string) as symbol
	return symbol{c, false}
	
	/// <summary>
	/// Convert a string to an uppercase Symbol.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
function String2Symbol(c as string) as symbol
	return symbol{c, true}
	
	
	/// <summary>
	/// Convert a symbol to string
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
function Symbol2String(s as symbol) as string
	return s:ToString()

	/// <summary>
	/// Concatenate two Symbols.
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <returns>
	/// </returns>
function ConcatAtom(s1 as symbol,s2 as symbol) as symbol
	return Symbol{ s1:ToString()+ s2:ToString()}
	
	/// <summary>
	/// Concatenate three Symbols.
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <param name="s3"></param>
	/// <returns>
	/// </returns>
function ConcatAtom3(s1 as symbol,s2 as symbol,s3 as symbol) as symbol
	return Symbol{ s1:ToString() +s2:ToString() +s3:ToString() }

	/// <summary>
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <param name="s3"></param>
	/// <param name="s4"></param>
	/// <returns>
	/// </returns>
function ConcatAtom4(s1 as symbol,s2 as symbol,s3 as symbol,s4 as symbol) as symbol
	return Symbol{ s1:ToString() +s2:ToString() +s3:ToString() +s4:ToString() }
	
	/// <summary>
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <param name="s3"></param>
	/// <param name="s4"></param>
	/// <param name="s5"></param>
	/// <returns>
	/// </returns>
function ConcatAtom5(s1 as symbol,s2 as symbol,s3 as symbol,s4 as symbol,s5 as symbol) as symbol
	return Symbol{ s1:ToString() +s2:ToString() +s3:ToString() +s4:ToString() +s5:ToString()}
	
	
	/// <summary>
	/// Determine the number of Symbols in the atom table.
	/// </summary>
	/// <returns>
	/// </returns>
function MaxAtom() as dword
	return (DWORD) __Symbol.SymbolTable.Strings:Count
	
	
	
	/// <summary>
	/// </summary>
	/// <param name="s1"></param>
	/// <param name="s2"></param>
	/// <returns>
	/// </returns>
function SysCompAtom(s1 as symbol,s2 as symbol) as int
	return __StringCompare(s1:ToString(), s2:ToString())


	/// <summary>
	/// Convert a null-terminated string to a Symbol and add it to the atom table.
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
function SysAddAtom(s as STRING) as Symbol
	return Symbol { s, FALSE} 


	/// <summary>
	/// Convert a null-terminated string to a Symbol and add it to the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	function SysAddAtom(p as Psz) as Symbol
		return Symbol { Psz2String(p), FALSE} 


	/// <summary>
	/// Convert a null-terminated string to an uppercase Symbol and add it to the atom table.
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
	function SysAddAtomUpperA(s as string) as Symbol
		return Symbol { s, TRUE} 	
	
	/// <summary>
	/// Convert a null-terminated string to an uppercase Symbol and add it to the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	function SysAddAtomUpperA(p as Psz) as Symbol
		return Symbol { Psz2String(p), TRUE} 	

	/// <summary>
	/// Determine whether a Symbol is in the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	function SysFindAtom(s as STRING) as Symbol
		return __Symbol.Find(s)
			
	/// <summary>
	/// Determine whether a Symbol is in the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	function SysFindAtom(p as Psz) as Symbol
		return __Symbol.Find(Psz2String(p))
	
	/// <summary>
	/// Convert a Symbol to a null-terminated string.
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
	function SysGetAtomName(s as Symbol) as Psz
		return s:SysGetAtomName()