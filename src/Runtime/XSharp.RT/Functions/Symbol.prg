//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


/// <include file="XSharp.RT.Docs.xml" path="doc/String2Atom/*" />
FUNCTION String2Atom(c AS STRING) AS SYMBOL
	RETURN SYMBOL{c, FALSE}

/// <include file="XSharp.RT.Docs.xml" path="doc/String2Symbol/*" />
FUNCTION String2Symbol(c AS STRING) AS SYMBOL
	RETURN SYMBOL{c, TRUE}


/// <include file="XSharp.RT.Docs.xml" path="doc/Symbol2String/*" />
FUNCTION Symbol2String(s AS SYMBOL) AS STRING
	RETURN s:ToString()

/// <include file="XSharp.RT.Docs.xml" path="doc/ConcatAtom/*" />
FUNCTION ConcatAtom(s1 AS SYMBOL,s2 AS SYMBOL) AS SYMBOL
	RETURN SYMBOL{ s1:ToString()+ s2:ToString()}

/// <include file="XSharp.RT.Docs.xml" path="doc/ConcatAtom3/*" />
FUNCTION ConcatAtom3(s1 AS SYMBOL,s2 AS SYMBOL,s3 AS SYMBOL) AS SYMBOL
	RETURN SYMBOL{ s1:ToString() +s2:ToString() +s3:ToString() }

/// <include file="XSharp.RT.Docs.xml" path="doc/ConcatAtom4/*" />
FUNCTION ConcatAtom4(s1 AS SYMBOL,s2 AS SYMBOL,s3 AS SYMBOL,s4 AS SYMBOL) AS SYMBOL
	RETURN SYMBOL{ s1:ToString() +s2:ToString() +s3:ToString() +s4:ToString() }

/// <include file="XSharp.RT.Docs.xml" path="doc/ConcatAtom5/*" />
FUNCTION ConcatAtom5(s1 AS SYMBOL,s2 AS SYMBOL,s3 AS SYMBOL,s4 AS SYMBOL,s5 AS SYMBOL) AS SYMBOL
	RETURN SYMBOL{ s1:ToString() +s2:ToString() +s3:ToString() +s4:ToString() +s5:ToString()}


/// <include file="XSharp.RT.Docs.xml" path="doc/MaxAtom/*" />
FUNCTION MaxAtom() AS DWORD
	RETURN (DWORD) __Symbol.SymbolTable.Count



/// <include file="XSharp.RT.Docs.xml" path="doc/SysCompAtom/*" />
FUNCTION SysCompAtom(s1 AS SYMBOL,s2 AS SYMBOL) AS INT
	RETURN __StringCompare(s1:ToString(), s2:ToString())


/// <include file="XSharp.RT.Docs.xml" path="doc/SysAddAtom/*" />
FUNCTION SysAddAtom(s AS STRING) AS SYMBOL
	RETURN SYMBOL { s, FALSE}


 /// <include file="XSharp.RT.Docs.xml" path="doc/SysAddAtom/*" />
	FUNCTION SysAddAtom(p AS PSZ) AS SYMBOL
		RETURN SYMBOL { Psz2String(p), FALSE}


 /// <include file="XSharp.RT.Docs.xml" path="doc/SysAddAtomUpperA/*" />
	FUNCTION SysAddAtomUpperA(s AS STRING) AS SYMBOL
		RETURN SYMBOL { s, TRUE}

 /// <include file="XSharp.RT.Docs.xml" path="doc/SysAddAtomUpperA/*" />
	FUNCTION SysAddAtomUpperA(p AS PSZ) AS SYMBOL
		RETURN SYMBOL { Psz2String(p), TRUE}

 /// <include file="XSharp.RT.Docs.xml" path="doc/SysFindAtom/*" />
	FUNCTION SysFindAtom(s AS STRING) AS SYMBOL
		RETURN __Symbol.Find(s)

 /// <include file="XSharp.RT.Docs.xml" path="doc/SysFindAtom/*" />
	FUNCTION SysFindAtom(p AS PSZ) AS SYMBOL
		RETURN __Symbol.Find(Psz2String(p))

 /// <include file="XSharp.RT.Docs.xml" path="doc/SysGetAtomName/*" />
	FUNCTION SysGetAtomName(s AS SYMBOL) AS PSZ
		RETURN s:SysGetAtomName()
