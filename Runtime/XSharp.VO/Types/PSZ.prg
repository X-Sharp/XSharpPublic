//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Generic
using System.Runtime.InteropServices
BEGIN NAMESPACE XSharp
//Todo Implement PSZ and prevent memory leaks!
UNSAFE STRUCTURE __Psz
	public static _NULL_PSZ := __Psz{""} as __Psz
	PRIVATE _value as Byte PTR
	OPERATOR IMPLICIT( s as STRING) AS __Psz
		RETURN __Psz{s}
	OPERATOR IMPLICIT( p as IntPtr) AS __Psz
		RETURN __Psz{}
	OPERATOR IMPLICIT( p as __Psz) AS IntPtr
		RETURN IntPtr.Zero
	CONSTRUCTOR(s as STRING)
		_value := NULL
		RETURN
	PROPERTY __Value as Byte Ptr GET _value
END STRUCTURE
END NAMESPACE




/// <summary>
/// </summary>
/// <param name="pszSource"></param>
/// <returns>
/// </returns>
FUNCTION DbcsNext(pszSource AS __Psz) AS __Psz
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero

/// <summary>
/// </summary>
/// <param name="pszSource"></param>
/// <param name="pszCurr"></param>
/// <returns>
/// </returns>
FUNCTION DbcsPrev(pszSource AS __Psz,pszCurr AS __Psz) AS __Psz
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero