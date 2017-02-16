//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Generic
using System.Runtime.InteropServices

UNSAFE STRUCTURE __Psz
	public static _NULL_PSZ := __Psz{""} as __Psz
	PRIVATE _value as Byte PTR
	OPERATOR IMPLICIT( s as STRING) AS __Psz
		RETURN __Psz{}
	OPERATOR IMPLICIT( p as IntPtr) AS __Psz
		RETURN __Psz{}
	OPERATOR IMPLICIT( p as __Psz) AS IntPtr
		RETURN IntPtr.Zero
	CONSTRUCTOR(s as STRING)
		_value := NULL
		RETURN
	PROPERTY __Value as Byte Ptr GET _value
END STRUCTURE



STATIC CLASS XSharp.Internal.CompilerServices
	// Moved to the Core runtime
	// Function __StringSubtract
	// Static Method StringSubtract()

	STATIC METHOD String2Psz(s as STRING, pszList as List<IntPtr>) AS IntPtr
		LOCAL pResult as IntPtr
		IF s == null || s:Length == 0
			pResult := Marshal.AllocHGlobal(1)
		ELSE
			VAR encoding := System.Text.Encoding.Default
			VAR bytes    := encoding:GetBytes(s)
			VAR len      := bytes.Length
			pResult := Marshal.AllocHGlobal(len)
			Marshal.Copy(bytes, 0, pResult, len)
		ENDIF
		pszList:Add(pResult)
		RETURN pResult

	STATIC METHOD String2PszRelease(pszList as List<IntPtr>) AS VOID
		FOREACH VAR p in pszList
			TRY
				Marshal.FreeHGlobal(p)
			END TRY
		NEXT
		RETURN 
	//	STATIC METHOD EnterBeginSequence
	//	STATIC METHOD ExitBeginSequence
END CLASS