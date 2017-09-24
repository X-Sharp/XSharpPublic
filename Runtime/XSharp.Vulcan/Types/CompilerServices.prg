//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Generic
using System.Runtime.InteropServices

STATIC CLASS XSharp.Internal.CompilerServices

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
