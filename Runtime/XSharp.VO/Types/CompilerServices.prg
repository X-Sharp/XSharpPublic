//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Generic
using System.Runtime.InteropServices

STATIC CLASS XSharp.Internal.CompilerServices

	STATIC METHOD __StringSubtract (lhs AS STRING, rhs AS STRING) AS STRING
		IF lhs != NULL .and. rhs != NULL
			VAR len := lhs:Length + rhs:Length
			RETURN (lhs:TrimEnd() + rhs:TrimEnd()):PadRight(len)
		ELSEIF lhs != NULL
			RETURN lhs
		ELSEIF rhs != NULL
			RETURN rhs
		ENDIF
		RETURN String.Empty


	STATIC METHOD String2Psz(s AS STRING, pszList AS List<IntPtr>) AS IntPtr
		LOCAL pResult AS IntPtr
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

	STATIC METHOD String2PszRelease(pszList AS List<IntPtr>) AS VOID
		FOREACH VAR p in pszList
			TRY
				Marshal.FreeHGlobal(p)
			END TRY
		NEXT
		RETURN 
	//	STATIC METHOD EnterBeginSequence
	//	STATIC METHOD ExitBeginSequence
END CLASS
