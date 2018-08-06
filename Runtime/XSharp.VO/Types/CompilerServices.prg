//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Runtime.InteropServices

STATIC CLASS XSharp.Internal.CompilerServices
	
	STATIC METHOD __StringSubtract (lhs AS STRING, rhs AS STRING) AS STRING
		IF lhs != null .and. rhs != null
			VAR len := lhs:Length + rhs:Length
			RETURN (lhs:TrimEnd() + rhs:TrimEnd()):PadRight(len)
		ELSEIF lhs != null
			RETURN lhs
		ELSEIF rhs != null
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
			VAR len      := bytes:Length
			pResult := Marshal.AllocHGlobal(len+1)
			Marshal.Copy(bytes, 0, pResult, len)
			Marshal.WriteByte(pResult, len, 0)	 // end of string
		ENDIF
		pszList:Add(pResult)
		RETURN pResult
	
	STATIC METHOD String2PszRelease(pszList AS List<IntPtr>) AS VOID
		FOREACH VAR p IN pszList
			TRY
				Marshal.FreeHGlobal(p)
			END TRY
		NEXT
		RETURN 
	STATIC METHOD EnterBeginSequence AS VOID
		RuntimeState.GetInstance():BreakLevel+= 1
	STATIC METHOD ExitBeginSequence	 AS VOID
		RuntimeState.GetInstance():BreakLevel-= 1
	STATIC METHOD CanBreak AS LOGIC
		RETURN RuntimeState.GetInstance():BreakLevel > 0

END CLASS
