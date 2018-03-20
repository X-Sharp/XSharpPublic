//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Generic
using System.Runtime.InteropServices

static class XSharp.Internal.CompilerServices
	
	static method __StringSubtract (lhs as string, rhs as string) as string
		if lhs != null .and. rhs != null
			var len := lhs:Length + rhs:Length
			return (lhs:TrimEnd() + rhs:TrimEnd()):PadRight(len)
		elseif lhs != null
			return lhs
		elseif rhs != null
			return rhs
		endif
		return String.Empty
	
	
	static method String2Psz(s as string, pszList as List<IntPtr>) as IntPtr
		local pResult as IntPtr
		if s == null || s:Length == 0
			pResult := Marshal.AllocHGlobal(1)
		else
			var encoding := System.Text.Encoding.Default
			var bytes    := encoding:GetBytes(s)
			var len      := bytes:Length
			pResult := Marshal.AllocHGlobal(len)
			Marshal.Copy(bytes, 0, pResult, len)
		endif
		pszList:Add(pResult)
		return pResult
	
	static method String2PszRelease(pszList as List<IntPtr>) as void
		foreach var p in pszList
			try
				Marshal.FreeHGlobal(p)
			end try
		next
		return 
	static method EnterBeginSequence as void
		throw NotImplementedException{}
	static method ExitBeginSequence	 as void
		throw NotImplementedException{}
	
end class
