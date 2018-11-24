//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Runtime.InteropServices
/// <summary>
/// This class contains helper code that is called by the compiler to support various XBase language constructs, such as the
/// automatic memory management of PSZ values created with String2Psz().
/// </summary>
STATIC CLASS XSharp.Internal.CompilerServices
	///<summary>
    /// Subtract 2 strings.
    ///</summary>
	STATIC METHOD __StringSubtract (lhs AS STRING, rhs AS STRING) AS STRING
		IF lhs != NULL .AND. rhs != NULL
			VAR len := lhs:Length + rhs:Length
			RETURN (lhs:TrimEnd() + rhs:TrimEnd()):PadRight(len)
		ELSEIF lhs != NULL
			RETURN lhs
		ELSEIF rhs != NULL
			RETURN rhs
		ENDIF
		RETURN String.Empty
	
	///<summary>
    /// Allocate a PSZ and add it to the list
    ///</summary>
	STATIC METHOD String2Psz(s AS STRING, pszList AS List<IntPtr>) AS IntPtr
		LOCAL pResult AS IntPtr
		IF s == NULL || s:Length == 0
			pResult := MemAlloc(1)
            Marshal.WriteByte(pResult, 0, 0)	 // end of string
        ELSE
            pResult := String2Mem(s)
		ENDIF
		pszList:Add(pResult)
		RETURN pResult
	
	///<summary>
    /// Free all PSZ values in the List
    ///</summary>
	STATIC METHOD String2PszRelease(pszList AS List<IntPtr>) AS VOID
		FOREACH VAR p IN pszList
			TRY
				MemFree(p)
			END TRY
		NEXT
		RETURN
    
	///<summary>
    /// Increment the SEQUENCE counter for a BEGIN SEQUENCE statement
    ///</summary>
    STATIC METHOD EnterBeginSequence AS VOID
		RuntimeState.GetInstance():BreakLevel+= 1

	///<summary>
    /// Decrement the SEQUENCE counter for a BEGIN SEQUENCE statement
    ///</summary>
    STATIC METHOD ExitBeginSequence	 AS VOID
		RuntimeState.GetInstance():BreakLevel-= 1

    ///<summary>
    /// Determine if we are inside a BEGIN SEQUENCE .. END by looking at the SEQUENCE counter in the runtime.
    ///</summary>
	STATIC METHOD CanBreak AS LOGIC
		RETURN RuntimeState.GetInstance():BreakLevel > 0

END CLASS
