//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Globalization
// StringComparer class that takes care of Windows and Clipper string comparisons
/// <exclude />
STATIC CLASS XSharp.StringCompareHelpers
	PRIVATE STATIC collationTable AS BYTE[]
	PRIVATE STATIC encDos	AS System.Text.Encoding
    PRIVATE STATIC encWin	AS System.Text.Encoding
	PRIVATE STATIC nCPWin	AS LONG
	PRIVATE STATIC bLHS		AS BYTE[]       // cache byte array to avoid having to allocate bytes for every comparison
	PRIVATE STATIC bRHS		AS BYTE[]
	PRIVATE STATIC gate		AS OBJECT
    /// <exclude />
	STATIC CONSTRUCTOR
		// Register event Handlers, so we can reread tye DOS and Windows codepages
		// and collation table when the user changes these
		RuntimeState.OnCodePageChanged += Changed
		RuntimeState.OnCollationChanged += Changed
		getValues()
		bLHS := BYTE[]{512}
		bRHS := BYTE[]{512}
		gate := OBJECT{}

    /// <exclude />
	STATIC METHOD Changed (o AS OBJECT, e AS eventArgs) AS VOID
		getvalues()

    /// <exclude />
	STATIC METHOD GetValues() AS VOID
		nCPWin			:= runtimestate.WinCodePage
		collationTable	:= RuntimeState.CollationTable
		encDos			:= System.Text.Encoding.GetEncoding(runtimestate.DosCodePage)
        encWin			:= System.Text.Encoding.GetEncoding(nCPWin)
		RETURN
		
    /// <exclude />
	STATIC METHOD CompareWindows(strLHS AS STRING, strRHS AS STRING) AS INT
		LOCAL nLen	AS INT
        LOCAL result AS INT
		nLen := Math.Min(strLHS:Length, strRHS:Length)
        BEGIN LOCK gate
			IF nLen > bLHS:Length
				bLHS := BYTE[]{nLen}
				bRHS := BYTE[]{nLen}
			ENDIF
			encWin:GetBytes(strLHS, 0, nLen, bLHS, 0)
			encWin:GetBytes(strRHS, 0, nLen, bRHS, 0)
            result := Win32.CompareStringAnsi(nCPWin, Win32.SORT_STRINGSORT,bLHS, strLHS:Length, bRHS, strRHS:Length) -2
        END LOCK
        RETURN result
        
		
		
    /// <exclude />
    STATIC METHOD CompareClipper(strLHS AS STRING, strRHS AS STRING) AS INT
		LOCAL rLen   AS INT
		LOCAL nLen	AS INT
		// when we get here then reference equality is not TRUE. THat has been checked
		// before this method is called.
		nLen := Math.Min(strLHS:Length, strRHS:Length)
		BEGIN LOCK gate
			IF nLen > bLHS:Length
				bLHS := BYTE[]{nLen}
				bRHS := BYTE[]{nLen}
			ENDIF
			encDos:GetBytes(strLHS, 0, nLen, bLHS, 0)
			encDos:GetBytes(strRHS, 0, nLen, bRHS, 0)
			LOCAL nPos AS LONG
			BEGIN UNCHECKED
				FOR nPos := 0 TO nLen -1
					VAR nL := bLHS[nPos]
					VAR nR := bRHS[nPos]
					// no need to lookup the same character. The weight table will
					// have the same value for both
					IF nL != nR
						nL := collationTable[nL]
						nR := collationTable[nR]
						IF nL < nR
							RETURN -1
						ELSEIF nL > nR
							RETURN 1
						ELSE
							// equal, so continue with the next chars
							// this normally only happens when 2 characters are mapped to the same weight
							// that could for example happen when ü and u have the same weight
							// I am not sure if this ever happens. If would creating an index unreliable
							// most likely the ü will be sorted between u and v. 
						ENDIF
					ENDIF
				NEXT
			END UNCHECKED
		END LOCK
		// all bytes that we compared are equal so return 0 when the strings have the same length
		// otherwise the shorter string is smaller than the longer string
		nLen := strLHS:Length
		rLen := strRHS:Length
		RETURN IIF(nLen ==rLen, 0, IIF(nLen < rLen, -1, 1))
END CLASS


