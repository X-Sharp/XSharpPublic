//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Globalization
// functions used by the compiler
STATIC CLASS XSharp.StringCompareHelpers
	PRIVATE STATIC collationTable AS BYTE[]
	PRIVATE STATIC encDos AS System.Text.Encoding
	PRIVATE STATIC nCP	 AS LONG
	PRIVATE STATIC bLHS AS BYTE[]
	PRIVATE STATIC bRHS AS BYTE[]
	PRIVATE STATIC gate AS OBJECT
	STATIC CONSTRUCTOR
		RuntimeState.OnCodePageChanged += Changed
		RuntimeState.OnCollationChanged += Changed
		getValues()
		bLHS := BYTE[]{512}
		bRHS := BYTE[]{512}
		gate := OBJECT{}
	STATIC METHOD Changed (o AS OBJECT, e AS eventArgs) AS VOID
		getvalues()
		
	STATIC METHOD GetValues() AS VOID
		nCP := runtimestate.WinCodePage
		collationTable := RuntimeState.CollationTable
		encDos   := System.Text.Encoding.GetEncoding(runtimestate.DosCodePage)
		RETURN
		
	STATIC METHOD CompareWindows(strLHS AS STRING, strRHS AS STRING) AS INT
		RETURN Win32.CompareStringAnsi(nCP, Win32.SORT_STRINGSORT,strLHS, strLHS:Length, strRHS, strRHS:Length) -2
		
	STATIC METHOD CompareClipper(strLHS AS STRING, strRHS AS STRING) AS INT
		LOCAL rLen   AS INT
		LOCAL nLen	AS INT
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
					if nL != nR
						nL := collationTable[nL]
						nR := collationTable[nR]
						IF nL < nR
							RETURN -1
						ELSEIF nL > nR
							RETURN 1
						ELSE
							// equal, so continue with the next chars
						ENDIF
					endif
				NEXT
			END UNCHECKED
			// all chars equal so return 0
		END LOCK
		nLen := strLHS:Length
		rLen := strRHS:Length
		return iif(nLen ==rLen, 0, iif(nLen < rLen, -1, 1))
END CLASS


/// <summary>
/// Compare 2 strings. This function is used by the compiler for string comparisons
/// </summary>
/// <param name="strLHS">The first string .</param>
/// <param name="strRHS">The second string.</param>
/// <returns>
/// -1 strA precedes strB in the sort order. 
///  0 strA occurs in the same position as strB in the sort order. 
///  1 strA follows strB in the sort order. 
/// Note this this function should respect SetCollation() and SetInternational() and SetExact()
/// </returns>

