//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// ResourceReader.prg
// Helper class used to read native dialogs ( & menus ?)
//
USING System.Text
CLASS ResourceReader
	CONSTRUCTOR()
		SUPER()
	STATIC METHOD ReadIdAndText(pWORD AS WORD PTR, nID REF WORD, sID REF STRING) AS WORD PTR
		IF pWORD[1] == 0			// No Data
			nID		:= 0
			pWORD	+= 1
		ELSEIF pWORD[1] == 0XFFFF	// Predefined ID
			nID := pWORD[2]
			pWORD+=2
		ELSE						// Unicode String
			pWORD += 1
			sID   := ReadText(pWORD)
			pWORD += sID:Length+1
		ENDIF							
		RETURN pWORD
	
	STATIC METHOD ReadText(pWORD AS WORD PTR) AS STRING
		LOCAL SB AS StringBuilder
		SB := StringBuilder{}
		DO WHILE pWORD[1] != 0
			SB:Append((Char) pWORD[1])
			pWORD++
		ENDDO
		RETURN SB:ToString()
	

END CLASS
