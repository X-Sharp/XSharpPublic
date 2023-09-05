//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

INTERNAL VOSTRUCT __WCDropFiles
	MEMBER wSize AS DWORD				// Size of data structure
	MEMBER ptMousePos IS _winPOINT		//Position of mouse in the
	
	MEMBER fInNonClientArea AS LOGIC	//window's non-client area
	MEMBER fUnicode AS LOGIC			// are the pathnames in Unicaode?


INTERNAL VOSTRUCT WCColor
	MEMBER bBlue 	AS BYTE
	MEMBER bGreen 	AS BYTE
	MEMBER bRed 	AS BYTE
	MEMBER bNotUsed AS BYTE

INTERNAL VOSTRUCT _winPOINT
	MEMBER x AS LONGINT
	MEMBER y AS LONGINT
