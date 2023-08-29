//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

CLASS OleDragEvent INHERIT VObject
	//RvdH 030825 This code has been moved from the Ole Classes
	PROTECT hWnd AS IntPtr
	PROTECT dwDragEvent AS DWORD
	PROTECT pDataObject AS IntPtr
	PROTECT dwEffect AS DWORD
	PROTECT cObjectName AS STRING
	PROTECT cServerName AS STRING
	PROTECT oPoint AS Point

	ACCESS DataObject 
		RETURN pDataObject

	ACCESS Effect 
		RETURN LONGINT(_CAST, dwEffect)

	ASSIGN Effect(dwNewValue) 
		dwEffect := dwNewValue
		RETURN 

	CONSTRUCTOR(DragInfo) 
		LOCAL DI AS OleDragEventInfo
		SUPER()
		DI := DragInfo

		hWnd := DI:hDocWnd
		pDataObject := DI:pDataObject
		cObjectName := Psz2String(DI:pszObjectName)
		cServerName := Psz2String(DI:pszServerName)
		dwEffect := DI:dwEffect
		oPoint := Point{DI:dwMouseX, DI:dwMouseY}
		RETURN 

	ACCESS ObjectName 
		RETURN cObjectName

	ACCESS Position as Point
		//Todo
		//LOCAL oWnd AS OBJECT

		//oWnd := __WCGetWindowByHandle(hWnd)

		//IF IsInstanceOf(oWnd, #Window)
		//     RETURN __WCConvertPoint(oWnd, oPoint)
		//ENDIF
		RETURN NULL_OBJECT

	ACCESS ServerName 
		RETURN cServerName

END CLASS

VOSTRUCT OleDragEventInfo
	MEMBER hDocWnd AS IntPtr
	MEMBER dwDragEvent AS DWORD
	MEMBER pDataObject AS IntPtr
	MEMBER dwEffect AS DWORD
	MEMBER pszObjectName AS PSZ
	MEMBER pszServerName AS PSZ
	MEMBER dwMouseX AS LONGINT
MEMBER dwMouseY AS LONGINT
