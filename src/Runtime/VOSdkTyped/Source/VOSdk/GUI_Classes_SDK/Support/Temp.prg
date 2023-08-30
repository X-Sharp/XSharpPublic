//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System.Windows.Forms
/// <exclude />
FUNCTION __DBCSEnabled() AS LOGIC
	//RETURN LOGIC(_CAST, GetSystemMetrics(SM_DBCSENABLED))
	RETURN FALSE

/// <exclude />
FUNCTION __ShowLastError()
	// Todo:
	//LOCAL dwErr AS DWORD
	//LOCAL lpMsgBuf AS PSZ

	//dwErr := GetLastError()

	//FormatMessage(_OR(FORMAT_MESSAGE_ALLOCATE_BUFFER, FORMAT_MESSAGE_FROM_SYSTEM), ;
	//	NULL_PTR, dwErr, 0, @lpMsgBuf, 0, NULL_PTR)

	//Windows.Forms.MessageBox.Show("Error #: "+NTrim(dwErr)+", Text: "+AsString(lpMsgBuf), "GetLastError",MessageBoxButtons.OK, MessageBoxIcon.Information)

	//LocalFree(lpMsgBuf)
	RETURN NIL

//_DLL FUNC DragObject(hDesktop AS PTR, hWnd AS PTR, uFlags AS DWORD, dw AS DWORD, hCursor AS PTR);
//	AS DWORD PASCAL:USER32.DragObject

// FUNCTION MyMakeLong(wLow AS WORD, wHigh AS WORD) AS LONG
// 	RETURN LONG(_CAST, _or((DWORD(wHigh) << 16), DWORD(wLow)))




//FUNCTION IsShiftPressed()
//	LOCAL keys AS System.Windows.Forms.Keys
//	keys := System.Windows.Forms.Control.ModifierKeys
//	RETURN (keys == System.Windows.Forms.Keys.Shift)

//FUNCTION IsControlPressed()
//	LOCAL keys AS System.Windows.Forms.Keys
//	keys := System.Windows.Forms.Control.ModifierKeys
//	RETURN (keys == System.Windows.Forms.Keys.Control)

//FUNCTION IsAltPressed()
//	LOCAL keys AS System.Windows.Forms.Keys
//	keys := System.Windows.Forms.Control.ModifierKeys
//	RETURN (keys == System.Windows.Forms.Keys.Alt)




#ifdef DOCUMENTATION
FUNCTION DebOut(uParam params USUAL[]) AS VOID
RETURN
FUNCTION CreateInstance( args PARAMS USUAL[]) AS USUAL
RETURN NIL

#endif
