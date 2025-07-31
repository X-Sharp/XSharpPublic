// WindowFunctions.prg



USING System
USING System.Collections.Generic
USING System.Text

INTERNAL _DLL FUNCTION FindWindow(lpClassName AS STRING , lpWindowName AS STRING ) AS PTR PASCAL:USER32.FindWindowA
INTERNAL _DLL FUNCTION IsWindowVisible(hwnd AS PTR) AS LOGIC PASCAL:USER32.IsWindowVisible
DEFINE SW_HIDE                       := 0
DEFINE SW_NORMAL                     := 1


/// <include file="VFPDocs.xml" path="Runtimefunctions/wvisible/*" />
FUNCTION WVISIBLE( windowName AS STRING ) AS LOGIC
	VAR hwnd := FindWindow(null, (STRING)windowName)
	IF ( hwnd != NULL)
		RETURN IsWindowVisible( hwnd )
	ENDIF
	RETURN FALSE

/// <include file="VFPDocs.xml" path="Runtimefunctions/wexist/*" />
FUNCTION WEXIST( windowName AS STRING ) AS LOGIC
	VAR hwnd := FindWindow(null, (STRING)windowName)
	RETURN ( hwnd != NULL )
