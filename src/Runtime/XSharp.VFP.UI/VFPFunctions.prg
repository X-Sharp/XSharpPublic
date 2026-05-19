// VFPFunctions.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.



USING System
USING System.Collections.Generic
USING System.Text
USING System.Runtime.InteropServices

INTERNAL _DLL FUNCTION SetForegroundWindow(hwnd AS PTR) AS LOGIC PASCAL:USER32.SetForegroundWindow
INTERNAL _DLL FUNCTION FindWindow(lpClassName AS STRING , lpWindowName AS STRING ) AS PTR PASCAL:USER32.FindWindowA
INTERNAL _DLL FUNCTION SetActiveWindow( hwnd AS PTR) AS PTR PASCAL:USER32.SetActiveWindow
INTERNAL _DLL FUNCTION IsWindowVisible(hwnd AS PTR) AS LOGIC PASCAL:USER32.IsWindowVisible
INTERNAL _DLL FUNCTION ShowWindow(hwnd AS PTR, nCmdShow AS INT) AS LOGIC PASCAL:USER32.ShowWindow
DEFINE SW_HIDE                       := 0
DEFINE SW_NORMAL                     := 1


// FUNCTION CreateObject(symClassName,InitParamsList) AS OBJECT CLIPPER
// 	//
// 	VAR oRet := CreateInstance( symClassName,InitParamsList )
// 	RETURN oRet

/// <summary>
/// VFP <c>CREATE()</c> alias — creates an instance of <paramref name="symClassName"/> via <c>CreateObject</c> with optional constructor parameters.
/// </summary>
FUNCTION Create(symClassName,InitParamsList) AS OBJECT CLIPPER
	RETURN CreateObject(symClassName,InitParamsList)


/// <summary>
/// Brings a top-level window to the foreground by title. Uses <c>FindWindow</c> + <c>SetForegroundWindow</c>; silently ignored if no window matches.
/// </summary>
FUNCTION VFPActivateForm( windowName ) AS VOID CLIPPER
	//Find the window,
	VAR hWnd := FindWindow(NULL, (STRING)windowName )
	IF (hWnd != NULL ) //If found
		SetForegroundWindow(hWnd) //Activate it
		// SetActiveWindow( hWnd )
	ENDIF
	RETURN

/// <summary>
/// Hides a top-level window by title using <c>SW_HIDE</c>. Silently ignored if no window matches.
/// </summary>
FUNCTION VFPHideForm( windowName ) AS VOID CLIPPER
	//Find the window,
	VAR hWnd := FindWindow(NULL, (STRING)windowName )
	IF (hWnd != NULL ) //If found
		ShowWindow(hWnd, SW_HIDE)
	ENDIF
	RETURN


/// <summary>
/// Returns <c>.T.</c> if a top-level window with the given title exists and is currently visible. VFP <c>WVISIBLE()</c> equivalent.
/// </summary>
FUNCTION WVISIBLE( windowName AS STRING ) AS LOGIC
	VAR hwnd := FindWindow(null, (STRING)windowName)
	IF ( hwnd != NULL)
		RETURN IsWindowVisible( hwnd )
	ENDIF
	RETURN FALSE

/// <summary>
/// Returns <c>.T.</c> if a top-level window with the given title exists (visible or not). VFP <c>WEXIST()</c> equivalent.
/// </summary>
FUNCTION WEXIST( windowName AS STRING ) AS LOGIC
	VAR hwnd := FindWindow(null, (STRING)windowName)
	RETURN ( hwnd != NULL )

// FUNCTION MessageBox(eMessageText As USUAL, cTitleBarText := "" As STRING, nTimeOut := 0 As INT) As INT
//     RETURN MessageBox(eMessageText, 0, cTitleBarText, nTimeOut)

