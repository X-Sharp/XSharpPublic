﻿// VFPFunctions.prg
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

FUNCTION Create(symClassName,InitParamsList) AS OBJECT CLIPPER
	RETURN CreateObject(symClassName,InitParamsList)


FUNCTION VFPActivateForm( windowName ) AS VOID CLIPPER
	//Find the window,
	VAR hWnd := FindWindow(NULL, (STRING)windowName )
	IF (hWnd != NULL ) //If found
		SetForegroundWindow(hWnd) //Activate it
		// SetActiveWindow( hWnd )
	ENDIF
	RETURN

FUNCTION VFPHideForm( windowName ) AS VOID CLIPPER
	//Find the window,
	VAR hWnd := FindWindow(NULL, (STRING)windowName )
	IF (hWnd != NULL ) //If found
		ShowWindow(hWnd, SW_HIDE)
	ENDIF
	RETURN


FUNCTION WVISIBLE( windowName AS STRING ) AS LOGIC
	VAR hwnd := FindWindow(null, (STRING)windowName)
	IF ( hwnd != NULL)
		RETURN IsWindowVisible( hwnd )
	ENDIF
	RETURN FALSE

FUNCTION WEXIST( windowName AS STRING ) AS LOGIC
	VAR hwnd := FindWindow(null, (STRING)windowName)
	RETURN ( hwnd != NULL )

// FUNCTION MessageBox(eMessageText As USUAL, cTitleBarText := "" As STRING, nTimeOut := 0 As INT) As INT
//     RETURN MessageBox(eMessageText, 0, cTitleBarText, nTimeOut)

