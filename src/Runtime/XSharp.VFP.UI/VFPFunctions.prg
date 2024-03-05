// VFPFunctions.prg
// Created by    : fabri
// Creation Date : 1/5/2022 1:55:48 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING System.Runtime.InteropServices

_DLL FUNCTION SetForegroundWindow(hwnd AS PTR) AS LOGIC PASCAL:USER32.SetForegroundWindow
_DLL FUNCTION FindWindow(lpClassName AS STRING , lpWindowName AS STRING ) AS PTR PASCAL:USER32.FindWindowA
_DLL FUNCTION SetActiveWindow( hwnd AS PTR) AS PTR PASCAL:USER32.SetActiveWindow
_DLL FUNCTION IsWindowVisible(hwnd AS PTR) AS LOGIC PASCAL:USER32.IsWindowVisible
_DLL FUNCTION ShowWindow(hwnd AS PTR, nCmdShow AS INT) AS LOGIC PASCAL:USER32.ShowWindow
DEFINE SW_HIDE                       := 0
DEFINE SW_NORMAL                     := 1


FUNCTION CreateObject(symClassName,InitParamsList) AS OBJECT CLIPPER
	//
	VAR oRet := CreateInstance( symClassName,InitParamsList )
	RETURN oRet

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

FUNCTION Sys(nSetting, uNewValue) As USUAL CLIPPER
	LOCAL retVal AS OBJECT
	retVal := FALSE
	//
	SWITCH nSetting
	CASE 5
		retVal := GetDefaultDir()
	CASE 16
		var asm := System.Reflection.Assembly.GetExecutingAssembly()
		var path := asm:Location
		retVal := path
	CASE 987
		RETURN FALSE
	CASE 2003
		retVal := Environment.CurrentDirectory

	END SWITCH
	RETURN retVal


// FUNCTION MessageBox(eMessageText As USUAL, cTitleBarText := "" As STRING, nTimeOut := 0 As INT) As INT
//     RETURN MessageBox(eMessageText, 0, cTitleBarText, nTimeOut)

