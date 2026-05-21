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

/// <summary>
/// Converts a VFP caption string to WinForms format:<br/>
/// <c>\&lt;X</c> → <c>&amp;X</c> (access key), <c>\\</c> → <c>\</c> (literal backslash), <c>\!</c> → empty (suppress accelerator).
/// </summary>
FUNCTION __VFPConvertCaption(s AS STRING) AS STRING
    IF String.IsNullOrEmpty(s) .OR. s:IndexOf('\') < 0
        RETURN s
    ENDIF
    VAR sb := System.Text.StringBuilder{}
    VAR i  := 0
    DO WHILE i < s:Length
        IF s[i] == c'\\' .AND. i + 1 < s:Length
            SWITCH s[i+1]
            CASE c'<'
                sb:Append('&')
                i += 2
            CASE c'\\'
                sb:Append('\')
                i += 2
            CASE c'!'
                i += 2    // \! disables accelerator display — skip both chars
            OTHERWISE
                sb:Append(s[i])
                i++
            END SWITCH
        ELSE
            sb:Append(s[i])
            i++
        ENDIF
    ENDDO
    RETURN sb:ToString()

/// <summary>
/// Case-insensitive <c>CreateInstance</c> that first searches <c>XSharp.VFP.UI</c> by namespace-qualified name,
/// then by short name. Allows VFP code like <c>CREATEOBJECT("textbox")</c> to resolve to <c>XSharp.VFP.UI.TextBox</c>.
/// </summary>
FUNCTION __VFPCreateInstance(cClass AS STRING) AS OBJECT
    LOCAL oAssembly AS System.Reflection.Assembly
    oAssembly := System.Reflection.Assembly.GetExecutingAssembly()
    VAR fullName := "XSharp.VFP.UI." + cClass
    FOREACH VAR t IN oAssembly:GetExportedTypes()
        IF String.Equals(t:FullName, fullName, StringComparison.OrdinalIgnoreCase)
            RETURN System.Activator.CreateInstance(t)
        ENDIF
    NEXT
    FOREACH VAR t IN oAssembly:GetExportedTypes()
        IF String.Equals(t:Name, cClass, StringComparison.OrdinalIgnoreCase)
            RETURN System.Activator.CreateInstance(t)
        ENDIF
    NEXT
    RETURN CreateInstance(cClass)

/// <summary>
/// Implements VFP <c>READ EVENTS</c>: starts a WinForms message loop only when none is already running.
/// Inside a VFPXPorter-generated app the main loop (<c>Application.Run</c> in VFPStart.prg) is already
/// active, so this is a no-op in that context.
/// </summary>
FUNCTION __VFPReadEvents() AS VOID
    IF !System.Windows.Forms.Application.MessageLoop
        System.Windows.Forms.Application.Run()
    ENDIF

/// <summary>
/// Implements VFP <c>CLEAR EVENTS</c>: exits the current thread's WinForms message loop when one is running.
/// </summary>
FUNCTION __VFPClearEvents() AS VOID
    IF System.Windows.Forms.Application.MessageLoop
        System.Windows.Forms.Application.ExitThread()
    ENDIF

