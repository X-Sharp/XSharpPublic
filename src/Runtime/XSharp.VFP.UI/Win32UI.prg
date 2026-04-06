//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.VFP.UI

    INTERNAL STATIC CLASS VfpWin32UI
        PUBLIC CONST DESKTOP_HORZRES := 117 AS INT
        PUBLIC CONST DESKTOP_VERTRES := 118 AS INT

        [DllImport("user32.dll", CharSet := CharSet.Unicode, EntryPoint := "MessageBoxTimeoutW")] ;
        INTERNAL STATIC EXTERN METHOD MessageBoxTimeout( ;
            hWnd AS IntPtr, ;
            lpText AS STRING, ;
            lpCaption AS STRING, ;
            uType AS DWORD, ;
            wLanguageId AS WORD, ;
            dwMilliseconds AS DWORD) AS LONG

        [DllImport("gdi32.dll", CharSet := CharSet.Auto, SetLastError := TRUE, ExactSpelling := TRUE)];
        STATIC EXTERN METHOD GetDeviceCaps(hDC AS IntPtr, nIndex AS INT) AS INT

        [DllImport("user32.dll", CharSet := CharSet.Auto, SetLastError := TRUE, ExactSpelling := TRUE)];
        STATIC EXTERN METHOD GetDC(hWnd AS IntPtr) AS IntPtr

        [DllImport("user32.dll", CharSet := CharSet.Auto, SetLastError := TRUE, ExactSpelling := TRUE)];
        STATIC EXTERN METHOD ReleaseDC(hWnd AS IntPtr, hDC AS IntPtr) AS INT

        [DllImport("user32.dll", CharSet := CharSet.Auto, SetLastError := TRUE)] ;
		INTERNAL STATIC EXTERN METHOD FindWindow(lpClassName AS STRING , lpWindowName AS STRING ) AS IntPtr

        [DllImport("user32.dll")];
		[RETURN:MarshalAs(UnmanagedType.Bool)];
		INTERNAL STATIC EXTERN METHOD EnumChildWindows(hWndParent AS IntPtr , lpEnumFunc AS EnumChildProc , lParam AS IntPtr ) AS LOGIC

        [DllImport("user32.dll")];
		INTERNAL STATIC EXTERN METHOD GetDlgCtrlID(hWndCtrl AS IntPtr ) AS LONG

        [DllImport("user32.dll", SetLastError := TRUE)];
		[RETURN:MarshalAs(UnmanagedType.Bool)];
        INTERNAL STATIC EXTERN METHOD PostMessage(hWnd AS IntPtr , Msg AS DWORD , wParam AS IntPtr , lParam AS IntPtr ) AS LOGIC

        INTERNAL DELEGATE EnumChildProc(hWnd AS IntPtr , lParam AS IntPtr ) AS LOGIC

        PRIVATE CONST WM_COMMAND  := 273U AS DWORD

		PUBLIC STATIC METHOD FindMessageBox(caption AS STRING ) AS IntPtr
			RETURN VfpWin32UI.FindWindow("#32770", caption)
		END METHOD

		PUBLIC STATIC METHOD SendCommandToDlgButton(hWnd AS IntPtr , dlgButtonId AS LONG ) AS VOID
			IF hWnd != IntPtr.Zero
				VfpWin32UI.EnumChildWindows(hWnd, { handle , param =>
					VAR dlgCtrlID := VfpWin32UI.GetDlgCtrlID(handle)
					IF dlgCtrlID == dlgButtonId
						VfpWin32UI.PostMessage(hWnd, WM_COMMAND, IntPtr{dlgCtrlID}, handle)
					ENDIF
					RETURN dlgCtrlID != dlgButtonId
				}, IntPtr.Zero)
			ENDIF
        END METHOD
    END CLASS

END NAMESPACE
