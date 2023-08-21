//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



USING System
USING System.Collections.Generic
USING System.Text
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.VFP

    INTERNAL PARTIAL STATIC CLASS Win32


    INTERNAL STATIC METHOD GetParentWindow() AS IntPtr
        LOCAL hResult AS IntPtr
        hResult := UnsafeNativeMethods.GetActiveWindow()
        IF hResult == IntPtr.Zero
            hResult := UnsafeNativeMethods.GetDesktopWindow()
        ENDIF
        RETURN hResult
	INTERNAL STATIC CLASS UnsafeNativeMethods
        [DllImport("user32.dll", CharSet := CharSet.Ansi)];
        INTERNAL STATIC METHOD GetActiveWindow() AS IntPtr PASCAL

        [DllImport("user32.dll", CharSet := CharSet.Ansi)];
        INTERNAL STATIC METHOD GetDesktopWindow() AS IntPtr PASCAL

		INTERNAL DELEGATE EnumChildProc(hWnd AS IntPtr , lParam AS IntPtr ) AS LOGIC

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
	END CLASS

	PUBLIC STATIC METHOD FindMessageBox(caption AS STRING ) AS IntPtr
		RETURN UnsafeNativeMethods.FindWindow("#32770", caption)

    PRIVATE CONST WM_COMMAND  := 273U AS DWORD

	PUBLIC STATIC METHOD SendCommandToDlgButton(hWnd AS IntPtr , dlgButtonId AS LONG ) AS VOID
		IF hWnd != IntPtr.Zero
			UnsafeNativeMethods.EnumChildWindows(hWnd, { handle , param =>
				VAR dlgCtrlID := UnsafeNativeMethods.GetDlgCtrlID(handle)
				IF dlgCtrlID == dlgButtonId
					UnsafeNativeMethods.PostMessage(hWnd, WM_COMMAND, IntPtr{dlgCtrlID}, handle)
				ENDIF
				RETURN dlgCtrlID != dlgButtonId
			}, IntPtr.Zero)
		ENDIF
END CLASS
END NAMESPACE // XSharp.VFP
