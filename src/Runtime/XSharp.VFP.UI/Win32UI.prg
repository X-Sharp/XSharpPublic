//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.VFP.UI

    INTERNAL STATIC CLASS VfpWin32UI
        // GetPrinter settings
        PUBLIC CONST DESKTOP_HORZRES := 117 AS INT
        PUBLIC CONST DESKTOP_VERTRES := 118 AS INT
        PUBLIC CONST PD_PRINTSETUP := 0x40U AS DWORD

        // SYSMETRIC(): the VFP help points at GetSystemMetrics()
        PUBLIC CONST SM_CXSCREEN       :=  0 AS INT
        PUBLIC CONST SM_CYSCREEN       :=  1 AS INT
        PUBLIC CONST SM_CXVSCROLL      :=  2 AS INT
        PUBLIC CONST SM_CYHSCROLL      :=  3 AS INT
        PUBLIC CONST SM_CYCAPTION      :=  4 AS INT
        PUBLIC CONST SM_CXBORDER       :=  5 AS INT
        PUBLIC CONST SM_CYBORDER       :=  6 AS INT
        PUBLIC CONST SM_CXDLGFRAME     :=  7 AS INT
        PUBLIC CONST SM_CYDLGFRAME     :=  8 AS INT
        PUBLIC CONST SM_CYVTHUMB       :=  9 AS INT
        PUBLIC CONST SM_CXHTHUMB       := 10 AS INT
        PUBLIC CONST SM_CXICON         := 11 AS INT
        PUBLIC CONST SM_CYICON         := 12 AS INT
        PUBLIC CONST SM_CXCURSOR       := 13 AS INT
        PUBLIC CONST SM_CYCURSOR       := 14 AS INT
        PUBLIC CONST SM_CYMENU         := 15 AS INT
        PUBLIC CONST SM_CXFULLSCREEN   := 16 AS INT
        PUBLIC CONST SM_CYFULLSCREEN   := 17 AS INT
        PUBLIC CONST SM_CYKANJIWINDOW  := 18 AS INT
        PUBLIC CONST SM_MOUSEPRESENT   := 19 AS INT
        PUBLIC CONST SM_CYVSCROLL      := 20 AS INT
        PUBLIC CONST SM_CXHSCROLL      := 21 AS INT
        PUBLIC CONST SM_DEBUG          := 22 AS INT
        PUBLIC CONST SM_SWAPBUTTON     := 23 AS INT
        PUBLIC CONST SM_CXMIN          := 28 AS INT
        PUBLIC CONST SM_CYMIN          := 29 AS INT
        PUBLIC CONST SM_CXSIZE         := 30 AS INT
        PUBLIC CONST SM_CYSIZE         := 31 AS INT
        PUBLIC CONST SM_CXMINTRACK     := 34 AS INT
        PUBLIC CONST SM_CYMINTRACK     := 35 AS INT
        PUBLIC CONST SM_CXSMSIZE       := 52 AS INT
        PUBLIC CONST SM_CYSMSIZE       := 53 AS INT

        [StructLayout(LayoutKind.Sequential, CharSet := CharSet.Unicode)];
        PUBLIC STRUCT PRINTDLGW
            PUBLIC lStructSize AS DWORD
            PUBLIC hwndOwner AS IntPtr
            PUBLIC hDevMode AS IntPtr
            PUBLIC hDevNames AS IntPtr
            PUBLIC hDC AS IntPtr
            PUBLIC Flags AS DWORD
            PUBLIC nFromPage AS WORD
            PUBLIC nToPage AS WORD
            PUBLIC nMinPage AS WORD
            PUBLIC nMaxPage AS WORD
            PUBLIC nCopies AS WORD
            PUBLIC hInstance AS IntPtr
            PUBLIC lCustData AS IntPtr
            PUBLIC lpfnPrintHook AS IntPtr
            PUBLIC lpfnSetupHook AS IntPtr
            PUBLIC lpPrintTemplateName AS IntPtr
            PUBLIC lpSetupTemplateName AS IntPtr
            PUBLIC hPrintTemplate AS IntPtr
            PUBLIC hSetupTemplate AS IntPtr
        END STRUCT
        [DllImport("comdlg32.dll", CharSet := CharSet.Unicode, SetLastError := TRUE, EntryPoint := "PrintDlgW")];
        INTERNAL STATIC EXTERN METHOD PrintDlg(lppd REF PRINTDLGW) AS LOGIC
        [DllImport("kernel32.dll")];
        INTERNAL STATIC EXTERN METHOD GlobalLock(hMem AS IntPtr) AS IntPtr
        [DllImport("kernel32.dll")];
        [RETURN:MarshalAs(UnmanagedType.Bool)];
        INTERNAL STATIC EXTERN METHOD GlobalUnlock(hMem AS IntPtr) AS LOGIC
        [DllImport("kernel32.dll")];
        INTERNAL STATIC EXTERN METHOD GlobalFree(hMem AS IntPtr) AS IntPtr

        PUBLIC STATIC METHOD ShowPrintSetup(hwndOwner AS IntPtr) AS STRING
            LOCAL pd AS PRINTDLGW
            LOCAL cName := "" AS STRING
            pd := PRINTDLGW{}
            pd:lStructSize := (DWORD) Marshal.SizeOf(typeof(PRINTDLGW))
            pd:hwndOwner   := hwndOwner
            pd:Flags       := PD_PRINTSETUP
            IF PrintDlg(REF pd)
                IF pd:hDevNames != IntPtr.Zero
                    VAR pLock := GlobalLock(pd:hDevNames)
                    IF pLock != IntPtr.Zero
                        TRY
                            VAR wDeviceOffset := (INT) (WORD) Marshal.ReadInt16(pLock, 2)
                            cName := Marshal.PtrToStringUni(IntPtr.Add(pLock, wDeviceOffset * 2))
                        FINALLY
                            GlobalUnlock(pd:hDevNames)
                        END TRY
                    ENDIF
                ENDIF
            ENDIF
            IF pd:hDevMode  != IntPtr.Zero ; GlobalFree(pd:hDevMode)  ; ENDIF
            IF pd:hDevNames != IntPtr.Zero ; GlobalFree(pd:hDevNames) ; ENDIF
            RETURN IIF(cName == NULL, "", cName)
        END METHOD
        // GetPrinter settings

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
        STATIC EXTERN METHOD GetSystemMetrics(nIndex AS INT) AS INT

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

        [StructLayout(LayoutKind.Sequential, CharSet := CharSet.Auto)];
        PUBLIC STRUCT TEXTMETRIC
            PUBLIC tmHeight AS INT
            PUBLIC tmAscent AS INT
            PUBLIC tmDescent AS INT
            PUBLIC tmInternalLeading AS INT
            PUBLIC tmExternalLeading AS INT
            PUBLIC tmAveCharWidth AS INT
            PUBLIC tmMaxCharWidth AS INT
            PUBLIC tmWeight AS INT
            PUBLIC tmOverhang AS INT
            PUBLIC tmDigitizedAspectX AS INT
            PUBLIC tmDigitizedAspectY AS INT
            PUBLIC tmFirstChar AS CHAR
            PUBLIC tmLastChar AS CHAR
            PUBLIC tmDefaultChar AS CHAR
            PUBLIC tmBreakChar AS CHAR
            PUBLIC tmItalic AS BYTE
            PUBLIC tmUnderlined AS BYTE
            PUBLIC tmStruckOut AS BYTE
            PUBLIC tmPitchAndFamily AS BYTE
            PUBLIC tmCharSet AS BYTE
        END STRUCT

        [DllImport("gdi32.dll", CharSet := CharSet.Auto, SetLastError := TRUE, ExactSpelling := TRUE)];
        STATIC EXTERN METHOD GetTextMetrics(hDC AS IntPtr, lpMetrics OUT TEXTMETRIC) AS LOGIC

        [DllImport("gdi32.dll", CharSet := CharSet.Auto, SetLastError := TRUE, ExactSpelling := TRUE)];
        STATIC EXTERN METHOD SelectObject(hDC AS IntPtr, hObject as IntPtr) as IntPtr

        [DllImport("gdi32.dll", CharSet := CharSet.Auto, SetLastError := TRUE, ExactSpelling := TRUE)];
        STATIC EXTERN METHOD DeleteObject(hObject AS IntPtr) AS Logic

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
