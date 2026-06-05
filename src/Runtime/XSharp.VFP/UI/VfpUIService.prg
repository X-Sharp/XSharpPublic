//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Reflection
USING System.IO
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.VFP

    PUBLIC INTERFACE IVfpUIProvider
        METHOD ShowMessageBox(cMessage AS STRING, nDialogBoxType AS LONG, cTitleBarText AS STRING, nTimeOut AS LONG) AS LONG
        METHOD SysMetric(nScreenElement AS LONG) AS LONG
        METHOD ShowAssertDialog(cExpression AS STRING, cMessage AS STRING) AS AssertResult
        METHOD GetColor(nDefaultColorNumber AS USUAL) AS INT
        METHOD GetFont(cFontName AS USUAL, nFontSize AS USUAL, cFontStyle AS USUAL, nFontCharSet AS USUAL) AS STRING
        METHOD GetPrinters(nValue AS INT) AS ARRAY
        METHOD GetDir(cDirectory AS STRING, cText AS STRING, cCaption AS STRING, nFlags AS LONG, lRootOnly AS LOGIC) AS STRING
        METHOD GetFile(cFileExtensions AS STRING, cText AS STRING, cOpenButtonCaption AS STRING, nButtonType AS LONG, cTitleBarCaption AS STRING) AS STRING
        METHOD GetPict(cFileExtensions AS STRING, cFileNameCaption AS STRING, cOpenButtonCaption AS STRING) AS STRING
        METHOD LoadPicture(cFileName AS STRING) AS OBJECT
        METHOD FontMetric(nAttribute AS LONG, cFontName AS USUAL, nFontSize AS USUAL, cFontStyle AS USUAL) AS LONG
    END INTERFACE

    PUBLIC STATIC CLASS VfpUIService
        STATIC PRIVATE _provider AS IVfpUIProvider

        STATIC PUBLIC PROPERTY Provider AS IVfpUIProvider
            GET
                IF _provider == NULL
                    LoadProvider()
                ENDIF
                RETURN _provider
            END GET
            SET
                _provider := VALUE
            END SET
        END PROPERTY

        STATIC PRIVATE METHOD LoadProvider() AS VOID
            // Exclude non interactive solutions (services/IIS)
            IF !Environment.UserInteractive
                _provider := HeadlessUIProvider{}
                RETURN
            ENDIF

            TRY
                VAR oAsm := XSharp.AssemblyHelper.Load("XSharp.VFP.UI")

                IF oAsm != NULL
                    VAR oType := oAsm:GetType("XSharp.VFP.UI.VfpUIProvider", FALSE, TRUE)
                    IF oType != NULL AND typeof(IVfpUIProvider):IsAssignableFrom(oType)
                        _provider := (IVfpUIProvider) Activator.CreateInstance(oType)
                    ENDIF
                ENDIF
            CATCH
                _provider := NULL
            END TRY

            IF _provider == NULL
                _provider := HeadlessUIProvider{}
            ENDIF
        END METHOD
    END CLASS

    PUBLIC CLASS HeadlessUIProvider IMPLEMENTS IVfpUIProvider
        PUBLIC METHOD ShowMessageBox(cMessage AS STRING, nDialogBoxType AS LONG, cTitleBarText AS STRING, nTimeOut AS LONG) AS LONG
            Console.WriteLine("MESSAGEBOX: " + cTitleBarText + " - " + cMessage)
            RETURN 1
        END METHOD

        PUBLIC METHOD SysMetric(nScreenElement AS LONG) AS LONG
            IF XSharp.RuntimeState.RunningOnWindows
            SWITCH nScreenElement
                CASE 1 // SYSMETRIC_SCREENWIDTH (Physical)
                    VAR hDC := Win32.GetDC(IntPtr.Zero)
                    VAR nRes := Win32.GetDeviceCaps(hDC, Win32.DESKTOP_HORZRES)
                    Win32.ReleaseDC(IntPtr.Zero, hDC)
                    RETURN nRes
                CASE 2 // SYSMETRIC_SCREENHEIGHT (Physical)
                    VAR hDC := Win32.GetDC(IntPtr.Zero)
                    VAR nRes := Win32.GetDeviceCaps(hDC, Win32.DESKTOP_VERTRES)
                    Win32.ReleaseDC(IntPtr.Zero, hDC)
                    RETURN nRes

                // Mapeo manual de GetSystemMetrics para el resto (Equivalentes a SystemInformation)
                CASE 3;  RETURN Win32.GetSystemMetrics(28) // SM_CXMIN - Minimized window width
                CASE 4;  RETURN Win32.GetSystemMetrics(29) // SM_CYMIN - Minimized window height
                CASE 5;  RETURN Win32.GetSystemMetrics(2)  // SM_CXVSCROLL
                CASE 6;  RETURN Win32.GetSystemMetrics(20) // SM_CYVSCROLL (arrow height)
                CASE 7;  RETURN Win32.GetSystemMetrics(21) // SM_CXHSCROLL (arrow width)
                CASE 8;  RETURN Win32.GetSystemMetrics(3)  // SM_CYHSCROLL
                CASE 9;  RETURN Win32.GetSystemMetrics(4)  // SM_CYCAPTION
                CASE 10; RETURN Win32.GetSystemMetrics(5)  // SM_CXBORDER
                CASE 11; RETURN Win32.GetSystemMetrics(6)  // SM_CYBORDER
                CASE 12; RETURN Win32.GetSystemMetrics(32) // SM_CXFRAME
                CASE 13; RETURN Win32.GetSystemMetrics(33) // SM_CYFRAME
                CASE 14; RETURN Win32.GetSystemMetrics(10) // SM_CXHTHUMB
                CASE 15; RETURN Win32.GetSystemMetrics(9)  // SM_CYVTHUMB
                CASE 16; RETURN Win32.GetSystemMetrics(11) // SM_CXICON
                CASE 17; RETURN Win32.GetSystemMetrics(12) // SM_CYICON
                CASE 18; RETURN Win32.GetSystemMetrics(13) // SM_CXCURSOR
                CASE 19; RETURN Win32.GetSystemMetrics(14) // SM_CYCURSOR
                CASE 20; RETURN Win32.GetSystemMetrics(15) // SM_CYMENU
                CASE 21; RETURN Win32.GetSystemMetrics(16) // SM_CXFULLSCREEN
                CASE 22; RETURN Win32.GetSystemMetrics(17) // SM_CYFULLSCREEN
                CASE 23; RETURN Win32.GetSystemMetrics(18) // SM_CYKANJIWINDOW
                CASE 24; RETURN Win32.GetSystemMetrics(34) // SM_CXMINTRACK
                CASE 25; RETURN Win32.GetSystemMetrics(35) // SM_CYMINTRACK
                CASE 26; RETURN Win32.GetSystemMetrics(28) // SM_CXMIN
                CASE 27; RETURN Win32.GetSystemMetrics(29) // SM_CYMIN
                CASE 30; RETURN iif(Win32.GetSystemMetrics(19) != 0, 1, 0) // SM_MOUSEPRESENT
                CASE 31; RETURN iif(Win32.GetSystemMetrics(22) != 0, 1, 0) // SM_DEBUG
                CASE 32; RETURN iif(Win32.GetSystemMetrics(23) != 0, 1, 0) // SM_SWAPBUTTON
                CASE 33; RETURN Win32.GetSystemMetrics(30) // SM_CXSIZE (half height button width aprox)
                CASE 34; RETURN Win32.GetSystemMetrics(31) // SM_CYSIZE
            END SWITCH
            ENDIF
            RETURN 0
        END METHOD

        PUBLIC METHOD ShowAssertDialog(cExpression AS STRING, cMessage AS STRING) AS AssertResult
            Console.WriteLine("ASSERT FAILED: " + cMessage)
            RETURN AssertResult.Ignore
        END METHOD

        PUBLIC METHOD GetColor(nDefaultColorNumber AS USUAL) AS INT
            RETURN -1
        END METHOD

        PUBLIC METHOD GetFont(cFontName AS USUAL, nFontSize AS USUAL, cFontStyle AS USUAL, nFontCharSet AS USUAL) AS STRING
            RETURN ""
        END METHOD

        PUBLIC METHOD GetPrinters(nValue AS INT) AS ARRAY
            RETURN {}
        END METHOD

        METHOD GetDir(cDirectory AS STRING, cText AS STRING, cCaption AS STRING, nFlags AS LONG, lRootOnly AS LOGIC) AS STRING
            RETURN ""
        END METHOD

        METHOD GetFile(cFileExtensions AS STRING, cText AS STRING, cOpenButtonCaption AS STRING, nButtonType AS LONG, cTitleBarCaption AS STRING) AS STRING
            RETURN ""
        END METHOD

        METHOD GetPict(cFileExtensions AS STRING, cFileNameCaption AS STRING, cOpenButtonCaption AS STRING) AS STRING
            RETURN ""
        END METHOD

        METHOD LoadPicture(cFileName AS STRING) AS OBJECT
            Console.WriteLine("LOADPICTURE: Attempted to load: " + cFileName)
            RETURN NULL_OBJECT
        END METHOD

        METHOD FontMetric(nAttribute AS LONG, cFontName AS USUAL, nFontSize AS USUAL, cFontStyle AS USUAL) AS LONG
            RETURN 0
        END METHOD

    END CLASS

END NAMESPACE
