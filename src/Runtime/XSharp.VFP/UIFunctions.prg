//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Windows.Forms



/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/messagebox/*" />
FUNCTION MessageBox( eMessageText AS USUAL, nDialogBoxType := 0 AS LONG, cTitleBarText := "" AS STRING,nTimeOut := 0 AS LONG) AS LONG
    LOCAL cMessage AS STRING
    IF !IsString(eMessageText)
        cMessage := AsString(eMessageText)
    ELSE
        cMessage := eMessageText
    ENDIF
    IF String.IsNullOrEmpty(cTitleBarText)
        cTitleBarText := System.IO.Path.GetFileNameWithoutExtension(System.Reflection.Assembly.GetEntryAssembly():Location)
    ENDIF
    LOCAL nButton := MessageBoxButtons.OK               AS MessageBoxButtons
    LOCAL nIcon   := MessageBoxIcon.None                AS MessageBoxIcon
    LOCAL nDefault := MessageBoxDefaultButton.Button1   AS MessageBoxDefaultButton
    nButton  := (MessageBoxButtons)         _AND(nDialogBoxType, 0x0F)
    nIcon    := (MessageBoxIcon)            _AND(nDialogBoxType, 0xF0)
    nDefault := (MessageBoxDefaultButton)   _AND(nDialogBoxType, 0xF00)
    IF nTimeOut >= 1
        RETURN XSharp.VFP.AutoCloseMessageBox.Show(cMessage, cTitleBarText, nTimeOut, nButton, nIcon, nDefault)
    ENDIF
    RETURN System.Windows.Forms.MessageBox.Show(cMessage, cTitleBarText, nButton, nIcon, nDefault)



/// <include file="VFPDocs.xml" path="Runtimefunctions/sysmetric/*" />
FUNCTION SysMetric( nScreenElement AS LONG) AS LONG
    SWITCH nScreenElement
    case 1
        //1 Screen width
        return SystemInformation.PrimaryMonitorMaximizedWindowSize.Width
    case 2
        //2 Screen height.
        return SystemInformation.PrimaryMonitorMaximizedWindowSize.Height

    case 3
        //3 Width of sizable window frame
        return SystemInformation.MinimizedWindowSpacingSize.Width

    case 4
        //4 Height of sizable window frame
        return SystemInformation.MinimizedWindowSpacingSize.Height

    case 5
        //5 Width of scroll arrows on vertical scroll bar
        return SystemInformation.VerticalScrollBarWidth

    case 6
        //6 Height of scroll arrows on vertical scroll bar
        return SystemInformation.VerticalScrollBarArrowHeight

    case 7
        //7 Width of scroll arrows on horizontal scroll bar
        return SystemInformation.HorizontalScrollBarArrowWidth

    case 8
        //8 Height of scroll arrows on horizontal scroll bar
        return SystemInformation.HorizontalScrollBarHeight

    case 9
        //9 Height of window title
        return SystemInformation.CaptionHeight

    case 10
        //10 Width of non-sizable window frame
        return SystemInformation.FixedFrameBorderSize.Width

    case 11
        //11 Height of non-sizable window frame
        return SystemInformation.FixedFrameBorderSize.Height

    case 12
        //12 Width of DOUBLE or PANEL window frame
        return SystemInformation.FrameBorderSize.Width

    case 13
        //13 Height of DOUBLE or PANEL window frame
        return SystemInformation.FrameBorderSize.Height

    case 14
        //14 Scroll box width on horizontal scroll bar in text editing windows
        return SystemInformation.HorizontalScrollBarThumbWidth

    case 15
        //15 Scroll box height on vertical scroll bar in text editing windows
        return SystemInformation.VerticalScrollBarWidth

    case 16
        //16 Minimized window icon width
        return SystemInformation.IconSize.Width

    case 17
        //17 Minimized window icon height
        return SystemInformation.IconSize.Height

    case 18
        // 18 Maximum insertion point width
        return 0
    case 19
        // 19 Maximum insertion point height
        return 0

    case 20
        //20 Single-line menu bar height
        return SystemInformation.MenuHeight

    case 21
        //21 Maximized window width
        return SystemInformation.MaxWindowTrackSize.Width

    case 22
        //22 Maximized window height
        return SystemInformation.MaxWindowTrackSize.Height

    case 23
        //23 Kanji window height
        return SystemInformation.KanjiWindowHeight

    case 24
        //24 Minimum sizable window width
        return SystemInformation.MinWindowTrackSize.Width

    case 25
        //25 Minimum sizable window height
        return SystemInformation.MinWindowTrackSize.Height

    case 26
        //26 Minimum window width
        return SystemInformation.MinimumWindowSize.Width

    case 27
        //27 Minimum window height
        return SystemInformation.MinimumWindowSize.Height

    case 30
        //30 1 if mouse hardware present; otherwise 0
        return iif(SystemInformation.MousePresent == true,  1, 0)

    case 31
        //31 1 for Microsoft Windows debugging version; otherwise 0
        return iif(SystemInformation.DebugOS == true , 1, 0)

    case 32
        //32 1 if mouse buttons swapped; otherwise 0
        return iif(SystemInformation.MouseButtonsSwapped == true, 1, 0)

    case 33
        //33 Width of a button in a half-caption window's caption or title bar
        return SystemInformation.ToolWindowCaptionButtonSize.Width;

    case 34
        //34 Height of half-caption window caption area
        return SystemInformation.ToolWindowCaptionHeight

    END SWITCH
    RETURN 0


