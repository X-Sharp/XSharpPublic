//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Management
USING System.Drawing
USING System.Drawing.Printing
USING System.Windows.Forms
USING Microsoft.Win32
USING System.Reflection
USING System.Text
USING System.IO
USING XSharp.VFP

BEGIN NAMESPACE XSharp.VFP.UI
    /// <summary>
    /// Official implementation of Windows Forms for VFP dialogs
    /// This class will be instantiated via Reflection from XSharp.VFP.dll
    /// </summary>
    PUBLIC CLASS VfpUIProvider IMPLEMENTS IVfpUIProvider
        #region (Sysmetric, MessageBox, etc)
        PUBLIC METHOD ShowMessageBox(cMessage AS STRING, nDialogBoxType AS LONG, cTitleBarText AS STRING, nTimeOut AS LONG) AS LONG
            LOCAL nResult AS LONG
            LOCAL dwTimeOut := IIF(nTimeOut > 0, (DWORD)nTimeOut, 0xFFFFFFFF) AS DWORD

            VAR sw := System.Diagnostics.Stopwatch.StartNew()
            nResult := VfpWin32UI.MessageBoxTimeout( ;
                IntPtr.Zero, ;
                cMessage, ;
                cTitleBarText, ;
                (DWORD)nDialogBoxType, ;
                0, ;
                dwTimeout)

            sw:Stop()

            // In native API 32000 (0x7D00) means TIMEOUT
            IF nResult == 32000
                RETURN -1
            ENDIF

            IF nTimeOut > 0 .AND. sw:ElapsedMilliseconds >= (nTimeOut - 50)
                VAR nDefaultButtonID := (nDialogBoxType & 0xF00) >> 8
                IF nResult <= 1 .OR. nResult == (LONG)nDefaultButtonID
                    RETURN -1
                ENDIF
            ENDIF

            RETURN nResult
        END METHOD

        PUBLIC METHOD SysMetric(nScreenElement AS LONG) AS LONG
            SWITCH nScreenElement
            CASE 1 // SYSMETRIC_SCREENWIDTH
                LOCAL hDC := VfpWin32UI.GetDC(IntPtr.Zero) AS IntPtr
                LOCAL nRes := VfpWin32UI.GetDeviceCaps(hDC, VfpWin32UI.DESKTOP_HORZRES) AS LONG
                VfpWin32UI.ReleaseDC(IntPtr.Zero, hDC)
                RETURN nRes
            CASE 2 // SYSMETRIC_SCREENHEIGHT
                LOCAL hDC := VfpWin32UI.GetDC(IntPtr.Zero) AS IntPtr
                LOCAL nRes := VfpWin32UI.GetDeviceCaps(hDC, VfpWin32UI.DESKTOP_VERTRES) AS LONG
                VfpWin32UI.ReleaseDC(IntPtr.Zero, hDC)
                RETURN nRes
            CASE 3; RETURN SystemInformation.MinimizedWindowSpacingSize.Width
            CASE 4; RETURN SystemInformation.MinimizedWindowSpacingSize.Height
            CASE 5; RETURN SystemInformation.VerticalScrollBarWidth
            CASE 6; RETURN SystemInformation.VerticalScrollBarArrowHeight
            CASE 7; RETURN SystemInformation.HorizontalScrollBarArrowWidth
            CASE 8; RETURN SystemInformation.HorizontalScrollBarHeight
            CASE 9; RETURN SystemInformation.CaptionHeight
            CASE 10; RETURN SystemInformation.FixedFrameBorderSize.Width
            CASE 11; RETURN SystemInformation.FixedFrameBorderSize.Height
            CASE 12; RETURN SystemInformation.FrameBorderSize.Width
            CASE 13; RETURN SystemInformation.FrameBorderSize.Height
            CASE 14; RETURN SystemInformation.HorizontalScrollBarThumbWidth
            CASE 15; RETURN SystemInformation.VerticalScrollBarWidth
            CASE 16; RETURN SystemInformation.IconSize.Width
            CASE 17; RETURN SystemInformation.IconSize.Height
            CASE 18; RETURN 0
            CASE 19; RETURN 0
            CASE 20; RETURN SystemInformation.MenuHeight
            CASE 21; RETURN SystemInformation.MaxWindowTrackSize.Width
            CASE 22; RETURN SystemInformation.MaxWindowTrackSize.Height
            CASE 23; RETURN SystemInformation.KanjiWindowHeight
            CASE 24; RETURN SystemInformation.MinWindowTrackSize.Width
            CASE 25; RETURN SystemInformation.MinWindowTrackSize.Height
            CASE 26; RETURN SystemInformation.MinimumWindowSize.Width
            CASE 27; RETURN SystemInformation.MinimumWindowSize.Height
            CASE 30; RETURN iif(SystemInformation.MousePresent == TRUE,  1, 0)
            CASE 31; RETURN iif(SystemInformation.DebugOS == TRUE , 1, 0)
            CASE 32; RETURN iif(SystemInformation.MouseButtonsSwapped == TRUE, 1, 0)
            CASE 33; RETURN SystemInformation.ToolWindowCaptionButtonSize.Width
            CASE 34; RETURN SystemInformation.ToolWindowCaptionHeight
            END SWITCH
            RETURN 0
        END METHOD

        PUBLIC METHOD ShowAssertDialog(cExpression AS STRING, cMessage AS STRING) AS AssertResult
            LOCAL oDlg AS AssertDialog
            oDlg := AssertDialog{}
            oDlg:Text := "Assertion failed"
            oDlg:Message := cMessage
            oDlg:ShowDialog()

            RETURN oDlg:Result
        END METHOD

        PUBLIC METHOD GetColor(nDefaultColorNumber AS USUAL) AS INT
            LOCAL hRegKey AS RegistryKey
            LOCAL iColorref := -1 AS INT
            LOCAL cRegKeyName := "Software\Microsoft\VisualFoxPro\9.0\ChooseColor" AS STRING

            IF ! IsNumeric(nDefaultColorNumber)
                nDefaultColorNumber := 0
            ENDIF

            VAR iCustomColorsArr := INT[]{16}
            IF RuntimeState.RunningOnWindows
                IF ( hRegKey := Registry.CurrentUser:OpenSubKey( cRegKeyName , TRUE )) != NULL
                    iCustomColorsArr := __ReadColors( hRegKey )
                    hRegKey:Close()
                ELSE
                    hRegKey := Registry.CurrentUser:CreateSubKey(cRegKeyName )
                    var white        := 16777215
                    iCustomColorsArr :=	<INT> { white, white, white, white, white, white , white, white, ;
                        white, white, white, white, white, white , white, white}
                    __WriteColors( hRegKey , iCustomColorsArr )
                    hRegKey:Close()
                ENDIF
            ENDIF

            VAR oDlg := ColorDialog{}
            oDlg:Color := ColorTranslator.FromWin32((INT)nDefaultColorNumber)
            IF RuntimeState.RunningOnWindows
                oDlg:CustomColors := iCustomColorsArr
            ENDIF

            IF oDlg:ShowDialog() == DialogResult.OK
                iColorref := ColorTranslator.ToWin32(oDlg:Color)
                IF RuntimeState.RunningOnWindows
                    IF ( hRegKey := Registry.CurrentUser:OpenSubKey( cRegKeyName , TRUE )) != NULL
                        __WriteColors( hRegKey , oDlg:CustomColors )
                        hRegKey:Close()
                    ENDIF
                ENDIF
            ENDIF
            RETURN iColorref
        END METHOD

        PRIVATE METHOD __WriteColors ( hSubKey AS RegistryKey , iCustomColorsArr AS INT[] ) AS VOID
            LOCAL i, j AS INT
            VAR bRegistryArr := BYTE[]{iCustomColorsArr:Length * 4}
            j := 1
            FOR i := 1 TO iCustomColorsArr:Length
                VAR oColor := ColorTranslator.FromWin32(iCustomColorsArr [i])
                bRegistryArr[j] := oColor:R
                bRegistryArr[j+1] := oColor:G
                bRegistryArr[j+2] := oColor:B
                j := j + 4
            NEXT
            hSubKey:SetValue ( "" , bRegistryArr , RegistryValueKind.Binary)
            RETURN
        END METHOD

        PRIVATE METHOD __ReadColors ( hSubKey AS RegistryKey) AS INT[]
            LOCAL i, j AS INT
            LOCAL iCustomColorsArr AS INT[]
            VAR bRegistryArr := (BYTE[]) hSubKey:GetValue("")
            IF bRegistryArr != NULL
                j := 1
                iCustomColorsArr :=INT[]{bRegistryArr:Length / 4}
                FOR i := 1 TO bRegistryArr:Length STEP 4
                    VAR oColor := Color.FromArgb ( 0 , bRegistryArr[i] , bRegistryArr[i+1] , bRegistryArr[i+2] )
                    iCustomColorsArr[j] := ColorTranslator.ToWin32(oColor)
                    j ++
                NEXT
            ELSE
                iCustomColorsArr := INT[]{0}
            ENDIF
            RETURN iCustomColorsArr
        END METHOD

      PUBLIC METHOD GetFont(cFontName AS USUAL, nFontSize AS USUAL, cFontStyle AS USUAL, nFontCharSet AS USUAL) AS STRING
            LOCAL oDlg AS FontDialog
            LOCAL cSelectedFont := "" AS STRING
            LOCAL eFontstyle AS FontStyle
            LOCAL lReturnCharSet AS LOGIC

            oDlg := FontDialog{}
            oDlg:ShowEffects := FALSE
            oDlg:ShowHelp := TRUE
            oDlg:MinSize := 4
            oDlg:MaxSize := 127

            lReturnCharSet := !IsNil(nFontCharSet)

            IF !IsNil(cFontName)
                LOCAL nSize := 10 AS INT
                IF IsNumeric(nFontSize) .AND. nFontSize > 0
                    nSize := (INT)nFontSize
                ENDIF

                eFontstyle := FontStyle.Regular
                IF IsString(cFontStyle)
                    VAR sStyle := ((STRING)cFontStyle):ToUpper()
                    IF sStyle:Contains("B") ; eFontstyle |= FontStyle.Bold ; ENDIF
                    IF sStyle:Contains("I") ; eFontstyle |= FontStyle.Italic ; ENDIF
                ENDIF

                LOCAL nCharSet := 1 AS BYTE
                IF IsNumeric(nFontCharSet)
                    nCharSet := (BYTE)nFontCharSet
                ENDIF

                TRY
                    oDlg:Font := Font{(STRING)cFontName, (Single)nSize, eFontstyle, GraphicsUnit.Point, nCharSet}
                CATCH
                    NOP
                END TRY
            ENDIF

            IF oDlg:ShowDialog() == DialogResult.OK
                VAR ft := oDlg:Font
                LOCAL sStyle := "" AS STRING
                sStyle := IIF(ft:Bold, "B", "") + IIF(ft:Italic, "I", "")
                IF String.IsNullOrEmpty(sStyle) ; sStyle := "N" ; ENDIF

                cSelectedFont := ft:Name + "," + ;
                                 ((INT)Math.Round(ft:SizeInPoints)):ToString() + "," + ;
                                 sStyle

                IF lReturnCharSet
                    cSelectedFont += "," + ft:GdiCharSet:ToString()
                ENDIF
            ENDIF

            RETURN cSelectedFont
        END METHOD

        PUBLIC METHOD GetPrinters(nValue AS INT) AS ARRAY
            LOCAL iCount AS INT
            LOCAL aResult AS ARRAY
            LOCAL cPrinterName AS STRING

            iCount := PrinterSettings.InstalledPrinters:Count
            IF iCount > 0
                VAR nCols := (DWORD)IIF(nValue > 0, 5, 2)
                aResult := __FoxArray{(DWORD)iCount, nCols}

                FOR VAR i := 0 TO iCount - 1
                    cPrinterName := PrinterSettings.InstalledPrinters[i]

                    TRY
                        VAR oProperty := ManagementObject{"Win32_Printer.DeviceID='" + cPrinterName + "'"}

                        aResult[i + 1, 1] := cPrinterName
                        aResult[i + 1, 2] := IIF(oProperty["PortName"] == NULL, "", (STRING)oProperty["PortName"])

                        IF nValue > 0
                            aResult[i + 1, 3] := IIF(oProperty["DriverName"] == NULL, "", (STRING)oProperty["DriverName"])
                            aResult[i + 1, 4] := IIF(oProperty["Comment"] == NULL, "", (STRING)oProperty["Comment"])
                            aResult[i + 1, 5] := IIF(oProperty["Location"] == NULL, "", (STRING)oProperty["Location"])
                        ENDIF
                    CATCH
                        aResult[i + 1, 1] := cPrinterName
                        aResult[i + 1, 2] := "Unknown"
                    END TRY
                NEXT
                RETURN aResult
            ENDIF

            RETURN {}
        END METHOD

        METHOD LoadPicture(cFileName AS STRING) AS OBJECT
            LOCAL oImage AS OBJECT

            TRY
                oImage := System.Drawing.Image.FromFile(cFileName)
            CATCH e AS Exception
                VAR err := Error.WrapRawException(e)
                err:FuncSym := "LOADPICTURE"
                THROW err
            END TRY

            RETURN oImage
        END METHOD
        #endregion

        #region GET* Dialogs
        PUBLIC METHOD GetDir(cDirectory AS STRING, cText AS STRING, cCaption AS STRING, nFlags AS LONG, lRootOnly AS LOGIC) AS STRING
            VAR oDlg := FolderBrowserDialog{}

            IF !String.IsNullOrEmpty(cText)
                oDlg:Description := cText
            ENDIF

            IF !String.IsNullOrEmpty(cDirectory)
                VAR cFullPath := Path.GetFullPath(cDirectory)
                IF Directory.Exists(cFullPath)
                    oDlg:SelectedPath := cFullPath
                    IF lRootOnly
                        oDlg:RootFolder := Environment.SpecialFolder.MyComputer
                    ENDIF
                ENDIF
            ENDIF

            IF oDlg:ShowDialog() == DialogResult.OK
                VAR cResult := oDlg:SelectedPath
                IF !cResult:EndsWith(Path.DirectorySeparatorChar:ToString())
                    cResult += Path.DirectorySeparatorChar:ToString()
                ENDIF
                RETURN cResult
            ENDIF

            RETURN ""
        END METHOD

        PUBLIC METHOD GetFile(cFileExtensions AS STRING, cText AS STRING, cOpenButtonCaption AS STRING, nButtonType AS LONG, cTitleBarCaption AS STRING) AS STRING
            VAR oDlg := OpenFileDialog{}

            IF !String.IsNullOrEmpty(cTitleBarCaption)
                oDlg:Title := cTitleBarCaption
            ELSEIF !String.IsNullOrEmpty(cText)
                oDlg:Title := cText
            ENDIF

            oDlg:Filter := SELF:ParseVfpFilter(cFileExtensions, FALSE)
            oDlg:CheckFileExists := TRUE

            IF oDlg:ShowDialog() == DialogResult.OK
                RETURN oDlg:FileName
            ENDIF

            RETURN ""
        END METHOD

        PUBLIC METHOD GetPict(cFileExtensions AS STRING, cFileNameCaption AS STRING, cOpenButtonCaption AS STRING) AS STRING
            VAR oDlg := OpenFileDialog{}

            oDlg:Title := IIF(String.IsNullOrEmpty(cFileNameCaption), "Open Picture", cFileNameCaption)
            oDlg:Filter := SELF:ParseVfpFilter(cFileExtensions, TRUE)
            oDlg:CheckFileExists := TRUE

            IF oDlg:ShowDialog() == DialogResult.OK
                RETURN oDlg:FileName
            ENDIF

            RETURN ""
        END METHOD
        #endregion

        #region Private Helper: Filter Parser
        PRIVATE METHOD ParseVfpFilter(cVfpFilter AS STRING, lIsPicture AS LOGIC) AS STRING
            IF String.IsNullOrWhiteSpace(cVfpFilter)
                IF lIsPicture
                    RETURN "All Pictures|*.bmp;*.dib;*.jpg;*.jpeg;*.jpe;*.jfif;*.gif;*.png;*.ico;*.cur;*.ani;*.tif;*.tiff;*.emf|" + ;
                           "Bitmaps (*.bmp;*.dib)|*.bmp;*.dib|" + ;
                           "JPEG (*.jpg;*.jpeg;*.jpe;*.jfif)|*.jpg;*.jpeg;*.jpe;*.jfif|" + ;
                           "GIF (*.gif)|*.gif|" + ;
                           "PNG (*.png)|*.png|" + ;
                           "Icons/Cursors (*.ico;*.cur;*.ani)|*.ico;*.cur;*.ani|" + ;
                           "All Files (*.*)|*.*"
                ELSE
                    RETURN "All Files (*.*)|*.*"
                ENDIF
            ENDIF

            VAR sb := StringBuilder{}
            VAR aParts := cVfpFilter:Split(<CHAR>{c';'}, StringSplitOptions.RemoveEmptyEntries)

            FOREACH cPart AS STRING IN aParts
                VAR cDesc := ""
                VAR cExts := ""
                VAR nColon := cPart:IndexOf(":")

                IF nColon >= 0
                    cDesc := cPart:Substring(0, nColon):Trim()
                    cExts := cPart:Substring(nColon + 1):Trim()
                ELSE
                    cExts := cPart:Trim()
                    cDesc := cExts:ToUpper() + " Files"
                ENDIF

                VAR aExtList := cExts:Split(<char>{c','}, StringSplitOptions.RemoveEmptyEntries)
                VAR cCleanedExts := ""

                FOREACH cRawExt AS STRING IN aExtList
                    VAR cE := cRawExt:Trim()
                    IF !cE:StartsWith("*.")
                        IF cE:StartsWith(".")
                            cE := "*" + cE
                        ELSE
                            cE := "*." + cE
                        ENDIF
                    ENDIF
                    cCleanedExts += cE + ";"
                NEXT

                cCleanedExts := cCleanedExts:TrimEnd(<char>{c';'})

                IF sb:Length > 0
                    sb:Append("|")
                ENDIF

                sb:Append(cDesc):Append("|"):Append(cCleanedExts)
            NEXT

            IF !cVfpFilter:Contains("*.*")
                sb:Append("|All Files (*.*)|*.*")
            ENDIF

            RETURN sb:ToString()
        END METHOD
        #endregion
    END CLASS

END NAMESPACE
