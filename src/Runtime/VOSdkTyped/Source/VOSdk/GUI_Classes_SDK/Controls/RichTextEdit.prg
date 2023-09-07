//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//#define EM_FINDTEXTW			(WM_USER + 123)
//#define EM_FINDTEXTEXW			(WM_USER + 124)


/// <include file="Gui.xml" path="doc/RichEdit/*" />
CLASS RichEdit INHERIT MultiLineEdit
    PROTECT oBackgroundColor AS Color
    PROTECT pStreamPos AS BYTE PTR

    /// <exclude />
    PROPERTY ControlType AS ControlType GET ControlType.RichEdit

    /// <include file="Gui.xml" path="doc/RichEdit.ctor/*" />

    CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle)
        GuiWin32.LoadLibrary("RICHED20.DLL")
        IF IsNil(kStyle) .OR. !IsLong(kStyle)
            kStyle := _OR(ES_MULTILINE, ES_AUTOHSCROLL, ES_AUTOVSCROLL)
        ENDIF

        SUPER(oOwner, xID, oPoint, oDimension, kStyle)
        SELF:cClassName := RICHEDIT_CLASS

        GuiWin32.SendMessage(oCtrl:Handle, EM_SETEVENTMASK, 0, LONGINT(_CAST, _OR(ENM_CHANGE, ENM_PROTECTED, ENM_SCROLL, ENM_SELCHANGE, ENM_UPDATE)))

        RETURN

    /// <exclude />
    PROPERTY __RichEdit AS VORichTextBox GET (VORichTextBox) oCtrl

    /// <exclude />
    property __StreamPos AS BYTE PTR GET pStreamPos SET pStreamPos := value

    /// <exclude />
    PROPERTY __Value as usual
        set
            LOCAL sVal AS STRING
            sVal := AsString(value)
            IF	sVal:StartsWith("{\rtf", StringComparison.OrdinalIgnoreCase)
                __RichEdit:Rtf := sVal
            ELSE
                __RichEdit:Text := sVal
            ENDIF
            SUPER:__Value := value
        end set
    end property

    /// <include file="Gui.xml" path="doc/RichEdit.Alignment/*" />
    PROPERTY Alignment AS LONG GET __RichEdit:SelectionAlignment SET __RichEdit:SelectionAlignment := (System.Windows.Forms.HorizontalAlignment) value


    /// <include file="Gui.xml" path="doc/RichEdit.BackgroundColor/*" />
    PROPERTY BackgroundColor AS Color
        GET
            RETURN __RichEdit:BackColor
        end get
        set
            oBackgroundColor := value
            __RichEdit:BackColor := value
        end set
    end property

    /// <include file="Gui.xml" path="doc/RichEdit.Background/*" />
    PROPERTY Background AS Brush
        GET
            return SUPER:Background
        END GET
        SET
        LOCAL dwColor AS DWORD
        dwColor:=WC.GetBrushColor(value)
        SELF:BackgroundColor:= dwColor
        __RichEdit:BackColor := SELF:BackgroundColor
        END SET
    END PROPERTY


    /// <include file="Gui.xml" path="doc/RichEdit.CanPaste/*" />
    METHOD CanPaste(dwClipboardFormat AS LONG)
        RETURN __RichEdit:CanPaste(System.Windows.Forms.DataFormats.GetFormat(dwClipboardFormat))

    /// <include file="Gui.xml" path="doc/RichEdit.EnableAdvancedTypography/*" />
    METHOD EnableAdvancedTypography(lEnable := TRUE AS LOGIC) AS VOID
        LOCAL dwOption AS LONG

        dwOption := iif (lEnable, TO_ADVANCEDTYPOGRAPHY, TO_SIMPLELINEBREAK)
        GuiWin32.SendMessage(oCtrl:Handle, EM_SETTYPOGRAPHYOPTIONS, dwOption, LONGINT(_CAST, dwOption))
        RETURN

    /// <include file="Gui.xml" path="doc/RichEdit.ControlFont/*" />
    //ACCESS ControlFont
    //Todo Font
    //	RETURN  NULL_OBJECT
    ////PP-040322 Update from S Ebert
    //LOCAL oFont AS Font
    //LOCAL strucCharFormat IS _winCHARFORMAT
    //LOCAL wFamily AS WORD
    //strucCharFormat:cbSize := _SIZEOF(_winCHARFORMAT)
    //strucCharFormat:dwMask := DWORD(_CAST, _OR(CFM_FACE, CFM_SIZE, CFM_BOLD, CFM_ITALIC, CFM_STRIKEOUT, CFM_UNDERLINE))
    //GuiWin32.SendMessage(oCtrl:Handle, EM_GETCHARFORMAT, DWORD(_CAST, TRUE), LONGINT(_CAST, @strucCharFormat))

    //DO CASE
    //CASE _AND(strucCharFormat:bPitchAndFamily, FF_DECORATIVE) == 1
    //	wFamily := FONTDECORATIVE
    //CASE _AND(strucCharFormat:bPitchAndFamily, FF_MODERN) == 1
    //	wFamily := FONTMODERN
    //CASE _AND(strucCharFormat:bPitchAndFamily, FF_ROMAN) == 1
    //	wFamily := FONTROMAN
    //CASE _AND(strucCharFormat:bPitchAndFamily, FF_SCRIPT) == 1
    //	wFamily := FONTSCRIPT
    //CASE _AND(strucCharFormat:bPitchAndFamily, FF_SWISS) == 1
    //	wFamily := FONTSWISS
    //OTHERWISE
    //	wFamily := FONTANY
    //ENDCASE

    //oFont := Font{wFamily, (strucCharFormat:yHeight / TWIPSCONVERSION), Psz2String(@strucCharFormat:szFaceName[1])}

    //IF _AND(strucCharFormat:dwEffects, CFE_BOLD) != 0
    //	oFont:Bold := TRUE
    //ENDIF
    //IF _AND(strucCharFormat:dwEffects, CFE_ITALIC) != 0
    //	oFont:Italic := TRUE
    //ENDIF
    //IF _AND(strucCharFormat:dwEffects, CFE_UNDERLINE) != 0
    //	oFont:Underline := TRUE
    //ENDIF
    //IF _AND(strucCharFormat:dwEffects, CFE_STRIKEOUT) != 0
    //	oFont:Strikethru := TRUE
    //ENDIF

    //RETURN oFont

    //ASSIGN Font (oNewFont)
    //Todo Font
    //LOCAL strucCharFormat IS _winCHARFORMAT
    //LOCAL cFaceName AS STRING

    //// First retrieve the current effects
    //strucCharFormat:cbSize := _SIZEOF(_winCHARFORMAT)
    //strucCharFormat:dwMask := DWORD(_CAST, _OR(CFM_FACE, CFM_SIZE, CFM_BOLD, CFM_ITALIC, CFM_STRIKEOUT, CFM_UNDERLINE))
    //GuiWin32.SendMessage(oCtrl:Handle, EM_GETCHARFORMAT, DWORD(_CAST, TRUE), LONGINT(_CAST, @strucCharFormat))

    //IF (_AND(strucCharFormat:dwEffects, CFE_BOLD) != 0 .AND. !oNewFont:Bold) .OR. ;
    //(_AND(strucCharFormat:dwEffects, CFE_BOLD) == 0 .AND. oNewFont:Bold)
    //	strucCharFormat:dwEffects := _XOR(strucCharFormat:dwEffects, CFE_BOLD)
    //ENDIF
    //IF (_AND(strucCharFormat:dwEffects, CFE_ITALIC) != 0 .AND. !oNewFont:Italic) .OR. ;
    //(_AND(strucCharFormat:dwEffects, CFE_ITALIC) == 0 .AND. oNewFont:Italic)
    //	strucCharFormat:dwEffects := _XOR(strucCharFormat:dwEffects, CFE_ITALIC)
    //ENDIF
    //IF (_AND(strucCharFormat:dwEffects, CFE_STRIKEOUT) != 0 .AND. !oNewFont:Strikethru) .OR. ;
    //(_AND(strucCharFormat:dwEffects, CFE_STRIKEOUT) == 0 .AND. oNewFont:Strikethru)
    //	strucCharFormat:dwEffects := _XOR(strucCharFormat:dwEffects, CFE_STRIKEOUT)
    //ENDIF
    //IF (_AND(strucCharFormat:dwEffects, CFE_UNDERLINE) != 0 .AND. !oNewFont:Underline) .OR. ;
    //(_AND(strucCharFormat:dwEffects, CFE_UNDERLINE) == 0 .AND. oNewFont:Underline)
    //	strucCharFormat:dwEffects := _XOR(strucCharFormat:dwEffects, CFE_UNDERLINE)
    //ENDIF

    //// Set the values
    //strucCharFormat:bCharSet := oNewFont:__FontCharSet
    //strucCharFormat:bPitchAndFamily := oNewFont:__FontPitchAndFamily
    //strucCharFormat:yHeight := oNewFont:__PointSize * TWIPSCONVERSION
    //cFaceName := oNewFont:__FontFaceName
    //IF (NULL_STRING != cFaceName)
    //	MemCopy(@strucCharFormat:szFaceName[1], String2Psz(cFaceName), SLen(cFaceName)+1)
    //ELSE
    //	MemClear(@strucCharFormat:szFaceName[1], LF_FACESIZE)
    //END
    //// GuiWin32.SendMessage(oCtrl:Handle, EM_SETCHARFORMAT, _Or(SCF_SELECTION, SCF_WORD), LONG(_CAST, @strucCharFormat))
    //GuiWin32.SendMessage(oCtrl:Handle, EM_SETCHARFORMAT, SCF_SELECTION, LONGINT(_CAST, @strucCharFormat))

    //RETURN

    /// <include file="Gui.xml" path="doc/RichEdit.GetOption/*" />
    METHOD GetOption(kOption AS LONG) AS LOGIC
        // check to see if the specified option is set
        RETURN _AND(kOption, GuiWin32.SendMessage(oCtrl:Handle, EM_SETOPTIONS, 0, 0)) != 0

    /// <include file="Gui.xml" path="doc/RichEdit.GetTabStops/*" />
    METHOD GetTabStops() AS INT[]
        RETURN __RichEdit:SelectionTabs

    /// <include file="Gui.xml" path="doc/RichEdit.GetTextRange/*" />
    METHOD GetTextRange(oRange as Range) as string
        LOCAL sRet AS STRING
        LOCAL strucTextRange IS winTextRange

        strucTextRange:chrg_Min := oRange:Min - 1
        strucTextRange:chrg_Max := oRange:Max - 1
        strucTextRange:lpstrText := MemAlloc(oRange:Max - oRange:Min + 1)

        IF (PTR(_CAST, strucTextRange:lpstrText) != NULL_PTR)
            GuiWin32.SendMessage(oCtrl:Handle, EM_GETTEXTRANGE, 0, LONGINT(_CAST, @strucTextRange))
            sRet := Psz2String(strucTextRange:lpstrText)
            MemFree(strucTextRange:lpstrText)
        ENDIF

        RETURN sRet

    /// <include file="Gui.xml" path="doc/RichEdit.GetWordBreak/*" />
    METHOD GetWordBreak(nCharPos AS LONG, kWordBreakType AS LONG) AS LONG
        IF kWordBreakType == REGWB_ISDELIMITER
            RETURN LOGIC(_CAST, GuiWin32.SendMessage(oCtrl:Handle, EM_FINDWORDBREAK, kWordBreakType, (LONG) nCharPos - 1))
        ENDIF
        RETURN GuiWin32.SendMessage(oCtrl:Handle, EM_FINDWORDBREAK, kWordBreakType, (LONG) nCharPos - 1)

    /// <include file="Gui.xml" path="doc/RichEdit.HideSelection/*" />
    METHOD HideSelection(lTemporary := TRUE AS LOGIC) AS VOID
        GuiWin32.SendMessage(oCtrl:Handle, EM_HIDESELECTION, DWORD(_CAST, TRUE), LONGINT(_CAST, lTemporary))
        RETURN

        /*
        Rich Edit version DLL
        1.0 Riched32.dll
        2.0 Riched20.dll
        3.0 Riched20.dll
        4.1 Msftedit.dll

        The following list describes which versions of Rich Edit are included in which releases of Microsoft Windows®.

        Windows XP SP1 Includes Rich Edit 4.1, Rich Edit 3.0, and a Rich Edit 1.0 emulator.
        Windows XP Includes Rich Edit 3.0 with a Rich Edit 1.0 emulator.
        Windows Me Includes Rich Edit 1.0 and 3.0.
        Windows 2000 Includes Rich Edit 3.0 with a Rich Edit 1.0 emulator.
        Windows NT 4.0 Includes Rich Edit 1.0 and 2.0.
        Windows 98 Includes Rich Edit 1.0 and 2.0.
        Windows 95 Includes only Rich Edit 1.0. However, Riched20.dll is compatible with Windows 95 and may be installed by an application that requires it.
        */

    /// <include file="Gui.xml" path="doc/RichEdit.LineFromCharacter/*" />
    METHOD LineFromCharacter(nCharacterPos AS LONG)
        RETURN __RichEdit:GetLineFromCharIndex(	nCharacterPos-1)+1

    /// <include file="Gui.xml" path="doc/RichEdit.LoadFromFile/*" />

    METHOD LoadFromFile(cFileName AS STRING) AS LOGIC
        RETURN SELF:LoadFromFile(cFileName, SF_RTF)

    /// <include file="Gui.xml" path="doc/RichEdit.LoadFromFile/*" />
    METHOD LoadFromFile(cFileName AS STRING, dwFormat AS LONG) AS LOGIC
        LOCAL lOk AS LOGIC
        TRY
            DO CASE
            CASE dwFormat == SF_RTF
                __RichEdit:LoadFile(cFileName, System.Windows.Forms.RichTextBoxStreamType.RichText)
            OTHERWISE
                __RichEdit:LoadFile(cFileName, System.Windows.Forms.RichTextBoxStreamType.PlainText)
            ENDCASE
            lOk := TRUE
        CATCH AS Exception
            lOk := FALSE
        END TRY
        RETURN lOk

    /// <inheritdoc />
        PROPERTY Margins  AS Dimension
        GET
        IF SELF:ValidateControl()
            RETURN (Dimension) __RichEdit:Margin
        ENDIF

        RETURN Dimension{0,0}
        END GET
        SET
        IF SELF:ValidateControl()
            LOCAL oPadding AS System.Windows.Forms.Padding
            oPadding := __RichEdit:Margin
            IF oPadding:Left != value:Width .or. oPadding:Right != value:Height
                oPadding:Left := value:Width
                oPadding:Right := value:Height
                __RichEdit:Margin := oPadding
            ENDIF
        ENDIF

        end set
        end property
    /// <include file="Gui.xml" path="doc/RichEdit.Margin/*" />
    METHOD Margin(nStart AS LONG, nRight AS LONG, nOffset AS LONG) AS LONG
        LOCAL strucParaFormat IS _winPARAFORMAT

        strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
        strucParaFormat:dwMask := DWORD(_CAST, _OR(PFM_STARTINDENT, PFM_RIGHTINDENT, PFM_OFFSET))
        strucParaFormat:dxStartIndent := nStart
        strucParaFormat:dxRightIndent := nRight
        strucParaFormat:dxOffset := nOffset
        GuiWin32.SendMessage(oCtrl:Handle, EM_SETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

        RETURN 1

    /// <include file="Gui.xml" path="doc/RichEdit.Numbering/*" />
    PROPERTY Numbering AS WORD
    GET
        LOCAL strucParaFormat IS _winPARAFORMAT
        strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
        strucParaFormat:dwMask := PFM_NUMBERING
        GuiWin32.SendMessage(oCtrl:Handle, EM_GETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

        RETURN strucParaFormat:wNumbering
    END GET
    SET
        LOCAL strucParaFormat IS _winPARAFORMAT
        strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
        strucParaFormat:dwMask := PFM_NUMBERING
        strucParaFormat:wNumbering := value
        GuiWin32.SendMessage(oCtrl:Handle, EM_SETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

    END SET
    END PROPERTY

    /// <include file="Gui.xml" path="doc/RichEdit.PasteSpecial/*" />
    METHOD PasteSpecial(dwClipboardFormat AS DWORD) AS VOID
        GuiWin32.SendMessage(oCtrl:Handle, EM_PASTESPECIAL, dwClipboardFormat, 0)
        RETURN

    /// <include file="Gui.xml" path="doc/RichEdit.PrimaryIndent/*" />
    PROPERTY PrimaryIndent  AS LONG GET __RichEdit:SelectionIndent SET __RichEdit:SelectionIndent := value

    /// <include file="Gui.xml" path="doc/RichEdit.Print/*" />
    METHOD Print(oPrintingDevice, oRange)
        //Todo Richedit Print
        //LOCAL strucFormatRange IS _winFormatRange
        //LOCAL strucPrintDlg IS _winPrintDlg
        //LOCAL strucDocInfo IS _winDocInfo
        //LOCAL liTextOut AS LONGINT
        //LOCAL liTextPos AS LONGINT //SE
        //LOCAL liTextAmt AS LONGINT
        //LOCAL hDC AS PTR
        //LOCAL cDeviceName AS STRING
        //LOCAL pszDevice AS PSZ
        //LOCAL pszClassName AS PSZ
        //LOCAL lBanding AS LOGIC
        //LOCAL rc1 IS _WINRECT
        //LOCAL rc2 IS _WINRECT
        //LOCAL DIM strucTextLength[2] AS DWORD

    ////RvdH 070717 Changed text length calculatio to 'precize'
    //strucTextLength[1] :=   2| 8	// GTL_PRECISE | GTL_NUMCHARS
    //strucTextLength[2] := CP_ACP
    //liTextAmt := GuiWin32.SendMessage(oCtrl:Handle, EM_GETTEXTLENGTHEX, DWORD(_CAST,@strucTextLength), 0)  //SE
    //IF liTextAmt = 0  //SE
    //	RETURN FALSE   //SE
    //ENDIF             //SE

    //IF !IsNil(oPrintingDevice)
    //	IF IsString(oPrintingDevice)
    //		cDeviceName := oPrintingDevice
    //	ELSE
    //		cDeviceName := oPrintingDevice:Device
    //	ENDIF
    //	pszDevice := StringAlloc(cDeviceName)
    //	hDC := CreateDC(NULL_PSZ, pszDevice, NULL_PSZ, NULL_PTR)
    //	IF (hDC == NULL_PTR)
    //		MemFree(pszDevice)
    //		RETURN FALSE
    //	ENDIF
    //	MemFree(pszDevice)
    //ELSE
    //	// give user a print dialog box to get hDC
    //	strucPrintDlg:lStructSize := _SIZEOF(_winPrintDlg)
    //	strucPrintDlg:hwndOwner := oCtrl:Handle
    //	strucPrintDlg:hDevMode := NULL_PTR
    //	strucPrintDlg:hDevNames := NULL_PTR
    //	strucPrintDlg:hDC := NULL_PTR
    //	strucPrintDlg:Flags := _OR(PD_RETURNDC, PD_NOPAGENUMS, PD_NOSELECTION, PD_PRINTSETUP)
    //	strucPrintDlg:nFromPage := 0
    //	strucPrintDlg:nToPage := 0
    //	strucPrintDlg:nMinPage := 0
    //	strucPrintDlg:nMaxPage := 0
    //	strucPrintDlg:nCopies := 0
    //	strucPrintDlg:hInstance := NULL_PTR
    //	strucPrintDlg:lCustData := 0
    //	strucPrintDlg:lpfnPrintHook := NULL_PTR
    //	strucPrintDlg:lpfnSetupHook := NULL_PTR
    //	strucPrintDlg:lpPrintTemplateName := NULL_PSZ
    //	strucPrintDlg:lpSetupTemplateName := NULL_PSZ
    //	strucPrintDlg:hPrintTemplate := NULL_PTR
    //	strucPrintDlg:hSetupTemplate := NULL_PTR

    //	IF !__LoadComDlgDLL() .OR. ! LOGIC(_CAST, PCALL(gpfnPrintDlg, @strucPrintDlg))
    //		RETURN FALSE
    //	ENDIF

    //	hDC := strucPrintDlg:hDC
    //ENDIF

    //strucFormatRange:hDC := strucFormatRange:hDCTarget := hDC

    //// area to render to and area of rendering device
    //strucFormatRange:rcPage:left   := 0
    //strucFormatRange:rcPage:top    := 0
    //strucFormatRange:rcPage:right  := MulDiv(GetDeviceCaps(hdc, PHYSICALWIDTH),  1440, GetDeviceCaps(hdc, LOGPIXELSX))
    //strucFormatRange:rcPage:bottom := MulDiv(GetDeviceCaps(hdc, PHYSICALHEIGHT), 1440, GetDeviceCaps(hdc, LOGPIXELSY))

    ////strucFormatRange.rc := strucFormatRange.rcPage // start with full page
    //CopyRect(@strucFormatRange:rc, @strucFormatRange:rcPage)	//SE

    //IF (strucFormatRange:rcPage:right > 2*3*1440/4 + 1440)
    //	strucFormatRange:rc:right  -= (strucFormatRange:rc:left := 3*1440/4)
    //ENDIF
    //IF (strucFormatRange:rcPage:bottom > 3*1440)
    //	strucFormatRange:rc:bottom -= (strucFormatRange:rc:top := 1440)
    //ENDIF

    //// range to print
    //IF IsNil(oRange)
    //	strucFormatRange:chrg:cpMin := 0
    //	strucFormatRange:chrg:cpMax := -1
    //ELSE
    //	strucFormatRange:chrg:cpMin := oRange:Min - 1
    //	strucFormatRange:chrg:cpMax := oRange:Max - 1
    //ENDIF

    //// add null terminator plus 1
    ////liTextAmt += 2 //SE
    //liTextOut := 0
    //lBanding  := (_AND(GetDeviceCaps(hDC, RASTERCAPS), RC_BANDING) = RC_BANDING) // == 0) //SE wrong value, or is the name lBanding wrong ????

    //pszClassName := StringAlloc(Symbol2String(ClassName(SELF)))
    //strucDocInfo:cbSize       := _SIZEOF(_winDocInfo)
    //strucDocInfo:lpszDocName  := pszClassName
    //strucDocInfo:lpszOutput   := NULL_PSZ
    //strucDocInfo:lpszDataType := NULL_PSZ
    //strucDocInfo:fwType       := 0

    //StartDoc(hDC, @strucDocInfo)
    ////RvdH 070717 Save rects. They are changed by the RTF control !
    //CopyRect(@rc1, @strucFormatRange:rc)
    //CopyRect(@rc2, @strucFormatRange:rcPage)
    //GuiWin32.SendMessage(oCtrl:Handle, EM_FORMATRANGE, 0, 0L)
    //SetMapMode(hDC, MM_TEXT)
    //WHILE (liTextOut < liTextAmt)
    //	StartPage(hDC)
    //	//RvdH 070717 Restore rects. They are changed by the RTF control !
    //	CopyRect(@strucFormatRange:rc		, @rc1)
    //	CopyRect(@strucFormatRange:rcPage, @rc2)

    //	liTextPos := liTextOut //SE
    //	IF (! lBanding)
    //		liTextOut := GuiWin32.SendMessage(oCtrl:Handle, EM_FORMATRANGE, 1, LONGINT(_CAST, @strucFormatRange))
    //		IF liTextOut <= liTextPos //SE
    //			liTextOut := liTextAmt //SE
    //		ENDIF //SE
    //	ELSE
    //		liTextOut := GuiWin32.SendMessage(oCtrl:Handle, EM_FORMATRANGE, 0, LONGINT(_CAST, @strucFormatRange))
    //		IF liTextOut > liTextPos //SE
    //			IF (GuiWin32.SendMessage(oCtrl:Handle, EM_DISPLAYBAND, 0, LONGINT(_CAST, @strucFormatRange:rc )) == 0)
    //				//DeleteDC(hDC)
    //				//RETURN FALSE
    //				EXIT //SE otherwise memory of pszClassName becomes not free.
    //			ENDIF
    //		ELSE //SE
    //			liTextOut := liTextAmt //SE
    //		ENDIF //SE
    //	ENDIF

    //	EndPage(hDC)
    //	strucFormatRange:chrg:cpMin := liTextOut
    //	strucFormatRange:chrg:cpMax := -1
    //ENDDO
    ////RvdH 070717 No need for Endpage anymore. Is always done in the Loop
    ////EndPage(hDC)
    //EndDoc(hDC)
    //DeleteDC(hDC)
    //MemFree(pszClassName)

    RETURN TRUE

    /// <include file="Gui.xml" path="doc/RichEdit.Protected/*" />
    PROPERTY Protected  AS LOGIC GET __RichEdit:SelectionProtected SET __RichEdit:SelectionProtected := Value

    /// <include file="Gui.xml" path="doc/RichEdit.RightMargin/*" />
    PROPERTY RightMargin AS LONG GET  __RichEdit:SelectionRightIndent SET __RichEdit:SelectionRightIndent := value

    /// <include file="Gui.xml" path="doc/RichEdit.SaveToFile/*" />
    METHOD SaveToFile(cFileName AS STRING, dwFormat := SF_RTF AS LONG)  AS LOGIC
        LOCAL lOk AS LOGIC
        TRY
            __RichEdit:SaveFile(cFileName, (System.Windows.Forms.RichTextBoxStreamType) dwFormat)
            lOk := TRUE
        CATCH AS System.IO.IOException
            lOk := FALSE
        CATCH e AS ArgumentException
            THROW e
        END TRY
        RETURN lOk

    /// <include file="Gui.xml" path="doc/RichEdit.SecondaryIndent/*" />
    PROPERTY  SecondaryIndent AS LONG GET __RichEdit:SelectionHangingIndent SET __RichEdit:SelectionHangingIndent := value

    /// <include file="Gui.xml" path="doc/RichEdit.Seek/*" />
    METHOD Seek(cText, oRange, lMatchCase, lWholeWord, lReturnRange, lSearchUp) AS USUAL
        LOCAL strucFindTextEx AS winFindTextEx
        LOCAL dwFlags AS DWORD
        LOCAL liRet   AS LONGINT
        strucFindTextEx := winFindTextEx{}

        DEFAULT( REF lMatchCase,   FALSE)
        DEFAULT( REF lWholeWord,   FALSE)
        DEFAULT( REF lReturnRange, FALSE)
        DEFAULT( REF lSearchUp,    FALSE)

        IF lSearchUp
            dwFlags := 0
        ELSE
            dwFlags := FR_DOWN
        ENDIF

        IF lMatchCase
            dwFlags := _OR(dwFlags, FR_MATCHCASE)
        ENDIF

        IF lWholeWord
            dwFlags := _OR(dwFlags, FR_WHOLEWORD)
        ENDIF

        strucFindTextEx:lpstrText  := cText
        strucFindTextEx:chrg_Min := ((Range)oRange):Min - 1
        strucFindTextEx:chrg_Max := ((Range)oRange):Max - 1
        liRet := GuiWin32.SendMessage(oCtrl:Handle, EM_FINDTEXTEXW, dwFlags, LONGINT(_CAST, @strucFindTextEx))

        IF lReturnRange
            IF liRet >= 0l
                RETURN Range{strucFindTextEx:chrgText_Min + 1, strucFindTextEx:chrgText_Max + 1}
            ENDIF
            RETURN Range{0,0}
        ENDIF

        RETURN liRet + 1l

    /// <include file="Gui.xml" path="doc/RichEdit.SelectedText/*" />
    PROPERTY SelectedText as string GET __RichEdit:SelectedText

    /// <include file="Gui.xml" path="doc/RichEdit.Selection/*" />
    PROPERTY Selection  AS Selection
        GET
            LOCAL nStart AS LONG
            nStart := __RichEdit:SelectionStart
            RETURN Selection{nStart, nStart+ __RichEdit:SelectionLength+1}
        END GET
        SET
            __RichEdit:SelectionStart := value:Start
            __RichEdit:SelectionLength := value:Finish - value:Start
        end set
    end property

    /// <include file="Gui.xml" path="doc/RichEdit.SelectionType/*" />
    PROPERTY SelectionType AS LONG GET __RichEdit:SelectionType

    /// <include file="Gui.xml" path="doc/RichEdit.SetOption/*" />
    METHOD SetOption(kOption AS LONG, symOperation := #Add AS Symbol) AS VOID
        LOCAL dwOperation AS DWORD

        DO CASE
        CASE symOperation == #Change
            dwOperation := ECOOP_SET

        CASE symOperation == #Add
            dwOperation := ECOOP_OR

        CASE symOperation == #Keep
            dwOperation := ECOOP_AND

        CASE symOperation == #Remove
            dwOperation := ECOOP_XOR
        END CASE

        GuiWin32.SendMessage(oCtrl:Handle, EM_SETOPTIONS, dwOperation, LONGINT(kOption))

        RETURN

    /// <include file="Gui.xml" path="doc/RichEdit.SetTabStops/*" />
    METHOD SetTabStops(aTabStops AS INT[]) AS VOID
        __RichEdit:SelectionTabs:= aTabStops
        RETURN

    /// <include file="Gui.xml" path="doc/RichEdit.ShowSelection/*" />
    METHOD ShowSelection(lTemporary := TRUE as LOGIC) AS VOID
        GuiWin32.SendMessage(oCtrl:Handle, EM_HIDESELECTION, DWORD(_CAST, FALSE), LONGINT(_CAST, lTemporary))
        RETURN

    /// <include file="Gui.xml" path="doc/RichEdit.TabStopCount/*" />
    PROPERTY TabStopCount AS LONG
        GET
            LOCAL strucParaFormat	IS _winPARAFORMAT
            strucParaFormat:cbSize	:= _SIZEOF(_winPARAFORMAT)
            strucParaFormat:dwMask	:= PFM_TABSTOPS
            GuiWin32.SendMessage(oCtrl:Handle, EM_GETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

            RETURN strucParaFormat:cTabCount
        end get
        set
            LOCAL strucParaFormat IS _winPARAFORMAT
            strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
            strucParaFormat:dwMask := PFM_TABSTOPS
            strucParaFormat:cTabCount := (SHORT) value
            GuiWin32.SendMessage(oCtrl:Handle, EM_SETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))
        end set
    end property

    /// <include file="Gui.xml" path="doc/RichEdit.TextColor/*" />
    PROPERTY TextColor AS Color GET __RichEdit:SelectionColor SET __RichEdit:SelectionColor := value

    /// <include file="Gui.xml" path="doc/RichEdit.TextLimit/*" />
    PROPERTY TextLimit AS LONG GET __RichEdit:MaxLength SET __RichEdit:MaxLength := value

    /// <exclude />
    METHOD __GetValue(dwType AS DWORD) AS STRING
        LOCAL sReturn AS STRING
        IF dwType == SF_RTF
            sReturn := __RichEdit:Rtf
        ELSE
            sReturn := __RichEdit:Text
        ENDIF

        RETURN sReturn

    /// <include file="Gui.xml" path="doc/RichEdit.Value/*" />
    PROPERTY Value AS USUAL GET  __RichEdit:Rtf SET SELF:__Value := value

    /// <include file="Gui.xml" path="doc/RichEdit.ValueAsText/*" />
    PROPERTY ValueAsText AS STRING GET __RichEdit:Text


END CLASS

INTERNAL VOSTRUCT _winPARAFORMAT ALIGN 1
    MEMBER cbSize AS DWORD
MEMBER dwMask AS DWORD
MEMBER wNumbering AS WORD
MEMBER wReserved AS WORD
MEMBER dxStartIndent AS LONGINT
MEMBER dxRightIndent AS LONGINT
MEMBER dxOffset AS LONGINT
MEMBER wAlignment AS WORD
MEMBER cTabCount AS SHORTINT
MEMBER DIM rgxTabs[MAX_TAB_STOPS] AS LONGINT


INTERNAL STRUCTURE winFindTextEx
    PUBLIC chrg_Min	AS LONGINT
    PUBLIC chrg_Max AS LONGINT
    PUBLIC lpstrText AS STRING
    PUBLIC chrgText_Min AS LONGINT
    PUBLIC chrgText_Max AS LONGINT
END STRUCTURE


INTERNAL VOSTRUCT winTextRange
    MEMBER chrg_Min	AS LONGINT
MEMBER chrg_Max AS LONGINT
MEMBER lpstrText AS PSZ
#region defines
DEFINE RICHTAB_CENTER  := 0x01000000
DEFINE RICHTAB_DECIMAL := 0x03000000
DEFINE RICHTAB_NORMAL  := 0x00000000
DEFINE RICHTAB_RIGHT   := 0x02000000
DEFINE RICHTAB_WORDBAR := 0x04000000
DEFINE RICHEDIT_CLASS := "RichEdit20A"
#endregion


