




//#define EM_FINDTEXTW			(WM_USER + 123)
//#define EM_FINDTEXTEXW			(WM_USER + 124)


CLASS RichEdit INHERIT MultiLineEdit
	PROTECT oBackgroundColor AS Color
	PROTECT pStreamPos AS BYTE PTR

    PROPERTY ControlType AS ControlType GET ControlType.RichEdit


    CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 
		

		Win32.LoadLibrary("RICHED20.DLL")

		IF IsNil(kStyle) .OR. !IsLong(kStyle)
			kStyle := _OR(ES_MULTILINE, ES_AUTOHSCROLL, ES_AUTOVSCROLL)
		ENDIF

		SUPER(oOwner, xID, oPoint, oDimension, kStyle)
		SELF:cClassName := RICHEDIT_CLASS

		Win32.SendMessage(oCtrl:Handle, EM_SETEVENTMASK, 0, LONGINT(_CAST, _OR(ENM_CHANGE, ENM_PROTECTED, ENM_SCROLL, ENM_SELCHANGE, ENM_UPDATE)))

		RETURN 




	ACCESS __RichEdit AS VORichTextBox
		RETURN (VORichTextBox) oCtrl

	ACCESS __StreamPos AS PTR STRICT 
		RETURN pStreamPos

	ASSIGN __StreamPos(pNewPos AS PTR)  STRICT 
		pStreamPos := pNewPos
		RETURN 

	ASSIGN __Value(uNewVal AS USUAL)  STRICT 
		LOCAL sVal AS STRING
		sVal := AsString(uNewVal)
		IF	sVal:StartsWith("{\rtf", StringComparison.OrdinalIgnoreCase)
			__RichEdit:Rtf := sVal
		ELSE
			__RichEdit:Text := sVal
		ENDIF
		SUPER:__Value := uNewVal
		RETURN 

	ACCESS Alignment AS LONG
		RETURN __RichEdit:SelectionAlignment

	ASSIGN Alignment(kAlignment AS LONG) 
		__RichEdit:SelectionAlignment := (System.Windows.Forms.HorizontalAlignment) kAlignment
		RETURN 

	ACCESS BackgroundColor AS Color
		RETURN __RichEdit:BackColor

	ASSIGN Background(oBrush AS Brush) 
		LOCAL dwColor AS DWORD
		dwColor:=WC.GetBrushColor(oBrush)
		SELF:BackgroundColor:= Color.FromColorRef(dwColor)
		__RichEdit:BackColor := SELF:BackgroundColor

	ASSIGN BackgroundColor(oColor AS Color) 
		oBackgroundColor := oColor
		__RichEdit:BackColor := oColor
		RETURN 

	METHOD CanPaste(dwClipboardFormat AS LONG) 
		RETURN __RichEdit:CanPaste(System.Windows.Forms.DataFormats.GetFormat(dwClipboardFormat))

	METHOD EnableAdvancedTypography(lEnable) 
		LOCAL dwOption AS DWORD

		IF IsLogic(lEnable) .AND. ! lEnable
			dwOption := TO_SIMPLELINEBREAK
		ELSE
			dwOption := TO_ADVANCEDTYPOGRAPHY
		ENDIF
		Win32.SendMessage(oCtrl:Handle, EM_SETTYPOGRAPHYOPTIONS, dwOption, LONGINT(_CAST, dwOption))
		RETURN SELF

	//ACCESS Font 
		//Todo
	//	RETURN  NULL_OBJECT
	////PP-040322 Update from S Ebert
	//LOCAL oFont AS Font
	//LOCAL strucCharFormat IS _winCHARFORMAT
	//LOCAL wFamily AS WORD

	

	//strucCharFormat:cbSize := _SIZEOF(_winCHARFORMAT)
	//strucCharFormat:dwMask := DWORD(_CAST, _OR(CFM_FACE, CFM_SIZE, CFM_BOLD, CFM_ITALIC, CFM_STRIKEOUT, CFM_UNDERLINE))
	//Win32.SendMessage(oCtrl:Handle, EM_GETCHARFORMAT, DWORD(_CAST, TRUE), LONGINT(_CAST, @strucCharFormat))

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
		//Todo
		//LOCAL strucCharFormat IS _winCHARFORMAT
		//LOCAL cFaceName AS STRING

		//// First retrieve the current effects
		//strucCharFormat:cbSize := _SIZEOF(_winCHARFORMAT)
		//strucCharFormat:dwMask := DWORD(_CAST, _OR(CFM_FACE, CFM_SIZE, CFM_BOLD, CFM_ITALIC, CFM_STRIKEOUT, CFM_UNDERLINE))
		//Win32.SendMessage(oCtrl:Handle, EM_GETCHARFORMAT, DWORD(_CAST, TRUE), LONGINT(_CAST, @strucCharFormat))

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
		//// Win32.SendMessage(oCtrl:Handle, EM_SETCHARFORMAT, _Or(SCF_SELECTION, SCF_WORD), LONG(_CAST, @strucCharFormat))
		//Win32.SendMessage(oCtrl:Handle, EM_SETCHARFORMAT, SCF_SELECTION, LONGINT(_CAST, @strucCharFormat))

		//RETURN 

	METHOD GetOption(kOption) 
		

		// check to see if the specified option is set
		RETURN _AND(LONGINT(kOption), Win32.SendMessage(oCtrl:Handle, EM_SETOPTIONS, 0, 0)) != 0

	METHOD GetTabStops() AS INT[]
		RETURN __RichEdit:SelectionTabs

	METHOD GetTextRange(oRange) 
		LOCAL sRet AS STRING
		LOCAL strucTextRange IS winTextRange

		strucTextRange:chrg_Min := oRange:Min - 1
		strucTextRange:chrg_Max := oRange:Max - 1
		strucTextRange:lpstrText := MemAlloc(oRange:Max - oRange:Min + 1)

		IF (PTR(_CAST, strucTextRange:lpstrText) != NULL_PTR)
			Win32.SendMessage(oCtrl:Handle, EM_GETTEXTRANGE, 0, LONGINT(_CAST, @strucTextRange))
			sRet := Psz2String(strucTextRange:lpstrText)
			MemFree(strucTextRange:lpstrText)
		ENDIF

		RETURN sRet

	METHOD GetWordBreak(nCharPos, kWordBreakType) 
		

		IF kWordBreakType == REGWB_ISDELIMITER
			RETURN LOGIC(_CAST, Win32.SendMessage(oCtrl:Handle, EM_FINDWORDBREAK, kWordBreakType, (LONG) nCharPos - 1))
		ENDIF

		RETURN Win32.SendMessage(oCtrl:Handle, EM_FINDWORDBREAK, kWordBreakType, (LONG) nCharPos - 1)

	METHOD HideSelection(lTemporary) 
		

		Default(@lTemporary, TRUE)

		Win32.SendMessage(oCtrl:Handle, EM_HIDESELECTION, DWORD(_CAST, TRUE), LONGINT(_CAST, lTemporary))

		RETURN NIL

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

	METHOD LineFromCharacter(nCharacterPos AS LONG) 
		RETURN __RichEdit:GetLineFromCharIndex(	nCharacterPos-1)+1


	METHOD LoadFromFile(cFileName AS STRING) 
		RETURN SELF:LoadFromFile(cFileName, SF_RTF)

	METHOD LoadFromFile(cFileName AS STRING, dwFormat AS LONG) 
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


	ACCESS Margins  AS Dimension
		LOCAL nLeft AS LONG
		LOCAL nTop AS LONG
		IF SELF:ValidateControl()
			nLeft := __RichEdit:Margin:Left
			nTop  := __RichEdit:Margin:Top
		ENDIF

		RETURN Dimension{nLeft, nTop}

	ASSIGN Margins(oNewMargins AS Dimension) 
		IF SELF:ValidateControl()
			LOCAL oPadding AS System.Windows.Forms.Padding
			oPadding := __RichEdit:Margin
			IF oPadding:Left != oNewMargins:Width .or. oPadding:Right != oNewMargins:Height
				oPadding:Left := oNewMargins:Width
				oPadding:Right := oNewMargins:Height
				__RichEdit:Margin := oPadding
			ENDIF
		ENDIF

		RETURN 

	METHOD Margin(nStart, nRight, nOffset) 
		LOCAL strucParaFormat IS _winPARAFORMAT

		strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
		strucParaFormat:dwMask := DWORD(_CAST, _OR(PFM_STARTINDENT, PFM_RIGHTINDENT, PFM_OFFSET))
		strucParaFormat:dxStartIndent := nStart
		strucParaFormat:dxRightIndent := nRight
		strucParaFormat:dxOffset := nOffset
		Win32.SendMessage(oCtrl:Handle, EM_SETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

		RETURN 1

	ACCESS Numbering 
		LOCAL strucParaFormat IS _winPARAFORMAT
		strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
		strucParaFormat:dwMask := PFM_NUMBERING
		Win32.SendMessage(oCtrl:Handle, EM_GETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

		RETURN strucParaFormat:wNumbering

	ASSIGN Numbering(kNumbering) 
		LOCAL strucParaFormat IS _winPARAFORMAT
		strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
		strucParaFormat:dwMask := PFM_NUMBERING
		strucParaFormat:wNumbering := kNumbering
		Win32.SendMessage(oCtrl:Handle, EM_SETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

		RETURN 

	METHOD PasteSpecial(dwClipboardFormat) 
		

		Win32.SendMessage(oCtrl:Handle, EM_PASTESPECIAL, dwClipboardFormat, 0)
		RETURN NIL

	ACCESS PrimaryIndent  AS LONG
		RETURN __RichEdit:SelectionIndent


	ASSIGN PrimaryIndent(nIndent AS LONG) 
		__RichEdit:SelectionIndent := nIndent

	METHOD Print(oPrintingDevice, oRange) 
		//Todo
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
		//liTextAmt := Win32.SendMessage(oCtrl:Handle, EM_GETTEXTLENGTHEX, DWORD(_CAST,@strucTextLength), 0)  //SE
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
		//Win32.SendMessage(oCtrl:Handle, EM_FORMATRANGE, 0, 0L)
		//SetMapMode(hDC, MM_TEXT)
		//WHILE (liTextOut < liTextAmt)
		//	StartPage(hDC)                                              
		//	//RvdH 070717 Restore rects. They are changed by the RTF control !
		//	CopyRect(@strucFormatRange:rc		, @rc1)
		//	CopyRect(@strucFormatRange:rcPage, @rc2)

		//	liTextPos := liTextOut //SE
		//	IF (! lBanding)
		//		liTextOut := Win32.SendMessage(oCtrl:Handle, EM_FORMATRANGE, 1, LONGINT(_CAST, @strucFormatRange))
		//		IF liTextOut <= liTextPos //SE
		//			liTextOut := liTextAmt //SE
		//		ENDIF //SE
		//	ELSE
		//		liTextOut := Win32.SendMessage(oCtrl:Handle, EM_FORMATRANGE, 0, LONGINT(_CAST, @strucFormatRange))
		//		IF liTextOut > liTextPos //SE
		//			IF (Win32.SendMessage(oCtrl:Handle, EM_DISPLAYBAND, 0, LONGINT(_CAST, @strucFormatRange:rc )) == 0)
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

	ACCESS @@Protected  AS LOGIC
		RETURN __RichEdit:SelectionProtected

	ASSIGN @@Protected(lEnable AS LOGIC) 
		__RichEdit:SelectionProtected := lEnable
		RETURN 

	ACCESS RightMargin AS LONG
		RETURN __RichEdit:SelectionRightIndent
	
	ASSIGN RightMargin(nRightMargin AS LONG) 
		__RichEdit:SelectionRightIndent := nRightMargin
		RETURN 

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

	ACCESS SecondaryIndent AS LONG
		RETURN __RichEdit:SelectionHangingIndent

	ASSIGN SecondaryIndent(nIndent  AS LONG) 
		__RichEdit:SelectionHangingIndent := nIndent
		RETURN 

	METHOD Seek(cText, oRange, lMatchCase, lWholeWord, lReturnRange, lSearchUp) 
		LOCAL strucFindTextEx AS winFindTextEx
		LOCAL dwFlags AS DWORD
		LOCAL liRet   AS LONGINT
		strucFindTextEx := winFindTextEx{}

		

		Default(@lMatchCase,   FALSE)
		Default(@lWholeWord,   FALSE)
		Default(@lReturnRange, FALSE)
		Default(@lSearchUp,    FALSE)

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
		strucFindTextEx:chrg_Min := oRange:Min - 1
		strucFindTextEx:chrg_Max := oRange:Max - 1
		liRet := Win32.SendMessage(oCtrl:Handle, EM_FINDTEXTEXW, dwFlags, LONGINT(_CAST, @strucFindTextEx))

		IF lReturnRange
			IF liRet >= 0l
				RETURN Range{strucFindTextEx:chrgText_Min + 1, strucFindTextEx:chrgText_Max + 1}
			ENDIF
			RETURN Range{0,0}
		ENDIF

		RETURN liRet + 1l

	ACCESS SelectedText as string
		RETURN __RichEdit:SelectedText

	ACCESS Selection  AS Selection
		LOCAL nStart AS LONG
		nStart := __RichEdit:SelectionStart
		RETURN Selection{nStart, nStart+ __RichEdit:SelectionLength+1}

	ASSIGN Selection(oSelection AS Selection) 
		__RichEdit:SelectionStart := oSelection:Start
		__RichEdit:SelectionLength := oSelection:Finish - oSelection:Start
		RETURN 

	ACCESS SelectionType AS LONG
		RETURN (LONG) __RichEdit:SelectionType

	METHOD SetOption(kOption, symOperation) 
		LOCAL dwOperation AS DWORD

		

		Default(@symOperation, #Add)

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

		Win32.SendMessage(oCtrl:Handle, EM_SETOPTIONS, dwOperation, LONGINT(kOption))

		RETURN NIL

	METHOD SetTabStops(aTabStops AS INT[]) AS VOID
		__RichEdit:SelectionTabs:= aTabStops
		RETURN 

	METHOD ShowSelection(lTemporary) 
		

		Default(@lTemporary, TRUE)

		Win32.SendMessage(oCtrl:Handle, EM_HIDESELECTION, DWORD(_CAST, FALSE), LONGINT(_CAST, lTemporary))

		RETURN NIL

	ACCESS TabStopCount AS LONG
		LOCAL strucParaFormat	IS _winPARAFORMAT
		strucParaFormat:cbSize	:= _SIZEOF(_winPARAFORMAT)
		strucParaFormat:dwMask	:= PFM_TABSTOPS
		Win32.SendMessage(oCtrl:Handle, EM_GETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

		RETURN strucParaFormat:cTabCount
		
	ASSIGN TabStopCount(nTabStops AS LONG) 
		LOCAL strucParaFormat IS _winPARAFORMAT
		strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
		strucParaFormat:dwMask := PFM_TABSTOPS
		strucParaFormat:cTabCount := (SHORT) nTabStops
		Win32.SendMessage(oCtrl:Handle, EM_SETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

		RETURN 

	ACCESS TextColor AS Color
		RETURN (Color) __RichEdit:SelectionColor

	ASSIGN TextColor(oColor  AS Color) 
		__RichEdit:SelectionColor := oColor
		RETURN 

	ASSIGN TextLimit(dwTextLimit AS LONG) 
		__RichEdit:MaxLength := dwTextLimit
		RETURN 

	METHOD __GetValue(dwType AS DWORD) AS STRING
		LOCAL sReturn AS STRING
		IF dwType == SF_RTF
			sReturn := __RichEdit:Rtf
		ELSE
			sReturn := __RichEdit:Text
		ENDIF
			
		RETURN sReturn

	ACCESS Value 
		RETURN __RichEdit:Rtf
	
	ACCESS ValueAsText
		RETURN __RichEdit:Text
	
	ASSIGN Value(uNewValue) 
		SELF:__Value := uNewValue
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
	EXPORT chrg_Min	AS LONGINT
	EXPORT chrg_Max AS LONGINT
	EXPORT lpstrText AS STRING
	EXPORT chrgText_Min AS LONGINT
	EXPORT chrgText_Max AS LONGINT
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
