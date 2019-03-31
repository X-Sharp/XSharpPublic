#ifdef __VULCAN__
   #using System.Runtime.InteropServices
   INTERNAL DELEGATE RichEditCallback( dwCookie AS DWORD, pbBuff AS BYTE PTR, cb AS LONGINT, pcb AS LONGINT PTR ) AS DWORD
#endif

CLASS RichEdit INHERIT MultiLineEdit
	PROTECT oBackgroundColor AS Color
	PROTECT pStreamPos AS BYTE PTR

	//PP-030828 Strong typing
	ACCESS __StreamPos AS PTR STRICT 
	//PP-030828 Strong typing
	RETURN pStreamPos

ASSIGN __StreamPos(pNewPos AS PTR)  STRICT 
	//PP-030828 Strong typing
	pStreamPos := pNewPos

	RETURN 

ASSIGN __Value(uNewVal AS USUAL)  STRICT 
	//PP-030828 Strong typing
	LOCAL pBufStart AS BYTE PTR
	LOCAL strucEditStream IS _WinEDITSTREAM
	LOCAL sVal AS STRING
	LOCAL dwType AS DWORD

	DEFAULT(@uNewVal, NULL_STRING)
	IF !IsString(uNewVal)
		RETURN NULL_STRING
	ENDIF

	sVal := uNewVal
	pBufStart := PTR(_CAST, StringAlloc(sVal))
	pStreamPos := pBufStart
	dwType := IIF((Lower(Left(sVal, 5)) == "{\rtf"), SF_RTF, SF_TEXT)

#ifdef __VULCAN__
   LOCAL gch AS GCHandle
   LOCAL del AS RichEditCallback
   
   gch := GCHandle.Alloc( SELF )
	strucEditStream:dwCookie := (DWORD) GCHandle.ToIntPtr( gch )
	del := RichEditCallback{ NULL, @__SetRTFValue() }
	strucEditStream:pfnCallBack := Marshal.GetFunctionPointerForDelegate( (System.Delegate) del )
#else
	RegisterKid(@strucEditStream:dwCookie, 1, FALSE)
	strucEditStream:dwCookie := DWORD(_CAST, SELF)
	strucEditStream:pfnCallBack := @__SetRTFValue()
#endif	
	strucEditStream:dwError := 0

	SendMessage(SELF:Handle(), EM_STREAMIN, dwType, LONGINT(_CAST, @strucEditStream))

	MemFree(pBufStart)
#ifdef __VULCAN__	
   gch:Free()
   GC.KeepAlive( del )
#else
	UnRegisterKid(@strucEditStream:dwCookie)
#endif	
	pStreamPos := 0
	//RvdH 080212 Make sure uValue is set  (this is used by other methods, such as __Update)
	SUPER:__Value := uNewVal
	RETURN 

ACCESS Alignment 
	LOCAL strucParaFormat IS _winPARAFORMAT

	
	strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
	strucParaFormat:dwMask := PFM_ALIGNMENT
	SendMessage(SELF:Handle(), EM_GETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

	RETURN strucParaFormat:wAlignment

ASSIGN Alignment(kAlignment) 
	LOCAL strucParaFormat IS _winPARAFORMAT

	

	strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
	strucParaFormat:dwMask := PFM_ALIGNMENT
	strucParaFormat:wAlignment := kAlignment
	SendMessage(SELF:Handle(), EM_SETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

	RETURN 

ACCESS BackgroundColor 
	RETURN oBackgroundColor

ASSIGN Background(oBrush) 
	LOCAL dwColor AS DWORD
	dwColor:=__WCGetBrushColor(oBrush)
	SELF:BackgroundColor:=Color{dwColor,-1}


ASSIGN BackgroundColor(oColor) 

	oBackgroundColor := oColor
	SendMessage(SELF:Handle(), EM_SETBKGNDCOLOR, DWORD(_CAST, FALSE), oColor:ColorRef)

	RETURN 

METHOD CanPaste(dwClipboardFormat) 
	LOCAL dwFormat AS DWORD
	

	// EM_CANPASTE can be used to test for specific clipboard formats,
	// using the wParam to specify the format. If 0 is specified, it
	// tries any format currently on the clipboard.

	DEFAULT(@dwClipboardFormat, 0)
	dwFormat := dwClipboardFormat

	RETURN LOGIC(_CAST, SendMessage(SELF:Handle(), EM_CANPASTE, dwFormat, 0))

METHOD EnableAdvancedTypography(lEnable) 
	//PP-030910 from S Ebert
	LOCAL dwOption AS DWORD

	IF IsLogic(lEnable) .AND. ! lEnable
		dwOption := TO_SIMPLELINEBREAK
	ELSE
		dwOption := TO_ADVANCEDTYPOGRAPHY
	ENDIF
	SendMessage(SELF:Handle(), EM_SETTYPOGRAPHYOPTIONS, dwOption, LONGINT(_CAST, dwOption))
	RETURN SELF

ACCESS ControlFont 
	//PP-040322 Update from S Ebert
	LOCAL oFont AS Font
	LOCAL strucCharFormat IS _winCHARFORMAT
	LOCAL wFamily AS WORD

	

	strucCharFormat:cbSize := _SIZEOF(_winCHARFORMAT)
	strucCharFormat:dwMask := DWORD(_CAST, _OR(CFM_FACE, CFM_SIZE, CFM_BOLD, CFM_ITALIC, CFM_STRIKEOUT, CFM_UNDERLINE))
	SendMessage(SELF:Handle(), EM_GETCHARFORMAT, DWORD(_CAST, TRUE), LONGINT(_CAST, @strucCharFormat))

	DO CASE
	CASE _AND(strucCharFormat:bPitchAndFamily, FF_DECORATIVE) == 1
		wFamily := FONTDECORATIVE
	CASE _AND(strucCharFormat:bPitchAndFamily, FF_MODERN) == 1
		wFamily := FONTMODERN
	CASE _AND(strucCharFormat:bPitchAndFamily, FF_ROMAN) == 1
		wFamily := FONTROMAN
	CASE _AND(strucCharFormat:bPitchAndFamily, FF_SCRIPT) == 1
		wFamily := FONTSCRIPT
	CASE _AND(strucCharFormat:bPitchAndFamily, FF_SWISS) == 1
		wFamily := FONTSWISS
	OTHERWISE
		wFamily := FONTANY
	ENDCASE

	oFont := Font{wFamily, (strucCharFormat:yHeight / TWIPSCONVERSION), Psz2String(@strucCharFormat:szFaceName[1])}

	IF _AND(strucCharFormat:dwEffects, CFE_BOLD) != 0
		oFont:Bold := TRUE
	ENDIF
	IF _AND(strucCharFormat:dwEffects, CFE_ITALIC) != 0
		oFont:Italic := TRUE
	ENDIF
	IF _AND(strucCharFormat:dwEffects, CFE_UNDERLINE) != 0
		oFont:Underline := TRUE
	ENDIF
	IF _AND(strucCharFormat:dwEffects, CFE_STRIKEOUT) != 0
		oFont:Strikethru := TRUE
	ENDIF

	RETURN oFont

ASSIGN ControlFont (oNewFont) 
	LOCAL strucCharFormat IS _winCHARFORMAT
	LOCAL cFaceName AS STRING

	// First retrieve the current effects
	strucCharFormat:cbSize := _SIZEOF(_winCHARFORMAT)
	strucCharFormat:dwMask := DWORD(_CAST, _OR(CFM_FACE, CFM_SIZE, CFM_BOLD, CFM_ITALIC, CFM_STRIKEOUT, CFM_UNDERLINE))
	SendMessage(SELF:Handle(), EM_GETCHARFORMAT, DWORD(_CAST, TRUE), LONGINT(_CAST, @strucCharFormat))

	IF (_AND(strucCharFormat:dwEffects, CFE_BOLD) != 0 .AND. !oNewFont:Bold) .OR. ;
		(_AND(strucCharFormat:dwEffects, CFE_BOLD) == 0 .AND. oNewFont:Bold)
		strucCharFormat:dwEffects := _XOR(strucCharFormat:dwEffects, CFE_BOLD)
	ENDIF
	IF (_AND(strucCharFormat:dwEffects, CFE_ITALIC) != 0 .AND. !oNewFont:Italic) .OR. ;
		(_AND(strucCharFormat:dwEffects, CFE_ITALIC) == 0 .AND. oNewFont:Italic)
		strucCharFormat:dwEffects := _XOR(strucCharFormat:dwEffects, CFE_ITALIC)
	ENDIF
	IF (_AND(strucCharFormat:dwEffects, CFE_STRIKEOUT) != 0 .AND. !oNewFont:Strikethru) .OR. ;
		(_AND(strucCharFormat:dwEffects, CFE_STRIKEOUT) == 0 .AND. oNewFont:Strikethru)
		strucCharFormat:dwEffects := _XOR(strucCharFormat:dwEffects, CFE_STRIKEOUT)
	ENDIF
	IF (_AND(strucCharFormat:dwEffects, CFE_UNDERLINE) != 0 .AND. !oNewFont:Underline) .OR. ;
		(_AND(strucCharFormat:dwEffects, CFE_UNDERLINE) == 0 .AND. oNewFont:Underline)
		strucCharFormat:dwEffects := _XOR(strucCharFormat:dwEffects, CFE_UNDERLINE)
	ENDIF

	// Set the values
	strucCharFormat:bCharSet := oNewFont:__FontCharSet
	strucCharFormat:bPitchAndFamily := oNewFont:__FontPitchAndFamily
	strucCharFormat:yHeight := oNewFont:__PointSize * TWIPSCONVERSION
	cFaceName := oNewFont:__FontFaceName
	IF (NULL_STRING != cFaceName)
		MemCopy(@strucCharFormat:szFaceName[1], String2Psz(cFaceName), SLen(cFaceName)+1)
	ELSE
		MemClear(@strucCharFormat:szFaceName[1], LF_FACESIZE)
	END
	// SendMessage(SELF:Handle(), EM_SETCHARFORMAT, _Or(SCF_SELECTION, SCF_WORD), LONG(_CAST, @strucCharFormat))
	SendMessage(SELF:Handle(), EM_SETCHARFORMAT, SCF_SELECTION, LONGINT(_CAST, @strucCharFormat))

	RETURN 

METHOD GetOption(kOption) 
	

	// check to see if the specified option is set
	RETURN _AND(LONGINT(kOption), SendMessage(SELF:Handle(), EM_SETOPTIONS, 0, 0)) != 0

METHOD GetTabStops() 
	//PP-030910 from S Ebert
	LOCAL strucParaFormat IS _winPARAFORMAT
	LOCAL dwTabCount AS DWORD
	LOCAL dwTabValue AS DWORD
	LOCAL dwTabType  AS DWORD
	LOCAL dwI        AS DWORD
	LOCAL fTabValue  AS FLOAT
	LOCAL aTabStops  AS ARRAY

	strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
	strucParaFormat:dwMask := PFM_TABSTOPS
	SendMessage(SELF:Handle(), EM_SETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

	dwTabCount := strucParaFormat:cTabCount
	IF dwTabCount > 0
		aTabStops := ArrayCreate(dwTabCount)

		FOR dwI := 1 UPTO dwTabCount
			dwTabValue := DWORD(strucParaFormat:rgxTabs[dwI])
			dwTabType  := _AND(dwTabValue, DWORD(_CAST,0x0F000000))
			dwTabValue := _AND(dwTabValue, DWORD(_CAST,0x00FFFFFF))
			fTabValue  := FLOAT(dwTabValue) * 2.54 / 1440.
			IF dwTabType = RICHTAB_NORMAL
				aTabStops[dwI] := fTabValue
			ELSE
				aTabStops[dwI] := {fTabValue, dwTabType}
			ENDIF
		NEXT  // dwI
	ENDIF

	RETURN aTabStops

METHOD GetTextRange(oRange) 
	LOCAL strucTextRange IS _winTextRange
	LOCAL sRet AS STRING

	

	strucTextRange:chrg:cpMin := oRange:Min - 1
	strucTextRange:chrg:cpMax := oRange:Max - 1
	strucTextRange:lpstrText := MemAlloc(oRange:Max - oRange:Min + 1)

	IF (PTR(_CAST, strucTextRange:lpstrText) != NULL_PTR)
		SendMessage(SELF:Handle(), EM_GETTEXTRANGE, 0, LONGINT(_CAST, @strucTextRange))
		sRet := Psz2String(strucTextRange:lpstrText)
		MemFree(strucTextRange:lpstrText)
	ENDIF

	RETURN sRet

METHOD GetWordBreak(nCharPos, kWordBreakType) 
	

	IF kWordBreakType == REGWB_ISDELIMITER
		RETURN LOGIC(_CAST, SendMessage(SELF:Handle(), EM_FINDWORDBREAK, kWordBreakType, nCharPos - 1))
	ENDIF

	RETURN SendMessage(SELF:Handle(), EM_FINDWORDBREAK, kWordBreakType, nCharPos - 1)

METHOD HideSelection(lTemporary) 
	

	DEFAULT(@lTemporary, TRUE)

	SendMessage(SELF:Handle(), EM_HIDESELECTION, DWORD(_CAST, TRUE), LONGINT(_CAST, lTemporary))

	RETURN NIL

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 
	//PP-040508 Update S.Ebert
	

	LoadLibrary(PSZ(_CAST, "RICHED20.DLL"))

	IF IsNil(kStyle) .OR. !IsLong(kStyle)
		kStyle := _OR(ES_MULTILINE, ES_AUTOHSCROLL, ES_AUTOVSCROLL)
	ENDIF

	SUPER(oOwner, xID, oPoint, oDimension, kStyle)
	SELF:__ClassName := RICHEDIT_CLASS

	SendMessage(SELF:Handle(), EM_SETEVENTMASK, 0, LONGINT(_CAST, _OR(ENM_CHANGE, ENM_PROTECTED, ENM_SCROLL, ENM_SELCHANGE, ENM_UPDATE)))

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

METHOD LineFromCharacter(nCharacterPos) 
	

	RETURN SendMessage(SELF:Handle(), EM_EXLINEFROMCHAR, 0, nCharacterPos - 1) + 1

METHOD LoadFromFile(cFileName, dwFormat) 
	LOCAL es IS _WINEDITSTREAM
	LOCAL hFile AS PTR
	LOCAL dwFmt AS DWORD

	IF PCount() > 1     // dcaton was ArgCount() but ArgCount doesn't make any sense
		DEFAULT(@dwFormat, SF_RTF) // tk new
		dwFmt := dwFormat
	ELSE
		dwFmt := SF_RTF
	ENDIF

	hFile := FOpen(cFileName, FO_READ)

	IF hFile != F_ERROR
		es:dwCookie := DWORD(_CAST, hFile)
		es:dwError := 0
		
#ifdef __VULCAN__
      LOCAL del AS RichEditCallback
	   del := RichEditCallback{ NULL, @__LoadCallBack() }
      es:pfnCallback := Marshal.GetFunctionPointerForDelegate( (System.Delegate) del )
#else
		es:pfnCallback := @__LoadCallback()
#endif	

		SendMessage(SELF:handle(), EM_STREAMIN, dwFmt, LONGINT(_CAST, @es)) // tk changed

#ifdef __VULCAN__
      GC.KeepAlive( del )
#endif

		FClose(hFile)

		RETURN TRUE
	ENDIF

	RETURN FALSE

METHOD Margin(nStart, nRight, nOffset) 
	LOCAL strucParaFormat IS _winPARAFORMAT

	

	strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
	strucParaFormat:dwMask := DWORD(_CAST, _OR(PFM_STARTINDENT, PFM_RIGHTINDENT, PFM_OFFSET))
	strucParaFormat:dxStartIndent := nStart
	strucParaFormat:dxRightIndent := nRight
	strucParaFormat:dxOffset := nOffset
	SendMessage(SELF:Handle(), EM_SETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

	RETURN 1

ACCESS Numbering 
	LOCAL strucParaFormat IS _winPARAFORMAT

	

	strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
	strucParaFormat:dwMask := PFM_NUMBERING
	SendMessage(SELF:Handle(), EM_GETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

	RETURN strucParaFormat:wNumbering

ASSIGN Numbering(kNumbering) 
	LOCAL strucParaFormat IS _winPARAFORMAT

	

	strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
	strucParaFormat:dwMask := PFM_NUMBERING
	strucParaFormat:wNumbering := kNumbering
	SendMessage(SELF:Handle(), EM_SETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

	RETURN 

METHOD PasteSpecial(dwClipboardFormat) 
	

	SendMessage(SELF:Handle(), EM_PASTESPECIAL, dwClipboardFormat, 0)
	RETURN NIL

ACCESS PrimaryIndent 
	LOCAL strucParaFormat IS _winPARAFORMAT

	

	strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
	strucParaFormat:dwMask := PFM_STARTINDENT
	SendMessage(SELF:Handle(), EM_GETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

	RETURN strucParaFormat:dxStartIndent


ASSIGN PrimaryIndent(nIndent) 
	LOCAL strucParaFormat IS _winPARAFORMAT

	

	strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
	strucParaFormat:dwMask := PFM_STARTINDENT
	strucParaFormat:dxStartIndent := nIndent
	SendMessage(SELF:Handle(), EM_SETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

	RETURN 

METHOD Print(oPrintingDevice, oRange) 
	//PP-040322 Update from S Ebert
	LOCAL strucFormatRange IS _winFormatRange
	LOCAL strucPrintDlg IS _winPrintDlg
	LOCAL strucDocInfo IS _winDocInfo
	LOCAL liTextOut AS LONGINT
	LOCAL liTextPos AS LONGINT //SE
	LOCAL liTextAmt AS LONGINT
	LOCAL hDC AS PTR
	LOCAL cDeviceName AS STRING
	LOCAL pszDevice AS PSZ
	LOCAL pszClassName AS PSZ
	LOCAL lBanding AS LOGIC 
	LOCAL rc1 IS _WINRECT
	LOCAL rc2 IS _WINRECT      
	LOCAL DIM strucTextLength[2] AS DWORD

	//RvdH 070717 Changed text length calculatio to 'precize'
	strucTextLength[1] :=   2| 8	// GTL_PRECISE | GTL_NUMCHARS
	strucTextLength[2] := CP_ACP
	liTextAmt := SendMessage(SELF:Handle(), EM_GETTEXTLENGTHEX, DWORD(_CAST,@strucTextLength), 0)  //SE
	IF liTextAmt = 0  //SE
		RETURN FALSE   //SE
	ENDIF             //SE

	IF !IsNil(oPrintingDevice)
		IF IsString(oPrintingDevice)
			cDeviceName := oPrintingDevice
		ELSE
			cDeviceName := oPrintingDevice:Device
		ENDIF
		pszDevice := StringAlloc(cDeviceName)
		hDC := CreateDC(NULL_PSZ, pszDevice, NULL_PSZ, NULL_PTR)
		IF (hDC == NULL_PTR)
			MemFree(pszDevice)
			RETURN FALSE
		ENDIF
		MemFree(pszDevice)
	ELSE
		// give user a print dialog box to get hDC
		strucPrintDlg:lStructSize := _SIZEOF(_winPrintDlg)
		strucPrintDlg:hwndOwner := SELF:Handle()
		strucPrintDlg:hDevMode := NULL_PTR
		strucPrintDlg:hDevNames := NULL_PTR
		strucPrintDlg:hDC := NULL_PTR
		strucPrintDlg:Flags := _OR(PD_RETURNDC, PD_NOPAGENUMS, PD_NOSELECTION, PD_PRINTSETUP)
		strucPrintDlg:nFromPage := 0
		strucPrintDlg:nToPage := 0
		strucPrintDlg:nMinPage := 0
		strucPrintDlg:nMaxPage := 0
		strucPrintDlg:nCopies := 0
		strucPrintDlg:hInstance := NULL_PTR
		strucPrintDlg:lCustData := 0
		strucPrintDlg:lpfnPrintHook := NULL_PTR
		strucPrintDlg:lpfnSetupHook := NULL_PTR
		strucPrintDlg:lpPrintTemplateName := NULL_PSZ
		strucPrintDlg:lpSetupTemplateName := NULL_PSZ
		strucPrintDlg:hPrintTemplate := NULL_PTR
		strucPrintDlg:hSetupTemplate := NULL_PTR

		IF !__LoadComDlgDLL() .OR. ! LOGIC(_CAST, PCALL(gpfnPrintDlg, @strucPrintDlg))
			RETURN FALSE
		ENDIF

		hDC := strucPrintDlg:hDC
	ENDIF

	strucFormatRange:hDC := strucFormatRange:hDCTarget := hDC

	// area to render to and area of rendering device
	strucFormatRange:rcPage:left   := 0
	strucFormatRange:rcPage:top    := 0
	strucFormatRange:rcPage:right  := MulDiv(GetDeviceCaps(hdc, PHYSICALWIDTH),  1440, GetDeviceCaps(hdc, LOGPIXELSX))
	strucFormatRange:rcPage:bottom := MulDiv(GetDeviceCaps(hdc, PHYSICALHEIGHT), 1440, GetDeviceCaps(hdc, LOGPIXELSY))

	//strucFormatRange.rc := strucFormatRange.rcPage // start with full page
	CopyRect(@strucFormatRange:rc, @strucFormatRange:rcPage)	//SE

	IF (strucFormatRange:rcPage:right > 2*3*1440/4 + 1440)
		strucFormatRange:rc:right  -= (strucFormatRange:rc:left := 3*1440/4)
	ENDIF
	IF (strucFormatRange:rcPage:bottom > 3*1440)
		strucFormatRange:rc:bottom -= (strucFormatRange:rc:top := 1440)
	ENDIF

	// range to print
	IF IsNil(oRange)
		strucFormatRange:chrg:cpMin := 0
		strucFormatRange:chrg:cpMax := -1
	ELSE
		strucFormatRange:chrg:cpMin := oRange:Min - 1
		strucFormatRange:chrg:cpMax := oRange:Max - 1
	ENDIF

	// add null terminator plus 1
	//liTextAmt += 2 //SE
	liTextOut := 0
	lBanding  := (_AND(GetDeviceCaps(hDC, RASTERCAPS), RC_BANDING) = RC_BANDING) // == 0) //SE wrong value, or is the name lBanding wrong ????

	pszClassName := StringAlloc(Symbol2String(ClassName(SELF)))
	strucDocInfo:cbSize       := _SIZEOF(_winDocInfo)
	strucDocInfo:lpszDocName  := pszClassName
	strucDocInfo:lpszOutput   := NULL_PSZ
	strucDocInfo:lpszDataType := NULL_PSZ
	strucDocInfo:fwType       := 0

	StartDoc(hDC, @strucDocInfo)
	//RvdH 070717 Save rects. They are changed by the RTF control !
	CopyRect(@rc1, @strucFormatRange:rc)
	CopyRect(@rc2, @strucFormatRange:rcPage)
	SendMessage(SELF:Handle(), EM_FORMATRANGE, 0, 0L)
	SetMapMode(hDC, MM_TEXT)
	WHILE (liTextOut < liTextAmt)
	   StartPage(hDC)                                              
	   //RvdH 070717 Restore rects. They are changed by the RTF control !
		CopyRect(@strucFormatRange:rc		, @rc1)
		CopyRect(@strucFormatRange:rcPage, @rc2)

		liTextPos := liTextOut //SE
		IF (! lBanding)
			liTextOut := SendMessage(SELF:Handle(), EM_FORMATRANGE, 1, LONGINT(_CAST, @strucFormatRange))
			IF liTextOut <= liTextPos //SE
				liTextOut := liTextAmt //SE
			ENDIF //SE
		ELSE
			liTextOut := SendMessage(SELF:Handle(), EM_FORMATRANGE, 0, LONGINT(_CAST, @strucFormatRange))
			IF liTextOut > liTextPos //SE
				IF (SendMessage(SELF:Handle(), EM_DISPLAYBAND, 0, LONGINT(_CAST, @strucFormatRange:rc )) == 0)
					//DeleteDC(hDC)
					//RETURN FALSE
					EXIT //SE otherwise memory of pszClassName becomes not free.
				ENDIF
			ELSE //SE
				liTextOut := liTextAmt //SE
			ENDIF //SE
		ENDIF

		EndPage(hDC)
		strucFormatRange:chrg:cpMin := liTextOut
		strucFormatRange:chrg:cpMax := -1
	ENDDO
	//RvdH 070717 No need for Endpage anymore. Is always done in the Loop
	//EndPage(hDC)
	EndDoc(hDC)
	DeleteDC(hDC)
	MemFree(pszClassName)

	RETURN TRUE

ACCESS @@Protected 
	LOCAL strucCharFormat IS _winCHARFORMAT

	

	strucCharFormat:cbSize := _SIZEOF(_winCHARFORMAT)
	strucCharFormat:dwMask := CFM_PROTECTED
	SendMessage(SELF:Handle(), EM_GETCHARFORMAT, DWORD(_CAST, TRUE), LONGINT(_CAST, @strucCharFormat))

	RETURN _AND(strucCharFormat:dwEffects, CFE_PROTECTED) != 0

ASSIGN @@Protected(lEnable) 
	LOCAL strucCharFormat IS _winCHARFORMAT

	strucCharFormat:cbSize := _SIZEOF(_winCHARFORMAT)
	strucCharFormat:dwMask := CFM_PROTECTED
	IF lEnable
		strucCharFormat:dwEffects := CFE_PROTECTED
	ELSE
		strucCharFormat:dwEffects := _NOT(CFE_PROTECTED)
	ENDIF
	SendMessage(SELF:Handle(), EM_SETCHARFORMAT, _OR(SCF_SELECTION, SCF_WORD), LONGINT(_CAST, @strucCharFormat))

	RETURN 

ACCESS RightMargin 
	LOCAL strucParaFormat IS _winPARAFORMAT

	

	strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
	strucParaFormat:dwMask := PFM_RIGHTINDENT
	SendMessage(SELF:Handle(), EM_GETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

	RETURN strucParaFormat:dxRightIndent

ASSIGN RightMargin(nRightMargin) 
	LOCAL strucParaFormat IS _winPARAFORMAT

	

	strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
	strucParaFormat:dwMask := PFM_RIGHTINDENT
	strucParaFormat:dxRightIndent := nRightMargin
	SendMessage(SELF:Handle(), EM_SETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

	RETURN 

METHOD SaveToFile(cFileName, dwFormat) 
	LOCAL hFile AS PTR
	LOCAL es IS _WINEDITSTREAM
	LOCAL dwFmt AS DWORD

	//RvdH 071203 Fixed parameter testing
	IF IsNumeric(dwFormat)
		dwFmt := dwFormat
	ELSE
		dwFmt := SF_RTF
	ENDIF
// 	IF PCount() > 1
// 		Default(@dwFormat, SF_RTF) // tk new
// 		dwFmt := dwFormat
// 	ELSE
// 		dwFmt := SF_RTF
// 	ENDIF

	hFile := FCreate(cFileName, FC_NORMAL)

	IF (hFile != F_ERROR)
		es:dwCookie := DWORD(_CAST, hFile)
		es:dwError := 0
#ifdef __VULCAN__
      LOCAL del AS RichEditCallback
	   del := RichEditCallback{ NULL, @__SaveCallBack() }
      es:pfnCallback := Marshal.GetFunctionPointerForDelegate( (System.Delegate) del )
#else
		es:pfnCallback := @__SaveCallBack()
#endif	

		SendMessage(SELF:handle(), EM_STREAMOUT, dwFmt, LONGINT(_CAST, @es)) // tk changed

#ifdef __VULCAN__
      GC.KeepAlive( del )
#endif

		FClose(hFile)

		RETURN TRUE
	ENDIF

	RETURN FALSE

ACCESS SecondaryIndent 
	LOCAL strucParaFormat IS _winPARAFORMAT

	

	strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
	strucParaFormat:dwMask := PFM_OFFSET
	SendMessage(SELF:Handle(), EM_GETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

	RETURN strucParaFormat:dxOffset

ASSIGN SecondaryIndent(nIndent) 
	LOCAL strucParaFormat IS _winPARAFORMAT

	

	strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
	strucParaFormat:dwMask := PFM_OFFSET
	strucParaFormat:dxOffset := nIndent
	SendMessage(SELF:Handle(), EM_SETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

	RETURN 

METHOD Seek(cText, oRange, lMatchCase, lWholeWord, lReturnRange, lSearchUp) 
	//SE-060522
	LOCAL strucFindTextEx IS _winFindTextEx
	LOCAL pszText AS PSZ
	LOCAL dwFlags AS DWORD
	LOCAL liRet   AS LONGINT

	

   DEFAULT(@lMatchCase,   FALSE)
	DEFAULT(@lWholeWord,   FALSE)
	DEFAULT(@lReturnRange, FALSE)
	DEFAULT(@lSearchUp,    FALSE)

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

	pszText := StringAlloc(cText)
	strucFindTextEx:lpstrText  := pszText
	strucFindTextEx:chrg:cpMin := oRange:Min - 1
	strucFindTextEx:chrg:cpMax := oRange:Max - 1
	liRet := SendMessage(SELF:Handle(), EM_FINDTEXTEX, dwFlags, LONGINT(_CAST, @strucFindTextEx))
	MemFree(pszText)

	IF lReturnRange
		IF liRet >= 0l
			RETURN Range{strucFindTextEx:chrgText:cpMin + 1, strucFindTextEx:chrgText:cpMax + 1}
		ENDIF
	   RETURN Range{0,0}
	ENDIF

   RETURN liRet + 1l

ACCESS SelectedText 
	//PP-040508 Update S.Ebert
	LOCAL strucCharRange IS _winCharRange
	LOCAL liLength       AS LONGINT
	LOCAL pszText        AS PSZ
	LOCAL cText          AS STRING

	

	SendMessage(SELF:Handle(), EM_EXGETSEL, 0, LONGINT(_CAST, @strucCharRange))
	liLength := strucCharRange:cpMax - strucCharRange:cpMin
	IF liLength > 0
		pszText  := MemAlloc(DWORD(liLength+1))
		IF pszText != NULL_PSZ
			liLength := SendMessage(SELF:Handle(), EM_GETSELTEXT, 0, LONGINT(_CAST, pszText))
			IF liLength > 0
				cText := Psz2String(pszText)
			ENDIF
			MemFree(pszText)
		ENDIF
	ENDIF

	RETURN cText

	/*

	//The old version allocates to much memory in most cases

	dwLength := SendMessage(SELF:Handle(), WM_GETTEXTLENGTH, 0, 0)
	pszText := MemAlloc(dwLength+1)
	dwLength := SendMessage(SELF:Handle(), EM_GETSELTEXT, 0, LONG(_CAST, pszText))
	IF (dwLength > 0)
		cText := Psz2String(pszText)
	ENDIF
	MemFree(pszText)

	RETURN cText
	*/

ACCESS Selection 
	LOCAL strucCharRange IS _winCharRange

	

	SendMessage(SELF:Handle(), EM_EXGETSEL, 0, LONGINT(_CAST, @strucCharRange))
	RETURN Selection{strucCharRange:cpMin + 1, strucCharRange:cpMax + 1}

ASSIGN Selection(oSelection) 
	LOCAL strucCharRange IS _winCharRange

	strucCharRange:cpMin := oSelection:Start - 1
	strucCharRange:cpMax := oSelection:Finish - 1
	SendMessage(SELF:Handle(), EM_EXSETSEL, 0, LONGINT(_CAST, @strucCharRange))

	RETURN 

ACCESS SelectionType 
	

	RETURN SendMessage(SELF:Handle(), EM_SELECTIONTYPE, 0, 0)

METHOD SetOption(kOption, symOperation) 
	LOCAL dwOperation AS DWORD

	

	DEFAULT(@symOperation, #Add)

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

	SendMessage(SELF:Handle(), EM_SETOPTIONS, dwOperation, LONGINT(kOption))

	RETURN NIL

METHOD SetTabStops(aTabStops) 
	//PP-030910 from S Ebert
	LOCAL strucParaFormat IS _winPARAFORMAT
	LOCAL dwTabCount AS DWORD
	LOCAL dwTabValue AS DWORD
	LOCAL dwTabType  AS DWORD
	LOCAL dwI        AS DWORD
	LOCAL fTabValue  AS FLOAT
	LOCAL nTabStop   AS LONG
	LOCAL dwCount    AS LONG

	IF IsNumeric(aTabStops)
		// It should be a nummer from which the TabStops should be calculated
		// Sometimes a different TabStop value must be specified to assure that
		// the text stays properly aligned with different font sizes
		nTabStop := aTabStops
		aTabStops := {}
		FOR dwCount:=1 UPTO MAX_TAB_STOPS
			AAdd(aTabStops,{dwCount*nTabStop,RICHTAB_NORMAL})
		NEXT
	ENDIF

	IF IsArray(aTabStops)
		dwTabCount := Min(ALen(aTabStops), 32)
		strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
		strucParaFormat:dwMask := PFM_TABSTOPS
		strucParaFormat:cTabCount := SHORTINT(dwTabCount)

		FOR dwI := 1 UPTO dwTabCount
			IF IsArray(aTabStops[dwI])
				fTabValue := aTabStops[dwI,1]
				dwTabType := aTabStops[dwI,2]
			ELSE
				fTabValue := aTabStops[dwI]
				dwTabType := RICHTAB_NORMAL
			ENDIF
			dwTabValue := DWORD(FLOAT(Abs(fTabValue))*1440./2.54) // conversion from cm into twips
			dwTabValue := _OR(dwTabValue, dwTabType)
			strucParaFormat:rgxTabs[dwI] := LONGINT(_CAST, dwTabValue)
		NEXT  // dwI
		SendMessage(SELF:Handle(), EM_SETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))
	ENDIF
	RETURN SELF

METHOD ShowSelection(lTemporary) 
	

	DEFAULT(@lTemporary, TRUE)

	SendMessage(SELF:Handle(), EM_HIDESELECTION, DWORD(_CAST, FALSE), LONGINT(_CAST, lTemporary))

	RETURN NIL

ACCESS TabStopCount 
	LOCAL strucParaFormat IS _winPARAFORMAT

	

	strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
	strucParaFormat:dwMask := PFM_TABSTOPS
	SendMessage(SELF:Handle(), EM_GETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

	RETURN strucParaFormat:cTabCount

ASSIGN TabStopCount(nTabStops) 
	LOCAL strucParaFormat IS _winPARAFORMAT

	

	strucParaFormat:cbSize := _SIZEOF(_winPARAFORMAT)
	strucParaFormat:dwMask := PFM_TABSTOPS
	strucParaFormat:cTabCount := nTabStops
	SendMessage(SELF:Handle(), EM_SETPARAFORMAT, 0, LONGINT(_CAST, @strucParaFormat))

	RETURN 

ACCESS TextColor 
	LOCAL strucCharFormat IS _winCHARFORMAT
	LOCAL oColor AS Color

	

	strucCharFormat:cbSize := _SIZEOF(_winCHARFORMAT)
	strucCharFormat:dwMask := CFM_COLOR
	SendMessage(SELF:Handle(), EM_GETCHARFORMAT, DWORD(_CAST, TRUE), LONGINT(_CAST, @strucCharFormat))

	//PP-031001: Fix color assignment
	oColor := Color{}
	oColor:ColorRef := strucCharFormat:crTextColor

	RETURN oColor

ASSIGN TextColor(oColor) 
	LOCAL strucCharFormat IS _winCHARFORMAT
	strucCharFormat:cbSize := _SIZEOF(_winCHARFORMAT)
	strucCharFormat:dwMask := CFM_COLOR
	strucCharFormat:crTextColor := oColor:ColorRef
	SendMessage(SELF:Handle(), EM_SETCHARFORMAT, _OR(SCF_SELECTION, SCF_WORD), LONGINT(_CAST, @strucCharFormat))

	RETURN 

ASSIGN TextLimit(dwTextLimit) 
	

	SendMessage(SELF:Handle(), EM_EXLIMITTEXT, 0, dwTextLimit)
	RETURN 

METHOD __GetValue(dwType AS DWORD) AS STRING
	LOCAL pBufStart AS BYTE PTR
	LOCAL strucEditStream IS _winEDITSTREAM
	LOCAL liSize AS LONGINT
	LOCAL sReturn AS STRING

	liSize := 4 * SendMessage(SELF:Handle(), EM_GETLIMITTEXT, 0, 0)

	IF (liSize == 0)
		RETURN NULL_STRING
	ENDIF

	pBufStart := MemAlloc(DWORD(liSize))
	MemSet(pBufStart, 0, DWORD(liSize))
	pStreamPos := pBufStart

	IF (pStreamPos == NULL_PTR)
		RETURN NULL_STRING
	ENDIF

#ifdef __VULCAN__
   LOCAL gch AS System.Runtime.InteropServices.GCHandle
   LOCAL del AS RichEditCallback
   
   gch := GCHandle.Alloc( SELF )
	strucEditStream:dwCookie := (DWORD) GCHandle.ToIntPtr( gch )
	del := RichEditCallback{ NULL, @__GetRTFValue() }
	strucEditStream:pfnCallBack := Marshal.GetFunctionPointerForDelegate( (System.Delegate) del )
#else
	RegisterKid(@strucEditStream:dwCookie, 1, FALSE)
	strucEditStream:dwCookie := DWORD(_CAST, SELF)
	strucEditStream:pfnCallBack := @__GetRTFValue()
#endif	
	strucEditStream:dwError := 0

	SendMessage(SELF:Handle(), EM_STREAMOUT, dwType, LONGINT(_CAST, @strucEditStream))

	sReturn := Psz2String(PSZ(_CAST, pBufStart))
	MemFree(pBufStart)
#ifdef __VULCAN__	
   gch:Free()
   GC.KeepAlive( del )
#else
	UnRegisterKid(@strucEditStream:dwCookie)
#endif	
	pStreamPos := NULL_PTR

	RETURN sReturn

ACCESS Value 
   RETURN __GetValue(SF_RTF)
   
ACCESS ValueAsText
   RETURN __GetValue(SF_TEXT)
   
ASSIGN Value(uNewValue) 
	RETURN SELF:__Value := uNewValue
END CLASS

FUNCTION __GetRTFValue(dwCookie AS DWORD, pbBuff AS BYTE PTR, cb AS LONGINT, pcb AS LONGINT PTR) AS DWORD /* WINCALL */
	LOCAL oRTFEdit AS RichEdit
	LOCAL pStreamPos AS BYTE PTR

#ifdef __VULCAN__
   LOCAL gch := GCHandle.FromIntPtr( (IntPtr) dwCookie ) AS GCHandle
   oRTFEdit := (RichEdit) gch:Target
#else
	oRTFEdit := OBJECT(_CAST, dwCookie)
#endif	
	pStreamPos := oRTFEdit:__StreamPos

	MemCopy(pStreamPos, pbBuff, DWORD(cb))
	pStreamPos := pStreamPos + cb
	BYTE(pStreamPos + 1) := 0
	LONGINT(pcb) := cb
	oRTFEdit:__StreamPos := pStreamPos

	RETURN 0

FUNCTION __LoadCallback(dwCookie AS DWORD, pbBuff AS BYTE PTR, cb AS LONGINT, pcb AS LONGINT PTR) AS DWORD /* WINCALL */

	LONGINT(pcb) := LONGINT(FRead3(PTR(_CAST, dwCookie), pbBuff, DWORD(cb)))

	IF (LONGINT(pcb) <= 0)
		LONGINT(pcb) := 0
	ENDIF

	RETURN 0

FUNCTION __SaveCallBack(dwCookie AS DWORD, pbBuff AS BYTE PTR, cb AS LONGINT, pcb AS LONGINT PTR) AS DWORD /* WINCALL */
	LONGINT(pcb) := LONGINT(FWrite3(PTR(_CAST, dwCookie), pbBuff, DWORD(cb)))

	RETURN 0

FUNCTION __SetRTFValue(dwCookie AS DWORD, pbBuff AS BYTE PTR, cb AS LONGINT, pcb AS LONGINT PTR) AS DWORD /* WINCALL */
	LOCAL oRTFEdit AS RichEdit
	LOCAL pStreamPos AS BYTE PTR
	LOCAL liBytesLeft AS LONGINT
	LOCAL liCopyBytes AS LONGINT

#ifdef __VULCAN__
   LOCAL gch := GCHandle.FromIntPtr( (IntPtr) dwCookie ) AS GCHandle
   oRTFEdit := (RichEdit) gch:Target
#else
	oRTFEdit := OBJECT(_CAST, dwCookie)
#endif	
	pStreamPos := oRTFEdit:__StreamPos

	liBytesLeft := LONGINT(_CAST, PszLen(PSZ(_CAST, pStreamPos)))
	liCopyBytes := Min(liBytesLeft, cb)

	MemCopy(pbBuff, pStreamPos, DWORD(liCopyBytes))
	pStreamPos := pStreamPos + liCopyBytes
	LONGINT(pcb) := liCopyBytes

	oRTFEdit:__StreamPos := pStreamPos

	RETURN 0




#region defines
DEFINE RICHTAB_CENTER  := 0x01000000
DEFINE RICHTAB_DECIMAL := 0x03000000
DEFINE RICHTAB_NORMAL  := 0x00000000
DEFINE RICHTAB_RIGHT   := 0x02000000
DEFINE RICHTAB_WORDBAR := 0x04000000
DEFINE RICHEDIT_CLASS := "RichEdit20A"
#endregion
