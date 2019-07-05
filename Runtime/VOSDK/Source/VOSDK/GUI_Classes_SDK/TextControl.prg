CLASS TextControl INHERIT Control
	PROTECT cSavedText AS STRING // save text before DESTROY, for 1.0 compatibility
	PROTECT oFont AS Font
	PROTECT lManageColor AS LOGIC
	PROTECT oTextColor AS Color
	PROTECT wXChars AS DWORD
	PROTECT wYChars AS DWORD
	PROTECT symImeFlag AS SYMBOL // default, don't check for IME

	//PP-030828 Strong typing
	METHOD __GetText() AS STRING STRICT 
	//PP-030828 Strong typing
	LOCAL iLength, iReturn AS INT
	LOCAL cBuffer AS PTR
	LOCAL DIM aBuf[128] AS BYTE
	LOCAL sRet AS STRING

	IF SELF:ValidateControl()
		iLength := SELF:Length
		IF (iLength > 0)
			IF iLength <128
				cBuffer := @aBuf[1]
			ELSE
				cBuffer := MemAlloc(DWORD(iLength+1))
			ENDIF
			iReturn := GetWindowText(hwnd, cBuffer, iLength+1)
			sRet := Psz2String(cBuffer)
			IF iReturn <= iLength
				sRet := Left(sRet, DWORD(iReturn))
			ENDIF
			IF (cBuffer != @aBuf[1])
				MemFree(cBuffer)
			ENDIF
		ENDIF
		RETURN sRet
	ENDIF

	RETURN cSavedText


METHOD __InitTextMetrics() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL strucRect IS _WinRect
	LOCAL dwDBUnits AS DWORD
	LOCAL iWidth AS INT
	LOCAL iHeight AS INT
	
	GetWindowRect(SELF:Handle(),@strucRect)

	iWidth := strucRect:Right - strucRect:Left
	iHeight := strucRect:Bottom - strucRect:Top
	dwDBUnits := DWORD(GetDialogBaseUnits())
	wXChars := DWORD(_CAST, iWidth)/LoWord(dwDBUnits)
	wYChars := DWORD(_CAST, iHeight)/HiWord(dwDBUnits)
	IF wYChars==0
		wYchars:=1
	ENDIF
	RETURN

METHOD __RescalCntlB(hFont AS PTR) AS VOID STRICT  //Hong. For Font Method
	//PP-030828 Strong typing
	LOCAL tm IS _winTextMetric
	LOCAL oNewDimension AS Dimension
	LOCAL hDCtmp AS PTR
	LOCAL hOldFont AS PTR
	LOCAL dwHeight AS DWORD
	LOCAL dwWidth AS DWORD

	hDCtmp := GetDC(SELF:Handle())
	hOldFont := SelectObject(hDCtmp, hfont)
	SELF:__InitTextMetrics()
	GetTextMetrics(hDCtmp, @tm)
	SelectObject(hDCtmp, hOldFont)
	ReleaseDC(SELF:Handle(), hDCtmp)
	// *******************
	// Adjust width/height
	// *******************
	dwWidth := wXChars * DWORD(_CAST, tm:tmAveCharWidth)
	dwHeight := wYChars * DWORD(_CAST, (tm:tmHeight + 2*tm:tmExternalLeading))

	oNewDimension := Dimension{dwWidth, dwHeight}
	InvalidateRect(SELF:Handle(), NULL, TRUE)
	SELF:Size := oNewDimension
	RETURN

METHOD __SetColors(_hDC AS PTR) AS PTR STRICT 
	//PP-030828 Strong typing
	//SE-050804 see new version of Control:__SetColors()
	LOCAL hBr AS PTR
	LOCAL strucLogBrush IS _WinLogBrush

    IF (oTextColor != NULL_OBJECT) .OR. (oControlBackGround != NULL_OBJECT)
		IF (lManageColor)
			SetTextColor(_hDC, oTextColor:ColorRef)
		ELSE
			SetTextColor(_hDC, GetSysColor(COLOR_WINDOWTEXT))
		ENDIF
	ENDIF

   hBr := SUPER:__SetColors(_hDC)

	IF hBr = NULL_PTR .AND. oTextColor != NULL_OBJECT
		IF IsInstanceOf(SELF, #Edit) .OR. IsInstanceOf(SELF, #ListBox)
			hBr := GetSysColorBrush(COLOR_WINDOW)
		ELSE
			hBr := GetSysColorBrush(COLOR_3DFACE)
		ENDIF
		GetObject(hBr, _SIZEOF(_WinLogBrush), @strucLogBrush)
		SetBkColor(_hDC, strucLogBrush:lbColor)
	ENDIF

	RETURN hBr

METHOD __SetText(cNewText AS STRING) AS STRING STRICT 
	//PP-030828 Strong typing
	

//PP-040410 Parameter checking not required with strong typing
// 	IF !IsString(cNewText)
// 		WCError{#__SetText,#TextControl,__WCSTypeError,cNewText,1}:Throw()
// 	ENDIF

	IF SELF:ValidateControl()
		SendMessage(hwnd, WM_SETTEXT, 0, LONGINT(_CAST, String2Psz(cNewText)))
	ENDIF

	RETURN cNewText

METHOD __Update() AS Control STRICT 
	//PP-030828 Strong typing
	// Added version of __Update() for TextControl
	LOCAL cText AS STRING
	LOCAL cNewText AS STRING
	LOCAL uOldValue AS USUAL

	
	IF SELF:Modified
		cText := SELF:TextValue
		uOldValue := AsString(uValue)
		IF IsInstanceOfUsual(SELF:FieldSpec, #FieldSpec)

			uValue := SELF:FieldSpec:Val(cText)

			// If theres a picture clause we need to reformat the data at this point
			//RvdH 060608 optimized
			//IF ((!IsNil(SELF:FieldSpec:Picture)) .AND. !Empty(SELF:FieldSpec:Picture))
			IF SLen(SELF:FieldSpec:Picture) > 0
				cNewText := SELF:FieldSpec:Transform(uValue)
			ELSEIF IsNil(uValue)
				cNewText := ""
			ELSE
				cNewText := AsString(uValue)
			ENDIF

			IF !(cNewText == cText)
				SELF:TextValue := cNewText
			ENDIF
		ELSE
			uValue := cText
		ENDIF

		SELF:Modified := .F. 
		SELF:ValueChanged := !(uOldValue == AsString(uValue))
	ENDIF
	RETURN SELF

ACCESS Caption 
	

	IF !IsString(cCaption)
		cCaption := SELF:__GetText()
	ENDIF

	RETURN cCaption

ASSIGN Caption(cNewCaption) 
	

	IF !IsString(cNewCaption)
		WCError{#Caption,#TextControl,__WCSTypeError,cNewCaption,1}:Throw()
	ENDIF
	cCaption := cNewCaption

	RETURN SELF:__SetText(cNewCaption)

ACCESS ControlFont
    // RvdH 080609 Report from J Bieler:
    // We renamed the Textcontrol:Font access to ControlFont
    // So we must check for oControl:ControlFont and not 
    // oParent:Font when the parent is a TextControl
    // Also some parent may not have a Font access (like Toolbars)
    LOCAL oControl AS TextControl
    IF (oFont == NULL_OBJECT)
            IF IsInstanceOf(oParent, #TextControl) 
                   oControl := oParent
                   IF oControl:ControlFont != NULL_OBJECT
                        RETURN oControl:ControlFont
                   ENDIF
            ELSEIF IsAccess(oParent, #Font)
                   oFont := oParent:Font
                   IF oFont != NULL_OBJECT
                        RETURN oFont
                   ENDIF
            ELSEIF IsAccess(oParent, #ControlFont)
                   oFont := oParent:ControlFont
                   IF oFont != NULL_OBJECT
                        RETURN oFont
                   ENDIF
            ENDIF
            oFont := Font{FONTSYSTEM8}
            oFont:Create()
    ENDIF
    RETURN SELF:oFont

ASSIGN ControlFont(oNewFont)  
	LOCAL hFont AS PTR

	oFont:= oNewFont
	IF oFont != NULL_OBJECT
		oFont:Create()
		hFont := oFont:Handle()
	ELSE
		hFont := GetStockObject(DEFAULT_GUI_FONT)
		IF (hFont == NULL_PTR)
			hFont := GetStockObject(SYSTEM_FONT)
		ENDIF
	ENDIF

	SELF:__RescalCntlB(hFont)
	SendMessage(SELF:Handle(), WM_SETFONT, DWORD(_CAST, hFont), LONGINT(_CAST, TRUE))

	RETURN 

METHOD Create() 
	IF SUPER:Create() != NULL_PTR
		SELF:__InitTextMetrics()
	ENDIF

	RETURN hWnd

ACCESS CurrentText 
	RETURN SELF:__GetText()

ASSIGN CurrentText(cNewText) 
	LOCAL cCurrentText AS STRING
	LOCAL cOldValue AS STRING

	
	IF !IsString(cNewText)
		WCError{#CurrentText,#TextControl,__WCSTypeError,cNewText,1}:Throw()
	ENDIF

	cCurrentText := SELF:__SetText(cNewText)
	cOldValue := AsString(uValue)

	IF IsInstanceOfUsual(SELF:FieldSpec, #FieldSpec)
		uValue := SELF:FieldSpec:Val(cCurrentText)
		SELF:ValueChanged := !(cOldValue == AsString(uValue))
	ELSE
		uValue := cCurrentText
		SELF:ValueChanged := !(cOldValue == uValue)
	ENDIF

	RETURN 

METHOD Destroy() 
	// save the current text for TextValue Accesses afte enddialog/endwindow (for 1.0 compatibility)
	IF IsWindow(hwnd)
		cSavedText := SELF:__GetText()
	ENDIF

	IF !InCollect()
		oFont := NULL_OBJECT
	ENDIF

	SUPER:Destroy()
	RETURN SELF

METHOD EnableAutoComplete(dwFlags) 
	//PP-030902
	DEFAULT(@dwFlags,SHACF_DEFAULT)
	RETURN ShellAutoComplete(SELF:handle(),dwFlags)

METHOD Font(oNewFont, lRescal)  // hong. 01/22/96
	LOCAL lRescalCntl AS LOGIC
	LOCAL oOldFont AS Font
	LOCAL hFont AS PTR

	IF !IsNil(oNewFont)
		IF !IsInstanceOfUsual(oNewFont, #Font)
			WCError{#Font, #TextControl, __WCSTypeError, oNewFont,1}:Throw()
		ENDIF
	ENDIF

	IF IsNil(lRescal)
		lRescalCntl := FALSE
	ELSE
		lRescalCntl := lRescal
	ENDIF

	oOldFont := SELF:ControlFont
	oFont := oNewFont
	IF (oFont != NULL_OBJECT)
		oFont:Create()
		hFont := oFont:Handle()
	ELSE
		hFont := GetStockObject(DEFAULT_GUI_FONT)
		IF hFont == NULL_PTR
			hFont := GetStockObject(SYSTEM_FONT)
		ENDIF
	ENDIF

	IF lRescalCntl
		SELF:__RescalCntlB(hFont)
	ENDIF
	SendMessage(SELF:Handle(), WM_SETFONT, DWORD(_CAST, hFont), 1)

	RETURN oOldFont

#ifndef __VULCAN__
ACCESS Font  // hong. 01/22/96
	
	RETURN SELF:ControlFont

ASSIGN Font(oNewFont)  // hong. 01/22/96
	

	SELF:ControlFont := oNewFont

	RETURN 
#endif	

METHOD Ime(symIme) 
	

	IF (symIme == NIL)
		RETURN SELF:symImeFlag
	ENDIF
	SELF:symImeFlag := symIme
	RETURN symIme

CONSTRUCTOR(oOwner, xId, oPoint, oDimension, cRegclass, kStyle, lDataAware) 
	

	symIMEFlag := #AUTO
	SUPER(oOwner, xID, oPoint, oDimension, cRegclass, kStyle, lDataAware)
	IF IsInstanceOfUsual(xID, #ResourceID)
		SELF:__InitTextMetrics()
	ENDIF

	RETURN 

ACCESS Length 
	LOCAL lRetVal AS LONGINT

	
	IF SELF:ValidateControl()
		lRetVal := GetWindowTextLength(hwnd)
		IF (lRetVal == CB_ERR)
			lRetVal := 0
		ENDIF
	ENDIF

	RETURN lRetVal

METHOD RemoveEditBalloonTip(hControl) 
	//PP-030902
	DEFAULT(@hControl,SELF:handle())
	SendMessage(hControl,EM_HIDEBALLOONTIP,0,0)
	RETURN SELF

METHOD SetCueBanner(cText,hControl) 
	//PP-030902
	// Requires XP or greater
	LOCAL pMsg AS PTR
	LOCAL lReturn AS LOGIC

	DEFAULT(@hControl,SELF:handle())
	pMsg := String2W(cText)

	lReturn := LOGIC(_CAST,SendMessage(hControl,EM_SETCUEBANNER,0,LONGINT(_CAST,pMsg)))

   	SysFreeString(pMsg)

	RETURN lReturn

METHOD ShowEditBalloonTip(cTitle,cText,dwIcon,hControl) 
	//PP-030902
	// Requires XP or greater
	LOCAL pMsg IS _WINEDITBALLOONTIP

	DEFAULT(@hControl,SELF:handle())
	pMsg:cbStruct := _SIZEOF(_WINEDITBALLOONTIP)
	pMsg:pszTitle := String2W(cTitle)
	pMsg:pszText := String2W(cText)
	pMsg:ttiIcon := dwIcon

	SendMessage(hControl,EM_SHOWBALLOONTIP,0,LONGINT(_CAST,@pMsg))

  	SysFreeString(pMsg:pszText)
  	SysFreeString(pMsg:pszTitle)
	RETURN SELF

ACCESS TextColor 
	//RvdH 050509 Make sure we always return an Object
	

	IF SELF:oTextColor == NULL_OBJECT
      oTextColor := Color{GetSysColor(COLOR_WINDOWTEXT), -1l}
	ENDIF
	RETURN oTextColor

ASSIGN TextColor(_oColor) 
   //PP-040627 Update. S.Ebert
	LOCAL dwDefaultColor AS DWORD
	LOCAL dwNewColor AS DWORD
	LOCAL dwOldColor AS DWORD
	LOCAL hHandle AS PTR
    LOCAL oColor    AS Color
	

	IF ! IsInstanceOfUsual(_oColor,#Color)
		WCError{#TextColor,#TextControl,__WCSTypeError,_oColor,1}:Throw()
	ENDIF
    oColor := _oColor
	dwDefaultColor := GetSysColor(COLOR_WINDOWTEXT)
	dwNewColor     := oColor:ColorRef
	IF lManageColor
	   dwOldColor := oTextColor:ColorRef
   ELSE
      dwOldColor := dwDefaultColor
   ENDIF

	oTextColor := oColor

   //Check if we need to revert to system default
	lManageColor := (dwNewColor != dwDefaultColor)

   IF dwNewColor != dwOldColor
	   hHandle := SELF:Handle()
      IF hHandle != NULL_PTR
   		InvalidateRect(hHandle, NULL_PTR, TRUE)
   		UpdateWindow(hHandle)
      ENDIF
   ENDIF

	RETURN 

ACCESS TextValue 
	// TextValue - returns current contents
	

	RETURN SELF:__GetText()

ASSIGN TextValue(cNewCaption) 
	LOCAL cTextValue AS STRING
	LOCAL cOldValue AS STRING

	
	IF !IsString(cNewCaption)
		WCError{#TextValue,#TextControl,__WCSTypeError,cNewCaption,1}:Throw()
	ENDIF

	cTextValue := SELF:__SetText(cNewCaption)
	cOldValue := AsString(uValue)

	IF IsInstanceOfUsual(SELF:FieldSpec, #FieldSpec)
		uValue := SELF:FieldSpec:Val(cTextValue)
		SELF:ValueChanged := !(cOldValue == AsString(uValue))
	ELSE
		uValue := cTextValue
		SELF:ValueChanged := !(cOldValue == uValue)
	ENDIF

	RETURN 

END CLASS

VOSTRUCT _WINEDITBALLOONTIP
	//PP-030902
	MEMBER cbStruct AS DWORD
    MEMBER pszTitle AS PSZ
	MEMBER pszText AS PSZ
    MEMBER ttiIcon AS INT



#region defines
//DEFINE ECM_FIRST := 0x1500      // Edit control messages
DEFINE EM_SETCUEBANNER := (ECM_FIRST + 1)
DEFINE EM_GETCUEBANNER := (ECM_FIRST + 2)
DEFINE EM_SHOWBALLOONTIP := (ECM_FIRST + 3)
DEFINE EM_HIDEBALLOONTIP := (ECM_FIRST + 4)
DEFINE TTI_NONE := 0
DEFINE TTI_INFO := 1
DEFINE TTI_WARNING := 2
DEFINE TTI_ERROR := 3
#endregion
