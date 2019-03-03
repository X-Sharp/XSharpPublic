CLASS Edit INHERIT TextControl
	PROTECT lNoNotify AS LOGIC
	PROTECT lForceModFlag2True AS LOGIC

	//PP-030828 Strong typing
	ASSIGN __ForceModFlag2True(lNewValue AS LOGIC)  STRICT 
	//PP-030828 Strong typing
	

	RETURN (lForceModFlag2True := lNewValue)

ACCESS __NoNotify AS LOGIC STRICT 
	//PP-030828 Strong typing
	

	RETURN lNoNotify

METHOD CanUndo() 
	

	IF SELF:ValidateControl()
		RETURN LOGIC(_CAST, SendMessage(SELF:Handle(), EM_CANUNDO, 0, 0 ))
	ENDIF

	RETURN FALSE

ACCESS Caption 
	

	RETURN cCaption

ASSIGN Caption(cNewCaption) 
	
	IF !IsString(cNewCaption)
		WCError{#Caption,#Edit,__WCSTypeError,cNewCaption,1}:@@Throw()
	ENDIF
	RETURN cCaption := cNewCaption

METHOD Clear() 
	
	IF SELF:ValidateControl()
		SendMessage (SELF:Handle(), WM_CLEAR, 0, 0 )
	ENDIF
	RETURN SELF

METHOD Copy() 
	LOCAL dwTemp AS DWORD
	LOCAL hHandle AS PTR

	

	IF SELF:ValidateControl()
		hHandle := SELF:Handle()
		dwTemp := DWORD(SendMessage(hHandle, EM_GETSEL, 0, 0))
		SendMessage(hHandle, WM_COPY, 0, 0)
	ENDIF

	RETURN SELF

METHOD Cut() 
	

	IF SELF:ValidateControl()
		SendMessage(SELF:Handle(), WM_CUT, 0, 0)
	ENDIF

	RETURN SELF

METHOD Font(oNewFont, lRescal) 
	LOCAL uRet AS USUAL
	LOCAL oMargins AS Pair

	IF SELF:ValidateControl()
		oMargins := SELF:Margins
		uRet := SUPER:Font(oNewFont, lRescal)
		SELF:Margins := oMargins
	ENDIF

	RETURN uRet

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 
	LOCAL dwStyle AS DWORD

	

	IF !IsInstanceOfUsual(xID,#ResourceID)
		dwStyle:= _OR(WS_CHILD, WS_CLIPSIBLINGS)
		IF !IsNil(kStyle)
			dwStyle := _OR(DWORD(kStyle), dwStyle)
		ENDIF
		SUPER(oOwner, xID, oPoint, oDimension, "Edit", dwStyle, TRUE)
	ELSE
		SUPER(oOwner, xID, oPoint, oDimension, , kStyle, TRUE)
	ENDIF

	RETURN 


METHOD IsPassword() 
LOCAL Style		AS LONG

	// DHer: 18/12/2008
	Style := GetWindowLong(SELF:Handle(),GWL_STYLE)

RETURN _AND(Style,ES_PASSWORD)!=0

ACCESS Margins 
	LOCAL dwRet AS DWORD

	IF SELF:ValidateControl()
		dwRet := DWORD(SendMessage(SELF:Handle(), EM_GETMARGINS, 0, 0))
	ENDIF

	RETURN Dimension{LoWord(dwRet), HiWord(dwRet)}

ASSIGN Margins(oNewMargins) 
	LOCAL wLeft, wRight AS WORD
	

	IF SELF:ValidateControl()
		wLeft := oNewMargins:Width
		wRight := oNewMargins:Height
		SendMessage(SELF:Handle(), EM_SETMARGINS, _OR(EC_LEFTMARGIN, EC_RIGHTMARGIN), MAKELPARAM(wLeft, wRight))
	ENDIF

	RETURN 

ACCESS Modified 
	

	IF SELF:ValidateControl()
		IF lForceModFlag2True
			RETURN TRUE
		ELSE
			RETURN (SendMessage(SELF:Handle(), EM_GETMODIFY, 0, 0 ) != 0)
		ENDIF
	ENDIF

	RETURN FALSE

ASSIGN Modified(lModified) 
	

	IF !IsLogic(lModified)
		WCError{#Modified,#Edit,__WCSTypeError,lModified,1}:@@Throw()
	ENDIF

	IF SELF:ValidateControl()
		SendMessage(SELF:Handle(), EM_SETMODIFY, DWORD(_CAST, lModified), 0)
	ENDIF

	RETURN 

METHOD Paste(cNewString) 
	LOCAL hHandle AS PTR

	

	IF SELF:ValidateControl()
		IF IsNil(cNewString)
			hHandle := SELF:Handle()
			SendMessage(hHandle, WM_CLEAR, 0, 0)
			SendMessage(hHandle, WM_PASTE, 0, 0)
		ELSE
			IF !IsString(cNewString)
				WCError{#Paste,#Edit,__WCSTypeError,cNewString,1}:@@Throw()
			ENDIF
			SendMessage(SELF:Handle(), EM_REPLACESEL, 0, LONGINT(_CAST, String2Psz(cNewString)))
		ENDIF
	ENDIF

	RETURN SELF


ACCESS ReadOnly 
	IF SELF:ValidateControl()
		RETURN (_AND(GetWindowLong(SELF:Handle(), GWL_STYLE), ES_READONLY) > 0)
	ENDIF
	RETURN (_AND(dwStyle, DWORD(_CAST, ES_READONLY)) > 0)

ASSIGN ReadOnly(lNewValue) 
	IF SELF:ValidateControl()
		SendMessage(SELF:Handle(), EM_SETREADONLY, DWORD(_CAST, lNewValue), 0L)
	ELSE
		IF (lNewValue)
			dwStyle := _OR(dwStyle, DWORD(_CAST, ES_READONLY))
		ELSE
			dwStyle := _AND(dwStyle, DWORD(_CAST, _NOT(ES_READONLY)))
		ENDIF
	ENDIF

	RETURN 

ACCESS SelectedText 
	//PP-040508 Update S.Ebert
	LOCAL dwLast  AS DWORD
	LOCAL dwFirst AS DWORD
	LOCAL cReturn AS STRING

	

	IF SELF:ValidateControl()
		SendMessage(hWnd, EM_GETSEL, DWORD(_CAST,@dwFirst), LONGINT(_CAST,@dwLast))
		IF dwLast > dwFirst
			cReturn := SubStr3(SELF:__GetText(), dwFirst+1, dwLast - dwFirst)
		ENDIF
	ENDIF

	RETURN cReturn

ASSIGN SelectedText(cNewString) 
	
	IF !IsString(cNewString)
		WCError{#SelectedText,#Edit,__WCSTypeError,cNewString,1}:@@Throw()
	ENDIF
	IF SELF:ValidateControl()
		SendMessage( SELF:Handle(), EM_REPLACESEL, 0, LONGINT(_CAST, String2Psz(cNewString)))
	ENDIF

	RETURN 

ACCESS Selection 
	LOCAL dwStart AS DWORD
	LOCAL dwFinish AS DWORD

	IF SELF:ValidateControl()
		SendMessage(SELF:Handle(), EM_GETSEL, DWORD(_CAST, @dwStart), LONGINT(_CAST, @dwFinish))
		RETURN Selection{dwStart, dwFinish}
	ENDIF

	RETURN Selection{0,0}

ASSIGN Selection(oSelection) 
	LOCAL oSel AS Selection
	IF !IsInstanceOfUsual(oSelection,#Selection)
		WCError{#Selection,#Edit,__WCSTypeError,oSelection,1}:@@Throw()
	ENDIF
	oSel := oSelection
	IF SELF:ValidateControl()
		//PP-040418 Issue 12761
		PostMessage(SELF:Handle(), EM_SETSEL, DWORD(oSel:Start), oSel:Finish)
	ENDIF

	RETURN 

METHOD SelectAll()
	// Selects the whole text in an edit control
	IF SELF:ValidateControl()
		SendMessage( SELF:Handle(), EM_SETSEL, 0, -1 )
	ENDIF

RETURN NIL
	
METHOD SelectNone()
	// Deselects the text in an edit control
	IF SELF:ValidateControl()
		SendMessage( SELF:Handle(), EM_SETSEL, 0, 0 )
	ENDIF

RETURN NIL
		
METHOD SetSelectionFocus() 
	// DHer: 18/12/2008
	SELF:Owner:SetFocus()
	SELF:SetFocus()
	SELF:Selection := Selection{0,SLen(SELF:Textvalue)+1}

RETURN NIL


ACCESS TextLimit 
	IF SELF:ValidateControl()
		RETURN SendMessage(SELF:Handle(), EM_GETLIMITTEXT, 0, 0)
	ENDIF

	RETURN 0

ASSIGN TextLimit(nChars) 
	LOCAL dwTextLen AS DWORD
	LOCAL ptrBuffer AS PTR
	LOCAL wChars AS DWORD
	LOCAL hHandle AS PTR

	IF !IsNumeric(nChars)
		WCError{#TextLimit,#Edit,__WCSTypeError,nChars,1}:@@Throw()
	ENDIF

	IF SELF:ValidateControl()
		hHandle := SELF:Handle()
		wChars := nChars
		dwTextLen := DWORD(SendMessage(hHandle, WM_GETTEXTLENGTH, 0, 0))

		IF (dwTextLen > wChars)
			ptrBuffer := MemAlloc(wChars+1)
			SendMessage(hHandle, WM_GETTEXT, wChars+1, LONGINT(_CAST, ptrBuffer))
			SendMessage(hHandle, WM_SETTEXT, 0, LONGINT(_CAST, ptrBuffer))
			MemFree(ptrBuffer)
		ENDIF

		SendMessage(hHandle, EM_LIMITTEXT, wChars, 0)
	ENDIF

	RETURN 

ACCESS TextValue 
	

	RETURN SELF:__GetText()

ASSIGN TextValue(cNewText) 
	LOCAL cTextValue AS STRING
	LOCAL cOldValue AS STRING

	

	IF !IsString(cNewText)
		WCError{#TextValue,#Edit,__WCSTypeError,cNewText,1}:@@Throw()
	ENDIF

	cOldValue := AsString(uValue)
	cTextValue := SELF:__SetText(cNewText)

	IF IsInstanceOfUsual(SELF:FieldSpec, #FieldSpec)
		uValue := SELF:FieldSpec:Val(cTextValue)
	ELSE
		uValue := cTextValue
	ENDIF

	SELF:ValueChanged := !(cOldValue == AsString(uValue))

	RETURN 

METHOD Undo() 
	

	IF SELF:ValidateControl()
		RETURN (SendMessage(SELF:Handle(), EM_UNDO, 0, 0) != 0)
	ENDIF

	RETURN FALSE
END CLASS

CLASS SingleLineEdit INHERIT Edit
	PROTECT oEditString AS __FormattedString
	PROTECT wOverWrite AS LONGINT  		//RvdH 070205 changed from WORD to LONG
	PROTECT wScrMode AS LONGINT      	//RvdH 070205 changed from WORD to LONG
	PROTECT wFocusSel AS LONGINT        //RvdH 070205 changed from WORD to LONG
	PROTECT lAllowSelection AS LOGIC
	PROTECT lAutoFocusChange AS LOGIC

	//PP-030828 Strong typing
	ASSIGN __AllowSelection(lNewValue AS LOGIC)  STRICT 
	//PP-030828 Strong typing
	RETURN (lAllowSelection := lNewValue)

ACCESS __CurPos AS LONGINT STRICT 
	//PP-030828 Strong typing
	LOCAL liStart AS LONGINT

	SendMessage(SELF:Handle(), EM_GETSEL, DWORD(_CAST, @liStart), 0)

	liStart++
	RETURN liStart

ASSIGN __CurPos(iNewPos AS LONGINT)  STRICT 
	//PP-030828 Strong typing
	
	IF (iNewPos > 0)
		SendMessage(SELF:Handle(), EM_SETSEL, DWORD(iNewPos-1), iNewPos-1)
	ELSE
		SendMessage(SELF:Handle(), EM_SETSEL, 0, 0)
	ENDIF

	RETURN 

ACCESS __EditString AS __FormattedString STRICT  
	//PP-030828 Strong typing
	

	RETURN oEditString

ACCESS __FSLength AS DWORD STRICT 
	//PP-030828 Strong typing

	IF (oFieldSpec != NULL_OBJECT)
		RETURN ofieldSpec:Length
	ENDIF

	RETURN 0xFFFF

METHOD __Update() AS Control STRICT 
	//PP-030828 Strong typing
	// Added version of __Update() for TextControl
	LOCAL cText,cNumText AS STRING
	LOCAL cNewText AS STRING
	LOCAL uOldValue AS USUAL

	IF (NULL_STRING == SELF:Picture)
		RETURN SUPER:__Update()
	ELSEIF SELF:Modified
		cText := SELF:TextValue
		uOldValue := AsString(uValue)

		IF (oFieldSpec != NULL_OBJECT)
			IF (oFieldSpec:ValType == "N")
				IF (oEditString != NULL_OBJECT)
					cNumText := SubStr(cText, oEditString:NextEditPos(0))
				ELSE
					cNumText := cText
				ENDIF
				IF (oEditString != NULL_OBJECT) .AND. (At2(",", oEditString:Picture) != 0)
					cNumText := StrTran(cNumText, Chr(SetThousandSep()), "")
				ENDIF
				cNumText := StrTran(cNumText, " ", "")
				uValue := Val(cNumText)
			ELSEIF (IVarGet(oFieldSpec, #Nullable) == TRUE)
				uValue := Unformat(cText, SELF:Picture, oEditString:Type+"0")
			ELSE
				uValue := Unformat(cText, SELF:Picture, oEditString:Type)
			ENDIF
		ELSE
			uValue := Unformat(cText, SELF:Picture, oEditString:Type)
		ENDIF

		cNewText := Transform(uValue, SELF:Picture)
		IF !(cNewText == cText)
			SELF:TextValue := cNewText
		ENDIF
		SELF:Modified := FALSE
		SELF:ValueChanged := !(uOldValue == AsString(uValue))
	ENDIF
	RETURN SELF

ASSIGN __Value(uNewValue AS USUAL)  STRICT 
	//PP-030828 Strong typing
	

	IF (oEditString != NULL_OBJECT .AND. (oFieldSpec == NULL_OBJECT))
		oEditString:UsualValue := uNewValue
		SELF:uValue := uNewValue
		oHLStatus := NULL_OBJECT
		SELF:Modified := FALSE
		SELF:lChanged := TRUE
	ELSE
		SUPER:__Value := uNewValue
	ENDIF
	RETURN 

ACCESS AutoFocusChange 

	RETURN lAutoFocusChange

ASSIGN AutoFocusChange(lNewVal) 

	RETURN (lAutoFocusChange := lNewVal)


METHOD CreateFormattedString(cPicture, cType, cDefTempl) 
	//PP-031115 New method, replaces __FormattedString assignment with method call, allowing overriding
	//PP-040101 Fixed method name spelling
	oEditString := __FormattedString{SELF, cPicture, cType, wOverWrite, cDefTempl, wScrMode}
	RETURN SELF

METHOD Dispatch(oEvent) 
	LOCAL oEvt := oEvent AS @@Event
	LOCAL uMsg 		AS DWORD
	LOCAL wParam 	AS DWORD
	LOCAL lShiftOn AS LOGIC
	LOCAL lCtrlOn AS LOGIC
	LOCAL lExtSel AS LOGIC

	uMsg 	 := oEvt:uMsg 
	wParam := oEvt:wParam 
	
	IF ((oEditString != NULL_OBJECT) .AND. (_AND(GetWindowLong(hwnd, GWL_STYLE), ES_READONLY) == 0))
		IF (((uMsg == WM_KEYDOWN) .OR. (uMsg == WM_KEYUP) .OR. (uMsg == WM_CHAR)))
			#ifndef __VULCAN__
			lShiftOn := LOGIC(_CAST, _AND(GetKeyState(VK_SHIFT), SHORTINT(_CAST, 0x8000)))
			lCtrlOn := LOGIC(_CAST, _AND(GetKeyState(VK_CONTROL), SHORTINT(_CAST, 0x8000)))
			#else
			lShiftOn := GetKeyState(VK_SHIFT) < 0
			lCtrlOn := GetKeyState(VK_CONTROL) < 0
			#endif
			lExtSel := lShiftOn .AND. ((uMsg == WM_KEYDOWN) .OR. (uMsg == WM_KEYUP)) .AND.;
				((wParam == VK_LEFT) .OR. (wParam == VK_RIGHT) .OR. (wParam == VK_END) .OR. (wParam == VK_HOME))
			IF !((lAllowSelection .AND. lExtSel) .OR. (lCtrlOn .AND. (uMsg != WM_CHAR)))

				//PP-040429 The following IF clause was within the ProcessKeyEvent IF clause
				// It was moved here in response to emailed bug report from Willie Moore, 2004-04-28
				//PP-030505 Bug:100
				IF uMsg == WM_CHAR
					IF SELF:Selection:Start = 0 .AND. SELF:oEditString <> NULL_OBJECT
						SELF:oEditString:TestFirstChar(Chr( wParam))
					ENDIF
				ENDIF

				//IF oEditString:ProcessKeyEvent(__ObjectCastClassPtr(oEvt, __pCKeyEvent))
				IF oEditString:ProcessKeyEvent(KeyEvent{oEvt})
					// 2.5b fix to invoke event handler
					SUPER:Dispatch(oEvt)
					RETURN 1L
				ENDIF
			ENDIF

			//elseif (uMsg == EM_SETSEL) .and. (oEvt:lParam < 0)
			// self:__CurPos := oEditString:NextEditPos(1)
			// return 1L

		ELSEIF (uMsg == WM_PASTE)
			SELF:oEditString:Paste()
			RETURN 1L

		ELSEIF (uMsg == WM_CUT)
			SELF:oEditString:Cut()
			RETURN 1L
		ELSEIF (uMsg == WM_SYSKEYDOWN)
			//SELF:KeyDown(__ObjectCastClassPtr(oEvt, __pCKeyEvent))
			SELF:KeyDown(KeyEvent{oEvt})
			RETURN SELF:EventReturnValue
		ELSEIF (uMsg == WM_SYSKEYUP)
			//SELF:KeyUp(__ObjectCastClassPtr(oEvt, __pCKeyEvent))
			SELF:KeyUp(KeyEvent{oEvt})
			RETURN SELF:EventReturnValue
		ENDIF

	ENDIF

	RETURN SUPER:Dispatch(oEvt)

ASSIGN FieldSpec(oNewFS) 
	LOCAL sPic AS STRING
	LOCAL pos AS USUAL
	LOCAL vt AS STRING
	LOCAL sDefTempl AS STRING

	
	SUPER:FieldSpec := oNewFS

	vt := oFieldSpec:ValType
	DO CASE
	CASE (vt == "C")
		sDefTempl := Replicate("X", Min(oFieldSpec:Length, MAX_FMTSTRING_LEN))
	CASE (vt == "L")
		sDefTempl := "L"
	CASE oFieldSpec:ValType == "N"
		sDefTempl := Replicate("#", Min(oFieldSpec:Length, MAX_FMTSTRING_LEN))
		IF (oFieldSpec:Decimals > 0)
			pos := oFieldSpec:Length - oFieldSpec:Decimals
			sDefTempl := Left(sDefTempl, Pos - 1) + "." + Replicate("#", oFieldSpec:Decimals)
		ENDIF
	ENDCASE

	IF (NULL_STRING != SELF:Picture)
		sPic := SELF:Picture
	ELSEIF	(NULL_STRING != oFieldSpec:Picture)
		sPic := oFieldSpec:Picture
	ELSEIF (vt == "D")
		sPic := "@D"
	ELSEIF !__DBCSEnabled()
		sPic := sDefTempl
	ENDIF

	IF (NULL_STRING != sPic)
		//PP-031115 Replace __FormattedString assignment with method call, allowing overriding
		// oEditString := __FormattedString{SELF, sPic, vT, wOverWrite, sDefTempl, wScrMode}
		//PP-040101 Fixed method name spelling
		SELF:CreateFormattedString(sPic, vT, sDefTempl)
	ENDIF

	DO CASE
	//RvdH 050602 Added default value for STRINGS (issue 13031)
	CASE (vt == "C")
		SELF:__Value := ""
	CASE (vt == "L")
		SELF:__Value := FALSE
	CASE (vt == "N")
		IF (oFieldSpec:Decimals > 0)
			SELF:__Value := 0.0
		ELSE
			SELF:__Value := 0
		ENDIF
	ENDCASE

	SELF:modified := FALSE

	RETURN 

METHOD FocusChange(oFocusChangeEvent) 
	LOCAL iPos AS INT

	
	//PP-030505 Bug:93 SendMessage changed to PostMessage

	IF oFocusChangeEvent:GotFocus
		IF (oEditString != NULL_OBJECT)
			oEditString:lendkey := FALSE
		ENDIF
		IF (wFocusSel == FSEL_HOME)
			IF (oEditString != NULL_OBJECT)
				iPos := 1
				IF !oEditString:IsEditPos(iPos)
					iPos := oEditString:NextEditPos(iPos)
				ENDIF
				iPos--
			ENDIF
			PostMessage(SELF:Handle(), EM_SETSEL, DWORD(_CAST, iPos), iPos)
		ELSEIF (wFocusSel == FSEL_END)
			IF (oEditString != NULL_OBJECT)
				iPos := oEditString:PrevEditPos(INT(_CAST, SLen(SELF:CurrentText)))
			ELSE
				iPos := INT(_CAST, SLen(SELF:CurrentText))
			ENDIF
			PostMessage(SELF:Handle(), EM_SETSEL, DWORD(_CAST, iPos), iPos)
		ELSEIF (wFocusSel == FSEL_TRIM)
			iPos := INT(_CAST, SLen(RTrim(SELF:CurrentText)))
			PostMessage(SELF:Handle(), EM_SETSEL, 0, iPos)
		ELSEIF (wFocusSel == FSEL_TRIMEND)
			iPos := INT(_CAST, SLen(RTrim(SELF:CurrentText)))
			PostMessage(SELF:Handle(), EM_SETSEL, DWORD(iPos), iPos)
		ENDIF
	ENDIF

	RETURN SUPER:FocusChange(oFocusChangeEvent)

ACCESS FocusSelect 
	

	RETURN wFocusSel

ASSIGN FocusSelect(wNewValue) 
	

	IF !IsLong(wNewValue)
		WCError{#FocusSelect,#SingleLineEdit,__WCSTypeError,wNewValue,1}:@@Throw()
	ENDIF

	IF (wNewValue < 0) .OR. (wNewValue > 4)
		wNewValue := FSEL_TRIM
	ENDIF

	RETURN (wFocusSel := wNewValue)

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 
	

	SUPER(oOwner, xID, oPoint, oDimension, kStyle)
	wOverWrite := OVERWRITE_NEVER
	wScrMode := SCRMODE_FULL
	wFocusSel := FSEL_TRIM
	lAllowSelection := TRUE

	RETURN 

ACCESS OverWrite 
	

	RETURN wOverWrite

ASSIGN OverWrite(wNewValue) 
	

	IF !IsLong(wNewValue)
		WCError{#OverWrite,#SingleLineEdit,__WCSTypeError,wNewValue,1}:@@Throw()
	ENDIF

	IF (wNewValue < 0) .OR. (wNewValue > 2)
		wNewValue := OVERWRITE_NEVER
	ENDIF

	wOverWrite := wNewValue
	IF (oEditString != NULL_OBJECT)
		oEditString:wOverwrite := wOverWrite
	ENDIF

	RETURN 

ACCESS Picture 
	

	IF oEditString == NULL_OBJECT
		RETURN NULL_STRING
	ENDIF
	RETURN oEditString:Picture

ASSIGN Picture(cNewPicture) 
	LOCAL uOldVal AS USUAL

	

	IF !IsString(cNewPicture)
		WCError{#Picture,#SingleLineEdit,__WCSTypeError,cNewPicture,1}:@@Throw()
	ENDIF

	IF (oEditString == NULL_OBJECT)
		lNoNotify := TRUE
		//PP-031115 Replace __FormattedString assignment with method call, allowing overriding
		// oEditString := __FormattedString{SELF, cNewPicture, "C", wOverWrite,,wScrMode}
		//PP-040101 Fixed method name spelling
		SELF:CreateFormattedString(cNewPicture, "C", NIL)
		lNoNotify := FALSE
	ELSE
		IF (cNewPicture == NULL_STRING)
			oEditString := NULL_OBJECT
			RETURN NULL_STRING
		ELSE
			uOldVal := uValue // new for 2.0b, should allow picture change on the fly
			oEditString:Picture := cNewPicture
			oEditString:UsualValue := uOldVal // new for 2.0b
		ENDIF
	ENDIF

	RETURN oEditString:Picture

ACCESS ScrollMode 
	

	RETURN wScrMode

ASSIGN ScrollMode(wNewValue) 
	

	IF !IsLong(wNewValue)
		WCError{#ScrMode,#SingleLineEdit,__WCSTypeError,wNewValue,1}:@@Throw()
	ENDIF

	IF (wNewValue < 0) .OR. (wNewValue > 2)
		wNewValue := SCRMODE_FULL
	ENDIF

	wScrMode := wNewValue
	IF (oEditString != NULL_OBJECT)
		oEditString:wScrMode := wScrMode
	ENDIF

	RETURN 

ACCESS TextValue 
LOCAL cText			AS STRING
LOCAL cOrigText		AS STRING
LOCAL cFormat		AS STRING
LOCAL nPosDay		AS DWORD
LOCAL nPosMonth		AS DWORD
LOCAL nDay			AS INT
LOCAL nMonth		AS INT

	// DHer: 18/12/2008
	// Error in VO : Date "25/2 /2005" gets translated to in "25/04/2005" !!!  (in DD/MM/YYYY format)

	cOrigText := SUPER:TextValue
	cText := cOrigText
	IF SELF:oFieldSpec<>NULL_OBJECT
		IF SELF:oFieldSpec:ValType="D"				// When Date
			IF GetFocus()<>SELF:Handle()			   // When Losing Focus
				IF At2(" ",cText) <> 0			   // When space in cText
					cFormat     := Upper(GetDateFormat())
					nPosDay     := At("DD",cFormat)
					nPosMonth   := At("MM",cFormat)
					IF nPosDay<>0
						nDay := Val(SubStr(cText,nPosDay,2))
						IF nDay<>0
							cText := Stuff(cText, nPosDay, 2, StrZero(nDay,2,0))
						ENDIF
					ENDIF
					IF nPosMonth<>0
						nMonth := Val(SubStr(cText,nPosMonth,2))
						IF nMonth<>0
							cText := Stuff(cText, nPosMonth, 2, StrZero(nMonth,2,0))
						ENDIF
					ENDIF
					IF nDay<>0 .AND. nMonth<>0
						IF cOrigText<>cText
							SELF:__SetText(cText)
						ENDIF
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDIF

RETURN cText

ASSIGN TextValue(cNewText) 
	

	SUPER:TextValue := cNewText

	IF (oEditString != NULL_OBJECT)
		IF (IsInstanceOf(oFieldSpec, #FieldSpec) .AND. (IVarGet(oFieldSpec, #Nullable) == TRUE))
			uValue := Unformat(cNewText, SELF:Picture, oEditString:Type+"0")
		ELSE
			uValue := Unformat(cNewText, SELF:Picture, oEditString:Type)
		ENDIF
		oEditString:UsualValue := uValue
	ENDIF

	RETURN 

METHOD Undo() 
	

	IF (oEditString != NULL_OBJECT)
		RETURN oEditString:Undo()
	ENDIF
	RETURN SUPER:Undo()



END CLASS

VOSTRUCT strucPictureFuncFlags
	MEMBER lLeftJust AS LOGIC
	MEMBER lDispCR AS LOGIC
	MEMBER lSetDate AS LOGIC
	MEMBER lBritDate AS LOGIC
	MEMBER lInsNonTemp AS LOGIC
	MEMBER lDispDB AS LOGIC
	MEMBER lZeroBlank AS LOGIC
	MEMBER lNegInParen AS LOGIC
	MEMBER lConvUpper AS LOGIC
	MEMBER lAlphaOnly AS LOGIC

	//static global dim aKeyStates[256] as byte



#region defines
DEFINE FSEL_ALL := 0
DEFINE FSEL_END := 3
DEFINE FSEL_HOME := 2
DEFINE FSEL_TRIM := 1
DEFINE FSEL_TRIMEND := 4
DEFINE MAX_FMTSTRING_LEN := 4096
DEFINE OVERWRITE_ALWAYS := 2
DEFINE OVERWRITE_NEVER := 0
DEFINE OVERWRITE_ONKEY := 1
DEFINE SCRMODE_FULL := 0
DEFINE SCRMODE_NO := 2
DEFINE SCRMODE_PART := 1
#endregion
