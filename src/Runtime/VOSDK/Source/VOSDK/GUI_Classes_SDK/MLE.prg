/// <include file="Gui.xml" path="doc/MultiLineEdit/*" />
CLASS MultiLineEdit INHERIT Edit
	PROTECT ptrOldMLEProc AS PTR


/// <include file="Gui.xml" path="doc/MultiLineEdit.Dispatch/*" />
METHOD Dispatch(oEvent)


	LOCAL oEvt := oEvent AS @@Event


	IF (oEvt:uMsg == WM_KEYDOWN) .AND. (oEvt:wParam == VK_ESCAPE)
		RETURN 1L
	ENDIF
	RETURN SUPER:Dispatch(oEvent)


/// <include file="Gui.xml" path="doc/MultiLineEdit.GetLine/*" />
METHOD GetLine(nLineNumber, nMaxLength)
   //SE-070525
	LOCAL hHandle AS PTR
	LOCAL dwIndex AS DWORD
	LOCAL dwChars AS DWORD
	//LOCAL ptrBuffer AS _GetLine
   LOCAL ptrBuffer AS WORD PTR


	LOCAL dwRetVal AS DWORD
	LOCAL sBuf AS STRING


	IF SELF:ValidateControl()
		hHandle := SELF:Handle()
		IF IsNil(nLineNumber) .OR. (IsLong(nLineNumber) .AND. nLineNumber==0)
			dwIndex := DWORD(SendMessage(hHandle, EM_LINEFROMCHAR, 0xffffffff, 0))
		ELSEIF !IsLong(nLineNumber)
			WCError{#GetLine, #MultiLineEdit, __WCSTypeError, nLineNumber, 1}:Throw()
		ELSE
			dwIndex := nLineNumber-1
		ENDIF


		IF IsNil(nMaxLength) .OR. (IsLong(nMaxLength) .AND. nMaxLength==-1)
			dwChars := SELF:GetLineLength(dwIndex+1)
		ELSEIF !IsLong(nMaxLength)
			WCError{#GetLine, #MultiLineEdit, __WCSTypeError, nMaxLength, 2}:Throw()
		ELSE
			dwChars := nMaxLength
		ENDIF


		//ptrBuffer := MemAlloc(dwChars+2)
		//ptrBuffer.wByteCount := dwChars
      ptrBuffer       := MemAlloc(dwChars+1)
      WORD(ptrBuffer) := LoWord(dwChars)




		dwRetVal := DWORD(SendMessage(hHandle, EM_GETLINE, dwIndex, LONGINT(_CAST, ptrBuffer)))
		IF (dwRetVal != 0)
			sBuf := Mem2String(ptrBuffer, dwRetVal)
		ENDIF
		MemFree(ptrBuffer)
	ENDIF


	RETURN sBuf


/// <include file="Gui.xml" path="doc/MultiLineEdit.GetLineLength/*" />
METHOD GetLineLength(nLineNumber)
	LOCAL hHandle AS PTR
	LOCAL dwIndex AS DWORD






	IF IsNil(nLineNumber) .OR. (IsLong(nLineNumber) .AND. nLineNumber==0)
		dwIndex := 0xffffffff
	ELSEIF !IsLong(nLineNumber)
		WCError{#GetLineLength, #MultiLineEdit, __WCSTypeError, nLineNumber, 1}:Throw()
	ELSE
		dwIndex := nLineNumber-1
	ENDIF


	IF SELF:ValidateControl()
		hHandle := SELF:Handle()
		RETURN SendMessage(hHandle,	EM_LINELENGTH, DWORD(SendMessage(hHandle, EM_LINEINDEX, dwIndex, 0)), 0)
	ENDIF


	RETURN 0


/// <include file="Gui.xml" path="doc/MultiLineEdit.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle)




	//PP-030902
	// dwStyle := _Or(dwStyle, WS_HSCROLL)
	SUPER( oOwner, xID, oPoint, oDimension, kStyle )
	IF !(xID IS ResourceID)
		SELF:SetStyle(ES_MultiLine)
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/MultiLineEdit.LineCount/*" />
ACCESS LineCount




	IF SELF:ValidateControl()
		RETURN SendMessage(SELF:Handle(), EM_GETLINECOUNT, 0, 0)
	ENDIF


	RETURN 0


/// <include file="Gui.xml" path="doc/MultiLineEdit.LineDown/*" />
METHOD LineDown ( )




	IF SELF:ValidateControl()
		SendMessage(SELF:Handle(), EM_SCROLL, SB_LINEDOWN, 0)
	ENDIF


	RETURN SELF


/// <include file="Gui.xml" path="doc/MultiLineEdit.LineUp/*" />
METHOD LineUp ( )




	IF SELF:ValidateControl()
		SendMessage(SELF:Handle(), EM_SCROLL, SB_LINEUP, 0)
	ENDIF


	RETURN SELF


/// <include file="Gui.xml" path="doc/MultiLineEdit.PageDown/*" />
METHOD PageDown ( )




	IF SELF:ValidateControl()
		SendMessage(SELF:Handle(), EM_SCROLL, SB_PageDown, 0)
	ENDIF


	RETURN SELF


/// <include file="Gui.xml" path="doc/MultiLineEdit.PageUp/*" />
METHOD PageUp()




	IF SELF:ValidateControl()
		SendMessage(SELF:Handle(), EM_SCROLL, SB_PageUp, 0)
	ENDIF


	RETURN SELF


/// <include file="Gui.xml" path="doc/MultiLineEdit.ScrollHorizontal/*" />
METHOD ScrollHorizontal(nChars)




	IF !IsLong(nChars)
		WCError{#ScrollHorizontal, #MultiLineEdit, __WCSTypeError, nChars, 1}:Throw()
	ENDIF


	IF SELF:ValidateControl()
		SendMessage(SELF:Handle(), EM_LINESCROLL, DWORD(_CAST, nChars), 0)
	ENDIF


	RETURN SELF


/// <include file="Gui.xml" path="doc/MultiLineEdit.ScrollVertical/*" />
METHOD ScrollVertical(nLines)




	IF !IsLong(nLines)
		WCError{#ScrollVertical, #MultiLineEdit, __WCSTypeError, nLines, 1}:Throw()
	ENDIF


	IF SELF:ValidateControl()
		SendMessage(SELF:Handle(), EM_LINESCROLL, 0, nLines)
	ENDIF


	RETURN SELF


// STATIC STRUCTURE _GetLine
// 	MEMBER wByteCount AS DWORD
// 	MEMBER bString AS BYTE //The string


END CLASS


