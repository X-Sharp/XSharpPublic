

CLASS MultiLineEdit INHERIT Edit


    PROPERTY ControlType AS ControlType GET ControlType.Mle


	METHOD __GetText() AS STRING STRICT 
		LOCAL cText AS STRING
		cText := SUPER:__GetText()
		IF cText:Contains(e"\n")
			// Remove Line Feeds without CR and replace CR with CRLF
			cText := cText:Replace(e"\r","")
			cText := cText:Replace(e"\n",e"\r\n")
		ENDIF
		RETURN cText

	ACCESS __MultiLineEdit AS VOMLETextBox
		RETURN (VOMLETextBox) oCtrl
	
	METHOD GetLine(nLineNumber, nMaxLength) 
		LOCAL dwIndex AS LONG
		LOCAL sBuf AS STRING
		DEFAULT(@nLineNumber, 0)
		DEFAULT(@nMaxLength, 0)
		IF SELF:ValidateControl()
			IF nLineNumber==0
				dwIndex :=  __MultiLineEdit:GetLineFromCharIndex(__MultiLineEdit:SelectionStart)
			ELSE
				dwIndex := nLineNumber-1
			ENDIF
			IF dwIndex >=0 .and. dwIndex < __MultiLineEdit:Lines:Length
				sBuf := __MultiLineEdit:Lines[dwIndex]
			ENDIF
			IF sBuf != NULL .and. nMaxLength >= 0 .and. sBuf:Length > 0
				sBuf := sBuf:Substring(0,nMaxLength)
			ENDIF
		ENDIF
		RETURN sBuf

	METHOD GetLineLength(nLineNumber) 
		LOCAL sLine AS STRING
		IF SELF:ValidateControl()
			sLine := SELF:GetLine(nLineNumber)
			IF sLine != NULL
				RETURN sLine:Length
			ENDIF
		ENDIF
		RETURN 0

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 
		SUPER( oOwner, xID, oPoint, oDimension, kStyle )
		IF !IsInstanceOfUsual(xID,#ResourceID)
			SELF:SetStyle(ES_MultiLine)
		ENDIF

		RETURN 

	ACCESS LineCount 
		IF SELF:ValidateControl()
			RETURN __MultiLineEdit:Lines:Length
		ENDIF
		RETURN 0

	METHOD LineDown ( ) 
		IF SELF:ValidateControl()
			Win32.SendMessage(oCtrl:Handle, EM_SCROLL, SB_LINEDOWN, 0)
		ENDIF
		RETURN SELF

	METHOD LineUp ( ) 
		IF SELF:ValidateControl()
			Win32.SendMessage(oCtrl:Handle, EM_SCROLL, SB_LINEUP, 0)
		ENDIF

		RETURN SELF

	METHOD PageDown ( ) 
		IF SELF:ValidateControl()
			Win32.SendMessage(oCtrl:Handle, EM_SCROLL, SB_PageDown, 0)
		ENDIF

		RETURN SELF

	METHOD PageUp() 
		IF SELF:ValidateControl()
			Win32.SendMessage(oCtrl:Handle, EM_SCROLL, SB_PageUp, 0)
		ENDIF

		RETURN SELF

	METHOD @@ScrollHorizontal(nChars) 
		IF !IsLong(nChars)
			WCError{#ScrollHorizontal, #MultiLineEdit, __WCSTypeError, nChars, 1}:@@Throw()
		ENDIF

		IF SELF:ValidateControl()
			Win32.SendMessage(oCtrl:Handle, EM_LINESCROLL, DWORD(_CAST, nChars), 0)
		ENDIF

		RETURN SELF

	METHOD @@ScrollVertical(nLines) 
		IF !IsLong(nLines)
			WCError{#ScrollVertical, #MultiLineEdit, __WCSTypeError, nLines, 1}:@@Throw()
		ENDIF

		IF SELF:ValidateControl()
			Win32.SendMessage(oCtrl:Handle, EM_LINESCROLL, 0, (LONG) nLines)
		ENDIF

		RETURN SELF

END CLASS

