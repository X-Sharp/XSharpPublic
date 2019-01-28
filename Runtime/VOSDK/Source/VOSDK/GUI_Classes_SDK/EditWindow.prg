CLASS EditWindow INHERIT ControlWindow

METHOD AsString() 
	

	RETURN SELF:TextValue

METHOD Clear() 
	

	oCtrl:Clear()

	RETURN SELF

METHOD Copy() 
	

	oCtrl:Copy()

	RETURN SELF

METHOD Cut() 
	

	oCtrl:Cut()

	RETURN SELF

ASSIGN Font(oNewFont) 
	LOCAL hFont AS PTR

	

	IF (hWnd != NULL_PTR)
		SELF:__SetFont(oNewFont)
		IF (oNewFont != NULL_OBJECT)
			oNewFont:Create()
			hFont := oNewFont:Handle()
		ELSE
			hFont := GetStockObject ( System_Font )
		ENDIF
		SendMessage(hWnd, WM_SETFONT, DWORD(_CAST, hFont), 1)
	ENDIF

	RETURN 

METHOD GetLine(nLineNumber, nMaxLength) 
	

	RETURN oCtrl:GetLine(nLineNumber, nMaxLength)

CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	

	SUPER(MultiLineEdit{oOwner,xID,oPoint,oDimension})

	RETURN 

ACCESS Length 
	

	RETURN oCtrl:Length

ACCESS LineCount 
	

	RETURN oCtrl:LineCount

METHOD LineDown() 
	

	oCtrl:LineDown()

	RETURN SELF

METHOD LineUp() 
	

	oCtrl:LineUp()

	RETURN SELF

METHOD PageDown() 
	

	oCtrl:PageDown()

	RETURN SELF

METHOD PageUp() 
	

	oCtrl:PageUp()

	RETURN SELF

METHOD Paste(cNewString) 
	

	oCtrl:Paste(cNewString)

	RETURN SELF

METHOD ScrollHorizontal(nChars) 
	

	oCtrl:ScrollHorizontal(nChars)

	RETURN SELF

METHOD ScrollVertical(nLines) 
	

	oCtrl:ScrollVertical(nLines)

	RETURN SELF

ACCESS Selection 
	

	RETURN oCtrl:Selection

ASSIGN Selection(oSelection) 
	

	RETURN oCtrl:Selection:=oSelection

ASSIGN TextLimit(nChars) 
	

	RETURN oCtrl:TextLimit:=nChars

ACCESS TextValue 
	

	RETURN oCtrl:TextValue

ASSIGN TextValue(cText) 
	

	RETURN oCtrl:TextValue:=cText

METHOD Undo() 
	

	RETURN oCtrl:Undo()

ACCESS Value 
	

	RETURN SELF:TextValue

ASSIGN Value(uValue) 
	

	RETURN SELF:TextValue := AsString(uValue)
END CLASS

