



CLASS EditWindow INHERIT ControlWindow
	PROTECT oMle as MultiLineEdit
	METHOD AsString() 
		RETURN SELF:TextValue

	METHOD Clear() 
		oMle:Clear()
		RETURN SELF

	METHOD Copy() 
		oMle:Copy()
		RETURN SELF

	METHOD Cut() 
		IF oMle:__IsValid
			oMle:Cut()
		ENDIF
		RETURN SELF

	ASSIGN Font(oNewFont AS Font) 
		oFont := oNewFont
		SELF:__SetFont()
		RETURN 

	METHOD GetLine(nLineNumber, nMaxLength) 
		IF oMle:__IsValid
			RETURN oMle:GetLine(nLineNumber, nMaxLength)
		ENDIF
		RETURN STRING.Empty
	
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
		oMle := MultiLineEdit{oOwner,xID,oPoint,oDimension}
		SUPER(oMle)
		RETURN 

	ACCESS Length 
		IF oMle:__IsValid
			RETURN oMle:Length
		ENDIF
		RETURN 0
		
	ACCESS LineCount 
		IF oMle:__IsValid
			RETURN oMle:LineCount
		ENDIF
		RETURN 0

	METHOD LineDown() 
		IF oMle:__IsValid
			oMle:LineDown()
		ENDIF
		RETURN SELF

	METHOD LineUp() 
		IF oMle:__IsValid
			oMle:LineUp()
		ENDIF
		
		RETURN SELF

	METHOD PageDown() 
		IF oMle:__IsValid
			oMle:PageDown()
		ENDIF
		RETURN SELF

	METHOD PageUp() 
		IF oMle:__IsValid
			oMle:PageUp()
		ENDIF
		RETURN SELF

	METHOD Paste(cNewString) 
		IF oMle:__IsValid
			oMle:Paste(cNewString)
		ENDIF
		RETURN SELF

	METHOD ScrollHorizontal(nChars) 
		IF oMle:__IsValid
			oMle:ScrollHorizontal(nChars)
		ENDIF
		RETURN SELF

	METHOD ScrollVertical(nLines) 
		IF oMle:__IsValid
			oMle:ScrollVertical(nLines)
		ENDIF
		RETURN SELF

	ACCESS Selection 
		IF oMle:__IsValid
			RETURN oMle:Selection
		ENDIF
		RETURN Selection{}

	ASSIGN Selection(oSelection) 
		IF oMle:__IsValid
			oMle:Selection:=oSelection
		ENDIF

	ASSIGN TextLimit(nChars) 
		IF oMle:__IsValid
			oMle:TextLimit:=nChars
		ENDIF

	ACCESS TextValue 
		IF oMle:__IsValid
			RETURN oMle:TextValue
		ENDIF
		RETURN STRING.Empty
	
	ASSIGN TextValue(cText) 
		IF oMle:__IsValid
			oMle:TextValue:=cText
		ENDIF

	METHOD Undo() 
		IF oMle:__IsValid
			RETURN oMle:Undo()
		ENDIF
		RETURN SELF

	ACCESS Value 
		IF oMle:__IsValid
			RETURN SELF:TextValue
		ENDIF
		RETURN STRING.Empty

	ASSIGN Value(uValue) 
		IF oMle:__IsValid
			SELF:TextValue := AsString(uValue)
		ENDIF
END CLASS

