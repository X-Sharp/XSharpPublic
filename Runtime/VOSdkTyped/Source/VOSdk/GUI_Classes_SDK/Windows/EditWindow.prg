


/// <include file="Gui.xml" path="doc/EditWindow/*" />
CLASS EditWindow INHERIT ControlWindow
	PROTECT oMle as Edit
	METHOD AsString()
		RETURN SELF:TextValue

/// <include file="Gui.xml" path="doc/EditWindow.Clear/*" />
	METHOD Clear()
		oMle:Clear()
		RETURN SELF

/// <include file="Gui.xml" path="doc/EditWindow.Copy/*" />
	METHOD Copy()
		IF oMle:__IsValid
		oMle:Copy()
		ENDIF
		RETURN SELF

/// <include file="Gui.xml" path="doc/EditWindow.Cut/*" />
	METHOD Cut()
		IF oMle:__IsValid
			oMle:Cut()
		ENDIF
		RETURN SELF

/// <include file="Gui.xml" path="doc/EditWindow.Font/*" />
	ASSIGN Font(oNewFont AS Font)
		oFont := oNewFont
		SELF:__SetFont()
		RETURN

/// <include file="Gui.xml" path="doc/EditWindow.GetLine/*" />
	METHOD GetLine(nLineNumber, nMaxLength)
		IF oMle:__IsValid
			RETURN oMle:GetLine(nLineNumber, nMaxLength)
		ENDIF
		RETURN STRING.Empty

/// <include file="Gui.xml" path="doc/EditWindow.ctor/*" />
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension)
		oMle := MultiLineEdit{oOwner,xID,oPoint,oDimension}
		SUPER(oMle)
		RETURN

/// <include file="Gui.xml" path="doc/EditWindow.Length/*" />
	ACCESS Length
		IF oMle:__IsValid
			RETURN oMle:Length
		ENDIF
		RETURN 0

/// <include file="Gui.xml" path="doc/EditWindow.LineCount/*" />
	ACCESS LineCount
		IF oMle:__IsValid
			RETURN oMle:LineCount
		ENDIF
		RETURN 0

/// <include file="Gui.xml" path="doc/EditWindow.LineDown/*" />
	METHOD LineDown()
		IF oMle:__IsValid
			oMle:LineDown()
		ENDIF
		RETURN SELF

/// <include file="Gui.xml" path="doc/EditWindow.LineUp/*" />
	METHOD LineUp()
		IF oMle:__IsValid
			oMle:LineUp()
		ENDIF

		RETURN SELF

/// <include file="Gui.xml" path="doc/EditWindow.PageDown/*" />
	METHOD PageDown()
		IF oMle:__IsValid
			oMle:PageDown()
		ENDIF
		RETURN SELF

/// <include file="Gui.xml" path="doc/EditWindow.PageUp/*" />
	METHOD PageUp()
		IF oMle:__IsValid
			oMle:PageUp()
		ENDIF
		RETURN SELF

/// <include file="Gui.xml" path="doc/EditWindow.Paste/*" />
	METHOD Paste(cNewString)
		IF oMle:__IsValid
			oMle:Paste(cNewString)
		ENDIF
		RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.ScrollHorizontal/*" />
METHOD ScrollHorizontal(nChars)
		IF oMle:__IsValid
			oMle:ScrollHorizontal(nChars)
		ENDIF
		RETURN SELF

/// <include file="Gui.xml" path="doc/EditWindow.ScrollVertical/*" />
	METHOD ScrollVertical(nLines)
		IF oMle:__IsValid
			oMle:ScrollVertical(nLines)
		ENDIF
		RETURN SELF

/// <include file="Gui.xml" path="doc/EditWindow.Selection/*" />
	ACCESS Selection
		IF oMle:__IsValid
			RETURN oMle:Selection
		ENDIF
		RETURN Selection{}

/// <include file="Gui.xml" path="doc/EditWindow.Selection/*" />
	ASSIGN Selection(oSelection)
		IF oMle:__IsValid
			oMle:Selection:=oSelection
		ENDIF

/// <include file="Gui.xml" path="doc/EditWindow.TextLimit/*" />
	ASSIGN TextLimit(nChars)
		IF oMle:__IsValid
			oMle:TextLimit:=nChars
		ENDIF

/// <include file="Gui.xml" path="doc/EditWindow.TextValue/*" />
	ACCESS TextValue
		IF oMle:__IsValid
			RETURN oMle:TextValue
		ENDIF
		RETURN STRING.Empty

/// <include file="Gui.xml" path="doc/EditWindow.TextValue/*" />
	ASSIGN TextValue(cText)
		IF oMle:__IsValid
			oMle:TextValue:=cText
		ENDIF

/// <include file="Gui.xml" path="doc/EditWindow.Undo/*" />
	METHOD Undo()
		IF oMle:__IsValid
			RETURN oMle:Undo()
		ENDIF
		RETURN SELF

/// <include file="Gui.xml" path="doc/EditWindow.Value/*" />
	ACCESS Value
		IF oMle:__IsValid
			RETURN SELF:TextValue
		ENDIF
		RETURN STRING.Empty

/// <include file="Gui.xml" path="doc/EditWindow.Value/*" />
	ASSIGN Value(uValue)
		IF oMle:__IsValid
			SELF:TextValue := AsString(uValue)
		ENDIF
END CLASS

