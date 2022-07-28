/// <include file="Gui.xml" path="doc/EditWindow/*" />
CLASS EditWindow INHERIT ControlWindow


/// <include file="Gui.xml" path="doc/EditWindow.AsString/*" />
METHOD AsString()




	RETURN SELF:TextValue


/// <include file="Gui.xml" path="doc/EditWindow.Clear/*" />
METHOD Clear()


	oCtrl:Clear()
	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.Copy/*" />
METHOD Copy()


	oCtrl:Copy()


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.Cut/*" />
METHOD Cut()


	oCtrl:Cut()


	RETURN SELF

/// <include file="Gui.xml" path="doc/EditWindow.Font/*" />
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


/// <include file="Gui.xml" path="doc/EditWindow.GetLine/*" />
METHOD GetLine(nLineNumber, nMaxLength)




	RETURN oCtrl:GetLine(nLineNumber, nMaxLength)


/// <include file="Gui.xml" path="doc/EditWindow.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension)




	SUPER(MultiLineEdit{oOwner,xID,oPoint,oDimension})


	RETURN


/// <include file="Gui.xml" path="doc/EditWindow.Length/*" />
ACCESS Length




	RETURN oCtrl:Length


/// <include file="Gui.xml" path="doc/EditWindow.LineCount/*" />
ACCESS LineCount




	RETURN oCtrl:LineCount


/// <include file="Gui.xml" path="doc/EditWindow.LineDown/*" />
METHOD LineDown()




	oCtrl:LineDown()


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.LineUp/*" />
METHOD LineUp()




	oCtrl:LineUp()


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.PageDown/*" />
METHOD PageDown()




	oCtrl:PageDown()


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.PageUp/*" />
METHOD PageUp()




	oCtrl:PageUp()


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.Paste/*" />
METHOD Paste(cNewString)




	oCtrl:Paste(cNewString)


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.ScrollHorizontal/*" />
METHOD ScrollHorizontal(nChars)




	oCtrl:ScrollHorizontal(nChars)


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.ScrollVertical/*" />
METHOD ScrollVertical(nLines)




	oCtrl:ScrollVertical(nLines)


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.Selection/*" />
ACCESS Selection




	RETURN oCtrl:Selection


/// <include file="Gui.xml" path="doc/EditWindow.Selection/*" />
ASSIGN Selection(oSelection)




	RETURN oCtrl:Selection:=oSelection


/// <include file="Gui.xml" path="doc/EditWindow.TextLimit/*" />
ASSIGN TextLimit(nChars)




	RETURN oCtrl:TextLimit:=nChars


/// <include file="Gui.xml" path="doc/EditWindow.TextValue/*" />
ACCESS TextValue




	RETURN oCtrl:TextValue


/// <include file="Gui.xml" path="doc/EditWindow.TextValue/*" />
ASSIGN TextValue(cText)




	RETURN oCtrl:TextValue:=cText


/// <include file="Gui.xml" path="doc/EditWindow.Undo/*" />
METHOD Undo()




	RETURN oCtrl:Undo()


/// <include file="Gui.xml" path="doc/EditWindow.Value/*" />
ACCESS Value




	RETURN SELF:TextValue


/// <include file="Gui.xml" path="doc/EditWindow.Value/*" />
ASSIGN Value(uValue)




	RETURN SELF:TextValue := AsString(uValue)
END CLASS


