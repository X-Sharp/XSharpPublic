/// <include file="Gui.xml" path="doc/EditWindow/*" />
#pragma options("lb", off)
CLASS EditWindow INHERIT ControlWindow
   PROPERTY __Edit AS MultiLineEdit GET (MultilineEdit) SELF:oCtrl


/// <include file="Gui.xml" path="doc/EditWindow.AsString/*" />
METHOD AsString()




	RETURN SELF:TextValue


/// <include file="Gui.xml" path="doc/EditWindow.Clear/*" />
METHOD Clear()


	SELF:__Edit:Clear()
	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.Copy/*" />
METHOD Copy()


	SELF:__Edit:Copy()


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.Cut/*" />
METHOD Cut()


	SELF:__Edit:Cut()


	RETURN SELF

/// <include file="Gui.xml" path="doc/EditWindow.Font/*" />
ASSIGN Font(oNewFont)
    LOCAL hFont AS PTR
    LOCAL oFont AS Font
    oFont := oNewFont






	IF (hWnd != NULL_PTR)
		SELF:__SetFont(oFont)
		IF (oFont != NULL_OBJECT)
			oFont:Create()
			hFont := oFont:Handle()
		ELSE
			hFont := GetStockObject ( System_Font )
		ENDIF
		SendMessage(hWnd, WM_SETFONT, DWORD(_CAST, hFont), 1)
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/EditWindow.GetLine/*" />
METHOD GetLine(nLineNumber, nMaxLength)




	RETURN SELF:__Edit:GetLine(nLineNumber, nMaxLength)


/// <include file="Gui.xml" path="doc/EditWindow.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension)




	SUPER(MultiLineEdit{oOwner,xID,oPoint,oDimension})
	RETURN


/// <include file="Gui.xml" path="doc/EditWindow.Length/*" />
ACCESS Length




	RETURN SELF:__Edit:Length


/// <include file="Gui.xml" path="doc/EditWindow.LineCount/*" />
ACCESS LineCount




	RETURN SELF:__Edit:LineCount


/// <include file="Gui.xml" path="doc/EditWindow.LineDown/*" />
METHOD LineDown()




	SELF:__Edit:LineDown()


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.LineUp/*" />
METHOD LineUp()




	SELF:__Edit:LineUp()


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.PageDown/*" />
METHOD PageDown()




	SELF:__Edit:PageDown()


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.PageUp/*" />
METHOD PageUp()




	SELF:__Edit:PageUp()


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.Paste/*" />
METHOD Paste(cNewString)




	SELF:__Edit:Paste(cNewString)


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.ScrollHorizontal/*" />
METHOD ScrollHorizontal(nChars)




	SELF:__Edit:ScrollHorizontal(nChars)


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.ScrollVertical/*" />
METHOD ScrollVertical(nLines)




	SELF:__Edit:ScrollVertical(nLines)


	RETURN SELF


/// <include file="Gui.xml" path="doc/EditWindow.Selection/*" />
ACCESS Selection




	RETURN SELF:__Edit:Selection


/// <include file="Gui.xml" path="doc/EditWindow.Selection/*" />
ASSIGN Selection(oSelection)




	RETURN SELF:__Edit:Selection:=oSelection


/// <include file="Gui.xml" path="doc/EditWindow.TextLimit/*" />
ASSIGN TextLimit(nChars)




	RETURN SELF:__Edit:TextLimit:=nChars


/// <include file="Gui.xml" path="doc/EditWindow.TextValue/*" />
ACCESS TextValue




	RETURN SELF:__Edit:TextValue


/// <include file="Gui.xml" path="doc/EditWindow.TextValue/*" />
ASSIGN TextValue(cText)




	RETURN SELF:__Edit:TextValue:=cText


/// <include file="Gui.xml" path="doc/EditWindow.Undo/*" />
METHOD Undo()




	RETURN SELF:__Edit:Undo()


/// <include file="Gui.xml" path="doc/EditWindow.Value/*" />
ACCESS Value




	RETURN SELF:TextValue


/// <include file="Gui.xml" path="doc/EditWindow.Value/*" />
ASSIGN Value(uValue)




	RETURN SELF:TextValue := AsString(uValue)
END CLASS


