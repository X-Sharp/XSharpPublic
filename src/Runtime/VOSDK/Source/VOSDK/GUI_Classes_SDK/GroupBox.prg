/// <include file="Gui.xml" path="doc/GroupBox/*" />
CLASS GroupBox INHERIT TextControl


/// <include file="Gui.xml" path="doc/GroupBox.AsString/*" />
METHOD AsString ()


	RETURN "#"+Symbol2String(ClassName(SELF))+":"+SELF:Caption


/// <include file="Gui.xml" path="doc/GroupBox.CurrentText/*" />
ACCESS CurrentText


	RETURN Null_String


/// <include file="Gui.xml" path="doc/GroupBox.CurrentText/*" />
ASSIGN CurrentText(cValue)


	RETURN


/// <include file="Gui.xml" path="doc/GroupBox.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cText, lDataAware)




	Default(@lDataAware, FALSE)


	IF (xID IS ResourceID)
		SUPER(oOwner, xID, oPoint, oDimension, , , lDataAware)
	ELSE
		SUPER(oOwner, xID, oPoint, oDimension, "Button", _Or(BS_GroupBox, WS_TabStop), lDataAware)
		IF !IsNil(cText)
			SELF:Caption := cText
		ENDIF
	ENDIF


	RETURN
END CLASS


