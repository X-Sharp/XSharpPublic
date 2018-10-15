PARTIAL CLASS GroupBox INHERIT TextControl

METHOD AsString () 
	
	RETURN "#"+Symbol2String(ClassName(SELF))+":"+SELF:Caption

ACCESS CurrentText 
	
	RETURN Null_String

ASSIGN CurrentText(cValue) 
	
	RETURN 

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cText, lDataAware) 
	

	Default(@lDataAware, FALSE)

	IF IsInstanceOfUsual(xID,#ResourceID)
		SUPER(oOwner, xID, oPoint, oDimension, , , lDataAware)
	ELSE
		SUPER(oOwner, xID, oPoint, oDimension, "Button", _Or(BS_GroupBox, WS_TabStop), lDataAware)
		IF !IsNil(cText)
			SELF:Caption := cText
		ENDIF
	ENDIF

	RETURN 
END CLASS

