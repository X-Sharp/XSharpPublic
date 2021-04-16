/// <include file="Gui.xml" path="doc/PushButton/*" />
CLASS PushButton INHERIT Button


/// <include file="Gui.xml" path="doc/PushButton.ctor/*" />
CONSTRUCTOR( oOwner, xID, oPoint, oDimension, cText, kStyle) 
	
	


	SUPER(oOwner, xID, oPoint, oDimension, cText, kStyle, FALSE)
	IF IsInstanceOfUsual(xID,#ResourceID)
		SELF:SetStyle(BS_PushButton)
	ENDIF


	RETURN 


/// <include file="Gui.xml" path="doc/PushButton.Value/*" />
ACCESS Value() 
	
	


	RETURN FALSE


/// <include file="Gui.xml" path="doc/PushButton.Value/*" />
ASSIGN Value(uNewValue) 
	
	


	RETURN 
END CLASS


