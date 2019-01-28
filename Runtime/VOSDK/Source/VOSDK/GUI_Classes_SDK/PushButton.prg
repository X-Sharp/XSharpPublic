CLASS PushButton INHERIT Button

CONSTRUCTOR( oOwner, xID, oPoint, oDimension, cText, kStyle) 
	

	SUPER(oOwner, xID, oPoint, oDimension, cText, kStyle, FALSE)
	IF IsInstanceOfUsual(xID,#ResourceID)
		SELF:SetStyle(BS_PushButton)
	ENDIF

	RETURN 

ACCESS Value() 
	

	RETURN FALSE

ASSIGN Value(uNewValue) 
	

	RETURN 
END CLASS

