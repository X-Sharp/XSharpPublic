
CLASS PushButton INHERIT Button

    PROPERTY ControlType AS ControlType GET ControlType.Button

	CONSTRUCTOR( oOwner, xID, oPoint, oDimension, cText, kStyle) 
		SUPER(oOwner, xID, oPoint, oDimension, cText, kStyle, FALSE)
		IF IsInstanceOfUsual(xID,#ResourceID)
			SELF:SetStyle(BS_PushButton)
		ENDIF

		RETURN 

	ACCESS __Button AS VOButton 
		RETURN (VOButton) oCtrl

	
	ACCESS Value() 
		RETURN FALSE

	ASSIGN Value(uNewValue) 
		RETURN 

END CLASS

